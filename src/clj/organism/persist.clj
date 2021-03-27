(ns organism.persist
  (:require
   [organism.mongo :as db]))

(defn serialize-state
  [state]
  (-> state
      (update :elements pr-str)
      (update :captures pr-str)
      (update-in [:player-turn :organism-turns] pr-str)))

(defn conditional-string
  [s]
  (if (string? s)
    (read-string s)
    s))

(defn deserialize-state
  [state]
  (-> state
      (dissoc :_id)
      (update :elements read-string)
      (update :captures conditional-string) ;; TODO: remove this once migrated
      (update-in [:player-turn :organism-turns] read-string)))

(defn history-key
  [key]
  (str "history-" key))

(defn chat-key
  [key]
  (str "chat-" key))

(defn player-key
  [key]
  (str "player-games-" key))

(defn update-player-game!
  [db player game-key state]
  (let [current-player (-> state :player-turn :player)
        round (:round state)]
    (println "updating" player game-key status)
    (db/upsert!
     db (player-key player)
     {:game game}
     {:game game
      :round round
      :status status
      :current-player current-player})))

(defn complete-player-game!
  [db player game-key state]
  (println "completing" player game-key status)
  (db/upsert!
   db (player-key player)
   {:game game}
   {:game game
    :round round
    :status status
    :winner winner}))

(defn update-player-games!
  [db players game-key state]
  (doseq [player (reverse players)]
    (update-player-game! db player game-key state)))

(defn create-game!
  [db {:keys [key invocation game chat] :as game-state}]
  (let [initial-state (serialize-state (:state game))
        game-state (update-in game-state [:game :adjacencies] pr-str)
        game-state (update-in game-state [:game :players] pr-str)]
    (println "CREATING GAME" game-state)
    ;; TODO: add player-games-* collection for each player containing their games
    (db/index! db :games [:key] {:unique true})
    (db/insert! db :games game-state)
    (db/insert! db (history-key key) initial-state)
    (doseq [player (reverse (:players invocation))]
      (db/index! db (player-key player) [:game] {:unique true})
      (update-player-game!
       db (player-key player) key
       (:state game)))))

(defn update-state!
  [db key state]
  (let [serial (serialize-state state)]
    (db/insert! db (history-key key) serial)))

(defn update-chat!
  [db key line]
  (db/insert! db (chat-key key) line))

(defn load-game-state
  [db key]
  (db/find-last db (history-key key) {}))

(defn reset-state!
  [db key]
  (let [history (history-key key)
        recent (db/find-last db history {})]
    (db/delete! db history {:_id (:_id recent)})))

(defn complete-game!
  [db game-key winner]
  (let [game (db/one db :games {:key game-key})
        players (-> game :invocation :players)]
    (for [player (reverse players)]
      (complete-player-game!
       db player game-key
       winner))
    (db/upsert! db :games {:key game-key} {:winner winner})))

(defn load-history
  [db key]
  (let [records (db/query db (history-key key) {})]
    (mapv deserialize-state records)))

(defn load-chat
  [db key]
  (let [records (db/query db (chat-key key) {})]
    (mapv
     (fn [record]
       (select-keys record [:type :player :time :message]))
     records)))

(defn load-player-games
  [db player]
  (let [records (db/query db (player-key player) {})
        records (map #(select-keys % [:game :state]) records)
        states (group-by :state records)]
    states))

(defn load-game
  [db key]
  (if-let [game-state (db/one db :games {:key key})]
    (let [history (load-history db key)
          chat (load-chat db key)]
      (-> game-state
          (assoc-in [:game :state] (last history))
          (assoc :history history)
          (assoc :chat chat)
          (update-in [:game :players] conditional-string)
          (update-in [:game :adjacencies] read-string)
          (select-keys [:key :invocation :game :chat :history])))))
