(ns organism.persist
  (:require
   [clojure.walk :as walk]
   [organism.board :as board]
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

(defn create-open-game!
  [db game-key invocation]
  (println "creating open game!" game-key)
  (db/index! db :open-games [:key] {:unique true})
  (db/merge!
   db :open-games
   {:key game-key}
   {:invocation invocation}))

(defn remove-open-game!
  [db game-key]
  (println "removing open game!" game-key)
  (db/delete!
   db :open-games
   {:key game-key}))

(defn invocation-colors
  [invocation]
  (board/find-player-colors
   (:players invocation)
   (map last (:colors invocation))))

(defn create-player-game!
  [db game-key invocation player state]
  (let [round (:round state)
        players (:players invocation)
        current-player (-> state :player-turn :player)
        player-colors (invocation-colors invocation)]
    (println "creating" player game-key)
    (db/insert!
     db (player-key player)
     {:game game-key
      :round round
      :status "active"
      :player-colors player-colors
      :players players
      :current-player current-player
      :winner nil})))

(defn create-player-games!
  [db game-key invocation state]
  (let [players (:players invocation)]
    (doseq [player (reverse players)]
      (create-player-game! db game-key invocation player state))))

(defn update-player-game!
  [db game-key player state]
  (let [current-player (-> state :player-turn :player)
        round (:round state)]
    (println "updating" player game-key current-player)
    (db/merge!
     db (player-key player)
     {:game game-key}
     {:round round
      :current-player current-player})))

(defn update-player-games!
  [db game-key players state]
  (doseq [player (reverse players)]
    (update-player-game! db game-key player state)))

(defn complete-player-game!
  [db game-key player winner state]
  (println "completing" player game-key)
  (let [round (:round state)]
    (db/merge!
     db (player-key player)
     {:game game-key}
     {:round round
      :status "complete"
      :winner winner})))

(defn complete-player-games!
  [db game-key players winner state]
  (doseq [player (reverse players)]
    (complete-player-game! db game-key player winner state)))

(defn create-game!
  [db {:keys [key invocation game chat] :as game-state}]
  (let [initial-state (serialize-state (:state game))
        game-state (update-in game-state [:game :adjacencies] pr-str)
        game-state (update-in game-state [:game :players] pr-str)
        player-colors (invocation-colors invocation)]
    (println "CREATING GAME" game-state)
    (db/index! db :games [:key] {:unique true})
    (db/insert! db :games game-state)
    (db/insert! db (history-key key) initial-state)
    (doseq [player (reverse (:players invocation))]
      (db/index! db (player-key player) [:game] {:unique true}))
    (create-player-games! db key invocation initial-state)))

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
  [db game-key state]
  (let [game-state (db/one db :games {:key game-key})
        serialized (serialize-state state)
        winner (:winner state)
        players (-> game-state :invocation :players)]
    (db/merge!
     db :games
     {:key game-key}
     {:game (assoc (:game game-state) :state serialized)})
    (complete-player-games!
     db game-key players
     winner state)))

(defn deserialize-player-game
  [player-game]
  (-> player-game
      (dissoc :_id)
      (update :player-colors walk/stringify-keys)))

(defn find-open-game
  [db key]
  (dissoc
   (db/one db :open-games {:key key})
   :_id))

(defn load-open-games
  [db]
  (let [records (db/query db :open-games {})
        records (map #(dissoc % :_id) records)]
    records))

(defn load-player-games
  [db player]
  (let [records (db/query db (player-key player) {})
        records (map deserialize-player-game records)
        states (group-by :status records)
        open (load-open-games db)]
    (assoc states "open" open)))

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
