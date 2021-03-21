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

(defn create-game!
  [db {:keys [key invocation game chat] :as game-state}]
  (let [initial-state (serialize-state (:state game))
        game-state (update-in game-state [:game :adjacencies] pr-str)]
    (println "CREATING GAME" game-state)
    (db/index! db :games [:key] {:unique true})
    (db/insert! db :games game-state)
    (db/insert! db (history-key key) initial-state)))

(defn update-state!
  [db key state]
  (let [serial (serialize-state state)]
    (db/insert! db (history-key key) serial)))

(defn update-chat!
  [db key line]
  (db/insert! db (chat-key key) line))

(defn reset-state!
  [db key]
  (let [history (history-key key)
        recent (db/find-last db history {})]
    (db/delete! db history {:_id (:_id recent)})))

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
          (update-in [:game :adjacencies] read-string)
          (select-keys [:key :invocation :game :chat :history])))))
