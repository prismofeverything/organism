(ns organism.persist
  (:require
   [organism.mongo :as db]))

(defn serialize-state
  [state]
  (-> state
      (update :elements pr-str)
      (update-in [:player-turn :organism-turns] pr-str)))

(defn deserialize-state
  [state]
  (println "DESERIALIZE" state)
  (-> state
      (dissoc :_id)
      (update :elements read-string)
      (update-in [:player-turn :organism-turns] read-string)))

(defn create-game!
  [db {:keys [key invocation game chat] :as game-state}]
  (let [initial-state (serialize-state (:state game))
        game-state (assoc game-state :current-state initial-state)
        game-state (update-in game-state [:game :adjacencies] pr-str)]
    (println "CREATING GAME" game-state)
    (db/insert! db :games game-state)
    (db/insert! db :history {:key key :state initial-state})))

(defn update-state!
  [db key state]
  (let [serial (serialize-state state)]
    (db/upsert! db :games {:key key} {:$set {:current-state serial}})
    (db/insert! db :history {:key key :state serial})))

(defn reset-state!
  [db key]
  (let [recent (db/find-last db :history {:key key})]
    (db/delete! db :history {:_id (:_id recent)})))

(defn load-history
  [db key]
  (let [records (db/query db :history {:key key})]
    (mapv
     (comp deserialize-state :state)
     records)))

(defn load-game
  [db key]
  (if-let [game-state (db/one db :games {:key key})]
    (let [current-state (-> game-state :current-state deserialize-state)
          history (load-history db key)]
      (-> game-state
          (assoc-in [:game :state] current-state)
          (assoc :history history)
          (update-in [:game :adjacencies] read-string)
          (select-keys [:key :invocation :game :chat :history])))))
