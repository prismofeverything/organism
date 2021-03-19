(ns organism.persist
  (:require
   [organism.mongo :as db]))

(defn create-game!
  [db {:keys [key invocation game chat] :as game-state}]
  (let [initial-state (:state game)
        game-state (assoc game-state :current-state initial-state)
        game-state (update-in game-state [:game :adjacencies] pr-str)]
    (println "CREATING GAME" game-state)
    (db/insert! db :games game-state)
    (db/insert! db :history {:key key :state initial-state})))

(defn update-state!
  [db key state]
  (db/upsert! db :games {:key key} {:$set {:current-state state}})
  (db/insert! db :history {:key key :state state}))

(defn reset-state!
  [db key]
  (let [recent (db/find-last db :history {:key key})]
    (db/delete! db :history {:_id (:_id recent)})))

(defn load-game
  [db key]
  (if-let [game-state (db/one db :games {:key key})]
    (let [current-state (:current-state game-state)
          history (db/query db :history {:key key})]
      (-> game-state
          (assoc-in [:game :state] current-state)
          (assoc :history history)
          (update-in [:game :adjacencies] read-string)
          (select-keys [:key :invocation :created :game :chat :history])))))
