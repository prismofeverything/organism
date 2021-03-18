(ns organism.persist
  (:require
   [organism.mongo :as db]))

(defn create-game!
  [db {:keys [key invocation game chat] :as game-state}]
  (let [initial-state (:state game)
        game-state (assoc game-state :current-state initial-state)]
    (db/insert! :games game-state)
    (db/insert! :history {:key key :state initial-state})))

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
  (let [game-state (db/one db :games {:key key})
        current-state (:current-state game-state)
        history (db/query db :history {:key key})
        game-state (assoc-in game-state [:game :state] current-state)
        game-state (assoc game-state :history history)]
    (-> game-state
        (assoc-in [:game :state] current-state)
        (assoc :history history)
        (select-keys [:key :invocation :created :game :chat :history]))))
