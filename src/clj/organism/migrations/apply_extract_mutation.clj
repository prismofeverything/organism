(ns organism.migrations.apply-extract-mutation
  (:require
   [organism.board :as board]
   [organism.mongo :as db]
   [organism.handler :as handler]))

(defn migrate!
  [db]
  (let [games (db/find-all db :games)]
    (doseq [game games]
      (if-not (= (:key game) "eraglem")
        (let [game-key (:key game)
              invocation (:invocation game)
              mutations {:EXTRACT true}
              updated-game (assoc (:game game) :mutations mutations)
              updated-invocation (assoc invocation :mutations mutations)]
          (println "updating mutations for" game-key ":" mutations)
          (db/merge!
           db :games
           {:key game-key}
           {:invocation updated-invocation
            :game updated-game}))))))

(defn -main
  []
  (let [db (db/connect! handler/mongo-connection)]
    (println "migrating mutations for existing games")
    (migrate! db)))
