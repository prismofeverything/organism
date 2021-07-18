(ns organism.migrations.fix-extract-mutation
  (:require
   [organism.board :as board]
   [organism.mongo :as db]
   [organism.handler :as handler]))

(defn migrate!
  [db]
  (let [games (db/find-all db :games)]
    (doseq [game games]
      (let [inner-game (:game game)
            game-key (:key inner-game)]
        (db/merge!
         db :games
         {:key game-key}
         inner-game)))))

(defn -main
  []
  (let [db (db/connect! handler/mongo-connection)]
    (println "migrating mutations for existing games")
    (migrate! db)))
