(ns organism.migrations.initial-players-collection
  (:require
   [organism.board :as board]
   [organism.mongo :as db]
   [organism.handler :as handler]))

(defn migrate!
  [db]
  (db/index! db :players [:key] {:unique true})
  (let [games (db/find-all db :games)]
    (doseq [game games]
      (println "migrating game" (:key game))
      (let [invocation (:invocation game)
            player-colors (board/invocation-player-colors invocation)]
        (doseq [[player color] player-colors]
          (println "adding player" player "with color" color)
          (db/merge! db :players {:key player} {:color color}))))))

(defn purge-players!
  [db]
  (db/purge! db :players))

(defn -main
  []
  (let [db (db/connect! handler/mongo-connection)]
    (println "migrating players collection")
    (purge-players! db)
    (migrate! db)))
