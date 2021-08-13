(ns organism.migrations.remove-empty-games
  (:require
   [organism.mongo :as db]
   [organism.persist :as persist]
   [organism.handler :as handler]))

(defn migrate!
  [db]
  (let [players (persist/load-players db)]
    (doseq [player players]
      (let [player-key (persist/player-games-key (:key player))
            player-games (db/query db player-key {})]
        (doseq [player-game player-games]
          (when-not (:status player-game)
            (println "removing empty game" (:game player-game) "from" player-key)
            (db/delete!
             db player-key
             {:game (:game player-game)})))))))

(defn -main
  []
  (let [db (db/connect! handler/mongo-connection)]
    (println "removing empty games")
    (migrate! db)))
