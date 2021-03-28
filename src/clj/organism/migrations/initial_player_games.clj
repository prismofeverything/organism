(ns organism.migrations.initial-player-games
  (:require
   [organism.mongo :as db]
   [organism.persist :as persist]
   [organism.handler :as handler]))

(defn migrate!
  [db]
  (let [games (db/find-all db :games)]
    (doseq [game games]
      (let [game-key (:key game)
            players (-> game :invocation :players)
            colors (map last (-> game :invocation :colors))
            game-state (persist/load-game-state db game-key)]
        (persist/update-player-games! db game-key players colors game-state)))))

(defn purge-player-games!
  [db]
  (let [games (db/find-all db :games)]
    (doseq [game games]
      (let [game-key (:key game)
            players (-> game :invocation :players)]
        (doseq [player players]
          (println "purging player games for" player)
          (db/drop! db (persist/player-key player)))))))

(defn -main
  []
  (let [db (db/connect! handler/mongo-connection)]
    (println "migrating initial player games")
    (purge-player-games! db)
    (migrate! db)))