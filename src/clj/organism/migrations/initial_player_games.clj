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
            game-state (persist/load-game-state db game-key)
            current-player (-> game-state :player-turn :player)]
        (println "migrating game" game-key)
        (doseq [player players]
          (println "migrating player" player)
          (let [state (if (= player current-player) "active" "current")]
            (println "state is" state)
            (persist/store-player-game! db player game-key state)))))))

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
