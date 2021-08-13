(ns organism.migrations.complete-player-games
  (:require
   [organism.mongo :as db]
   [organism.persist :as persist]
   [organism.handler :as handler]))

(defn migrate!
  [db]
  (let [players (persist/load-players db)]
    (doseq [player players]
      (println "completing player games for" player)
      (let [player-key (persist/player-games-key (:key player))
            player-games (db/query db player-key {})]
        (doseq [player-game player-games]
          (when (= "complete" (:status player-game))
            (let [game-key (:game player-game)
                  history-count (db/number db (persist/history-key game-key))]
              (println "completing" game-key "at" history-count "choices")
              (println player-game)
              (db/merge!
               db player-key
               {:game game-key}
               {:witness history-count}))))))))

(defn -main
  []
  (let [db (db/connect! handler/mongo-connection)]
    (println "completing player games")
    (migrate! db)))
