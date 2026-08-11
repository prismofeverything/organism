(ns organism.scripts.rate-players
  "Recompute every player's rating from the full game history.

   Runs on deploy to backfill, and any time K, tau, or the rating period
   changes. Safe to run repeatedly — it replays from scratch either way.

     lein run -m organism.scripts.rate-players"
  (:require
   [organism.handler :as handler]
   [organism.leaderboard :as leaderboard]
   [organism.mongo :as db]
   [organism.routes.organism-bot]))

(defn -main
  [& _args]
  (let [db (db/connect! handler/mongo-connection)
        {:keys [games players]} (leaderboard/rate-all! db)]
    (println "rated" players "players over" games "games")
    (doseq [[player {:keys [elo glicko rd games wins]}]
            (sort-by (comp - :glicko val) (leaderboard/load-ratings db))]
      (println
       (format "%-24s elo %7.1f   glicko %7.1f ±%-6.1f  %3d games  %3d wins"
               player (double elo) (double glicko) (double rd) (int games) (int wins))))
    (shutdown-agents)))
