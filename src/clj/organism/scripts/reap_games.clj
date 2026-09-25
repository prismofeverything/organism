(ns organism.scripts.reap-games
  "Remove games whose deletion grace period expired with no objection.

     lein run -m organism.scripts.reap-games --dry-run
     lein run -m organism.scripts.reap-games

   Run it dry first, and keep running it dry until the list looks right —
   deletion is not reversible.

   In production this runs nightly from cron, installed by `./deploy.sh ship`;
   `./deploy.sh reap` shows what the schedule is about to do. Nothing ran it for
   a long time and deleting a game therefore did nothing visible — the mark went
   on and stayed on. The box has no Leiningen, so the scheduled job goes through
   the uberjar instead:

     java -cp organism.jar clojure.main -m organism.scripts.reap-games"
  (:require
   [organism.handler :as handler]
   [organism.mongo :as db]
   [organism.reap :as reap]))

(defn -main
  [& args]
  (let [dry-run? (boolean (some #{"--dry-run" "-n"} args))
        connection (db/connect! handler/mongo-connection)
        {:keys [deleted-count kept-count]} (reap/sweep! connection {:dry-run? dry-run?})]
    (println (if dry-run?
               (str "DRY RUN - would delete " deleted-count " game(s)")
               (str "deleted " deleted-count " game(s)")))
    (when (pos? kept-count)
      (println "cleared" kept-count "stale mark(s) on games that moved again"))
    (System/exit 0)))
