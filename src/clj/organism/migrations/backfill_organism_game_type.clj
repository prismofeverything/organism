(ns organism.migrations.backfill-organism-game-type
  "Legacy organism games and player-game records predate the :game-type field,
   so game-type filters (e.g. the players/stats page) no longer see them.

   Two facts make the backfill safe:
     - The :games collection is organism-only (organism is the original game;
       journey/oroboros/eridu/future each use their own game collections).
     - Every untyped player-games record is a legacy organism game — the other
       games were added later and always stamped :game-type.

   This stamps :game-type \"organism\" on every untyped document in :games and
   in the per-player player-games-* collections. Idempotent: re-running only
   touches documents that are still untyped."
  (:require
   [clojure.string :as str]
   [organism.mongo :as db]
   [organism.handler :as handler]))

(def missing-type {:game-type {:$exists false}})

(defn migrate!
  [db]
  ;; 1) :games is organism-only → tag every untyped game.
  (let [before (db/number db :games missing-type)]
    (println "games: tagging" before "untyped game(s) as organism")
    (when (pos? before)
      (db/merge-all! db :games missing-type {:game-type "organism"}))
    (println "games: remaining untyped =" (db/number db :games missing-type)))
  ;; 2) player-games-* collections: untyped records are legacy organism.
  (let [colls (filter #(str/starts-with? % "player-games-") (db/collections db))
        tagged (atom 0)]
    (println "scanning" (count colls) "player-games collection(s)")
    (doseq [coll colls]
      (let [n (db/number db coll missing-type)]
        (when (pos? n)
          (swap! tagged + n)
          (db/merge-all! db coll missing-type {:game-type "organism"}))))
    (println "player-games: tagged" @tagged "untyped record(s) as organism")))

(defn -main
  []
  (let [db (db/connect! handler/mongo-connection)]
    (println "backfilling organism :game-type on legacy documents")
    (migrate! db)
    (println "done")))
