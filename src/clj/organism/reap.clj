(ns organism.reap
  "Sweep away games whose deletion grace period ran out with nobody objecting.

   A mark is cleared by any move (persist/update-player-game!) and by any
   participant pressing keep, so a game only ever reaches the reaper when the
   whole table stayed silent for the entire window.

   The activity check here is a second belt. Bot turns write history through
   update-state! without touching the player-games records, so an all-bot game
   can keep moving without ever clearing its mark — the last history entry is
   the only signal that catches that."
  (:require
   [organism.mongo :as db]
   [organism.persist :as persist]))

(defn classify
  "Marked games whose deadline has passed, split by whether anything happened
   after the mark. {true [revived...] false [doomed...]}"
  [db now]
  (->> (db/query db :games {"deletion.deadline" {"$lt" now}})
       (map
        (fn [{:keys [key deletion] :as game}]
          (let [activity (persist/last-activity-at db key)
                marked-at (or (:marked-at deletion) 0)]
            {:key key
             :deletion deletion
             :players (get-in game [:invocation :players])
             :last-activity activity
             :revived? (boolean (and activity (> activity marked-at)))})))
       (group-by :revived?)))

(defn sweep!
  "Delete every expired game, and clear the mark on any that moved again since
   they were marked. With :dry-run? true nothing is written — run it that way
   first, because deletion does not come back."
  ([db] (sweep! db {}))
  ([db {:keys [dry-run? now]}]
   (let [now (or now (persist/now-seconds))
         {revived true doomed false} (classify db now)]
     (doseq [{:keys [key deletion]} doomed]
       (if dry-run?
         (println "would delete" key "- marked by" (:marked-by deletion))
         (do
           (println "reaping" key "- marked by" (:marked-by deletion))
           (persist/delete-game! db key))))
     (doseq [{:keys [key]} revived]
       (if dry-run?
         (println "would clear stale mark on" key "- it moved again")
         (persist/unmark-game-for-deletion! db key)))
     {:dry-run? (boolean dry-run?)
      :deleted-count (count doomed)
      :deleted (mapv :key doomed)
      :kept-count (count revived)
      :kept (mapv :key revived)})))
