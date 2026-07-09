(ns organism.chroma.leaderboard-test
  "Leaderboard aggregation over the completed-game ARCHIVE in live MongoDB, and the
   key fix: two finished games by the same player (same play-key) accumulate instead
   of overwriting each other."
  (:require [clojure.test :refer [deftest is testing]]
            [organism.mongo :as db]
            [organism.routes.chroma-ws :as ws]
            [organism.routes.chroma :as chroma]
            [organism.persist-chroma :as pc]
            [organism.chroma.engine :as e]))

(def new-server-game @#'ws/new-server-game)
(def maybe-resolve @#'ws/maybe-resolve)

(defn- mongo []
  (try (db/connect! {:host "localhost" :port 27017 :database "organism-test"})
       (catch Exception _ nil)))

(defn- play-to-end
  "Drive a fresh human(seat 0)+bots game to game-over by feeding seat 0 a legal
   placement each :place step and skipping each :swap step."
  [server0]
  (loop [server (maybe-resolve server0), guard 0]
    (let [G (:state server)]
      (cond
        (:over G) server
        (> guard 300) server
        :else
        (recur
         (e/with-config G
           (case (:phase server)
             :place (let [moves (e/enumerate-moves G 0)
                          entry (if (seq moves)
                                  (let [m (first moves)] {:c (:c m) :chit (:chit m) :k (:k m)})
                                  :pass)]
                      (maybe-resolve (assoc-in server [:pending-placements 0] entry)))
             :swap (maybe-resolve (assoc-in server [:pending-swaps 0] :skip))
             server))
         (inc guard))))))

(deftest completed-games-accumulate-not-overwrite
  (when-let [db (mongo)]
    (testing "two finished games by the same player both land in the leaderboard"
      (let [players ["AdaLB" "Bot 2" "Bot 3"]
            play-key "adalb"]
        ;; clean any prior archive rows for this player set
        (db/delete! db :chroma-completed {:players (pr-str players)})
        (pc/delete-game! db play-key)
        ;; game 1
        (let [g1 (play-to-end (new-server-game play-key {:players players :bots [1 2] :seed 101}))]
          (is (:over (:state g1)) "game 1 finished")
          (pc/save-game! db play-key g1)
          (pc/archive-completed! db g1)
          ;; game 2 — SAME play-key (would overwrite the live doc), different :game-id
          (let [g2 (play-to-end (new-server-game play-key {:players players :bots [1 2] :seed 202}))]
            (is (:over (:state g2)) "game 2 finished")
            (is (not= (:game-id g1) (:game-id g2)) "each game has a distinct id")
            (pc/save-game! db play-key g2)
            (pc/archive-completed! db g2)))
        ;; the live doc was overwritten (1), but the archive kept BOTH
        (let [data (chroma/leaderboard-data db)
              ada (first (filter #(= "AdaLB" (:player %)) (:aggregate data)))]
          (is (some? ada) "AdaLB present in standings")
          (is (= 2 (:games ada)) "BOTH finished games counted (no overwrite)")
          (is (false? (:bot ada)) "human flagged")
          (is (number? (:avg-points ada)))
          (is (>= (count (filter #(= "AdaLB" (:player %)) (:hall-of-fame data))) 2)
              "both games appear in hall of fame"))
        ;; cleanup
        (db/delete! db :chroma-completed {:players (pr-str players)})
        (pc/delete-game! db play-key)))))
