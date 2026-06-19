(ns organism.chroma.leaderboard-test
  "Verifies leaderboard aggregation over completed games in live MongoDB."
  (:require [clojure.test :refer [deftest is testing]]
            [organism.mongo :as db]
            [organism.routes.chroma-ws :as ws]
            [organism.routes.chroma :as chroma]
            [organism.persist-chroma :as pc]))

(def new-server-game @#'ws/new-server-game)
(def maybe-resolve @#'ws/maybe-resolve)

(defn- mongo []
  (try (db/connect! {:host "localhost" :port 27017 :database "organism-test"})
       (catch Exception _ nil)))

(deftest leaderboard-aggregates-completed-games
  (when-let [db (mongo)]
    (testing "finished games produce per-player standings + hall of fame"
      (let [keys ["lb-game-1" "lb-game-2"]]
        (doseq [k keys] (pc/delete-game! db k))
        ;; two completed all-bot games (maybe-resolve runs all-bot games to the end)
        (doseq [[k seed] [["lb-game-1" 111] ["lb-game-2" 222]]]
          (let [server (maybe-resolve
                        (new-server-game k {:players ["Ada" "Bot 2" "Bot 3"] :bots [1 2]
                                            :palette "CMY" :seed seed}))]
            (is (:over (:state server)) "game finished")
            (pc/save-game! db k server)))
        (let [data (chroma/leaderboard-data db)
              agg (:aggregate data)
              hall (:hall-of-fame data)
              ada (first (filter #(= "Ada" (:player %)) agg))]
          (is (seq agg) "aggregate has rows")
          (is (some? ada) "human player Ada present")
          (is (= 2 (:games ada)) "Ada played 2 games")
          (is (false? (:bot ada)) "Ada flagged human")
          (is (<= 0 (:wins ada) 2))
          (is (number? (:avg-points ada)))
          (is (seq hall) "hall of fame populated")
          (is (every? #(contains? % :game-key-short) hall) "hall entries have short keys"))
        (doseq [k keys] (pc/delete-game! db k))))))
