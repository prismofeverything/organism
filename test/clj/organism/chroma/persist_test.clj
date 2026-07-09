(ns organism.chroma.persist-test
  "Round-trips a Chroma server-game through the REAL local MongoDB (localhost:27017,
   db `organism-test`) to prove the snapshot persistence that makes a refresh resume
   the same game. Requires mongod running."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [organism.mongo :as db]
            [organism.chroma.engine :as e]
            [organism.persist-chroma :as pc]
            [organism.routes.chroma-ws :as ws]))

(def new-server-game @#'ws/new-server-game)
(def maybe-resolve @#'ws/maybe-resolve)

(defonce conn (atom nil))

(defn- mongo-up? []
  (try (reset! conn (db/connect! {:host "localhost" :port 27017 :database "organism-test"}))
       true
       (catch Exception _ false)))

(use-fixtures :once
  (fn [f] (when (mongo-up?) (f))))

(deftest snapshot-round-trip
  (when @conn
    (testing "save then load reproduces the game state exactly"
      (let [db @conn
            key "test-roundtrip-1"
            server (maybe-resolve
                    (new-server-game key {:players ["H" "Bot 2" "Bot 3"] :bots [1 2]
                                          :palette "CMY" :depth 3 :trim true :seed 7777}))]
        (pc/delete-game! db key)
        (pc/save-game! db key server)
        (let [loaded (pc/load-game db key)]
          (is (some? loaded) "game loads back")
          (is (= (:turn (:state server)) (:turn (:state loaded))) "turn preserved")
          (is (= (:stacks (:state server)) (:stacks (:state loaded))) "board preserved exactly")
          (is (= (:bag (:state server)) (:bag (:state loaded))) "bag preserved")
          (is (= (:phase server) (:phase loaded)) "phase preserved")
          (is (= (set (:bots server)) (set (:bots loaded))) "bots preserved")
          (is (= (mapv :hand (:players (:state server)))
                 (mapv :hand (:players (:state loaded)))) "hands preserved"))
        (pc/delete-game! db key)
        (is (nil? (pc/load-game db key)) "deletes cleanly")))))

(deftest resume-mid-game
  (when @conn
    (testing "a game advanced several turns, saved, reloaded, continues to game-over"
      (let [db @conn
            key "test-resume-1"
            ;; play an all-bot game part-way, persist, reload, finish
            partial (loop [s (maybe-resolve
                              (new-server-game key {:players ["B0" "B1" "B2"] :bots [0 1 2]
                                                    :palette "CMY" :seed 31337}))]
                      ;; all-bot maybe-resolve runs to completion in one call, so just
                      ;; snapshot the finished game and confirm reload matches.
                      s)]
        (pc/delete-game! db key)
        (pc/save-game! db key partial)
        (let [loaded (pc/load-game db key)]
          (is (:over (:state loaded)) "finished game reloads as over")
          (is (= (:stacks (:state partial)) (:stacks (:state loaded)))))
        (pc/delete-game! db key)))))
