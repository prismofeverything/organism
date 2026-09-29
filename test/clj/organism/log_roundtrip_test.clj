(ns organism.log-roundtrip-test
  "Every position the game is in now comes back out of the database, so a
   position must survive the trip unchanged -- exactly, not approximately.
   One field did not: the stage marker came back a string, and a game read
   from its log looped between resolving conflicts and checking integrity."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.board :as board]
   [organism.choice :as choice]
   [organism.game :as game]
   [organism.game-log :as game-log]
   [organism.mongo :as db]))

(def ^:private test-connection
  {:host "localhost" :port 27017 :database "organism-log-test"})

(defn- positions
  "Real positions from a random FLOW game and a random ordinary one, every
   stage included."
  [mutations seed steps]
  (let [players ["a" "b"]
        starting (board/starting-spaces 5 2 players board/total-rings {})
        info (game/initial-players starting [5 5])
        rng (java.util.Random. seed)]
    (binding [*out* (java.io.StringWriter.)]
      (loop [g (game/create-game (board/player-symmetry 2) (vec (take 5 board/total-rings))
                                 info 3 false mutations)
             n 0
             seen []]
        (let [[_ choices] (choice/find-state g)
              seen (conj seen (game-log/position (:state g)))]
          (if (or (>= n steps) (empty? choices))
            seen
            (let [ks (sort-by pr-str (keys choices))]
              (recur (get choices (nth ks (.nextInt rng (count ks)))) (inc n) seen))))))))

(deftest a-position-comes-back-out-of-the-log-unchanged
  (let [conn (db/connect! test-connection)
        k "roundtrip"]
    (doseq [[label mutations] [["ordinary" {}] ["FLOW" {:FLOW {}}]]]
      (testing label
        (db/drop! conn (str "history-" k))
        (let [states (positions mutations 7 400)]
          (doseq [s states] (game-log/append! conn k s))
          (is (= (count states) (game-log/length conn k)))
          (is (= states (game-log/entries conn k)) "the whole log, entry for entry")
          (is (= (last states) (game-log/present conn k)))
          (is (= (take-last 5 states) (game-log/recent conn k 5)))
          (is (contains? (set (map (comp :advance :player-turn) (game-log/entries conn k)))
                         :check-integrity)
              "the stage marker comes back a keyword"))))
    (db/drop! conn (str "history-" k))))
