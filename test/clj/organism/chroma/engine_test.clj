(ns organism.chroma.engine-test
  (:require [clojure.test :refer [deftest is testing]]
            [organism.chroma.engine :as e]))

(def ^:const TOTAL (* e/PER_COLOR 6)) ; 180 palette chits in play, invariant

(defn- board-palette-count
  "Count palette-color chits on the board (the K center seed is excluded)."
  [G]
  (->> (vals (:stacks G)) (apply concat) (remove #(= % "K")) count))

(defn- hand-count [G]
  (reduce + (map #(count (:hand %)) (:players G))))

(defn- bag-count [G] (reduce + (vals (:bag G))))

(defn- discarded-count [G] (reduce + (map #(:discarded % 0) (:players G))))

(defn- ledger [G]
  (+ (bag-count G) (hand-count G) (board-palette-count G) (discarded-count G)))

(deftest geometry
  (is (= 91 (count e/cells)) "edge-6 hex board has 91 cells")
  (is (contains? e/cell-set [0 0]) "center present")
  (is (= 5 e/max-ring)))

(deftest seeding
  (e/with-config {:palette :CMY}
    (let [s (e/seed-map)]
      (is (= "K" (s [0 0])) "center seeded black")
      (is (pos? (count s)) "seed map non-empty")
      ;; every seeded palette chit was removed from the starting bag
      (let [g (e/new-game [{:isBot true} {:isBot true} {:isBot true}] {:seed 1})]
        (is (= TOTAL (ledger g)) "fresh game conserves 180 palette chits")))))

(deftest hand-deal
  (let [g (e/new-game [{:isBot true} {:isBot true} {:isBot true}] {:seed 7})]
    (is (every? #(<= (count (:hand %)) e/START_HAND) (:players g)) "no hand exceeds START_HAND")
    (is (= 3 (count (:players g))))
    (is (every? :target (:players g)) "every seat has a target")))

(deftest conservation-and-bounds
  (testing "a full autonomous game conserves chits and never overfills a hand"
    (doseq [seed [1 2 3 42 99 1000 123456]]
      (let [g0 (e/new-game [{:isBot true} {:isBot true} {:isBot true}]
                           {:seed seed :removed e/trim-cells})]
        (loop [g g0, n 0]
          (is (= TOTAL (ledger g)) (str "ledger holds at seed " seed " turn " (:turn g)))
          (is (every? #(<= (count (:hand %)) e/START_HAND) (:players g))
              (str "hand bound at seed " seed " turn " (:turn g)))
          (when (and (not (:over g)) (< n 200))
            (recur (e/step g) (inc n))))))))

(deftest no-replay-rule
  (testing "enumerate-moves never offers the color a player placed last turn"
    (let [g (e/new-game [{:isBot true} {:isBot true} {:isBot true}] {:seed 5})
          g (assoc-in g [:players 0 :lastPlaced] "C")]
      (e/with-config g
        (is (not-any? #(= (:chit %) "C") (e/enumerate-moves g 0))
            "C is filtered out as last-placed")))))

(deftest classify-basics
  (e/with-config {:palette :CMY}
    (is (= "white" (e/classify (e/mix-stack []))) "empty stack reads as white-ish (clear)")
    (is (= "C" (e/classify (e/mix-stack ["C"]))) "single cyan classifies as C")
    (is (= "M" (e/classify (e/mix-stack ["M"]))) "single magenta classifies as M")
    ;; a dark over-stack should drop to mud
    (is (= "mud" (e/classify (e/mix-stack ["C" "M" "Y"]))) "cyan+magenta+yellow stacks to mud")))

(deftest wheel-swaps
  (e/with-config {:palette :CMY}
    ;; CMY order: C B M R Y G ; opposite = +3
    (is (= "R" (e/wheel-opposite "C")))
    (is (= "C" (e/wheel-opposite "R")))
    ;; between two 2-apart colors
    (is (some? (e/wheel-between "C" "M")) "C and M are 2 apart -> a between color exists")
    (is (nil? (e/wheel-between "C" "B")) "adjacent colors have no clean between")))

(deftest scoring-shape
  (let [g (e/new-game [{:isBot true} {:isBot true} {:isBot true}] {:seed 11 :removed e/trim-cells})
        g (reduce (fn [g _] (if (:over g) g (e/step g))) g (range 60))
        rows (e/with-config g (e/score-game (:players g) (:stacks g)))]
    (is (= 3 (count rows)) "one score row per player")
    (is (every? #(contains? % :mult) rows))
    (is (every? #(>= (:mult %) 0) rows) "no negative scores")))
