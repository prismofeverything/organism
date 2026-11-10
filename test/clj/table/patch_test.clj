(ns table.patch-test
  (:require [clojure.test :refer [deftest is testing]]
            [table.patch :as p]))

(def board {:id "b" :kind "board" :pos [0 1.5 0] :rot [0 0 0 1] :size [500 3 500]
            :strokes [{:tool :pen :pts [[0.1 0.1]]}]})
(def cube {:id "c" :kind "shape" :shape "cube" :pos [10 8 10] :rot [0 0 0 1] :color "#f00"})
(def table {"b" board "c" cube})

(defn- round-trip [a b]
  (is (= b (p/apply-patch a (p/diff a b)))))

(deftest diff-picks-the-smallest-change
  (testing "a move is only a move"
    (let [b (assoc-in table ["c" :pos] [20 8 20])]
      (is (= {:move {"c" [[20 8 20] [0 0 0 1]]}} (p/diff table b)))
      (round-trip table b)))
  (testing "drawing appends; undoing it trims"
    (let [s {:tool :line :pts [[0 0] [1 1]]}
          b (update-in table ["b" :strokes] conj s)]
      (is (= {:stroke {"b" [s]}} (p/diff table b)))
      (is (= {:trim {"b" 1}} (p/diff b table)))
      (round-trip table b)
      (round-trip b table)))
  (testing "clearing a drawing"
    (round-trip table (update table "b" dissoc :strokes)))
  (testing "anything else puts the object"
    (let [b (assoc-in table ["c" :color] "#00f")]
      (is (= {:put {"c" (get b "c")}} (p/diff table b)))
      (round-trip table b)))
  (testing "added and removed"
    (round-trip table (-> table (dissoc "c") (assoc "d" (assoc cube :id "d")))))
  (testing "nothing changed"
    (is (p/empty-patch? (p/diff table table)))))

(deftest stale-patches-are-harmless
  (is (= {"b" board}
         (p/apply-patch {"b" board} {:move {"c" [[0 0 0] [0 0 0 1]]} :stroke {"c" []} :trim {"c" 0}}))))

(deftest only-and-split
  (let [patch {:move {"c" [[1 1 1] [0 0 0 1]]} :put {"b" board} :remove #{"x"}}]
    (is (= {:move {"c" [[1 1 1] [0 0 0 1]]}} (p/only patch #{"c"})))
    (is (= #{"b" "c" "x"} (p/ids patch)))
    (let [parts (p/split patch 60)]
      (is (> (count parts) 1))
      (is (= (p/apply-patch table patch)
             (reduce p/apply-patch table parts))))))
