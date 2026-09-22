(ns universe.deck-test
  "The chart is counted, not argued.  The headline test re-enumerates every
   one of the 5,461,512 five-card hands and checks this namespace's classifier
   lands each one in the row universe/hands.py counted it into."
  (:require
   [clojure.test :refer [deftest testing is]]
   [clojure.math.combinatorics :as combo]
   [universe.deck :as deck]))

(deftest chart-is-complete
  (testing "nineteen rows summing to C(60,5)"
    (is (= 19 (count deck/chart)))
    (is (= deck/total-hands (reduce + (map :count deck/chart)))))
  (testing "eighteen strengths -- the chart ties in exactly one place"
    (is (= 18 (count (distinct (map :strength deck/chart)))))
    (let [tied (->> deck/chart (group-by :strength) vals (filter #(> (count %) 1)))]
      (is (= 1 (count tied)))
      (is (= #{:color-tetrad :shape-split-pentad} (set (map :name (first tied)))))
      (is (apply = (map :count (first tied))))))
  (testing "rarer hands are stronger, everywhere"
    (doseq [[a b] (partition 2 1 (sort-by :count deck/chart))]
      (is (>= (:strength a) (:strength b))
          (str (:name a) " is rarer than " (:name b) " but not stronger")))))

(deftest card-indices-round-trip
  (testing "every card is its own index into the lattice"
    (doseq [c deck/all-cards]
      (is (= c (deck/card (deck/color c) (deck/shape c) (deck/number c))))))
  (testing "the lattice is complete -- each combination exactly once"
    (is (= 60 (count (distinct (map (juxt deck/color deck/shape deck/number)
                                    deck/all-cards)))))))

(deftest classifies-every-hand
  (testing "re-enumerating C(60,5) reproduces the chart exactly"
    (let [counted (->> (combo/combinations deck/all-cards 5)
                       (reduce (fn [acc hand]
                                 (update acc (:name (deck/classify hand)) (fnil inc 0)))
                               {}))]
      (is (= deck/total-hands (reduce + (vals counted)))
          "every hand landed in some row")
      (is (= (count deck/chart) (count counted))
          "every row was reached")
      (doseq [{:keys [name count]} deck/chart]
        (is (= count (get counted name))
            (str name " counted " (get counted name) ", chart says " count))))))

;; ── Comparison ─────────────────────────────────────────────────────────────

(defn- hand [& specs]
  (mapv (fn [[c s n]] (deck/card c s n)) specs))

(deftest the-rarest-hand
  (testing "five of one suit is the whole run, and the only singularity"
    (let [h (hand [:purple :eye 1] [:purple :eye 2] [:purple :eye 3]
                  [:purple :eye 4] [:purple :eye 5])]
      (is (= :singularity (:name (deck/classify h))))
      (is (= 17 (:strength (deck/classify h)))))))

(deftest numbers-break-ties-before-colors
  (testing "a higher group wins, whatever the colors"
    (let [fives (hand [:yellow :eye 5] [:yellow :helix 5] [:green :eye 1]
                      [:green :helix 2] [:green :star 3])
          twos  (hand [:purple :eye 2] [:purple :helix 2] [:purple :star 1]
                      [:green :eye 3] [:yellow :star 4])]
      (is (= :dyad (:name (deck/classify fives))))
      (is (= :dyad (:name (deck/classify twos))))
      (is (pos? (deck/compare-hands fives twos))
          "a pair of fives beats a pair of twos even in the weaker colors")))
  (testing "group size is read before the number"
    (let [trip-ones (hand [:purple :eye 1] [:green :eye 1] [:yellow :eye 1]
                          [:purple :helix 2] [:green :star 3])
          pair-fives (hand [:purple :eye 5] [:green :eye 5] [:yellow :helix 1]
                           [:purple :star 2] [:green :pyramid 3])]
      (is (pos? (deck/compare-hands trip-ones pair-fives))))))

(deftest colors-break-what-numbers-cannot
  (testing "identical numbers, purple beats yellow"
    (let [purple (hand [:purple :eye 3] [:purple :helix 3] [:purple :star 1]
                       [:purple :pyramid 2] [:green :eye 4])
          yellow (hand [:yellow :eye 3] [:yellow :helix 3] [:yellow :star 1]
                       [:yellow :pyramid 2] [:green :helix 4])]
      (is (= (deck/number-groups purple) (deck/number-groups yellow)))
      (is (pos? (deck/compare-hands purple yellow)))))
  (testing "shape never breaks a tie -- same numbers, same colors, split"
    (let [a (hand [:purple :eye 3] [:green :eye 3] [:yellow :eye 1]
                  [:purple :helix 2] [:green :star 4])
          b (hand [:purple :star 3] [:green :star 3] [:yellow :star 1]
                  [:purple :pyramid 2] [:green :eye 4])]
      (is (zero? (deck/compare-hands a b)))
      (is (= [0 1] (deck/winners [a b])) "both win, so the pot splits"))))

(deftest winners-picks-every-best-hand
  (let [strong (hand [:purple :eye 5] [:green :eye 5] [:yellow :eye 5]
                     [:purple :helix 5] [:green :star 1])
        weak   (hand [:purple :eye 1] [:green :helix 1] [:yellow :star 2]
                     [:purple :pyramid 3] [:green :eye 4])]
    (is (= [0] (deck/winners [strong weak])))
    (is (= [1] (deck/winners [weak strong])))
    (is (= [] (deck/winners [])))))
