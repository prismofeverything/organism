(ns universe.seven-test
  "The best-five-of-seven table: two to you, five to the board, ranked by the
   seven-card chart that universe/hands7.py measured."
  (:require
   [clojure.data.json :as json]
   [clojure.math.combinatorics :as combo]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest testing is]]
   [universe.deck :as deck]
   [universe.holdem :as holdem]
   [universe.player :as player]))

(defn- seeded-deck [seed]
  (let [cards (java.util.ArrayList. ^java.util.Collection (vec deck/all-cards))]
    (java.util.Collections/shuffle cards (java.util.Random. seed))
    (vec cards)))

(defn- chips [state]
  (+ (reduce + 0 (map :stack (:players state))) (holdem/pot state)))

(def ^:private row-name
  "hands7.json names rows by poker pattern and suit; the chart by keyword."
  {["straight" "perfect"] :singularity})

(defn- chart-name [{:strs [numbers suit]}]
  (or (row-name [numbers suit])
      (let [group {"one pair" "dyad" "two pair" "split-tetrad" "three of a kind" "triad"
                   "full house" "split-pentad" "four of a kind" "tetrad"
                   "five of a kind" "pentad" "straight" "sequence"}]
        (keyword (str (when (not= "mixed" suit) (str suit "-")) (group numbers))))))

(deftest the-order-is-the-measured-one
  (testing "seven-order is hands7.json's order: the rows it saw, rarest held
            first, then the two it never saw kept"
    (let [data (json/read-str (slurp (io/file "universe/hands7.json")))]
      (is (= deck/seven-order
             (mapv chart-name (concat (get data "rows") (get data "impossible")))))))

  (testing "the colored and shaped hands keep their five-card order"
    (let [suited (fn [order] (filter #(re-find #"^(color|shape)-|^singularity" (name %)) order))
          five   (map :name (sort-by (comp - :strength) deck/chart))]
      (is (= (suited five) (suited deck/seven-order))))))

(defn- card [color shape number] (deck/card color shape number))

(deftest the-best-five-is-kept
  (testing "holding a dyad and a split tetrad, the dyad is kept -- it ranks
            above in seven cards"
    ;; numbers 1 1 2 2 3 4 4, colors and shapes cycled so no suit forms
    (let [cards [(card :purple :eye 1) (card :green :helix 1) (card :yellow :pyramid 2)
                 (card :purple :star 2) (card :green :eye 3) (card :yellow :helix 4)
                 (card :purple :pyramid 4)]]
      (is (= :dyad (:name (deck/seven-row cards))))
      (is (= 5 (count (deck/best-five cards))))))

  (testing "five cards are already a hand"
    (let [five (vec (take 5 (seeded-deck 1)))]
      (is (= (set five) (set (deck/best-five five)))))))

(deftest triad-and-split-tetrad-are-never-kept
  (let [kept (frequencies
              (for [seed (range 3000)]
                (:name (deck/seven-row (take 7 (seeded-deck seed))))))]
    (is (not (contains? kept :triad)))
    (is (not (contains? kept :split-tetrad)))
    (is (contains? kept :dyad) "while a plain pair is")))

(defn- passive
  "Check when free, call otherwise: the board always comes out."
  [state]
  (let [{:keys [check]} (holdem/legal-actions state)]
    (if check {:action :check} {:action :call})))

(deftest the-board-is-a-flop-a-turn-and-a-river
  (let [start (holdem/start-hand (holdem/create-game ["a" "b" "c"] {:seven? true})
                                 (seeded-deck 5))
        boards (loop [s start seen [] guard 0]
                 (if (or (holdem/hand-over? s) (> guard 200))
                   (conj seen (count (:board s)))
                   (recur (holdem/act s (:to-act s) (passive s))
                          (conj seen (count (:board s)))
                          (inc guard))))]
    (is (= [0 3 4 5] (distinct boards)))
    (testing "and the showdown reads everyone's best five"
      (let [s (loop [s start] (if (holdem/hand-over? s) s
                                  (recur (holdem/act s (:to-act s) (passive s)))))]
        (is (every? (comp :best :hand) (vals (get-in s [:result :hands]))))))))

(deftest the-three-card-table-is-unchanged
  (let [s (loop [s (holdem/start-hand (holdem/create-game ["a" "b"]) (seeded-deck 5))]
            (if (holdem/hand-over? s) s (recur (holdem/act s (:to-act s) (passive s)))))]
    (is (= 3 (count (:board s))))
    (is (not (:seven? s)))))

(defn- pick [state ^java.util.Random rng]
  (let [{:keys [check call min-raise-to max-raise-to]} (holdem/legal-actions state)
        options (cond-> [{:action :fold}]
                  check        (conj {:action :check})
                  (pos? call)  (conj {:action :call})
                  min-raise-to (conj {:action :raise
                                      :to (+ min-raise-to
                                             (.nextInt rng (inc (- max-raise-to min-raise-to))))}))]
    (nth options (.nextInt rng (count options)))))

(deftest chips-are-conserved-at-seven-cards
  (doseq [players [["a" "b"] ["a" "b" "c" "d"] ["a" "b" "c" "d" "e" "f" "g" "h" "i"]]
          seed    (range 4)]
    (let [rng (java.util.Random. seed)
          total (* 1000 (count players))]
      (loop [s (holdem/create-game players {:seven? true}) n 0]
        (when-not (or (holdem/game-over? s) (> n 200) (< (count (holdem/with-chips s)) 2))
          (let [s (loop [s (holdem/start-hand s (seeded-deck (.nextLong rng))) guard 0]
                    (if (or (holdem/hand-over? s) (> guard 500))
                      s
                      (recur (holdem/act s (:to-act s) (pick s rng)) (inc guard))))]
            (is (= total (chips s)) (str (count players) " players, seed " seed))
            (recur s (inc n))))))))

(deftest the-equity-players-play-a-seven-card-table
  (testing "HARUSPEX and the others deal the rest of a five-card board, not a
            three-card one -- they used to count a negative number of cards
            to come and crash, leaving the table waiting on them"
    (let [act (player/actor (get player/profiles "HARUSPEX") 200)]
      (doseq [seed (range 3)]
        (loop [s (holdem/start-hand (holdem/create-game ["a" "b" "c"] {:seven? true})
                                    (seeded-deck seed))
               guard 0]
          (when-not (or (holdem/hand-over? s) (> guard 200))
            (let [action (act s)]
              (is (map? action))
              (recur (holdem/act s (:to-act s) action) (inc guard))))))))
  (testing "and still value a seven-card deal as the best five of it"
    (let [e (player/equity [0 1] [2 3 4 5 6] 2 300 (java.util.Random. 1)
                           (holdem/create-game ["a" "b"] {:seven? true}))]
      (is (<= 0.0 e 1.0)))))

(deftest the-fast-score-is-value-seven
  (testing "seven-code orders any two five-card hands exactly as value-seven
            does, ties included -- it is the same ranking, packed into one number"
    (let [rng (java.util.Random. 42)
          hands (repeatedly 4000 #(let [d (seeded-deck (.nextLong rng))] (subvec d 0 5)))
          sign (fn [x] (cond (pos? x) 1 (neg? x) -1 :else 0))]
      (doseq [[a b] (partition 2 1 hands)]
        (is (= (sign (compare (deck/value-seven a) (deck/value-seven b)))
               (sign (compare (apply deck/seven-code a) (apply deck/seven-code b))))))))
  (testing "and every chart row, ties within it too"
    (let [rng (java.util.Random. 9)]
      (doseq [_ (range 3000)]
        (let [seven (subvec (seeded-deck (.nextLong rng)) 0 7)
              best (deck/best-five seven)]
          (is (= (deck/best-code seven) (apply deck/seven-code best)))
          (is (= (deck/value-seven best)
                 (reduce (fn [a b] (if (neg? (compare a b)) b a))
                         (for [five (combo/combinations seven 5)] (deck/value-seven five))))))))))

(deftest a-view-carries-the-last-few-hands
  (testing "mid-table, the rail gets the last hands and only those -- the
            whole history still waits for the end"
    (let [rng (java.util.Random. 3)
          state (loop [s (holdem/create-game ["a" "b" "c"] {:seven? true}) n 0]
                  (if (>= n 7)
                    s
                    (recur (loop [s (holdem/start-hand s (seeded-deck (.nextLong rng)))]
                             (if (holdem/hand-over? s) s
                                 (recur (holdem/act s (:to-act s) (passive s)))))
                           (inc n))))
          view (holdem/view state "a")]
      (is (nil? (:winner state)))
      (is (nil? (:history view)))
      (is (= holdem/recent-count (count (:recent view))))
      (is (= (map :hand (take-last holdem/recent-count (:history state)))
             (map :hand (:recent view)))))))
