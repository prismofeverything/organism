(ns organism.rating-test
  (:require
   [clojure.test :refer :all]
   [organism.rating :as rating]))

(defn close?
  ([a b] (close? a b 0.01))
  ([a b tolerance] (< (Math/abs (- (double a) (double b))) tolerance)))

;; ── Pair decomposition ──────────────────────────────────────────────────────

(deftest pairings-test
  (testing "heads up is a single full-weight pairing each way"
    (let [pairs (rating/pairings ["a" "b"] "a")]
      (is (= [{:opponent "b" :score 1.0 :weight 1.0}] (get pairs "a")))
      (is (= [{:opponent "a" :score 0.0 :weight 1.0}] (get pairs "b")))))

  (testing "losers draw with each other — the game records no order among them"
    (let [pairs (rating/pairings ["a" "b" "c" "d"] "a")]
      (is (= [1.0 1.0 1.0] (mapv :score (get pairs "a"))))
      (is (= #{0.0 0.5} (set (map :score (get pairs "b")))))
      (is (= 0.0 (:score (first (filter #(= "a" (:opponent %)) (get pairs "b"))))))))

  (testing "every player's pairings weigh one game, however crowded the table"
    (doseq [n (range 2 7)]
      (let [players (mapv str (range n))
            pairs (rating/pairings players "0")]
        (doseq [[_ opponents] pairs]
          (is (close? 1.0 (reduce + (map :weight opponents)))))))))

(deftest rateable-test
  (is (rating/rateable? ["a" "b"] "a"))
  (is (not (rating/rateable? ["a" "b"] nil)) "abandoned games teach nothing")
  (is (not (rating/rateable? ["a"] "a")) "solitaire has no opponent")
  (is (not (rating/rateable? ["a" "b"] "c")) "winner must be at the table"))

;; ── Elo ─────────────────────────────────────────────────────────────────────

(deftest expected-test
  (is (close? 0.5 (rating/expected 1500 1500)))
  (is (close? 0.909 (rating/expected 1900 1500) 0.001) "400 points is 10:1")
  (is (close? 0.24 (rating/expected 1500 1700))))

(deftest elo-heads-up-test
  (testing "even match, fresh players: the full K/2 changes hands"
    (let [deltas (rating/elo-deltas {} ["a" "b"] "a")]
      (is (close? 16.0 (get deltas "a")))
      (is (close? -16.0 (get deltas "b"))))))

(deftest elo-multiplayer-test
  (testing "a three player upset, worked by hand"
    (let [ratings {"a" (assoc rating/fresh :elo 1500.0)
                   "b" (assoc rating/fresh :elo 1700.0)
                   "c" (assoc rating/fresh :elo 1400.0)}
          deltas (rating/elo-deltas ratings ["a" "b" "c"] "a")]
      (is (close? 17.9 (get deltas "a") 0.1) "beat the favorite")
      (is (close? -17.7 (get deltas "b") 0.1) "the favorite pays for it")
      (is (close? -0.2 (get deltas "c") 0.1)
          "the underdog barely moves — they drew with the favorite")))

  (testing "a win is a win: table size does not change what the winner earns"
    (doseq [n (range 2 7)]
      (let [players (mapv str (range n))
            deltas (rating/elo-deltas {} players "0")]
        (is (close? 16.0 (get deltas "0"))
            (str n " players")))))

  (testing "points only move between the players at the table"
    (doseq [n (range 2 7)]
      (let [deltas (rating/elo-deltas {} (mapv str (range n)) "0")]
        (is (close? 0.0 (reduce + (vals deltas)) 0.0001))))))

(deftest k-factor-test
  (is (= 32.0 (rating/k-factor 0)))
  (is (= 32.0 (rating/k-factor 4)))
  (is (= 16.0 (rating/k-factor 5)) "settles down once established"))

;; ── Glicko-2 ────────────────────────────────────────────────────────────────

(deftest glicko-published-example-test
  (testing "Glickman's worked example (glicko.net/glicko/glicko2.pdf, p.3)"
    (let [player {:glicko 1500.0 :rd 200.0 :volatility 0.06}
          results [{:rating 1400.0 :rd 30.0  :score 1.0 :weight 1.0}
                   {:rating 1550.0 :rd 100.0 :score 0.0 :weight 1.0}
                   {:rating 1700.0 :rd 300.0 :score 0.0 :weight 1.0}]
          {:keys [glicko rd volatility]} (rating/glicko-update player results)]
      (is (close? 1464.06 glicko 0.01))
      (is (close? 151.52 rd 0.01))
      (is (close? 0.05999 volatility 0.00001)))))

(deftest glicko-idle-test
  (testing "sitting out a period only costs certainty, never rating"
    (let [player {:glicko 1600.0 :rd 80.0 :volatility 0.06}
          {:keys [glicko rd]} (rating/glicko-update player [])]
      (is (close? 1600.0 glicko) "rating is untouched")
      (is (> rd 80.0) "but we know less than we did")))

  (testing "uncertainty grows monotonically while away, but never past unknown"
    (let [away (reductions
                (fn [player _] (rating/glicko-update player []))
                {:glicko 1600.0 :rd 80.0 :volatility 0.06}
                (range 400))
          deviations (map :rd away)]
      (is (apply <= deviations) "each idle period widens it")
      (is (every? #(<= % rating/initial-rd) deviations)
          "an absent player is never less known than a brand new one"))))

(deftest glicko-certainty-test
  (testing "playing narrows the deviation"
    (let [after (rating/glicko-update
                 {:glicko 1500.0 :rd 350.0 :volatility 0.06}
                 [{:rating 1500.0 :rd 50.0 :score 1.0 :weight 1.0}])]
      (is (< (:rd after) 350.0))
      (is (> (:glicko after) 1500.0))))

  (testing "a settled player moves less than a newcomer on the same result"
    (let [newcomer (rating/glicko-update
                    {:glicko 1500.0 :rd 350.0 :volatility 0.06}
                    [{:rating 1500.0 :rd 50.0 :score 1.0 :weight 1.0}])
          settled (rating/glicko-update
                   {:glicko 1500.0 :rd 50.0 :volatility 0.06}
                   [{:rating 1500.0 :rd 50.0 :score 1.0 :weight 1.0}])]
      (is (> (- (:glicko newcomer) 1500.0)
             (- (:glicko settled) 1500.0))))))

;; ── Replay ──────────────────────────────────────────────────────────────────

(deftest replay-test
  (testing "the consistent winner rises in both systems"
    (let [games (for [i (range 12)]
                  {:players ["ada" "bo" "cy"] :winner "ada" :period (quot i 4)})
          final (rating/replay games)]
      (is (> (get-in final ["ada" :elo]) 1600.0))
      (is (> (get-in final ["ada" :glicko]) 1600.0))
      (is (< (get-in final ["bo" :elo]) 1500.0))
      (is (= 12 (get-in final ["ada" :games])))
      (is (= 12 (get-in final ["ada" :wins])))
      (is (= 0 (get-in final ["bo" :wins])))
      (is (< (get-in final ["ada" :rd]) 200.0)
          "twelve games narrows the deviation well below a newcomer's 350")))

  (testing "Elo is conserved across a closed pool"
    (let [games [{:players ["a" "b"] :winner "a" :period 0}
                 {:players ["b" "c"] :winner "b" :period 0}
                 {:players ["a" "c" "d"] :winner "c" :period 1}]
          final (rating/replay games)
          total (reduce + (map :elo (vals final)))]
      (is (close? (* 4 rating/initial-rating) total 0.001))))

  (testing "a player who stops playing keeps their rating but loses certainty"
    (let [games (concat
                 [{:players ["quit" "stay"] :winner "quit" :period 0}]
                 (for [i (range 1 30)]
                   {:players ["stay" "other"] :winner "stay" :period i}))
          final (rating/replay games)]
      (is (> (get-in final ["quit" :glicko]) 1500.0) "still rated above average")
      (is (> (get-in final ["quit" :rd]) (get-in final ["stay" :rd]))
          "but far less certain than the player who kept showing up"))))
