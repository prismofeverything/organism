(ns universe.player-test
  "The opponents that work out what a hand is worth.

   ORACLE reads its own five cards and stops, which leaves out both things that
   decide a hand of hold'em: whether it is ahead of the cards it cannot see, and
   whether the price is worth paying. These tests pin the two pieces that fix
   that — equity and pot odds — and then check the whole thing where it counts,
   by taking ORACLE's chips."
  (:require
   [clojure.test :refer [deftest testing is]]
   [universe.deck :as deck]
   [universe.holdem :as holdem]
   [universe.player :as player]))

(defn- rng [seed] (java.util.Random. seed))

(deftest equity-knows-what-a-hand-is-worth
  (testing "with nobody to beat, the hand is worth the whole pot"
    (is (= 1.0 (player/equity [0 1] [] 0 200 (rng 1)))))

  (testing "a made hand on a finished board beats a weak one against the same field"
    ;; Cards are colour*20 + shape*5 + number. Three of one number is a real
    ;; hand; three unrelated cards are not.
    (let [board [0 20 40]                         ; number 1 in all three colours
          strong [5 25]                           ; number 1 again, twice over
          weak [9 33]
          s (player/equity strong board 2 400 (rng 2))
          w (player/equity weak board 2 400 (rng 2))]
      (is (> s w) (str "strong " s " should beat weak " w))))

  (testing "more opponents means less of the pot, for the same cards"
    (let [board [0 20 40] hole [5 25]
          one (player/equity hole board 1 400 (rng 3))
          three (player/equity hole board 3 400 (rng 3))]
      (is (>= one three))))

  (testing "equity is a share, so it stays inside its bounds"
    (doseq [n [1 2 3]]
      (let [e (player/equity [7 19] [] n 200 (rng 4))]
        (is (<= 0.0 e 1.0))))))

(deftest price-is-the-share-a-call-must-win
  (testing "a free look costs nothing"
    (is (= 0.0 (player/price 100 0))))
  (testing "calling half the pot has to win a third of the time"
    (is (< 0.33 (player/price 100 50) 0.34)))
  (testing "matching the pot has to win half"
    (is (= 0.5 (player/price 100 100)))))

(defn- oracle
  "The bot that was on the site, copied so the two play exactly as they would."
  [state]
  (let [{:keys [check call min-raise-to max-raise-to]} (holdem/legal-actions state)
        seat (:to-act state)
        cards (concat (get-in state [:hands seat]) (:board state))
        heat (if (= 5 (count cards))
               (/ (:strength (deck/classify cards)) 17.0)
               (let [biggest (apply max (vals (frequencies (map deck/number cards))))]
                 (min 0.85 (* 0.22 biggest))))
        pot (holdem/pot state)
        price (if (pos? pot) (/ (double call) pot) 0.0)]
    (cond
      (and min-raise-to (> heat 0.6) (< (rand) 0.5))
      {:action :raise :to (min max-raise-to (max min-raise-to (quot (* 2 pot) 3)))}
      (zero? call) (if check {:action :check} {:action :call})
      (< price heat) {:action :call}
      :else {:action :fold})))

(defn- table
  "One table. `actors` is {seat step-fn}. Returns the final stacks."
  [seed actors hands]
  (let [shared (rng seed)
        shuffled #(let [v (java.util.ArrayList. ^java.util.Collection deck/all-cards)]
                    (java.util.Collections/shuffle v shared)
                    (vec v))]
    (loop [s (holdem/start-hand (holdem/create-game ["A" "B"]) (shuffled))
           played 0 guard 0]
      (if (or (>= played hands) (holdem/game-over? s) (> guard 20000)
              (< (count (holdem/with-chips s)) 2))
        (mapv #(holdem/stack s %) (range (holdem/seat-count s)))
        (if (holdem/current-player s)
          (let [seat (:to-act s)]
            (recur (holdem/act s seat ((get actors seat) s)) played (inc guard)))
          (recur (holdem/start-hand s (shuffled)) (inc played) (inc guard)))))))

(deftest the-new-players-take-oracles-chips
  (testing "each profile finishes ahead of ORACLE across alternating seats"
    ;; Short tables so the suite stays quick; the seats alternate so blinds and
    ;; position cannot be what decides it.
    (doseq [[label profile] player/profiles]
      (let [net (reduce
                 +
                 (for [seed (range 6)]
                   (let [swap? (odd? seed)
                         bot (player/actor profile 200)
                         actors (if swap? {0 oracle 1 bot} {0 bot 1 oracle})
                         stacks (table (+ 4400 seed) actors 60)]
                     (- (nth stacks (if swap? 1 0))
                        (nth stacks (if swap? 0 1))))))]
        (is (pos? net) (str label " finished " net " chips against ORACLE"))))))
