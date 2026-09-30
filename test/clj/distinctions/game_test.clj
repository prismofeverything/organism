(ns distinctions.game-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [distinctions.bot :as bot]
   [distinctions.game :as game]))

(defn- seeded-deck [seed]
  (let [cards (java.util.ArrayList. ^java.util.Collection game/all-cards)]
    (java.util.Collections/shuffle cards (java.util.Random. seed))
    (vec cards)))

(deftest the-deck
  (testing "every one of the 64 answers the six questions differently"
    (is (= 64 (count (set (map game/attrs game/all-cards))))))
  (testing "each target is held by exactly half the deck"
    (is (every? #(= 32 %) (vals (game/target-counts game/all-cards))))))

(deftest dealing
  (let [s (game/start (game/create-game ["a" "b" "c"]) (seeded-deck 1))]
    (is (every? #(= 8 (count %)) (vals (:hands s))))
    (is (= 1 (count (:discard s))))
    (is (= (- 64 25) (count (:deck s))))
    (is (= 64 (count (set (concat (:deck s) (:discard s) (mapcat val (:hands s)))))))))

(deftest refusals
  (let [s (game/start (game/create-game ["a" "b"]) (seeded-deck 2))]
    (testing "out of turn"
      (is (identical? s (game/act s 1 {:action :draw}))))
    (testing "discarding before drawing"
      (is (identical? s (game/act s 0 {:action :discard :card (first (get-in s [:hands 0]))}))))
    (testing "throwing back what you just took"
      (let [t (game/act s 0 {:action :take})]
        (is (identical? t (game/act t 0 {:action :discard :card (:taken t)})))))))

(deftest the-view-hides-what-it-should
  (let [s (game/start (game/create-game ["a" "b"]) (seeded-deck 3))
        v (game/view s "b")]
    (is (= #{1} (set (keys (:hands v)))))
    (is (nil? (:deck v)))
    (is (= 47 (:deck-count v)))
    (is (nil? (:actions v)))
    (is (:actions (game/view s "a")))))

(defn- play-out [seed n-players]
  (loop [s (game/start (game/create-game (map str (range n-players))) (seeded-deck seed))
         i 0]
    (if (or (game/game-over? s) (> i 20000))
      s
      (let [cards (+ (count (:deck s)) (count (:discard s)) (reduce + (map count (vals (:hands s)))))]
        (assert (= 64 cards) (str "cards leaked at step " i))
        (recur (bot/step s 0.0) (inc i))))))

(deftest two-teams-of-four
  (testing "a team agreeing on background/foreground/composition and one on eye/rays/inversion"
    (let [[a b] (game/covered [0 1 2 3 13 21 29 37])]
      (is (= [0 1 2 3] (:cards a)))
      ;; these four also happen to agree on eye; three is the least, not the most
      (is (every? (set (map first (:shared a))) [:background :foreground :composition]))
      (is (= #{:eye :rays :inversion} (set (map first (:shared b)))))))
  (testing "one card off and it is not"
    (is (nil? (game/covered [0 1 2 3 13 21 29 38]))))
  (testing "a 3-cube is not: both halves agree on the same three"
    (is (nil? (game/covered [0 1 2 3 4 5 6 7]))))
  (testing "progress sees a finished hand as missing nothing"
    (is (zero? (:missing (game/cover-progress [0 1 2 3 13 21 29 37])))))
  (doseq [seed (range 60) n [1 2 3 4]]
    (let [s    (play-out seed n)
          hand (get-in s [:hands (game/seat-of s (:winner s))])]
      (is (game/game-over? s) (str "cover seed " seed " with " n))
      (is (= 8 (count hand)))
      (is (= (:made s) (game/covered hand))))))
