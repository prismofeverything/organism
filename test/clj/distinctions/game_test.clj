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

(defn- play-out [seed n-players & [opts]]
  (loop [s (game/start (game/create-game (map str (range n-players)) opts) (seeded-deck seed))
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

(deftest solo-turns-a-fresh-card
  (let [s  (game/start (game/create-game ["me"]) (seeded-deck 7))
        s1 (game/act s 0 {:action :draw})
        c  (first (remove #{(:taken s1)} (get-in s1 [:hands 0])))
        s2 (game/act s1 0 {:action :discard :card c})]
    (when-not (game/game-over? s2)
      (is (not= c (game/top-discard s2)) "the pile shows a new card, not the one thrown")
      (is (= c (peek (pop (:discard s2)))) "the thrown card is under it")
      (is (= (dec (count (:deck s1))) (count (:deck s2)))))))

(deftest the-path
  (testing "white/red circle to its opposite, one change at a time"
    ;; 32 = white red circle; flip comp, eye, rays, inversion, foreground, background
    (let [run [32 40 44 46 47 63 31]
          {:keys [cards steps]} (game/path (shuffle run))]
      (is (= 31 (bit-xor 32 63)))
      (is (#{run (vec (reverse run))} cards))
      (is (= 6 (count (distinct steps))))))
  (testing "a step of two changes breaks it"
    (is (nil? (game/path [32 40 46 47 63 31 0]))))
  (testing "a card without its opposite is no path"
    (is (nil? (game/path [32 40 44 46 47 63 15]))))
  (testing "progress counts a whole path as seven"
    (is (= 7 (:length (game/path-progress [32 40 44 46 47 63 31])))))
  (testing "a path table deals seven"
    (is (= 7 (:hand-size (game/create-game ["a"] {:goal :path})))))
  (doseq [seed (range 25) n [1 2 3]]
    (let [s    (play-out seed n {:goal :path})
          hand (get-in s [:hands (game/seat-of s (:winner s))])]
      (is (game/game-over? s) (str "path seed " seed " with " n))
      (is (= 7 (count hand)))
      (is (= (:made s) (game/path hand))))))
