(ns universe.network-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [universe.deck :as deck]
   [universe.network :as net]))

(defn- c [color shape number] (deck/card color shape number))

(defn- seeded-deck [seed]
  (let [cards (java.util.ArrayList. ^java.util.Collection deck/all-cards)]
    (java.util.Collections/shuffle cards (java.util.Random. seed))
    (vec cards)))

(deftest links
  (is (net/linked? (c :green :pyramid 5) (c :green :eye 5)) "shape differs")
  (is (net/linked? (c :green :pyramid 5) (c :yellow :pyramid 5)) "color differs")
  (is (net/linked? (c :green :pyramid 5) (c :green :pyramid 4)) "number differs by one")
  (is (not (net/linked? (c :green :pyramid 5) (c :green :pyramid 3))) "number differs by two")
  (is (not (net/linked? (c :green :pyramid 5) (c :yellow :eye 5))) "two differ")
  (is (not (net/linked? (c :green :pyramid 5) (c :green :pyramid 5))) "the same card"))

(def ten
  "A network of ten: up the numbers in green pyramids, across the shapes at
   5, across the colors at the star."
  [(c :green :pyramid 1) (c :green :pyramid 2) (c :green :pyramid 3) (c :green :pyramid 4)
   (c :green :pyramid 5) (c :green :eye 5) (c :green :helix 5) (c :green :star 5)
   (c :purple :star 5) (c :yellow :star 5)])

(deftest networks
  (is (net/spans? ten))
  (is (net/connected? ten))
  (is (= (sort ten) (net/network ten)))
  (testing "break a link and it is no network"
    (is (nil? (net/network (assoc ten 2 (c :purple :pyramid 3)))))))

(defn- dealt [n seed]
  (net/start (net/create-game (map str (range n))) (seeded-deck seed)))

(deftest a-turn
  (let [s    (dealt 2 1)
        hand (get-in s [:hands 0])]
    (is (every? #(= 10 (count %)) (vals (:hands s))) "ten each")
    (testing "set two aside: three draws, then a discard"
      (let [t (net/act s 0 {:action :draw :from :deck :set-aside (take 2 hand)})]
        (is (= :drawing (:step t)))
        (is (= 2 (:draws-left t)))
        (is (= 9 (count (get-in t [:hands 0]))))
        (let [u (-> t (net/act 0 {:action :draw :from :deck}) (net/act 0 {:action :draw :from :deck}))]
          (is (= :discard (:step u)))
          (is (= 11 (count (get-in u [:hands 0]))))
          (let [out (first (get-in u [:hands 0]))
                v   (net/act u 0 {:action :discard :card out})]
            (is (= 10 (count (get-in v [:hands 0]))) "back to ten")
            (is (= (conj (vec (take 2 hand)) out) (get-in v [:piles 0]))
                "the set-aside cards first, in order, the discard on top")
            (is (= 1 (:to-act v)))))))
    (testing "no set-aside after the first draw"
      (let [t (net/act s 0 {:action :draw :from :deck :set-aside [(first hand)]})]
        (is (identical? t (net/act t 0 {:action :draw :from :deck :set-aside [(second hand)]})))))))

(deftest drawing-from-any-pile
  (let [s (-> (dealt 3 2) (assoc-in [:piles 2] [40 41]) (assoc-in [:piles 0] [42]))]
    (is (some #{41} (get-in (net/act s 0 {:action :draw :from [:pile 2]}) [:hands 0])))
    (is (some #{42} (get-in (net/act s 0 {:action :draw :from [:pile 0]}) [:hands 0])) "your own too")
    (is (identical? s (net/act s 0 {:action :draw :from [:pile 1]})) "not an empty one")))

(deftest a-person-may-throw-back-a-face-up-card
  (let [s (-> (dealt 2 3) (assoc-in [:piles 1] [44]))
        t (net/act s 0 {:action :draw :from [:pile 1]})]
    (is (not (identical? t (net/act t 0 {:action :discard :card 44}))))))

(deftest completing-a-network-ends-it
  (let [s (-> (dealt 2 4)
              (assoc-in [:hands 0] (vec (butlast ten)))
              (assoc-in [:piles 0] [30 31])
              (assoc-in [:piles 1] [32 33 34 35])
              (assoc :deck (vec (cons (last ten) (remove (set ten) (range 60))))))
        ;; nine plus the tenth makes ten -- but the turn needs a discard, so
        ;; hand it an eleventh first and let it throw that
        s (assoc-in s [:hands 0] (conj (vec (butlast ten)) 36))
        t (-> s (net/act 0 {:action :draw :from :deck}) (net/act 0 {:action :discard :card 36}))]
    (is (net/game-over? t))
    (is (zero? (net/missing t 0)))
    (is (= 3 (net/score t 0)) "two on the pile and the one just thrown")
    (testing "everyone else pays for what their best group lacks"
      (is (= (+ 4 (* 3 (net/missing t 1))) (net/score t 1))))))

(deftest the-third-run-out-ends-it
  (let [s (-> (dealt 2 6)
              (assoc :deck [] :run-outs 2)
              (assoc-in [:piles 0] [1 2 3])
              (assoc-in [:piles 1] [4 5]))
        t (net/act s 0 {:action :draw :from :deck})]
    (is (net/game-over? t))))

(defn- cards-in [s]
  (+ (count (:deck s)) (count (:flip s)) (count (:aside s))
     (reduce + (map count (vals (:hands s))))
     (reduce + (map count (vals (:piles s))))))

(deftest bots-finish
  (doseq [seed (range 30) n [1 2 3 4]]
    (let [s (loop [s (dealt n seed) i 0]
              (assert (= 60 (cards-in s)) "no card made or lost")
              ;; a turn is two to four actions; a few hundred rounds is far
              ;; past anything a working game does with a cap of three run-outs
              (if (or (net/game-over? s) (> i 1500))
                s
                (let [t (net/act s (:to-act s) (net/bot-action s))]
                  (assert (not (identical? s t)) "the bot only plays legal moves")
                  (recur t (inc i)))))]
      (is (net/game-over? s) (str "seed " seed " at " n))
      (is (:winner s)))))
