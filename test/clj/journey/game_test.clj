(ns journey.game-test
  (:require
   [clojure.test :refer :all]
   [clojure.pprint :refer [pprint]]
   [journey.game :as game]))

(deftest initial-state-test
  (testing "initial state structure"
    (let [state (game/initial-state ["alice" "bob"])]
      (pprint state)
      (is (= ["alice" "bob"] (:turn-order state)))
      (is (= 2 (count (:players state))))
      (is (map? (:board state)))
      (is (map? (:bag state)))
      (is (vector? (:deck state)))
      (is (= (* game/card-suits game/cards-per-suit) (count (:deck state))))
      (is (map? (:cipher state))))))

(deftest bag-test
  (testing "full bag has correct counts"
    (let [bag (game/full-bag)]
      (pprint bag)
      (is (= (set game/tile-colors) (set (keys bag))))
      (is (every? #(= game/num-worlds-per-color %) (vals bag))))))

(deftest cipher-test
  (testing "cipher has center and 6 color-associated positions"
    (let [cipher (game/initial-cipher)]
      (pprint cipher)
      (is (= 7 (count cipher)))
      (is (contains? cipher [0 0]))
      (is (nil? (get-in cipher [[0 0] :color])))
      (is (= (set game/tile-colors)
             (set (keep :color (vals cipher))))))))

(deftest draw-from-bag-test
  (testing "drawing from the bag reduces count"
    (let [bag   (game/full-bag)
          color (first game/tile-colors)
          [bag2 drawn] (game/draw-from-bag bag)]
      (println "drew:" drawn)
      (is (some? drawn))
      (is (= (dec game/num-worlds-per-color) (get bag2 drawn))))))
