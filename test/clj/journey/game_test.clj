(ns journey.game-test
  (:require
   [clojure.test :refer :all]
   [clojure.pprint :refer [pprint]]
   [journey.game :as game]
   [journey.choice :as choice]))

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

(deftest choose-action-type-test
  (testing "find-state returns action type choices at start of turn"
    (let [state (game/initial-state ["alice" "bob"])
          [phase choices] (choice/find-state state)]
      (println "phase:" phase)
      (println "choice keys:" (keys choices))
      (is (= :choose-action-type phase))
      (is (= (set game/action-types) (set (keys choices))))
      (doseq [action-type game/action-types]
        (let [next-state (get choices action-type)]
          (is (= action-type (get-in next-state [:player-turn :action-type])))
          (is (= (keyword (str "choose-" (name action-type)))
                 (game/current-phase next-state))))))))

;; All test positions use [2,0] as target to avoid the NEUTRAL tower at [0,0].
;; Sundivers around [2,0]:
;;   dir 0 → [3,0]   dir 2 → [2,-1]   dir 3 → [1,0]   dir 4 → [1,1]
;; Foundry (dir-diff 2): dirs {0,2} → sundivers [3,0] and [2,-1],
;;   two valid targets: [2,0] and [3,-1] (their other common neighbor)
;; Matrix (dir-diff 3): dirs {0,3} → sundivers [3,0] and [1,0], target [2,0]
;; Tower  (dirs 0,2,4): sundivers [3,0],[2,-1],[1,1], target [2,0]

(defn place-tile [state pos color]
  (assoc-in state [:board pos] (game/make-tile color)))

(defn place-sundiver [state player pos]
  (-> state
      (update-in [:board pos] #(or % (game/make-tile :blue)))
      (assoc-in [:board pos :sundivers player] 1)))

(deftest convert-patterns-test
  (testing "foundry: two sundivers at 120° → two target choices"
    ;; sundivers at [3,0] and [2,-1] are at dirs 0 and 2 from [2,0]
    ;; they share two common neighbors: [2,0] and [3,-1]
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-tile [3 -1] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [2 -1]))
          foundries (filter #(= :foundry (:type %)) (game/find-conversions state "alice"))]
      (is (= 2 (count foundries)))
      (is (every? #(= #{[3 0] [2 -1]} (set (:sundivers %))) foundries))
      (is (= #{[2 0] [3 -1]} (set (map :target foundries))))))

  (testing "matrix: two sundivers directly across → one target"
    ;; sundivers at [3,0] and [1,0] are at dirs 0 and 3 from [2,0]
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [1 0]))
          matrices (filter #(= :matrix (:type %)) (game/find-conversions state "alice"))]
      (is (= 1 (count matrices)))
      (is (= [2 0] (:target (first matrices))))))

  (testing "tower: three equally spaced sundivers → one target"
    ;; sundivers at dirs 0,2,4 from [2,0]: [3,0],[2,-1],[1,1]
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [2 -1])
                    (place-sundiver "alice" [1 1]))
          towers (filter #(= :tower (:type %)) (game/find-conversions state "alice"))]
      (is (= 1 (count towers)))
      (is (= [2 0] (:target (first towers))))))

  (testing "convert places station and returns sundivers to reserve"
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [1 0]))
          after (game/convert state "alice" :matrix [2 0] [[3 0] [1 0]])]
      (is (= :matrix (get-in after [:board [2 0] :station :type])))
      (is (= "alice" (get-in after [:board [2 0] :station :player])))
      (is (= 1 (get-in after [:board [2 0] :station :level])))
      (is (= 0 (get-in after [:board [3 0] :sundivers "alice"] 0)))
      (is (= 0 (get-in after [:board [1 0] :sundivers "alice"] 0))))))

(deftest draw-from-bag-test
  (testing "drawing from the bag reduces count"
    (let [bag   (game/full-bag)
          color (first game/tile-colors)
          [bag2 drawn] (game/draw-from-bag bag)]
      (println "drew:" drawn)
      (is (some? drawn))
      (is (= (dec game/num-worlds-per-color) (get bag2 drawn))))))
