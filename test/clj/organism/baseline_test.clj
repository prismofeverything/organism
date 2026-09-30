(ns organism.baseline-test
  "Proof that the original game is still runnable, and that each tightening is
   the only thing standing between the two.

   A rule you cannot switch off cannot be diffed against anything, and that is
   how `require-useful-action` withheld a legal turn for six days: the tightened
   game was the only game the engine could play, so there was nothing to compare
   it to. Every rule is a flag now, and these tests hold each flag to account —
   with it on the tightening bites, with it off the original behaviour returns.

   That makes the baseline usable for the thing it is for: taking a position,
   listing what the original game allows and what the tightened game allows, and
   asking whether every difference is one a named rule meant to make."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.board :as board]
   [organism.choice :as choice]
   [organism.game :as game]))

(defn- element [player organism type space food]
  {:player player :organism organism :type type :space space :food food :captures []})

(defn- position
  [players rings player elements]
  (let [n (count players)
        starting (board/starting-spaces rings n players board/total-rings {})
        info (game/initial-players starting (vec (repeat n board/default-player-captures)))]
    (-> (game/create-game (board/player-symmetry n)
                          (vec (take rings board/total-rings)) info 3 false {})
        (assoc-in [:state :elements] elements)
        (assoc-in [:state :food] {})
        (assoc-in [:state :player-turn]
                  {:player player :introduction {} :organism-turns [] :advance nil})
        game/find-organisms)))

(defn- offered-types
  [game player]
  (let [organism (first (keys (game/player-organisms game player)))
        [_ choices] (choice/find-state (game/choose-organism game organism))]
    (set (keys choices))))

;; An organism that can only eat: no food anywhere, so nothing to move and no
;; growth to pay for.
(def can-only-eat
  (position
   ["orb" "mass"] 4 "orb"
   {["D" 0] (element "orb" 0 :grow ["D" 0] 0)
    ["D" 1] (element "orb" 0 :move ["D" 1] 0)
    ["D" 2] (element "orb" 0 :eat  ["D" 2] 0)
    ["D" 9] (element "mass" 1 :eat  ["D" 9] 1)
    ["D" 10] (element "mass" 1 :move ["D" 10] 1)
    ["D" 11] (element "mass" 1 :grow ["D" 11] 1)}))

(deftest the-type-choice-is-never-narrowed
  (testing "every type the organism has is offered, useful or not"
    (is (= #{:eat :grow :move} (offered-types can-only-eat "orb"))))

  (testing "the same as the original game: the useful-action rule governs
            passing, not declaring"
    (game/with-original-rules
      (is (= #{:eat :grow :move} (offered-types can-only-eat "orb"))))))

(deftest the-eat-threshold-is-the-only-thing-stopping-a-full-eater
  (let [fed (assoc-in can-only-eat [:state :elements ["D" 2] :food] 6)
        eater (get-in fed [:state :elements ["D" 2]])]
    (testing "on: an eater at or past the threshold may not eat"
      (is (not (game/can-eat? fed eater))))
    (testing "off: the original game let it eat without limit"
      (game/with-original-rules
        (is (game/can-eat? fed eater))))
    (testing "and the threshold is what draws the line, not the food limit"
      (binding [game/*eat-threshold* 8]
        (is (game/can-eat? fed eater))))))

(deftest passing-returns-when-the-rule-is-off
  (let [declared (-> can-only-eat
                     (game/choose-organism
                      (first (keys (game/player-organisms can-only-eat "orb"))))
                     (game/choose-action-type :eat))]
    (testing "on: passing is not offered beside an action that could be taken"
      (let [[phase choices] (choice/find-state declared)]
        (is (= :choose-action phase))
        (is (not (contains? choices :pass)))))
    (testing "off: the original game always offered it"
      (game/with-original-rules
        (let [[phase choices] (choice/find-state declared)]
          (is (= :choose-action phase))
          (is (contains? choices :pass)))))))

(deftest a-stalemate-only-ends-the-game-while-the-rule-is-on
  ;; Every space but the centre taken, each player holding half of every ring so
  ;; both regions carry all three types and the centre is sealed to both.
  (let [types [:eat :grow :move]
        packed (reduce
                (fn [g [_ spaces]]
                  (let [half (/ (count spaces) 2)]
                    (reduce
                     (fn [g [i space]]
                       (game/add-element g (if (< i half) "orb" "mass") 0
                                         (nth types (mod i 3)) space 0))
                     g (map-indexed vector spaces))))
                (position ["orb" "mass"] 4 "orb" {})
                (rest (game/build-rings 6 (:rings (position ["orb" "mass"] 4 "orb" {})))))]
    (testing "on: the locked board ends, against whoever locked it"
      (is (game/stalemate? packed))
      (is (= "mass" (game/victory? packed)) "orb was on turn, so orb loses it"))
    (testing "off: the original game carries on forever"
      (game/with-original-rules
        (is (game/stalemate? packed) "still recognisably locked")
        (is (nil? (game/victory? packed)) "but nobody wins")))))
