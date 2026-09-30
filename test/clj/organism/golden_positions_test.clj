(ns organism.golden-positions-test
  "Boards with the answer stated by a person.

   Every other test of the rules checks the implementation against itself. The
   cross-engine parity suite proves Clojure, Python and Rust agree — and they
   agreed perfectly while all three withheld a legal move, because the same
   misreading was written into all three. The rule tests assert the behaviour
   intended by whoever wrote the rule, which is the thing that can be wrong.

   These are different. Each case is a board and a claim about it made by the
   game's designer, not derived from any implementation. A board carries no
   rules — it is pieces and food — so a position is safe to pin even when it was
   recorded by a build whose rules were wrong. What makes it golden is the stated
   answer, and the answer is the one place ground truth can enter.

   Add a case whenever a rule is decided or a rule is found wrong. Name the
   position and say what must be true of it in plain words."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.board :as board]
   [organism.choice :as choice]
   [organism.game :as game]))

(defn- position
  "A board with these elements and nothing else, `player` to act with no action
   type declared yet."
  [players rings player elements]
  (let [count-players (count players)
        starting (board/starting-spaces rings count-players players board/total-rings {})
        info (game/initial-players starting (vec (repeat count-players board/default-player-captures)))]
    (-> (game/create-game (board/player-symmetry count-players)
                          (vec (take rings board/total-rings)) info 3 false {})
        (assoc-in [:state :elements] elements)
        (assoc-in [:state :food] {})
        (assoc-in [:state :player-turn]
                  {:player player :introduction {} :organism-turns [] :advance nil})
        game/find-organisms)))

(defn- element [player organism type space food]
  {:player player :organism organism :type type :space space :food food :captures []})

(defn- types-offered
  "The action types this player's first organism may declare."
  [game player]
  (let [organism (first (keys (game/player-organisms game player)))
        [phase choices] (choice/find-state (game/choose-organism game organism))]
    [phase (set (keys choices))]))

;; ── SleepyOrganismyt, state 47 ──────────────────────────────────────────────
;;
;; Reported by a player who could not choose GROW and should have been able to.
;; Two-player, four rings, round 3, Harx to act. Harx's organism holds four
;; food — three on its eat element, one on its move — and none on either of its
;; two growers, with three spaces it could grow into.
;;
;; Ryan, on the rule: "you want to choose the grow action but don't have any
;; food on growers?" — declaring GROW is legal here. A turn is several actions
;; and any of them may be a circulate, so the organism circulates food onto a
;; grower and grows. Food on the wrong element is a detour, not a wall.
;;
;; `require-useful-action` withheld GROW here for six days, in every engine and
;; on the live site, because it asked whether the action could be taken *this
;; instant* rather than at any point in the turn.

(def sleepy-organism-47
  (position
   ["Harx" "Dizzoj"] 4 "Harx"
   {["D" 0]  (element "Harx" 7 :grow ["D" 0] 0)
    ["D" 1]  (element "Harx" 7 :move ["D" 1] 1)
    ["D" 2]  (element "Harx" 7 :eat  ["D" 2] 3)
    ["D" 17] (element "Harx" 7 :grow ["D" 17] 0)
    ["D" 9]  (element "Dizzoj" 6 :eat  ["D" 9] 1)
    ["D" 10] (element "Dizzoj" 6 :move ["D" 10] 1)
    ["D" 11] (element "Dizzoj" 6 :grow ["D" 11] 2)
    ["C" 8]  (element "Dizzoj" 6 :eat  ["C" 8] 1)}))

(deftest food-on-the-wrong-element-does-not-forbid-growing
  (testing "GROW is offered although both growers are empty — the organism can
            circulate food onto one and grow"
    (let [[phase offered] (types-offered sleepy-organism-47 "Harx")]
      (is (= :choose-action-type phase))
      (is (contains? offered :grow)
          "the reported bug: GROW was withheld because the food was not yet on a grower")
      (is (= #{:eat :grow :move} offered)
          "all three were available to this organism")))

  (testing "the organism really does hold the food, just not on the growers"
    (let [elements (vals (get-in sleepy-organism-47 [:state :elements]))
          harx (filter #(= "Harx" (:player %)) elements)
          growers (filter #(= :grow (:type %)) harx)]
      (is (= 4 (reduce + 0 (map :food harx))) "four food in the organism")
      (is (zero? (reduce + 0 (map :food growers))) "none of it on a grower")))

  (testing "and there is somewhere to grow into, so the turn was worth declaring"
    (let [elements (vals (get-in sleepy-organism-47 [:state :elements]))
          growers (filter #(and (= "Harx" (:player %)) (= :grow (:type %))) elements)]
      (is (pos? (count (game/growable-spaces sleepy-organism-47 (map :space growers))))))))

(deftest an-organism-with-no-food-anywhere-may-still-declare-growing
  (testing "declaring is never filtered by what the turn could accomplish: a
            GROW with nothing to spend is the player's call, and the turn then
            offers what it can -- or a pass"
    (let [game (position
                ["Harx" "Dizzoj"] 4 "Harx"
                {["D" 0] (element "Harx" 7 :grow ["D" 0] 0)
                 ["D" 1] (element "Harx" 7 :move ["D" 1] 0)
                 ["D" 2] (element "Harx" 7 :eat  ["D" 2] 0)
                 ["D" 9] (element "Dizzoj" 6 :eat  ["D" 9] 1)
                 ["D" 10] (element "Dizzoj" 6 :move ["D" 10] 1)
                 ["D" 11] (element "Dizzoj" 6 :grow ["D" 11] 2)})
          [_ offered] (types-offered game "Harx")]
      (is (contains? offered :grow)
          "every type the organism has an element of is offered")
      (is (contains? offered :eat)
          "eating needs no food, only an empty neighbour"))))
