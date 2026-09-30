(ns organism.rules-test
  "The three rules that close the holes deliberate passing, endless eating and
   self-sacrifice opened. Each was found by watching trained agents play the
   game as originally written and exploit it; `docs/rule-tightening-experiment.md`
   records what they did and what closing it cost.

   The same rules are enforced by the Python and Rust engines, and the parity
   checks in `tests/` are what keep the three agreeing. These tests pin the
   Clojure side on its own, so a rule cannot quietly revert here and be noticed
   only as a parity failure elsewhere."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.choice :as choice]
   [organism.examples :as examples]
   [organism.game :as game]))

(defn- turn-for
  "Put `player` on turn with a single organism, so find-state describes that
   organism's choices."
  [game player]
  (-> game
      game/find-organisms
      (assoc-in [:state :player-turn :player] player)
      (assoc-in [:state :player-turn :organism-turns] [])))

(deftest eat-threshold-stops-eating-without-capping-what-may-be-held
  (testing "an eater below the threshold, beside an open space, can eat"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 4))]
      (is (game/can-eat? game (game/get-element game [:orange 0])))))

  (testing "at the threshold it cannot, however much food is on the board"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 5))]
      (is (not (game/can-eat? game (game/get-element game [:orange 0]))))))

  (testing "the threshold governs eating, not holding: food arriving by other
            means is kept, and only bars that element from eating again"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 9))]
      (is (= 9 (:food (game/get-element game [:orange 0]))))
      (is (not (game/can-eat? game (game/get-element game [:orange 0]))))))

  (testing "the rule is the game's, not the caller's: the original game is a rebind away"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 5))]
      (binding [game/*eat-threshold* game/*food-limit*]
        (is (game/can-eat? game (game/get-element game [:orange 0])))))))

(deftest useful-action-rule-removes-deliberate-passing-without-stranding-anyone
  (testing "declaring is not filtered: every type the organism has is offered,
            even where only eating could accomplish anything"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 0)
                   (game/add-element "orb" 0 :grow [:orange 1] 0)
                   (game/add-element "orb" 0 :move [:orange 2] 0)
                   (turn-for "orb"))
          [phase choices] (choice/find-state game)]
      (is (= :choose-action-type phase))
      (is (= #{:eat :grow :move} (set (keys choices))))))

  (testing "with food to spend, the actions that food makes possible appear"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 1)
                   (game/add-element "orb" 0 :grow [:orange 1] 3)
                   (game/add-element "orb" 0 :move [:orange 2] 1)
                   (turn-for "orb"))
          [phase choices] (choice/find-state game)]
      (is (= :choose-action-type phase))
      (is (= #{:eat :grow :move} (set (keys choices))))))

  (testing "passing is not offered beside an action that could be taken"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 0)
                   (game/add-element "orb" 0 :grow [:orange 1] 0)
                   (game/add-element "orb" 0 :move [:orange 2] 0)
                   (turn-for "orb"))
          ;; find-organisms relabels, so ask for the id it settled on.
          organism (first (keys (game/player-organisms game "orb")))
          game (-> game
                   (game/choose-organism organism)
                   (game/choose-action-type :eat))
          [phase choices] (choice/find-state game)]
      (is (= :choose-action phase))
      (is (not (contains? choices :pass)))
      (is (contains? choices :eat))))

  (testing "an organism that truly cannot act is still offered a move to make"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 0)
                   (game/add-element "orb" 0 :grow [:orange 1] 0)
                   (game/add-element "orb" 0 :move [:orange 2] 0)
                   ;; Wall the organism in so nothing can move, grow or eat.
                   (game/add-element "mass" 1 :eat [:blue 0] 0)
                   (turn-for "orb"))
          [_ choices] (choice/find-state game)]
      (is (seq choices)))))

(deftest a-self-wipe-takes-its-food-with-it
  (testing "a player wiped off the board on their own turn leaves nothing behind"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 1)
                   (game/add-element "orb" 0 :move [:orange 1] 1)
                   (game/add-element "mass" 1 :eat [:orange 9] 0)
                   (game/add-element "mass" 1 :grow [:orange 10] 0)
                   (game/add-element "mass" 1 :move [:orange 11] 0)
                   (game/check-integrity "orb"))]
      ;; orb's organism was missing a grow element, so it dies on orb's own
      ;; turn. Deconstructing used to drop food+1 on each space it held.
      (is (empty? (get (game/player-elements game) "orb")))
      (is (zero? (get-in game [:state :food [:orange 0]] 0)))
      (is (zero? (get-in game [:state :food [:orange 1]] 0)))))

  (testing "a player who still holds an organism keeps the food a loss drops"
    (let [game (-> examples/two-player-close
                   (game/add-element "orb" 0 :eat [:orange 0] 1)
                   (game/add-element "orb" 0 :move [:orange 1] 1)
                   ;; A second, intact organism elsewhere on the board.
                   (game/add-element "orb" 1 :eat [:blue 6] 0)
                   (game/add-element "orb" 1 :grow [:blue 7] 0)
                   (game/add-element "orb" 1 :move [:blue 8] 0)
                   (game/check-integrity "orb"))]
      (is (seq (get (game/player-elements game) "orb")))
      (is (pos? (get-in game [:state :food [:orange 0]] 0))))))
