(ns organism.organism-test
  (:require
   [clojure.test :refer :all]
   [organism.game :refer :all]
   [organism.tree :as tree])
  (:import
   [organism.game
    Action
    OrganismTurn
    PlayerTurn]))

(deftest build-rings-test
  (testing "building the rings"
    (let [rings (build-rings 5 [:yellow :red :blue :orange])]
      (println rings)
      (is (= 4 (count rings)))
      (is (= 31 (count (rings->spaces rings)))))))

(deftest space-adjacencies-test
  (testing "finding the adjacencies for a single space"
    (let [five-rings (build-rings 5 [:yellow :red :blue :orange :green])
          five-space [:red 3]
          five-adjacent (space-adjacencies five-rings five-space)
          six-rings (build-rings 6 [:yellow :red :blue :orange :green])
          six-sxace [:green 15]
          six-adjacent (space-adjacencies six-rings six-sxace)]
      (println "five adjacent" five-space five-adjacent)
      (println "six adjacent" six-sxace six-adjacent)
      (is (= 6 (count five-adjacent))))))

(deftest ring-adjacencies-test
  (testing "finding all adjacencies for a given ring"
    (let [five-rings (build-rings 5 [:yellow :red :blue :orange :green])
          blue-adjacent (ring-adjacencies five-rings :blue)]
      (is (= 10 (count blue-adjacent))))))

(deftest adjacencies-test
  (testing "finding adjacencies"
    (let [five-rings (build-rings 5 [:yellow :red :blue :orange :green :purple :grey :pink])
          five-adjacencies (find-adjacencies five-rings)
          six-rings (build-rings 6 [:yellow :red :blue :orange :green :purple :grey])
          six-adjacencies (find-adjacencies six-rings)]
      (is (= 141 (count five-adjacencies)))
      (is (= 127 (count six-adjacencies))))))

(deftest initial-state-test
  (testing "the creation of initial state from player info and adjacencies"
    (let [rings (build-rings 6 [:yellow :red :blue :orange])
          adjacencies (find-adjacencies rings)
          player-info [["orb" [[:orange 0] [:orange 1] [:orange 2]]]
                       ["mass" [[:orange 9] [:orange 10] [:orange 11]]]]
          initial (initial-state adjacencies [:yellow 0] player-info)]
      (println initial))))

(deftest corner-notches-test
  (testing "if the corners are removed"
    (let [game (create-game
                5
                [:yellow :red :blue]
                [["alone" [[:red 1] [:red 2] [:red 3]]]]
                true)]
      (is (not (some #{[:blue 0]} (-> game :adjacencies keys))))
      (is (= 11 (count (get game :adjacencies)))))))

(def two-player-close
  (create-game
   6
   [:yellow :red :blue :orange]
   [["orb" [[:orange 0] [:orange 1] [:orange 2]]]
    ["mass" [[:orange 9] [:orange 10] [:orange 11]]]]
   false))

(deftest introduce-test
  (testing "how introducing a new organism works"
    (let [state two-player-close
          state (introduce
                 state "orb"
                 {:eat [:orange 0]
                  :grow [:orange 2]
                  :move [:orange 1]})]
      (println (get-in state [:players "orb"]))
      (is (= :move (get-in state [:spaces [:orange 1] :element :type])))
      (is (= 1 (get-in state [:spaces [:orange 0] :element :food]))))))

(deftest action-test
  (testing "applying the various actions to the state"
    (let [state two-player-close
          state (introduce
                 state "orb"
                 {:eat [:orange 0]
                  :grow [:orange 2]
                  :move [:orange 1]})
          state (move
                 state "orb"
                 [:orange 2]
                 [:blue 1])
          state (introduce
                 state "mass"
                 {:eat [:orange 9]
                  :grow [:orange 10]
                  :move [:orange 11]})
          state (grow
                 state "mass"
                 {[:orange 10] 1}
                 [:blue 6]
                 :move)
          state (grow
                 state "orb"
                 {[:blue 1] 1}
                 [:red 1]
                 :grow)
          state (move
                 state "mass"
                 [:orange 10]
                 [:blue 7])
          state (circulate
                 state "orb"
                 [:orange 0]
                 [:blue 1])
          state (circulate
                 state "orb"
                 [:orange 1]
                 [:red 1])
          state (eat
                 state "mass"
                 [:orange 9])]
      ;; (println state)
      (println (player-elements state)))))

(deftest conflict-test
  (testing "resolving complex chains of conflict"
    (let [state (-> two-player-close
                    (add-element "orb" 0 :eat [:orange 5] 0)
                    (add-element "orb" 0 :move [:blue 4] 2)
                    (add-element "mass" 1 :grow [:blue 3] 2)
                    (resolve-conflicts "orb"))
          elements (player-elements state)
          orb-elements (get elements "orb")]
      (println "post conflict" elements)
      (is (= 1 (count orb-elements)))
      (is (= :eat (:type (first orb-elements))))
      (is (= 3 (:food (first orb-elements))))
      (is (= 1 (count (get-in state [:players "orb" :captures]))))
      (is (= 1 (count (get-in state [:players "mass" :captures])))))))

(def integrity-position
  (-> two-player-close
      (add-element "orb" 8 :eat [:orange 5] 0)
      (add-element "orb" 11 :move [:blue 4] 2)
      (add-element "orb" 44 :grow [:red 2] 1)
      (add-element "mass" 8 :eat [:orange 16] 1)
      (add-element "mass" 5 :grow [:orange 15] 1)
      (add-element "mass" 13 :grow [:blue 8] 0)
      (add-element "mass" 5 :move [:blue 7] 1)))

(def center-position
  (-> two-player-close
      (add-element "orb" 8 :eat [:blue 5] 0)
      (add-element "orb" 11 :move [:red 2] 2)
      (add-element "orb" 44 :grow [:yellow 0] 1)))

(def conflict-position
  (-> two-player-close
      (add-element "orb" 0 :grow [:orange 4] 1)
      (add-element "orb" 0 :eat [:orange 5] 0)
      (add-element "orb" 0 :move [:blue 4] 2)
      (add-element "orb" 0 :grow [:red 2] 1)
      (add-element "mass" 1 :eat [:blue 9] 1)
      (add-element "mass" 1 :move [:red 5] 1)
      (add-element "mass" 1 :grow [:blue 11] 1)
      (add-element "mass" 1 :grow [:orange 17] 0)))

(deftest trace-organism-test
  (testing "trace from the current space along all adjacent elements"
    (let [state
          (-> integrity-position
              (clear-organisms)
              (trace-organism [:orange 5] 111))
          organisms (group-organisms state)]
      (println "traced" organisms)
      (is (= 2 (count organisms)))
      (is (= 3 (count (get organisms ["orb" 111])))))))

(deftest clear-organisms-test
  (testing "clearing the organism state"
    (let [state
          (-> integrity-position
              (clear-organisms))
          organisms (group-organisms state)]
      (println "cleared" organisms)
      (is (= 2 (count organisms)))
      (is (= 3 (count (get organisms ["orb" nil])))))))

(deftest organisms-test
  (testing "identifying contiguous groups of elements as distinct organisms"
    (let [state (-> integrity-position (find-organisms))
          organisms (group-organisms state)
          survival (evaluate-survival organisms)]
      (println "organisms" organisms)
      (println "survival" survival)
      (is (= 3 (count organisms)))
      (is (= 2 (count (filter #(= false %) (vals survival))))))))

(deftest award-center-test
  (testing "awarding a point for occupying the center"
    (let [state
          (-> center-position
              (award-center "orb"))]
      (is (= 1 (count (get-in state [:players "orb" :captures])))))))

(deftest integrity-test
  (testing "resolution of integrity with respect to captures and removal of non-alive organisms"
    (let [state
          (-> conflict-position
              (move "orb" [:red 2] [:yellow 0])
              (resolve-conflicts "orb")
              (check-integrity "orb"))
          organisms (group-organisms state)]
      (println "sacrifice" (:players state) organisms)
      (is (= 1 (count organisms)))
      (is (= 3 (count (get-in state [:players "orb" :captures])))))))

(deftest turn-test
  (testing "taking a turn"
    (let [orb-turn (PlayerTurn.
                    "orb"
                    {:organism 0
                     :eat [:orange 0]
                     :grow [:orange 1]
                     :move [:orange 2]}
                    [(OrganismTurn.
                      0
                      :grow
                      [(Action.
                        :grow
                        {:from {[:orange 1] 1}
                         :to [:blue 0]
                         :element :eat})])])
          state (-> two-player-close
                    (take-turn orb-turn))
          organisms (group-organisms state)]
      (println "turn" orb-turn)
      (println organisms)
      (is (= 4 (count (last (first organisms))))))))

(deftest walk-test
  (testing "walking through every possible turn from a given position and player"
    (let [game two-player-close
          walk (tree/walk-turn game "orb")]
      (println "walk length" (count walk))
      (println "first seven actions")
      (clojure.pprint/pprint
       (map
        (fn [[minimal choices]]
          [minimal (map last choices)])
        (take 7 walk))))))
