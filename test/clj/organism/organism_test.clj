(ns organism.organism-test
  (:require
   [clojure.pprint :refer (pprint)]
   [clojure.test :refer :all]
   [organism.game :refer :all]
   [organism.choice :as choice]
   [organism.examples :as examples])
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
    (let [colors [:yellow :red :blue :orange]
          rings (build-rings 6 colors)
          adjacencies (find-adjacencies rings)
          player-info [["orb" [[:orange 0] [:orange 1] [:orange 2]]]
                       ["mass" [[:orange 9] [:orange 10] [:orange 11]]]]
          initial (initial-state colors adjacencies [:yellow 0] player-info)]
      (println initial))))

(deftest find-corners-test
  (testing "the process for finding corners"
    (let [symmetry 6
          six-rings (build-rings symmetry [:yellow :red :blue :orange :green :purple :grey])
          six-adjacencies (find-adjacencies six-rings)
          corners (find-corners six-adjacencies :grey symmetry)]
      (is (every? (fn [[color space]] (mod space symmetry)) corners))
      (println "CORNERS")
      (pprint corners))))

(deftest corner-notches-test
  (testing "if the corners are removed"
    (let [game (create-game
                5
                [:yellow :red :blue]
                [["alone" [[:red 1] [:red 2] [:red 3]]]]
                true)]
      (is (not (some #{[:blue 0]} (-> game :adjacencies keys))))
      (is (= 11 (count (get game :adjacencies)))))))

(deftest create-game-test
  (testing "creating a basic game"
    (let [game (create-game
                6
                [:A :B :C :D :E :F :G]
                [["orb" [[:G 1] [:G 2] [:G 3]]]
                 ["mass" [[:G 7] [:G 8] [:G 9]]]
                 ["brone" [[:G 13] [:G 14] [:G 15]]]
                 ["laam" [[:G 19] [:G 20] [:G 21]]]
                 ["stuk" [[:G 25] [:G 26] [:G 27]]]
                 ["faast" [[:G 31] [:G 32] [:G 33]]]]
                true)]
      (mapv
       (fn [[key value]]
         (println "CREATE" key)
         (println value))
       game))))

(deftest introduce-test
  (testing "how introducing a new organism works"
    (let [game examples/two-player-close
          game (introduce
                 game "orb"
                 {:eat [:orange 0]
                  :grow [:orange 2]
                  :move [:orange 1]})]
      (println (get-in game [:players "orb"]))
      (is (= :move (get-in game [:state :elements [:orange 1] :type])))
      (is (= 1 (get-in game [:state :elements [:orange 0] :food]))))))

(deftest conflict-test
  (testing "resolving complex chains of conflict"
    (let [game (-> examples/two-player-close
                    (add-element "orb" 0 :eat [:orange 5] 0)
                    (add-element "orb" 0 :move [:blue 4] 2)
                    (add-element "mass" 1 :grow [:blue 3] 2))
          resolved (resolve-conflicts game "orb")
          elements (player-elements game)
          aftermath (player-elements resolved)
          orb-elements (get aftermath "orb")]
      (println "conflict elements" elements)
      (println "aftermath" aftermath)
      (println "post conflict" (:state resolved))
      (is (= 1 (count orb-elements)))
      (is (= :eat (:type (first orb-elements))))
      (is (= 3 (:food (first orb-elements))))
      (is (= 1 (count (get-captures resolved "orb"))))
      (is (= 1 (count (get-captures resolved "mass")))))))

(def integrity-position
  (-> examples/two-player-close
      (add-element "orb" 8 :eat [:orange 5] 0)
      (add-element "orb" 11 :move [:blue 4] 2)
      (add-element "orb" 44 :grow [:red 2] 1)
      (add-element "mass" 8 :eat [:orange 16] 1)
      (add-element "mass" 5 :grow [:orange 15] 1)
      (add-element "mass" 13 :grow [:blue 8] 0)
      (add-element "mass" 5 :move [:blue 7] 1)))

(def center-position
  (-> examples/two-player-close
      (add-element "orb" 8 :eat [:blue 5] 0)
      (add-element "orb" 11 :move [:red 2] 2)
      (add-element "orb" 44 :grow [:yellow 0] 1)))

(def conflict-position
  (-> examples/two-player-close
      (add-element "orb" 0 :grow [:orange 4] 1)
      (add-element "orb" 0 :eat [:orange 5] 0)
      (add-element "orb" 0 :move [:blue 4] 2)
      (add-element "orb" 0 :grow [:red 2] 1)
      (add-element "mass" 1 :eat [:blue 9] 1)
      (add-element "mass" 1 :move [:red 5] 1)
      (add-element "mass" 1 :grow [:blue 11] 1)
      (add-element "mass" 1 :grow [:orange 17] 0)))

(deftest growable-spaces-test
  (testing "find all adjacent spaces that can be grown into"
    (let [state conflict-position
          organisms (group-organisms state)
          grower-space [:red 2]
          grower (get-element state grower-space)
          open (open-spaces state grower-space)
          growable (growable-adjacent state grower-space)]
      (println "open" open)
      (println "growable" growable)
      (is (= 5 (count open)))
      (is (= 4 (count growable))))))

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
      (is (= 1 (count (get-captures state "orb")))))))

(deftest integrity-test
  (testing "resolution of integrity with respect to captures and removal of non-alive organisms"
    (let [game
          (-> conflict-position
              (start-turn "orb")
              (choose-organism 0)
              (choose-action-type :move)
              (move {:from [:red 2] :to [:yellow 0]})
              (resolve-conflicts "orb")
              (check-integrity "orb"))
          organisms (group-organisms game)]
      (println "sacrifice" (:state game) organisms)
      (is (= 1 (count organisms)))
      (is (= 3 (count (get-captures game "orb")))))))

(deftest turn-test
  (testing "taking a turn"
    (let [orb-turn
          ;; game/PlayerTurn
          {:player "orb"

           :introduction
           {:organism 0
            :eat [:orange 0]
            :grow [:orange 1]
            :move [:orange 2]}

           :organism-turns
           ;; game/OrganismTurn
           [{:organism 0
             :choice :grow
             :num-actions 1
             :actions
             ;; game/Action
             [{:type :grow
               :action
               {:from {[:orange 1] 1}
                :to [:blue 0]
                :element :eat}}]}]}
          game (-> examples/two-player-close
                   (apply-turn orb-turn))
          organisms (group-organisms game)]
      (println)
      (println "turn" orb-turn)
      (println)
      (println)
      (println "game" game)
      (println)
      (println "organisms" organisms)
      (println)
      (is (= "mass" (get-in game [:state :player-turn :player])))
      (is (= 4 (count (last (first organisms))))))))

(def two-organism-position
  (-> examples/two-player-close
      (add-element "orb" 0 :grow [:orange 4] 1)
      (add-element "orb" 0 :eat [:orange 5] 0)
      (add-element "orb" 0 :move [:blue 4] 2)
      (add-element "orb" 0 :grow [:red 2] 1)
      (add-element "orb" 1 :eat [:blue 9] 1)
      (add-element "orb" 1 :move [:red 5] 1)
      (add-element "orb" 1 :grow [:blue 11] 1)
      (add-element "orb" 1 :grow [:orange 17] 0)))

(def large-organism-position
  (-> examples/two-player-close
      (add-element "orb" 0 :grow [:orange 4] 1)
      (add-element "orb" 0 :eat [:orange 5] 0)
      (add-element "orb" 0 :move [:blue 4] 2)
      (add-element "orb" 0 :grow [:red 2] 1)
      (add-element "orb" 0 :eat [:blue 9] 1)
      (add-element "orb" 0 :eat [:yellow 0] 1)
      (add-element "orb" 0 :move [:red 5] 1)
      (add-element "orb" 0 :move [:blue 11] 1)
      (add-element "orb" 0 :grow [:orange 17] 0)))

(deftest introduce-choices-test
  (testing "the choices for introduction"
    (let [game examples/two-player-close
          choices (choice/find-choices game)]
      (assert (= 6 (count choices))))))

(deftest choices-walk-test
  (testing "taking a path through game space"
    (let [game examples/two-player-close
          later (choice/take-path
                 game
                 [0 0 0 0
                  0 0 0 0 0 0
                  0 0 0 0
                  0 0 0 0])]
      (println)
      (println)
      (println)
      (println "TAKE PATH")
      (pprint later)
      (println)
      (println)
      (println))))

