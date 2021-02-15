(ns organism.organism-test
  (:require
   [clojure.test :refer :all]
   [organism.game :refer :all]))

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
          initial (initial-state adjacencies player-info)]
      (println initial))))

(def two-player-close
  (create-game
   6
   [:yellow :red :blue :orange]
   [["orb" [[:orange 0] [:orange 1] [:orange 2]]]
    ["mass" [[:orange 9] [:orange 10] [:orange 11]]]]))

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
                    (add-element "orb" :eat [:orange 5] 0)
                    (add-element "orb" :move [:blue 4] 2)
                    (add-element "mass" :grow [:blue 3] 2)
                    (resolve-conflicts "orb"))
          elements (player-elements state)
          orb-elements (get elements "orb")]
      (is (= 1 (count orb-elements)))
      (is (= :eat (:type (first orb-elements))))
      (is (= 3 (:food (first orb-elements)))))))
