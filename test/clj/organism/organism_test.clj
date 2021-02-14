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
          initial (initial-state player-info adjacencies)]
      (print initial))))
