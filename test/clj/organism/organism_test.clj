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
          five-adjacent (space-adjacencies five-rings [:orange 10])
          six-rings (build-rings 6 [:yellow :red :blue :orange :green])
          six-adjacent (space-adjacencies six-rings [:orange 15])]
      (println "five adjacent" five-adjacent)
      (println "six adjacent" six-adjacent)
      (is (= 6 (count (last five-adjacent)))))))

;; (deftest adjacencies-test
;;   (testing "finding adjacencies"
;;     (let [rings (build-rings 5 [:yellow :red :blue :orange])
;;           adjacencies (find-adjacencies rings)]
;;       (println adjacencies)
;;       (is (= 4 (count rings)))
;;       (is (= 31 (count (rings->spaces rings)))))))
