(ns organism.board-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [organism.board :as board]))

(deftest ring-counts-provide-starting-clearance
  (testing "the smallest board for each supported player count leaves three spaces clear on both sides"
    ;; A three-space gap belongs to the organism on either side of it.
    (is (= {1 3, 2 3, 3 4, 4 5, 5 7, 6 7, 7 7, 8 9, 9 10, 10 11}
           (into {} (map (fn [n] [n (board/minimum-ring-count n)]) (range 1 11)))))
    (is (nil? (board/minimum-ring-count 13))))
  (testing "every offered size meets the geometry rule"
    (doseq [players (range 1 11)
            rings (board/available-ring-counts players)]
      (is (board/starting-clearance? rings players)
          (str players " players / " rings " rings")))))
