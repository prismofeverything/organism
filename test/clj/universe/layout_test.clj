(ns universe.layout-test
  "The table was hand-tuned three times and each fix broke the last one, so the
   layout is solved rather than chosen -- and this checks the solution across
   every window size and table size it will meet, on the same rectangles the
   renderer draws."
  (:require
   [clojure.test :refer [deftest testing is]]
   [universe.layout :as layout]))

(def ^:private areas
  "Table areas for the viewports people actually have, after the right rail,
   the header and the action bar are taken out."
  (for [[vw vh] [[1280 800] [1366 768] [1440 900] [1536 864]
                 [1680 1050] [1920 1080] [2560 1440]
                 [1100 700] [3440 1440]]]
    (let [{:keys [w h]} (layout/table-area vw vh)]
      [w h (str vw "x" vh)])))

(defn- pairs [n] (for [i (range n) j (range (inc i) n)] [i j]))

(deftest nothing-ever-overlaps
  (testing "no seat touches another, or the board, at any size"
    (doseq [[w h label] areas
            n (range 2 10)]
      (let [{:keys [seats board]} (layout/solve w h n)]
        (is (= n (count seats)) (str label " n=" n " lost a seat"))
        (doseq [[i j] (pairs n)]
          (is (not (layout/overlap? (nth seats i) (nth seats j)))
              (str label " n=" n ": seat " i " and seat " j " collide")))
        (doseq [[i s] (map-indexed vector seats)]
          (is (not (layout/overlap? s board 0))
              (str label " n=" n ": seat " i " runs into the board")))))))

(deftest nothing-falls-off-the-table
  (testing "every seat is inside the area the solver was given"
    (doseq [[w h label] areas
            n (range 2 10)]
      (let [{:keys [seats height]} (layout/solve w h n)]
        (doseq [[i s] (map-indexed vector seats)]
          (is (>= (:left s) -1)   (str label " n=" n " seat " i " off the left"))
          (is (<= (:right s) (inc w))  (str label " n=" n " seat " i " off the right"))
          (is (>= (:top s) -1)    (str label " n=" n " seat " i " off the top"))
          (is (<= (:bottom s) (inc height))
              (str label " n=" n " seat " i " off the bottom")))))))

(deftest the-proportions-hold
  (testing "your hand, the board and the others stay in their ratio"
    (doseq [[w h _] areas
            n (range 2 10)]
      (let [{:keys [yours board others]} (:sizes (layout/solve w h n))]
        (is (> yours board others 0))
        ;; 2.1 : 1.5 : 1, allowing for rounding at small scales
        (is (< 1.9 (/ (double yours) others) 2.3))
        (is (< 1.35 (/ (double board) others) 1.65))))))

(deftest your-hand-is-the-one-you-can-read
  (testing "your cards are always the biggest thing on the table"
    (doseq [[w h label] areas
            n (range 2 10)]
      (let [{:keys [sizes seats]} (layout/solve w h n)]
        (is (= (:yours sizes) (:card (first seats)))
            (str label ": seat 0 is you and draws your size"))
        (is (every? #(= (:others sizes) (:card %)) (rest seats))))))
  (testing "the page scrolls before the cards shrink, wherever there is width"
    (doseq [[w h label] areas
            :when (>= w 880)
            n [2 6 9]]
      (let [{:keys [sizes]} (layout/solve w h n)]
        (is (>= (:yours sizes) 105)
            (str label " n=" n " left your hand at " (:yours sizes) "px"))
        (is (>= (:others sizes) 50)
            (str label " n=" n " left the others at " (:others sizes) "px")))))
  (testing "a window too narrow to hold the ring gives up size, not correctness"
    (doseq [n [2 6 9]]
      (let [{:keys [sizes seats]} (layout/solve 720 544 n)]
        (is (= n (count seats)) "every seat still placed")
        (is (>= (:yours sizes) 60) (str "n=" n " at " (:yours sizes) "px"))))))

(deftest more-room-is-never-worse
  (testing "a bigger area never gives smaller cards"
    (doseq [n [2 6 9]]
      (let [at (fn [w h] (:others (:sizes (layout/solve w h n))))]
        (doseq [[w h] [[900 600] [1100 700] [1300 800] [1500 900] [1700 1000]]]
          (is (<= (at w h) (at (+ w 200) (+ h 100)))
              (str "n=" n ": growing the area from " w "x" h " shrank the cards")))))))

(deftest a-cramped-window-scrolls-rather-than-collapsing
  (testing "too short an area keeps the cards readable and asks for height"
    (let [{:keys [sizes height]} (layout/solve 1200 300 6)]
      (is (>= (:others sizes) 20) "cards did not collapse to nothing")
      (is (> height 300) "asked the page for the height it needs"))))

(deftest heads-up-puts-the-other-player-opposite
  (let [{:keys [seats cy]} (layout/solve 1200 760 2)]
    (is (= 2 (count seats)))
    (is (< (:y (first seats)) cy) "you are above the middle")
    (is (> (:y (second seats)) cy) "and they are below it")))
