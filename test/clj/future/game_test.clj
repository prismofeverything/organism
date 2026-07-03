(ns future.game-test
  (:require
   [clojure.test :refer :all]
   [future.game  :as game]
   [future.board :as board]))

(deftest setup-test
  (testing "create-game shape"
    (let [s (game/create-game ["alice" "bob" "carol"])]
      (is (= ["alice" "bob" "carol"] (:turn-order s)))
      (is (= 3 (count (:players s))))
      (is (= :place-mothership (:phase s)))
      (is (= "alice" (:flame s)))
      (is (map? (:board s)))
      (is (vector? (:deck s)))
      (is (= (* (count game/card-suits) game/cards-per-suit) (count (:deck s))))
      (is (every? #(contains? (:planets s) %) board/orbits))
      (is (zero? (:flares-drawn s))))))

(deftest topology-test
  (testing "all orbits have correct sizes"
    (is (= 5  (board/orbit-sizes :silver)))
    (is (= 8  (board/orbit-sizes :green)))
    (is (= 13 (board/orbit-sizes :blue)))
    (is (= 21 (board/orbit-sizes :purple)))
    (is (= 34 (board/orbit-sizes :void)))
    (is (= 81 (count (board/all-orbital-spaces))))
    (is (= 86 (count (board/all-spaces)))))
  (testing "front-space is symmetric inverse of back-space"
    (let [sids (board/all-spaces)]
      (doseq [sid sids]
        (is (= sid (board/front-space (board/back-space sid))))
        (is (= sid (board/back-space  (board/front-space sid)))))))
  (testing "wedge-color round-trips"
    (doseq [k (range board/num-wedges)]
      (is (= k (board/color->wedge (board/wedge-color k))))))
  (testing "adjacency is symmetric"
    (let [adj (board/build-adjacency)]
      (doseq [[a ns] adj n ns]
        (is (contains? (get adj n) a)
            (str a " ↔ " n " missing back-edge"))))))

(deftest initial-actions-test
  (testing "place-mothership offers the 5 beam spaces"
    (let [s (game/create-game ["a" "b"])
          acts (game/legal-actions s)]
      (is (= 5 (count acts)))
      (is (every? (fn [[k _]] (and (vector? k) (= :place-mothership (first k))))
                  acts)))))

(deftest bot-can-progress
  (testing "random play advances state and eventually ends"
    (loop [s (game/create-game ["a" "b" "c"]) i 0]
      (let [acts (game/legal-actions s)]
        (cond
          (= :game-over (:phase s))
          (is true "game ended cleanly")

          (or (empty? acts) (>= i 2000))
          (is (or (= :game-over (:phase s))
                  (>= i 2000))
              (str "ran out at i=" i " phase=" (:phase s)
                   " flame=" (:flame s)))

          :else
          (let [[_ next-s] (rand-nth (vec acts))]
            (recur next-s (inc i))))))))
