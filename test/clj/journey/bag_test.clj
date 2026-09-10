(ns journey.bag-test
  "The world bag, and the loss it is supposed to trigger when it runs dry.

   Found by journey.game-test/player-asked-is-the-player-charged-test, which
   walks 40 randomised games and only reached this state occasionally:
   cipher-place-beacon decremented a bag colour without checking any was left,
   a count went to -1, and (every? zero? ...) then reported a bag with nothing
   in it as non-empty — forever. That hid the loss, and sent rand-nth at an
   empty collection."
  (:require
   [clojure.test :refer [deftest testing is]]
   [journey.game :as game]))

(defn- drained
  ([] (drained {}))
  ([overrides] (merge (zipmap game/tile-colors (repeat 0)) overrides)))

(deftest a-bag-with-nothing-drawable-reads-as-empty
  (testing "all zero"
    (is (true? (game/bag-empty? (drained)))))
  (testing "and the negative count that used to read as non-empty forever"
    (is (true? (game/bag-empty? (drained {:sun -1})))))
  (testing "one tile left is not empty"
    (is (false? (game/bag-empty? (drained {:sun 1})))))
  (testing "a negative alongside a positive is still not empty"
    (is (false? (game/bag-empty? (drained {:sun -1 :green 2}))))))

(deftest drawing-from-an-empty-bag-reports-instead-of-throwing
  (is (nil? (game/draw-from-bag (drained))))
  (is (nil? (game/draw-from-bag (drained {:sun -1}))))
  (testing "a negative colour is never handed out as a draw"
    (let [[_ color] (game/draw-from-bag (drained {:sun -3 :green 1}))]
      (is (= :green color)))))

(deftest a-full-bag-drains-exactly-once-through
  (let [total (* (count game/tile-colors) game/num-worlds-per-color)]
    (loop [bag (game/full-bag) drawn 0]
      (if-let [[remaining _color] (game/draw-from-bag bag)]
        (if (< drawn (inc total))
          (recur remaining (inc drawn))
          (is false "drew more tiles than the bag holds"))
        (do
          (is (= total drawn) "drained exactly the whole bag")
          (is (true? (game/bag-empty? bag))))))))

(deftest exploring-an-empty-bag-is-a-loss
  (testing "the rule that the negative count was quietly suppressing"
    (doseq [bag [(drained) (drained {:sun -1})]]
      (let [after (game/explore {:bag bag :board {} :player-turn {}} "alice" [0 0])]
        (is (= :loss (get-in after [:game-over :type])))
        (is (= :game-over (get-in after [:player-turn :phase])))
        (is (nil? (get-in after [:board [0 0]])) "no tile placed on a loss"))))

  (testing "a bag with something in it still explores normally"
    (let [after (game/explore {:bag (drained {:green 1}) :board {} :player-turn {}}
                              "alice" [0 0])]
      (is (nil? (:game-over after)))
      (is (= :green (get-in after [:board [0 0] :color])))
      (is (= 1 (get-in after [:board [0 0] :sundivers "alice"])))
      (is (true? (game/bag-empty? (:bag after))) "and that was the last one"))))

(deftest placing-a-beacon-cannot-drive-the-bag-negative
  (testing "the unchecked decrement this all came from"
    (let [state {:bag (drained) :cipher {[0 0] {:colors {}}}}
          after (game/cipher-place-beacon state "alice" [0 0] :sun true)]
      (is (= 0 (get-in after [:bag :sun])))
      (is (true? (game/bag-empty? (:bag after))))))

  (testing "a colour with stock still comes out of the bag"
    (let [state {:bag (drained {:sun 2}) :cipher {[0 0] {:colors {}}}}
          after (game/cipher-place-beacon state "alice" [0 0] :sun true)]
      (is (= 1 (get-in after [:bag :sun]))))))
