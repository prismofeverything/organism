(ns organism.introduction-test
  (:require [clojure.test :refer [deftest is testing]]
            [organism.game :as game]))

(def homes [[:D 0] [:D 1] [:D 2]])

(defn position []
  (-> (game/create-game 6 [:A :B :C :D]
                        [["orb" {:starting-spaces homes :capture-limit 5}]
                         ["mass" {:starting-spaces [[:D 9] [:D 10] [:D 11]] :capture-limit 5}]]
                        3 false)
      (assoc-in [:state :food] {[:D 0] 8 [:D 1] 9 [:D 2] 10 [:C 0] 6})
      (game/add-element "mass" 1 :eat [:C 0] 4)))

(deftest introduction-clears-home-food-and-preserves-adjacent-food
  (doseq [[label introduce fields]
          [[:spaces game/introduce-spaces
            {:organism 0 :spaces (zipmap homes [:eat :grow :move])}]
           [:elements game/introduce-elements
            {:organism 0 :eat [:D 0] :grow [:D 1] :move [:D 2]}]]]
    (testing (name label)
      (let [before (position)
            after (introduce before "orb" fields)]
        (is (= {[:C 0] 6} (get-in after [:state :food])))
        (is (= [1 1 1] (mapv #(get-in after [:state :elements % :food]) homes)))
        (is (nil? (game/get-element after [:C 0])))
        (is (= 8 (game/free-food-present before [:D 0])))))))

(deftest circulation-transfers-half-rounded-up
  (doseq [food [0 1 2 3 5 10 111]]
    (let [before (-> (position)
                     (game/add-element "orb" 0 :eat [:D 0] food)
                     (game/add-element "orb" 0 :move [:D 1] 4))
          after (game/circulate before {:from [:D 0] :to [:D 1]})
          moved (quot (inc food) 2)]
      (is (= (- food moved) (:food (game/get-element after [:D 0]))))
      (is (= (+ 4 moved) (:food (game/get-element after [:D 1])))))))
