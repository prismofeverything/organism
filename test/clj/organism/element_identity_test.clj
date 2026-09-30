(ns organism.element-identity-test
  "An element is the same element for as long as it is on the board.

   The views animate a move by finding the element that left one space and
   arrived at another. They used to find it by its organism's number, but the
   board regroups after every move and renames its organisms, and a third of
   all moves were drawn as one piece vanishing and another growing in its
   place. Now each element carries an :id from the moment it is placed. Random
   games, base and FLOW, played through the real rules, hold to:

     every element has an id, and no two share one
     an id keeps its player and type for life
     an id is never given to a second element
     every move is seen as a move"
  (:require
   [clojure.test :refer [deftest is]]
   [organism.board :as board]
   [organism.choice :as choice]
   [organism.game :as game]
   [organism.transitions :as transitions]))

(defn- new-game [mutations seed]
  (let [players ["a" "b"]
        starting (board/starting-spaces 4 2 players board/total-rings mutations)
        info (game/initial-players starting [5 5])]
    (with-meta
      (game/create-game (board/player-symmetry 2) (vec (take 4 board/total-rings)) info 3 false mutations)
      {:seed seed})))

(defn- walk
  "The positions of one random game, `steps` choices long, or until it ends."
  [game steps ^java.util.Random rng]
  (loop [game game n 0 positions [(:state game)]]
    (let [[_ choices] (choice/find-state game)
          options (vec (sort-by pr-str (keep :state (vals choices))))]
      (if (or (>= n steps) (empty? options) (get-in game [:state :winner]))
        positions
        (let [state (nth options (.nextInt rng (count options)))]
          (recur (assoc game :state state) (inc n) (conj positions state)))))))

(defn- check-game [label game steps seed]
  (let [positions (walk game steps (java.util.Random. seed))
        lives (atom {})]
    (doseq [[before after] (partition 2 1 positions)
            :let [elements (vals (:elements after))
                  ids (map :id elements)]]
      (is (every? some? ids) (str label ": every element has an id"))
      (is (= (count ids) (count (set ids))) (str label ": no two elements share an id"))
      (doseq [{:keys [id player type]} elements]
        (let [life (get @lives id)]
          (is (or (nil? life) (= life [player type]))
              (str label ": id " id " keeps its player and type"))
          (swap! lives assoc id [player type])))
      ;; an id seen once, gone, and back again would be a second element
      (let [gone (remove (set ids) (map :id (vals (:elements before))))]
        (swap! lives #(reduce (fn [m id] (assoc m id :gone)) % gone)))
      (let [by-id (fn [state] (into {} (for [[space el] (:elements state)] [(:id el) space])))
            was (by-id before) now (by-id after)
            moved (set (for [[id space] now
                             :when (and (contains? was id) (not= space (get was id)))]
                         [(get was id) space]))
            seen (set (for [{:keys [type from to]} (transitions/diff before after)
                            :when (= :move type)]
                        [from to]))]
        (is (= moved seen) (str label ": every move is seen as a move"))))
    (count positions)))

(deftest an-element-is-the-same-element-for-life
  (doseq [[label mutations] [["base" {}] ["FLOW" {:FLOW true}]]
          seed (range 6)]
    (let [played (check-game (str label " seed " seed) (new-game mutations seed) 400 seed)]
      (is (< 20 played) (str label " seed " seed ": the game went somewhere")))))
