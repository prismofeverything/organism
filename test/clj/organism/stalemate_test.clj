(ns organism.stalemate-test
  "Positions nothing can ever change.

   Elements only appear or move through growth and movement, so when every
   player is stuck at once the layout is frozen for good and no one becomes
   unstuck. Left alone such a game runs until something else stops it — a
   repetition cutoff in training, and on the website nothing at all.

   The lock is real and not exotic: a lone mover walled in behind its own
   organism, the pieces beside it blocked because an enemy of the same type
   sits next to the only gap, and the pieces with room not mobile because no
   mover is adjacent to them."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.examples :as examples]
   [organism.game :as game]))

(def types [:eat :grow :move])

(defn- packed
  "Every space but the centre taken, split down the middle: each player holds
   one half of every ring, so both regions are connected and carry all three
   types. Types run around each ring in order, which puts an eat, a grow and a
   move of BOTH players next to the centre.

   That is what seals the centre. Growing into it is blocked for each player by
   the other being adjacent to it, and moving into it is blocked because
   whichever type tries finds the same type of the opponent beside the gap. No
   other space on the board is empty, so nothing else can be entered either."
  []
  (reduce
   (fn [game [_colour spaces]]
     (let [half (/ (count spaces) 2)]
       (reduce
        (fn [game [index space]]
          (game/add-element game (if (< index half) "orb" "mass") 0
                            (nth types (mod index 3)) space 0))
        game
        (map-indexed vector spaces))))
   examples/two-player-close
   ;; The centre is its own one-space ring; leave it empty.
   (rest (game/build-rings 6 (:rings examples/two-player-close)))))

(deftest a-locked-board-is-recognised-immediately
  (testing "every space but the centre is taken, so nobody can eat, move or grow"
    (let [game (packed)]
      (is (game/stalemate? game))))

  (testing "and no player is judged able to act"
    (let [game (game/find-organisms (packed))]
      (is (not (game/player-can-act? game "orb")))
      (is (not (game/player-can-act? game "mass"))))))

(deftest a-board-with-anywhere-left-to-go-is-not-a-stalemate
  (testing "one empty space is enough: something can eat, and so the food moves"
    (let [game (-> (packed)
                   (game/remove-element [:orange 5]))]
      (is (not (game/stalemate? game)))))

  (testing "the opening is not a stalemate — nobody has introduced yet"
    (is (not (game/stalemate? examples/two-player-close))))

  (testing "an organism missing a type is about to be removed, which is a change"
    (let [game (-> (packed)
                   (game/remove-element [:orange 5])
                   (game/remove-element [:orange 6]))]
      (is (not (game/stalemate? game))))))

(deftest holding-the-centre-is-never-a-stalemate
  (testing "the centre pays its owner a capture every turn, so that game ends
            on its own however frozen the rest of the board looks"
    (let [game (-> (packed)
                   (game/add-element "orb" 0 :eat (:center examples/two-player-close) 0))]
      (is (not (game/stalemate? game)))
      ;; And it is genuinely frozen otherwise — the centre is the only reason.
      (is (nil? (game/get-element (packed) (:center examples/two-player-close)))))))
