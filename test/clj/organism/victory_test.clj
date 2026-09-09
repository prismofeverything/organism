(ns organism.victory-test
  "Ties. A player who reaches the victory condition at the same moment as an
   opponent, on their own turn, has caused the tie — and causing a tie loses.

   Before this rule existed a tie returned no winner at all and play simply
   carried on past the end of the game, which is what got reported."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.choice :as choice]
   [organism.game :as game]))

;; ── Synthetic boards ──────────────────────────────────────────────────────
;; Only the parts the victory check actually reads: who owns how many living
;; organisms (or captures), and whose turn is being finished.

(defn- living-organisms
  "Elements making up `n` living organisms for `player`. Three distinct element
   types each, which is exactly what alive-elements? asks for."
  [player n]
  (into
   {}
   (for [organism (range n)
         [index type] (map-indexed vector [:eat :move :grow])]
     [[player organism index]
      {:player player :organism organism :type type}])))

(defn- organism-board
  "A game where `counts` is {player → living organisms} and `acting` is the
   player whose turn is being finished."
  ([acting counts] (organism-board acting counts 3))
  ([acting counts organism-victory]
   {:organism-victory organism-victory
    ;; turn-order and round are only here so the no-winner case can fall
    ;; through to start-next-turn the way a real game does.
    :turn-order (vec (keys counts))
    :mutations {}
    :state {:round 0
            :elements (into {} (mapcat (fn [[player n]] (living-organisms player n))
                                       counts))
            :player-turn {:player acting :advance :check-integrity}}}))

(defn- capture-board
  "A game where `counts` is {player → captures taken} against a shared limit."
  [acting counts limit]
  {:turn-order (vec (keys counts))
   :mutations {}
   :players (into {} (map (fn [player] [player {:capture-limit limit}]) (keys counts)))
   :state {:captures (into {} (map (fn [[player n]] [player (vec (repeat n :element))])
                                   counts))
           :player-turn {:player acting :advance :check-integrity}}})

;; ── find-leader ───────────────────────────────────────────────────────────

(deftest find-leader-settles-ties-against-whoever-caused-them
  (testing "a clear leader wins whether or not they are the one acting"
    (is (= "alice" (game/find-leader [["alice" 4] ["bob" 3]] "alice")))
    (is (= "bob" (game/find-leader [["alice" 3] ["bob" 4]] "alice"))))

  (testing "the player whose turn made the tie loses it"
    (is (= "bob" (game/find-leader [["alice" 3] ["bob" 3]] "alice")))
    (is (= "alice" (game/find-leader [["alice" 3] ["bob" 3]] "bob"))))

  (testing "with nobody to blame a tie still has no winner"
    (is (nil? (game/find-leader [["alice" 3] ["bob" 3]] "carol")))
    (is (nil? (game/find-leader [["alice" 3] ["bob" 3]]))))

  (testing "dropping the acting player has to leave exactly one standing"
    (is (= "carol" (game/find-leader [["alice" 3] ["bob" 2] ["carol" 3]] "alice")))
    (is (nil? (game/find-leader [["alice" 3] ["bob" 3] ["carol" 3]] "alice")))))

;; ── organism victory ──────────────────────────────────────────────────────

(deftest organism-victory-breaks-ties
  (testing "reaching three organisms alone still wins, acting or not"
    (is (= "alice" (game/organism-victory? (organism-board "alice" {"alice" 3 "bob" 2}))))
    (is (= "bob" (game/organism-victory? (organism-board "alice" {"alice" 2 "bob" 3})))))

  (testing "nobody there yet is nobody winning"
    (is (nil? (game/organism-victory? (organism-board "alice" {"alice" 2 "bob" 2})))))

  (testing "the reported bug: both reach three on alice's turn, so bob wins"
    (is (= "bob" (game/organism-victory? (organism-board "alice" {"alice" 3 "bob" 3})))))

  (testing "and the same board on bob's turn goes the other way"
    (is (= "alice" (game/organism-victory? (organism-board "bob" {"alice" 3 "bob" 3})))))

  (testing "more organisms still beats a tie-break"
    (is (= "alice" (game/organism-victory? (organism-board "alice" {"alice" 4 "bob" 3}))))))

;; ── capture victory ───────────────────────────────────────────────────────

(deftest capture-victory-breaks-ties
  (testing "one player over the limit wins outright"
    (is (= "alice" (game/capture-victory? (capture-board "alice" {"alice" 2 "bob" 1} 2)))))

  (testing "under the limit nobody wins"
    (is (nil? (game/capture-victory? (capture-board "alice" {"alice" 1 "bob" 1} 2)))))

  (testing "hitting the limit together on alice's turn hands it to bob"
    (is (= "bob" (game/capture-victory? (capture-board "alice" {"alice" 2 "bob" 2} 2))))))

;; ── the loop that kept going ──────────────────────────────────────────────

(deftest a-tied-game-actually-ends
  (testing "victory? is what find-state consults, and it now names a winner"
    (is (= "bob" (game/victory? (organism-board "alice" {"alice" 3 "bob" 3})))))

  (testing "find-state reaches :player-victory instead of playing on"
    (let [board (organism-board "alice" {"alice" 3 "bob" 3})
          [phase choices] (choice/find-state board)]
      (is (= :player-victory phase))
      (is (= "bob" (get-in (:advance choices) [:state :winner])))))

  (testing "an untied board in the same position carries on to the next turn"
    (let [board (organism-board "alice" {"alice" 2 "bob" 2})
          [phase _] (choice/find-state board)]
      (is (not= :player-victory phase)))))
