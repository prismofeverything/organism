(ns organism.flow-test
  "FLOW, as written out in docs/flow-rules.md.

   The claims here are the designer's: every organism declares before anything
   is performed, by clicking one of its elements, in any order; the turn then
   proceeds one action at a time for all organisms at once — every choice judged
   against the board as the action began, nothing happening until the last is
   made, and then all of them resolving together so that the order they were
   clicked in cannot matter; between actions, what touches is one organism and
   what has parted is two."
  (:require
   [clojure.math.combinatorics :as combine]
   [clojure.test :refer [deftest testing is]]
   [organism.board :as board]
   [organism.choice :as choice]
   [organism.game :as game]))

(defn- element [player organism type space food]
  {:player player :organism organism :type type :space space :food food :captures []})

(defn- position
  ([mutations elements] (position mutations elements {}))
  ([mutations elements food]
   (let [players ["a" "b"]
         starting (board/starting-spaces 4 2 players board/total-rings {})
         info (game/initial-players starting [5 5])]
     (-> (game/create-game (board/player-symmetry 2)
                           (vec (take 4 board/total-rings)) info 3 false mutations)
         (assoc-in [:state :elements] elements)
         (assoc-in [:state :food] food)
         (assoc-in [:state :player-turn]
                   {:player "a" :introduction {} :organism-turns [] :advance nil})
         game/find-organisms))))

(def opponent
  {["D" 9]  (element "b" 9 :eat  ["D" 9] 1)
   ["D" 10] (element "b" 9 :move ["D" 10] 1)
   ["D" 11] (element "b" 9 :grow ["D" 11] 1)})

;; ── Driving the game ────────────────────────────────────────────────────────

(defn- phase [game] (first (choice/find-state game)))
(defn- offered [game] (set (keys (second (choice/find-state game)))))

(defn- pick
  [game key]
  (let [[phase choices] (choice/find-state game)]
    (assert (contains? choices key)
            (str "no " (pr-str key) " among " (pr-str (sort-by pr-str (keys choices)))
                 " at " phase))
    (get choices key)))

(defn- picks [game & keys] (reduce pick game keys))

(defn- organism-at [game space] (get-in game [:state :elements space :organism]))

(defn- name-at
  "What FLOW keys the organism at `space` by: its first space."
  [game space]
  (game/organism-name (get (game/player-organisms game "a") (organism-at game space))))

(defn- only
  "Take the one choice on offer."
  [game]
  (let [offer (offered game)]
    (assert (= 1 (count offer)) (str "expected one choice, got " offer))
    (pick game (first offer))))

(defn- declare-types
  "Declare a type for the organism at each space, in the order given."
  [game & spaces-and-types]
  (reduce (fn [game [space type]] (pick game [(name-at game space) type]))
          game (partition 2 spaces-and-types)))

(defn- choose
  "Make a choice of `type` in the organism at `space`, then fill in its fields.
   `:only` takes the single option on offer."
  [game space type & fields]
  (reduce (fn [game field] (if (= :only field) (only game) (pick game field)))
          (pick game [(name-at game space) type])
          fields))

(defn- declaration-of
  "The index of the declaration acting through the organism at `space`."
  [game space]
  (first (keep-indexed
          (fn [index {:keys [organisms]}]
            (when (contains? organisms (organism-at game space)) index))
          (get-in game [:state :player-turn :organism-turns]))))

(defn- declared-types
  "{space-of-an-organism type} for each declaration."
  [game & spaces]
  (into {} (for [space spaces]
             [space (get-in game [:state :player-turn :organism-turns
                                  (declaration-of game space) :choice])])))

(defn- commit
  [game]
  (assert (= :flow-commit (phase game)) (str "cannot commit at " (phase game)))
  (:advance (second (choice/find-state game))))

(defn- board-of [game] (select-keys (:state game) [:elements :food]))

(defn- grow-costs
  "What growing an element of `type` in the organism at `space` would cost, as
   the set of totals the contributions on offer add up to."
  [game space type]
  (let [game (choose game space :grow type)
        [phase contributions] (choice/find-state game)]
    (assert (= :grow-from phase) (str "at " phase))
    (set (map #(reduce + (vals %)) (keys contributions)))))

(defn- quietly* [f] (binding [*out* (java.io.StringWriter.)] (f)))
(defmacro ^:private quietly [& body] `(quietly* (fn [] ~@body)))

;; ── Positions ───────────────────────────────────────────────────────────────

;; Two organisms, one empty space between them: D3 touches the first's grower
;; at D2 and the second's eater at D4, so growing into it joins them.
(def neighbours
  (merge
   opponent
   {["D" 0] (element "a" 0 :eat  ["D" 0] 0)
    ["D" 1] (element "a" 0 :grow ["D" 1] 3)
    ["D" 2] (element "a" 0 :grow ["D" 2] 3)
    ["C" 1] (element "a" 0 :move ["C" 1] 0)
    ["D" 4] (element "a" 1 :eat  ["D" 4] 0)
    ["D" 5] (element "a" 1 :grow ["D" 5] 3)
    ["D" 6] (element "a" 1 :move ["D" 6] 0)
    ["C" 4] (element "a" 1 :grow ["C" 4] 3)}))

;; Two organisms one step apart. The first's mover at C1 stepping to C2 touches
;; its own D2 and the second's D4 and C3, joining them; stepping back parts them.
(def apart
  (merge
   opponent
   {["D" 0] (element "a" 0 :eat  ["D" 0] 0)
    ["D" 1] (element "a" 0 :grow ["D" 1] 3)
    ["D" 2] (element "a" 0 :eat  ["D" 2] 0)
    ["C" 1] (element "a" 0 :move ["C" 1] 1)
    ["C" 0] (element "a" 0 :move ["C" 0] 1)
    ["D" 4] (element "a" 1 :eat  ["D" 4] 0)
    ["C" 3] (element "a" 1 :grow ["C" 3] 3)
    ["D" 5] (element "a" 1 :grow ["D" 5] 3)
    ["D" 6] (element "a" 1 :grow ["D" 6] 3)
    ["C" 4] (element "a" 1 :move ["C" 4] 0)}))

;; One organism in a line, a mover at D2 in the middle. Stepping out to C1
;; leaves D0, D1, C1 and D3–D6, both halves alive; stepping back rejoins them.
(def line
  (merge
   opponent
   {["D" 0] (element "a" 0 :grow ["D" 0] 2)
    ["D" 1] (element "a" 0 :eat  ["D" 1] 0)
    ["D" 2] (element "a" 0 :move ["D" 2] 1)
    ["D" 3] (element "a" 0 :eat  ["D" 3] 0)
    ["D" 4] (element "a" 0 :grow ["D" 4] 2)
    ["D" 5] (element "a" 0 :move ["D" 5] 1)
    ["D" 6] (element "a" 0 :move ["D" 6] 0)}))

;; ── Declaring ───────────────────────────────────────────────────────────────

(deftest declaring-is-one-click-per-organism-in-any-order
  (let [game (position {:FLOW {}} neighbours)
        x (name-at game ["D" 0])
        y (name-at game ["D" 4])]
    (testing "the turn opens on declaring, and every organism can be declared"
      (is (= :flow-declare (phase game)))
      (is (every? (offered game) [[x :grow] [y :eat] [y :grow]])))

    (testing "either organism can go first"
      (is (= :flow-declare (phase (pick game [y :eat]))))
      (is (= :flow-declare (phase (pick game [x :grow])))))

    (testing "a declared organism can change its mind until the last declares"
      (let [game (declare-types game ["D" 0] :eat ["D" 0] :grow ["D" 4] :eat)]
        (is (= {["D" 0] :grow ["D" 4] :eat} (declared-types game ["D" 0] ["D" 4])))))

    (testing "once all have declared, the turn moves on to choosing, and nothing
              on the board has changed"
      (let [declared (declare-types game ["D" 4] :eat ["D" 0] :grow)]
        (is (= :flow-choose (phase declared)))
        (is (= (board-of game) (board-of declared)))))

    (testing "each gets one action per element of its type"
      (let [declared (declare-types game ["D" 4] :grow ["D" 0] :grow)]
        (is (= [2 2] (map :num-actions (get-in declared [:state :player-turn :organism-turns]))))))))

;; ── An action ───────────────────────────────────────────────────────────────

(defn- neighbours-declared []
  (declare-types (position {:FLOW {}} neighbours) ["D" 0] :grow ["D" 4] :grow))

(deftest a-choice-changes-nothing-until-every-organism-has-chosen
  (let [game (neighbours-declared)
        first-choice (choose game ["D" 1] :grow :eat {["D" 1] 1} ["D" 3])]
    (testing "a made choice is pending: the board is as it was"
      (is (= (board-of game) (board-of first-choice)))
      (is (= :flow-choose (phase first-choice))))

    (testing "and can be taken back"
      (let [cancel [:cancel (declaration-of game ["D" 0])]]
        (is (contains? (offered first-choice) cancel))
        (is (= game (pick first-choice cancel)))))

    (testing "the other organism can still choose in any order"
      (is (seq (filter (fn [[o _]] (= o (name-at game ["D" 4]))) (offered first-choice)))))

    (testing "when the last organism has chosen, the action commits all at once"
      (let [both (choose first-choice ["D" 5] :grow :eat {["D" 5] 1} ["C" 3])
            committed (commit both)]
        (is (= (board-of game) (board-of both)) "still nothing, until the commit")
        (is (= :eat (get-in committed [:state :elements ["D" 3] :type])))
        (is (= :eat (get-in committed [:state :elements ["C" 3] :type])))
        (is (= 2 (get-in committed [:state :elements ["D" 1] :food])))
        (is (= 2 (get-in committed [:state :elements ["D" 5] :food])))))))

(defn- every-order
  "The board after making `choices` in every order, then committing. Each
   choice is a function from game to game."
  [game choices]
  (set (for [order (combine/permutations choices)]
         (board-of (commit (reduce (fn [game f] (f game)) game order))))))

(deftest the-order-choices-are-made-in-cannot-change-the-outcome
  (testing "a growth that joins two organisms, and the other organism's growth"
    (is (= 1 (count (every-order
                     (neighbours-declared)
                     [#(choose % ["D" 1] :grow :eat {["D" 1] 1} ["D" 3])
                      #(choose % ["D" 5] :grow :eat {["D" 5] 1} ["C" 3])])))))

  (testing "a move that joins, and a growth beside it"
    (let [game (declare-types (position {:FLOW {}} apart) ["D" 0] :move ["D" 4] :grow)]
      (is (= 1 (count (every-order
                       game
                       [#(choose % ["C" 1] :move ["C" 1] ["C" 2])
                        #(choose % ["D" 6] :grow :eat {["D" 6] 1} ["D" 7])])))))))

;; ── Two becoming one ────────────────────────────────────────────────────────

(deftest growing-together-makes-one-organism-from-the-next-action
  (let [game (neighbours-declared)
        chosen (-> game
                   (choose ["D" 1] :grow :eat {["D" 1] 1} ["D" 3])
                   (choose ["C" 4] :grow :eat {["C" 4] 1} ["D" 7]))]
    (testing "within the action the second's growth costs its own eater, not the
              joined organism's"
      (is (= #{1} (grow-costs (choose game ["D" 1] :grow :eat {["D" 1] 1} ["D" 3])
                              ["D" 5] :eat))))

    (let [game (commit chosen)]
      (testing "after the commit, one organism, both declarations acting through it"
        (is (apply = (map (partial organism-at game) [["D" 0] ["D" 3] ["D" 4] ["D" 7]])))
        (is (apply = (map :organisms (get-in game [:state :player-turn :organism-turns])))))

      (testing "growth costs what the joined organism costs: four eaters now"
        (is (= #{4} (grow-costs game ["D" 1] :eat))))

      (testing "food circulates across the join"
        (is (contains? (offered (choose game ["D" 1] :circulate ["D" 1])) ["D" 5])))

      (testing "two declarations through one organism cannot spend the same food"
        ;; D1 has 2 left; pay all of it into the first growth
        (let [game (choose game ["D" 1] :grow :eat {["D" 1] 2 ["D" 2] 2} ["C" 3])
              contributions (keys (second (choice/find-state
                                           (choose game ["D" 1] :grow :eat))))]
          (is (seq contributions))
          (is (not-any? #(pos? (get % ["D" 1] 0)) contributions) "D1 is spent")
          (is (every? #(<= (get % ["D" 2] 0) 1) contributions) "D2 has one left")
          (is (not (contains? (offered (pick game [(name-at game ["D" 1]) :circulate]))
                              ["D" 1]))
              "nor circulate what is already spent")))

      (testing "nor grow into the same space"
        (let [game (choose game ["D" 1] :grow :eat {["D" 1] 2 ["D" 2] 2} ["C" 3])]
          (is (not (contains? (offered (choose game ["D" 1] :grow :eat {["D" 5] 3 ["C" 4] 1}))
                              ["C" 3]))))))))

(deftest moving-into-contact-makes-one-organism-from-the-next-action
  (let [game (declare-types (position {:FLOW {}} apart) ["D" 0] :move ["D" 4] :grow)
        moving (choose game ["C" 1] :move ["C" 1] ["C" 2])]
    (testing "a space claimed by one choice is not available to another"
      (is (not (contains? (offered (choose moving ["C" 3] :grow :eat {["C" 3] 1})) ["C" 2]))))

    (testing "within the action the second still pays its own cost"
      (is (= #{1} (grow-costs moving ["D" 5] :eat))))

    (let [game (commit (choose moving ["D" 6] :grow :eat {["D" 6] 1} ["D" 7]))]
      (testing "after the commit, one organism"
        (is (apply = (map (partial organism-at game) [["D" 0] ["C" 2] ["D" 4] ["D" 7]]))))

      (testing "growing costs the joined organism's four eaters"
        (is (= #{4} (grow-costs game ["D" 5] :eat))))

      (testing "and food circulates across the join"
        (is (contains? (offered (choose game ["D" 1] :circulate ["D" 1])) ["D" 5]))))))

(deftest the-base-game-cannot-circulate-across-a-join
  (testing "the edge case FLOW removes: joined this turn, still two organisms"
    (let [base (position {} apart)
          x (organism-at base ["D" 0])
          y (organism-at base ["D" 4])
          game (-> (picks base x :move)
                   (picks :move ["C" 1] ["C" 2])
                   (picks :circulate ["D" 1] ["D" 0])
                   (picks y :grow :circulate ["D" 5]))]
      (is (not (contains? (offered game) ["D" 1]))))))

;; ── One becoming two ────────────────────────────────────────────────────────

(deftest moving-apart-makes-two-organisms-from-the-next-action
  (let [game (declare-types (position {:FLOW {}} apart) ["D" 0] :move ["D" 4] :grow)
        joined (commit (-> game
                           (choose ["C" 1] :move ["C" 1] ["C" 2])
                           (choose ["D" 6] :grow :eat {["D" 6] 1} ["D" 7])))
        parting (choose joined ["C" 2] :move ["C" 2] ["C" 1])]
    (testing "the action they part in, they are still one: a growth costs the
              joined organism's four eaters"
      (is (= #{4} (grow-costs parting ["D" 5] :eat))))

    (let [game (commit (choose parting ["D" 5] :grow :eat {["D" 5] 2 ["C" 3] 2} ["B" 2]))]
      (testing "after the commit, two organisms"
        (is (not= (organism-at game ["D" 0]) (organism-at game ["D" 4]))))

      (testing "the declaration they shared can act in either half: while joined,
                its growth was the whole organism's"
        (is (every? (offered game) [[(name-at game ["D" 4]) :grow]
                                    [(name-at game ["D" 0]) :circulate]])))

      (testing "in the second half, growth costs its own three eaters"
        (is (= #{3} (grow-costs game ["D" 5] :eat))))

      (testing "and food no longer crosses between them"
        (is (= #{["D" 0] ["D" 2] ["C" 0] ["C" 1]}
               (offered (choose game ["D" 1] :circulate ["D" 1]))))))))

(deftest splitting-and-rejoining-in-one-turn
  (let [game (declare-types (position {:FLOW {}} line) ["D" 0] :move)
        split (commit (choose game ["D" 2] :move ["D" 2] ["C" 1]))]
    (testing "one organism, one declaration, an action per mover"
      (is (= [3] (map :num-actions (get-in game [:state :player-turn :organism-turns])))))

    (testing "stepping out splits it, and either half can act"
      (is (not= (organism-at split ["D" 0]) (organism-at split ["D" 5])))
      (is (every? (offered split) [[(name-at split ["D" 0]) :move]
                                   [(name-at split ["D" 5]) :move]])))

    (testing "no circulating between the halves"
      (is (= #{["D" 1] ["C" 1]} (offered (choose split ["D" 0] :circulate ["D" 0])))))

    (let [rejoined (commit (choose split ["C" 1] :move ["C" 1] ["D" 2]))]
      (testing "stepping back in rejoins it"
        (is (= (organism-at rejoined ["D" 0]) (organism-at rejoined ["D" 5]))))

      (testing "and food circulates end to end again"
        (is (contains? (offered (choose rejoined ["D" 0] :circulate ["D" 0])) ["D" 4]))))))

;; ── Free food ───────────────────────────────────────────────────────────────

(deftest free-food-goes-to-one-choice
  (let [game (declare-types (position {:FLOW {}} neighbours {["D" 3] 2})
                            ["D" 4] :eat ["D" 0] :grow)
        eating (choose game ["D" 4] :eat ["D" 4] ["D" 3])]
    (testing "once an eater has claimed a space's free food, nothing may grow into it"
      (is (contains? (offered (choose game ["D" 1] :grow :eat {["D" 1] 1})) ["D" 3]))
      (is (not (contains? (offered (choose eating ["D" 1] :grow :eat {["D" 1] 1})) ["D" 3]))))

    (testing "the eater gets it"
      (let [game (commit (choose eating ["D" 1] :grow :eat {["D" 1] 1} ["C" 2]))]
        (is (= 3 (get-in game [:state :elements ["D" 4] :food])))
        (is (zero? (game/free-food-present game ["D" 3])))))))

;; ── Blocking ────────────────────────────────────────────────────────────────
;;
;; Two unfed organisms whose eaters share one open neighbour, D3, holding free
;; food; an enemy sits at C2 between them. Neither has food, so neither can
;; circulate, move or grow: each has one choice, and it is the same space.

(def one-meal
  {["D" 9]  (element "b" 9 :eat  ["D" 9] 1)
   ["D" 10] (element "b" 9 :move ["D" 10] 1)
   ["D" 11] (element "b" 9 :grow ["D" 11] 1)
   ["C" 2]  (element "b" 8 :grow ["C" 2] 0)
   ["D" 2]  (element "a" 0 :eat  ["D" 2] 0)
   ["D" 1]  (element "a" 0 :grow ["D" 1] 0)
   ["C" 1]  (element "a" 0 :move ["C" 1] 0)
   ["D" 4]  (element "a" 1 :eat  ["D" 4] 0)
   ["D" 5]  (element "a" 1 :grow ["D" 5] 0)
   ["C" 3]  (element "a" 1 :move ["C" 3] 0)})

;; The same, but the first eater has somewhere else to eat: C1 is empty, its
;; mover moved round to D0.
(def two-meals
  (-> one-meal
      (dissoc ["C" 1])
      (assoc ["D" 0] (element "a" 0 :move ["D" 0] 0))))

(defn- meal [elements]
  (declare-types (position {:FLOW {}} elements {["D" 3] 2}) ["D" 2] :eat ["D" 4] :eat))

(deftest a-choice-blocked-by-another-is-changed-not-passed
  (let [game (meal two-meals)
        greedy (choose game ["D" 2] :eat ["D" 2] ["D" 3])
        y (name-at game ["D" 4])]
    (testing "when the first eats the food both wanted, the second has nothing"
      (is (not-any? (fn [[o _]] (= o y)) (offered greedy))))

    (testing "but the action does not commit and the second may not pass: the
              first could have eaten elsewhere, so the choice is taken back"
      (is (= :flow-choose (phase greedy)))
      (is (not-any? #(= :pass (first %)) (offered greedy)))
      (is (contains? (offered greedy) [:cancel (declaration-of game ["D" 2])])))

    (testing "eating elsewhere leaves the food for the second"
      (let [game (-> game
                     (choose ["D" 2] :eat ["D" 2] ["C" 1])
                     (choose ["D" 4] :eat ["D" 4] ["D" 3])
                     commit)]
        (is (= 1 (get-in game [:state :elements ["D" 2] :food])))
        (is (= 3 (get-in game [:state :elements ["D" 4] :food])))))))

(deftest when-no-choices-can-all-be-made-one-organism-passes
  (let [game (meal one-meal)
        x-first (choose game ["D" 2] :eat ["D" 2] ["D" 3])
        y-first (choose game ["D" 4] :eat ["D" 4] ["D" 3])]
    (testing "whichever eats, the other may pass — there is no other way"
      (is (contains? (offered x-first) [:pass (declaration-of game ["D" 4])]))
      (is (contains? (offered y-first) [:pass (declaration-of game ["D" 2])])))

    (testing "and the player chooses which"
      (let [game (commit (pick x-first [:pass (declaration-of game ["D" 4])]))]
        (is (= 3 (get-in game [:state :elements ["D" 2] :food])))
        (is (zero? (get-in game [:state :elements ["D" 4] :food])))))))

;; ── The whole turn ──────────────────────────────────────────────────────────

(deftest a-flow-turn-resolves-into-the-next-players-turn
  (let [game (-> (neighbours-declared)
                 (choose ["D" 1] :grow :eat {["D" 1] 1} ["D" 3])
                 (choose ["C" 4] :grow :eat {["C" 4] 1} ["D" 7])
                 commit
                 (choose ["D" 1] :grow :move {["D" 1] 2} ["C" 2])
                 (choose ["D" 5] :grow :move {["D" 5] 2} ["C" 3])
                 commit)]
    (testing "after the last action, conflict and integrity as ever"
      (is (= :actions-complete (phase game)))
      (let [game (-> game
                     (#(:advance (second (choice/find-state %))))
                     (#(:advance (second (choice/find-state %))))
                     (#(:advance (second (choice/find-state %)))))]
        (is (= "b" (game/current-player game)))
        (is (= 1 (count (game/player-organisms (game/find-organisms game) "a"))))))))

(deftest the-base-game-is-unchanged
  (testing "without FLOW, one organism takes its whole turn, and a split organism
            still circulates between its halves"
    (let [base (position {} line)
          game (-> (picks base :move :move ["D" 2] ["C" 1])
                   (picks :circulate ["D" 0]))]
      (is (contains? (offered game) ["D" 5])))))

(defn- random-flow-game
  "Play a FLOW game from the opening at random, reporting the phases seen."
  [seed steps]
  (let [random (java.util.Random. seed)
        players ["a" "b"]
        starting (board/starting-spaces 4 2 players board/total-rings {})
        info (game/initial-players starting [5 5])
        game (game/create-game (board/player-symmetry 2)
                               (vec (take 4 board/total-rings)) info 3 false {:FLOW {}})]
    (loop [game game n 0 seen #{}]
      (let [[phase choices] (choice/find-state game)]
        (if (or (>= n steps) (empty? choices) (get-in game [:state :winner]))
          seen
          ;; never take a choice back, or a random walk can undo itself forever
          (let [keys (sort-by pr-str (remove #(and (vector? %) (= :cancel (first %)))
                                             (keys choices)))
                key (nth keys (.nextInt random (count keys)))]
            (recur (get choices key) (inc n) (conj seen phase))))))))

(deftest flow-games-play-through
  (doseq [seed (range 4)]
    (let [seen (quietly (random-flow-game seed 800))]
      (is (every? seen [:flow-declare :flow-choose :flow-commit :check-integrity])
          (str "seed " seed " saw " seen)))))
