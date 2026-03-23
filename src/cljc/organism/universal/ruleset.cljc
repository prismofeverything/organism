(ns organism.universal.ruleset
  "RuleSet: compact description of a parameterized abstract board game.
   Matches the Python alphazero.game_search.ruleset.RuleSet.")

;; A ruleset is a plain map:
;; {:board-symmetry  5       ; 3-6
;;  :num-rings       5       ; 3-6
;;  :num-types       3       ; 2-4
;;  :elements-per-player 3   ; 2-5
;;  :food-enabled    true
;;  :food-initial    1       ; 0-2
;;  :can-move        true
;;  :can-eat         true
;;  :can-grow        true
;;  :can-capture     false   ; direct capture action
;;  :can-circulate   true
;;  :conflict        [1 2 1] ; per pair (i,j) i<j: 0=coexist 1=i-wins 2=j-wins 3=mutual
;;  :win-type        :captures  ; or :population
;;  :win-threshold   5
;;  :num-players     2}

(defn num-conflict-pairs
  "Number of unordered pairs for n types."
  [n]
  (quot (* n (dec n)) 2))

(defn conflict-index
  "Index into the conflict vector for the pair (i, j) where i < j."
  [num-types i j]
  (let [a (min i j)
        b (max i j)]
    (loop [idx 0 a' 0]
      (if (= a' a)
        (+ idx (- b a 1))
        (recur (+ idx (- num-types 1 a')) (inc a'))))))

(defn conflict-outcome
  "Raw conflict value for two types: 0=coexist 1=i-wins 2=j-wins 3=mutual.
   i is always the smaller index."
  [{:keys [num-types conflict]} type-a type-b]
  (if (= type-a type-b)
    0
    (let [i (min type-a type-b)
          j (max type-a type-b)
          idx (conflict-index num-types i j)]
      (get conflict idx 0))))

(defn outcome-for
  "Returns :wins if attacker beats defender, :loses if defender wins,
   :coexist if they coexist, :mutual if mutual destruction."
  [ruleset attacker defender]
  (let [raw (conflict-outcome ruleset attacker defender)]
    (case raw
      0 :coexist
      3 :mutual
      (let [i (min attacker defender)
            winner (if (= raw 1) i (max attacker defender))]
        (if (= winner attacker) :wins :loses)))))

(defn action-types
  "List of enabled action type keywords for this ruleset."
  [rs]
  (cond-> []
    (:can-move rs)      (conj :move)
    (:can-eat rs)       (conj :eat)
    (:can-grow rs)      (conj :grow)
    (:can-capture rs)   (conj :capture)
    (:can-circulate rs) (conj :circulate)))

;; ── Presets ────────────────────────────────────────────────────────────────────

(def organism-like
  {:board-symmetry 5 :num-rings 5 :num-types 3 :elements-per-player 3
   :food-enabled true :food-initial 1
   :can-move true :can-eat true :can-grow true :can-capture false :can-circulate true
   :conflict [1 2 1]
   :win-type :captures :win-threshold 5
   :num-players 5})

(def heterarchy-minimal
  {:board-symmetry 4 :num-rings 4 :num-types 3 :elements-per-player 3
   :food-enabled false :food-initial 0
   :can-move true :can-eat false :can-grow false :can-capture true :can-circulate false
   :conflict [1 2 1]
   :win-type :captures :win-threshold 3
   :num-players 2})

;; ── Search-discovered games (ranked by richness score) ─────────────────────────

(def discovered-games
  [{:name "Hexgrow"
    :richness 1814
    :description "6-fold 3-ring, all actions, grow to win. Highest strategic depth found."
    :ruleset
    {:board-symmetry 6 :num-rings 3 :num-types 3 :elements-per-player 3
     :food-enabled true :food-initial 1
     :can-move true :can-eat true :can-grow true :can-capture true :can-circulate true
     :conflict [1 2 0] :win-type :population :win-threshold 5 :num-players 2}}

   {:name "Hexgrow-4"
    :richness 1034
    :description "Larger board (4 rings), same mechanics. Higher branching factor."
    :ruleset
    {:board-symmetry 6 :num-rings 4 :num-types 3 :elements-per-player 3
     :food-enabled true :food-initial 1
     :can-move true :can-eat true :can-grow true :can-capture true :can-circulate true
     :conflict [1 2 0] :win-type :population :win-threshold 5 :num-players 2}}

   {:name "Four Elements"
    :richness 527
    :description "4 types, no grow, lots of food. Perfectly balanced. Capture to win."
    :ruleset
    {:board-symmetry 4 :num-rings 6 :num-types 4 :elements-per-player 3
     :food-enabled true :food-initial 2
     :can-move true :can-eat true :can-grow false :can-capture true :can-circulate true
     :conflict [1 3 0 3 2 2] :win-type :captures :win-threshold 3 :num-players 2}}

   {:name "Triangle March"
    :richness 439
    :description "3-fold 6-ring, move-only, 4 types. Pure positional game."
    :ruleset
    {:board-symmetry 3 :num-rings 6 :num-types 4 :elements-per-player 5
     :food-enabled false :food-initial 0
     :can-move true :can-eat false :can-grow false :can-capture false :can-circulate false
     :conflict [2 2 1 3 2 1] :win-type :captures :win-threshold 3 :num-players 2}}

   {:name "Full Spectrum"
    :richness 321
    :description "4 types, all actions, small board. Highest interaction rate."
    :ruleset
    {:board-symmetry 4 :num-rings 3 :num-types 4 :elements-per-player 2
     :food-enabled true :food-initial 2
     :can-move true :can-eat true :can-grow true :can-capture true :can-circulate true
     :conflict [1 1 2 2 3 3] :win-type :population :win-threshold 5 :num-players 2}}

   {:name "Binary Clash"
    :richness 179
    :description "2 types, mutual destruction. Most balanced game found."
    :ruleset
    {:board-symmetry 6 :num-rings 3 :num-types 2 :elements-per-player 3
     :food-enabled true :food-initial 0
     :can-move true :can-eat true :can-grow true :can-capture true :can-circulate false
     :conflict [3] :win-type :population :win-threshold 5 :num-players 2}}

   {:name "Pentagonal"
    :richness 96
    :description "5-fold symmetry like Organism, compact 3-ring board."
    :ruleset
    {:board-symmetry 5 :num-rings 3 :num-types 3 :elements-per-player 3
     :food-enabled true :food-initial 1
     :can-move true :can-eat true :can-grow true :can-capture true :can-circulate true
     :conflict [1 2 0] :win-type :population :win-threshold 5 :num-players 2}}])
