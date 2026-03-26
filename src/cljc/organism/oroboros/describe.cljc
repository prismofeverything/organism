(ns organism.oroboros.describe
  "Convert v3 rule ASTs to human-readable descriptions.
   Also defines canonical game encodings (Go, Organism, Journey, discovered games)."
  (:require
   [clojure.string :as str]))

;; ── Opcode constants (mirror schema.py) ──────────────────────────────────────

(def SRC 0) (def TGT 1) (def SELF 0) (def ENEMY 1)

;; Predicates
(def P_TRUE 0) (def P_EMPTY 1) (def P_OCCUPIED 2) (def P_OWNER_IS 3)
(def P_TYPE_IS 4) (def P_ADJACENT 5) (def P_DISTANCE_LEQ 6)
(def P_RESOURCE_GEQ 7) (def P_GLOBAL_GEQ 8) (def P_TYPE_BEATS 9)
(def P_SAME_TYPE 10) (def P_DIFF_TYPE 11)
(def P_GROUP_HAS_LIBERTY 12) (def P_GROUP_NO_LIBERTY 13)
(def P_GROUP_SIZE_GEQ 14) (def P_GROUP_TYPES_GEQ 15)
;; N-ary spatial patterns
(def P_PATTERN_COUNT_GEQ 16) (def P_PATTERN_RING 17)
(def P_PATTERN_LINE 18) (def P_SAME_REGION 19)
;; Stochastic / information
(def P_BAG_NONEMPTY 40) (def P_DECK_NONEMPTY 41)
(def P_HAND_HAS 42) (def P_SUBBOARD_AT 43)
;; Combinators
(def P_AND 20) (def P_OR 21) (def P_NOT 22)

;; Effects
(def E_NOP 0) (def E_REMOVE_PIECE 1) (def E_PLACE_PIECE 2)
(def E_MOVE_PIECE 3) (def E_SWAP_PIECES 4) (def E_PUSH_PIECE 5)
(def E_ADD_RESOURCE 6) (def E_TRANSFER_RES 7) (def E_COLLECT_RES 8)
(def E_INC_COUNTER 9) (def E_SET_RESOURCE 10) (def E_REMOVE_GROUP 11)
(def E_INC_COUNTER_BY_GROUP 12)
;; Stochastic
(def E_DRAW_BAG 13) (def E_DRAW_DECK 14) (def E_SHUFFLE_DECK 15) (def E_DISCARD 16)
;; Sub-board / overlay
(def E_SUBBOARD_PLACE 17) (def E_SUBBOARD_CLEAR 18) (def E_LINK_NODES 19)
;; Sequencing / control
(def E_SEQ 20) (def E_IF 21) (def E_GOTO_PHASE 22)

;; Termination
(def T_COUNTER_GEQ 30) (def T_NO_PIECES 31) (def T_PIECE_COUNT_GEQ 32)
(def T_ROUND_GEQ 33) (def T_ONE_PLAYER_LEFT 34) (def T_TERRITORY_GEQ 35)
(def T_SCORE_TERRITORY 36) (def T_BAG_EMPTY 37) (def T_SUBBOARD_MATCH 38)

;; ── Helpers ──────────────────────────────────────────────────────────────────

(defn- which-str [w] (if (= w SRC) "your piece" "the target"))
(defn- owner-str [o] (case (int o) 0 "your" 1 "an enemy" 2 "any" "a"))

;; ── Predicate → English ──────────────────────────────────────────────────────

(defn pred->text [pred]
  (when (and (sequential? pred) (seq pred))
    (let [op (first pred)]
      (case (int op)
        0  nil
        1  (str (which-str (second pred)) "'s space is empty")
        2  (str (which-str (second pred)) "'s space is occupied")
        3  (str (which-str (second pred)) " is " (owner-str (nth pred 2)))
        4  (str (which-str (second pred)) " is type " (nth pred 2))
        5  "target is adjacent"
        6  (let [d (second pred)] (if (> d 50) "anywhere on board" (str "within " d " steps")))
        7  (str (which-str (second pred)) " has " (nth pred 3) "+ of resource " (nth pred 2))
        8  (str "counter " (second pred) " is " (nth pred 2) "+")
        9  "your type beats their type"
        10 "same piece type"
        11 "different piece types"
        12 (str (which-str (second pred)) "'s group has liberties")
        13 (str (which-str (second pred)) "'s group has no liberties")
        14 (str (which-str (second pred)) "'s group has " (nth pred 2) "+ pieces")
        15 (str (which-str (second pred)) "'s group has " (nth pred 2) "+ distinct types")
        ;; N-ary patterns
        16 (str (nth pred 2) "+ " (owner-str (nth pred 1)) " neighbors around " (which-str (second pred)))
        17 (str (nth pred 2) " " (owner-str (nth pred 1)) " pieces at " (nth pred 3) "-step spacing around " (which-str (second pred)))
        18 (str (nth pred 2) " " (owner-str (nth pred 1)) " pieces in a line through " (which-str (second pred)))
        19 "same connected region"
        ;; Stochastic
        40 (str "bag " (second pred) " has items")
        41 (str "deck " (second pred) " has cards")
        42 (str "hand slot " (second pred) " has type " (nth pred 2))
        43 (str "sub-board " (second pred) " at position " (nth pred 2) " has " (owner-str (nth pred 3 0)) " token")
        ;; Combinators
        20 (let [l (pred->text (second pred))
                 r (pred->text (nth pred 2))]
             (str/join " and " (remove nil? [l r])))
        21 (let [l (pred->text (second pred))
                 r (pred->text (nth pred 2))]
             (str "(" (str/join " or " (remove nil? [l r])) ")"))
        22 (str "not " (pred->text (second pred)))
        (str "predicate " op)))))

;; ── Effect → English ─────────────────────────────────────────────────────────

(defn effect->text [eff]
  (when (and (sequential? eff) (seq eff))
    (let [op (first eff)]
      (case (int op)
        0  nil
        1  (str "remove " (which-str (second eff)))
        2  (str "place " (owner-str (nth eff 2 0)) " piece at " (which-str (second eff)))
        3  (str "move piece to " (if (= (nth eff 2) TGT) "target" "source"))
        4  "swap positions"
        5  (str "push " (which-str (second eff)) " away")
        6  (let [delta (nth eff 3 1)]
             (str (if (neg? delta) "spend " "gain ")
                  (Math/abs delta) " resource on " (which-str (second eff))))
        7  (str "transfer resource to " (if (= (nth eff 2) TGT) "target" "source"))
        8  (str "collect resource from " (which-str (second eff)))
        9  "score a point"
        10 (str "set resource to " (nth eff 3 0))
        11 (str "remove entire group at " (which-str (second eff)))
        12 "score points equal to group size"
        ;; Stochastic
        13 (str "draw from bag " (second eff))
        14 (str "draw a card from deck " (second eff))
        15 (str "shuffle deck " (second eff))
        16 (str "discard from hand")
        ;; Sub-board / overlay
        17 (str "place token on sub-board " (second eff))
        18 (str "clear sub-board " (second eff) " at position " (nth eff 2))
        19 (str "create link between " (which-str (second eff)) " and " (which-str (nth eff 2)))
        ;; Sequencing / control
        20 (let [a (effect->text (second eff))
                 b (effect->text (nth eff 2))]
             (str/join ", then " (remove nil? [a b])))
        21 (str "if " (pred->text (second eff))
               " then " (effect->text (nth eff 2))
               " else " (effect->text (nth eff 3 [0])))
        22 (str "go to phase " (second eff))
        (str "effect " op)))))

;; ── Termination → English ────────────────────────────────────────────────────

(defn term->text [pred]
  (when (and (sequential? pred) (seq pred))
    (let [op (first pred)]
      (case (int op)
        30 (str "a player reaches " (nth pred 2) " points")
        31 "a player has no pieces left"
        32 (str "a player has " (nth pred 2) "+ pieces")
        33 (str "round " (second pred) " is reached")
        34 "only one player remains"
        35 (str "a player controls " (nth pred 2) "%+ of the board")
        36 "territory is scored"
        37 (str "bag " (second pred) " is empty")
        38 (str "sub-board " (second pred) " pattern matches the main board")
        (str "condition " op)))))

;; ── Rule → description card data ────────────────────────────────────────────

(defn rule->card [{:keys [predicate effect requires-piece]}]
  {:when (pred->text predicate)
   :then (effect->text effect)
   :needs-piece requires-piece})

;; ── Conflict table → descriptions ────────────────────────────────────────────

(defn conflict-pairs [num-types conflict-table]
  (when (> num-types 1)
    (let [outcomes {0 "coexist" 1 "type A wins" 2 "type B wins" 3 "mutual destruction"}]
      (for [i (range num-types)
            j (range (inc i) num-types)
            :let [idx (loop [x 0 a 0]
                        (if (= a i) (+ x (- j i 1))
                          (recur (+ x (- num-types 1 a)) (inc a))))
                  raw (get conflict-table idx 0)]]
        {:a i :b j :outcome (get outcomes raw "unknown")}))))

;; ═════════════════════════════════════════════════════════════════════════════
;; CANONICAL GAME ENCODINGS
;; ═════════════════════════════════════════════════════════════════════════════

(def go-encoding
  {:name "Go"
   :subtitle "The ancient game of territory"
   :description "Place stones on empty intersections. Enemy groups with no liberties are captured. At the end, score by territory (empty spaces you enclose) plus captured stones."
   :schema {:topology "square (19x19 standard, 9x9 shown)"
            :size "81-361 nodes" :pieces 1 :players 2 :resources 0
            :extended "none"}
   :rules
   [{:predicate [P_AND [P_DISTANCE_LEQ 99] [P_EMPTY TGT]]
     :effect [E_PLACE_PIECE TGT SELF 0]
     :requires-piece false
     :label "Place a stone on any empty intersection"}]
   :interactions
   [{:trigger "adjacency"
     :predicate [P_AND [P_OWNER_IS TGT ENEMY] [P_GROUP_NO_LIBERTY TGT]]
     :effect [E_SEQ [E_INC_COUNTER_BY_GROUP TGT -1] [E_REMOVE_GROUP TGT]]
     :label "Enemy groups with no liberties (empty adjacent spaces) are captured"}]
   :terminations
   [{:predicate [T_ROUND_GEQ 60]
     :text "Both players pass (or round limit). Score = territory (enclosed empty spaces) + captures. Highest wins."}]
   :conflict []
   :description-bits 127})

(def organism-encoding
  {:name "Organism"
   :subtitle "Heterarchical life on rings"
   :description "Three element types (eat/grow/move) in a rock-paper-scissors hierarchy on a ring board. Eat food, grow new pieces, move, and circulate resources between friendly pieces. Win by 5 captures or growing 3 complete organisms."
   :schema {:topology "radial (5-fold, 5 rings)" :size "46 nodes" :pieces 3
            :players "2-5" :resources 1 :extended "none"}
   :rules
   [{:predicate [P_AND [P_ADJACENT] [P_AND [P_OWNER_IS SRC SELF] [P_EMPTY TGT]]]
     :effect [E_MOVE_PIECE SRC TGT]
     :requires-piece true
     :label "Move: relocate piece to adjacent empty space"}
    {:predicate [P_AND [P_ADJACENT] [P_AND [P_OWNER_IS SRC SELF] [P_RESOURCE_GEQ TGT 0 1]]]
     :effect [E_COLLECT_RES SRC 0]
     :requires-piece true
     :label "Eat: collect food from adjacent space"}
    {:predicate [P_AND [P_ADJACENT] [P_AND [P_AND [P_OWNER_IS SRC SELF] [P_RESOURCE_GEQ SRC 0 1]] [P_EMPTY TGT]]]
     :effect [E_SEQ [E_ADD_RESOURCE SRC 0 -1] [E_PLACE_PIECE TGT SELF -10]]
     :requires-piece true
     :label "Grow: spend 1 food to spawn a new piece"}
    {:predicate [P_AND [P_ADJACENT] [P_AND [P_AND [P_OWNER_IS SRC SELF] [P_RESOURCE_GEQ SRC 0 1]] [P_OWNER_IS TGT SELF]]]
     :effect [E_TRANSFER_RES SRC TGT 0 1]
     :requires-piece true
     :label "Circulate: pass 1 food to a friendly neighbor"}]
   :interactions
   [{:trigger "adjacency"
     :predicate [P_AND [P_OWNER_IS TGT ENEMY] [P_TYPE_BEATS]]
     :effect [E_SEQ [E_REMOVE_PIECE TGT] [E_INC_COUNTER -1 1]]
     :label "Heterarchy conflict: stronger type captures weaker adjacent enemy"}]
   :terminations
   [{:predicate [T_COUNTER_GEQ 0 5] :text "First player to 5 captures wins"}
    {:predicate [T_ROUND_GEQ 100] :text "After 100 rounds, most captures wins"}]
   :conflict [{:a 0 :b 1 :outcome "type 0 (eat) beats type 1 (grow)"}
              {:a 0 :b 2 :outcome "type 2 (move) beats type 0 (eat)"}
              {:a 1 :b 2 :outcome "type 1 (grow) beats type 2 (move)"}]
   :description-bits 403})

(def journey-encoding
  {:name "Journey"
   :subtitle "Cooperative exploration through space"
   :description "Players launch sundivers from a drifting ark onto an expanding hex board. Build stations by arranging sundivers in geometric patterns (2 at 120° = foundry, 2 opposite = matrix, 3 equilateral = tower). Stations let you recruit, place beacons on a cipher, or steer the ark. Win by landing the ark on a tile whose neighborhood matches the cipher pattern."
   :schema {:topology "hex (infinite, wrapping at radius 9)"
            :size "~200+ nodes" :pieces "4 (sundiver, foundry, matrix, tower)"
            :players "3-5"
            :resources "sundivers (habitat + reserve), beacons, gates, cards"
            :extended "1 bag (world tiles), 1 deck (cards), 1 sub-board (cipher), overlay (gates)"}
   :phases
   [{:name "Choose action" :decider "current player"
     :description "Pick one: move sundivers, convert sundivers into a station, or activate a station"}
    {:name "Move" :decider "current player"
     :description "Launch sundiver from ark onto the board, or fly existing sundivers to adjacent/gate-connected spaces. Exploring new tiles draws from the world bag."}
    {:name "Convert" :decider "current player"
     :description "Arrange 2-3 sundivers in geometric patterns to build a station. Foundry (120° apart): recruits. Matrix (opposite): places beacons. Tower (equilateral): steers the ark."}
    {:name "Activate" :decider "current player + station owner"
     :description "Use a station's ability. Level determines action count. If using another player's station, they get bonus actions too."}
    {:name "Draw cards" :decider "auto"
     :description "Draw cards equal to gates created. Flare cards advance the ark. Keep one card in hand."}
    {:name "Captain drift" :decider "captain"
     :description "The captain steers the ark (left/right/straight), discovers new tiles, and may join discovered beacons to the cipher."}]
   :rules
   [{:predicate [P_EMPTY TGT]
     :effect [E_SEQ [E_PLACE_PIECE TGT SELF 0] [E_IF [P_NOT [P_OCCUPIED TGT]] [E_DRAW_BAG 0 0] [E_NOP]]]
     :requires-piece false
     :label "Launch sundiver onto board (explore new tile if undiscovered)"}
    {:predicate [P_AND [P_ADJACENT] [P_AND [P_OWNER_IS SRC SELF] [P_EMPTY TGT]]]
     :effect [E_MOVE_PIECE SRC TGT]
     :requires-piece true
     :label "Fly sundiver to adjacent empty space"}
    {:predicate [P_AND [P_EMPTY TGT] [P_PATTERN_RING TGT SELF 2 2]]
     :effect [E_SEQ [E_PLACE_PIECE TGT SELF 1] [E_DRAW_DECK 0 0]]
     :requires-piece false
     :label "Convert: build foundry from 2 sundivers at 120° (recruits sundivers)"}
    {:predicate [P_AND [P_EMPTY TGT] [P_PATTERN_RING TGT SELF 2 3]]
     :effect [E_SEQ [E_PLACE_PIECE TGT SELF 2] [E_DRAW_DECK 0 0]]
     :requires-piece false
     :label "Convert: build matrix from 2 opposite sundivers (places beacons on cipher)"}
    {:predicate [P_AND [P_EMPTY TGT] [P_PATTERN_RING TGT SELF 3 2]]
     :effect [E_SEQ [E_PLACE_PIECE TGT SELF 3] [E_DRAW_DECK 0 0]]
     :requires-piece false
     :label "Convert: build tower from 3 equilateral sundivers (steers the ark)"}
    {:predicate [P_AND [P_TYPE_IS SRC 1] [P_OWNER_IS SRC SELF]]
     :effect [E_ADD_RESOURCE SRC 0 2]
     :requires-piece true
     :label "Activate foundry: move sundivers from reserve to habitat"}
    {:predicate [P_AND [P_TYPE_IS SRC 2] [P_OWNER_IS SRC SELF]]
     :effect [E_SUBBOARD_PLACE 0 0 SELF]
     :requires-piece true
     :label "Activate matrix: place a beacon on the cipher"}
    {:predicate [P_AND [P_TYPE_IS SRC 3] [P_OWNER_IS SRC SELF]]
     :effect [E_SEQ [E_DRAW_BAG 0 0] [E_INC_COUNTER -1 1]]
     :requires-piece true
     :label "Activate tower: advance the ark and explore"}
    {:predicate [P_AND [P_OWNER_IS SRC SELF] [P_OWNER_IS TGT SELF]]
     :effect [E_LINK_NODES SRC TGT]
     :requires-piece true
     :label "Create gate: link two positions for fast travel"}]
   :interactions
   [{:trigger "adjacency"
     :predicate [P_AND [P_OWNER_IS SRC SELF] [P_OWNER_IS TGT ENEMY]]
     :effect [E_INC_COUNTER -1 1]
     :label "Gate toll: when using another player's gate, they collect a beacon fee"}]
   :terminations
   [{:predicate [T_SUBBOARD_MATCH 0]
     :text "Ark lands on a tile whose cipher pattern matches the explored board. Score = beacons on matching cipher positions."}
    {:predicate [T_BAG_EMPTY 0]
     :text "World bag exhausted without landing. All players lose."}
    {:predicate [T_ROUND_GEQ 200]
     :text "Turn limit reached. Highest beacon score wins."}]
   :conflict []
   :description-bits 664
   :notes "Journey uses a 6-phase turn program with role-based decision control (current player, station owner, captain), a card deck with flare processing, geometric station conversion patterns (120°/180°/equilateral), and a cooperative cipher-matching endgame. The encoding above captures the full structure using turn phases, bags, decks, sub-boards, overlay networks, and N-ary pattern predicates."})

;; ── Discovered games ─────────────────────────────────────────────────────────

(def discovered-encodings
  [{:name "Hex Chase"
    :subtitle "Discovered by search (richness 78)"
    :description "Hex board, 1 type, move+leap with action points. Eliminate to win. Near-perfect balance."
    :schema {:topology "hex" :size "37 nodes" :pieces 1 :players 2 :resources 0 :extended "none"}
    :rules
    [{:predicate [P_AND [P_ADJACENT] [P_AND [P_OWNER_IS SRC SELF] [P_EMPTY TGT]]]
      :effect [E_MOVE_PIECE SRC TGT]
      :requires-piece true
      :label "Move to adjacent empty space"}
     {:predicate [P_AND [P_DISTANCE_LEQ 2] [P_AND [P_OWNER_IS SRC SELF] [P_EMPTY TGT]]]
      :effect [E_MOVE_PIECE SRC TGT]
      :requires-piece true
      :label "Leap up to 2 spaces"}]
    :interactions []
    :terminations [{:predicate [T_ONE_PLAYER_LEFT] :text "Last player standing wins"}]
    :conflict []
    :description-bits 180}

   {:name "Grid Leaper"
    :subtitle "Discovered by search (richness 98)"
    :description "5x5 square grid with fog of war. 4 types with complex conflict table. Move+leap, 2 actions per turn."
    :schema {:topology "square" :size "25 nodes" :pieces 4 :players 2 :resources 1 :extended "fog (radius 2), energy"}
    :rules
    [{:predicate [P_AND [P_ADJACENT] [P_AND [P_OWNER_IS SRC SELF] [P_EMPTY TGT]]]
      :effect [E_MOVE_PIECE SRC TGT]
      :requires-piece true
      :label "Move to adjacent empty space"}
     {:predicate [P_AND [P_DISTANCE_LEQ 2] [P_AND [P_OWNER_IS SRC SELF] [P_EMPTY TGT]]]
      :effect [E_MOVE_PIECE SRC TGT]
      :requires-piece true
      :label "Leap up to 2 spaces"}]
    :interactions
    [{:trigger "move_into"
      :predicate [P_AND [P_OWNER_IS TGT ENEMY] [P_TYPE_BEATS]]
      :effect [E_SEQ [E_REMOVE_PIECE TGT] [E_INC_COUNTER -1 1]]
      :label "Moving adjacent to a weaker enemy captures them"}]
    :terminations [{:predicate [T_COUNTER_GEQ 0 2] :text "First to 2 captures wins"}]
    :conflict [{:a 0 :b 1 :outcome "coexist"}
               {:a 0 :b 2 :outcome "type 0 beats type 2"}
               {:a 0 :b 3 :outcome "mutual destruction"}
               {:a 1 :b 2 :outcome "type 1 beats type 2"}
               {:a 1 :b 3 :outcome "type 1 beats type 3"}
               {:a 2 :b 3 :outcome "mutual destruction"}]
    :description-bits 220}])

(def all-encodings
  (vec (concat [go-encoding organism-encoding journey-encoding] discovered-encodings)))
