(ns organism.oroboros.ruleset
  "RuleSet: compact description of a parameterized abstract board game.
   Games are defined as programs (predicate/effect ASTs) interpreted by the engine.")

;; ── Game presets ────────────────────────────────────────────────────────────

(def go-game
  {:name "Go"
   :richness 999
   :description "Place stones, capture groups with no liberties, score by territory."
   :ruleset {:schema {:topology_type "square" :topology_size 7
                      :num_piece_types 1 :num_players 2
                      :num_resource_slots 0 :num_global_counters 2}
             :conflict_table []
             :rules [{:predicate [20 [6 99] [1 1]]  ;; AND(DIST_LEQ(99), EMPTY(TGT))
                      :effect [2 1 0 0]              ;; PLACE_PIECE(TGT, SELF, 0)
                      :requires_piece false}]
             :interactions [{:trigger "adjacency"
                             :predicate [20 [3 1 1] [13 1]]  ;; AND(OWNER_IS(TGT,ENEMY), GROUP_NO_LIBERTY(TGT))
                             :effect [20 [12 1 -1] [11 1]]}] ;; SEQ(INC_COUNTER_BY_GROUP(TGT,-1), REMOVE_GROUP(TGT))
             :terminations [{:predicate [33 50]      ;; ROUND_GEQ(50)
                             :winner_mode 4}]        ;; W_TERRITORY
             :actions_per_turn 1 :turn_limit 50 :placement "empty"}})

(def organism-game
  {:name "Organism"
   :richness 998
   :description "3-type heterarchy on ring board. Move, eat, grow, circulate. Capture to win."
   :ruleset {:schema {:topology_type "radial" :topology_size 5 :topology_symmetry 5
                      :num_piece_types 3 :num_players 2
                      :num_resource_slots 1 :num_global_counters 2}
             :conflict_table [1 2 1]
             :rules [;; Move
                     {:predicate [20 [5] [20 [3 0 0] [1 1]]]
                      :effect [3 0 1] :requires_piece true}
                     ;; Eat
                     {:predicate [20 [5] [20 [3 0 0] [7 1 0 1]]]
                      :effect [8 0 0] :requires_piece true}
                     ;; Grow
                     {:predicate [20 [5] [20 [20 [3 0 0] [7 0 0 1]] [1 1]]]
                      :effect [20 [6 0 0 -1] [2 1 0 -10]]
                      :requires_piece true}
                     ;; Circulate
                     {:predicate [20 [5] [20 [20 [3 0 0] [7 0 0 1]] [3 1 0]]]
                      :effect [7 0 1 0 1] :requires_piece true}]
             :interactions [{:trigger "adjacency"
                             :predicate [20 [3 1 1] [9]]
                             :effect [20 [1 1] [9 -1 1]]}]
             :terminations [{:predicate [30 0 5] :winner_mode 0}
                            {:predicate [30 1 5] :winner_mode 0}
                            {:predicate [33 100] :winner_mode 1}]
             :actions_per_turn 1 :turn_limit 100 :placement "spread"}})

(def discovered-games [go-game organism-game])
