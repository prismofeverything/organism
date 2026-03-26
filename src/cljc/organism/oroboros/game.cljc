(ns organism.oroboros.game
  "Game engine: interprets RuleSet programs.
   Ports alphazero.oroboros.game_v3 to Clojure for web play."
  (:require
   [organism.oroboros.interpreter :as interp]
   [organism.oroboros.topology :as topo]))

(declare initial-state)

;; ── Helpers ──────────────────────────────────────────────────────────────────

(defn- empty-node [num-res-slots]
  (reduce (fn [m i] (assoc m (str "resource_" i) 0))
          {:owner -1 :piece_type -1}
          (range num-res-slots)))

(defn- nodes-within
  "BFS: all nodes within max-range steps of src."
  [adjacencies src max-range]
  (if (<= max-range 1)
    (get adjacencies src [])
    (loop [frontier [[src 0]] visited #{src} result []]
      (if (empty? frontier) result
        (let [[node dist] (first frontier)
              rest-f (rest frontier)]
          (if (>= dist max-range)
            (recur rest-f visited result)
            (let [nbs (get adjacencies node [])
                  new-pairs (reduce (fn [acc nb]
                                      (if (contains? visited nb) acc
                                        (conj acc [nb (inc dist)])))
                                    [] nbs)
                  new-ids (mapv first new-pairs)]
              (recur (into (vec rest-f) new-pairs)
                     (into visited new-ids)
                     (into result new-ids)))))))))

(defn- predicate-max-range
  "Extract max distance from a predicate AST."
  [pred]
  (if (or (nil? pred) (not (sequential? pred)) (empty? pred)) 99
    (let [op (int (first pred))]
      (case op
        5  1     ;; P_ADJACENT
        6  (int (second pred))  ;; P_DISTANCE_LEQ
        20 (min (predicate-max-range (second pred))
                (predicate-max-range (nth pred 2)))
        21 (max (predicate-max-range (second pred))
                (predicate-max-range (nth pred 2)))
        99))))

;; ── Game creation ────────────────────────────────────────────────────────────

(defn create-game
  "Create a game from a ruleset map."
  [ruleset]
  (let [schema (:schema ruleset)
        topo-type (or (:topology_type schema) (:topology schema) "hex")
        topo-size (or (:topology_size schema) (:size schema) 4)
        topo-sym (or (:topology_symmetry schema) 4)
        topology (topo/build-topology topo-type topo-size topo-sym)
        state (initial-state ruleset topology)]
    {:ruleset ruleset :topology topology :state state}))

(defn initial-state
  [ruleset topology]
  (let [schema (:schema ruleset)
        num-players (or (:num_players schema) (:num-players schema) 2)
        num-types (or (:num_piece_types schema) (:num-piece-types schema) (:pieces schema) 2)
        num-res (or (:num_resource_slots schema) (:num-resource-slots schema) 0)
        num-counters (or (:num_global_counters schema) (:num-global-counters schema) 2)
        nodes (:nodes topology)
        n (count nodes)
        placement (or (:placement ruleset) "spread")
        ;; Initialize all nodes as empty
        node-map (reduce (fn [m node] (assoc m node (empty-node num-res)))
                         {} nodes)]
    (let [;; Place starting pieces for non-empty boards
          node-map
          (if (= placement "empty")
            node-map
            (let [epp (max 1 (min (quot n (* num-players 2)) 4))
                  spacing (max 1 (quot n num-players))]
              (reduce
               (fn [nm [p-idx player]]
                 (reduce
                  (fn [nm e-idx]
                    (let [idx (mod (+ (* p-idx spacing) 1 (* e-idx 2)) n)
                          node (loop [i idx safety 0]
                                 (let [nd (nth nodes (mod i n))]
                                   (if (and (< safety n) (>= (:owner (get nm nd)) 0))
                                     (recur (inc i) (inc safety))
                                     nd)))]
                      (assoc nm node {:owner p-idx
                                      :piece_type (mod e-idx (max num-types 1))
                                      :resource_0 1})))
                  nm (range epp)))
               node-map
               (map-indexed vector (range num-players)))))
          globals (reduce (fn [m i] (assoc m (str "counter_" i) 0))
                          {"round" 0
                           "current_player" 0
                           "actions_remaining" (or (:actions_per_turn ruleset)
                                                   (:actions-per-turn ruleset) 1)}
                          (range num-counters))]
      {:nodes node-map
       :globals globals
       :winner nil
       :turn_order (vec (range num-players))})))

;; ── Win checking ─────────────────────────────────────────────────────────────

(defn- score-leader [state num-players]
  (let [scores (for [p (range num-players)]
                 [(+ (* 10 (get-in state [:globals (str "counter_" p)] 0))
                     (* 3 (count (filter #(= p (:owner %)) (vals (:nodes state))))))
                  p])]
    (second (apply max-key first scores))))

(defn- find-territory-winner [state topology num-players]
  "Go-style territory + captures scoring."
  (let [scores (reduce (fn [m p] (assoc m p (get-in state [:globals (str "counter_" p)] 0)))
                       {} (range num-players))
        adjacencies (:adjacencies topology)]
    ;; Flood-fill empty regions
    (let [scores
          (loop [visited #{} remaining (keys (:nodes state)) scores scores]
            (if (empty? remaining) scores
              (let [node (first remaining)
                    rest-r (rest remaining)]
                (if (or (contains? visited node) (>= (get-in state [:nodes node :owner] -1) 0))
                  (recur (conj visited node) rest-r scores)
                  ;; BFS empty region
                  (let [[region borders]
                        (loop [frontier [node] vis #{} region #{} borders #{}]
                          (if (empty? frontier) [region borders]
                            (let [n (first frontier) rest-f (rest frontier)]
                              (if (contains? vis n) (recur rest-f vis region borders)
                                (let [nd (get-in state [:nodes n])
                                      owner (get nd :owner -1)]
                                  (if (>= owner 0)
                                    (recur rest-f (conj vis n) region (conj borders owner))
                                    (recur (into (vec rest-f) (get adjacencies n []))
                                           (conj vis n) (conj region n) borders)))))))]
                    (let [new-scores (if (= 1 (count borders))
                                      (update scores (first borders) + (count region))
                                      scores)]
                      (recur (into visited region) rest-r new-scores)))))))]
      (second (apply max-key (fn [[p s]] s) scores)))))

(defn- check-termination [state ruleset topology player]
  (let [num-players (or (:num_players (:schema ruleset))
                        (:num-players (:schema ruleset)) 2)
        turn-limit (or (:turn_limit ruleset) (:turn-limit ruleset) 50)]
    ;; Check each termination spec
    (or
     (some
      (fn [ts]
        (let [pred (or (:predicate ts) (first ts))]
          (when (interp/eval-termination pred state num-players player)
            (let [winner-mode (or (:winner_mode ts) (:winner-mode ts) 0)]
              (case (int winner-mode)
                0 player  ;; triggering player
                1 (score-leader state num-players) ;; highest counter
                2 (second (apply max-key
                            (fn [[p c]] c)
                            (for [p (range num-players)]
                              [p (count (filter #(= p (:owner %)) (vals (:nodes state))))])))
                3 (let [alive (into #{} (keep #(let [o (:owner %)] (when (>= o 0) o))
                                              (vals (:nodes state))))]
                    (if (= 1 (count alive)) (first alive) 0))
                4 (find-territory-winner state topology num-players)
                player)))))
      (or (:terminations ruleset) []))
     ;; Turn limit fallback
     (when (>= (get-in state [:globals "round"] 0) turn-limit)
       (score-leader state num-players)))))

;; ── Turn advancement ─────────────────────────────────────────────────────────

(defn- advance-turn [state ruleset]
  (let [num-players (or (:num_players (:schema ruleset))
                        (:num-players (:schema ruleset)) 2)
        apt (or (:actions_per_turn ruleset) (:actions-per-turn ruleset) 1)
        remaining (dec (get-in state [:globals "actions_remaining"] 1))]
    (if (pos? remaining)
      (assoc-in state [:globals "actions_remaining"] remaining)
      (let [cp (get-in state [:globals "current_player"] 0)
            nidx (mod (inc cp) num-players)
            new-round? (zero? nidx)]
        (cond-> state
          true (assoc-in [:globals "current_player"] nidx)
          true (assoc-in [:globals "actions_remaining"] apt)
          new-round? (update-in [:globals "round"] inc))))))

;; ── Apply a rule ─────────────────────────────────────────────────────────────

(defn- apply-rule [state ruleset topology rule src tgt player]
  (let [adjacencies (:adjacencies topology)
        num-res (or (:num_resource_slots (:schema ruleset))
                    (:num-resource-slots (:schema ruleset)) 0)
        conflict (or (:conflict_table ruleset) (:conflict-table ruleset) [])
        num-types (or (:num_piece_types (:schema ruleset))
                      (:num-piece-types (:schema ruleset))
                      (:pieces (:schema ruleset)) 2)
        ;; Pay resource cost if any
        cost (or (:resource_cost rule) (:resource-cost rule) 0)
        state (if (and (pos? cost) (>= src 0) (get-in state [:nodes src]))
                (update-in state [:nodes src "resource_0"]
                           #(max 0 (- (or % 0) cost)))
                state)
        ;; Apply effect
        effect (or (:effect rule) [0])
        state (interp/apply-effect effect state adjacencies src tgt player num-res)
        ;; Resolve adjacency interactions
        state (reduce
               (fn [state ir]
                 (let [trigger (or (:trigger ir) "adjacency")]
                   (if (not= trigger "adjacency") state
                     (let [affected tgt]
                       (reduce
                        (fn [state adj]
                          (let [nd-a (get-in state [:nodes affected])
                                nd-b (get-in state [:nodes adj])]
                            (if (or (nil? nd-a) (nil? nd-b)
                                    (neg? (:owner nd-a)) (neg? (:owner nd-b))
                                    (= (:owner nd-a) (:owner nd-b)))
                              state
                              (if (interp/eval-predicate
                                   (:predicate ir) state adjacencies affected adj player
                                   conflict num-types)
                                (interp/apply-effect (:effect ir) state adjacencies
                                                     affected adj player num-res)
                                state))))
                        state (get adjacencies affected []))))))
               state (or (:interactions ruleset) []))
        ;; Check win
        winner (check-termination state ruleset topology player)]
    (if winner
      (assoc state :winner winner)
      (advance-turn state ruleset))))

;; ── Legal actions ────────────────────────────────────────────────────────────

(defn legal-actions
  "Returns {[rule-idx src tgt] → next-state}."
  [ruleset topology state]
  (if (:winner state) {}
    (let [player (get-in state [:globals "current_player"] 0)
          adjacencies (:adjacencies topology)
          conflict (or (:conflict_table ruleset) (:conflict-table ruleset) [])
          num-types (or (:num_piece_types (:schema ruleset))
                        (:num-piece-types (:schema ruleset))
                        (:pieces (:schema ruleset)) 2)
          rules (vec (or (:rules ruleset) []))
          result (volatile! {})]

      (doseq [[slot rule] (map-indexed vector rules)]
        (let [requires-piece (if (contains? rule :requires_piece)
                               (:requires_piece rule)
                               (:requires-piece rule true))
              pred (:predicate rule)
              max-range (predicate-max-range pred)]
          (if requires-piece
            ;; For each owned piece
            (doseq [[node nd] (:nodes state)
                    :when (= (:owner nd) player)]
              (doseq [target (nodes-within adjacencies node max-range)
                      :when (interp/eval-predicate pred state adjacencies node target
                                                   player conflict num-types)]
                (let [k [slot node target]]
                  (when-not (get @result k)
                    (vswap! result assoc k
                            (apply-rule (assoc state :_deep true) ruleset topology
                                        rule node target player))))))
            ;; Placement: no source needed
            (doseq [target (:nodes topology)
                    :when (interp/eval-predicate pred state adjacencies target target
                                                 player conflict num-types)]
              (let [k [slot -1 target]]
                (when-not (get @result k)
                  (vswap! result assoc k
                          (apply-rule (assoc state :_deep true) ruleset topology
                                      rule -1 target player))))))))

      ;; PASS always available
      (vswap! result assoc ["pass" nil nil]
              (advance-turn (dissoc state :_deep) ruleset))

      @result)))

;; ── Public API ───────────────────────────────────────────────────────────────

(defn current-player [state]
  (when-not (:winner state)
    (str (get-in state [:globals "current_player"] 0))))
