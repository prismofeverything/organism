(ns organism.oroboros.interpreter
  "AST interpreter for v3 predicate and effect evaluation.
   Ports alphazero.oroboros.interpreter to Clojure.")

;; ── Opcodes (must match schema.py) ───────────────────────────────────────────

(def SRC 0) (def TGT 1) (def SELF 0) (def ENEMY 1) (def ANY_OWNER 2)

;; Predicates
(def P_TRUE 0) (def P_EMPTY 1) (def P_OCCUPIED 2) (def P_OWNER_IS 3)
(def P_TYPE_IS 4) (def P_ADJACENT 5) (def P_DISTANCE_LEQ 6)
(def P_RESOURCE_GEQ 7) (def P_GLOBAL_GEQ 8) (def P_TYPE_BEATS 9)
(def P_SAME_TYPE 10) (def P_DIFF_TYPE 11)
(def P_GROUP_HAS_LIBERTY 12) (def P_GROUP_NO_LIBERTY 13)
(def P_GROUP_SIZE_GEQ 14) (def P_GROUP_TYPES_GEQ 15)
(def P_AND 20) (def P_OR 21) (def P_NOT 22)

;; Effects
(def E_NOP 0) (def E_REMOVE_PIECE 1) (def E_PLACE_PIECE 2)
(def E_MOVE_PIECE 3) (def E_SWAP_PIECES 4) (def E_PUSH_PIECE 5)
(def E_ADD_RESOURCE 6) (def E_TRANSFER_RES 7) (def E_COLLECT_RES 8)
(def E_INC_COUNTER 9) (def E_SET_RESOURCE 10) (def E_REMOVE_GROUP 11)
(def E_INC_COUNTER_BY_GROUP 12) (def E_SEQ 20) (def E_IF 21)

;; Termination
(def T_COUNTER_GEQ 30) (def T_NO_PIECES 31) (def T_PIECE_COUNT_GEQ 32)
(def T_ROUND_GEQ 33) (def T_ONE_PLAYER_LEFT 34)

;; Type refs
(def COPY_SRC_TYPE -10)

;; ── Helpers ──────────────────────────────────────────────────────────────────

(defn- empty-node [num-resource-slots]
  (let [nd {:owner -1 :piece_type -1}]
    (reduce (fn [m i] (assoc m (str "resource_" i) 0))
            nd (range num-resource-slots))))

(defn- node-owner [state node]
  (get-in state [:nodes node :owner] -1))

(defn- node-type [state node]
  (get-in state [:nodes node :piece_type] -1))

(defn- bfs-distance [adjacencies a b]
  (if (= a b) 0
    (loop [frontier [[a 0]] visited #{a}]
      (if (empty? frontier) 999
        (let [[node dist] (first frontier)
              rest-f (rest frontier)]
          (let [nbs (get adjacencies node [])
                found (some #(when (= % b) (inc dist)) nbs)]
            (if found found
              (let [new-pairs (reduce (fn [acc nb]
                                        (if (contains? visited nb) acc
                                          (conj acc [nb (inc dist)])))
                                      [] nbs)]
                (recur (into (vec rest-f) new-pairs)
                       (into visited (map first new-pairs)))))))))))

(defn- find-group
  "BFS to find connected same-owner pieces."
  [state adjacencies node]
  (let [owner (node-owner state node)]
    (if (neg? owner) #{}
      (loop [frontier [node] visited #{}]
        (if (empty? frontier) visited
          (let [n (first frontier)
                rest-f (rest frontier)]
            (if (contains? visited n)
              (recur rest-f visited)
              (let [n-owner (node-owner state n)]
                (if (not= n-owner owner)
                  (recur rest-f visited)
                  (let [nbs (get adjacencies n [])
                        new-nbs (remove visited nbs)]
                    (recur (into (vec rest-f) new-nbs)
                           (conj visited n))))))))))))

(defn- group-liberties [state adjacencies group]
  (count
   (into #{}
     (for [n group
           nb (get adjacencies n [])
           :when (not (contains? group nb))
           :when (neg? (node-owner state nb))]
       nb))))

(defn- group-types [state group]
  (into #{} (for [n group
                   :let [t (node-type state n)]
                   :when (>= t 0)] t)))

;; ── Conflict resolution ──────────────────────────────────────────────────────

(defn- conflict-index [num-types i j]
  (let [a (min i j) b (max i j)]
    (loop [idx 0 x 0]
      (if (= x a) (+ idx (- b a 1))
        (recur (+ idx (- num-types 1 x)) (inc x))))))

(defn- conflict-outcome [conflict-table num-types attacker defender]
  (if (= attacker defender) :coexist
    (let [idx (conflict-index num-types attacker defender)
          raw (get conflict-table idx 0)]
      (case (int raw)
        0 :coexist
        3 :mutual
        (let [i (min attacker defender)
              winner (if (= raw 1) i (max attacker defender))]
          (if (= winner attacker) :wins :loses))))))

;; ── Predicate evaluation ─────────────────────────────────────────────────────

(defn eval-predicate
  [pred state adjacencies src tgt player conflict-table num-types]
  (if (or (nil? pred) (not (sequential? pred)) (empty? pred)) true
    (let [op (int (first pred))]
      (case op
        0  true ;; P_TRUE
        1  (let [n (if (= (second pred) SRC) src tgt)]
             (neg? (node-owner state n)))
        2  (let [n (if (= (second pred) SRC) src tgt)]
             (>= (node-owner state n) 0))
        3  (let [n (if (= (second pred) SRC) src tgt)
                 owner (node-owner state n)
                 ref (int (nth pred 2))]
             (cond (= ref SELF) (= owner player)
                   (= ref ENEMY) (and (>= owner 0) (not= owner player))
                   (= ref ANY_OWNER) (>= owner 0)
                   :else (= owner ref)))
        4  (let [n (if (= (second pred) SRC) src tgt)]
             (= (node-type state n) (int (nth pred 2))))
        5  (some #{tgt} (get adjacencies src []))
        6  (<= (bfs-distance adjacencies src tgt) (int (second pred)))
        7  (let [n (if (= (second pred) SRC) src tgt)
                 slot (str "resource_" (nth pred 2))
                 threshold (int (nth pred 3))]
             (>= (get-in state [:nodes n slot] 0) threshold))
        8  (let [idx (int (second pred)) threshold (int (nth pred 2))
                 actual-idx (if (neg? idx) player idx)]
             (>= (get-in state [:globals (str "counter_" actual-idx)] 0) threshold))
        9  (let [st (node-type state src) tt (node-type state tgt)]
             (and (>= st 0) (>= tt 0) (not= st tt)
                  (= :wins (conflict-outcome conflict-table num-types st tt))))
        10 (let [st (node-type state src) tt (node-type state tgt)]
             (and (>= st 0) (>= tt 0) (= st tt)))
        11 (let [st (node-type state src) tt (node-type state tgt)]
             (and (>= st 0) (>= tt 0) (not= st tt)))
        ;; Group predicates
        12 (let [n (if (= (second pred) SRC) src tgt)
                 g (find-group state adjacencies n)]
             (and (seq g) (pos? (group-liberties state adjacencies g))))
        13 (let [n (if (= (second pred) SRC) src tgt)
                 g (find-group state adjacencies n)]
             (and (seq g) (zero? (group-liberties state adjacencies g))))
        14 (let [n (if (= (second pred) SRC) src tgt)
                 g (find-group state adjacencies n)]
             (>= (count g) (int (nth pred 2 1))))
        15 (let [n (if (= (second pred) SRC) src tgt)
                 g (find-group state adjacencies n)]
             (>= (count (group-types state g)) (int (nth pred 2 2))))
        ;; Combinators
        20 (and (eval-predicate (second pred) state adjacencies src tgt player conflict-table num-types)
                (eval-predicate (nth pred 2) state adjacencies src tgt player conflict-table num-types))
        21 (or  (eval-predicate (second pred) state adjacencies src tgt player conflict-table num-types)
                (eval-predicate (nth pred 2) state adjacencies src tgt player conflict-table num-types))
        22 (not (eval-predicate (second pred) state adjacencies src tgt player conflict-table num-types))
        ;; default
        true))))

;; ── Effect application ───────────────────────────────────────────────────────

(defn apply-effect
  [eff state adjacencies src tgt player num-res-slots]
  (if (or (nil? eff) (not (sequential? eff)) (empty? eff)) state
    (let [op (int (first eff))]
      (case op
        0  state ;; NOP
        1  (let [n (if (= (second eff) SRC) src tgt)]
             (assoc-in state [:nodes n] (empty-node num-res-slots)))
        2  (let [n (if (= (second eff) SRC) src tgt)
                 owner-ref (int (nth eff 2 SELF))
                 type-ref (int (nth eff 3 0))
                 owner (if (= owner-ref SELF) player owner-ref)
                 ptype (cond (= type-ref COPY_SRC_TYPE) (node-type state src)
                             (neg? type-ref) 0
                             :else type-ref)
                 nd (assoc (empty-node num-res-slots) :owner owner :piece_type ptype)]
             (assoc-in state [:nodes n] nd))
        3  (let [from-n (if (= (second eff) SRC) src tgt)
                 to-n (if (= (nth eff 2) TGT) tgt src)
                 nd (get-in state [:nodes from-n])]
             (if (and nd (>= (:owner nd) 0))
               (-> state
                   (assoc-in [:nodes to-n] nd)
                   (assoc-in [:nodes from-n] (empty-node num-res-slots)))
               state))
        4  (let [nd-s (get-in state [:nodes src] (empty-node num-res-slots))
                 nd-t (get-in state [:nodes tgt] (empty-node num-res-slots))]
             (-> state
                 (assoc-in [:nodes src] nd-t)
                 (assoc-in [:nodes tgt] nd-s)))
        5  (let [which-n (if (= (second eff) SRC) src tgt)
                 away-n (if (= (nth eff 2) SRC) src tgt)
                 nd (get-in state [:nodes which-n])]
             (if (and nd (>= (:owner nd) 0))
               (let [candidates (filter (fn [n] (and (not= n away-n)
                                                     (neg? (node-owner state n))))
                                        (get adjacencies which-n []))]
                 (if (seq candidates)
                   (-> state
                       (assoc-in [:nodes (first candidates)] nd)
                       (assoc-in [:nodes which-n] (empty-node num-res-slots)))
                   state))
               state))
        6  (let [n (if (= (second eff) SRC) src tgt)
                 slot (str "resource_" (nth eff 2 0))
                 delta (int (nth eff 3 1))]
             (update-in state [:nodes n slot] (fn [v] (max 0 (+ (or v 0) delta)))))
        7  (let [from-n (if (= (second eff) SRC) src tgt)
                 to-n (if (= (nth eff 2) TGT) tgt src)
                 slot (str "resource_" (nth eff 3 0))
                 amount (int (nth eff 4 1))
                 actual (min amount (get-in state [:nodes from-n slot] 0))]
             (-> state
                 (update-in [:nodes from-n slot] #(- (or % 0) actual))
                 (update-in [:nodes to-n slot] #(+ (or % 0) actual))))
        8  (let [n (if (= (second eff) SRC) src tgt)
                 slot (str "resource_" (nth eff 2 0))
                 nd-tgt (get-in state [:nodes tgt])
                 free (if (and nd-tgt (neg? (:owner nd-tgt)))
                        (get nd-tgt slot 0) 0)]
             (if (pos? free)
               (-> state
                   (update-in [:nodes src slot] #(+ (or % 0) free))
                   (assoc-in [:nodes tgt slot] 0))
               state))
        9  (let [idx (int (nth eff 1 0))
                 delta (int (nth eff 2 1))
                 actual-idx (if (neg? idx) player idx)
                 key (str "counter_" actual-idx)]
             (update-in state [:globals key] #(+ (or % 0) delta)))
        10 (let [n (if (= (second eff) SRC) src tgt)
                 slot (str "resource_" (nth eff 2 0))
                 value (int (nth eff 3 0))]
             (assoc-in state [:nodes n slot] value))
        11 (let [n (if (= (second eff) SRC) src tgt)
                 g (find-group state adjacencies n)]
             (reduce (fn [s gn] (assoc-in s [:nodes gn] (empty-node num-res-slots)))
                     state g))
        12 (let [n (if (= (second eff) SRC) src tgt)
                 counter-idx (int (nth eff 2 0))
                 actual-idx (if (neg? counter-idx) player counter-idx)
                 g (find-group state adjacencies n)
                 key (str "counter_" actual-idx)]
             (update-in state [:globals key] #(+ (or % 0) (count g))))
        ;; SEQ
        20 (let [state (apply-effect (second eff) state adjacencies src tgt player num-res-slots)]
             (apply-effect (nth eff 2) state adjacencies src tgt player num-res-slots))
        ;; IF
        21 (if (eval-predicate (second eff) state adjacencies src tgt player () 0)
             (apply-effect (nth eff 2) state adjacencies src tgt player num-res-slots)
             (apply-effect (nth eff 3 [0]) state adjacencies src tgt player num-res-slots))
        ;; default
        state))))

;; ── Termination evaluation ───────────────────────────────────────────────────

(defn eval-termination [pred state num-players player]
  (if (or (nil? pred) (not (sequential? pred)) (empty? pred)) false
    (let [op (int (first pred))]
      (case op
        30 (let [idx (int (nth pred 1 0))
                 threshold (int (nth pred 2 1))]
             ;; Check all player counters
             (some (fn [p]
                     (>= (get-in state [:globals (str "counter_" p)] 0) threshold))
                   (range num-players)))
        31 (let [ref (int (nth pred 1 ENEMY))]
             (if (= ref SELF)
               (not-any? #(= player (:owner %)) (vals (:nodes state)))
               (some (fn [p]
                       (and (not= p player)
                            (not-any? #(= p (:owner %)) (vals (:nodes state)))))
                     (range num-players))))
        32 (let [threshold (int (nth pred 2 5))]
             (some (fn [p]
                     (>= (count (filter #(= p (:owner %)) (vals (:nodes state))))
                         threshold))
                   (range num-players)))
        33 (>= (get-in state [:globals "round"] 0) (int (second pred)))
        34 (let [alive (into #{} (keep #(let [o (:owner %)] (when (>= o 0) o))
                                       (vals (:nodes state))))]
             (<= (count alive) 1))
        false))))
