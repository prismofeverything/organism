(ns organism.universal.game
  "Universal game engine: one-action-per-turn board game on a ring board.
   Reuses organism.game for ring board geometry."
  (:require
   [organism.game :as og]
   [organism.universal.ruleset :as rs]))

;; ── Board building ──────────────────────────────────────────────────────────────

(def ring-labels
  ["core" "B" "C" "D" "E" "F" "G" "H"])

(defn build-board
  "Build ring board from a ruleset. Returns {:rings, :adjacencies, :all-spaces}."
  [{:keys [board-symmetry num-rings] :as ruleset}]
  (let [colors (take num-rings ring-labels)
        rings (og/build-rings board-symmetry colors)
        adjacencies (og/find-adjacencies rings)
        ;; Remove corner notches from outer ring
        outer-color (last colors)
        notches (when (> num-rings 3)
                  (set (map (fn [n] [outer-color (* n (dec num-rings))])
                            (range board-symmetry))))
        adjacencies (if notches
                      (reduce
                       (fn [adj notch]
                         (let [adj (dissoc adj notch)]
                           (reduce-kv
                            (fn [a space neighbors]
                              (assoc a space (vec (remove #{notch} neighbors))))
                            {} adj)))
                       adjacencies notches)
                      adjacencies)
        all-spaces (vec (keys adjacencies))]
    {:rings rings
     :adjacencies adjacencies
     :all-spaces all-spaces
     :notches (or notches #{})}))

;; ── State ───────────────────────────────────────────────────────────────────────

(defn initial-state
  "Create initial game state from a ruleset and board."
  [{:keys [num-players elements-per-player num-types food-initial
           board-symmetry num-rings] :as ruleset}
   {:keys [adjacencies all-spaces rings] :as board}]
  (let [players (mapv str (range num-players))
        ;; Place elements on outer ring, spaced evenly
        outer-ring (last rings)
        outer-color (first outer-ring)
        outer-spaces (filterv #(contains? adjacencies %) (second outer-ring))
        n-outer (count outer-spaces)
        spacing (max 1 (quot n-outer num-players))
        elements
        (reduce
         (fn [els [p-idx player]]
           (reduce
            (fn [els e-idx]
              (let [step (mod (+ (* p-idx spacing) 1 (* e-idx 2)) n-outer)
                    space (nth outer-spaces step)
                    ;; If space occupied, find next free
                    space (if (get els space)
                            (first (filter #(and (not (get els %))
                                                 (contains? adjacencies %))
                                           (drop step (cycle outer-spaces))))
                            space)
                    etype (mod e-idx num-types)]
                (assoc els space {:player player :type etype :food food-initial})))
            els (range elements-per-player)))
         {} (map-indexed vector players))]
    {:round 0
     :elements elements
     :free-food {}
     :captures (zipmap players (repeat 0))
     :current-player (first players)
     :winner nil
     :turn-order players}))

;; ── Helpers ─────────────────────────────────────────────────────────────────────

(defn advance-turn
  "Move to the next player's turn."
  [state players]
  (let [idx (.indexOf players (:current-player state))
        nidx (mod (inc idx) (count players))
        new-round (if (zero? nidx) (inc (:round state)) (:round state))]
    (assoc state
           :current-player (nth players nidx)
           :round new-round)))

(defn check-win
  "Check if any player has reached the win condition."
  [{:keys [win-type win-threshold] :as ruleset} state]
  (case win-type
    :captures
    (reduce-kv
     (fn [state player caps]
       (if (>= caps win-threshold)
         (reduced (assoc state :winner player))
         state))
     state (:captures state))

    :population
    (let [pop-counts (reduce-kv
                      (fn [m _ el] (update m (:player el) (fnil inc 0)))
                      {} (:elements state))]
      (reduce-kv
       (fn [state player pop]
         (if (>= pop win-threshold)
           (reduced (assoc state :winner player))
           state))
       state pop-counts))

    state))

(defn resolve-conflict-at
  "Resolve conflicts for element at space with adjacent enemies."
  [ruleset adjacencies state space active-player]
  (if-not (get-in state [:elements space])
    state
    (reduce
     (fn [state adj-space]
       (if-not (get-in state [:elements space])
         (reduced state) ; our element was destroyed
         (let [el (get-in state [:elements space])
               other (get-in state [:elements adj-space])]
           (if (or (nil? other) (= (:player el) (:player other)))
             state
             (let [outcome (rs/outcome-for ruleset (:type el) (:type other))]
               (case outcome
                 :coexist state

                 :mutual
                 (let [drop-el (+ 1 (:food el 0))
                       drop-oth (+ 1 (:food other 0))]
                   (-> state
                       (update :elements dissoc space adj-space)
                       (update-in [:free-food space] (fnil + 0) drop-el)
                       (update-in [:free-food adj-space] (fnil + 0) drop-oth)
                       (update-in [:captures active-player] (fnil inc 0))))

                 :wins
                 (let [dropped (+ 1 (:food other 0))]
                   (-> state
                       (update :elements dissoc adj-space)
                       (update-in [:free-food adj-space] (fnil + 0) dropped)
                       (update-in [:captures (:player el)] (fnil inc 0))))

                 :loses
                 (let [dropped (+ 1 (:food el 0))]
                   (-> state
                       (update :elements dissoc space)
                       (update-in [:free-food space] (fnil + 0) dropped)
                       (update-in [:captures (:player other)] (fnil inc 0))))))))))
     state
     (get adjacencies space []))))

;; ── Actions ─────────────────────────────────────────────────────────────────────

(defn apply-move
  "Move element from from-sp to to-sp."
  [ruleset adjacencies state from-sp to-sp]
  (let [el (get-in state [:elements from-sp])
        free (get-in state [:free-food to-sp] 0)
        el (update el :food + free)
        state (-> state
                  (update :elements dissoc from-sp)
                  (assoc-in [:elements to-sp] el)
                  (update :free-food dissoc to-sp))]
    (resolve-conflict-at ruleset adjacencies state to-sp (:current-player state))))

(defn apply-eat
  "Eat food from adjacent space."
  [_ruleset _adjacencies state from-sp to-sp]
  (let [food (get-in state [:free-food to-sp] 0)]
    (-> state
        (update-in [:elements from-sp :food] + (max food 1))
        (update :free-food dissoc to-sp))))

(defn apply-grow
  "Grow a new element at to-sp (costs 1 food from from-sp)."
  [ruleset adjacencies state from-sp to-sp grow-type]
  (let [state (-> state
                  (update-in [:elements from-sp :food] dec)
                  (assoc-in [:elements to-sp]
                            {:player (:current-player state)
                             :type grow-type
                             :food 0}))]
    (resolve-conflict-at ruleset adjacencies state to-sp (:current-player state))))

(defn apply-capture
  "Directly capture an enemy element."
  [_ruleset _adjacencies state _from-sp to-sp]
  (let [other (get-in state [:elements to-sp])
        dropped (+ 1 (get other :food 0))]
    (-> state
        (update :elements dissoc to-sp)
        (update-in [:free-food to-sp] (fnil + 0) dropped)
        (update-in [:captures (:current-player state)] (fnil inc 0)))))

(defn apply-circulate
  "Transfer 1 food from from-sp element to to-sp element."
  [_ruleset _adjacencies state from-sp to-sp]
  (-> state
      (update-in [:elements from-sp :food] dec)
      (update-in [:elements to-sp :food] inc)))

(defn apply-action
  "Apply an action and advance to next turn (unless game over)."
  [ruleset adjacencies state action]
  (let [[action-type from-sp to-sp & args] action
        state (case action-type
                :move      (apply-move ruleset adjacencies state from-sp to-sp)
                :eat       (apply-eat ruleset adjacencies state from-sp to-sp)
                :grow      (apply-grow ruleset adjacencies state from-sp to-sp (first args))
                :capture   (apply-capture ruleset adjacencies state from-sp to-sp)
                :circulate (apply-circulate ruleset adjacencies state from-sp to-sp)
                :pass      state)
        state (check-win ruleset state)]
    (if (:winner state)
      state
      (advance-turn state (:turn-order state)))))

;; ── Legal actions ───────────────────────────────────────────────────────────────

(defn legal-actions
  "Returns {[action-type from-space to-space ...] → next-state}."
  [{:keys [can-move can-eat can-grow can-capture can-circulate
           food-enabled num-types] :as ruleset}
   {:keys [adjacencies] :as board}
   state]
  (if (:winner state)
    {}
    (let [player (:current-player state)
          actions (transient {})]

      ;; For each owned element, compute available actions
      (doseq [[space el] (:elements state)
              :when (= (:player el) player)]
        (let [adj (get adjacencies space [])]

          ;; MOVE
          (when can-move
            (doseq [to-sp adj
                    :when (not (get-in state [:elements to-sp]))
                    :let [blocked?
                          (some
                           (fn [adj2]
                             (let [other (get-in state [:elements adj2])]
                               (and other
                                    (not= (:player other) player)
                                    (= (:type other) (:type el)))))
                           (get adjacencies to-sp []))]
                    :when (not blocked?)]
              (let [k [:move space to-sp]]
                (assoc! actions k
                        (apply-action ruleset adjacencies state k)))))

          ;; EAT
          (when (and can-eat food-enabled)
            (doseq [to-sp adj
                    :when (pos? (get-in state [:free-food to-sp] 0))]
              (let [k [:eat space to-sp]]
                (when-not (get actions k)
                  (assoc! actions k
                          (apply-action ruleset adjacencies state k))))))

          ;; GROW
          (when (and can-grow food-enabled (pos? (:food el 0)))
            (doseq [grow-type (range num-types)
                    to-sp adj
                    :when (not (get-in state [:elements to-sp]))]
              (let [k [:grow space to-sp grow-type]]
                (when-not (get actions k)
                  (assoc! actions k
                          (apply-action ruleset adjacencies state k))))))

          ;; CAPTURE
          (when can-capture
            (doseq [to-sp adj
                    :let [other (get-in state [:elements to-sp])]
                    :when (and other (not= (:player other) player))
                    :let [res (rs/outcome-for ruleset (:type el) (:type other))]
                    :when (= res :wins)]
              (let [k [:capture space to-sp]]
                (when-not (get actions k)
                  (assoc! actions k
                          (apply-action ruleset adjacencies state k))))))

          ;; CIRCULATE
          (when (and can-circulate food-enabled (pos? (:food el 0)))
            (doseq [to-sp adj
                    :let [other (get-in state [:elements to-sp])]
                    :when (and other
                               (= (:player other) player)
                               (< (:food other 0) 20))]
              (let [k [:circulate space to-sp]]
                (when-not (get actions k)
                  (assoc! actions k
                          (apply-action ruleset adjacencies state k))))))))

      ;; PASS (always available)
      (let [pass-state (apply-action ruleset adjacencies state [:pass nil nil])]
        (assoc! actions [:pass nil nil] pass-state))

      (persistent! actions))))

;; ── Game container ──────────────────────────────────────────────────────────────

(defn create-game
  "Create a game from a ruleset map. Returns {:ruleset, :board, :state}."
  [ruleset]
  (let [board (build-board ruleset)
        state (initial-state ruleset board)]
    {:ruleset ruleset
     :board board
     :state state}))

(defn current-player [state]
  (when-not (:winner state)
    (:current-player state)))
