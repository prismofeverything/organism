(ns organism.universal.game-v2
  "V2 game engine: any topology × any action types × any win condition.
   Ports alphazero.oroboros.game_v2 to Clojure for shared client/server use."
  (:require
   [organism.universal.topology :as topo]
   [organism.universal.ruleset-v2 :as rs2]))

(declare initial-state)
(declare resolve-interactions)

;; ── Board + initial state ────────────────────────────────────────────────────────

(defn create-game
  "Create a game from a v2 ruleset. Returns {:ruleset :topology :state}."
  [ruleset]
  (let [board (:board ruleset)
        topology (topo/build-topology
                  (:topology board) (:size board) (:symmetry board 4))
        state (initial-state ruleset topology)]
    {:ruleset ruleset :topology topology :state state}))

(defn initial-state
  "Create initial game state."
  [ruleset topology]
  (let [players (mapv str (range (:num-players ruleset)))
        nodes (:nodes topology)
        n (count nodes)
        epp (get-in ruleset [:pieces :elements-per-player] 3)
        num-types (get-in ruleset [:pieces :num-types] 2)
        init-food (get-in ruleset [:resources :initial-amount] 0)
        spacing (max 1 (quot n (:num-players ruleset)))
        ;; Place elements spread across the board
        elements
        (reduce
         (fn [els [p-idx player]]
           (reduce
            (fn [els e-idx]
              (let [idx (mod (+ (* p-idx spacing) 1 (* e-idx 2)) n)
                    ;; Find next unoccupied node
                    node (loop [i idx safety 0]
                           (if (and (< safety n) (get els (nth nodes i)))
                             (recur (mod (inc i) n) (inc safety))
                             (nth nodes i)))]
                (assoc els node {:player player
                                 :type (mod e-idx num-types)
                                 :food init-food})))
            els (range epp)))
         {} (map-indexed vector players))]
    {:round 0
     :elements elements
     :free-food {}
     :captures (zipmap players (repeat 0))
     :current-player (first players)
     :winner nil
     :turn-order players
     :actions-remaining (rs2/actions-per-turn ruleset)}))

;; ── Reachability ─────────────────────────────────────────────────────────────────

(defn reachable
  "Nodes reachable from source within max-range steps. Returns vec of node IDs."
  [topology source max-range can-jump? elements]
  (if (<= max-range 1)
    (vec (get-in topology [:adjacencies source] []))
    ;; BFS
    (loop [frontier [[source 0]] visited #{source} result []]
      (if (empty? frontier)
        result
        (let [[node dist] (first frontier)
              rest-frontier (rest frontier)]
          (if (>= dist max-range)
            (recur rest-frontier visited result)
            (let [neighbors (get-in topology [:adjacencies node] [])
                  new-entries
                  (reduce
                   (fn [acc nb]
                     (if (contains? visited nb)
                       acc
                       (if (or can-jump? (not (get elements nb)))
                         (conj acc [nb (inc dist)])
                         ;; Can't pass through occupied, but it's a valid target
                         (conj acc [nb (inc dist)]))))
                   [] neighbors)
                  new-ids (mapv first new-entries)
                  new-visited (into visited new-ids)]
              (recur (into (vec rest-frontier)
                           (if can-jump?
                             new-entries
                             (filterv (fn [[nb _]] (nil? (get elements nb))) new-entries)))
                     new-visited
                     (into result new-ids)))))))))

;; ── Target checking ──────────────────────────────────────────────────────────────

(defn- check-target
  "Check if target satisfies the action's target-owner filter."
  [state action-type player from-node target ruleset]
  (let [occupant (get-in state [:elements target])
        target-owner (:target-owner action-type)]
    (case target-owner
      "empty" (and (nil? occupant)
                   (if (= (:effect action-type) "collect")
                     (pos? (get-in state [:free-food target] 0))
                     true))
      "enemy" (and occupant
                   (not= (:player occupant) player)
                   (if (:requires-beats action-type)
                     (let [actor-el (get-in state [:elements from-node])
                           num-types (get-in ruleset [:pieces :num-types])
                           conflict (get-in ruleset [:interactions :conflict])]
                       (and actor-el
                            (= :wins (rs2/outcome-for
                                      num-types conflict
                                      (:type actor-el) (:type occupant)))))
                     true))
      "self"  (and occupant (= (:player occupant) player))
      "any"   true
      false)))

;; ── Effect application ───────────────────────────────────────────────────────────

(defn- apply-effect
  "Apply an action effect to the state. Returns updated state."
  [state effect-name from-node to-node action-type ruleset topology]
  (let [player (:current-player state)]
    (case effect-name
      "relocate"
      (if-let [el (get-in state [:elements from-node])]
        (let [free (get-in state [:free-food to-node] 0)
              el (update el :food + free)]
          (-> state
              (update :elements dissoc from-node)
              (assoc-in [:elements to-node] el)
              (update :free-food dissoc to-node)
              (resolve-interactions to-node player ruleset topology)))
        state)

      "remove_target"
      (if-let [target-el (get-in state [:elements to-node])]
        (let [dropped (+ 1 (get target-el :food 0))]
          (-> state
              (update :elements dissoc to-node)
              (update-in [:free-food to-node] (fnil + 0) dropped)
              (update-in [:captures player] (fnil inc 0))))
        state)

      "spawn"
      (let [spawn-type (let [st (:spawn-type action-type -1)]
                         (if (or (nil? st) (neg? st))
                           (get-in state [:elements from-node :type] 0)
                           st))]
        (-> state
            (assoc-in [:elements to-node] {:player player :type spawn-type :food 0})
            (resolve-interactions to-node player ruleset topology)))

      "collect"
      (let [food (get-in state [:free-food to-node] 0)]
        (-> state
            (update-in [:elements from-node :food] + (max food 1))
            (update :free-food dissoc to-node)))

      "transfer_resource"
      (if (get-in state [:elements to-node])
        (update-in state [:elements to-node :food] (fnil inc 0))
        state)

      "swap"
      (let [el-a (get-in state [:elements from-node])
            el-b (get-in state [:elements to-node])]
        (cond-> (update state :elements dissoc from-node to-node)
          el-a (assoc-in [:elements to-node] el-a)
          el-b (assoc-in [:elements from-node] el-b)))

      "push"
      (if-let [target-el (get-in state [:elements to-node])]
        (let [push-targets (filter
                            (fn [n] (and (not= n from-node)
                                         (not (get-in state [:elements n]))))
                            (get-in topology [:adjacencies to-node] []))]
          (if (seq push-targets)
            (let [dest (first push-targets)]
              (-> state
                  (update :elements dissoc to-node)
                  (assoc-in [:elements dest] target-el)))
            state))
        state)

      "convert"
      (if (get-in state [:elements to-node])
        (assoc-in state [:elements to-node :player] player)
        state)

      "place"
      (assoc-in state [:elements to-node] {:player player :type 0 :food 0})

      ;; Unknown effect: no-op
      state)))

;; ── Conflict resolution ──────────────────────────────────────────────────────────

(defn- resolve-interactions
  "Resolve conflicts at a space with adjacent enemies."
  [state space active-player ruleset topology]
  (let [trigger (get-in ruleset [:interactions :trigger] "adjacency")]
    (if (= trigger "explicit")
      state
      (if-not (get-in state [:elements space])
        state
        (reduce
         (fn [state adj]
           (if-not (get-in state [:elements space])
             (reduced state)
             (let [el (get-in state [:elements space])
                   other (get-in state [:elements adj])]
               (if (or (nil? other) (= (:player el) (:player other)))
                 state
                 (let [num-types (get-in ruleset [:pieces :num-types])
                       conflict (get-in ruleset [:interactions :conflict])
                       outcome (rs2/outcome-for num-types conflict (:type el) (:type other))]
                   (case outcome
                     :coexist state
                     :mutual
                     (let [state (reduce
                                  (fn [s sp]
                                    (if-let [e (get-in s [:elements sp])]
                                      (let [d (+ 1 (get e :food 0))]
                                        (-> s
                                            (update :elements dissoc sp)
                                            (update-in [:free-food sp] (fnil + 0) d)))
                                      s))
                                  state [space adj])]
                       (update-in state [:captures active-player] (fnil inc 0)))
                     :wins
                     (let [d (+ 1 (get other :food 0))]
                       (-> state
                           (update :elements dissoc adj)
                           (update-in [:free-food adj] (fnil + 0) d)
                           (update-in [:captures (:player el)] (fnil inc 0))))
                     :loses
                     (let [d (+ 1 (get el :food 0))]
                       (-> state
                           (update :elements dissoc space)
                           (update-in [:free-food space] (fnil + 0) d)
                           (update-in [:captures (:player other)] (fnil inc 0))))))))))
         state
         (get-in topology [:adjacencies space] []))))))

;; ── Win conditions ───────────────────────────────────────────────────────────────

(defn- score-leader
  "Player with best composite score. Random tiebreak via round parity."
  [state players round]
  (let [scored
        (map
         (fn [p]
           (let [pop (count (filter #(= p (:player %)) (vals (:elements state))))
                 caps (get-in state [:captures p] 0)
                 food (reduce + 0 (map #(get % :food 0)
                                       (filter #(= p (:player %)) (vals (:elements state)))))
                 ;; Tiebreak: alternate by player index + round
                 tie (/ (+ (.indexOf (vec players) p) round) 1000.0)]
             [(+ (* caps 10) (* pop 3) food tie) p]))
         players)]
    (second (apply max-key first scored))))

(defn- check-win
  "Check all win conditions. Returns state with :winner set if game over."
  [state ruleset players topology]
  (let [win (:win ruleset)
        condition (:condition win)
        threshold (:threshold win)]
    (or
     ;; Primary win condition
     (case condition
       "captures"
       (some (fn [p] (when (>= (get-in state [:captures p] 0) threshold)
                        (assoc state :winner p)))
             players)

       "population"
       (some (fn [p]
               (let [pop (count (filter #(= p (:player %)) (vals (:elements state))))]
                 (when (>= pop threshold)
                   (assoc state :winner p))))
             players)

       "elimination"
       (let [alive (filter (fn [p] (some #(= p (:player %)) (vals (:elements state)))) players)]
         (cond
           (= 1 (count alive)) (assoc state :winner (first alive))
           (= 0 (count alive)) (assoc state :winner (first players))
           :else nil))

       "territory"
       (let [total (max (count (:nodes topology)) 1)]
         (some (fn [p]
                 (let [controlled (count (filter #(= p (:player %)) (vals (:elements state))))]
                   (when (>= (* controlled 100) (* threshold total))
                     (assoc state :winner p))))
               players))

       "score_at_limit" nil  ;; handled by turn limit below

       nil)

     ;; Turn limit fallback
     (when (and (pos? (:turn-limit win 0))
                (>= (:round state) (:turn-limit win)))
       (assoc state :winner (score-leader state players (:round state))))

     ;; No winner yet
     state)))

;; ── Turn advancement ─────────────────────────────────────────────────────────────

(defn- advance-turn
  "Decrement actions-remaining; advance to next player when 0."
  [state ruleset players]
  (let [remaining (dec (get state :actions-remaining 1))]
    (if (pos? remaining)
      (assoc state :actions-remaining remaining)
      (let [idx (.indexOf (vec players) (:current-player state))
            nidx (mod (inc idx) (count players))
            new-round? (zero? nidx)
            state (assoc state
                         :current-player (nth players nidx)
                         :actions-remaining (rs2/actions-per-turn ruleset))]
        (cond-> state
          new-round? (update :round inc)
          ;; Energy regen on new round
          (and new-round? (= "energy" (get-in ruleset [:resources :system])))
          (update :elements
                  (fn [els]
                    (reduce-kv
                     (fn [m k v]
                       (assoc m k (update v :food + (get-in ruleset [:resources :regen-rate] 0))))
                     {} els))))))))

;; ── Core: apply action ───────────────────────────────────────────────────────────

(defn apply-action
  "Apply an action and advance turn. Returns next state."
  [ruleset topology state action-key]
  (let [[action-name from-node to-node] action-key
        action-type (rs2/get-action-type action-name)
        players (:turn-order state)
        state (if (= action-name "pass")
                state
                (let [state (if (and action-type
                                       (pos? (:resource-cost action-type 0))
                                       (number? from-node)
                                       (get-in state [:elements from-node]))
                                (update-in state [:elements from-node :food]
                                           #(max 0 (- (or % 0) (:resource-cost action-type))))
                                state)]
                    (apply-effect state (:effect action-type "relocate")
                                 from-node to-node action-type ruleset topology)))
        state (check-win state ruleset players topology)]
    (if (:winner state)
      state
      (advance-turn state ruleset players))))

;; ── Legal actions ────────────────────────────────────────────────────────────────

(defn legal-actions
  "Returns {[action-name from-node to-node] → next-state}."
  [ruleset topology state]
  (if (:winner state)
    {}
    (let [player (:current-player state)
          action-types (rs2/action-types-for ruleset)
          result (volatile! {})]

      (doseq [{:keys [name requires-piece max-range can-jump resource-cost] :as at}
              action-types]
        (if requires-piece
          (doseq [[node el] (:elements state)
                  :when (= (:player el) player)
                  :when (or (neg? (get at :actor-type-filter -1))
                            (= (:type el) (:actor-type-filter at)))
                  :when (>= (get el :food 0) (or resource-cost 0))]
            (doseq [target (reachable topology node (or max-range 1) can-jump (:elements state))
                    :when (check-target state at player node target ruleset)]
              (let [k [name node target]]
                (when-not (get @result k)
                  (vswap! result assoc k (apply-action ruleset topology state k))))))
          (doseq [target (:nodes topology)
                  :when (check-target state at player -1 target ruleset)]
            (let [k [name -1 target]]
              (when-not (get @result k)
                (vswap! result assoc k (apply-action ruleset topology state k)))))))

      ;; PASS always available
      (vswap! result assoc ["pass" nil nil]
              (apply-action ruleset topology state ["pass" nil nil]))

      @result)))

;; ── Public API ───────────────────────────────────────────────────────────────────

(defn current-player [state]
  (when-not (:winner state)
    (:current-player state)))
