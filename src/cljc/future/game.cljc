(ns future.game
  "Game state and logic for Future.

   Board layers (can overlap on a space):
     :structures  {sid → {:piece type :owner player ...} | nil}
     :sundivers   {sid → [{:owner player :resource color|nil}]}
     :planets     {orbit → sid}

   Phases:
     :place-mothership  — player places mothership on beam, then takes action
     :pre-action        — optional mothership orbit shift
     :action            — choose move / convert / activate
     :moving            — executing move sub-steps
     :activating        — executing activation actions

   End of turn:
     1. Draw cards (min 1, more from activation)
     2. Last card → planet advance + unexhaust components
     3. Any flare → pull mothership toward sun
     4. 13th flare → everyone loses
     5. Mothership advances to flame space
     6. Flame placed in front of next player's mothership
     7. Check win condition"
  (:require
   [future.board :as board]))

;; ── Player colors ───────────────────────────────────────────────────────────

(def player-colors
  "Setup order: outermost → innermost."
  [:void :purple :blue :green :silver])

;; ── Cards ───────────────────────────────────────────────────────────────────

(def card-suits [:sun :silver :green :blue :purple :void])

(defn make-deck []
  (vec (for [suit card-suits, value (range 1 14)]
         {:suit suit :value value})))

(defn shuffle-deck [deck] (vec (shuffle deck)))

;; ── Planet advancement rates ────────────────────────────────────────────────

(def planet-advance-rate
  {:silver 1 :green 1 :blue 1 :purple 2 :void 3})

;; ── Resource market pricing ─────────────────────────────────────────────────

(def initial-resources {:silver 1 :green 2 :blue 3 :purple 4 :void 5})
(def initial-cities {:silver 0 :green 0 :blue 0 :purple 0 :void 0})

(defn resource-price
  "Cost to buy given how many are in market. nil = can't buy."
  [market-count]
  (case (int market-count)
    0 nil, 1 5, 2 3, 3 2, 4 1, 5 1, 1))

;; ── Station levels ──────────────────────────────────────────────────────────

(defn station-level [state orbit]
  (min 3 (inc (get-in state [:cities orbit] 0))))

(defn level-actions [level]
  (case (int level)
    1 {:base 1 :bonus 1}, 2 {:base 2 :bonus 1}, 3 {:base 3 :bonus 2},
    {:base 1 :bonus 1}))

;; ── Initial player state ────────────────────────────────────────────────────

(defn initial-player-state [color]
  {:color color, :mothership :supply, :habitat 8, :reserve 5,
   :energy-nodes 3, :towers 3, :components 7, :city-tokens 8,
   :links 21, :energy 5})

;; ── Solar network ───────────────────────────────────────────────────────────

(defn initial-solar-network []
  (into {} (for [o board/orbits] [o {:available [] :exhausted []}])))

;; ── Board queries ───────────────────────────────────────────────────────────

(defn structure-at [state sid] (get-in state [:structures sid]))
(defn sundivers-at [state sid] (get-in state [:sundivers sid] []))

(defn has-player-sundiver? [state sid player]
  (some #(= (:owner %) player) (sundivers-at state sid)))

(defn space-has-structure? [state sid]
  (some? (structure-at state sid)))

(defn player-mothership-space [state player]
  (let [ms (get-in state [:players player :mothership])]
    (when (and ms (not= ms :supply)) ms)))

(defn orbits-with-stations [state player]
  (into #{}
        (keep (fn [[sid struct]]
                (when (and struct (= (:owner struct) player)
                           (#{:energy-node :tower} (:piece struct)))
                  (board/space-orbit sid))))
        (:structures state)))

(defn orbit-has-city? [state orbit]
  (pos? (get-in state [:cities orbit] 0)))

;; ── Board mutations ─────────────────────────────────────────────────────────

(defn place-structure [state sid piece-map]
  (assoc-in state [:structures sid] piece-map))

(defn remove-structure [state sid]
  (assoc-in state [:structures sid] nil))

(defn add-sundiver [state sid sundiver-map]
  (update-in state [:sundivers sid] (fnil conj []) sundiver-map))

(defn remove-one-sundiver [state sid player]
  (let [divs (sundivers-at state sid)
        idx (first (keep-indexed (fn [i d] (when (= (:owner d) player) i)) divs))]
    (if idx
      (let [removed (nth divs idx)
            remaining (into (subvec divs 0 idx) (subvec divs (inc idx)))]
        [(assoc-in state [:sundivers sid] remaining) removed])
      [state nil])))

(defn remove-sundiver-with-resource [state sid player]
  (let [divs (sundivers-at state sid)
        idx (first (keep-indexed
                    (fn [i d] (when (and (= (:owner d) player) (:resource d)) i))
                    divs))]
    (if idx
      (let [removed (nth divs idx)
            remaining (into (subvec divs 0 idx) (subvec divs (inc idx)))]
        [(assoc-in state [:sundivers sid] remaining) removed])
      [state nil])))

;; ── Current player / flame ──────────────────────────────────────────────────

(defn current-player [state]
  (when-not (:winner state) (:flame state)))

(declare check-win end-turn)

(defn player-color [state player]
  (get-in state [:players player :color]))

;; ── Planet advancement ──────────────────────────────────────────────────────

(defn advance-planet
  "Advance a planet forward (CCW) by n spaces in its orbit."
  [state orbit n]
  (let [sid (get-in state [:planets orbit])
        new-sid (nth (iterate board/front-space sid) n)]
    (assoc-in state [:planets orbit] new-sid)))

(defn advance-all-planets
  "Advance every planet by its rate."
  [state]
  (reduce (fn [s orbit]
            (advance-planet s orbit (planet-advance-rate orbit)))
          state board/orbits))

;; ── Component unexhaustion ──────────────────────────────────────────────────

(defn unexhaust-components
  "Move all exhausted components of a color back to available."
  [state orbit-color]
  (let [exhausted (get-in state [:solar-network orbit-color :exhausted] [])]
    (-> state
        (update-in [:solar-network orbit-color :available] into exhausted)
        (assoc-in [:solar-network orbit-color :exhausted] []))))

(defn unexhaust-all-components [state]
  (reduce unexhaust-components state board/orbits))

;; ── Mothership movement ────────────────────────────────────────────────────

(defn move-mothership-to
  "Move a player's mothership to a new space."
  [state player new-sid]
  (let [old-sid (player-mothership-space state player)]
    (cond-> state
      old-sid (remove-structure old-sid)
      true    (place-structure new-sid {:piece :mothership :owner player})
      true    (assoc-in [:players player :mothership] new-sid))))

(defn pull-mothership-toward-sun
  "Flare effect: pull mothership one orbit inward (frontmost adjacent).
   If already in silver: lose ceil(energy/2), advance 1 space forward instead."
  [state player]
  (let [ms-space (player-mothership-space state player)]
    (if (nil? ms-space)
      state
      (let [orbit (board/space-orbit ms-space)]
        (if (= orbit :silver)
          ;; In silver: lose half energy (rounded up), advance 1 forward
          (let [energy (get-in state [:players player :energy] 0)
                loss (long (Math/ceil (/ energy 2.0)))
                new-space (board/front-space ms-space)]
            (-> state
                (update-in [:players player :energy] - loss)
                (move-mothership-to player new-space)))
          ;; Move inward
          (let [inner (board/inner-orbit orbit)
                adj (:adjacency (:board state))
                target (board/frontmost-adjacent-in-ring adj ms-space inner)]
            (if target
              (move-mothership-to state player target)
              state)))))))

(defn shift-mothership
  "Pre-action: shift mothership up (outward) or down (inward) one orbit."
  [state player direction]
  (let [ms-space (player-mothership-space state player)
        orbit (board/space-orbit ms-space)
        target-orbit (case direction
                       :up   (board/outer-orbit orbit)
                       :down (board/inner-orbit orbit))
        adj (:adjacency (:board state))]
    (if target-orbit
      (let [target (board/frontmost-adjacent-in-ring adj ms-space target-orbit)]
        (if target
          (-> state
              (move-mothership-to player target)
              ;; Flame follows: stays in front of mothership
              (assoc :flame-space (board/front-space target)))
          state))
      state)))

;; ── Card draw resolution ────────────────────────────────────────────────────

(defn resolve-card-draws
  "Draw n cards. For each flare: pull mothership. Last card: advance planet + unexhaust.
   Returns updated state with drawn cards in hand."
  [state player n]
  (if (or (zero? n) (empty? (:deck state)))
    state
    (let [actual-n (min n (count (:deck state)))
          cards (vec (take actual-n (:deck state)))
          remaining-deck (vec (drop actual-n (:deck state)))
          ;; Process flares
          state
          (reduce
           (fn [s card]
             (if (= :sun (:suit card))
               (-> s
                   (update :flares-drawn inc)
                   (pull-mothership-toward-sun player))
               s))
           state cards)
          ;; Check 13th flare = everyone loses
          state (if (>= (:flares-drawn state) 13)
                  (assoc state :winner :loss)
                  state)
          ;; Last card effects (planet advance + unexhaust)
          last-card (peek cards)
          state (if (and last-card (not= :loss (:winner state)))
                  (if (= :sun (:suit last-card))
                    ;; Flare: all planets advance, all unexhaust
                    (-> state advance-all-planets unexhaust-all-components)
                    ;; Planet card: that planet advances, that color unexhausts
                    (let [color (:suit last-card)]
                      (-> state
                          (advance-planet color (planet-advance-rate color))
                          (unexhaust-components color))))
                  state)]
      (-> state
          (assoc :deck remaining-deck)
          (update-in [:hands player] into cards)))))

;; ── End of turn ─────────────────────────────────────────────────────────────

(defn end-turn
  "Draw cards, advance mothership to flame, pass flame to next player."
  [state]
  (let [player (current-player state)
        draw-count (max 1 (or (:activation-card-draws state) 0))
        ;; 1. Draw cards and resolve effects
        state (resolve-card-draws state player draw-count)]
    (if (= :loss (:winner state))
      state  ; game over
      (let [;; 2. Mothership advances to flame space
            state (if-let [fs (:flame-space state)]
                    (move-mothership-to state player fs)
                    state)
            ;; 3. Clean up transient state
            state (-> state
                      (dissoc :moves-left :activate-type :actions-left
                              :activation-card-draws))
            ;; 4. Pass flame to next player
            order (:flame-order state)
            current (:flame state)
            idx (.indexOf order current)
            next-idx (mod (inc idx) (count order))
            next-player (nth order next-idx)
            state (-> state
                      (assoc :flame (nth order next-idx))
                      (update :turn inc))
            ;; 5. Place flame in front of next player's mothership
            next-ms (player-mothership-space state next-player)
            state (if next-ms
                    (assoc state :flame-space (board/front-space next-ms))
                    state)
            ;; 6. Check win condition
            state (let [result (check-win state)]
                    (if result
                      (assoc state :winner :win :scores result)
                      state))
            ;; 7. Set next phase
            next-placed? (not= :supply (get-in state [:players next-player :mothership]))]
        (assoc state :phase (if next-placed? :pre-action :place-mothership))))))

;; ── Win condition ───────────────────────────────────────────────────────────

(defn- link-graph-components
  "Find connected components of the link graph.
   Returns a seq of sets of space-ids."
  [links]
  (let [;; Build adjacency from links
        adj (reduce (fn [m {:keys [a b]}]
                      (-> m
                          (update a (fnil conj #{}) b)
                          (update b (fnil conj #{}) a)))
                    {} links)]
    ;; BFS to find components
    (loop [unvisited (set (keys adj)) components []]
      (if (empty? unvisited)
        components
        (let [start (first unvisited)
              component
              (loop [frontier [start] visited #{}]
                (if (empty? frontier)
                  visited
                  (let [node (first frontier)
                        rest-f (rest frontier)]
                    (if (contains? visited node)
                      (recur rest-f visited)
                      (recur (into (vec rest-f) (get adj node []))
                             (conj visited node))))))]
          (recur (reduce disj unvisited component)
                 (conj components component)))))))

(defn- cities-in-component
  "All city structures on spaces within the given component."
  [state component]
  (for [sid component
        :let [struct (structure-at state sid)]
        :when (and struct (= :city (:piece struct)))]
    {:space sid :color (:color struct) :orbit (board/space-orbit sid)
     :owner (:owner struct)}))

(defn check-win
  "Check if any connected link network contains a city in each ring AND each color.
   Returns score map {player → score} if won, nil otherwise."
  [state]
  (let [components (link-graph-components (:links state))]
    (some
     (fn [component]
       (let [cities (cities-in-component state component)
             orbit-set (set (map :orbit cities))
             color-set (set (map :color cities))]
         (when (and (= orbit-set (set board/orbits))
                    (= color-set (set board/orbits))) ; colors = orbits (non-sun)
           ;; Score: for each player, count viable links
           ;; A link is viable if both endpoints have cities and the player
           ;; either built it or has a component of that city's color
           (let [city-map (into {} (for [c cities] [(:space c) c]))]
             (into {}
                   (for [pk (:flame-order state)]
                     (let [player-components
                           ;; Colors where this player has components in solar network
                           (set (for [orbit board/orbits
                                      :let [section (get-in state [:solar-network orbit])]
                                      owner (concat (:available section)
                                                    (:exhausted section))
                                      :when (= owner pk)]
                                  orbit))
                           viable-links
                           (count
                            (filter
                             (fn [{:keys [a b owner]}]
                               (let [city-a (get city-map a)
                                     city-b (get city-map b)]
                                 (and city-a city-b
                                      (or (= owner pk)
                                          (and (or (= (:owner city-a) pk)
                                                   (contains? player-components (:color city-a)))
                                               (or (= (:owner city-b) pk)
                                                   (contains? player-components (:color city-b))))))))
                             (:links state)))]
                       [pk viable-links])))))))
     components)))

;; ── Game creation ───────────────────────────────────────────────────────────

(defn create-game [player-keys]
  (let [n (count player-keys)
        colors (vec (take n player-colors))
        brd (board/build-board)
        deck (shuffle-deck (make-deck))
        [hands remaining-deck]
        (reduce (fn [[h d] pk]
                  [(assoc h pk (vec (take 5 d))) (vec (drop 5 d))])
                [{} deck] player-keys)
        player-map (into {}
                         (map-indexed
                          (fn [i pk] [pk (initial-player-state (nth colors i))])
                          player-keys))
        solar (reduce
               (fn [sn [i pk]]
                 (let [orbit (nth board/orbits-reversed i)]
                   (update-in sn [orbit :available] conj pk)))
               (initial-solar-network)
               (map-indexed vector player-keys))
        player-map (reduce
                    (fn [pm pk] (update-in pm [pk :components] dec))
                    player-map player-keys)
        energy-pool (- 89 (* 5 n))
        empty-structures (into {} (map (fn [s] [s nil]) (board/all-spaces)))
        empty-sundivers (into {} (map (fn [s] [s []]) (board/all-spaces)))]
    {:board           brd
     :flame           (first player-keys)
     :flame-space     nil
     :flame-order     (vec player-keys)
     :turn            0
     :phase           :place-mothership
     :planets         (into {} (for [o board/orbits] [o (board/space-id o 0)]))
     :deck            remaining-deck
     :discard         []
     :hands           hands
     :energy-pool     energy-pool
     :resources       initial-resources
     :cities          initial-cities
     :resource-costs  initial-resources
     :structures      empty-structures
     :sundivers       empty-sundivers
     :solar-network   solar
     :links           []
     :flares-drawn    0
     :players         player-map
     :winner          nil}))

;; ── PHASE: Place mothership ─────────────────────────────────────────────────
;; Player places mothership on beam, flame in front, then proceeds to :action.

(defn place-mothership-actions [state]
  (let [player (current-player state)]
    (into {}
          (for [orbit board/orbits
                :let [sid (board/beam-space orbit)]
                :when (not (space-has-structure? state sid))]
            [[:place-mothership orbit]
             (let [flame-sid (board/front-space sid)]
               (-> state
                   (place-structure sid {:piece :mothership :owner player})
                   (assoc-in [:players player :mothership] sid)
                   (assoc :flame-space flame-sid)
                   ;; Stay on same player's turn — proceed to :action
                   (assoc :phase :action)))]))))

;; ── PHASE: Pre-action (mothership orbit shift) ─────────────────────────────

(defn pre-action-actions [state]
  (let [player (current-player state)
        ms-space (player-mothership-space state player)
        orbit (when ms-space (board/space-orbit ms-space))
        inner (when orbit (board/inner-orbit orbit))
        outer (when orbit (board/outer-orbit orbit))
        skip {[:no-shift] (assoc state :phase :action)}]
    (cond-> skip
      inner (assoc [:shift-down]
                   (-> state
                       (shift-mothership player :down)
                       (assoc :phase :action)))
      outer (assoc [:shift-up]
                   (-> state
                       (shift-mothership player :up)
                       (assoc :phase :action))))))

;; ── PHASE: Moving ───────────────────────────────────────────────────────────

(defn move-allowance [state player]
  (+ 3 (count (orbits-with-stations state player))))

(defn launch-targets [state player]
  (when-let [ms-space (player-mothership-space state player)]
    (let [adj (:adjacency (:board state))
          orbit (board/space-orbit ms-space)
          front (board/front-space ms-space)
          inner-front (when-let [io (board/inner-orbit orbit)]
                        (board/frontmost-adjacent-in-ring adj ms-space io))
          outer-front (when-let [oo (board/outer-orbit orbit)]
                        (board/frontmost-adjacent-in-ring adj ms-space oo))]
      (->> [ms-space front inner-front outer-front]
           (filter some?)
           vec))))

(defn fly-targets [state from]
  (vec (get-in state [:board :adjacency from] #{})))

(defn link-reachable [state from]
  (into []
        (keep (fn [{:keys [a b]}]
                (cond (= a from) b, (= b from) a, :else nil)))
        (:links state)))

(defn can-dive-into-sun? [state sid player]
  (and (= :silver (board/space-orbit sid))
       (some (fn [d] (and (= (:owner d) player) (:resource d)))
             (sundivers-at state sid))))

(defn dive-into-sun [state sid player]
  (let [[state removed] (remove-sundiver-with-resource state sid player)]
    (if removed
      (-> state
          (update-in [:players player :reserve] inc)
          (update-in [:solar-network (:resource removed) :available] conj player)
          (update-in [:players player :components] dec))
      state)))

(defn single-move-actions [state player]
  (let [launch-acts
        (when (pos? (get-in state [:players player :habitat] 0))
          (for [target (launch-targets state player)]
            [[:launch target]
             (-> state
                 (add-sundiver target {:owner player :resource nil})
                 (update-in [:players player :habitat] dec))]))
        fly-acts
        (for [[sid divs] (:sundivers state)
              :when (some #(= (:owner %) player) divs)
              target (fly-targets state sid)]
          [[:fly sid target]
           (let [[s removed] (remove-one-sundiver state sid player)]
             (add-sundiver s target removed))])
        link-acts
        (for [[sid divs] (:sundivers state)
              :when (some #(= (:owner %) player) divs)
              target (link-reachable state sid)]
          [[:fly-link sid target]
           (let [[s removed] (remove-one-sundiver state sid player)]
             (add-sundiver s target removed))])
        dive-acts
        (for [[sid _] (:sundivers state)
              :when (can-dive-into-sun? state sid player)]
          [[:dive sid] (dive-into-sun state sid player)])]
    (into {} (concat launch-acts fly-acts link-acts dive-acts))))

(defn enter-move-phase [state player]
  (assoc state :phase :moving :moves-left (move-allowance state player)))

(defn move-phase-actions [state]
  (let [player (current-player state)
        moves-left (:moves-left state)
        steps (when (pos? moves-left) (single-move-actions state player))
        step-actions
        (into {}
              (map (fn [[ak next-s]]
                     (let [ml (dec moves-left)]
                       [ak (if (zero? ml)
                             (end-turn next-s)
                             (assoc next-s :moves-left ml))])))
              steps)]
    (assoc step-actions [:done-moving] (end-turn state))))

;; ── PHASE: Convert ──────────────────────────────────────────────────────────

(defn- player-sundiver-spaces [state player]
  (into []
        (keep (fn [[sid divs]]
                (when (some #(= (:owner %) player) divs) sid)))
        (:sundivers state)))

(defn energy-node-patterns [state player]
  (let [diver-spaces (set (player-sundiver-spaces state player))]
    (for [sid1 diver-spaces
          :let [orbit (board/space-orbit sid1)
                n (board/orbit-sizes orbit)
                idx1 (board/space-index sid1)
                idx-mid (mod (dec idx1) n)
                idx2 (mod (- idx1 2) n)
                mid-space [orbit idx-mid]
                sid2 [orbit idx2]]
          :when (and (contains? diver-spaces sid2)
                     (< (hash sid1) (hash sid2))
                     (not (space-has-structure? state mid-space)))]
      {:sid1 sid1 :sid2 sid2 :mid mid-space})))

(defn tower-patterns [state player]
  (let [adj (:adjacency (:board state))
        diver-spaces (vec (player-sundiver-spaces state player))]
    (for [i (range (count diver-spaces))
          j (range (inc i) (count diver-spaces))
          k (range (inc j) (count diver-spaces))
          :let [s1 (nth diver-spaces i)
                s2 (nth diver-spaces j)
                s3 (nth diver-spaces k)]
          :when (and (not= (board/space-orbit s1) (board/space-orbit s2))
                     (not= (board/space-orbit s1) (board/space-orbit s3))
                     (not= (board/space-orbit s2) (board/space-orbit s3))
                     (contains? (get adj s1 #{}) s2)
                     (contains? (get adj s1 #{}) s3)
                     (contains? (get adj s2 #{}) s3))]
      {:sid1 s1 :sid2 s2 :sid3 s3})))

(defn- compute-activation-total [state spaces]
  (reduce + (map (fn [sid]
                   (let [orbit (board/space-orbit sid)
                         {:keys [base bonus]} (level-actions (station-level state orbit))]
                     (+ base bonus)))
                 spaces)))

(defn- compute-card-draws [state spaces]
  (reduce + (map (fn [sid]
                   (station-level state (board/space-orbit sid)))
                 spaces)))

(defn- activatable-of-type [state player piece-type]
  (for [[sid divs] (:sundivers state)
        :when (some #(= (:owner %) player) divs)
        :let [struct (structure-at state sid)]
        :when (and struct (= (:piece struct) piece-type))]
    sid))

(defn- enter-activation [state activate-type spaces]
  (let [total (compute-activation-total state spaces)
        draws (compute-card-draws state spaces)]
    (assoc state
           :phase :activating
           :activate-type activate-type
           :actions-left total
           :activation-card-draws draws)))

(defn- convert-with-activation [state player convert-fn station-type new-station-sid]
  (let [state (convert-fn state)
        other-spaces (vec (activatable-of-type state player
                                               (if (= station-type :nodes)
                                                 :energy-node :tower)))
        all-spaces (distinct (cons new-station-sid other-spaces))
        total (compute-activation-total state all-spaces)
        draws (compute-card-draws state all-spaces)]
    (if (pos? total)
      (assoc state
             :phase :activating
             :activate-type station-type
             :actions-left total
             :activation-card-draws draws)
      (end-turn state))))

(defn energy-node-conversions [state player]
  (when (pos? (get-in state [:players player :energy-nodes] 0))
    (for [{:keys [sid1 sid2 mid]} (energy-node-patterns state player)]
      [[:convert-energy-node sid1 sid2 mid]
       (let [do-convert
             (fn [s]
               (let [[s _] (remove-one-sundiver s sid1 player)
                     [s _] (remove-one-sundiver s sid2 player)]
                 (-> s
                     (place-structure mid {:piece :energy-node :owner player})
                     (update-in [:players player :reserve] + 2)
                     (update-in [:players player :energy-nodes] dec))))]
         (convert-with-activation state player do-convert :nodes mid))])))

(defn tower-conversions [state player]
  (when (pos? (get-in state [:players player :towers] 0))
    (for [{:keys [sid1 sid2 sid3]} (tower-patterns state player)
          tower-space [sid1 sid2 sid3]
          :when (not (space-has-structure? state tower-space))]
      [[:convert-tower sid1 sid2 sid3 tower-space]
       (let [do-convert
             (fn [s]
               (let [[s _] (remove-one-sundiver s sid1 player)
                     [s _] (remove-one-sundiver s sid2 player)
                     [s _] (remove-one-sundiver s sid3 player)]
                 (-> s
                     (place-structure tower-space {:piece :tower :owner player})
                     (update-in [:players player :reserve] + 3)
                     (update-in [:players player :towers] dec))))]
         (convert-with-activation state player do-convert :towers tower-space))])))

;; ── PHASE: Activate ─────────────────────────────────────────────────────────

;; --- Energy node actions: +2 energy per action

(defn node-activation-actions [state]
  (let [player (current-player state)
        left (:actions-left state)]
    (if (zero? left)
      {[:done-activating] (end-turn state)}
      {[:node-gain-energy]
       (let [ns (-> state
                    (update-in [:players player :energy] + 2)
                    (update :actions-left dec))]
         (if (= 1 left) (end-turn ns) ns))
       [:done-activating] (end-turn state)})))

;; --- Planet actions

(defn- planet-spaces-with-sundiver [state player]
  (for [[orbit sid] (:planets state)
        :when (has-player-sundiver? state sid player)]
    {:orbit orbit :sid sid}))

(defn planet-buy-resource-actions [state player]
  (for [{:keys [orbit sid]} (planet-spaces-with-sundiver state player)
        :let [market-count (get-in state [:resources orbit] 0)
              price (resource-price market-count)]
        :when (and price (>= (get-in state [:players player :energy] 0) price)
                   (some (fn [d] (and (= (:owner d) player) (nil? (:resource d))))
                         (sundivers-at state sid)))]
    (let [divs (sundivers-at state sid)
          idx (first (keep-indexed
                      (fn [i d] (when (and (= (:owner d) player) (nil? (:resource d))) i))
                      divs))
          updated-divs (assoc-in (vec divs) [idx :resource] orbit)]
      [[:planet-buy orbit sid]
       (let [ns (-> state
                    (update-in [:players player :energy] - price)
                    (update-in [:resources orbit] dec)
                    (assoc-in [:sundivers sid] updated-divs)
                    (update :actions-left dec))]
         (if (= 1 (:actions-left state)) (end-turn ns) ns))])))

(defn planet-build-city-actions [state player]
  (for [{:keys [orbit sid]} (planet-spaces-with-sundiver state player)
        :when (not (orbit-has-city? state orbit))
        diver (filter (fn [d] (and (= (:owner d) player)
                                    (:resource d)
                                    (not= (:resource d) orbit)))
                      (sundivers-at state sid))
        :let [res-color (:resource diver)]
        :when (pos? (get-in state [:players player :city-tokens] 0))]
    [[:planet-build-city orbit sid res-color]
     (let [[ns _] (remove-sundiver-with-resource state sid player)
           ns (-> ns
                  (update-in [:players player :reserve] inc)
                  (update-in [:resources orbit] inc)
                  (update-in [:cities orbit] inc)
                  (place-structure sid {:piece :city :color res-color :owner player})
                  (update-in [:players player :city-tokens] dec)
                  (update :actions-left dec))]
       (if (= 1 (:actions-left state)) (end-turn ns) ns))]))

(defn planet-activation-actions [state]
  (let [player (current-player state)
        left (:actions-left state)]
    (if (zero? left)
      {[:done-activating] (end-turn state)}
      (merge
       (into {} (planet-buy-resource-actions state player))
       (into {} (planet-build-city-actions state player))
       {[:done-activating] (end-turn state)}))))

;; --- Tower actions: links

(defn links-at-space [state sid]
  (count (filter (fn [{:keys [a b]}] (or (= a sid) (= b sid))) (:links state))))

(defn- available-components-of-color [state c]
  (get-in state [:solar-network c :available] []))

(defn- exhausted-components-of-color [state c]
  (get-in state [:solar-network c :exhausted] []))

(defn- has-exhausted-component? [state c]
  (seq (exhausted-components-of-color state c)))

(defn- exhaust-component [state orbit-color]
  (let [avail (available-components-of-color state orbit-color)]
    (when (seq avail)
      (let [owner (first avail)]
        [(-> state
             (update-in [:solar-network orbit-color :available] #(vec (rest %)))
             (update-in [:solar-network orbit-color :exhausted] conj owner))
         owner]))))

(defn- all-city-spaces [state]
  (for [[sid struct] (:structures state)
        :when (and struct (= :city (:piece struct)))]
    [sid struct]))

(defn- link-origins-for-city [state city-color]
  (let [city-sids (for [[sid struct] (:structures state)
                        :when (and struct (= :city (:piece struct))
                                   (= city-color (:color struct)))]
                    sid)]
    (loop [frontier (vec city-sids) visited #{} result (set city-sids)]
      (if (empty? frontier)
        result
        (let [node (first frontier) rest-f (rest frontier)]
          (if (contains? visited node)
            (recur rest-f visited result)
            (let [linked (keep (fn [{:keys [a b]}]
                                 (cond (= a node) b (= b node) a :else nil))
                               (:links state))]
              (recur (into (vec rest-f) linked)
                     (conj visited node)
                     (into result linked)))))))))

(defn- link-extension-targets [state origin]
  (let [adj (get-in state [:board :adjacency origin] #{})]
    (filterv (fn [t] (< (links-at-space state t) 2)) adj)))

(defn tower-link-actions [state player]
  (when (and (pos? (get-in state [:players player :energy] 0))
             (pos? (get-in state [:players player :links] 0)))
    (mapcat
     (fn [[_csid city-struct]]
       (let [cc (:color city-struct)
             has-exh (has-exhausted-component? state cc)
             can-exh (seq (available-components-of-color state cc))]
         (when (or has-exh can-exh)
           (let [origins (link-origins-for-city state cc)]
             (for [origin origins
                   target (link-extension-targets state origin)
                   :when (< (links-at-space state origin) 2)]
               (let [need-exh (not has-exh)]
                 [[:tower-link origin target cc need-exh]
                  (let [base (-> state
                                 (update-in [:players player :energy] dec)
                                 (update-in [:players player :links] dec)
                                 (update :links conj {:a origin :b target :owner player}))
                        [base comp-owner]
                        (if need-exh
                          (or (exhaust-component base cc) [base nil])
                          [base nil])
                        paying-owner (or comp-owner
                                         (first (exhausted-components-of-color base cc)))
                        base (if (and paying-owner (not= paying-owner player))
                               (update-in base [:players paying-owner :energy] inc)
                               base)
                        base (update base :actions-left dec)]
                    (if (= 1 (:actions-left state)) (end-turn base) base))]))))))
     (all-city-spaces state))))

(defn tower-activation-actions [state]
  (let [player (current-player state)
        left (:actions-left state)]
    (if (zero? left)
      {[:done-activating] (end-turn state)}
      (merge
       (into {} (tower-link-actions state player))
       {[:done-activating] (end-turn state)}))))

(defn activation-actions [state]
  (case (:activate-type state)
    :nodes   (node-activation-actions state)
    :planets (planet-activation-actions state)
    :towers  (tower-activation-actions state)
    {[:done-activating] (end-turn state)}))

;; ── PHASE: Action (top-level) ───────────────────────────────────────────────

(defn action-phase-actions [state]
  (let [player (current-player state)
        move-action {[:move] (enter-move-phase state player)}
        en-actions (into {} (energy-node-conversions state player))
        tw-actions (into {} (tower-conversions state player))
        activate-nodes
        (let [spaces (vec (activatable-of-type state player :energy-node))]
          (when (seq spaces)
            {[:activate :nodes] (enter-activation state :nodes spaces)}))
        activate-towers
        (let [spaces (vec (activatable-of-type state player :tower))]
          (when (seq spaces)
            {[:activate :towers] (enter-activation state :towers spaces)}))
        activate-planets
        (let [planet-sps (for [[_orbit sid] (:planets state)
                               :when (has-player-sundiver? state sid player)]
                           sid)]
          (when (seq planet-sps)
            {[:activate :planets] (enter-activation state :planets planet-sps)}))
        pass-action {[:pass] (end-turn state)}]
    (merge move-action en-actions tw-actions
           activate-nodes activate-towers activate-planets
           pass-action)))

;; ── Legal actions (dispatch) ────────────────────────────────────────────────

(defn legal-actions [state]
  (if (:winner state)
    {}
    (case (:phase state)
      :place-mothership (place-mothership-actions state)
      :pre-action       (pre-action-actions state)
      :action           (action-phase-actions state)
      :moving           (move-phase-actions state)
      :activating       (activation-actions state)
      {[:pass] (end-turn state)})))
