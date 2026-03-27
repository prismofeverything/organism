(ns journey.choice
  (:require
   [journey.game :as game]))

(declare enter-cipher-phase)

;; Each choice function returns a map of {choice-key -> next-state}.
;; find-state returns [phase choices-map] for the current position in the turn.

(defn partial-map
  "Build a choices map by applying f to each option in s."
  [f s]
  (into {} (map (juxt identity f) s)))

;; --- move helpers ---

(defn mobile-positions
  "Positions on the board where the current player has sundivers that are not immobile."
  [state player]
  (filter
   (fn [pos]
     (and (pos? (get-in state [:board pos :sundivers player] 0))
          (not (game/immobile? state player pos))))
   (keys (:board state))))

(defn fly-destinations
  "All positions reachable by flying from from-pos: adjacent hexes plus ALL gate connections.
   Any player's gates can be used (the owner gets a fee, handled in fly-through-gate)."
  [state _player from-pos]
  (let [;; Collect gates from ALL players, not just the current player
        all-gate-connected
        (reduce
         (fn [acc p]
           (let [gates (get-in state [:players p :gates from-pos] #{})]
             (into acc gates)))
         #{}
         (:turn-order state))
        adjacent (set (game/hex-neighbors from-pos))]
    (when (seq all-gate-connected)
      (println "fly-destinations from:" from-pos
               "gates:" all-gate-connected
               "adjacent:" (count adjacent)))
    (into all-gate-connected adjacent)))

(defn use-move-point
  "Decrement :moves-remaining and return to :choose-move.
   When moves reach 0, choose-move-choices will only offer :done."
  [state]
  (-> state
      (update-in [:player-turn :action :moves-remaining] dec)
      (assoc-in [:player-turn :phase] :choose-move)))

;; --- move sub-phase choices ---

(defn choose-launch-destination-choices
  [state]
  (let [player (game/current-player state)
        ark    (:ark state)
        dir    (game/heading-direction state)
        ;; The 3 directional launch positions and their corresponding directions from the Ark
        flanks [(game/add-hex ark dir)
                (game/add-hex ark (game/rotate-ccw dir))
                (game/add-hex ark (game/rotate-cw dir))]
        flank-dirs [dir (game/rotate-ccw dir) (game/rotate-cw dir)]]
    (into {}
          (mapcat
           (fn [pos]
             (let [base-entry [pos (-> state (game/launch-sundiver player pos) use-move-point)]]
               (if-let [flank-idx (first (keep-indexed #(when (= %2 pos) %1) flanks))]
                 (let [fly-dir (nth flank-dirs flank-idx)]
                   (if (or (game/get-tile state pos)
                           ;; Don't wrap when tiles exist further along this ray
                           (some #(and (game/on-ray? ark fly-dir %)
                                       (> (game/hex-distance ark %) 1))
                                 (keys (:board state))))
                     [base-entry]
                     ;; Edge of known space: also offer wrap
                     (let [wrap-pos (game/wrap-target state ark fly-dir)]
                       [base-entry [[:wrap pos] (-> state (game/launch-sundiver player wrap-pos) use-move-point)]])))
                 ;; Ark position itself: no wrap
                 [base-entry])))
           (game/launch-positions state)))))

(defn choose-fly-from-choices
  [state]
  (let [player (game/current-player state)]
    (into {}
          (map (fn [pos]
                 [pos (-> state
                          (assoc-in [:player-turn :phase] :choose-fly-to)
                          (assoc-in [:player-turn :action :fly-from] pos))])
               (mobile-positions state player)))))

(defn choose-fly-to-choices
  [state]
  (let [player      (game/current-player state)
        from-pos    (get-in state [:player-turn :action :fly-from])
        after       (fn [s]
                      (-> s
                          (update-in [:player-turn :action] dissoc :fly-from)
                          use-move-point))]
    (into {}
          (mapcat
           (fn [to-pos]
             (let [owner (game/gate-owner state from-pos to-pos)
                   base-entry [to-pos (after (if owner
                                               (game/fly-through-gate state player from-pos to-pos)
                                               (game/fly-sundiver state player from-pos to-pos)))]]
               (if (and (nil? owner) (nil? (game/get-tile state to-pos))
                        (not (some #(and (game/on-ray? from-pos (nth game/hex-directions (game/adjacent-direction from-pos to-pos)) %)
                                         (> (game/hex-distance from-pos %) 1))
                                   (keys (:board state)))))
                 (let [dir-idx  (game/adjacent-direction from-pos to-pos)
                       fly-dir  (nth game/hex-directions dir-idx)
                       wrap-pos (game/wrap-target state from-pos fly-dir)]
                   [base-entry [[:wrap to-pos] (after (game/fly-sundiver state player from-pos wrap-pos))]])
                 [base-entry])))
           (fly-destinations state player from-pos)))))

(defn choose-move-choices
  "Options during the move action. Launch destinations and fly-from positions are
   inlined so they light up on the board directly. Done remains as a keyword choice.
   Launch keys are bare [q r] or [:wrap pos]. Fly-from keys are [:fly [q r]]."
  [state]
  (let [player    (game/current-player state)
        remaining (get-in state [:player-turn :action :moves-remaining] 0)
        habitat   (get-in state [:players player :habitat :sundivers] 0)
        mobile    (mobile-positions state player)
        ;; Inline launch destinations so they appear as clickable hexes
        launch-choices
        (when (and (pos? remaining) (pos? habitat))
          (try
            (let [launch-state (assoc-in state [:player-turn :phase] :choose-launch-destination)]
              (choose-launch-destination-choices launch-state))
            (catch #?(:clj Exception :cljs :default) _e nil)))
        ;; Inline fly-from positions with [:fly pos] tagged keys
        fly-from-choices
        (when (and (pos? remaining) (seq mobile))
          (try
            (let [fly-state (assoc-in state [:player-turn :phase] :choose-fly-from)]
              (into {}
                    (map (fn [[pos s]] [[:fly pos] s])
                         (choose-fly-from-choices fly-state))))
            (catch #?(:clj Exception :cljs :default) _e nil)))]
    (cond-> {:done (assoc-in state [:player-turn :phase] :draw-cards)}
      (seq launch-choices)    (merge launch-choices)
      (seq fly-from-choices)  (merge fly-from-choices))))

;; --- convert ---

(defn choose-convert-choices
  "Each valid conversion pattern maps to the state after performing it.
   Foundry patterns may appear twice (two possible target spaces) for the same pair."
  [state]
  (let [player (game/current-player state)]
    (into {}
          (map (fn [{:keys [type target sundivers] :as conversion}]
                 [conversion (game/convert state player type target sundivers)])
               (game/find-conversions state player)))))

;; --- activate ---

(defn choose-activate-choices
  "Choose which station type to activate."
  [state]
  (let [player (game/current-player state)]
    (partial-map
     (fn [stype] (game/start-activate state stype))
     (game/activatable-station-types state player))))

(defn choose-activate-station-choices
  "Player picks which station of the chosen type to activate next.
   Shows positions on the board. :done finishes activation."
  [state]
  (let [player       (game/current-player state)
        station-type (get-in state [:player-turn :action :station-type])
        activated    (get-in state [:player-turn :action :activated-stations] #{})
        ;; Find stations of this type with the player's sundiver, excluding already activated
        available    (remove activated
                            (game/stations-of-type-with-sundiver state player station-type))]
    (cond-> {:done (assoc-in state [:player-turn :phase] :draw-cards)}
      (seq available)
      (into (map (fn [pos]
                   [pos (-> state
                            (assoc-in [:player-turn :action :stations-queue] [pos])
                            (assoc-in [:player-turn :action :free-activation?] false)
                            (update-in [:player-turn :action :activated-stations] (fnil conj #{}) pos)
                            game/begin-next-station)])
                 available)))))

(defn choose-activate-owner-bonus-choices
  "After activator finishes base actions, owner decides to decline (0) or take full bonus.
   No partial bonus — all or nothing."
  [state]
  (let [bonus-total  (get-in state [:player-turn :action :bonus-total] 0)
        owner        (get-in state [:player-turn :action :current-owner])
        station-type (get-in state [:player-turn :action :station-type])
        max-feasible (case station-type
                       :matrix
                       (let [positions (count (game/matrix-beacon-positions state owner))
                             spendable (game/total-spendable-sundivers state owner)
                             beacons   (get-in state [:players owner :reserve :beacons] 0)]
                         (min bonus-total positions spendable beacons))
                       :tower
                       (min bonus-total (game/total-spendable-sundivers state owner))
                       bonus-total)
        options (if (pos? max-feasible) [0 max-feasible] [0])]
    (into {}
          (map
           (fn [n]
             [n (-> state
                    (assoc-in [:player-turn :action :owner-actions] n)
                    (assoc-in [:player-turn :choice-player] nil)
                    (game/begin-actor-actions :owner))])
           options))))

(defn choose-activate-self-bonus-choices
  "Activating their own station: choose to decline (0) or take full bonus.
   No partial bonus — it's all or nothing."
  [state]
  (let [bonus-total  (get-in state [:player-turn :action :bonus-total] 0)
        base         (get-in state [:player-turn :action :activator-actions] 0)
        player       (game/current-player state)
        station-type (get-in state [:player-turn :action :station-type])
        max-feasible (case station-type
                       :matrix
                       (let [positions (count (game/matrix-beacon-positions state player))
                             spendable (game/total-spendable-sundivers state player)
                             beacons   (get-in state [:players player :reserve :beacons] 0)]
                         (max 0 (min bonus-total (- positions base) (- spendable base) (- beacons base))))
                       :tower
                       (max 0 (min bonus-total (- (game/total-spendable-sundivers state player) base)))
                       bonus-total)
        ;; Only offer 0 (decline) and max-feasible (take full bonus)
        options (if (pos? max-feasible)
                  [0 max-feasible]
                  [0])]
    (into {}
          (map
           (fn [n]
             [n (-> state
                    (assoc-in [:player-turn :action :activator-actions] (+ base n))
                    (game/begin-actor-actions :activator))])
           options))))

;; --- activate helpers ---

(defn current-actor [state]
  (get-in state [:player-turn :action :current-actor]))

(defn actor-player [state]
  (game/actor-player state (current-actor state)))

(defn actor-actions-key [state]
  (let [actor (current-actor state)]
    (if (= actor :activator) :activator-actions :owner-actions)))

(defn tower-continue
  "After a tower action: continue to next heading choice, or check landing and advance."
  [state]
  (let [actor     (current-actor state)
        remaining (get-in state [:player-turn :action (actor-actions-key state)] 0)]
    (if (pos? remaining)
      (assoc-in state [:player-turn :phase] :choose-activate-tower-heading)
      (let [landings (game/available-landings state)]
        (if (seq landings)
          (-> state
              (assoc-in [:player-turn :action :post-land-actor] actor)
              (assoc-in [:player-turn :phase] :choose-land))
          (game/advance-after-actions state actor))))))

(defn tower-after-advance
  "After Ark advances: handle beacon discovery if present, then spend a sundiver."
  [state]
  (let [new-ark    (:ark state)
        act-player (actor-player state)
        tile       (game/get-tile state new-ark)]
    (if (and tile (:beacon tile))
      (let [beacon-owner (:beacon tile)
            s (game/discover-beacon state new-ark act-player)]
        (if (= beacon-owner act-player)
          ;; Own beacon: discover it, then pay tower action cost
          (assoc-in s [:player-turn :phase] :choose-activate-tower-spend)
          ;; Another player's beacon: offer join (spend happens after join resolves)
          (-> s
              (assoc-in [:player-turn :action :pending-join-actor] (current-actor s))
              (assoc-in [:player-turn :phase] :choose-activate-tower-join))))
      ;; No beacon: pay tower action cost
      (assoc-in state [:player-turn :phase] :choose-activate-tower-spend))))

;; --- matrix choice phases ---

(defn decrement-matrix-and-continue
  "After spending a sundiver for matrix: continue or advance to next actor/station."
  [state]
  (let [actor   (current-actor state)
        act-key (actor-actions-key state)
        s       (update-in state [:player-turn :action act-key] dec)]
    (if (pos? (get-in s [:player-turn :action act-key] 0))
      (assoc-in s [:player-turn :phase] :choose-activate-matrix-beacon)
      (game/advance-after-actions s actor))))

(defn choose-activate-matrix-beacon-choices
  [state]
  (let [player    (actor-player state)
        positions (game/matrix-beacon-positions state player)
        choices   (into {}
                        (map
                         (fn [pos]
                           [pos (-> state
                                    (game/place-beacon player pos)
                                    (assoc-in [:player-turn :phase] :choose-activate-matrix-spend))])
                         positions))]
    ;; If no valid positions, skip remaining actions and advance
    (if (seq choices)
      choices
      (let [actor (current-actor state)]
        {:skip (game/advance-after-actions state actor)}))))

(defn choose-activate-matrix-spend-choices
  [state]
  (let [player    (actor-player state)
        positions (game/sundiver-spend-positions state player)
        choices   (into {}
                        (map
                         (fn [pos]
                           [pos (-> state
                                    (game/spend-sundiver player pos)
                                    decrement-matrix-and-continue)])
                         positions))]
    (if (seq choices)
      choices
      (let [actor (current-actor state)]
        {:skip (game/advance-after-actions state actor)}))))

;; --- tower choice phases ---

(defn choose-activate-tower-heading-choices
  "Choose where the Ark advances. For each heading (straight/left/right), shows the
   destination tile. If unexplored, also shows the wrap destination. All as hex positions."
  [state]
  (let [ark (:ark state)]
    (into {}
          (mapcat
           (fn [turn-dir]
             (let [s        (game/turn-heading state turn-dir)
                   dest-pos (:heading-token s)
                   explored? (game/get-tile s dest-pos)]
               (if explored?
                 ;; Explored: just advance there
                 [[dest-pos (-> s game/advance-ark tower-after-advance)]]
                 ;; Unexplored: offer both direct (explore) and wrap
                 (let [dir      (game/heading-direction s)
                       wrap-pos (game/wrap-target s ark dir)
                       has-beyond? (some #(and (game/on-ray? ark dir %)
                                               (> (game/hex-distance ark %) 1))
                                         (keys (:board state)))]
                   (cond-> [[dest-pos (-> s game/advance-ark tower-after-advance)]]
                     (not has-beyond?)
                     (conj [wrap-pos (-> s (game/advance-ark-to wrap-pos) tower-after-advance)]))))))
           [:none :left :right]))))

(defn finish-tower-join
  "Execute join after spending is resolved, then pay the tower action cost."
  [state]
  (let [actor      (get-in state [:player-turn :action :pending-join-actor])
        act-player (game/actor-player state actor)]
    (-> state
        (game/join-beacon-to-cipher act-player)
        (update-in [:player-turn :action :beacons-joined] (fnil inc 0))
        (update-in [:player-turn :action] dissoc :pending-join-actor :join-spend-remaining)
        (assoc-in [:player-turn :phase] :choose-activate-tower-spend))))

(defn choose-activate-tower-join-choices
  "Activator may add their own beacon to the cipher for the discovered world.
   Cost is :beacons-joined sundivers (0 for first join, 1 for second, etc.)."
  [state]
  (let [actor          (get-in state [:player-turn :action :pending-join-actor])
        act-player     (game/actor-player state actor)
        join-cost      (get-in state [:player-turn :action :beacons-joined] 0)
        has-beacons    (pos? (get-in state [:players act-player :reserve :beacons] 0))
        can-afford     (>= (game/total-spendable-sundivers state act-player) join-cost)
        can-join        (and has-beacons can-afford)
        after-skip     (-> state
                           (update-in [:player-turn :action] dissoc :pending-join-actor)
                           (assoc-in [:player-turn :phase] :choose-activate-tower-spend))]
    (cond-> {:skip after-skip}
      can-join
      (assoc :join
             (if (zero? join-cost)
               (finish-tower-join state)
               (-> state
                   (assoc-in [:player-turn :action :join-spend-remaining] join-cost)
                   (assoc-in [:player-turn :phase] :choose-activate-tower-join-spend)))))))

(defn choose-activate-tower-join-spend-choices
  "Spend sundivers to pay the beacon-join cost."
  [state]
  (let [actor      (get-in state [:player-turn :action :pending-join-actor])
        act-player (game/actor-player state actor)
        remaining  (get-in state [:player-turn :action :join-spend-remaining] 0)
        positions  (game/sundiver-spend-positions state act-player)
        choices    (into {}
                         (map
                          (fn [pos]
                            [pos (let [s            (game/spend-sundiver state act-player pos)
                                       new-remaining (dec remaining)
                                       s            (assoc-in s [:player-turn :action :join-spend-remaining] new-remaining)]
                                   (if (zero? new-remaining)
                                     (finish-tower-join s)
                                     (assoc-in s [:player-turn :phase] :choose-activate-tower-join-spend)))])
                          positions))]
    (if (seq choices)
      choices
      {:skip (-> state
                 (update-in [:player-turn :action] dissoc :pending-join-actor :join-spend-remaining)
                 (assoc-in [:player-turn :phase] :choose-activate-tower-spend))})))

(defn choose-activate-tower-spend-choices
  "Spend 1 sundiver to pay for this tower action, then decrement and continue."
  [state]
  (let [player    (actor-player state)
        positions (game/sundiver-spend-positions state player)
        choices   (into {}
                        (map
                         (fn [pos]
                           [pos (-> state
                                    (game/spend-sundiver player pos)
                                    (update-in [:player-turn :action (actor-actions-key state)] dec)
                                    tower-continue)])
                         positions))]
    ;; If no spendable sundivers, skip remaining actions and advance
    (if (seq choices)
      choices
      (let [actor (current-actor state)]
        {:skip (game/advance-after-actions state actor)}))))

;; --- post-action: draw cards ---

(defn choose-draw-cards-choices
  "Single auto-choice that executes the draw and flare processing."
  [state]
  {:draw (game/process-draw-and-flares state)})

;; --- post-action: flare beacon joins (captain choice) ---

(defn advance-flare-join-or-keep
  "After a flare join decision: continue to next join or keep-card phase."
  [state]
  (let [remaining (get-in state [:player-turn :action :flare-join-indices] [])]
    (if (seq remaining)
      (assoc-in state [:player-turn :phase] :flare-beacon-join)
      (-> state
          (assoc-in [:player-turn :choice-player] nil)
          (assoc-in [:player-turn :phase] :keep-card)))))

(defn finish-flare-join
  "Execute the beacon join after spending is complete; continue."
  [state cipher-idx]
  (let [captain (:captain-flame state)]
    (-> state
        (game/join-beacon-to-cipher captain cipher-idx)
        (update-in [:player-turn :captain-beacons-joined] (fnil inc 0))
        (update-in [:player-turn :action :flare-join-indices] (comp vec rest))
        (update-in [:player-turn :action] dissoc :flare-join-spend-remaining :flare-join-cipher-idx)
        advance-flare-join-or-keep)))

(defn choose-flare-beacon-join-choices
  [state]
  (let [captain      (:captain-flame state)
        join-indices (get-in state [:player-turn :action :flare-join-indices] [])
        cipher-idx   (first join-indices)
        join-cost    (get-in state [:player-turn :captain-beacons-joined] 0)
        has-beacons  (pos? (get-in state [:players captain :reserve :beacons] 0))
        can-afford   (>= (game/total-spendable-sundivers state captain) join-cost)
        after-skip   (-> state
                         (update-in [:player-turn :action :flare-join-indices] (comp vec rest))
                         advance-flare-join-or-keep)]
    (cond-> {:skip after-skip}
      (and has-beacons can-afford)
      (assoc :join
             (if (zero? join-cost)
               (finish-flare-join state cipher-idx)
               (-> state
                   (assoc-in [:player-turn :action :flare-join-cipher-idx] cipher-idx)
                   (assoc-in [:player-turn :action :flare-join-spend-remaining] join-cost)
                   (assoc-in [:player-turn :phase] :flare-beacon-join-spend)))))))

(defn choose-flare-beacon-join-spend-choices
  [state]
  (let [captain    (:captain-flame state)
        cipher-idx (get-in state [:player-turn :action :flare-join-cipher-idx])
        remaining  (get-in state [:player-turn :action :flare-join-spend-remaining] 0)]
    (into {}
          (map
           (fn [pos]
             [pos (let [s       (game/spend-sundiver state captain pos)
                        new-rem (dec remaining)]
                    (if (zero? new-rem)
                      (finish-flare-join s cipher-idx)
                      (-> s
                          (assoc-in [:player-turn :action :flare-join-spend-remaining] new-rem)
                          (assoc-in [:player-turn :phase] :flare-beacon-join-spend))))])
           (game/sundiver-spend-positions state captain)))))

;; --- post-action: keep card ---

(defn advance-to-captain-or-cipher
  "After keeping a card: go to captain drift phase if current player is captain,
   otherwise enter cipher phase."
  [state]
  (let [player  (game/current-player state)
        captain (:captain-flame state)]
    (if (= player captain)
      (-> state
          (assoc-in [:player-turn :choice-player] nil)
          (assoc-in [:player-turn :phase] :choose-captain-drift))
      (enter-cipher-phase state))))

(defn choose-keep-card-choices
  "Player picks one drawn card to hold; previously held card and others are discarded.
   If no cards were drawn, auto-advances (single :continue choice)."
  [state]
  (let [player  (game/current-player state)
        drawn   (get-in state [:player-turn :action :drawn-cards] [])
        held    (get-in state [:players player :held-card])]
    (if (empty? drawn)
      {:continue (advance-to-captain-or-cipher state)}
      (cond-> {}
        ;; Keep each drawn card as an option
        true
        (into (map (fn [card]
                     [card (-> state
                               (assoc-in [:players player :held-card] card)
                               (update :discard into (remove #{card} drawn))
                               (cond-> held (update :discard conj held))
                               advance-to-captain-or-cipher)])
                   drawn))
        ;; Keep previously held card (discard all drawn)
        held
        (assoc :keep-held (-> state
                              (update :discard into drawn)
                              advance-to-captain-or-cipher))))))

;; --- cipher helpers ---

(defn pending-cipher-queue
  "Flat ordered list of beacons to place in cipher, from pending-cipher entries.
   Each entry: {:player player :color color :from-bag? bool}"
  [pending-cipher]
  (vec
   (mapcat
    (fn [{:keys [world beacon-owner joiners]}]
      (into [{:player beacon-owner :color world :from-bag? false}]
            (map (fn [p] {:player p :color world :from-bag? true}) joiners)))
    pending-cipher)))

(defn enter-cipher-phase
  "Initialize cipher queue from pending-cipher; skip to next turn if nothing to resolve."
  [state]
  (let [queue (pending-cipher-queue (:pending-cipher state))]
    (if (empty? queue)
      (game/begin-next-player-turn state)
      (-> state
          (assoc-in [:player-turn :cipher-queue] queue)
          (assoc-in [:player-turn :choice-player] (:player (first queue)))
          (assoc-in [:player-turn :phase] :cipher)))))

(defn advance-cipher
  "After a cipher beacon is placed: move to next in queue, or end the turn."
  [state]
  (let [queue (vec (rest (get-in state [:player-turn :cipher-queue] [])))]
    (if (empty? queue)
      (game/begin-next-player-turn state)
      (-> state
          (assoc-in [:player-turn :cipher-queue] queue)
          (assoc-in [:player-turn :choice-player] (:player (first queue)))
          (assoc-in [:player-turn :phase] :cipher)))))

(defn complete-cipher-placement
  "Apply the pending cipher beacon placement after spending is resolved."
  [state]
  (let [player    (get-in state [:player-turn :cipher-pending-player])
        pos       (get-in state [:player-turn :cipher-pending-pos])
        color     (get-in state [:player-turn :cipher-pending-color])
        from-bag? (get-in state [:player-turn :cipher-pending-from-bag])]
    (-> state
        (game/cipher-place-beacon player pos color from-bag?)
        (update-in [:player-turn] dissoc
                   :cipher-pending-player :cipher-pending-pos
                   :cipher-pending-color :cipher-pending-from-bag
                   :cipher-spend-remaining)
        advance-cipher)))

;; --- captain drift ---

(defn captain-drift-continuation
  "End of a captain beacon-join sequence.
   If :beacon-join-continuation is :draw-drift-card (regular drift), draws the drift card.
   Otherwise (drift-card flare beacon join) enters cipher directly."
  [state]
  (if (= :draw-drift-card (get-in state [:player-turn :beacon-join-continuation]))
    (-> state
        (update :player-turn dissoc :beacon-join-continuation)
        (assoc-in [:player-turn :phase] :draw-drift-card))
    (enter-cipher-phase state)))

(defn handle-captain-drift-beacon
  "After Ark advance during drift: discover beacon if present; offer join if another's.
   All paths continue to :draw-drift-card after any beacon resolution."
  [state]
  (let [captain (:captain-flame state)
        new-ark (:ark state)
        tile    (game/get-tile state new-ark)]
    (if (and tile (:beacon tile))
      (let [beacon-owner (:beacon tile)
            s (game/discover-beacon state new-ark captain)]
        (if (= beacon-owner captain)
          (assoc-in s [:player-turn :phase] :draw-drift-card)
          (-> s
              (assoc-in [:player-turn :beacon-join-continuation] :draw-drift-card)
              (assoc-in [:player-turn :phase] :captain-beacon-join))))
      (assoc-in state [:player-turn :phase] :draw-drift-card))))

(defn choose-captain-drift-choices
  "Captain may turn heading. Shows destination tiles (including wrap if at edge)."
  [state]
  (let [ark (:ark state)]
    (into {}
          (mapcat
           (fn [turn-dir]
             (let [s        (game/turn-heading state turn-dir)
                   dest-pos (:heading-token s)
                   explored? (game/get-tile s dest-pos)]
               (if explored?
                 [[dest-pos (-> s game/advance-ark handle-captain-drift-beacon)]]
                 (let [dir      (game/heading-direction s)
                       wrap-pos (game/wrap-target s ark dir)
                       has-beyond? (some #(and (game/on-ray? ark dir %)
                                               (> (game/hex-distance ark %) 1))
                                         (keys (:board state)))]
                   (cond-> [[dest-pos (-> s game/advance-ark handle-captain-drift-beacon)]]
                     (not has-beyond?)
                     (conj [wrap-pos (-> s (game/advance-ark-to wrap-pos) handle-captain-drift-beacon)]))))))
           [:none :left :right]))))

(defn finish-captain-join
  [state]
  (let [captain (:captain-flame state)]
    (-> state
        (game/join-beacon-to-cipher captain)
        (update-in [:player-turn :captain-beacons-joined] (fnil inc 0))
        (update-in [:player-turn :action] dissoc :captain-join-spend-remaining)
        captain-drift-continuation)))

(defn choose-captain-beacon-join-choices
  "Captain may add their own beacon to the cipher for the discovered world."
  [state]
  (let [captain     (:captain-flame state)
        join-cost   (get-in state [:player-turn :captain-beacons-joined] 0)
        has-beacons (pos? (get-in state [:players captain :reserve :beacons] 0))
        can-afford  (>= (game/total-spendable-sundivers state captain) join-cost)]
    (cond-> {:skip (captain-drift-continuation state)}
      (and has-beacons can-afford)
      (assoc :join
             (if (zero? join-cost)
               (finish-captain-join state)
               (-> state
                   (assoc-in [:player-turn :action :captain-join-spend-remaining] join-cost)
                   (assoc-in [:player-turn :phase] :captain-beacon-join-spend)))))))

;; --- drift card ---

(defn process-drift-flare-advance
  "Advance Ark to new-ark for a drift-card flare, handle beacon, then enter cipher.
   Uses captain-drift-continuation for beacon joins (no :beacon-join-continuation set,
   so it routes to enter-cipher-phase rather than another drift card draw)."
  [state new-ark]
  (let [captain (:captain-flame state)
        tile    (game/get-tile state new-ark)
        s       (game/advance-ark-to state new-ark)]
    (if (and tile (:beacon tile))
      (let [beacon-owner (:beacon tile)
            s (game/discover-beacon s new-ark captain)]
        (if (= beacon-owner captain)
          (enter-cipher-phase s)
          (assoc-in s [:player-turn :phase] :captain-beacon-join)))
      (enter-cipher-phase s))))

(defn choose-drift-flare-advance-choices
  "When drift-card flare advances Ark into unexplored space: :direct or :wrap.
   Wrap only if no tiles exist further along the ray."
  [state]
  (let [from-pos (:ark state)
        dir      (game/heading-direction state)
        wrap-pos (game/wrap-target state from-pos dir)
        next-pos (:heading-token state)
        has-tiles-beyond? (some #(and (game/on-ray? from-pos dir %)
                                      (> (game/hex-distance from-pos %) 1))
                                (keys (:board state)))]
    (cond-> {:direct (process-drift-flare-advance state next-pos)}
      (not has-tiles-beyond?)
      (assoc :wrap (process-drift-flare-advance state wrap-pos)))))

(defn choose-draw-drift-card-choices
  "Auto-choice to draw one drift card after captain drift.
   Flares advance the Ark (with wrap if unexplored); non-flares are discarded."
  [state]
  (let [[s cards] (game/draw-n-cards state 1)
        card      (first cards)]
    {:draw (if (game/flare? card)
             (let [s (update s :flares-drawn (fnil + 0) 1)]
               (if (>= (:flares-drawn s) 13)
                 (game/trigger-loss s)
                 (let [next-pos (:heading-token s)]
                   (if (game/get-tile s next-pos)
                     (process-drift-flare-advance s next-pos)
                     (-> s
                         (assoc-in [:player-turn :choice-player] (:captain-flame s))
                         (assoc-in [:player-turn :phase] :choose-drift-flare-advance))))))
             (enter-cipher-phase s))}))

(defn choose-captain-beacon-join-spend-choices
  [state]
  (let [captain   (:captain-flame state)
        remaining (get-in state [:player-turn :action :captain-join-spend-remaining] 0)]
    (into {}
          (map
           (fn [pos]
             [pos (let [s       (game/spend-sundiver state captain pos)
                        new-rem (dec remaining)]
                    (if (zero? new-rem)
                      (finish-captain-join s)
                      (-> s
                          (assoc-in [:player-turn :action :captain-join-spend-remaining] new-rem)
                          (assoc-in [:player-turn :phase] :captain-beacon-join-spend))))])
           (game/sundiver-spend-positions state captain)))))

;; --- cipher: resolve pending-cipher beacons one at a time ---

(defn choose-cipher-choices
  "Each pending-cipher beacon is placed at one of the 7 cipher positions.
   Only positions the player can afford are offered (cost <= spendable sundivers).
   If no position is affordable, the beacon is skipped and the queue advances."
  [state]
  (let [queue (get-in state [:player-turn :cipher-queue] [])]
    (if (empty? queue)
      {:done (game/begin-next-player-turn state)}
      (let [{:keys [player color from-bag?]} (first queue)
            spendable (game/total-spendable-sundivers state player)
            choices
            (into {}
                  (keep
                   (fn [pos]
                     (let [cost (game/cipher-placement-cost state pos color)]
                       (when (<= cost spendable)
                         [pos (if (zero? cost)
                                (-> state
                                    (game/cipher-place-beacon player pos color from-bag?)
                                    advance-cipher)
                                (-> state
                                    (assoc-in [:player-turn :cipher-pending-player] player)
                                    (assoc-in [:player-turn :cipher-pending-pos] pos)
                                    (assoc-in [:player-turn :cipher-pending-color] color)
                                    (assoc-in [:player-turn :cipher-pending-from-bag] from-bag?)
                                    (assoc-in [:player-turn :cipher-spend-remaining] cost)
                                    (assoc-in [:player-turn :phase] :cipher-spend)))])))
                   (keys (:cipher state))))]
        (if (seq choices)
          choices
          {:skip (advance-cipher state)})))))

(defn choose-cipher-spend-choices
  "Spend sundivers one at a time to pay for placing a new color at a cipher position."
  [state]
  (let [player    (get-in state [:player-turn :cipher-pending-player])
        remaining (get-in state [:player-turn :cipher-spend-remaining] 0)]
    (into {}
          (map
           (fn [pos]
             [pos (let [s       (game/spend-sundiver state player pos)
                        new-rem (dec remaining)]
                    (if (zero? new-rem)
                      (complete-cipher-placement s)
                      (-> s
                          (assoc-in [:player-turn :cipher-spend-remaining] new-rem)
                          (assoc-in [:player-turn :phase] :cipher-spend))))])
           (game/sundiver-spend-positions state player)))))

;; --- landing ---

(defn choose-land-choices
  "After completing a full set of tower actions: offer landing on any eligible tile,
   or continue to the next actor/station."
  [state]
  (let [actor    (get-in state [:player-turn :action :post-land-actor])
        landings (game/available-landings state)
        continue (-> state
                     (update-in [:player-turn :action] dissoc :post-land-actor)
                     (game/advance-after-actions actor))]
    (cond-> {:continue continue}
      (seq landings)
      (into (map (fn [pos] [[:land pos] (game/land-ark state pos)]) landings)))))

;; --- ark advance wrap choice ---

(defn choose-ark-advance-choices
  "When the Ark would advance into unexplored space: offer :direct (explore) or :wrap.
   Wrap is only offered if there are no tiles further along the ray (extreme edge)."
  [state]
  (let [from-pos (:ark state)
        dir      (game/heading-direction state)
        wrap-pos (game/wrap-target state from-pos dir)
        context  (get-in state [:player-turn :ark-advance-context])
        after-fn (case context
                   :tower tower-after-advance
                   :drift handle-captain-drift-beacon)
        ;; Only offer wrap if no tiles exist further along this ray
        has-tiles-beyond? (some #(and (game/on-ray? from-pos dir %)
                                      (> (game/hex-distance from-pos %) 1))
                                (keys (:board state)))]
    (cond-> {:direct (-> state game/advance-ark after-fn)}
      (not has-tiles-beyond?)
      (assoc :wrap (-> state (game/advance-ark-to wrap-pos) after-fn)))))

(defn choose-flare-advance-choices
  "When a flare would advance the Ark into unexplored space: offer :direct or :wrap.
   Wrap only if no tiles exist further along the ray."
  [state]
  (let [from-pos (:ark state)
        dir      (game/heading-direction state)
        wrap-pos (game/wrap-target state from-pos dir)
        next-pos (:heading-token state)
        has-tiles-beyond? (some #(and (game/on-ray? from-pos dir %)
                                      (> (game/hex-distance from-pos %) 1))
                                (keys (:board state)))]
    (cond-> {:direct (game/advance-flare-ark-to state next-pos)}
      (not has-tiles-beyond?)
      (assoc :wrap (game/advance-flare-ark-to state wrap-pos)))))

;; --- action type ---

(defn choose-action-type-choices
  [state]
  (into {}
        (keep (fn [action-type]
                (let [next-state (game/choose-action-type state action-type)]
                  (try
                    (let [choices (case action-type
                                   :move     (choose-move-choices next-state)
                                   :convert  (choose-convert-choices next-state)
                                   :activate (choose-activate-choices next-state))]
                      (when (seq choices)
                        [action-type next-state]))
                    (catch #?(:clj Exception :cljs :default) e
                      (println "choose-action-type error for" action-type
                               #?(:clj (.getMessage e) :cljs e))
                      ;; :move should always be available — return it with a safe fallback
                      (when (= :move action-type)
                        [action-type next-state])))))
              game/action-types)))

;; --- central dispatch ---

(defn find-state
  "Return [phase choices-map] describing what the current player must decide next.
   Auto-advances through phases that have exactly one choice (no real decision)."
  [state]
  (loop [state state]
    (let [phase   (game/current-phase state)
          choices (case phase
                  :choose-action-type        (choose-action-type-choices state)
                  :choose-move               (choose-move-choices state)
                  :choose-launch-destination (choose-launch-destination-choices state)
                  :choose-fly-from           (choose-fly-from-choices state)
                  :choose-fly-to             (choose-fly-to-choices state)
                  :choose-convert            (choose-convert-choices state)
                  :choose-activate                  (choose-activate-choices state)
                  :choose-activate-station           (choose-activate-station-choices state)
                  :choose-activate-self-bonus       (choose-activate-self-bonus-choices state)
                  :choose-activate-owner-bonus      (choose-activate-owner-bonus-choices state)
                  :choose-activate-matrix-beacon    (choose-activate-matrix-beacon-choices state)
                  :choose-activate-matrix-spend     (choose-activate-matrix-spend-choices state)
                  :choose-activate-tower-heading    (choose-activate-tower-heading-choices state)
                  :choose-activate-tower-join       (choose-activate-tower-join-choices state)
                  :choose-activate-tower-join-spend (choose-activate-tower-join-spend-choices state)
                  :choose-activate-tower-spend      (choose-activate-tower-spend-choices state)
                  :draw-cards                       (choose-draw-cards-choices state)
                  :flare-beacon-join                (choose-flare-beacon-join-choices state)
                  :flare-beacon-join-spend          (choose-flare-beacon-join-spend-choices state)
                  :keep-card                        (choose-keep-card-choices state)
                  :choose-captain-drift             (choose-captain-drift-choices state)
                  :choose-ark-advance               (choose-ark-advance-choices state)
                  :choose-flare-advance             (choose-flare-advance-choices state)
                  :draw-drift-card                  (choose-draw-drift-card-choices state)
                  :choose-drift-flare-advance       (choose-drift-flare-advance-choices state)
                  :captain-beacon-join              (choose-captain-beacon-join-choices state)
                  :captain-beacon-join-spend        (choose-captain-beacon-join-spend-choices state)
                  :cipher                           (choose-cipher-choices state)
                  :cipher-spend                     (choose-cipher-spend-choices state)
                  :choose-land                      (choose-land-choices state)
                  :game-over                        {}
                  {})]
      ;; Auto-advance: if exactly 1 choice, skip this phase entirely.
      ;; Never auto-advance these phases — the player must always choose explicitly.
      ;; Also never auto-advance choose-convert (player should see what they're building).
      (if (and (= 1 (count choices))
               (not (#{:choose-action-type :choose-move :choose-convert
                       :choose-activate :choose-activate-self-bonus
                       :choose-activate-owner-bonus :choose-land} phase)))
        (let [next-state (first (vals choices))]
          (if (and next-state (not= phase :game-over))
            (recur next-state)
            [phase choices]))
        [phase choices]))))

(defn find-state-raw
  "Like find-state but without auto-advance. Returns [phase choices-map] for the
   exact current phase, even if there's only one choice."
  [state]
  (let [phase (game/current-phase state)]
    [phase
     (case phase
       :choose-action-type        (choose-action-type-choices state)
       :choose-move               (choose-move-choices state)
       :choose-launch-destination (choose-launch-destination-choices state)
       :choose-fly-from           (choose-fly-from-choices state)
       :choose-fly-to             (choose-fly-to-choices state)
       :choose-convert            (choose-convert-choices state)
       :choose-activate                  (choose-activate-choices state)
       :choose-activate-station           (choose-activate-station-choices state)
       :choose-activate-self-bonus       (choose-activate-self-bonus-choices state)
       :choose-activate-owner-bonus      (choose-activate-owner-bonus-choices state)
       :choose-activate-matrix-beacon    (choose-activate-matrix-beacon-choices state)
       :choose-activate-matrix-spend     (choose-activate-matrix-spend-choices state)
       :choose-activate-tower-heading    (choose-activate-tower-heading-choices state)
       :choose-activate-tower-join       (choose-activate-tower-join-choices state)
       :choose-activate-tower-join-spend (choose-activate-tower-join-spend-choices state)
       :choose-activate-tower-spend      (choose-activate-tower-spend-choices state)
       :draw-cards                       (choose-draw-cards-choices state)
       :flare-beacon-join                (choose-flare-beacon-join-choices state)
       :flare-beacon-join-spend          (choose-flare-beacon-join-spend-choices state)
       :keep-card                        (choose-keep-card-choices state)
       :choose-captain-drift             (choose-captain-drift-choices state)
       :choose-ark-advance               (choose-ark-advance-choices state)
       :choose-flare-advance             (choose-flare-advance-choices state)
       :draw-drift-card                  (choose-draw-drift-card-choices state)
       :choose-drift-flare-advance       (choose-drift-flare-advance-choices state)
       :captain-beacon-join              (choose-captain-beacon-join-choices state)
       :captain-beacon-join-spend        (choose-captain-beacon-join-spend-choices state)
       :cipher                           (choose-cipher-choices state)
       :cipher-spend                     (choose-cipher-spend-choices state)
       :choose-land                      (choose-land-choices state)
       :game-over                        {}
       {})]))
