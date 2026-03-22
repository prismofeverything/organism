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
          (not (game/immobile? state pos))))
   (keys (:board state))))

(defn fly-destinations
  "All positions reachable by flying from from-pos: adjacent hexes plus gate connections.
   Gate connections take precedence when a position appears in both."
  [state player from-pos]
  (let [gate-connected (get-in state [:players player :gates from-pos] #{})
        adjacent       (set (game/hex-neighbors from-pos))]
    (into gate-connected adjacent)))

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
                 (if (game/get-tile state pos)
                   [base-entry]
                   ;; Unexplored directional position: also offer wrap
                   (let [wrap-pos (game/wrap-target state ark (nth flank-dirs flank-idx))]
                     [base-entry [[:wrap pos] (-> state (game/launch-sundiver player wrap-pos) use-move-point)]]))
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
  (let [player   (game/current-player state)
        from-pos (get-in state [:player-turn :action :fly-from])]
    (into {}
          (mapcat
           (fn [to-pos]
             (let [owner (game/gate-owner state from-pos to-pos)
                   after (fn [s] (-> s
                                     (update-in [:player-turn :action] dissoc :fly-from)
                                     use-move-point))
                   base-entry [to-pos (after (if owner
                                               (game/fly-through-gate state player from-pos to-pos)
                                               (game/fly-sundiver state player from-pos to-pos)))]]
               (if (and (nil? owner) (nil? (game/get-tile state to-pos)))
                 ;; Unexplored non-gate: also offer wrap
                 (let [dir-idx  (game/adjacent-direction from-pos to-pos)
                       fly-dir  (nth game/hex-directions dir-idx)
                       wrap-pos (game/wrap-target state from-pos fly-dir)]
                   [base-entry [[:wrap to-pos] (after (game/fly-sundiver state player from-pos wrap-pos))]])
                 [base-entry])))
           (fly-destinations state player from-pos)))))

(defn choose-move-choices
  "Options during the move action. Launch and fly are only offered while move points remain."
  [state]
  (let [player    (game/current-player state)
        remaining (get-in state [:player-turn :action :moves-remaining] 0)
        habitat   (get-in state [:players player :habitat :sundivers] 0)
        mobile    (mobile-positions state player)]
    (cond-> {:done (assoc-in state [:player-turn :phase] :draw-cards)}
      (and (pos? remaining) (pos? habitat))
      (assoc :launch (assoc-in state [:player-turn :phase] :choose-launch-destination))

      (and (pos? remaining) (seq mobile))
      (assoc :fly (assoc-in state [:player-turn :phase] :choose-fly-from)))))

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

(defn choose-activate-owner-bonus-choices
  "After activator finishes base actions, owner decides how many bonus actions to take (0..bonus-total)."
  [state]
  (let [bonus-total (get-in state [:player-turn :action :bonus-total] 0)]
    (into {}
          (map
           (fn [n]
             [n (-> state
                    (assoc-in [:player-turn :action :owner-actions] n)
                    (assoc-in [:player-turn :choice-player] nil)
                    (game/begin-actor-actions :owner))])
           (range (inc bonus-total))))))

(defn choose-activate-self-bonus-choices
  "Activating their own station: choose 0..bonus bonus actions on top of base."
  [state]
  (let [bonus-total (get-in state [:player-turn :action :bonus-total] 0)
        base        (get-in state [:player-turn :action :activator-actions] 0)]
    (into {}
          (map
           (fn [n]
             [n (-> state
                    (assoc-in [:player-turn :action :activator-actions] (+ base n))
                    (game/begin-actor-actions :activator))])
           (range (inc bonus-total))))))

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
  (let [player (actor-player state)]
    (into {}
          (map
           (fn [pos]
             [pos (-> state
                      (game/place-beacon player pos)
                      (assoc-in [:player-turn :phase] :choose-activate-matrix-spend))])
           (game/matrix-beacon-positions state player)))))

(defn choose-activate-matrix-spend-choices
  [state]
  (let [player (actor-player state)]
    (into {}
          (map
           (fn [pos]
             [pos (-> state
                      (game/spend-sundiver player pos)
                      decrement-matrix-and-continue)])
           (game/sundiver-spend-positions state player)))))

;; --- tower choice phases ---

(defn choose-activate-tower-heading-choices
  "Choose heading turn (:none/:left/:right), then advance Ark (with wrap if unexplored)."
  [state]
  (into {}
        (map
         (fn [turn-dir]
           [turn-dir (let [s (game/turn-heading state turn-dir)]
                       (if (game/get-tile s (:heading-token s))
                         (-> s game/advance-ark tower-after-advance)
                         (-> s
                             (assoc-in [:player-turn :ark-advance-context] :tower)
                             (assoc-in [:player-turn :phase] :choose-ark-advance))))])
         [:none :left :right])))

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
        remaining  (get-in state [:player-turn :action :join-spend-remaining] 0)]
    (into {}
          (map
           (fn [pos]
             [pos (let [s            (game/spend-sundiver state act-player pos)
                        new-remaining (dec remaining)
                        s            (assoc-in s [:player-turn :action :join-spend-remaining] new-remaining)]
                    (if (zero? new-remaining)
                      (finish-tower-join s)
                      (assoc-in s [:player-turn :phase] :choose-activate-tower-join-spend)))])
           (game/sundiver-spend-positions state act-player)))))

(defn choose-activate-tower-spend-choices
  "Spend 1 sundiver to pay for this tower action, then decrement and continue."
  [state]
  (let [player (actor-player state)]
    (into {}
          (map
           (fn [pos]
             [pos (-> state
                      (game/spend-sundiver player pos)
                      (update-in [:player-turn :action (actor-actions-key state)] dec)
                      tower-continue)])
           (game/sundiver-spend-positions state player)))))

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
      (assoc-in state [:player-turn :phase] :choose-captain-drift)
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

(defn handle-captain-drift-beacon
  "After Ark advance during drift: discover beacon if present; offer join if another's."
  [state]
  (let [captain (:captain-flame state)
        new-ark (:ark state)
        tile    (game/get-tile state new-ark)]
    (if (and tile (:beacon tile))
      (let [beacon-owner (:beacon tile)
            s (game/discover-beacon state new-ark captain)]
        (if (= beacon-owner captain)
          (enter-cipher-phase s)
          (assoc-in s [:player-turn :phase] :captain-beacon-join)))
      (enter-cipher-phase state))))

(defn choose-captain-drift-choices
  "Captain may turn heading once (:none/:left/:right), then Ark advances (with wrap if unexplored)."
  [state]
  (into {}
        (map
         (fn [turn-dir]
           [turn-dir (let [s (game/turn-heading state turn-dir)]
                       (if (game/get-tile s (:heading-token s))
                         (-> s game/advance-ark handle-captain-drift-beacon)
                         (-> s
                             (assoc-in [:player-turn :ark-advance-context] :drift)
                             (assoc-in [:player-turn :phase] :choose-ark-advance))))])
         [:none :left :right])))

(defn finish-captain-join
  [state]
  (let [captain (:captain-flame state)]
    (-> state
        (game/join-beacon-to-cipher captain)
        (update-in [:player-turn :captain-beacons-joined] (fnil inc 0))
        (update-in [:player-turn :action] dissoc :captain-join-spend-remaining)
        enter-cipher-phase)))

(defn choose-captain-beacon-join-choices
  "Captain may add their own beacon to the cipher for the discovered world."
  [state]
  (let [captain     (:captain-flame state)
        join-cost   (get-in state [:player-turn :captain-beacons-joined] 0)
        has-beacons (pos? (get-in state [:players captain :reserve :beacons] 0))
        can-afford  (>= (game/total-spendable-sundivers state captain) join-cost)]
    (cond-> {:skip (enter-cipher-phase state)}
      (and has-beacons can-afford)
      (assoc :join
             (if (zero? join-cost)
               (finish-captain-join state)
               (-> state
                   (assoc-in [:player-turn :action :captain-join-spend-remaining] join-cost)
                   (assoc-in [:player-turn :phase] :captain-beacon-join-spend)))))))

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
   If placing adds a new color to a position, the cost is paid first."
  [state]
  (let [queue (get-in state [:player-turn :cipher-queue] [])]
    (if (empty? queue)
      {:done (game/begin-next-player-turn state)}
      (let [{:keys [player color from-bag?]} (first queue)]
        (into {}
              (map
               (fn [pos]
                 [pos (let [cost (game/cipher-placement-cost state pos color)]
                        (if (zero? cost)
                          (-> state
                              (game/cipher-place-beacon player pos color from-bag?)
                              advance-cipher)
                          (-> state
                              (assoc-in [:player-turn :cipher-pending-player] player)
                              (assoc-in [:player-turn :cipher-pending-pos] pos)
                              (assoc-in [:player-turn :cipher-pending-color] color)
                              (assoc-in [:player-turn :cipher-pending-from-bag] from-bag?)
                              (assoc-in [:player-turn :cipher-spend-remaining] cost)
                              (assoc-in [:player-turn :phase] :cipher-spend))))])
               (keys (:cipher state))))))))

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
  "When the Ark would advance into unexplored space: offer :direct (explore) or :wrap."
  [state]
  (let [from-pos (:ark state)
        dir      (game/heading-direction state)
        wrap-pos (game/wrap-target state from-pos dir)
        context  (get-in state [:player-turn :ark-advance-context])
        after-fn (case context
                   :tower tower-after-advance
                   :drift handle-captain-drift-beacon)]
    {:direct (-> state game/advance-ark after-fn)
     :wrap   (-> state (game/advance-ark-to wrap-pos) after-fn)}))

(defn choose-flare-advance-choices
  "When a flare would advance the Ark into unexplored space: offer :direct or :wrap."
  [state]
  (let [from-pos (:ark state)
        dir      (game/heading-direction state)
        wrap-pos (game/wrap-target state from-pos dir)
        next-pos (:heading-token state)]
    {:direct (game/advance-flare-ark-to state next-pos)
     :wrap   (game/advance-flare-ark-to state wrap-pos)}))

;; --- action type ---

(defn choose-action-type-choices
  [state]
  (partial-map
   (partial game/choose-action-type state)
   game/action-types))

;; --- central dispatch ---

(defn find-state
  "Return [phase choices-map] describing what the current player must decide next."
  [state]
  (let [phase (game/current-phase state)]
    (case phase
      :choose-action-type        [:choose-action-type        (choose-action-type-choices state)]
      :choose-move               [:choose-move               (choose-move-choices state)]
      :choose-launch-destination [:choose-launch-destination (choose-launch-destination-choices state)]
      :choose-fly-from           [:choose-fly-from           (choose-fly-from-choices state)]
      :choose-fly-to             [:choose-fly-to             (choose-fly-to-choices state)]
      :choose-convert            [:choose-convert            (choose-convert-choices state)]
      :choose-activate                  [:choose-activate                  (choose-activate-choices state)]
      :choose-activate-self-bonus       [:choose-activate-self-bonus       (choose-activate-self-bonus-choices state)]
      :choose-activate-owner-bonus      [:choose-activate-owner-bonus      (choose-activate-owner-bonus-choices state)]
      :choose-activate-matrix-beacon    [:choose-activate-matrix-beacon    (choose-activate-matrix-beacon-choices state)]
      :choose-activate-matrix-spend     [:choose-activate-matrix-spend     (choose-activate-matrix-spend-choices state)]
      :choose-activate-tower-heading    [:choose-activate-tower-heading    (choose-activate-tower-heading-choices state)]
      :choose-activate-tower-join       [:choose-activate-tower-join       (choose-activate-tower-join-choices state)]
      :choose-activate-tower-join-spend [:choose-activate-tower-join-spend (choose-activate-tower-join-spend-choices state)]
      :choose-activate-tower-spend      [:choose-activate-tower-spend      (choose-activate-tower-spend-choices state)]
      :draw-cards                       [:draw-cards                       (choose-draw-cards-choices state)]
      :flare-beacon-join                [:flare-beacon-join                (choose-flare-beacon-join-choices state)]
      :flare-beacon-join-spend          [:flare-beacon-join-spend          (choose-flare-beacon-join-spend-choices state)]
      :keep-card                        [:keep-card                        (choose-keep-card-choices state)]
      :choose-captain-drift             [:choose-captain-drift             (choose-captain-drift-choices state)]
      :choose-ark-advance               [:choose-ark-advance               (choose-ark-advance-choices state)]
      :choose-flare-advance             [:choose-flare-advance             (choose-flare-advance-choices state)]
      :captain-beacon-join              [:captain-beacon-join              (choose-captain-beacon-join-choices state)]
      :captain-beacon-join-spend        [:captain-beacon-join-spend        (choose-captain-beacon-join-spend-choices state)]
      :cipher                           [:cipher                           (choose-cipher-choices state)]
      :cipher-spend                     [:cipher-spend                     (choose-cipher-spend-choices state)]
      :choose-land                      [:choose-land                      (choose-land-choices state)]
      :game-over                        [:game-over                        {}]
      [phase {}])))
