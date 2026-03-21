(ns journey.choice
  (:require
   [journey.game :as game]))

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
  (let [player (game/current-player state)]
    (into {}
          (map (fn [pos]
                 [pos (-> state
                          (game/launch-sundiver player pos)
                          use-move-point)])
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
          (map (fn [to-pos]
                 (let [owner (game/gate-owner state from-pos to-pos)]
                   [to-pos (-> (if owner
                                 (game/fly-through-gate state player from-pos to-pos)
                                 (game/fly-sundiver state player from-pos to-pos))
                               (update-in [:player-turn :action] dissoc :fly-from)
                               use-move-point)]))
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
  "Owner decides how many bonus actions to take (0..bonus-total).
   Remaining unclaimed bonus is offered to the activator next."
  [state]
  (let [bonus-total (get-in state [:player-turn :action :bonus-total] 0)]
    (into {}
          (map
           (fn [n]
             [n (let [remaining (- bonus-total n)
                      s (-> state
                            (assoc-in [:player-turn :action :owner-actions] n)
                            (assoc-in [:player-turn :choice-player] nil))]
                  (if (pos? remaining)
                    (assoc-in s [:player-turn :phase] :choose-activate-activator-extra)
                    (game/begin-actor-actions s :activator)))])
           (range (inc bonus-total))))))

(defn choose-activate-activator-extra-choices
  "Activator may take all unclaimed bonus actions or skip them."
  [state]
  (let [bonus-total (get-in state [:player-turn :action :bonus-total] 0)
        owner-took  (get-in state [:player-turn :action :owner-actions] 0)
        remaining   (- bonus-total owner-took)]
    {:take (-> state
               (update-in [:player-turn :action :activator-actions] + remaining)
               (game/begin-actor-actions :activator))
     :skip (game/begin-actor-actions state :activator)}))

;; --- activate helpers ---

(defn current-actor [state]
  (get-in state [:player-turn :action :current-actor]))

(defn actor-player [state]
  (game/actor-player state (current-actor state)))

(defn actor-actions-key [state]
  (let [actor (current-actor state)]
    (if (= actor :activator) :activator-actions :owner-actions)))

(defn tower-continue
  "After a tower action: continue to next heading choice or advance to next actor/station."
  [state]
  (let [actor (current-actor state)
        remaining (get-in state [:player-turn :action (actor-actions-key state)] 0)]
    (if (pos? remaining)
      (assoc-in state [:player-turn :phase] :choose-activate-tower-heading)
      (game/advance-after-actions state actor))))

(defn tower-after-advance
  "After Ark advances: handle beacon discovery if present."
  [state]
  (let [new-ark    (:ark state)
        act-player (actor-player state)
        tile       (game/get-tile state new-ark)]
    (if (and tile (:beacon tile))
      (let [beacon-owner (:beacon tile)
            s (game/discover-beacon state new-ark act-player)]
        (if (= beacon-owner act-player)
          ;; Own beacon: discover it and continue
          (-> s
              (update-in [:player-turn :action (actor-actions-key s)] dec)
              tower-continue)
          ;; Another player's beacon: offer join
          (-> s
              (assoc-in [:player-turn :action :pending-join-actor] (current-actor s))
              (assoc-in [:player-turn :phase] :choose-activate-tower-join))))
      ;; No beacon: continue
      (-> state
          (update-in [:player-turn :action (actor-actions-key state)] dec)
          tower-continue))))

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
  "Choose heading turn (:none/:left/:right), then advance Ark."
  [state]
  (into {}
        (map
         (fn [turn-dir]
           [turn-dir (-> state
                         (game/turn-heading turn-dir)
                         game/advance-ark
                         tower-after-advance)])
         [:none :left :right])))

(defn finish-tower-join
  "Execute join after spending is resolved, then continue tower actions."
  [state]
  (let [actor      (get-in state [:player-turn :action :pending-join-actor])
        act-player (game/actor-player state actor)
        act-key    (if (= actor :activator) :activator-actions :owner-actions)]
    (-> state
        (game/join-beacon-to-cipher act-player)
        (update-in [:player-turn :action :beacons-joined] (fnil inc 0))
        (update-in [:player-turn :action act-key] dec)
        (update-in [:player-turn :action] dissoc :pending-join-actor :join-spend-remaining)
        tower-continue)))

(defn choose-activate-tower-join-choices
  "Activator may add their own beacon to the cipher for the discovered world.
   Cost is :beacons-joined sundivers (0 for first join, 1 for second, etc.)."
  [state]
  (let [actor          (get-in state [:player-turn :action :pending-join-actor])
        act-player     (game/actor-player state actor)
        act-key        (if (= actor :activator) :activator-actions :owner-actions)
        join-cost      (get-in state [:player-turn :action :beacons-joined] 0)
        has-beacons    (pos? (get-in state [:players act-player :reserve :beacons] 0))
        can-afford     (>= (game/total-spendable-sundivers state act-player) join-cost)
        can-join        (and has-beacons can-afford)
        after-skip     (-> state
                           (update-in [:player-turn :action act-key] dec)
                           (update-in [:player-turn :action] dissoc :pending-join-actor)
                           tower-continue)]
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
  "After keeping a card: go to captain drift phase if current player is captain."
  [state]
  (let [player  (game/current-player state)
        captain (:captain-flame state)]
    (assoc-in state [:player-turn :phase]
              (if (= player captain) :choose-captain-drift :cipher))))

(defn choose-keep-card-choices
  "Player picks one drawn card to hold; previously held card and others are discarded.
   If no cards were drawn, this phase is skipped automatically."
  [state]
  (let [player  (game/current-player state)
        drawn   (get-in state [:player-turn :action :drawn-cards] [])
        held    (get-in state [:players player :held-card])]
    (cond-> {}
      ;; Keep each drawn card as an option
      (seq drawn)
      (into (map (fn [card]
                   [card (-> state
                             (assoc-in [:players player :held-card] card)
                             (update :discard into (remove #{card} drawn))
                             (cond-> held (update :discard conj held))
                             advance-to-captain-or-cipher)])
                 drawn))
      ;; Keep previously held card (discard all drawn)
      (and (seq drawn) held)
      (assoc :keep-held (-> state
                            (update :discard into drawn)
                            advance-to-captain-or-cipher)))))

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
          (assoc-in s [:player-turn :phase] :cipher)
          (assoc-in s [:player-turn :phase] :captain-beacon-join)))
      (assoc-in state [:player-turn :phase] :cipher))))

(defn choose-captain-drift-choices
  "Captain may turn heading once (:none/:left/:right), then Ark advances."
  [state]
  (into {}
        (map
         (fn [turn-dir]
           [turn-dir (-> state
                         (game/turn-heading turn-dir)
                         game/advance-ark
                         handle-captain-drift-beacon)])
         [:none :left :right])))

(defn finish-captain-join
  [state]
  (let [captain (:captain-flame state)]
    (-> state
        (game/join-beacon-to-cipher captain)
        (update-in [:player-turn :captain-beacons-joined] (fnil inc 0))
        (update-in [:player-turn :action] dissoc :captain-join-spend-remaining)
        (assoc-in [:player-turn :phase] :cipher))))

(defn choose-captain-beacon-join-choices
  "Captain may add their own beacon to the cipher for the discovered world."
  [state]
  (let [captain     (:captain-flame state)
        join-cost   (get-in state [:player-turn :captain-beacons-joined] 0)
        has-beacons (pos? (get-in state [:players captain :reserve :beacons] 0))
        can-afford  (>= (game/total-spendable-sundivers state captain) join-cost)]
    (cond-> {:skip (assoc-in state [:player-turn :phase] :cipher)}
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

;; --- cipher (stub: resolve pending-cipher, then next player's turn) ---

(defn choose-cipher-choices
  [state]
  {:done (game/begin-next-player-turn state)})

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
      :choose-activate-owner-bonus      [:choose-activate-owner-bonus      (choose-activate-owner-bonus-choices state)]
      :choose-activate-activator-extra  [:choose-activate-activator-extra  (choose-activate-activator-extra-choices state)]
      :choose-activate-matrix-beacon    [:choose-activate-matrix-beacon    (choose-activate-matrix-beacon-choices state)]
      :choose-activate-matrix-spend     [:choose-activate-matrix-spend     (choose-activate-matrix-spend-choices state)]
      :choose-activate-tower-heading    [:choose-activate-tower-heading    (choose-activate-tower-heading-choices state)]
      :choose-activate-tower-join       [:choose-activate-tower-join       (choose-activate-tower-join-choices state)]
      :choose-activate-tower-join-spend [:choose-activate-tower-join-spend (choose-activate-tower-join-spend-choices state)]
      :draw-cards                       [:draw-cards                       (choose-draw-cards-choices state)]
      :flare-beacon-join                [:flare-beacon-join                (choose-flare-beacon-join-choices state)]
      :flare-beacon-join-spend          [:flare-beacon-join-spend          (choose-flare-beacon-join-spend-choices state)]
      :keep-card                        [:keep-card                        (choose-keep-card-choices state)]
      :choose-captain-drift             [:choose-captain-drift             (choose-captain-drift-choices state)]
      :captain-beacon-join              [:captain-beacon-join              (choose-captain-beacon-join-choices state)]
      :captain-beacon-join-spend        [:captain-beacon-join-spend        (choose-captain-beacon-join-spend-choices state)]
      :cipher                           [:cipher                           (choose-cipher-choices state)]
      [phase {}])))
