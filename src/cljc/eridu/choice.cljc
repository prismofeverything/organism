(ns eridu.choice
  (:require
   [clojure.string :as str]
   [eridu.game :as game]))

;; =============================================================================
;; Helpers
;; =============================================================================

;; Forward declaration: travel-to-city is defined further down (Travel action),
;; but board 14's bonus move in choose-action-choices needs it.
(declare travel-to-city)

(defn partial-map [f s]
  (into {} (map (juxt identity f) s)))

(defn add-resource [state player resource n]
  (update-in state [:players player :resources resource] + n))

(defn spend-resource [state player resource]
  (-> state
      (update-in [:players player :resources resource] dec)
      ;; Passive trigger: resource-spent (board 29: gold → +2 amity)
      (game/apply-passive player :resource-spent {:resource resource})))

(defn add-amity [state player n]
  (-> state
      (update-in [:players player :amity] + n)
      (update-in [:turn-stats :amity] (fnil + 0) n)))

(defn add-glory [state player n]
  (-> state
      (update-in [:players player :glory] + n)
      (update-in [:turn-stats :glory] (fnil + 0) n)))

(defn city-surrounded-by-player?
  "True if every active route adjacent to `city` has one of `player`'s raiders."
  [state player city]
  (let [pc (count (:turn-order state))
        adj-routes (game/routes-from-city city (game/active-routes pc))
        adj-rks (map game/segment-route-key adj-routes)
        player-rks (set (keys (get-in state [:players player :raiders])))]
    (and (seq adj-rks)
         (every? #(contains? player-rks %) adj-rks))))

(defn- return-to-choose-action
  "Reconstruct player-turn to return to choose-action phase, preserving solo-landing."
  [state]
  (let [turn (:player-turn state)]
    (assoc state :player-turn
           (cond-> {:phase :choose-action
                    :space (:space turn)
                    :actions-remaining (:actions-remaining turn)
                    :used-icons (:used-icons turn)}
             (:solo-landing turn) (assoc :solo-landing true)))))

;; ── Game log ─────────────────────────────────────────────────────────────────

(defn add-log
  "Append a log entry to the game state."
  [state entry]
  (update state :log (fnil conj [])
          (merge {:round  (:round state 1)
                  :turn   (:turn-in-round state 1)
                  :player (game/current-player state)}
                 entry)))

;; =============================================================================
;; Phase 1: Choose a die
;; =============================================================================

(defn choose-die-choices
  "Player picks one of their available dice."
  [state]
  (let [player (game/current-player state)
        dice (get-in state [:players player :dice-available])]
    (into {}
          (map-indexed
           (fn [idx die-val]
             [idx (-> state
                      ;; Passive: turn-start trigger (board 35: no goods → gain one)
                      (game/apply-passive player :turn-start {})
                      ;; Clear per-turn passive flags
                      (update-in [:players player] dissoc :used-uruk-travel)
                      ;; Reset per-turn stats (keyed by player so event feats
                      ;; can't use another player's actions)
                      (assoc :turn-stats {:player player
                                          :amity 0 :glory 0
                                          :temples-flipped 0
                                          :magistrate-max-move 0
                                          :magistrate-moves {}
                                          :magistrate-raiders-flipped 0
                                          :magistrate-flips {}
                                          :sold-resource nil
                                          :sold-in-city nil
                                          :sell-amity 0 :sell-glory 0})
                      (update-in [:players player :dice-available]
                                 (fn [d] (into (subvec d 0 idx) (subvec d (inc idx)))))
                      (update-in [:players player :dice-used] conj die-val)
                      (assoc :player-turn {:phase :choose-astronomer
                                           :die-value die-val})
                      (add-log {:type :die :message (str "Selected die " die-val)
                                :die-value die-val}))])
           dice))))

;; =============================================================================
;; Phase 2: Choose which astronomer to move
;; =============================================================================

(defn choose-astronomer-choices
  "Player picks which of their astronomers to move.
   In solo mode, only offers astronomers from the active color pair."
  [state]
  (let [player (game/current-player state)
        astronomers (get-in state [:players player :astronomers])
        die-val (get-in state [:player-turn :die-value])
        ;; In solo mode, restrict to active pair's indices
        active-indices (if (game/solo-mode? state)
                         (set (game/solo-active-indices state))
                         (set (range (count astronomers))))
        color-label (when (game/solo-mode? state)
                      (let [round (:round state 1)]
                        (get game/solo-color-names (dec round) "?")))]
    ;; Offer each eligible astronomer as a choice
    (into {}
          (for [[idx current-space] (map-indexed vector astronomers)
                :when (contains? active-indices idx)]
            (let [dest (game/move-astronomer-clockwise current-space die-val)
                  astros-on-dest (count (game/astronomers-on-space state dest))]
              [idx (-> state
                       (assoc-in [:players player :astronomers idx] dest)
                       (assoc :player-turn {:phase :resolve-landing
                                            :landed-space dest})
                       (add-log {:type :astronomer
                                 :message (str "Moved "
                                               (when color-label (str color-label " "))
                                               "astronomer " (inc idx)
                                               " from space " current-space
                                               " to space " dest
                                               " (" (inc astros-on-dest) " astronomers there)")
                                 :from-space current-space :to-space dest}))])))))

;; =============================================================================
;; Phase 3: Resolve landing - count astronomers, decide actions vs role increase
;; =============================================================================

(defn resolve-landing-choices
  "After astronomer lands, determine if player takes actions or increases a role.
   If alone on space: may increase a role.
   If others present: take N actions (one per astronomer on space)."
  [state]
  (let [player (game/current-player state)
        space (get-in state [:player-turn :landed-space])
        all-on-space (game/astronomers-on-space state space)
        num-actions (count all-on-space)
        ;; Track space usage: [space astronomer-count] per player
        state (-> state
                  (update-in [:players player :space-visits] (fnil conj [])
                             {:space space :astronomers num-actions})
                  (update-in [:space-action-counts space] (fnil inc 0)))
        ;; If space 7, take first-player card
        state (if (= space 7)
                (-> state
                    (assoc :first-player player)
                    (add-log {:type :first-player :message "Takes First Player card (space 7)"}))
                state)]
    ;; Trigger landing passive (boards 16, 28, 31)
    (let [state (game/apply-passive state player :landing
                                    {:astronomer-count num-actions :space space})
          ;; Trigger action-space-7 passive (boards 6, 17, 22)
          state (if (= space 7)
                  (game/apply-passive state player :action-space-7 {:space space})
                  state)
          ;; Check bonus-extra-action flag (Board 16: 2-astronomer → 3rd action)
          pdata-post (game/player-data state player)
          extra-action? (:bonus-extra-action pdata-post)
          effective-actions (if (and extra-action? (> num-actions 1))
                             (inc num-actions)
                             num-actions)
          ;; Clear the flag after use
          state (if extra-action?
                  (update-in state [:players player] dissoc :bonus-extra-action)
                  state)]
      (if (= num-actions 1)
        ;; Alone on space: get 1 action AND a role increase afterward.
        ;; The solo-landing flag causes after-actions to transition to
        ;; :choose-role-increase instead of advancing the turn.
        {:begin (-> state
                    (assoc :player-turn
                           {:phase :choose-action
                            :space space
                            :actions-remaining 1
                            :used-icons #{}
                            :solo-landing true})
                    (add-log {:type :landing :message (str "Alone on space " space " — taking 1 action + role increase")}))}
        ;; Multiple astronomers: take N actions, no role increase
        {:begin (-> state
                    (assoc :player-turn
                           {:phase :choose-action
                            :space space
                            :actions-remaining effective-actions
                            :used-icons #{}})
                    (add-log {:type :landing
                              :message (str num-actions " astronomers on space " space
                                            " — taking " effective-actions " actions"
                                            (when extra-action?
                                              " (bonus 3rd action from board)"))}))}))))


;; =============================================================================
;; Role increase (when alone on a space)
;; =============================================================================

(defn- can-pay-cost? [resources cost]
  (cond
    (nil? cost) true
    (vector? cost) (every? #(pos? (get resources % 0)) cost)
    :else (pos? (get resources cost 0))))

(defn- pay-cost [state player cost]
  (cond
    (nil? cost) state
    (vector? cost) (reduce (fn [s c] (spend-resource s player c)) state cost)
    :else (spend-resource state player cost)))

(defn- cost-label [cost]
  (cond
    (nil? cost) ""
    (vector? cost) (str/join "+" (map name cost))
    :else (name cost)))

(defn choose-role-increase-choices
  "Player picks which role to increase (if below max)."
  [state]
  (let [player (game/current-player state)
        role-levels (get-in state [:players player :roles])
        resources (get-in state [:players player :resources])
        free-increase? (get-in state [:players player :free-role-increase])
        ;; Clear the free-role-increase flag so it only applies once
        state (if free-increase?
                (update-in state [:players player] dissoc :free-role-increase)
                state)
        increasable
        (for [role game/roles
              :let [current-level (get role-levels role 1)]
              :when (< current-level game/max-role-level)
              :let [next-level (inc current-level)
                    cost (get-in game/role-threshold-costs [role next-level])]
              :when (or free-increase? (can-pay-cost? resources cost))]
          [role (cond-> state
                  (and cost (not free-increase?)) (pay-cost player cost)
                  true (assoc-in [:players player :roles role] next-level)
                  true (add-log {:type :role-increase
                                 :message (str "Increased " (name role)
                                               " to level " next-level
                                               (if free-increase?
                                                 " (free — board 15)"
                                                 (when cost (str " (paid " (cost-label cost) ")"))))
                                 :role role :level next-level :cost cost})
                  ;; Passive trigger: role-increased (boards 15, 27)
                  true (game/apply-passive player :role-increased
                                           {:role role :level next-level})
                  ;; Transition to turn-complete instead of advancing immediately
                  true (assoc :player-turn {:phase :turn-complete}))])]
    (if (seq increasable)
      (into {:skip (-> state
                       (add-log {:type :role-increase :message "Skipped role increase"})
                       (assoc :player-turn {:phase :turn-complete}))}
            increasable)
      {:skip (-> state
                 (add-log {:type :role-increase :message "No roles available to increase"})
                 (assoc :player-turn {:phase :turn-complete}))})))

;; =============================================================================
;; Action selection (when multiple astronomers on space)
;; =============================================================================

(defn available-action-indices
  "Return indices of actions on the space that haven't been used (by icon type)."
  [space used-icons]
  (let [actions (:actions (get game/action-spaces space))]
    (vec
     (for [[idx _action] (map-indexed vector actions)
           :when (not (contains? used-icons idx))]
       idx))))

(defn choose-action-choices
  "Player picks an action from the current space."
  [state]
  (let [{:keys [space actions-remaining used-icons solo-landing]} (:player-turn state)
        solo-landing? solo-landing
        player (game/current-player state)
        pdata (game/player-data state player)
        ;; Board 22: bonus-repeat-action allows re-using the same action index on space 7
        repeat-action? (:bonus-repeat-action pdata)
        effective-used (if repeat-action? #{} used-icons)
        available (available-action-indices space effective-used)
        ;; Board 6: pending-free-travel injects a travel action
        free-travel? (:pending-free-travel pdata)
        has-travel-in-available? (some (fn [idx]
                                         (= :travel (:type (nth (:actions (get game/action-spaces space)) idx))))
                                       available)
        ;; ── Board 14 (Roads of Shulgi): bonus move between Uruk and an
        ;;    adjacent city by discarding one good. Does NOT consume an action
        ;;    or an icon (free), and stays in :choose-action. Capped to once per
        ;;    turn via :used-uruk-travel (cleared at turn start). ─────────────
        board-14? (and (game/has-passive? state player)
                       (= 14 (game/player-board-id state player)))
        caravan-city (:caravan pdata)
        held-good (first (filter #(pos? (get-in pdata [:resources %] 0))
                                 game/resource-types))
        uruk-neighbors (get-in state [:city-graph :uruk] #{})
        ;; Valid bonus-move destinations: from Uruk → each neighbor; from a
        ;; neighbor → Uruk. Only when the player still holds a good and hasn't
        ;; already used this bonus move this turn.
        uruk-dests (cond
                     (not (and board-14? held-good
                               (not (:used-uruk-travel pdata)))) []
                     (= caravan-city :uruk) (vec uruk-neighbors)
                     (contains? uruk-neighbors caravan-city) [:uruk]
                     :else [])
        uruk-move-choice-state
        (fn [dest]
          (-> state
              (spend-resource player held-good)
              (travel-to-city player dest)
              (assoc-in [:players player :used-uruk-travel] true)
              ;; Stay in choose-action; preserve the action budget exactly
              ;; (free bonus action — no icon consumed, no action spent).
              (assoc :player-turn
                     (cond-> {:phase :choose-action
                              :space space
                              :actions-remaining (or actions-remaining 0)
                              :used-icons used-icons}
                       solo-landing? (assoc :solo-landing true)))
              (add-log {:type :uruk-move
                        :message (str "Bonus move (board 14): discarded "
                                      (get game/resource-icons held-good "")
                                      " " (name held-good) " to move caravan to "
                                      (str/capitalize (name dest)))
                        :to dest :discarded held-good})))
        uruk-move-choices
        (into {} (for [dest uruk-dests]
                   [[:uruk-move dest] (uruk-move-choice-state dest)]))]
    (let [after-actions
          (fn [s]
            (let [sl? (get-in s [:player-turn :solo-landing] solo-landing?)
                  ;; Board 22: clear bonus-repeat-action flag when done with actions
                  s (update-in s [:players player] dissoc :bonus-repeat-action)]
              (if sl?
                (-> s
                    (assoc :player-turn {:phase :choose-role-increase})
                    (add-log {:type :landing
                              :message "Action complete — now choose role increase (alone on space)"}))
                (game/advance-turn s))))]
      (if (or (not (pos? (or actions-remaining 0))) (empty? available))
        ;; Done with actions — check for pending free travel, then solo-landing role increase.
        ;; Board 14 bonus moves are still offered here (they don't need an action).
        (if free-travel?
          (let [state (update-in state [:players player] dissoc :pending-free-travel)]
            (merge uruk-move-choices
                   {:free-travel (-> state
                                     (assoc :player-turn
                                            {:phase :resolve-travel
                                             :space space
                                             :action {:type :travel}
                                             :action-index -1
                                             :actions-remaining 0
                                             :used-icons used-icons
                                             :solo-landing solo-landing?})
                                     (add-log {:type :action-select
                                               :message "Free travel action (bonus board)"
                                               :action-type :travel}))
                    :done (-> (update-in state [:players player] dissoc :pending-free-travel)
                              after-actions)}))
          ;; No free travel, no actions left — set transitional phase
          (merge uruk-move-choices {:done (after-actions state)}))
        ;; ELSE: has remaining actions — offer action indices + done
        (let [base-choices
            (into {}
                  (for [idx available
                        :let [action (nth (:actions (get game/action-spaces space)) idx)]]
                    [idx (-> state
                             (assoc :player-turn
                                    (cond-> {:phase (case (:type action)
                                                      :take     :resolve-take
                                                      :sell     :resolve-sell
                                                      :deploy   :resolve-deploy
                                                      :travel   :resolve-travel
                                                      :influence :resolve-influence
                                                      :temple   :resolve-temple)
                                             :space space
                                             :action action
                                             :action-index idx
                                             :actions-remaining (dec actions-remaining)
                                             :used-icons (conj used-icons idx)}
                                      solo-landing? (assoc :solo-landing true)))
                             (add-log {:type :action-select
                                       :message (str "Selected action: "
                                                     (get game/action-icons (:type action) "")
                                                     " " (name (:type action))
                                                     (when (:resources action)
                                                       (str " (" (str/join ", " (map name (:resources action))) ")")))
                                       :action-type (:type action)}))]))
            ;; If free travel is pending and no travel available on this space, inject one
            free-travel-choice
            (when (and free-travel? (not has-travel-in-available?))
              {:free-travel (-> (update-in state [:players player] dissoc :pending-free-travel)
                               (assoc :player-turn
                                      {:phase :resolve-travel
                                       :space space
                                       :action {:type :travel}
                                       :action-index -1
                                       :actions-remaining actions-remaining ;; free, doesn't cost an action
                                       :used-icons used-icons})
                               (add-log {:type :action-select
                                         :message "Free travel action (bonus board)"
                                         :action-type :travel}))})]
        ;; Offer :done to end turn early (but not needed most of the time).
        ;; Board 14 bonus moves are merged alongside the normal action choices.
        (cond-> (merge base-choices free-travel-choice uruk-move-choices)
          ;; Only add :done when there are no must-take actions
          ;; (player can always use the action, so :done is optional escape)
          true (assoc :done (after-actions state))))))))

;; =============================================================================
;; Action: Take Goods
;; =============================================================================

(defn resolve-take-choices [state]
  (let [player (game/current-player state)
        turn (:player-turn state)
        resources (get-in state [:player-turn :action :resources])
        ;; Build a take-result state for an arbitrary resource list + log suffix.
        take-result
        (fn [take-resources log-suffix]
          (-> (reduce #(add-resource %1 player %2 1) state take-resources)
              ;; Trigger goods-taken passive (board 19: extra pottery)
              (game/apply-passive player :goods-taken {:resources take-resources})
              (assoc :player-turn
                     (cond-> {:phase :choose-action
                              :space (:space turn)
                              :actions-remaining (:actions-remaining turn)
                              :used-icons (:used-icons turn)}
                       (:solo-landing turn) (assoc :solo-landing true)))
              (add-log {:type :take
                        :message (str "Took goods: "
                                      (str/join ", " (map #(str (get game/resource-icons % "") " " (name %))
                                                          take-resources))
                                      log-suffix)})))
        ;; ── Board 30 (Council of Amar-Sin): may take goods based on ONE of
        ;;    the player's OTHER astronomers' wheel positions INSTEAD. ───────
        board-30? (and (game/has-passive? state player)
                       (= 30 (game/player-board-id state player)))
        current-space (:space turn)
        alt-choices
        (when board-30?
          (into {}
                (for [alt-space (distinct (get-in state [:players player :astronomers]))
                      :when (not= alt-space current-space)
                      :let [alt-res (game/space-take-resources alt-space)]
                      :when (seq alt-res)]
                  [[:alt-take alt-space]
                   (take-result alt-res
                                (str " (board 30: from astronomer on space "
                                     alt-space " instead)"))])))]
    (merge {:done (take-result resources "")}
           alt-choices)))

;; =============================================================================
;; Action: Sell
;; =============================================================================

(defn resolve-sell-choices
  "Sell: discard a good matching city demand, take demand marker, score amity."
  [state]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        city (:caravan pdata)
        demands (get-in state [:city-demands city] [])
        resources (:resources pdata)
        merchant-level (get-in pdata [:roles :merchant] 1)
        amity-score (get game/merchant-score merchant-level 2)
        leader-level (get-in pdata [:roles :leader] 1)
        has-magistrate? (game/magistrate-in-city? state city)
        glory-bonus (if has-magistrate? (get game/leader-bonus leader-level 0) 0)
        ;; Find demands the player can satisfy
        sellable (distinct
                  (for [demand demands
                        :when (pos? (get resources demand 0))]
                    demand))
        ;; ── Board 10: sell Gold to a city with NO demands ──────────────────
        ;; "You may sell Gold to cities with no demands. If you do, place a
        ;;  random Demand Token on that city." Default ruling: payout = normal
        ;; merchant-level amity (the placed demand is a side-effect, not
        ;; fulfilled). Only offered when the city has zero demands AND the
        ;; player holds gold AND board-10 slot-0 passive is uncovered.
        board-10? (and (game/has-passive? state player)
                       (= 10 (game/player-board-id state player))
                       (empty? demands)
                       (pos? (get resources :gold 0)))
        sell-gold-empty-choice
        (when board-10?
          {:sell-gold-empty
           (let [bag (:demand-bag state (game/full-demand-bag))
                 [bag' token] (game/draw-demand-token bag)
                 s (-> state
                       (spend-resource player :gold)
                       (cond-> bag' (assoc :demand-bag bag'))
                       (cond-> token (update-in [:city-demands city]
                                                (fnil conj []) token))
                       (add-amity player amity-score)
                       (assoc-in [:turn-stats :sold-resource] :gold)
                       (assoc-in [:turn-stats :sold-in-city] city)
                       (assoc-in [:turn-stats :sell-amity] amity-score)
                       (assoc-in [:turn-stats :sell-glory] glory-bonus))
                 s (cond-> s (pos? glory-bonus) (add-glory player glory-bonus))]
             (-> s
                 (update-in [:players player :sells-this-round] (fnil inc 0))
                 ;; Fire the :sold passive (same shape as a normal sell)
                 (game/apply-passive player :sold
                                     {:amity-scored amity-score
                                      :glory-scored glory-bonus
                                      :resource :gold
                                      :city city})
                 return-to-choose-action
                 (add-log {:type :sell
                           :message (str "Sold " (get game/resource-icons :gold "")
                                         " gold to " (str/capitalize (name city))
                                         " (no demands) → +" amity-score " Amity"
                                         " (Merchant lv" merchant-level ")"
                                         (when token
                                           (str "; placed " (name token)
                                                " demand (board 10)"))
                                         (when (pos? glory-bonus)
                                           (str ", +" glory-bonus " Glory")))
                            :city city :demand :gold
                            :placed-demand token
                            :amity amity-score :glory glory-bonus
                            :board-10 true})))})]
    (if (seq sellable)
      (into (merge {:skip (return-to-choose-action state)} sell-gold-empty-choice)
            (for [demand sellable]
              [demand (let [;; Remove demand from city
                            new-demands (let [idx (.indexOf demands demand)]
                                          (into (subvec (vec demands) 0 idx)
                                                (subvec (vec demands) (inc idx))))
                            s (-> state
                                  (spend-resource player demand)
                                  (assoc-in [:city-demands city] new-demands)
                                  (update-in [:players player :demand-tokens] conj demand)
                                  (add-amity player amity-score)
                                  ;; Track sell stats for event-based feats
                                  (assoc-in [:turn-stats :sold-resource] demand)
                                  (assoc-in [:turn-stats :sold-in-city] city)
                                  (assoc-in [:turn-stats :sell-amity] amity-score)
                                  (assoc-in [:turn-stats :sell-glory] glory-bonus))
                            s (cond-> s
                                (pos? glory-bonus) (add-glory player glory-bonus))]
                        (-> s
                            ;; Track sells for round budget
                            (update-in [:players player :sells-this-round] (fnil inc 0))
                            ;; Passive trigger: sold (boards 23, 26, 32)
                            (game/apply-passive player :sold
                                                {:amity-scored amity-score
                                                 :glory-scored glory-bonus
                                                 :resource demand
                                                 :city city})
                            return-to-choose-action
                            (add-log {:type :sell
                                      :message (str "Sold " (get game/resource-icons demand "")
                                                    " " (name demand) " in "
                                                    (str/capitalize (name city))
                                                    " → +" amity-score " Amity"
                                                    " (Merchant lv" merchant-level ")"
                                                    (when (pos? glory-bonus)
                                                      (str ", +" glory-bonus " Glory"
                                                           " (Leader lv" leader-level
                                                           " magistrate bonus)")))
                                      :city city :demand demand
                                      :amity amity-score :glory glory-bonus})))]))
      ;; Nothing to sell via demand-matching
      (merge {:skip (-> state
                        return-to-choose-action
                        (add-log {:type :sell :message (str "No sellable demands in "
                                                            (str/capitalize (name city)))}))}
             sell-gold-empty-choice))))

;; =============================================================================
;; Action: Temple
;; =============================================================================

(defn resolve-temple-choices
  "Place a temple face-up in a city where the caravan or a magistrate is."
  [state]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        priest-level (get-in pdata [:roles :priest] 1)
        max-temples (get game/priest-max-temples priest-level 3)
        placed (game/count-temples-placed pdata)
        supply (:temples-supply pdata)]
    (if (and (pos? supply) (< placed max-temples))
      (let [caravan-city (:caravan pdata)
            all-magistrate-cities (game/magistrate-cities state)
            ;; NORMAL temple action = 1 per city: only cities where the player
            ;; has no temple yet are valid. (Only bonus-board effects place a 2nd.)
            valid-cities (distinct
                          (for [city (cons caravan-city all-magistrate-cities)
                                :when (and city
                                           (not (game/has-temple? pdata city)))]
                            city))]
        (if (seq valid-cities)
          (into {:skip (return-to-choose-action state)}
                (for [city valid-cities]
                  [city (-> state
                            ;; add-temple conj's :face-up, decs supply, fires the
                            ;; :temple-placed passive (boards 13, 21).
                            (game/add-temple player city :face-up)
                            return-to-choose-action
                            (add-log {:type :temple
                                      :message (str "Placed face-up temple in "
                                                    (str/capitalize (name city))
                                                    (when (= city caravan-city) " (caravan)")
                                                    (when (game/magistrate-in-city? state city)
                                                      " (magistrate city)"))
                                      :city city}))]))
          {:skip (-> state return-to-choose-action
                     (add-log {:type :temple :message "No valid cities for temple"}))}))
      {:skip (-> state return-to-choose-action
                 (add-log {:type :temple
                           :message (str "Cannot place temple"
                                         (when-not (pos? supply) " (none in supply)")
                                         (when-not (< placed max-temples)
                                           (str " (max " max-temples " at Priest lv" (get-in pdata [:roles :priest] 1) ")"))
                                         )}))})))

;; =============================================================================
;; Action: Deploy
;; =============================================================================

(defn resolve-deploy-choices
  "Place or move up to 2 raiders on routes adjacent to caravan's city."
  [state]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        raider-level (get-in pdata [:roles :raider] 1)
        max-deployed (get game/raider-max-deployed raider-level 2)
        current-deployed (game/count-raiders-deployed pdata)
        supply (:raiders-supply pdata)
        turn (:player-turn state)
        caravan-city (:caravan pdata)
        adjacent-routes (game/routes-from-city caravan-city (:routes state))
        deploys-left (get-in state [:player-turn :deploys-left] 2)]
    (if (zero? deploys-left)
      {:done (return-to-choose-action state)}
      (let [;; Board 25: allow two raiders per path
            double-raiders? (get-in state [:players player :allow-double-raiders])
            ;; Routes where player can place a raider
            placeable (for [route adjacent-routes
                           :let [rk (game/segment-route-key route)]
                           :when (or double-raiders?
                                     (not (contains? (:raiders pdata) rk)))]
                        rk)
            can-place? (and (pos? supply)
                            (< current-deployed max-deployed)
                            (seq placeable))]
        (if can-place?
          (into {:skip (return-to-choose-action state)}
                (for [rk placeable
                      :let [[c1 c2] rk]]
                  [rk (let [s (-> state
                                  (assoc-in [:players player :raiders rk] :raiding)
                                  (update-in [:players player :raiders-supply] dec)
                                  ;; Track deploys for round budget
                                  (update-in [:players player :deploys-this-round] (fnil inc 0)))
                            ;; After placement, see if either endpoint city is now
                            ;; fully surrounded by this player's raiders (board 1).
                            surrounded (first (filter #(city-surrounded-by-player? s player %)
                                                      [c1 c2]))]
                        (-> s
                            ;; Passive trigger: deployed (boards 1, 7, 24, 25, 33)
                            (game/apply-passive player :deployed
                                                {:route rk
                                                 :surrounded-city surrounded})
                            (assoc :player-turn
                                   (cond-> {:phase :resolve-deploy
                                            :space (:space turn)
                                            :action (get-in state [:player-turn :action])
                                            :action-index (get-in state [:player-turn :action-index])
                                            :actions-remaining (:actions-remaining turn)
                                            :used-icons (:used-icons turn)
                                            :deploys-left (dec deploys-left)}
                                     (:solo-landing turn) (assoc :solo-landing true)))
                            (add-log {:type :deploy
                                      :message (str "Placed raider between "
                                                    (str/capitalize (name c1)) " and "
                                                    (str/capitalize (name c2))
                                                    " (raiding side)"
                                                    (when surrounded
                                                      (str " — surrounded "
                                                           (str/capitalize (name surrounded)))))
                                      :route rk
                                      :surrounded-city surrounded})))]))
          {:done (-> state return-to-choose-action
                     (add-log {:type :deploy :message "No more raiders to deploy"}))})))))

;; =============================================================================
;; Action: Travel
;; =============================================================================

(defn visit-temples-on-travel
  "When caravan enters a city with a face-up temple, flip it and score amity."
  [state player city]
  (let [pdata (game/player-data state player)]
    (if (game/city-has-own-face-up-temple? pdata city)
      (let [;; Flip ONE face-up temple in this city to face-down
            state (-> state
                      (game/flip-one-temple player city)
                      (update-in [:turn-stats :temples-flipped] (fnil inc 0)))
            face-down-count (inc (game/count-face-down-temples pdata))
            ;; Score amity = number of face-down temples
            state (add-amity state player face-down-count)
            ;; Magistrate bonus
            leader-level (get-in state [:players player :roles :leader] 1)
            has-magistrate? (game/magistrate-in-city? state city)
            glory-bonus (if has-magistrate? (get game/leader-bonus leader-level 0) 0)]
        (-> (cond-> state
              (pos? glory-bonus) (add-glory player glory-bonus))
            (add-log {:type :temple-visit
                      :message (str "Visited temple in " (str/capitalize (name city))
                                    " — flipped face-down → +" face-down-count " Amity"
                                    " (" face-down-count " face-down temples)"
                                    (when (pos? glory-bonus)
                                      (str ", +" glory-bonus " Glory"
                                           " (Leader lv" leader-level " magistrate bonus)")))
                      :city city :amity face-down-count :glory glory-bonus})
            ;; Passive triggers: boards 4 (sell), 9 (role+), 20 (pottery→glory)
            (game/apply-passive player :temple-flipped {:city city})))
      state)))

(defn flip-enemy-raiders-on-route
  "When caravan travels a route, flip any opposing raiders to :point side."
  [state player route-key]
  (reduce-kv
   (fn [s pk pdata]
     (if (and (not= pk player)
              (= :raiding (get-in pdata [:raiders route-key])))
       (-> s
           (assoc-in [:players pk :raiders route-key] :point)
           (add-log {:type :raider-flip
                     :message (str "Flipped " pk "'s raider on "
                                   (str/capitalize (name (first route-key))) "—"
                                   (str/capitalize (name (second route-key)))
                                   " to point side (caravan passed)")
                     :owner pk :route route-key}))
       s))
   state
   (:players state)))

(defn score-own-raider-on-route
  "When caravan travels a route with own :point raider, remove and score 4 glory."
  [state player route-key]
  (let [raider-state (get-in state [:players player :raiders route-key])]
    (if (= raider-state :point)
      (let [;; Fire passive first to set flags (e.g. board 8 :keep-scored-raider)
            state (game/apply-passive state player :raider-scored {:route route-key})
            keep? (get-in state [:players player :keep-scored-raider])
            amity-instead? (get-in state [:players player :raider-score-amity])
            state (if keep?
                    ;; Board 8: flip back to :raiding instead of removing
                    (-> state
                        (assoc-in [:players player :raiders route-key] :raiding)
                        (update-in [:players player] dissoc :keep-scored-raider))
                    ;; Normal: remove raider, return to supply
                    (-> state
                        (update-in [:players player :raiders] dissoc route-key)
                        (update-in [:players player :raiders-supply] inc)))
            ;; Clear board 34 flag
            state (if amity-instead?
                    (update-in state [:players player] dissoc :raider-score-amity)
                    state)]
        (-> state
            ;; Board 34: amity instead of glory
            (as-> s (if amity-instead?
                      (update-in s [:players player :amity] + 4)
                      (add-glory s player 4)))
            (add-log {:type :raider-score
                      :message (str "Scored own raider on "
                                    (str/capitalize (name (first route-key))) "—"
                                    (str/capitalize (name (second route-key)))
                                    (if amity-instead?
                                      " → +4 Amity (board 34)"
                                      " → +4 Glory")
                                    (if keep?
                                      " (raider flipped to active)"
                                      " (raider returned to supply)"))
                      :route route-key
                      :glory (if amity-instead? 0 4)
                      :amity (if amity-instead? 4 0)})))
      state)))

(defn travel-to-city
  "Move caravan to adjacent city, handling raider flips and temple visits."
  [state player destination]
  (let [current-city (get-in state [:players player :caravan])
        rk (game/route-key current-city destination)
        ;; Check if this is a river route
        is-river? (some #(and (= :river (:type %))
                              (= rk (game/route-key (:from %) (:to %))))
                        (:routes state))]
    (-> state
        (assoc-in [:players player :caravan] destination)
        (update-in [:players player :travels-this-round] (fnil inc 0))
        (update-in [:players player :total-travels] (fnil inc 0))
        (add-log {:type :travel
                  :message (str "Traveled from " (str/capitalize (name current-city))
                                " to " (str/capitalize (name destination)))
                  :from current-city :to destination})
        (flip-enemy-raiders-on-route player rk)
        (score-own-raider-on-route player rk)
        (visit-temples-on-travel player destination)
        ;; Passive trigger: river crossing (boards 3, 12)
        (cond-> is-river? (game/apply-passive player :river-crossed {:route rk})))))

(defn resolve-travel-choices
  "Travel: move caravan one space. May discard a good to move again."
  [state]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        city (:caravan pdata)
        neighbors (get-in state [:city-graph city])
        turn (:player-turn state)
        traveled? (get-in state [:player-turn :traveled?] false)]
    (if (empty? neighbors)
      {:skip (return-to-choose-action state)}
      (let [extended? (get-in state [:player-turn :extended?] false)
            move-choices
            (into {}
                  (for [dest neighbors]
                    [dest (let [s (travel-to-city state player dest)]
                            (assoc s :player-turn
                                   (cond-> {:phase (if extended?
                                                     :choose-action
                                                     :travel-continue)
                                            :space (:space turn)
                                            :actions-remaining (:actions-remaining turn)
                                            :used-icons (:used-icons turn)
                                            :traveled? true}
                                     (:solo-landing turn) (assoc :solo-landing true))))]))]
        (if traveled?
          ;; Already traveled once, this is the "discard good to travel again" option
          move-choices
          (assoc move-choices :skip (return-to-choose-action state)))))))

(defn travel-continue-choices
  "After first travel step, may discard a good to travel one more space."
  [state]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        resources (:resources pdata)
        has-goods? (some #(pos? (get resources % 0)) game/resource-types)
        turn (:player-turn state)]
    (if has-goods?
      ;; Offer to discard a good for extra movement
      (into {:done (return-to-choose-action state)}
            (for [r game/resource-types
                  :when (pos? (get resources r 0))]
              [r (-> state
                     (spend-resource player r)
                     (add-log {:type :travel-extend
                               :message (str "Discarded " (get game/resource-icons r "")
                                             " " (name r) " to travel again")
                               :resource r})
                     (assoc :player-turn
                            (cond-> {:phase :resolve-travel
                                     :space (:space turn)
                                     :actions-remaining (:actions-remaining turn)
                                     :used-icons (:used-icons turn)
                                     :traveled? true
                                     :extended? true}
                              (:solo-landing turn) (assoc :solo-landing true))))]))
      {:done (return-to-choose-action state)})))

;; =============================================================================
;; Action: Influence
;; =============================================================================

(defn flip-raiders-on-route
  "Flip ALL raiders (any player) on a route to their point side.
   Used when a magistrate passes through."
  [state route-key]
  (reduce-kv
   (fn [s pk pdata]
     (if (= :raiding (get-in pdata [:raiders route-key]))
       (-> s
           (assoc-in [:players pk :raiders route-key] :point)
           (add-log {:type :magistrate-raider-flip
                     :message (str "Magistrate flipped " pk "'s raider on "
                                   (str/capitalize (name (first route-key))) "—"
                                   (str/capitalize (name (second route-key)))
                                   " to point side")
                     :owner pk :route route-key}))
       s))
   state
   (:players state)))

(defn resolve-influence-choices
  "Move a magistrate clockwise along roads. Flip raiders on routes passed through."
  [state]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        leader-level (get-in pdata [:roles :leader] 1)
        max-move (get game/leader-movement leader-level 1)
        active-cities (set (keys (:city-graph state)))
        magistrate-entries (vec (:magistrates state))
        choices
        (into {}
              (for [[mag-id mag-city] magistrate-entries
                    steps (range 1 (inc max-move))
                    :let [;; Trace the full clockwise path
                          path (game/road-clockwise-path mag-city steps active-cities)
                          dest (if (seq path) (second (last path)) nil)
                          rk-key [mag-city dest steps]]
                    :when dest]
                [rk-key (let [;; Count raiders that will be flipped
                              raiders-on-path
                              (count (for [[from to] path
                                           :let [rk (game/route-key from to)]
                                           [pk pdata] (:players state)
                                           :when (= :raiding (get-in pdata [:raiders rk]))]
                                       rk))
                              ;; Update magistrate position (by ID, not city)
                              s (-> state
                                    (assoc-in [:magistrates mag-id] dest)
                                    ;; Track per-magistrate cumulative movement this turn
                                    (update-in [:turn-stats :magistrate-moves mag-id]
                                               (fnil + 0) steps)
                                    ;; Max single-magistrate cumulative move (for G1)
                                    (assoc-in [:turn-stats :magistrate-max-move]
                                              (apply max 0 (vals (update (get-in state [:turn-stats :magistrate-moves] {})
                                                                        mag-id (fnil + 0) steps))))
                                    ;; Track per-magistrate cumulative raiders flipped (for G2)
                                    (update-in [:turn-stats :magistrate-flips mag-id]
                                               (fnil + 0) raiders-on-path)
                                    (assoc-in [:turn-stats :magistrate-raiders-flipped]
                                              (apply max 0 (vals (update (get-in state [:turn-stats :magistrate-flips] {})
                                                                        mag-id (fnil + 0) raiders-on-path))))
                                    (add-log {:type :influence
                                              :message (str "Moved magistrate from "
                                                            (str/capitalize (name mag-city))
                                                            " to " (str/capitalize (name dest))
                                                            " (" steps " space"
                                                            (when (> steps 1) "s") ")"
                                                            " (Leader lv" leader-level ")")
                                              :from mag-city :to dest :steps steps}))]
                          ;; Flip raiders on each route the magistrate passes through
                          (-> (reduce (fn [st [from to]]
                                        (flip-raiders-on-route st (game/route-key from to)))
                                      s
                                      path)
                              ;; Passive trigger: magistrate-moved (board 5 — travel with it)
                              (game/apply-passive player :magistrate-moved
                                                  {:from mag-city :to dest})
                              return-to-choose-action))]))]
    (if (seq choices)
      (assoc choices :skip (return-to-choose-action state))
      {:skip (return-to-choose-action state)})))

;; =============================================================================
;; State machine
;; =============================================================================

(defn find-state-raw
  "Return [phase choices-map] for the current game state."
  [state]
  (if (:game-over state)
    [:game-over {}]
    (let [phase (game/current-phase state)]
      [phase
       (case phase
         :choose-die             (choose-die-choices state)
         :choose-astronomer      (choose-astronomer-choices state)
         :resolve-landing        (resolve-landing-choices state)
         :choose-role-increase   (choose-role-increase-choices state)
         :choose-action          (choose-action-choices state)
         :resolve-take           (resolve-take-choices state)
         :resolve-sell           (resolve-sell-choices state)
         :resolve-temple         (resolve-temple-choices state)
         :resolve-deploy         (resolve-deploy-choices state)
         :resolve-travel         (resolve-travel-choices state)
         :travel-continue        (travel-continue-choices state)
         :resolve-influence      (resolve-influence-choices state)
         :turn-complete          {:done (game/advance-turn state)}
         (do #?(:clj (println "WARNING: Unknown game phase:" phase))
             {}))])))

(defn advance-through-trivial
  "Advance state through single-choice phases that aren't in `protected`.
   Stops when the current phase is protected, has multiple choices, or has
   no choices.  Includes a 200-step safety valve."
  [state protected]
  (loop [s state n 0]
    (if (> n 200) s
      (let [[p cs] (find-state-raw s)]
        (if (and (= 1 (count cs))
                 (not (contains? protected p)))
          (let [ns (first (vals cs))]
            (if ns (recur ns (inc n)) s))
          s)))))

(defn find-state
  "Same as find-state-raw but auto-advances through single-choice auto-resolve phases."
  [state]
  (let [[phase choices] (find-state-raw state)]
    (if (and (= 1 (count choices))
             (contains? #{:resolve-take} phase))
      (let [next-state (first (vals choices))]
        (find-state next-state))
      [phase choices])))
