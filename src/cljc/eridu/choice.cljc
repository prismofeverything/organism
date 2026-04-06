(ns eridu.choice
  (:require
   [eridu.game :as game]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn partial-map [f s]
  (into {} (map (juxt identity f) s)))

(defn add-resource [state player resource n]
  (update-in state [:players player :resources resource] + n))

(defn spend-resource [state player resource]
  (update-in state [:players player :resources resource] dec))

(defn add-amity [state player n]
  (update-in state [:players player :amity] + n))

(defn add-glory [state player n]
  (update-in state [:players player :glory] + n))

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
                      (update-in [:players player :dice-available]
                                 (fn [d] (into (subvec d 0 idx) (subvec d (inc idx)))))
                      (update-in [:players player :dice-used] conj die-val)
                      (assoc :player-turn {:phase :choose-astronomer
                                           :die-value die-val}))])
           dice))))

;; =============================================================================
;; Phase 2: Choose which astronomer to move
;; =============================================================================

(defn choose-astronomer-choices
  "Player picks which of their astronomers to move."
  [state]
  (let [player (game/current-player state)
        astronomers (get-in state [:players player :astronomers])
        die-val (get-in state [:player-turn :die-value])]
    ;; Offer each astronomer (by index) as a choice
    (into {}
          (map-indexed
           (fn [idx current-space]
             (let [dest (game/move-astronomer-clockwise current-space die-val)]
               [idx (-> state
                        (assoc-in [:players player :astronomers idx] dest)
                        (assoc :player-turn {:phase :resolve-landing
                                            :landed-space dest}))]))
           astronomers))))

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
        ;; If space 7, take first-player card
        state (if (= space 7)
                (assoc state :first-player player)
                state)]
    (if (= num-actions 1)
      ;; Alone: may increase a role track (or skip)
      {:increase-role (assoc state :player-turn {:phase :choose-role-increase})
       :skip          (game/advance-turn state)}
      ;; Multiple astronomers: take N actions, can't repeat same icon
      {:begin (assoc state :player-turn
                     {:phase :choose-action
                      :space space
                      :actions-remaining num-actions
                      :used-icons #{}})})))

;; =============================================================================
;; Role increase (when alone on a space)
;; =============================================================================

(defn choose-role-increase-choices
  "Player picks which role to increase (if below max)."
  [state]
  (let [player (game/current-player state)
        role-levels (get-in state [:players player :roles])
        resources (get-in state [:players player :resources])
        increasable
        (for [role game/roles
              :let [current-level (get role-levels role 1)]
              :when (< current-level game/max-role-level)
              :let [next-level (inc current-level)
                    cost (get-in game/role-threshold-costs [role next-level])]
              :when (or (nil? cost) (pos? (get resources cost 0)))]
          [role (cond-> state
                  cost (spend-resource player cost)
                  true (assoc-in [:players player :roles role] next-level)
                  true (game/advance-turn))])]
    (if (seq increasable)
      (into {:skip (game/advance-turn state)} increasable)
      {:skip (game/advance-turn state)})))

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
  (let [{:keys [space actions-remaining used-icons]} (:player-turn state)
        available (available-action-indices space used-icons)]
    (if (or (zero? actions-remaining) (empty? available))
      ;; Done with actions
      {:done (game/advance-turn state)}
      (into {}
            (for [idx available
                  :let [action (nth (:actions (get game/action-spaces space)) idx)]]
              [idx (assoc state :player-turn
                          {:phase (case (:type action)
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
                           :used-icons (conj used-icons idx)})])))))

;; =============================================================================
;; Action: Take Goods
;; =============================================================================

(defn resolve-take-choices [state]
  (let [player (game/current-player state)
        resources (get-in state [:player-turn :action :resources])
        next-state (reduce #(add-resource %1 player %2 1) state resources)
        turn (:player-turn state)]
    {:done (assoc next-state :player-turn
                  {:phase :choose-action
                   :space (:space turn)
                   :actions-remaining (:actions-remaining turn)
                   :used-icons (:used-icons turn)})}))

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
        turn (:player-turn state)
        ;; Find demands the player can satisfy
        sellable (distinct
                  (for [demand demands
                        :when (pos? (get resources demand 0))]
                    demand))]
    (if (seq sellable)
      (into {:skip (assoc state :player-turn
                          {:phase :choose-action
                           :space (:space turn)
                           :actions-remaining (:actions-remaining turn)
                           :used-icons (:used-icons turn)})}
            (for [demand sellable]
              [demand (let [;; Remove demand from city
                            new-demands (let [idx (.indexOf demands demand)]
                                          (into (subvec (vec demands) 0 idx)
                                                (subvec (vec demands) (inc idx))))
                            s (-> state
                                  (spend-resource player demand)
                                  (assoc-in [:city-demands city] new-demands)
                                  (update-in [:players player :demand-tokens] conj demand)
                                  (add-amity player amity-score))]
                        (cond-> s
                          (pos? glory-bonus) (add-glory player glory-bonus)
                          true (assoc :player-turn
                                      {:phase :choose-action
                                       :space (:space turn)
                                       :actions-remaining (:actions-remaining turn)
                                       :used-icons (:used-icons turn)})))]))
      ;; Nothing to sell
      {:skip (assoc state :player-turn
                    {:phase :choose-action
                     :space (:space turn)
                     :actions-remaining (:actions-remaining turn)
                     :used-icons (:used-icons turn)})})))

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
        supply (:temples-supply pdata)
        turn (:player-turn state)
        return-to-actions (fn [s]
                            (assoc s :player-turn
                                   {:phase :choose-action
                                    :space (:space turn)
                                    :actions-remaining (:actions-remaining turn)
                                    :used-icons (:used-icons turn)}))]
    (if (and (pos? supply) (< placed max-temples))
      (let [caravan-city (:caravan pdata)
            all-magistrate-cities (vals (:magistrates state))
            valid-cities (distinct
                          (for [city (cons caravan-city all-magistrate-cities)
                                :when (and city
                                           (not (contains? (:temples pdata) city)))]
                            city))]
        (if (seq valid-cities)
          (into {:skip (return-to-actions state)}
                (for [city valid-cities]
                  [city (-> state
                            (assoc-in [:players player :temples city] :face-up)
                            (update-in [:players player :temples-supply] dec)
                            return-to-actions)]))
          {:skip (return-to-actions state)}))
      {:skip (return-to-actions state)})))

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
        deploys-left (get-in state [:player-turn :deploys-left] 2)
        return-to-actions (fn [s]
                            (assoc s :player-turn
                                   {:phase :choose-action
                                    :space (:space turn)
                                    :actions-remaining (:actions-remaining turn)
                                    :used-icons (:used-icons turn)}))]
    (if (zero? deploys-left)
      {:done (return-to-actions state)}
      (let [;; Routes where player can place a raider (doesn't already have one there)
            placeable (for [route adjacent-routes
                           :let [rk (game/route-key (:from route) (:to route))]
                           :when (not (contains? (:raiders pdata) rk))]
                        rk)
            can-place? (and (pos? supply)
                            (< current-deployed max-deployed)
                            (seq placeable))]
        (if can-place?
          (into {:skip (return-to-actions state)}
                (for [rk placeable]
                  [rk (-> state
                          (assoc-in [:players player :raiders rk] :raiding)
                          (update-in [:players player :raiders-supply] dec)
                          (assoc :player-turn
                                 {:phase :resolve-deploy
                                  :space (:space turn)
                                  :action (get-in state [:player-turn :action])
                                  :action-index (get-in state [:player-turn :action-index])
                                  :actions-remaining (:actions-remaining turn)
                                  :used-icons (:used-icons turn)
                                  :deploys-left (dec deploys-left)}))]))
          {:done (return-to-actions state)})))))

;; =============================================================================
;; Action: Travel
;; =============================================================================

(defn visit-temples-on-travel
  "When caravan enters a city with a face-up temple, flip it and score amity."
  [state player city]
  (let [pdata (game/player-data state player)
        temple-state (get-in pdata [:temples city])]
    (if (= temple-state :face-up)
      (let [;; Flip to face-down
            state (assoc-in state [:players player :temples city] :face-down)
            face-down-count (inc (game/count-face-down-temples pdata))
            ;; Score amity = number of face-down temples
            state (add-amity state player face-down-count)
            ;; Magistrate bonus
            leader-level (get-in state [:players player :roles :leader] 1)
            has-magistrate? (game/magistrate-in-city? state city)
            glory-bonus (if has-magistrate? (get game/leader-bonus leader-level 0) 0)]
        (cond-> state
          (pos? glory-bonus) (add-glory player glory-bonus)))
      state)))

(defn flip-enemy-raiders-on-route
  "When caravan travels a route, flip any opposing raiders to :point side."
  [state player route-key]
  (reduce-kv
   (fn [s pk pdata]
     (if (and (not= pk player)
              (= :raiding (get-in pdata [:raiders route-key])))
       (assoc-in s [:players pk :raiders route-key] :point)
       s))
   state
   (:players state)))

(defn score-own-raider-on-route
  "When caravan travels a route with own :point raider, remove and score 4 glory."
  [state player route-key]
  (let [raider-state (get-in state [:players player :raiders route-key])]
    (if (= raider-state :point)
      (-> state
          (update-in [:players player :raiders] dissoc route-key)
          (update-in [:players player :raiders-supply] inc)
          (add-glory player 4))
      state)))

(defn travel-to-city
  "Move caravan to adjacent city, handling raider flips and temple visits."
  [state player destination]
  (let [current-city (get-in state [:players player :caravan])
        rk (game/route-key current-city destination)]
    (-> state
        (assoc-in [:players player :caravan] destination)
        (flip-enemy-raiders-on-route player rk)
        (score-own-raider-on-route player rk)
        (visit-temples-on-travel player destination))))

(defn resolve-travel-choices
  "Travel: move caravan one space. May discard a good to move again."
  [state]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        city (:caravan pdata)
        neighbors (get-in state [:city-graph city])
        turn (:player-turn state)
        traveled? (get-in state [:player-turn :traveled?] false)
        return-to-actions (fn [s]
                            (assoc s :player-turn
                                   {:phase :choose-action
                                    :space (:space turn)
                                    :actions-remaining (:actions-remaining turn)
                                    :used-icons (:used-icons turn)}))]
    (if (empty? neighbors)
      {:skip (return-to-actions state)}
      (let [move-choices
            (into {}
                  (for [dest neighbors]
                    [dest (let [s (travel-to-city state player dest)]
                            (assoc s :player-turn
                                   {:phase :travel-continue
                                    :space (:space turn)
                                    :actions-remaining (:actions-remaining turn)
                                    :used-icons (:used-icons turn)
                                    :traveled? true}))]))]
        (if traveled?
          ;; Already traveled once, this is the "discard good to travel again" option
          move-choices
          (assoc move-choices :skip (return-to-actions state)))))))

(defn travel-continue-choices
  "After first travel step, may discard a good to travel one more space."
  [state]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        resources (:resources pdata)
        has-goods? (some #(pos? (get resources % 0)) game/resource-types)
        turn (:player-turn state)
        return-to-actions (fn [s]
                            (assoc s :player-turn
                                   {:phase :choose-action
                                    :space (:space turn)
                                    :actions-remaining (:actions-remaining turn)
                                    :used-icons (:used-icons turn)}))]
    (if has-goods?
      ;; Offer to discard a good for extra movement
      (into {:done (return-to-actions state)}
            (for [r game/resource-types
                  :when (pos? (get resources r 0))]
              [r (-> state
                     (spend-resource player r)
                     (assoc :player-turn
                            {:phase :resolve-travel
                             :space (:space turn)
                             :actions-remaining (:actions-remaining turn)
                             :used-icons (:used-icons turn)
                             :traveled? true}))]))
      {:done (return-to-actions state)})))

;; =============================================================================
;; Action: Influence
;; =============================================================================

(defn resolve-influence-choices
  "Move a magistrate clockwise along roads."
  [state]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        leader-level (get-in pdata [:roles :leader] 1)
        max-move (get game/leader-movement leader-level 1)
        active-cities (set (keys (:city-graph state)))
        turn (:player-turn state)
        return-to-actions (fn [s]
                            (assoc s :player-turn
                                   {:phase :choose-action
                                    :space (:space turn)
                                    :actions-remaining (:actions-remaining turn)
                                    :used-icons (:used-icons turn)}))
        magistrate-entries (vec (:magistrates state))
          choices
          (into {}
                (for [[mag-city _] magistrate-entries
                      steps (range 1 (inc max-move))
                      :let [dest (reduce (fn [c _]
                                           (game/road-clockwise-next c active-cities))
                                         mag-city
                                         (range steps))
                            rk-key [mag-city dest steps]]
                      :when dest]
                  [rk-key (-> state
                              (assoc :magistrates
                                     (-> (:magistrates state)
                                         (dissoc mag-city)
                                         (assoc dest :neutral)))
                              return-to-actions)]))]
      (if (seq choices)
        (assoc choices :skip (return-to-actions state))
        {:skip (return-to-actions state)})))

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
         {})])))

(defn find-state
  "Same as find-state-raw but auto-advances through single-choice auto-resolve phases."
  [state]
  (let [[phase choices] (find-state-raw state)]
    (if (and (= 1 (count choices))
             (contains? #{:resolve-take} phase))
      (let [next-state (first (vals choices))]
        (find-state next-state))
      [phase choices])))
