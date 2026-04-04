(ns eridu.choice
  (:require
   [eridu.game :as game]))

;; Each choice function returns a map of {choice-key -> next-state}.
;; find-state returns [phase choices-map] for the current position in the turn.

(defn partial-map
  "Build a choices map by applying f to each option in s."
  [f s]
  (into {} (map (juxt identity f) s)))

;; =============================================================================
;; Action helpers
;; =============================================================================

(defn take-resources
  "Give the current player the specified resources."
  [state resources]
  (let [player (game/current-player state)]
    (reduce
     (fn [s r]
       (update-in s [:players player :resources r] (fnil inc 0)))
     state
     resources)))

(defn spend-resource
  "Remove one unit of a resource from the current player."
  [state resource]
  (let [player (game/current-player state)]
    (update-in state [:players player :resources resource] (fnil dec 0))))

(defn add-amity [state n]
  (let [player (game/current-player state)]
    (update-in state [:players player :amity] + n)))

(defn add-glory [state n]
  (let [player (game/current-player state)]
    (update-in state [:players player :glory] + n)))

(defn move-caravan
  "Move the current player's caravan to a destination city."
  [state destination]
  (let [player (game/current-player state)]
    (assoc-in state [:players player :caravan] destination)))

(defn deploy-raider
  "Deploy a raider from the current player's supply to a city."
  [state city]
  (let [player (game/current-player state)]
    (-> state
        (update-in [:players player :raiders-remaining] dec)
        (update-in [:players player :raiders city] (fnil inc 0)))))

(defn build-temple
  "Build a temple from the current player's supply in a city."
  [state city]
  (let [player (game/current-player state)]
    (-> state
        (update-in [:players player :temples-remaining] dec)
        (update-in [:players player :temples city] (fnil inc 0)))))

(defn increase-role
  "Increase a role track by 1 level (capped at max)."
  [state role]
  (let [player (game/current-player state)]
    (update-in state [:players player :roles role]
               #(min game/max-role-level (inc %)))))

;; =============================================================================
;; Phase choices
;; =============================================================================

(defn choose-action-choices
  "Player picks which astrology space action to activate.
   Choices are the astronomer positions available to the current player."
  [state]
  (let [player (game/current-player state)
        astronomer-spaces (get-in state [:players player :astronomers])]
    (partial-map
     (fn [space]
       (assoc state :player-turn
              {:phase :choose-space-action
               :space space
               :actions (get-in state [:astrology space :actions])}))
     (distinct astronomer-spaces))))

(defn choose-space-action-choices
  "Player picks one of the 4 actions on their chosen astrology space."
  [state]
  (let [actions (get-in state [:player-turn :actions])]
    (into {}
          (map-indexed
           (fn [idx action]
             [idx (assoc state :player-turn
                         {:phase (case (:type action)
                                  :take    :resolve-take
                                  :sell    :resolve-sell
                                  :deploy  :choose-deploy-city
                                  :travel  :choose-travel-destination
                                  :build   :choose-build-city
                                  :influence :choose-influence-role
                                  :excel   :resolve-excel
                                  :temple  :choose-temple-city)
                          :action action})])
           actions))))

;; --- Take ---

(defn resolve-take-choices
  "Auto-resolve: give the player the resources and end turn."
  [state]
  (let [resources (get-in state [:player-turn :action :resources])
        next-state (-> state
                       (take-resources resources)
                       game/advance-turn)]
    {:done next-state}))

;; --- Sell ---

(defn resolve-sell-choices
  "Player chooses a resource to sell (must have > 0)."
  [state]
  (let [player (game/current-player state)
        resources (get-in state [:players player :resources])
        sellable (filter #(pos? (get resources % 0)) game/resource-types)]
    (if (seq sellable)
      (partial-map
       (fn [r]
         (-> state
             (spend-resource r)
             (add-glory 1)
             game/advance-turn))
       sellable)
      {:skip (game/advance-turn state)})))

;; --- Deploy ---

(defn choose-deploy-city-choices
  "Player chooses a city to deploy a raider to."
  [state]
  (let [player (game/current-player state)
        remaining (get-in state [:players player :raiders-remaining] 0)
        cities (keys (:city-graph state))]
    (if (pos? remaining)
      (partial-map
       (fn [city]
         (-> state
             (deploy-raider city)
             game/advance-turn))
       cities)
      {:skip (game/advance-turn state)})))

;; --- Travel ---

(defn choose-travel-destination-choices
  "Player moves their caravan to an adjacent city."
  [state]
  (let [player (game/current-player state)
        current-city (get-in state [:players player :caravan])
        neighbors (get-in state [:city-graph current-city])]
    (if (seq neighbors)
      (partial-map
       (fn [city]
         (-> state
             (move-caravan city)
             game/advance-turn))
       neighbors)
      {:skip (game/advance-turn state)})))

;; --- Build ---

(defn choose-build-city-choices
  "Player chooses a city to build a temple in."
  [state]
  (let [player (game/current-player state)
        remaining (get-in state [:players player :temples-remaining] 0)
        caravan-city (get-in state [:players player :caravan])]
    (if (pos? remaining)
      ;; Can build in the city where caravan is
      {caravan-city (-> state
                        (build-temple caravan-city)
                        game/advance-turn)
       :skip (game/advance-turn state)}
      {:skip (game/advance-turn state)})))

;; --- Influence ---

(defn choose-influence-role-choices
  "Player chooses a role to increase by 1 level."
  [state]
  (let [player (game/current-player state)
        role-levels (get-in state [:players player :roles])]
    (partial-map
     (fn [role]
       (-> state
           (increase-role role)
           game/advance-turn))
     (filter #(< (get role-levels % 0) game/max-role-level) game/roles))))

;; --- Excel ---

(defn resolve-excel-choices
  "Excel: gain 1 amity and 1 glory."
  [state]
  (let [next-state (-> state
                       (add-amity 1)
                       (add-glory 1)
                       game/advance-turn)]
    {:done next-state}))

;; --- Temple ---

(defn choose-temple-city-choices
  "Player chooses a city to build a temple in (temple action)."
  [state]
  (let [player (game/current-player state)
        remaining (get-in state [:players player :temples-remaining] 0)
        cities (keys (:city-graph state))]
    (if (pos? remaining)
      (partial-map
       (fn [city]
         (-> state
             (build-temple city)
             game/advance-turn))
       cities)
      {:skip (game/advance-turn state)})))

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
         :choose-action              (choose-action-choices state)
         :choose-space-action        (choose-space-action-choices state)
         :resolve-take               (resolve-take-choices state)
         :resolve-sell               (resolve-sell-choices state)
         :choose-deploy-city         (choose-deploy-city-choices state)
         :choose-travel-destination  (choose-travel-destination-choices state)
         :choose-build-city          (choose-build-city-choices state)
         :choose-influence-role      (choose-influence-role-choices state)
         :resolve-excel              (resolve-excel-choices state)
         :choose-temple-city         (choose-temple-city-choices state)
         {})])))

(defn find-state
  "Same as find-state-raw but auto-advances through single-choice non-interactive phases."
  [state]
  (let [[phase choices] (find-state-raw state)]
    (if (and (= 1 (count choices))
             (contains? #{:resolve-take :resolve-excel} phase))
      (let [next-state (first (vals choices))]
        (find-state next-state))
      [phase choices])))
