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
    (cond-> {:done (assoc-in state [:player-turn :phase] :choose-action-type)}
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

;; --- activate sub-choices (to be filled in) ---

(defn choose-activate-choices
  [_state]
  {})

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
      :choose-activate           [:choose-activate           (choose-activate-choices state)]
      [phase {}])))
