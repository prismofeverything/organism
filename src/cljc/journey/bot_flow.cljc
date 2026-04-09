(ns journey.bot-flow
  "Flowchart-based bot for journey.

   A bot is a map of named diagrams. Each diagram is a graph of tiles
   (conditions, logic gates, effects, jumps) connected by typed ports.

   Execution model:
   - The bot is asked to choose for the current game phase.
   - Start from the diagram named in :start-diagram (default :main).
   - Walk from the diagram's :start-tile, evaluating conditions to pick
     which output port to follow.
   - Terminate at an :effect tile, which converts intent into a concrete
     choice for the current phase. If the effect cannot apply (wrong phase
     etc.) the interpreter falls back to picking the first available choice.
   - :jump tiles transfer execution to another diagram (possibly itself).
     A visited set prevents infinite loops.

   The vocabulary (`conditions`, `logic-tiles`, `effects`) is data so the
   editor can introspect available tiles and their parameter shapes."
  (:require
   [journey.choice :as choice]
   [journey.game :as game]))

;; ── Helpers shared across condition/effect predicates ───────────────────────

(defn- compare-op [op a b]
  (case op
    :<  (< a b)
    :<= (<= a b)
    :=  (= a b)
    :>= (>= a b)
    :>  (> a b)
    false))

(defn- on-board-count
  "Total sundivers on board for player."
  [state player]
  (apply + (map #(get-in state [:board % :sundivers player] 0)
                (keys (:board state)))))

(defn- player-totals [state]
  (let [p   (game/current-player state)
        on  (on-board-count state p)
        hab (get-in state [:players p :habitat :sundivers] 0)
        res (get-in state [:players p :reserve :sundivers] 0)]
    {:on-board on :habitat hab :reserve res
     :total (+ on hab res)}))

(defn- has-conversion? [state ttype]
  (let [convs (game/find-conversions state (game/current-player state))]
    (if (= ttype :any)
      (boolean (seq convs))
      (boolean (some #(= ttype (:type %)) convs)))))

(defn- has-stations? [state ttype]
  (let [stations (get-in state [:players (game/current-player state) :stations])]
    (if (= ttype :any)
      (boolean (seq stations))
      (boolean (some #(= ttype (:type %)) stations)))))

(defn- beacon-positions [state]
  (filter #(get-in state [:board % :beacon]) (keys (:board state))))

(defn- has-beacons? [state]
  (boolean (seq (beacon-positions state))))

(defn- has-landings? [state]
  (boolean (seq (game/available-landings state))))

(defn- near-landable-positions [state]
  (filter (fn [pos]
            (let [tile-color (get-in state [:board pos :color])]
              (and tile-color
                   (game/cipher-color-active? state [0 0] tile-color)
                   (>= (game/count-cipher-matches state pos) 4))))
          (keys (:board state))))

(defn- pick-target-positions
  "Resolve a target keyword to a seq of board positions."
  [state target]
  (case target
    :landings      (game/available-landings state)
    :beacons       (beacon-positions state)
    :near-landable (near-landable-positions state)
    :any           (keys (:board state))
    nil))

;; ── Vocabulary: conditions ──────────────────────────────────────────────────
;; Each condition has:
;;   :label       — display name in editor
;;   :description — tooltip
;;   :params      — [{:key :type :default :options}], used by editor for inputs
;;   :outputs     — port keys, defaults to [:true :false]
;;   :eval        — (fn [state params] → boolean)

(def conditions
  {:sundivers-low?
   {:label "sundivers low"
    :description "Total sundivers (board + habitat + reserve) compared to threshold"
    :params [{:key :compare   :type :enum :default :<= :options [:< :<= := :>= :>]}
             {:key :threshold :type :int  :default 4}]
    :outputs [:true :false]
    :eval (fn [state {:keys [compare threshold]}]
            (compare-op (or compare :<=) (:total (player-totals state)) (or threshold 4)))}

   :on-board-count?
   {:label "sundivers on board"
    :description "Count of sundivers currently on board for the current player"
    :params [{:key :compare   :type :enum :default :>= :options [:< :<= := :>= :>]}
             {:key :threshold :type :int  :default 3}]
    :outputs [:true :false]
    :eval (fn [state {:keys [compare threshold]}]
            (compare-op (or compare :>=)
                        (on-board-count state (game/current-player state))
                        (or threshold 3)))}

   :reserve-count?
   {:label "in reserve"
    :description "Compare a reserve resource to a threshold"
    :params [{:key :resource  :type :enum :default :sundivers
              :options [:sundivers :towers :foundries :matrixes]}
             {:key :compare   :type :enum :default :>= :options [:< :<= := :>= :>]}
             {:key :threshold :type :int  :default 1}]
    :outputs [:true :false]
    :eval (fn [state {:keys [resource compare threshold]}]
            (let [v (get-in state [:players (game/current-player state)
                                   :reserve (or resource :sundivers)] 0)]
              (compare-op (or compare :>=) v (or threshold 1))))}

   :phase-is?
   {:label "phase is"
    :description "Current phase matches the configured phase"
    :params [{:key :phase :type :enum :default :choose-action-type
              :options [:choose-action-type :choose-move :choose-convert
                        :choose-activate :choose-launch-destination
                        :choose-fly-from :choose-fly-to
                        :choose-activate-station :choose-activate-matrix-beacon
                        :choose-activate-tower-heading
                        :cipher :choose-land
                        :choose-ark-advance :choose-captain-drift
                        :keep-card]}]
    :outputs [:true :false]
    :eval (fn [state {:keys [phase]}]
            (= phase (game/current-phase state)))}

   :has-conversion?
   {:label "can convert"
    :description "A conversion of the chosen type is available right now"
    :params [{:key :type :type :enum :default :any
              :options [:any :tower :foundry :matrix]}]
    :outputs [:true :false]
    :eval (fn [state {:keys [type]}] (has-conversion? state (or type :any)))}

   :has-stations?
   {:label "owns station"
    :description "Current player owns at least one station of the chosen type"
    :params [{:key :type :type :enum :default :any
              :options [:any :tower :foundry :matrix]}]
    :outputs [:true :false]
    :eval (fn [state {:keys [type]}] (has-stations? state (or type :any)))}

   :has-beacons?
   {:label "any beacons"
    :description "True when at least one beacon is placed on the board"
    :params []
    :outputs [:true :false]
    :eval (fn [state _] (has-beacons? state))}

   :has-landings?
   {:label "landing available"
    :description "True when at least one tile satisfies a landing condition"
    :params []
    :outputs [:true :false]
    :eval (fn [state _] (has-landings? state))}

   :is-captain?
   {:label "is captain"
    :description "Current player holds the captain flame"
    :params []
    :outputs [:true :false]
    :eval (fn [state _]
            (= (game/current-player state) (:captain-flame state)))}

   :random?
   {:label "random"
    :description "Take the true branch with the given probability"
    :params [{:key :probability :type :float :default 0.5}]
    :outputs [:true :false]
    :eval (fn [_state {:keys [probability]}]
            (< (rand) (or probability 0.5)))}})

;; ── Vocabulary: logic gates ─────────────────────────────────────────────────
;; Logic tiles aggregate signals. They have one output port :out and a single
;; "input pile" — links pointing at them are aggregated. The interpreter does
;; not actually evaluate them as boolean nodes — instead the editor presents
;; them as a way to fan in/out execution. We treat them as straight passthrough
;; with renamed ports for clarity.

(def logic-tiles
  {:any
   {:label "any"
    :description "Combine multiple paths into one (fan-in)"
    :params []
    :inputs [:in]
    :outputs [:out]}
   :branch
   {:label "branch"
    :description "Split execution into two parallel paths (tries first, then second)"
    :params []
    :inputs [:in]
    :outputs [:a :b]}})

;; ── Vocabulary: effects ─────────────────────────────────────────────────────
;; Each effect has :apply (fn [state choices params] → [choice-key next-state] or nil).
;; If it returns nil the interpreter falls back to its caller chain.

(defn- get-by-pred
  "Return the first [k v] entry in choices satisfying pred on the key."
  [choices pred]
  (some (fn [[k v]] (when (pred k) [k v])) choices))

(defn- pick-keyword* [choices kw]
  (when (contains? choices kw)
    [kw (get choices kw)]))

(defn- station-pref-conv [choices prefs]
  (some (fn [t]
          (get-by-pred choices
                       (fn [k] (and (map? k) (= t (:type k))))))
        prefs))

(defn- closest-pos
  "Among the position-keyed entries in choices, return [k v] for the position
   nearest to any of the targets. If no targets or no positions, returns nil."
  [choices targets]
  (when (seq targets)
    (let [pos-entries (filter (fn [[k _]]
                                (and (vector? k) (= 2 (count k)) (number? (first k))))
                              choices)]
      (when (seq pos-entries)
        (apply min-key
               (fn [[k _]]
                 (apply min (map #(game/hex-distance k %) targets)))
               pos-entries)))))

(def effects
  {:pick-action
   {:label "pick action"
    :description "Choose the named action in :choose-action-type"
    :params [{:key :action :type :enum :default :move
              :options [:move :convert :activate :pass]}]
    :inputs [:in]
    :outputs []
    :apply (fn [_state choices {:keys [action]}]
             (pick-keyword* choices (or action :move)))}

   :pick-convert
   {:label "pick convert"
    :description "Choose a station type during :choose-convert"
    :params [{:key :prefer-1 :type :enum :default :tower
              :options [:tower :matrix :foundry]}
             {:key :prefer-2 :type :enum :default :matrix
              :options [:tower :matrix :foundry]}
             {:key :prefer-3 :type :enum :default :foundry
              :options [:tower :matrix :foundry]}]
    :inputs [:in]
    :outputs []
    :apply (fn [_state choices {:keys [prefer-1 prefer-2 prefer-3]}]
             (let [prefs (distinct (filter some? [prefer-1 prefer-2 prefer-3
                                                  :tower :matrix :foundry]))]
               (station-pref-conv choices prefs)))}

   :pick-launch
   {:label "pick launch"
    :description "Choose where to launch a sundiver"
    :params [{:key :target :type :enum :default :beacons
              :options [:landings :beacons :near-landable :any]}]
    :inputs [:in]
    :outputs []
    :apply (fn [state choices {:keys [target]}]
             (let [non-wrap (into {} (remove #(and (vector? (key %))
                                                   (= :wrap (first (key %))))
                                             choices))
                   targets  (pick-target-positions state (or target :beacons))]
               (or (closest-pos non-wrap targets)
                   (first (seq non-wrap)))))}

   :pick-fly-from
   {:label "pick fly source"
    :description "Choose which sundiver to fly (the position with the most)"
    :params []
    :inputs [:in]
    :outputs []
    :apply (fn [state choices _]
             (let [player (game/current-player state)]
               (when (seq choices)
                 (let [k (apply max-key
                                #(get-in state [:board % :sundivers player] 0)
                                (keys choices))]
                   [k (get choices k)]))))}

   :pick-fly-to
   {:label "pick fly dest"
    :description "Choose where to fly toward — landings, beacons, or near-landable"
    :params [{:key :target :type :enum :default :beacons
              :options [:landings :beacons :near-landable :any]}]
    :inputs [:in]
    :outputs []
    :apply (fn [state choices {:keys [target]}]
             (let [non-wrap (into {} (remove #(and (vector? (key %))
                                                   (= :wrap (first (key %))))
                                             choices))
                   targets  (pick-target-positions state (or target :beacons))]
               (or (closest-pos non-wrap targets)
                   (first (seq non-wrap)))))}

   :pick-named
   {:label "pick named"
    :description "Pick a named choice such as :done, :skip, :join, :continue, :land"
    :params [{:key :choice :type :enum :default :done
              :options [:done :skip :join :continue :land :direct :wrap :draw :keep-held]}]
    :inputs [:in]
    :outputs []
    :apply (fn [_state choices {:keys [choice]}]
             (pick-keyword* choices (or choice :done)))}

   :take-max-bonus
   {:label "max bonus"
    :description "Take the maximum bonus actions for activate-self/owner phases"
    :params []
    :inputs [:in]
    :outputs []
    :apply (fn [_state choices _]
             (let [int-keys (filter integer? (keys choices))]
               (when (seq int-keys)
                 (let [k (apply max int-keys)]
                   [k (get choices k)]))))}

   :pick-first
   {:label "pick first"
    :description "Always picks the first available choice — a safe fallback"
    :params []
    :inputs [:in]
    :outputs []
    :apply (fn [_state choices _]
             (when (seq choices)
               [(first (keys choices)) (first (vals choices))]))}

   :jump
   {:label "jump to diagram"
    :description "Transfer execution to another diagram in this bot"
    :params [{:key :diagram :type :diagram-ref :default nil}]
    :inputs [:in]
    :outputs []
    ;; :apply is special — handled by interpreter (sees :kind :jump or :type :jump)
    :apply nil}})

;; ── Spec lookups (used by editor and interpreter) ──────────────────────────

(defn tile-spec
  "Return the spec map for a tile, or nil. Looks up by tile :kind and :type."
  [tile]
  (case (:kind tile)
    :condition (get conditions (:type tile))
    :logic     (get logic-tiles (:type tile))
    :effect    (get effects (:type tile))
    :jump      (get effects :jump)
    :start     {:label "start" :outputs [:out]}
    nil))

(defn all-categories
  "Return categories of available tiles for the editor palette."
  []
  [{:category :conditions
    :tiles    (for [[k v] conditions]
                {:kind :condition :type k :label (:label v)
                 :description (:description v)})}
   {:category :logic
    :tiles    (for [[k v] logic-tiles]
                {:kind :logic :type k :label (:label v)
                 :description (:description v)})}
   {:category :effects
    :tiles    (for [[k v] effects
                    :when (not= k :jump)]
                {:kind :effect :type k :label (:label v)
                 :description (:description v)})}
   {:category :flow
    :tiles    [{:kind :jump :type :jump :label "jump"
                :description "Jump to another diagram"}]}])

;; ── Interpreter ─────────────────────────────────────────────────────────────

(declare eval-tile)

(defn- find-link
  "Find the link from (from-id, port) within a diagram."
  [diagram from-id port]
  (some (fn [l]
          (when (and (= (get-in l [:from :tile]) from-id)
                     (= (get-in l [:from :port]) port))
            l))
        (:links diagram)))

(defn- follow-port
  [state choices definition diagram-name from-id port visited]
  (let [diagram (get-in definition [:diagrams diagram-name])
        link    (find-link diagram from-id port)]
    (when link
      (eval-tile state choices definition diagram-name
                 (get-in link [:to :tile]) visited))))

(defn- run-diagram
  [state choices definition diagram-name visited]
  (when-let [diagram (get-in definition [:diagrams diagram-name])]
    (when-let [start (or (:start-tile diagram)
                         (some (fn [[id t]] (when (= :start (:kind t)) id))
                               (:tiles diagram)))]
      (eval-tile state choices definition diagram-name start visited))))

(defn- eval-tile
  [state choices definition diagram-name tile-id visited]
  (let [token [diagram-name tile-id]]
    (when-not (contains? visited token)
      (let [visited' (conj visited token)
            tile     (get-in definition [:diagrams diagram-name :tiles tile-id])
            kind     (:kind tile)]
        (case kind
          :start
          (follow-port state choices definition diagram-name tile-id :out visited')

          :condition
          (let [spec (get conditions (:type tile))
                v    (when spec ((:eval spec) state (:params tile)))
                port (if v :true :false)]
            (or (follow-port state choices definition diagram-name tile-id port visited')
                ;; If the matched branch dead-ends, try the other branch
                (follow-port state choices definition diagram-name tile-id
                             (if (= port :true) :false :true) visited')))

          :logic
          (case (:type tile)
            :branch (or (follow-port state choices definition diagram-name tile-id :a visited')
                        (follow-port state choices definition diagram-name tile-id :b visited'))
            ;; default: forward through :out
            (follow-port state choices definition diagram-name tile-id :out visited'))

          :effect
          (when-let [spec (get effects (:type tile))]
            (when-let [f (:apply spec)]
              (f state choices (:params tile))))

          :jump
          (when-let [target (get-in tile [:params :diagram])]
            (run-diagram state choices definition target visited'))

          nil)))))

(defn agent-step
  "Top-level: given a bot definition and a game state, return [choice-key next-state]
   for the current phase, or nil. The interpreter falls back to picking the first
   available choice if the flowchart cannot resolve."
  [definition state]
  (let [[_phase choices] (choice/find-state-raw state)]
    (when (seq choices)
      (or (run-diagram state choices definition
                       (or (:start-diagram definition) :main) #{})
          ;; Final fallback: first available choice
          [(first (keys choices)) (first (vals choices))]))))

;; ── Default bot definition ──────────────────────────────────────────────────

(def default-bot
  "An empty bot template — one diagram named :main with a start tile and a
   single fallback :pick-first effect."
  {:start-diagram :main
   :diagrams
   {:main
    {:name        "main"
     :color       "#1e3a5a"
     :collapsed?  false
     :region      {:x 80 :y 80 :w 360 :h 220}
     :start-tile  :start
     :tiles {:start    {:id :start    :kind :start  :type :start
                        :pos [40 80] :params {}}
             :fallback {:id :fallback :kind :effect :type :pick-first
                        :pos [220 80] :params {}}}
     :links [{:from {:tile :start    :port :out}
              :to   {:tile :fallback :port :in}}]}}})
