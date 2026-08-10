(ns future.game
  "State, phase machine, and legal-actions for FUTURE.

   Public surface (consumed by future-ws and future/play.cljs):

     (create-game players)      → initial state
     (legal-actions state)      → {choice-key → next-state}
     (current-player state)     → PK (turn-holder / flame) | nil

   State shape follows §3 of the supplement:

     :board             topology
     :players           {PK → per-player-data}
     :turn-order        [PK …]
     :flame             PK           whose turn it is (turn-holder)
     :phase             phase-keyword
     :phase-data        {}           transient per-phase working state
     :turn              int
     :deck              [card …]     top of deck = last element
     :hands             {PK → [card …]} drawn this turn, unresolved
     :discard           [card …]
     :flares-drawn      int
     :market-resources  {color → 0..5}
     :market-cities     {color → 0..4}
     :energy-pool       int
     :planets           {color → sid}
     :sundivers         {sid → [{:owner :resource :on-planet?} …]}
     :resources         {sid → [color …]} free-standing resource tokens
     :cities            {sid → {:owner :color}}
     :links             #{{:a :b :owner} …}         color derived from src
     :solar-network     {wedge-idx → {:active {PK → int}
                                      :exhausted {PK → int}}}
     :winner            nil | :salvation | {:result :win :winner :scores}
                                          | {:result :none :scores}

   Every phase transition writes a new :phase keyword. Sub-phases
   (link-placement, bonus decisions) capture the outer phase's state in
   :phase-data :parent-activation so they can pop cleanly."
  (:require [future.board :as board]))

;; ── Constants (from §3.3) ─────────────────────────────────────────────────

(def starting-components-per-player 8)
(def starting-sundivers-per-player  13)
(def starting-links-per-player      13)
(def starting-platforms-per-player  5)
(def starting-habitat-sundivers     8)
(def starting-reserve-sundivers     5)
(def starting-energy-per-player     5)
(def initial-energy-pool            89)

(def initial-market-resources
  {:silver 1 :green 2 :blue 3 :purple 4 :void 5})

(def market-resource-cap 5)
(def cities-per-color    4)

(def planet-advance-rate
  {:silver 1 :green 1 :blue 1 :purple 2 :void 3})

(def card-suits [:silver :green :blue :purple :void :flare])
(def cards-per-suit 13)
(def flares-to-end 13)

(def movement-points 5)

(def resource-price-by-stock
  "Cost in energy to buy a resource given how many are in the market."
  {5 1, 4 1, 3 2, 2 3, 1 5, 0 nil})

(def city-level-actions
  {1 {:base 1 :bonus 1}
   2 {:base 2 :bonus 1}
   3 {:base 3 :bonus 2}})

(def city-level-cards
  "Cards drawn per city activation, by level."
  {1 1, 2 2, 3 3})

;; ── Cards & deck ──────────────────────────────────────────────────────────

(defn make-deck
  "Return a shuffled deck of 78 cards (6 suits × 13 values)."
  []
  (vec
    (shuffle
      (for [suit card-suits value (range 1 (inc cards-per-suit))]
        {:suit suit :value value}))))

(defn flare-card? [c] (= :flare (:suit c)))

;; ── Solar network helpers ─────────────────────────────────────────────────

(defn empty-wedge [] {:active {} :exhausted {}})

(defn empty-solar-network []
  (into {} (for [k (range board/num-wedges)] [k (empty-wedge)])))

(defn active-count
  "Active components in wedge for player."
  [solar wedge-idx player]
  (get-in solar [wedge-idx :active player] 0))

(defn exhausted-count
  "Exhausted components in wedge for player."
  [solar wedge-idx player]
  (get-in solar [wedge-idx :exhausted player] 0))

(defn total-in-wedge
  "Active + exhausted counts (all players) in the wedge."
  [solar wedge-idx]
  (+ (apply + (vals (get-in solar [wedge-idx :active] {})))
     (apply + (vals (get-in solar [wedge-idx :exhausted] {})))))

(defn add-component
  "Add one component to the active section of a wedge."
  [solar wedge-idx player]
  (update-in solar [wedge-idx :active player] (fnil inc 0)))

(defn move-component
  "Move one of player's components from :from kind to :to kind."
  [solar wedge-idx player from-k to-k]
  (let [n (get-in solar [wedge-idx from-k player] 0)]
    (if (pos? n)
      (-> solar
          (update-in [wedge-idx from-k]
                     (fn [m]
                       (let [n' (dec n)]
                         (if (zero? n') (dissoc m player) (assoc m player n')))))
          (update-in [wedge-idx to-k player] (fnil inc 0)))
      solar)))

(defn exhaust-one
  "Exhaust one of player's components in a wedge."
  [solar wedge-idx player]
  (move-component solar wedge-idx player :active :exhausted))

(defn unexhaust-all-in-wedge
  "Move every exhausted component in a wedge back to active."
  [solar wedge-idx]
  (let [exh (get-in solar [wedge-idx :exhausted] {})]
    (if (empty? exh)
      solar
      (-> solar
          (update-in [wedge-idx :active]
                     (fn [m] (reduce-kv (fn [acc p n] (update acc p (fnil + 0) n)) m exh)))
          (assoc-in [wedge-idx :exhausted] {})))))

(defn active-owners-of-color
  "Set of player keys with ≥1 active matching-color component."
  [state color]
  (let [k (board/color->wedge color)]
    (set
      (for [[p n] (get-in state [:solar-network k :active] {})
            :when (pos? n)]
        p))))

(defn player-total-solar-components
  "Sum of active + exhausted across all wedges for a player."
  [state pk]
  (apply +
    (for [k (range board/num-wedges)]
      (+ (active-count (:solar-network state) k pk)
         (exhausted-count (:solar-network state) k pk)))))

;; ── Piece helpers ─────────────────────────────────────────────────────────

(defn sundivers-at
  "Vector of sundiver records at sid (may be empty)."
  [state sid] (get-in state [:sundivers sid] []))

(defn city-at
  "City map at sid, or nil."
  [state sid] (get-in state [:cities sid]))

(defn city-here?
  "Is there a city at sid?"
  [state sid] (some? (city-at state sid)))

(defn player-sundivers-at
  "Sundivers at sid owned by player."
  [state sid player]
  (filterv #(= player (:owner %)) (sundivers-at state sid)))

(defn player-has-sundiver-at?
  "Does the player have at least one sundiver at sid?"
  [state sid player]
  (boolean (some #(= player (:owner %)) (sundivers-at state sid))))

(defn mothership-of
  "Mothership sid of a player, or nil if :supply."
  [state player]
  (let [ms (get-in state [:players player :mothership])]
    (when (and ms (not= ms :supply)) ms)))

(defn space-has-mothership?
  "Is any player's mothership at sid?"
  [state sid]
  (some (fn [[_ pd]] (= (:mothership pd) sid)) (:players state)))

(defn add-sundiver
  "Push a sundiver record onto sid."
  [state sid sd]
  (update-in state [:sundivers sid] (fnil conj []) sd))

(defn remove-sundiver-at
  "Remove the sundiver at position idx from sid."
  [state sid idx]
  (let [divs (sundivers-at state sid)]
    (assoc-in state [:sundivers sid]
              (vec (concat (subvec divs 0 idx) (subvec divs (inc idx)))))))

(defn drop-resource-on-space
  "Push a resource color onto free-standing resources at sid."
  [state sid color]
  (update-in state [:resources sid] (fnil conj []) color))

;; ── Public: current-player / choice-player ────────────────────────────────

(defn current-player
  "The turn-holder (flame). Nil at :game-over."
  [state]
  (when (and state (not= :game-over (:phase state)))
    (:flame state)))

(defn choice-player
  "The player who currently must make a choice — usually flame, but
   overridden by :owner-bonus-decision."
  [state]
  (case (:phase state)
    :owner-bonus-decision (get-in state [:phase-data :owner])
    (current-player state)))

(defn next-player
  "Next player in the turn-order rotation."
  [state player]
  (let [order (:turn-order state)
        i     (.indexOf order player)]
    (nth order (mod (inc i) (count order)))))

;; ── Energy helpers ────────────────────────────────────────────────────────

(defn give-energy
  "Give n energy to a player from the shared pool. Clamps at pool ≥ 0."
  [state player n]
  (let [n (max 0 (min n (:energy-pool state)))]
    (-> state
        (update :energy-pool - n)
        (update-in [:players player :energy] + n))))

(defn spend-energy
  "Pay n energy from a player to the pool."
  [state player n]
  (-> state
      (update-in [:players player :energy] - n)
      (update :energy-pool + n)))

(defn transfer-energy-to
  "Pay 1 energy from actor (if possible) to recipient; else from pool."
  [state actor recipient]
  (let [actor-energy (get-in state [:players actor :energy] 0)]
    (if (pos? actor-energy)
      (-> state
          (update-in [:players actor :energy] dec)
          (update-in [:players recipient :energy] inc))
      (-> state
          (update :energy-pool dec)
          (update-in [:players recipient :energy] inc)))))

;; ── Initial state ─────────────────────────────────────────────────────────

(defn- roll-orbital-dice
  "1d4 (tens) + 1d10 (ones); range 10..49."
  []
  (let [d4  (inc (rand-int 4))
        d10 (rand-int 10)]
    (+ (* 10 d4) d10)))

(defn- initial-planet-positions []
  (let [v (roll-orbital-dice)]
    (into {} (for [o board/orbits]
               [o (board/orbit-space o (mod v (board/ring-sizes o)))]))))

(defn initial-player
  "Fresh per-player data, with one component seeded into the solar network
   at setup (subtracted from :components here)."
  [wedge-color]
  {:wedge-color    wedge-color
   :mothership     :supply
   :habitat        starting-habitat-sundivers
   :reserve        starting-reserve-sundivers
   :energy         starting-energy-per-player
   :components     (dec starting-components-per-player)
   :city-platforms starting-platforms-per-player
   :links-supply   starting-links-per-player
   :vaporized      0})

(defn create-game
  "Return an initial game state. `players` is a vector of PK strings (up
   to 5). First player in the vector is the flame-holder."
  [players]
  (let [players       (vec players)
        n             (count players)
        wedge-colors  (vec (take n board/wedge-placement-order))
        player-map    (zipmap players (map initial-player wedge-colors))
        solar         (reduce
                        (fn [s [p c]]
                          (add-component s (board/color->wedge c) p))
                        (empty-solar-network)
                        (map vector players wedge-colors))
        starting-energy (* n starting-energy-per-player)
        empty-sd (into {} (for [s (board/all-spaces)] [s []]))]
    {:board             (board/build-board)
     :players           player-map
     :turn-order        players
     :flame             (first players)
     :phase             :place-mothership
     :phase-data        {}
     :turn              0
     :deck              (make-deck)
     :hands             (into {} (for [p players] [p []]))
     :discard           []
     :flares-drawn      0
     :market-resources  initial-market-resources
     :market-cities     {:silver 0 :green 0 :blue 0 :purple 0 :void 0}
     :energy-pool       (- initial-energy-pool starting-energy)
     :planets           (initial-planet-positions)
     :sundivers         empty-sd
     :resources         {}
     :cities            {}
     :links             #{}
     :solar-network     solar
     :winner            nil}))

;; ── Direction / geometry helpers ──────────────────────────────────────────

(defn set-mothership
  "Set a player's mothership to a space."
  [state player sid]
  (assoc-in state [:players player :mothership] sid))

(defn flame-space
  "Derived flame-space: front of the current flame-holder's mothership,
   or nil if their mothership is :supply."
  [state]
  (let [ms (mothership-of state (:flame state))]
    (when ms (board/front-space ms))))

(defn launch-targets
  "Set of unique spaces reachable by launching from habitat: mothership
   space, front-of-mothership, frontmost-adjacent-inner, frontmost-
   adjacent-outer. Up to 4 unique."
  [state player]
  (when-let [ms (mothership-of state player)]
    (let [adj    (get-in state [:board :adjacency])
          front  (board/front-space ms)
          r      (board/orbit-of ms)
          inner  (board/inner-orbit r)
          outer  (board/outer-orbit r)
          in-t   (when inner (board/frontmost-adjacent-in-ring adj ms inner))
          out-t  (when outer (board/frontmost-adjacent-in-ring adj ms outer))]
      (vec (distinct (keep identity [ms front in-t out-t]))))))

;; ── Link-graph helpers ────────────────────────────────────────────────────

(defn- link-endpoints [{:keys [a b]}] #{a b})

(defn- links-touching
  "All links (owner-filtered when owner supplied) that touch sid."
  ([state sid] (filter (fn [l] (contains? (link-endpoints l) sid)) (:links state)))
  ([state sid owner]
   (filter (fn [l] (and (= owner (:owner l))
                        (contains? (link-endpoints l) sid)))
           (:links state))))

(defn- player-link-adj
  "Undirected adjacency map from player-owned links."
  [state owner]
  (reduce (fn [m {:keys [a b]}]
            (-> m
                (update a (fnil conj #{}) b)
                (update b (fnil conj #{}) a)))
          {}
          (filter #(= owner (:owner %)) (:links state))))

(defn- link-bfs-in-graph
  "BFS over a precomputed adjacency map (see `link-graphs-for`)."
  [g start]
  (loop [frontier [start] seen #{start}]
    (if (empty? frontier)
      seen
      (let [n (first frontier) r (rest frontier)
            nexts (remove seen (get g n #{}))]
        (recur (into (vec r) nexts) (into seen nexts))))))

(defn- link-bfs
  "BFS from start over the player's link graph. Returns set of reachable
   sids (including start). Rebuilds the graph each call — prefer
   `link-bfs-in-graph` with a precomputed graph when doing many BFSes
   in a single legal-actions call."
  [state owner start]
  (link-bfs-in-graph (player-link-adj state owner) start))

(defn- link-graphs-for
  "Build per-owner adjacency maps once from (:links state). Returns
   `{owner → adjacency-map}` for all owners with ≥1 link."
  [state]
  (let [owners (into #{} (map :owner) (:links state))]
    (reduce (fn [m o] (assoc m o (player-link-adj state o))) {} owners)))

(defn- player-sundiver-spaces
  "Only spaces where `player` has ≥1 sundiver. Skips empty vectors —
   important because :sundivers is a full 86-key map, most empty."
  [state player]
  (reduce-kv (fn [acc sid divs]
               (if (and (seq divs) (some #(= player (:owner %)) divs))
                 (conj acc sid) acc))
             [] (:sundivers state)))

(defn- link-exists?
  "Is there any link between a and b already?"
  [state a b]
  (some (fn [l]
          (let [ep (link-endpoints l)]
            (and (contains? ep a) (contains? ep b))))
        (:links state)))

;; ── Lazy next-state values ────────────────────────────────────────────────
;;
;; `legal-actions` returns `{choice-key → Delay-of-next-state}` — deferring
;; the state-transition work until a caller actually picks a choice. Bots
;; and the WS handler only ever consume ONE next-state per call, so we no
;; longer waste work computing all of them.

(defn force-choice
  "Force a legal-actions value into its next-state. Accepts either a
   Delay or a plain state (for callers that constructed the map by hand)."
  [v]
  (cond
    (nil? v)              nil
    #?(:clj  (instance? clojure.lang.IDeref v)
       :cljs (satisfies? IDeref v)) @v
    :else v))

(defn next-state
  "Convenience: given a legal-actions map + choice-key, return the
   next-state (forcing the delay), or nil if the choice is not legal."
  [actions ck]
  (when-let [v (get actions ck)]
    (force-choice v)))

;; ── Auto-advance ──────────────────────────────────────────────────────────

(declare legal-actions* transition-to-game-over)

(defn- single-choice
  "If actions have exactly one entry, return the (forced) next-state, else nil."
  [actions]
  (when (= 1 (count actions))
    (force-choice (val (first actions)))))

;; Phases whose single-choice states we auto-advance through
(def ^:private auto-advance-phases
  #{:drawing-cards
    :orbit-planets
    :advance-mothership
    :pass-flame})

(defn- auto-advance
  "If (state) is in an auto-advance phase with exactly one legal choice,
   walk through until we reach something else. Idempotent."
  [state]
  (loop [s state
         guard 0]
    (cond
      (> guard 2000) s
      (nil? s)       s
      (not (contains? auto-advance-phases (:phase s))) s
      :else
      (let [as (legal-actions* s)
            nxt (single-choice as)]
        (if nxt (recur nxt (inc guard)) s)))))

;; ── PHASE: :place-mothership ──────────────────────────────────────────────

(defn- place-mothership-actions [state]
  (let [player (current-player state)]
    (into {}
      (for [sid board/beam-orbital-spaces
            :when (not (space-has-mothership? state sid))]
        [[:place-mothership sid]
         (delay
           (auto-advance
             (-> state
                 (set-mothership player sid)
                 (assoc :phase :choose-action-type)
                 (assoc :phase-data {}))))]))))

;; ── PHASE: :resolve-mothership ────────────────────────────────────────────

(defn- resolve-mothership-actions [state]
  (let [player (current-player state)
        ms     (mothership-of state player)
        r      (board/orbit-of ms)
        inner  (board/inner-orbit r)
        outer  (board/outer-orbit r)
        adj    (get-in state [:board :adjacency])
        base   {[:stay]
                (delay
                  (auto-advance
                    (-> state
                        (assoc :phase :choose-action-type)
                        (assoc :phase-data {}))))}
        with-in
        (if (and inner (not= :silver r))
          (if-let [target (board/frontmost-adjacent-in-ring adj ms inner)]
            (assoc base [:shift-in]
                   (delay
                     (auto-advance
                       (-> state
                           (set-mothership player target)
                           (assoc :phase :choose-action-type)
                           (assoc :phase-data {})))))
            base)
          base)
        with-out
        (if outer
          (if-let [target (board/frontmost-adjacent-in-ring adj ms outer)]
            (assoc with-in [:shift-out]
                   (delay
                     (auto-advance
                       (-> state
                           (set-mothership player target)
                           (assoc :phase :choose-action-type)
                           (assoc :phase-data {})))))
            with-in)
          with-in)]
    with-out))

;; ── Activation availability ───────────────────────────────────────────────

(defn- activatable-sun-spaces [state player]
  (vec
    (for [k (range board/num-wedges)
          :let [sid (board/sun-space k)]
          :when (player-has-sundiver-at? state sid player)]
      sid)))

(defn- activatable-planet-spaces [state player]
  (vec
    (for [[_ sid] (:planets state)
          :when (player-has-sundiver-at? state sid player)]
      sid)))

(defn- activatable-city-spaces [state player]
  (vec
    (for [[sid _] (:cities state)
          :when (player-has-sundiver-at? state sid player)]
      sid)))

;; ── PHASE: :choose-action-type ────────────────────────────────────────────

(defn- enter-moving [state]
  (assoc state
         :phase :moving
         :phase-data {:moves-left movement-points
                      :used-any? false}))

(defn- enter-activating-untargeted [state]
  (assoc state
         :phase :activating
         :phase-data {:target nil
                      :remaining #{}
                      :cards-owed 0
                      :activated-count 0
                      :exhausted-colors #{}
                      :exhaust-owners {}}))

(defn- choose-action-type-actions [state]
  (let [player (current-player state)
        base   {[:choose-move] (delay (auto-advance (enter-moving state)))}
        any-target?
        (or (seq (activatable-sun-spaces state player))
            (seq (activatable-planet-spaces state player))
            (seq (activatable-city-spaces state player)))]
    (if any-target?
      (assoc base [:choose-activate] (delay (auto-advance (enter-activating-untargeted state))))
      base)))

;; ── PHASE: :moving ────────────────────────────────────────────────────────

;; A trivial helper used below. We only iterate the neighbor set once,
;; filtering nothing.
(defn- neighbors-set [neighbors _sid] neighbors)

(defn- decrement-moves [state]
  (-> state
      (assoc-in [:phase-data :used-any?] true)
      (update-in [:phase-data :moves-left] dec)))

(defn- do-launch [state player dst]
  (-> state
      (update-in [:players player :habitat] dec)
      (add-sundiver dst {:owner player :resource nil :on-planet? false})
      decrement-moves))

(defn- pick-first-idx
  "Return the first index into vs matching pred, or nil."
  [pred vs]
  (first (keep-indexed (fn [i v] (when (pred v) i)) vs)))

(defn- do-fly [state player src dst]
  (let [divs (sundivers-at state src)
        idx  (pick-first-idx #(= player (:owner %)) divs)]
    (if idx
      (let [sd    (nth divs idx)
            sd'   (assoc sd :on-planet? false) ;; leaving space kills planet-ride
            state (remove-sundiver-at state src idx)
            state (add-sundiver state dst sd')]
        (decrement-moves state))
      state)))

(defn- do-path [state player src dst chain-owner]
  (let [divs (sundivers-at state src)
        idx  (pick-first-idx #(= player (:owner %)) divs)]
    (if idx
      (let [sd    (nth divs idx)
            sd'   (assoc sd :on-planet? false)
            state (remove-sundiver-at state src idx)
            state (add-sundiver state dst sd')
            state (if (and chain-owner (not= chain-owner player))
                    (give-energy state chain-owner 1)
                    state)]
        (decrement-moves state))
      state)))

(defn- planet-space-for-orbit [state r]
  (get-in state [:planets r]))

(defn- current-planet-at?
  "Is sid the current planet-space for its orbit?"
  [state sid]
  (and (board/orbital? sid)
       (= sid (planet-space-for-orbit state (board/orbit-of sid)))))

(defn- do-planet-flip
  "Toggle :on-planet? for a sundiver. **[POLICY]** Free — does not
   consume a movement point (previous policy charged 1)."
  [state sid idx on?]
  (let [divs (sundivers-at state sid)
        sd   (nth divs idx)
        sd'  (assoc sd :on-planet? (boolean on?))]
    (assoc-in state [:sundivers sid idx] sd')))

(defn- enter-drawing-cards
  "Transition to :drawing-cards with the given cards-owed. Zero owed is
   valid — just single-step through to the next phase."
  [state cards-owed]
  (auto-advance
    (-> state
        (assoc :phase :drawing-cards)
        (assoc :phase-data {:cards-owed cards-owed :cards-drawn 0}))))

(defn- moving-actions [state]
  (let [player (current-player state)
        moves  (get-in state [:phase-data :moves-left] 0)
        base   {[:done-moving] (delay (enter-drawing-cards state 1))}
        player-spaces (player-sundiver-spaces state player)

        ;; on/off toggles are FREE — always available, regardless of moves.
        planet-on-map
        (into {}
          (for [sid player-spaces
                :when (current-planet-at? state sid)
                [i sd] (map-indexed vector (sundivers-at state sid))
                :when (and (= player (:owner sd)) (not (:on-planet? sd)))]
            [[:planet-on [sid i]] (delay (do-planet-flip state sid i true))]))

        planet-off-map
        (into {}
          (for [sid player-spaces
                :when (current-planet-at? state sid)
                [i sd] (map-indexed vector (sundivers-at state sid))
                :when (and (= player (:owner sd)) (:on-planet? sd))]
            [[:planet-off [sid i]] (delay (do-planet-flip state sid i false))]))]
    (if (pos? moves)
      (let [adj (get-in state [:board :adjacency])
            link-graphs (link-graphs-for state)

            launch-map
            (if (pos? (get-in state [:players player :habitat] 0))
              (into {}
                (for [dst (launch-targets state player)]
                  [[:launch dst] (delay (do-launch state player dst))]))
              {})

            fly-map
            (into {}
              (for [sid player-spaces
                    dst (get adj sid #{})]
                [[:fly sid dst] (delay (do-fly state player sid dst))]))

            path-map
            (into {}
              (for [[owner g] link-graphs
                    sid player-spaces
                    :when (contains? g sid)
                    dst (disj (link-bfs-in-graph g sid) sid)]
                [[:path sid dst] (delay (do-path state player sid dst owner))]))]
        (merge base launch-map fly-map path-map planet-on-map planet-off-map))
      (merge base planet-on-map planet-off-map))))

;; ── PHASE: :activating ────────────────────────────────────────────────────

(defn- enter-activating-target [state target remaining]
  (assoc state
         :phase :activating
         :phase-data (merge (:phase-data state)
                            {:target target
                             :remaining (set remaining)
                             :activated-count 0})))

(defn- activate-space-transition
  "Given target and sid, return the state after entering the sub-phase."
  [state target sid]
  (let [pd (:phase-data state)]
    (case target
      :sun
      (assoc state
             :phase :activating-sun-space
             :phase-data (assoc pd :current sid))

      :planets
      (assoc state
             :phase :activating-planet-space
             :phase-data (assoc pd :current sid))

      :cities
      (let [city  (city-at state sid)
            orbit (board/orbit-of sid)
            ring-cities (get-in state [:market-cities orbit] 0)
            level (max 1 (min 3 ring-cities))
            {:keys [base]} (city-level-actions level)
            cards (get city-level-cards level 1)
            parent (assoc pd :target target
                             :cards-owed (+ (get pd :cards-owed 0) cards)
                             :level level)
            activator (:flame state)
            ;; Remove one activator sundiver from the city
            divs (sundivers-at state sid)
            aidx (pick-first-idx #(= activator (:owner %)) divs)
            state (if aidx
                    (-> state
                        (remove-sundiver-at sid aidx)
                        (update-in [:players activator :habitat] inc))
                    state)]
        (assoc state
               :phase :link-placement
               :phase-data {:actor activator
                            :actions-left base
                            :is-bonus? false
                            :parent-activation parent
                            :activation-space sid})))))

(defn- activating-actions [state]
  (let [player (current-player state)
        pd     (:phase-data state)
        target (:target pd)
        activated-count (:activated-count pd)]
    (cond
      (nil? target)
      (let [suns  (seq (activatable-sun-spaces state player))
            plans (seq (activatable-planet-spaces state player))
            cits  (seq (activatable-city-spaces state player))
            m     {}
            m     (if suns
                    (assoc m [:activate-sun]
                           (delay
                             (enter-activating-target state :sun
                                                      (activatable-sun-spaces state player))))
                    m)
            m     (if plans
                    (assoc m [:activate-planets]
                           (delay
                             (enter-activating-target state :planets
                                                      (activatable-planet-spaces state player))))
                    m)
            m     (if cits
                    (assoc m [:activate-cities]
                           (delay
                             (enter-activating-target state :cities
                                                      (activatable-city-spaces state player))))
                    m)]
        (if (empty? m)
          {[:no-activation-possible] (delay (enter-drawing-cards state 0))}
          m))

      :else
      (let [remaining (:remaining pd)
            valid-remaining
            (case target
              :sun     (set (filter #(player-has-sundiver-at? state % player) remaining))
              :planets (set (filter #(player-has-sundiver-at? state % player) remaining))
              :cities  (set (filter #(and (city-here? state %)
                                          (player-has-sundiver-at? state % player))
                                    remaining)))
            state (assoc-in state [:phase-data :remaining] valid-remaining)
            act-map
            (into {}
              (for [sid valid-remaining]
                [[:activate-space sid] (delay (activate-space-transition state target sid))]))
            can-done? (pos? activated-count)]
        (cond
          (and (zero? activated-count) (empty? act-map))
          {[:no-activation-possible] (delay (enter-drawing-cards state 0))}

          can-done?
          (assoc act-map [:done-activating]
                 (delay (enter-drawing-cards state (:cards-owed pd 0))))

          :else
          act-map)))))

;; ── PHASE: :activating-sun-space ──────────────────────────────────────────

(defn- return-to-activating
  "After a sub-phase completes, return to :activating (target set,
   activated-count+1). remove sid from remaining, bump cards-owed by
   this-space-cards."
  [state sid this-space-cards]
  (let [pd    (:phase-data state)]
    (assoc state
           :phase :activating
           :phase-data (-> pd
                           (assoc :current nil)
                           (update :activated-count (fnil inc 0))
                           (update :cards-owed (fnil + 0) this-space-cards)
                           (update :remaining (fnil disj #{}) sid)))))

(defn- do-sun-outer [state sid k idx]
  (let [player (current-player state)
        divs   (sundivers-at state sid)
        sd     (nth divs idx)
        state  (remove-sundiver-at state sid idx)
        state  (update-in state [:players player :habitat] inc)
        state  (if (:resource sd)
                 (drop-resource-on-space state sid (:resource sd))
                 state)
        sn     (:solar-network state)
        active (active-count sn k player)
        exh    (exhausted-count sn k player)
        gained (+ 2 (* 1 active) (* 2 exh))
        state  (give-energy state player gained)
        state  (update state :solar-network unexhaust-all-in-wedge k)]
    (return-to-activating state sid 1)))

(defn- do-sun-inner [state sid k idx]
  (let [player (current-player state)
        divs   (sundivers-at state sid)
        sd     (nth divs idx)
        color  (board/wedge-color k)
        state  (remove-sundiver-at state sid idx)
        state  (update-in state [:players player :reserve] inc)
        state  (if (:resource sd)
                 (drop-resource-on-space state sid (:resource sd))
                 state)
        _      (assert (= color (:resource sd)) "sun-inner requires matching resource")
        state  (update-in state [:players player :components] dec)
        state  (update state :solar-network add-component k player)]
    (return-to-activating state sid 1)))

(defn- activating-sun-space-actions [state]
  (let [player (current-player state)
        pd     (:phase-data state)
        sid    (:current pd)
        k      (board/wedge-of sid)
        color  (board/wedge-color k)
        divs   (sundivers-at state sid)
        m
        (into {}
          (for [[i sd] (map-indexed vector divs)
                :when (= player (:owner sd))
                [ck effect] (concat
                              [[[:sun-outer i] (delay (do-sun-outer state sid k i))]]
                              (when (and (= color (:resource sd))
                                         (pos? (get-in state [:players player :components] 0)))
                                [[[:sun-inner i] (delay (do-sun-inner state sid k i))]]))]
            [ck effect]))]
    (if (empty? m)
      {[:done-activating-space] (delay (return-to-activating state sid 0))}
      m)))

;; ── PHASE: :activating-planet-space ───────────────────────────────────────

(defn- do-planet-buy [state sid r idx]
  (let [player (current-player state)
        stock  (get-in state [:market-resources r] 0)
        price  (get resource-price-by-stock stock)
        divs   (sundivers-at state sid)
        sd     (nth divs idx)
        sd'    (assoc sd :resource r)
        state  (assoc-in state [:sundivers sid idx] sd')
        state  (spend-energy state player price)
        state  (update-in state [:market-resources r] dec)]
    (return-to-activating state sid 1)))

(defn- do-planet-build [state sid r idx res-color]
  (let [player (current-player state)
        divs   (sundivers-at state sid)
        state  (remove-sundiver-at state sid idx)
        state  (update-in state [:players player :vaporized] inc)
        state  (update-in state [:market-cities r] inc)
        state  (update-in state [:players player :city-platforms] dec)
        state  (assoc-in state [:cities sid] {:owner player :color res-color})
        state  (assoc-in state [:market-resources r] market-resource-cap)]
    (return-to-activating state sid 1)))

(defn- ring-has-city-of-color?
  "Is there a city of `color` anywhere in orbit `r`?"
  [state r color]
  (some (fn [sid]
          (let [c (city-at state sid)]
            (and c (= color (:color c)))))
        (board/orbit-spaces r)))

(defn- activating-planet-space-actions [state]
  (let [player (current-player state)
        pd     (:phase-data state)
        sid    (:current pd)
        r      (board/orbit-of sid)
        stock  (get-in state [:market-resources r] 0)
        price  (get resource-price-by-stock stock)
        energy (get-in state [:players player :energy] 0)
        divs   (sundivers-at state sid)
        buys
        (into {}
          (for [[i sd] (map-indexed vector divs)
                :when (and (= player (:owner sd))
                           (nil? (:resource sd))
                           (pos? stock)
                           price
                           (>= energy price))]
            [[:planet-buy i] (delay (do-planet-buy state sid r i))]))
        builds
        (into {}
          (for [[i sd] (map-indexed vector divs)
                :when (= player (:owner sd))
                :let  [res-color (:resource sd)]
                :when (and res-color
                           (not= res-color r)
                           (not (city-here? state sid))
                           (< (get-in state [:market-cities res-color] 0) cities-per-color)
                           (not (ring-has-city-of-color? state r res-color))
                           (pos? (get-in state [:players player :city-platforms] 0)))]
            [[:planet-build i res-color] (delay (do-planet-build state sid r i res-color))]))
        m (merge buys builds)]
    (if (empty? m)
      {[:done-activating-space] (delay (return-to-activating state sid 0))}
      m)))

;; ── PHASE: :link-placement ────────────────────────────────────────────────

(defn- outbound-color-of
  "outbound-color(src) per §5.9.3. `g-actor` is actor's link adjacency
   map (precompute once via `player-link-adj`)."
  [state g-actor src]
  (cond
    (board/sun? src)   (board/space-color src)
    (city-here? state src) (:color (city-at state src))
    :else
    (when (seq (get g-actor src))
      (let [reachable (link-bfs-in-graph g-actor src)
            city-colors (set
                          (for [sid reachable
                                :let [c (city-at state sid)]
                                :when c] (:color c)))]
        (when (= 1 (count city-colors))
          (first city-colors))))))

(defn- sun-anchored?
  "Is src reachable via actor's link graph to some sun wedge (or IS a sun
   wedge)? `g-actor` is actor's link adjacency map."
  [g-actor src]
  (or (board/sun? src)
      (some board/sun? (link-bfs-in-graph g-actor src))))

(defn- valid-link-start?
  "src is valid iff it's a sun wedge, a sun-anchored city, or a sun-
   anchored space with an actor link touching it (§5.9.1 clause 5).
   `g-actor` is actor's link adjacency map."
  [state g-actor src]
  (cond
    (board/sun? src) true
    (city-here? state src) (sun-anchored? g-actor src)
    :else (and (seq (get g-actor src))
               (sun-anchored? g-actor src))))

(defn- actor-link-count-at-in-graph
  "Number of actor's links touching sid, from a precomputed graph."
  [g-actor sid]
  (count (get g-actor sid #{})))

(defn- pay-per-link-transfer
  "If exhaust-owners[color] ≠ actor, transfer 1 energy from actor to
   that owner."
  [state actor color]
  (let [owner (get-in state [:phase-data :parent-activation :exhaust-owners color])]
    (if (and owner (not= owner actor))
      (transfer-energy-to state actor owner)
      state)))

(defn- apply-link
  "Place the link and pay all per-link costs."
  [state actor src dst color exhaust-pk]
  (let [state (-> state
                  (update-in [:players actor :energy] dec)
                  (update :energy-pool inc)
                  (update-in [:players actor :links-supply] dec)
                  (update :links conj {:a src :b dst :owner actor}))
        exhausted-colors (get-in state [:phase-data :parent-activation :exhausted-colors] #{})
        state (if (contains? exhausted-colors color)
                state
                ;; first link of this color this turn → exhaust one active
                ;; matching-color component of exhaust-pk
                (let [wk (board/color->wedge color)
                      state (update state :solar-network exhaust-one wk exhaust-pk)
                      state (assoc-in state [:phase-data :parent-activation :exhausted-colors]
                                      (conj exhausted-colors color))
                      state (assoc-in state [:phase-data :parent-activation :exhaust-owners color]
                                      exhaust-pk)]
                  state))
        state (pay-per-link-transfer state actor color)
        state (update-in state [:phase-data :actions-left] dec)]
    state))

(defn- link-placement-return
  "When link-placement finishes (either via [:done-linking] or when
   actions-left is exhausted), decide the next phase per §5.9.2."
  [state]
  (let [pd (:phase-data state)
        parent (:parent-activation pd)
        activation-space (:activation-space pd)
        is-bonus? (:is-bonus? pd)
        actor (:actor pd)
        activator (:flame state)
        city-owner (:owner (city-at state activation-space))]
    (if is-bonus?
      ;; back to activating (target set, activated-count > 0)
      (return-to-activating (assoc state :phase-data parent) activation-space 0)
      (cond
        ;; unowned city (shouldn't happen but degrade gracefully) — treat as self
        (nil? city-owner)
        (return-to-activating (assoc state :phase-data parent) activation-space 0)

        (= city-owner activator)
        ;; activator == owner; skip owner ask, go to activator bonus decision
        (assoc state
               :phase :activator-bonus-decision
               :phase-data {:activator activator
                            :bonus (:bonus (city-level-actions (:level parent)))
                            :parent-activation parent
                            :activation-space activation-space})

        :else
        (assoc state
               :phase :owner-bonus-decision
               :phase-data {:owner city-owner
                            :activator activator
                            :bonus (:bonus (city-level-actions (:level parent)))
                            :parent-activation parent
                            :activation-space activation-space})))))

(defn- link-placement-actions [state]
  (let [actor  (get-in state [:phase-data :actor])
        pd     (:phase-data state)
        adj    (get-in state [:board :adjacency])
        parent (:parent-activation pd)
        exhausted-colors (get parent :exhausted-colors #{})
        energy (get-in state [:players actor :energy] 0)
        links-supply (get-in state [:players actor :links-supply] 0)
        actions-left (get pd :actions-left 0)
        base {[:done-linking] (delay (link-placement-return state))}]
    (if (or (zero? actions-left) (zero? energy) (zero? links-supply))
      base
      ;; enumerate all valid links — precompute actor's link graph ONCE
      (let [g-actor (player-link-adj state actor)
            candidate-srcs
            (for [sid (get-in state [:board :spaces])
                  :when (and (valid-link-start? state g-actor sid)
                             (< (actor-link-count-at-in-graph g-actor sid) 2))]
              sid)
            link-map
            (reduce
              (fn [m src]
                (let [color (outbound-color-of state g-actor src)]
                  (if (nil? color)
                    m
                    (reduce
                      (fn [m dst]
                        (cond
                          (or (link-exists? state src dst)
                              (= src dst)
                              (>= (actor-link-count-at-in-graph g-actor dst) 2))
                          m

                          (contains? exhausted-colors color)
                          (assoc m [:link src dst]
                                 (delay
                                   (let [nxt (apply-link state actor src dst color nil)]
                                     (if (zero? (get-in nxt [:phase-data :actions-left] 0))
                                       (link-placement-return nxt)
                                       nxt))))

                          :else
                          (let [owners (active-owners-of-color state color)]
                            (if (empty? owners)
                              m
                              (reduce
                                (fn [m exhaust-pk]
                                  (assoc m [:link src dst exhaust-pk]
                                         (delay
                                           (let [nxt (apply-link state actor src dst color exhaust-pk)]
                                             (if (zero? (get-in nxt [:phase-data :actions-left] 0))
                                               (link-placement-return nxt)
                                               nxt)))))
                                m owners)))))
                      m
                      (get adj src #{})))))
              {} candidate-srcs)]
        (merge base link-map)))))

;; ── PHASE: :owner-bonus-decision ──────────────────────────────────────────

(defn- owner-bonus-decision-actions [state]
  (let [pd    (:phase-data state)
        owner (:owner pd)
        activator (:activator pd)
        bonus (:bonus pd)
        parent (:parent-activation pd)
        activation-space (:activation-space pd)
        take-state
        (assoc state
               :phase :link-placement
               :phase-data {:actor owner
                            :actions-left bonus
                            :is-bonus? true
                            :parent-activation parent
                            :activation-space activation-space})
        decline-state
        (assoc state
               :phase :activator-bonus-decision
               :phase-data {:activator activator
                            :bonus bonus
                            :parent-activation parent
                            :activation-space activation-space})]
    {[:take-bonus] (delay take-state)
     [:decline-bonus] (delay decline-state)}))

;; ── PHASE: :activator-bonus-decision ──────────────────────────────────────

(defn- activator-bonus-decision-actions [state]
  (let [pd (:phase-data state)
        activator (:activator pd)
        bonus (:bonus pd)
        parent (:parent-activation pd)
        activation-space (:activation-space pd)
        take-state
        (assoc state
               :phase :link-placement
               :phase-data {:actor activator
                            :actions-left bonus
                            :is-bonus? true
                            :parent-activation parent
                            :activation-space activation-space})
        decline-state
        (return-to-activating (assoc state :phase-data parent) activation-space 0)]
    {[:take-bonus] (delay take-state)
     [:decline-bonus] (delay decline-state)}))

;; ── PHASE: :drawing-cards ─────────────────────────────────────────────────

(defn- apply-flare-pull [state player]
  (let [ms (mothership-of state player)]
    (cond
      (nil? ms) state
      (board/sun? ms) state  ;; never actually reachable, defensive
      :else
      (let [r (board/orbit-of ms)]
        (if (= r :silver)
          (let [e    (get-in state [:players player :energy] 0)
                loss (long (Math/ceil (/ e 2.0)))]
            (-> state
                (update-in [:players player :energy] - loss)
                (update :energy-pool + loss)
                (set-mothership player (board/front-space ms))))
          (let [adj (get-in state [:board :adjacency])
                target (board/frontmost-adjacent-in-ring adj ms (board/inner-orbit r))]
            (if target
              (set-mothership state player target)
              state)))))))

(defn- draw-next-effect
  "Return the state resulting from a single [:draw-next] action."
  [state]
  (let [player (current-player state)
        deck   (:deck state)]
    (if (empty? deck)
      ;; no-op draw
      (let [pd (update (:phase-data state) :cards-drawn (fnil inc 0))
            done? (>= (:cards-drawn pd) (:cards-owed pd))
            state (assoc state :phase-data pd)]
        (if done?
          (auto-advance
            (assoc state :phase :orbit-planets :phase-data {:last-card nil}))
          state))
      (let [card (peek deck)
            state (-> state
                      (update :deck pop)
                      (update-in [:hands player] conj card))
            state (if (flare-card? card)
                    (let [state (update state :flares-drawn inc)
                          state (apply-flare-pull state player)]
                      state)
                    state)
            flares (:flares-drawn state)
            game-over? (>= flares flares-to-end)
            pd (update (:phase-data state) :cards-drawn (fnil inc 0))
            state (assoc state :phase-data pd)]
        (cond
          game-over?
          (transition-to-game-over state)

          (>= (:cards-drawn pd) (:cards-owed pd))
          (auto-advance
            (assoc state
                   :phase :orbit-planets
                   :phase-data {:last-card card}))

          :else
          state)))))

(defn- drawing-cards-actions [state]
  {[:draw-next] (delay (draw-next-effect state))})

;; ── Scoring (§6.4) ────────────────────────────────────────────────────────

(defn- link-graph-of [links]
  (reduce (fn [m {:keys [a b]}]
            (-> m
                (update a (fnil conj #{}) b)
                (update b (fnil conj #{}) a)))
          {} links))

(defn- connected-components [links]
  (let [g (link-graph-of links)
        nodes (set (keys g))]
    (loop [unvisited nodes
           comps []]
      (if (empty? unvisited)
        comps
        (let [start (first unvisited)
              comp (loop [frontier [start] seen #{}]
                     (if (empty? frontier)
                       seen
                       (let [n (first frontier) r (rest frontier)]
                         (if (contains? seen n)
                           (recur r seen)
                           (recur (into (vec r) (get g n #{})) (conj seen n))))))]
          (recur (apply disj unvisited comp) (conj comps comp)))))))

(defn- player-colors
  "Set of colors where player has ≥1 solar component (active+exhausted)."
  [state pk]
  (set
    (for [k (range board/num-wedges)
          :when (pos? (+ (active-count (:solar-network state) k pk)
                         (exhausted-count (:solar-network state) k pk)))]
      (board/wedge-color k))))

(defn- endpoint-valid?
  "Endpoint sid is valid for player P iff:
     - it's a city on a platform of P, OR
     - it's a city of color c and P has a solar component of color c, OR
     - it's a sun wedge and P has a component in that wedge."
  [state pk sid]
  (cond
    (board/sun? sid)
    (let [k (board/wedge-of sid)]
      (pos? (+ (active-count (:solar-network state) k pk)
               (exhausted-count (:solar-network state) k pk))))
    :else
    (when-let [c (city-at state sid)]
      (or (= pk (:owner c))
          (contains? (player-colors state pk) (:color c))))))

(defn- score-for-player [state pk]
  (let [player-links (filterv #(= pk (:owner %)) (:links state))
        comps (connected-components player-links)
        pcolors (player-colors state pk)]
    (reduce
      (fn [acc comp]
        ;; Enumerate all endpoint pairs in the component where both are
        ;; valid for pk. Include sun wedges as endpoints if any link in
        ;; comp touches one. Each pair contributes 1 point plus bonuses.
        (let [endpoints (filter #(endpoint-valid? state pk %) comp)
              pairs (for [i (range (count endpoints))
                          j (range (inc i) (count endpoints))]
                      [(nth (vec endpoints) i) (nth (vec endpoints) j)])
              bonus (reduce +
                      (for [[a b] pairs
                            end [a b]
                            :when (and (city-here? state end)
                                       (not= pk (:owner (city-at state end)))
                                       (contains? pcolors (:color (city-at state end))))
                            :let [k (board/color->wedge (:color (city-at state end)))
                                  n (+ (active-count (:solar-network state) k pk)
                                       (exhausted-count (:solar-network state) k pk))]]
                        (max 0 (dec n))))]
          (+ acc (count pairs) bonus)))
      0 comps)))

(defn compute-scores
  "Return {PK → score}."
  [state]
  (into {} (for [pk (:turn-order state)]
             [pk (score-for-player state pk)])))

(defn- decide-winner [state]
  (let [scores (compute-scores state)
        eq-scores? (apply = (vals scores))
        eq-comps? (apply = (map #(player-total-solar-components state %) (:turn-order state)))
        eq-cities? (apply = (map (fn [pk]
                                   (count (filter #(= pk (:owner %)) (vals (:cities state)))))
                                 (:turn-order state)))]
    (if (and eq-scores? eq-comps? eq-cities?)
      {:result :salvation :scores scores :salvation? true}
      (let [sorted (sort-by val > scores)
            groups (partition-by val sorted)
            singleton (first (drop-while #(> (count %) 1) groups))]
        (if singleton
          {:result :win :winner (first (first singleton)) :scores scores}
          {:result :none :scores scores})))))

(defn- transition-to-game-over
  "Terminal state: :game-over, compute scores, decide winner."
  [state]
  (let [decision (decide-winner state)]
    (assoc state
           :phase :game-over
           :phase-data {:scores (:scores decision)
                        :winner (if (= :win (:result decision))
                                  (:winner decision) nil)
                        :salvation? (= :salvation (:result decision))}
           :winner (if (= :salvation (:result decision))
                     :salvation
                     (dissoc decision :salvation?)))))

;; ── PHASE: :orbit-planets ─────────────────────────────────────────────────

(defn- orbit-planet [state r rate]
  (let [sid (get-in state [:planets r])
        n   (board/ring-sizes r)
        new-idx (mod (- (board/space-index sid) rate) n)
        new-sid (board/orbit-space r new-idx)
        ;; Move sundivers with :on-planet? true along
        divs-old (sundivers-at state sid)
        riding (filterv :on-planet? divs-old)
        staying (filterv #(not (:on-planet? %)) divs-old)
        state (assoc-in state [:sundivers sid] staying)
        state (reduce (fn [s sd] (add-sundiver s new-sid sd)) state riding)
        state (assoc-in state [:planets r] new-sid)]
    state))

(defn- resolve-orbit-effect [state]
  (let [last-card (get-in state [:phase-data :last-card])]
    (cond
      (nil? last-card) state

      (flare-card? last-card)
      (reduce (fn [s r] (orbit-planet s r (planet-advance-rate r)))
              state board/orbits)

      :else
      (let [color (:suit last-card)]
        (orbit-planet state color (planet-advance-rate color))))))

(defn- orbit-planets-actions [state]
  {[:orbit-resolved]
   (delay
     (auto-advance
       (-> state
           (resolve-orbit-effect)
           (assoc :phase :advance-mothership)
           (assoc :phase-data {}))))})

;; ── PHASE: :advance-mothership ────────────────────────────────────────────

(defn- advance-mothership-actions [state]
  (let [player (current-player state)
        ms     (mothership-of state player)
        state  (if ms
                 (set-mothership state player (board/front-space ms))
                 state)]
    {[:advance-resolved]
     (delay
       (auto-advance
         (-> state
             (assoc :phase :pass-flame)
             (assoc :phase-data {}))))}))

;; ── PHASE: :pass-flame ────────────────────────────────────────────────────

(defn- pass-flame-actions [state]
  (let [cur (current-player state)
        nxt (next-player state cur)
        state (-> state
                  (update :discard into (get-in state [:hands cur] []))
                  (assoc-in [:hands cur] [])
                  (assoc :flame nxt)
                  (update :turn inc))
        new-ms (mothership-of state nxt)
        phase (if new-ms :resolve-mothership :place-mothership)]
    {[:begin-next-turn]
     (delay
       (auto-advance
         (-> state
             (assoc :phase phase)
             (assoc :phase-data {}))))}))

;; ── PHASE: :game-over ─────────────────────────────────────────────────────

(defn- game-over-actions [state]
  ;; single-choice terminal fixed point
  {[:end] (delay state)})

;; ── Dispatch ──────────────────────────────────────────────────────────────

(defn- legal-actions*
  "Internal dispatch — no auto-advance at the outermost level."
  [state]
  (case (:phase state)
    :place-mothership         (place-mothership-actions state)
    :resolve-mothership       (resolve-mothership-actions state)
    :choose-action-type       (choose-action-type-actions state)
    :moving                   (moving-actions state)
    :activating               (activating-actions state)
    :activating-sun-space     (activating-sun-space-actions state)
    :activating-planet-space  (activating-planet-space-actions state)
    :link-placement           (link-placement-actions state)
    :owner-bonus-decision     (owner-bonus-decision-actions state)
    :activator-bonus-decision (activator-bonus-decision-actions state)
    :drawing-cards            (drawing-cards-actions state)
    :orbit-planets            (orbit-planets-actions state)
    :advance-mothership       (advance-mothership-actions state)
    :pass-flame               (pass-flame-actions state)
    :game-over                (game-over-actions state)
    {}))

(defn legal-actions
  "Public: map choice-key → next-state for the current phase. Callers see
   already-auto-advanced next-states (§9 auto-advance for single-choice
   phases)."
  [state]
  (legal-actions* state))
