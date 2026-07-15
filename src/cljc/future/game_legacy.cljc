(ns future.game-legacy)

;; ─── LEGACY: original implementation, preserved as reference ───
;; ─── Inert: all forms below live inside (comment ...).            ───

(comment

(ns future-legacy.game
  "State, transitions, and legal-actions for FUTURE.

   Public surface (called by future-ws and play.cljs):
     create-game     [players] → state
     legal-actions   [state]   → {choice-key → next-state}
     current-player  [state]   → player-key | nil

   State shape (top-level keys):
     :board             topology (adjacency, spaces, orbits, wedges)
     :players           {player-key → player-data}
     :turn-order        [player-key …]            insertion order
     :flame             player-key                whose turn it is
     :flame-space       sid | nil                 location of the flame token
     :phase             one of
                          :place-mothership :pre-action :action
                          :moving :activating :placing-links :game-over
     :phase-data        {}                        transient working state
     :turn              integer                   turn counter
     :deck              [card …]                  draw deck
     :hands             {player-key → [card …]}
     :discard           [card …]
     :flares-drawn      integer
     :market-resources  {color → 0..5}
     :market-cities     {color → 0..4}
     :energy-pool       integer (89 initial less starting energy)
     :planets           {orbit → sid}
     :sundivers         {sid → [{:owner :resource}]}
     :cities            {sid → {:color :owner}}
     :links             [{:a :b :owner :color}]
     :solar-network     {wedge-idx → {:active {p → n} :exhausted {p → n}}}
     :winner            nil | {:result :win|:loss :scores {p → n}}

   Per-player data:
     :wedge-color       color of the sun wedge seeded with this player
     :mothership        sid | :supply
     :habitat           sundivers in habitat
     :reserve           sundivers in reserve (not yet launched)
     :energy            energy held
     :components        components remaining in personal supply (off-board)
     :city-platforms    unspent platforms
     :links-supply      unspent links
     :vaporized         sundivers permanently removed (from city-building)"
  (:require [future-legacy.board :as board]))

;; ── Constants ───────────────────────────────────────────────────────────────

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
  {1 5, 2 3, 3 2, 4 1, 5 1})

(def city-level-actions
  {1 {:base 1 :bonus 1}
   2 {:base 2 :bonus 1}
   3 {:base 3 :bonus 2}})

;; ── Cards & deck ───────────────────────────────────────────────────────────

(defn make-deck []
  (vec
   (shuffle
    (for [suit card-suits, value (range 1 (inc cards-per-suit))]
      {:suit suit :value value}))))

(defn flare-card? [c] (= :flare (:suit c)))

;; ── Initial player state ───────────────────────────────────────────────────

(defn initial-player [wedge-color]
  {:wedge-color    wedge-color
   :mothership     :supply
   :habitat        starting-habitat-sundivers
   :reserve        starting-reserve-sundivers
   :energy         starting-energy-per-player
   ;; one component is seeded onto the sun at setup
   :components     (dec starting-components-per-player)
   :city-platforms starting-platforms-per-player
   :links-supply   starting-links-per-player
   :vaporized      0})

;; ── Solar network helpers ──────────────────────────────────────────────────

(defn empty-wedge [] {:active {} :exhausted {}})

(defn empty-solar-network []
  (into {} (for [k (range board/num-wedges)] [k (empty-wedge)])))

(defn add-component
  "Add a player's component to the active section of a wedge."
  [solar wedge-idx player]
  (update-in solar [wedge-idx :active player] (fnil inc 0)))

(defn wedge-counts [solar wedge-idx]
  (let [active    (get-in solar [wedge-idx :active] {})
        exhausted (get-in solar [wedge-idx :exhausted] {})]
    {:active (apply + (vals active))
     :exhausted (apply + (vals exhausted))}))

(defn active-count   [solar wedge-idx p] (get-in solar [wedge-idx :active p] 0))
(defn exhausted-count [solar wedge-idx p] (get-in solar [wedge-idx :exhausted p] 0))

(defn move-component
  "Move one component of player from :from kind to :to kind in a wedge."
  [solar wedge-idx player from-k to-k]
  (let [n (get-in solar [wedge-idx from-k player] 0)]
    (if (pos? n)
      (-> solar
          (update-in [wedge-idx from-k]
                     (fn [m] (let [n' (dec n)]
                               (if (zero? n') (dissoc m player) (assoc m player n')))))
          (update-in [wedge-idx to-k player] (fnil inc 0)))
      solar)))

(defn exhaust-one [solar wedge-idx player]
  (move-component solar wedge-idx player :active :exhausted))

(defn unexhaust-all-in-wedge [solar wedge-idx]
  (let [exh (get-in solar [wedge-idx :exhausted] {})]
    (-> solar
        (update-in [wedge-idx :active]
                   (fn [m] (reduce-kv (fn [acc p n] (update acc p (fnil + 0) n)) m exh)))
        (assoc-in [wedge-idx :exhausted] {}))))

(defn unexhaust-color [solar color]
  (unexhaust-all-in-wedge solar (board/color->wedge color)))

(defn unexhaust-all-colors [solar]
  (reduce unexhaust-all-in-wedge solar (range board/num-wedges)))

(defn player-has-any-component-of-color?
  "Does the player have at least one component (active or exhausted) of a color?"
  [state player color]
  (let [wedge (board/color->wedge color)
        sn    (:solar-network state)]
    (pos? (+ (active-count sn wedge player)
             (exhausted-count sn wedge player)))))

;; ── Board piece helpers ────────────────────────────────────────────────────

(defn sundivers-at [state sid] (get-in state [:sundivers sid] []))

(defn add-sundiver [state sid sd]
  (update-in state [:sundivers sid] (fnil conj []) sd))

(defn remove-sundiver-by-pred
  "Remove the first sundiver matching pred at sid. Returns [state removed]."
  [state sid pred]
  (let [divs (sundivers-at state sid)
        idx  (first (keep-indexed (fn [i d] (when (pred d) i)) divs))]
    (if idx
      [(assoc-in state [:sundivers sid]
                 (vec (concat (subvec divs 0 idx) (subvec divs (inc idx)))))
       (nth divs idx)]
      [state nil])))

(defn player-sundivers-at [state sid player]
  (filterv #(= (:owner %) player) (sundivers-at state sid)))

(defn player-has-sundiver? [state sid player]
  (boolean (some #(= (:owner %) player) (sundivers-at state sid))))

(defn city-at [state sid]      (get-in state [:cities sid]))
(defn city-here? [state sid]   (some? (city-at state sid)))

(defn mothership-of [state player]
  (let [ms (get-in state [:players player :mothership])]
    (when (and ms (not= ms :supply)) ms)))

(defn space-has-mothership? [state sid]
  (some (fn [[_ pd]] (= (:mothership pd) sid)) (:players state)))

;; ── Players & turn order ───────────────────────────────────────────────────

(defn current-player [state]
  (when-not (= :game-over (:phase state)) (:flame state)))

(defn next-player [state player]
  (let [order (:turn-order state)
        i (.indexOf order player)]
    (nth order (mod (inc i) (count order)))))

;; ── Setup ──────────────────────────────────────────────────────────────────

(defn- roll-orbital-dice
  "1d4 (tens) + 1d10 (ones); range 10..49."
  []
  (let [d4 (inc (rand-int 4))
        d10 (rand-int 10)]
    (+ (* 10 d4) d10)))

(defn- initial-planet-positions []
  (let [v (roll-orbital-dice)]
    (into {} (for [o board/orbits]
               [o (board/orbit-space o (mod v (board/orbit-sizes o)))]))))

(defn create-game
  "players: ordered vector of player keys. First player begins with the flame."
  [players]
  (let [n              (count players)
        wedge-colors   (vec (take n board/wedge-placement-order))
        player-map     (zipmap players (map initial-player wedge-colors))
        solar          (reduce
                         (fn [s [p c]]
                           (add-component s (board/color->wedge c) p))
                         (empty-solar-network)
                         (map vector players wedge-colors))
        deck           (make-deck)
        starting-energy (* n starting-energy-per-player)
        empty-sundivers (into {} (for [s (board/all-spaces)] [s []]))]
    {:board             (board/build-board)
     :players           player-map
     :turn-order        (vec players)
     :flame             (first players)
     :flame-space       nil
     :phase             :place-mothership
     :phase-data        {}
     :turn              0
     :deck              deck
     :hands             (into {} (for [p players] [p []]))
     :discard           []
     :flares-drawn      0
     :market-resources  initial-market-resources
     :market-cities     {:silver 0 :green 0 :blue 0 :purple 0 :void 0}
     :energy-pool       (- initial-energy-pool starting-energy)
     :planets           (initial-planet-positions)
     :sundivers         empty-sundivers
     :cities            {}
     :links             []
     :solar-network     solar
     :winner            nil}))

;; ── Energy helpers ─────────────────────────────────────────────────────────

(defn give-energy [state player n]
  (update-in state [:players player :energy] + n))

(defn spend-energy [state player n]
  (-> state
      (update-in [:players player :energy] - n)
      (update :energy-pool + n)))

(defn pay-to-market [state player n]
  ;; Energy paid into the market goes back to the energy pool.
  (spend-energy state player n))

(defn refund-from-pool [state player n]
  (-> state
      (update :energy-pool - n)
      (update-in [:players player :energy] + n)))

;; ── Mothership movement ────────────────────────────────────────────────────

(defn set-mothership-space [state player sid]
  (assoc-in state [:players player :mothership] sid))

(defn pull-mothership-toward-sun
  "Flare effect. If already at silver, advance forward one space and lose
   ceil(energy/2). On a sun wedge, no further pull (already at the sun)."
  [state player]
  (let [ms (mothership-of state player)]
    (cond
      (nil? ms) state
      (board/sun? ms) state

      :else
      (let [o (board/orbit-of ms)]
        (if (= o :silver)
          (let [e    (get-in state [:players player :energy] 0)
                loss (long (Math/ceil (/ e 2.0)))]
            (-> state
                (update-in [:players player :energy] - loss)
                (update :energy-pool + loss)
                (set-mothership-space player (board/front-space ms))))
          (let [target (board/frontmost-adjacent-in-orbit
                         (get-in state [:board :adjacency])
                         ms
                         (board/inner-orbit o))]
            (if target
              (set-mothership-space state player target)
              state)))))))

;; ── Forward declarations ───────────────────────────────────────────────────

(declare compute-scores decide-winner end-turn)

;; ── Card draw / resolution ─────────────────────────────────────────────────

(defn- draw-one [state player]
  (if (empty? (:deck state))
    [state nil]
    (let [card (peek (:deck state))]
      [(-> state
           (update :deck pop)
           (update-in [:hands player] conj card))
       card])))

(defn- apply-flare-effects [state player card]
  (if (flare-card? card)
    (-> state
        (update :flares-drawn inc)
        (pull-mothership-toward-sun player))
    state))

(defn- resolve-last-card-effect
  "After all draws, the LAST card determines what advances and which color
   refreshes. Flare → all planets advance and all components unexhaust."
  [state card]
  (if (nil? card)
    state
    (if (flare-card? card)
      ;; All planets advance one full step + all components refresh
      (-> (reduce (fn [s o]
                    (let [sid    (get-in s [:planets o])
                          rate   (planet-advance-rate o)
                          new-id (nth (iterate board/front-space sid) rate)]
                      (assoc-in s [:planets o] new-id)))
                  state board/orbits)
          (update :solar-network unexhaust-all-colors))
      (let [color (:suit card)
            rate  (planet-advance-rate color)
            sid   (get-in state [:planets color])
            new-id (nth (iterate board/front-space sid) rate)]
        (-> state
            (assoc-in [:planets color] new-id)
            (update :solar-network unexhaust-color color))))))

(defn draw-and-resolve
  "Draw n cards. For each: apply flare effect. After all draws: resolve last."
  [state player n]
  (loop [s state remaining n last-card nil drew-any false]
    (if (or (zero? remaining) (empty? (:deck s)))
      (let [s (if drew-any (resolve-last-card-effect s last-card) s)]
        (if (>= (:flares-drawn s) flares-to-end)
          (let [{:keys [scores winners]} (decide-winner s)
                result (if (seq winners) :win :loss)]
            (-> s
                (assoc :phase :game-over)
                (assoc :winner {:result  result
                                :reason  :thirteen-flares
                                :scores  scores
                                :winners winners})))
          s))
      (let [[s' card] (draw-one s player)]
        (if (nil? card)
          (recur s' 0 last-card drew-any)
          (recur (apply-flare-effects s' player card)
                 (dec remaining)
                 card
                 true))))))

;; ── Scoring ────────────────────────────────────────────────────────────────

(defn- link-graph
  "Build {sid → #{neighbor-sid}} from the player's links (any owner)."
  [links]
  (reduce (fn [m {:keys [a b]}]
            (-> m
                (update a (fnil conj #{}) b)
                (update b (fnil conj #{}) a)))
          {} links))

(defn- connected-components [links]
  (let [g (link-graph links)]
    (loop [unvisited (set (keys g))
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
                           (recur (into (vec r) (g n)) (conj seen n))))))]
          (recur (apply disj unvisited comp) (conj comps comp)))))))

(defn- city-endpoints-in [state component]
  (for [sid component
        :when (city-at state sid)]
    (assoc (city-at state sid) :sid sid)))

(defn- player-component-colors
  "Colors where the player has at least one solar-network component (active or exhausted)."
  [state player]
  (set
    (for [k (range board/num-wedges)
          :when (or (pos? (active-count (:solar-network state) k player))
                    (pos? (exhausted-count (:solar-network state) k player)))]
      (board/wedge-color k))))

(defn- path-valid-end?
  "Is this city a valid scoring endpoint for the player?"
  [state player city]
  (or (= player (:owner city))
      (contains? (player-component-colors state player) (:color city))))

(defn- score-paths-for-player
  "Count valid scoring paths the player created. For each pair of cities in a
   connected component where the player owns at least one link, if both ends
   are valid endpoints, that's a scoring path."
  [state player]
  (let [pcolors (player-component-colors state player)]
    (reduce
      (fn [score component]
        (let [cities (city-endpoints-in state component)
              ;; only consider components where this player owns any link
              player-link?
              (some #(and (or (contains? component (:a %)) (contains? component (:b %)))
                          (= player (:owner %)))
                    (:links state))]
          (if (and player-link? (>= (count cities) 2))
            (let [pairs (for [i (range (count cities))
                              j (range (inc i) (count cities))
                              :let [c1 (nth cities i)
                                    c2 (nth cities j)]
                              :when (and (path-valid-end? state player c1)
                                         (path-valid-end? state player c2))]
                          [c1 c2])
                  bonus (reduce + (map (fn [[c1 c2]]
                                         (let [extra (fn [c]
                                                       (when (and (not= player (:owner c))
                                                                  (contains? pcolors (:color c)))
                                                         (let [k (board/color->wedge (:color c))
                                                               n (+ (active-count (:solar-network state) k player)
                                                                    (exhausted-count (:solar-network state) k player))]
                                                           (max 0 (dec n)))))]
                                           (+ (or (extra c1) 0) (or (extra c2) 0))))
                                       pairs))]
              (+ score (count pairs) bonus))
            score)))
      0
      (connected-components (:links state)))))

(defn compute-scores [state]
  (into {} (for [p (:turn-order state)]
             [p (score-paths-for-player state p)])))

(defn- decide-winner
  "Highest UNTIED score wins. Tied players lose and we cascade down to the
   next group. If every group is tied (or every player has the same score),
   no one wins."
  [state]
  (let [scores  (compute-scores state)
        sorted  (sort-by val > scores)
        groups  (partition-by val sorted)
        singleton (first (drop-while #(> (count %) 1) groups))
        winners (when singleton [(first (first singleton))])]
    {:scores scores :winners (or winners [])}))

;; ── End of turn ────────────────────────────────────────────────────────────

(defn- start-next-turn [state]
  (let [cur     (:flame state)
        np      (next-player state cur)
        new-ms  (mothership-of state np)
        new-phase (cond
                    (= :game-over (:phase state)) :game-over
                    (nil? new-ms) :place-mothership
                    :else :pre-action)
        flame-space (when new-ms (board/front-space new-ms))]
    (-> state
        (assoc :flame np)
        (assoc :flame-space flame-space)
        (assoc :phase new-phase)
        (assoc :phase-data {})
        (update :turn inc))))

(defn end-turn
  "End the current player's turn:
     1. Draw queued cards
     2. Move mothership to flame-space (if set)
     3. Pass flame to next player and reset"
  ([state] (end-turn state 0))
  ([state extra-draws]
   (let [player    (:flame state)
         draws     (max 0 (+ extra-draws (get-in state [:phase-data :cards-pending] 0)))
         state'    (draw-and-resolve state player draws)]
     (if (= :game-over (:phase state'))
       state'
       (let [;; advance mothership to flame-space
             state' (if-let [fs (:flame-space state')]
                      (set-mothership-space state' player fs)
                      state')]
         (start-next-turn state'))))))

;; ── PHASE: place-mothership ────────────────────────────────────────────────

(defn place-mothership-actions [state]
  (let [player (current-player state)]
    (into {}
          (for [orbit board/orbits
                :let [sid (board/beam-space-for-orbit orbit)]
                :when (not (space-has-mothership? state sid))]
            [[:place-mothership sid]
             (-> state
                 (set-mothership-space player sid)
                 (assoc :flame-space (board/front-space sid))
                 (assoc :phase :action)
                 (assoc :phase-data {}))]))))

;; ── PHASE: pre-action (resolve mothership: shift or stay) ─────────────────

(defn- shift-mothership-to [state player direction]
  (let [ms (mothership-of state player)
        o  (board/orbit-of ms)
        target-orbit (case direction :in (board/inner-orbit o) :out (board/outer-orbit o))]
    (when target-orbit
      (let [adj (get-in state [:board :adjacency])
            target (board/frontmost-adjacent-in-orbit adj ms target-orbit)]
        (when target
          (-> state
              (set-mothership-space player target)
              (assoc :flame-space (board/front-space target))))))))

(defn pre-action-actions [state]
  (let [player (current-player state)
        ms     (mothership-of state player)
        o      (board/orbit-of ms)
        inner  (board/inner-orbit o)
        outer  (board/outer-orbit o)
        m {[:stay] (-> state (assoc :phase :action) (assoc :phase-data {}))}
        m (if inner
            (if-let [s (shift-mothership-to state player :in)]
              (assoc m [:shift-in] (-> s (assoc :phase :action) (assoc :phase-data {})))
              m)
            m)
        m (if outer
            (if-let [s (shift-mothership-to state player :out)]
              (assoc m [:shift-out] (-> s (assoc :phase :action) (assoc :phase-data {})))
              m)
            m)]
    m))

;; ── PHASE: action ──────────────────────────────────────────────────────────

(defn- enter-moving [state]
  (assoc state
         :phase :moving
         :phase-data {:moves-remaining movement-points
                      :cards-pending 1
                      :activated-spaces #{}
                      :exhausted-colors #{}}))

(defn- activatable-sun-spaces [state player]
  (for [k (range board/num-wedges)
        :let [sid (board/sun-space k)]
        :when (player-has-sundiver? state sid player)]
    sid))

(defn- activatable-planet-spaces [state player]
  (for [[_ sid] (:planets state)
        :when (player-has-sundiver? state sid player)]
    sid))

(defn- activatable-city-spaces [state player]
  (for [[sid _] (:cities state)
        :when (player-has-sundiver? state sid player)]
    sid))

(defn- enter-activating [state activate-type]
  (assoc state
         :phase :activating
         :phase-data {:activate-type activate-type
                      :activated-spaces #{}
                      :exhausted-colors #{}
                      :cards-pending 0}))

(defn action-actions [state]
  (let [player (current-player state)
        m {[:move] (enter-moving state)}
        m (if (seq (activatable-sun-spaces state player))
            (assoc m [:activate :sun] (enter-activating state :sun))
            m)
        m (if (seq (activatable-planet-spaces state player))
            (assoc m [:activate :planets] (enter-activating state :planets))
            m)
        m (if (seq (activatable-city-spaces state player))
            (assoc m [:activate :cities] (enter-activating state :cities))
            m)]
    m))

;; ── PHASE: moving ──────────────────────────────────────────────────────────

(defn- launch-targets
  "Where launching from habitat can land: mothership space, front of mothership,
   frontmost-adj inner ring, frontmost-adj outer ring."
  [state player]
  (when-let [ms (mothership-of state player)]
    (let [adj   (get-in state [:board :adjacency])
          front (board/front-space ms)
          o     (board/orbit-of ms)
          inner (when (and o (board/inner-orbit o))
                  (board/frontmost-adjacent-in-orbit adj ms (board/inner-orbit o)))
          outer (when (and o (board/outer-orbit o))
                  (board/frontmost-adjacent-in-orbit adj ms (board/outer-orbit o)))]
      (vec (distinct (filter some? [ms front inner outer]))))))

(defn- launch-sundiver [state player target]
  (-> state
      (update-in [:players player :habitat] dec)
      (add-sundiver target {:owner player :resource nil})))

(defn- fly-sundiver [state player from to]
  (let [[s removed] (remove-sundiver-by-pred state from #(= (:owner %) player))]
    (if removed
      (add-sundiver s to removed)
      state)))

(defn- player-link-chain
  "Spaces reachable from `start` via a chain of links all owned by `owner`."
  [state owner start]
  (let [links (filter #(= owner (:owner %)) (:links state))
        adj   (link-graph links)]
    (loop [frontier [start] seen #{}]
      (if (empty? frontier)
        seen
        (let [n (first frontier) r (rest frontier)]
          (if (contains? seen n)
            (recur r seen)
            (recur (into (vec r) (get adj n #{})) (conj seen n))))))))

(defn- link-travel-targets
  "Per-player reachable spaces from `from` via a uniform-color link chain.
   Returns a vector of {:to :owner}."
  [state from]
  (let [owners (set (for [{:keys [a b owner]} (:links state)
                          :when (or (= a from) (= b from))]
                      owner))]
    (vec
      (for [o owners
            t (disj (player-link-chain state o from) from)]
        {:to t :owner o}))))

(defn- decrement-moves [state]
  (let [n (dec (get-in state [:phase-data :moves-remaining]))]
    (if (zero? n)
      (end-turn state)
      (assoc-in state [:phase-data :moves-remaining] n))))

(defn moving-actions [state]
  (let [player (current-player state)
        moves  (get-in state [:phase-data :moves-remaining])
        base   {[:done-moving] (end-turn state)}]
    (if (zero? moves)
      base
      (let [launches
            (when (pos? (get-in state [:players player :habitat] 0))
              (into {}
                (for [t (launch-targets state player)]
                  [[:launch t]
                   (-> state (launch-sundiver player t) decrement-moves)])))
            flies
            (into {}
              (for [[sid divs] (:sundivers state)
                    :when (some #(= (:owner %) player) divs)
                    target (get-in state [:board :adjacency sid] #{})]
                [[:fly sid target]
                 (-> state (fly-sundiver player sid target) decrement-moves)]))
            ;; Travel along link chains
            link-travels
            (into {}
              (for [[sid divs] (:sundivers state)
                    :when (some #(= (:owner %) player) divs)
                    {:keys [to owner]} (link-travel-targets state sid)]
                [[:link-travel sid to owner]
                 (let [;; If the chain is owned by someone else, pay them +1 energy
                       s (if (and owner (not= owner player))
                           (refund-from-pool state owner 1)
                           state)
                       s (fly-sundiver s player sid to)]
                   (decrement-moves s))]))]
        (merge base launches flies link-travels)))))

;; ── PHASE: activating ─────────────────────────────────────────────────────

(defn- bump-cards [state n]
  (update-in state [:phase-data :cards-pending] (fnil + 0) n))

(defn- mark-activated [state sid]
  (update-in state [:phase-data :activated-spaces] (fnil conj #{}) sid))

(defn- already-activated? [state sid]
  (contains? (get-in state [:phase-data :activated-spaces] #{}) sid))

;; ---- SUN activation ----

(defn- sundiver-with-matching-resource-at-sun [state player wedge-idx]
  (let [color (board/wedge-color wedge-idx)
        sid   (board/sun-space wedge-idx)
        divs  (sundivers-at state sid)]
    (first
     (filter (fn [d] (and (= (:owner d) player) (= color (:resource d))))
             divs))))

(defn- activate-sun-outer [state player wedge-idx]
  ;; Take 2 + 1 per active + 2 per exhausted; return ANY ONE of player's
  ;; sundivers in the outer (non-matching) section to habitat; refresh all
  ;; exhausted in this wedge.
  (let [sn (:solar-network state)
        {:keys [active exhausted]} (wedge-counts sn wedge-idx)
        gained (+ 2 active (* 2 exhausted))
        sid (board/sun-space wedge-idx)
        color (board/wedge-color wedge-idx)
        ;; Pick a sundiver of this player to return; prefer one without matching resource
        pred-non-matching #(and (= (:owner %) player) (not= color (:resource %)))
        pred-any          #(= (:owner %) player)
        [s removed] (remove-sundiver-by-pred state sid pred-non-matching)
        [s removed] (if removed [s removed] (remove-sundiver-by-pred state sid pred-any))]
    (cond-> s
      removed (update-in [:players player :habitat] inc)
      ;; If removed sundiver was carrying a resource, lose it (return to market row)
      (and removed (:resource removed))
      (update-in [:market-resources (:resource removed)]
                 (fn [v] (min market-resource-cap (inc v))))
      true (give-energy player gained)
      true (update :solar-network unexhaust-all-in-wedge wedge-idx)
      true (mark-activated sid)
      true (bump-cards 1))))

(defn- activate-sun-inner [state player wedge-idx]
  ;; Return resource + sundiver to reserve, place active component.
  (let [sid (board/sun-space wedge-idx)
        color (board/wedge-color wedge-idx)
        [s removed] (remove-sundiver-by-pred
                      state sid
                      (fn [d] (and (= (:owner d) player) (= color (:resource d)))))]
    (if removed
      (-> s
          (update-in [:players player :reserve] inc)
          ;; resource returns to its market row
          (update-in [:market-resources color]
                     (fn [v] (min market-resource-cap (inc v))))
          (update :solar-network add-component wedge-idx player)
          (update-in [:players player :components] dec)
          (mark-activated sid)
          (bump-cards 1))
      state)))

(defn- sun-activation-actions [state player]
  (let [acts
        (apply merge
               (for [k (range board/num-wedges)
                     :let [sid (board/sun-space k)]
                     :when (and (not (already-activated? state sid))
                                (player-has-sundiver? state sid player))]
                 (let [base
                       {[:activate-sun-outer k]
                        (activate-sun-outer state player k)}
                       inner?
                       (and (sundiver-with-matching-resource-at-sun state player k)
                            (pos? (get-in state [:players player :components] 0)))]
                   (if inner?
                     (assoc base
                            [:activate-sun-inner k]
                            (activate-sun-inner state player k))
                     base))))]
    (assoc (or acts {}) [:done-activating] (end-turn state))))

;; ---- PLANET activation ----

(defn- planet-buy-resource-actions [state player]
  (apply merge
         (for [[orbit sid] (:planets state)
               :let [stock (get-in state [:market-resources orbit] 0)
                     price (get resource-price-by-stock stock)]
               :when (and price
                          (>= (get-in state [:players player :energy] 0) price)
                          (not (already-activated? state sid))
                          (some (fn [d] (and (= (:owner d) player) (nil? (:resource d))))
                                (sundivers-at state sid)))]
           (let [divs   (sundivers-at state sid)
                 idx    (first (keep-indexed
                                 (fn [i d] (when (and (= (:owner d) player)
                                                      (nil? (:resource d))) i))
                                 divs))
                 divs'  (assoc-in divs [idx :resource] orbit)]
             {[:planet-buy orbit sid]
              (-> state
                  (pay-to-market player price)
                  (update-in [:market-resources orbit] dec)
                  (assoc-in [:sundivers sid] divs')
                  (mark-activated sid)
                  (bump-cards 1))}))))

(defn- planet-build-city-actions [state player]
  (apply merge
         (for [[orbit sid] (:planets state)
               :let [no-city? (and (not (city-here? state sid))
                                   (zero? (get-in state [:market-cities orbit] 0))
                                   (pos? (get-in state [:players player :city-platforms] 0)))]
               :when no-city?
               diver (filter (fn [d] (and (= (:owner d) player)
                                          (:resource d)
                                          (not= (:resource d) orbit)))
                             (sundivers-at state sid))
               :when (not (already-activated? state sid))]
           (let [res-color (:resource diver)
                 [s _]     (remove-sundiver-by-pred
                             state sid
                             (fn [d] (and (= (:owner d) player)
                                          (= res-color (:resource d)))))
                 ;; Replenish that color's market row up to 5
                 replenish (fn [s]
                             (let [room (- market-resource-cap
                                           (get-in s [:market-resources orbit] 0))]
                               (if (pos? room)
                                 (update-in s [:market-resources orbit] + room)
                                 s)))]
             {[:planet-build-city orbit sid res-color]
              (-> s
                  (update-in [:players player :vaporized] inc)
                  (update-in [:market-cities orbit] inc)
                  (update-in [:players player :city-platforms] dec)
                  (assoc-in [:cities sid] {:color res-color :owner player})
                  replenish
                  (mark-activated sid)
                  (bump-cards 1))}))))

(defn- planet-activation-actions [state player]
  (let [acts (merge (planet-buy-resource-actions state player)
                    (planet-build-city-actions state player))]
    (assoc acts [:done-activating] (end-turn state))))

;; ---- CITY activation ----

(defn- enter-placing-links [state player city-sid]
  (let [color  (:color (city-at state city-sid))
        orbit  (board/orbit-of city-sid)
        ring-cities (get-in state [:market-cities orbit] 0)
        level  (max 1 (min 3 ring-cities))
        {:keys [base bonus]} (city-level-actions level)
        ;; ASSUMPTION: activator takes all actions (bonus negotiation skipped)
        n      (+ base bonus)
        ;; Return sundiver from this city to habitat
        [s _]  (remove-sundiver-by-pred state city-sid #(= (:owner %) player))
        s      (update-in s [:players player :habitat] inc)
        s      (-> s
                   (mark-activated city-sid)
                   (bump-cards level)
                   (assoc :phase :placing-links)
                   (assoc-in [:phase-data :links-remaining] n)
                   (assoc-in [:phase-data :city-being-activated] city-sid)
                   (assoc-in [:phase-data :city-color] color))]
    s))

(defn- city-activation-actions [state player]
  (let [acts
        (apply merge
               (for [[sid _] (:cities state)
                     :when (and (not (already-activated? state sid))
                                (player-has-sundiver? state sid player))]
                 {[:activate-city sid]
                  (enter-placing-links state player sid)}))]
    (assoc (or acts {}) [:done-activating] (end-turn state))))

(defn activating-actions [state]
  (let [player (current-player state)
        kind   (get-in state [:phase-data :activate-type])]
    (case kind
      :sun     (sun-activation-actions state player)
      :planets (planet-activation-actions state player)
      :cities  (city-activation-actions state player)
      {[:done-activating] (end-turn state)})))

;; ── PHASE: placing-links (city sub-phase) ─────────────────────────────────

(defn- player-links-touching [state player sid]
  (count (filter (fn [{:keys [a b owner]}]
                   (and (= owner player) (or (= a sid) (= b sid))))
                 (:links state))))

(defn- link-already-between? [state a b]
  (some (fn [{aa :a bb :b}]
          (or (and (= aa a) (= bb b))
              (and (= aa b) (= bb a))))
        (:links state)))

(defn- root-spaces-of-color
  "Roots: sun wedge of that color + any city of that color."
  [state color]
  (let [wedge-sid (board/sun-space (board/color->wedge color))
        city-sids (for [[sid c] (:cities state) :when (= color (:color c))] sid)]
    (set (cons wedge-sid city-sids))))

(defn- spaces-rooted-from-color
  "All spaces reachable from any root-of-color via this player's link tree."
  [state player color]
  (let [roots (root-spaces-of-color state color)
        owner-links (filter #(= player (:owner %)) (:links state))
        adj   (link-graph owner-links)]
    (loop [frontier (vec roots) seen #{}]
      (if (empty? frontier)
        (into seen roots)  ;; roots themselves are valid even if no links yet
        (let [n (first frontier) r (rest frontier)]
          (if (contains? seen n)
            (recur r seen)
            (recur (into (vec r) (get adj n #{})) (conj seen n))))))))

(defn- accessible-colors-for-player
  "Colors the player can pay access for THIS link: must be already-exhausted
   OR they have an active component to exhaust."
  [state player]
  (let [used (get-in state [:phase-data :exhausted-colors] #{})]
    (set
      (filter
        (fn [c]
          (let [k (board/color->wedge c)
                sn (:solar-network state)
                active (active-count sn k player)
                exh    (exhausted-count sn k player)]
            (or (contains? used c)
                (pos? active)
                (pos? exh))))
        board/orbits))))

(defn- pay-link-access
  "Pay access cost for a link of color c. If color hasn't been used this turn,
   exhaust one component of that color."
  [state player color]
  (let [used (get-in state [:phase-data :exhausted-colors] #{})]
    (if (contains? used color)
      state
      (let [k (board/color->wedge color)
            sn (:solar-network state)
            ;; prefer exhausting an active; otherwise reuse exhausted
            state (if (pos? (active-count sn k player))
                    (update state :solar-network exhaust-one k player)
                    state)]
        (update-in state [:phase-data :exhausted-colors] (fnil conj #{}) color)))))

(defn- place-one-link
  [state player origin dest color]
  (let [state (-> state
                  (spend-energy player 1)
                  (pay-link-access player color)
                  (update :links conj {:a origin :b dest :owner player :color color})
                  (update-in [:players player :links-supply] dec))
        remaining (dec (get-in state [:phase-data :links-remaining] 0))]
    (if (zero? remaining)
      (-> state
          (assoc :phase :activating)
          (update :phase-data dissoc :links-remaining :city-being-activated :city-color))
      (assoc-in state [:phase-data :links-remaining] remaining))))

(defn- skip-remaining-links [state]
  (-> state
      (assoc :phase :activating)
      (update :phase-data dissoc :links-remaining :city-being-activated :city-color)))

(defn placing-links-actions [state]
  (let [player (current-player state)
        adj    (get-in state [:board :adjacency])
        energy (get-in state [:players player :energy] 0)
        links-supply (get-in state [:players player :links-supply] 0)
        colors (accessible-colors-for-player state player)
        skip {[:skip-links] (skip-remaining-links state)}]
    (if (or (zero? energy) (zero? links-supply) (empty? colors))
      skip
      (let [acts (apply merge
                        (for [color colors
                              origin (spaces-rooted-from-color state player color)
                              :when (< (player-links-touching state player origin) 2)
                              dest (get adj origin #{})
                              :when (and (< (player-links-touching state player dest) 2)
                                         (not (link-already-between? state origin dest)))]
                          {[:link origin dest color]
                           (place-one-link state player origin dest color)}))]
        (merge skip (or acts {}))))))

;; ── Dispatch ───────────────────────────────────────────────────────────────

(defn legal-actions
  "Return {choice-key → next-state} for the current phase."
  [state]
  (case (:phase state)
    :place-mothership (place-mothership-actions state)
    :pre-action       (pre-action-actions state)
    :action           (action-actions state)
    :moving           (moving-actions state)
    :activating       (activating-actions state)
    :placing-links    (placing-links-actions state)
    :game-over        {}
    {}))

) ;; end (comment ...)
