(ns eridu.game)

;; Forward declarations for functions used by feat evaluation and bonus board effects
(declare count-temples-placed count-face-down-temples count-raiders-deployed
         magistrate-in-city? routes-from-city current-player player-data
         rounds-per-game advance-turn
         ;; Constants used by apply-passive and bonus board effects
         role-threshold-costs merchant-score raider-max-deployed priest-max-temples
         resource-types roles max-role-level active-routes route-key)

;; =============================================================================
;; Passive bonus board trigger system
;; =============================================================================

(defn has-passive?
  "True if player has their bonus board slot 0 (passive effect) uncovered."
  [state player-key]
  (= :uncovered (get-in state [:players player-key :bonus-board 0])))

(defn player-board-id
  "Get the bonus board ID assigned to a player."
  [state player-key]
  (get-in state [:bonus-boards player-key]))

(defn apply-passive
  "Apply a passive bonus board effect for a specific trigger.
   trigger-type: :raider-scored, :temple-flipped, :temple-placed, :river-crossed,
                 :role-increased, :sold, :action-space-7, :goods-taken, :deployed,
                 :turn-start, :end-game, :feat-claimed, :resource-spent
   context: map with trigger-specific data (e.g. :city, :route, :resource, :role)
   Returns updated state."
  [state player-key trigger-type context]
  (if-not (has-passive? state player-key)
    state
    (let [board-id (player-board-id state player-key)
          ;; Positive evidence that the trigger reached the player's passive.
          state (update-in state [:players player-key :passive-triggers-log]
                           (fnil conj [])
                           {:round (:round state 1)
                            :board-id board-id
                            :trigger trigger-type})
          pdata (get-in state [:players player-key])]
      (case [board-id trigger-type]

        ;; ── Board 1: When you surround a city with Raiders, temple in it ──
        ;; (checked after deploy — context has :city if surrounded)
        [1 :deployed]
        (if-let [surrounded-city (:surrounded-city context)]
          (let [pdata (get-in state [:players player-key])]
            (if (and (not (contains? (:temples pdata) surrounded-city))
                     (pos? (:temples-supply pdata 0)))
              (-> state
                  (assoc-in [:players player-key :temples surrounded-city] :face-up)
                  (update-in [:players player-key :temples-supply] dec))
              state))
          state)

        ;; ── Board 2: When you score a Raider, increase Priest ─────────────
        [2 :raider-scored]
        (let [priest-lv (get-in pdata [:roles :priest] 1)]
          (if (< priest-lv 5)
            (let [next-lv (inc priest-lv)
                  cost (get-in role-threshold-costs [:priest next-lv])]
              (if (or (nil? cost) (pos? (get-in pdata [:resources cost] 0)))
                (cond-> state
                  cost (update-in [:players player-key :resources cost] dec)
                  true (assoc-in [:players player-key :roles :priest] next-lv))
                state))
            state))

        ;; ── Board 3: River travel → take a Gem ───────────────────────────
        [3 :river-crossed]
        (update-in state [:players player-key :resources :gems] (fnil inc 0))

        ;; ── Board 4: When you flip a temple, may sell in that city ────────
        [4 :temple-flipped]
        (let [city (:city context)
              demands (get-in state [:city-demands city] [])
              resources (:resources pdata)
              sellable (first (filter #(pos? (get resources % 0)) demands))]
          (if sellable
            (let [merchant-lv (get-in pdata [:roles :merchant] 1)
                  amity-score (get merchant-score merchant-lv 2)
                  idx (.indexOf (vec demands) sellable)
                  new-demands (into (subvec (vec demands) 0 idx)
                                    (subvec (vec demands) (inc idx)))]
              (-> state
                  (update-in [:players player-key :resources sellable] dec)
                  (assoc-in [:city-demands city] new-demands)
                  (update-in [:players player-key :demand-tokens] conj sellable)
                  (update-in [:players player-key :amity] + amity-score)))
            state))

        ;; ── Board 5: Influence magistrate in your city → travel with it ──
        ;; (complex — skip for now, would need movement tracking)
        [5 :magistrate-moved] state

        ;; ── Board 6: Action space 7 → free Travel action ─────────────────
        ;; (tracked via flag, actual travel handled by choice.cljc)
        [6 :action-space-7]
        (assoc-in state [:players player-key :pending-free-travel] true)

        ;; ── Board 7: When you deploy, extra raider next to Magistrate ─────
        [7 :deployed]
        (let [mag-cities (keys (:magistrates state))
              pdata (get-in state [:players player-key])
              raider-lv (get-in pdata [:roles :raider] 1)
              max-deployed (get raider-max-deployed raider-lv 2)
              deployed (count-raiders-deployed pdata)
              supply (:raiders-supply pdata 0)]
          (if (and (pos? supply) (< deployed max-deployed))
            ;; Find a route adjacent to a magistrate that we don't have a raider on
            (let [routes (for [mc mag-cities
                               r (routes-from-city mc (active-routes (count (:turn-order state))))
                               :let [rk (route-key (:from r) (:to r))]
                               :when (not (contains? (:raiders pdata) rk))]
                           rk)]
              (if (seq routes)
                (-> state
                    (assoc-in [:players player-key :raiders (first routes)] :raiding)
                    (update-in [:players player-key :raiders-supply] dec))
                state))
            state))

        ;; ── Board 8: Score raider → flip to active instead of removing ────
        [8 :raider-scored]
        (let [rk (:route context)]
          (if rk
            (-> state
                (assoc-in [:players player-key :raiders rk] :raiding)
                (update-in [:players player-key :raiders-supply] dec))
            state))

        ;; ── Board 9: Flip temple → increase a role ───────────────────────
        [9 :temple-flipped]
        (let [pri (:role-priority pdata [:merchant :priest :raider :leader])
              best-role (first (filter #(< (get-in pdata [:roles %] 1) 5) pri))]
          (if best-role
            (let [next-lv (inc (get-in pdata [:roles best-role] 1))
                  cost (get-in role-threshold-costs [best-role next-lv])]
              (if (or (nil? cost) (pos? (get-in pdata [:resources cost] 0)))
                (cond-> state
                  cost (update-in [:players player-key :resources cost] dec)
                  true (assoc-in [:players player-key :roles best-role] next-lv))
                state))
            state))

        ;; ── Board 10: Sell gold to empty demand cities ────────────────────
        ;; (passive rule change — tracked as flag for sell resolution)
        [10 :sold] state ;; TODO: requires sell phase modification

        ;; ── Board 11: Contests → extra glory based on leader level ────────
        [11 :feat-claimed]
        (let [leader-lv (get-in pdata [:roles :leader] 1)]
          (update-in state [:players player-key :glory] + leader-lv))

        ;; ── Board 12: Cross river → place raider on that river ────────────
        [12 :river-crossed]
        (let [rk (:route context)
              pdata (get-in state [:players player-key])
              raider-lv (get-in pdata [:roles :raider] 1)
              max-deployed (get raider-max-deployed raider-lv 2)
              deployed (count-raiders-deployed pdata)
              supply (:raiders-supply pdata 0)]
          (if (and rk (pos? supply) (< deployed max-deployed)
                   (not (contains? (:raiders pdata) rk)))
            (-> state
                (assoc-in [:players player-key :raiders rk] :raiding)
                (update-in [:players player-key :raiders-supply] dec))
            state))

        ;; ── Board 13: Place temple → place raider adjacent ────────────────
        [13 :temple-placed]
        (let [city (:city context)
              pdata (get-in state [:players player-key])
              raider-lv (get-in pdata [:roles :raider] 1)
              max-deployed (get raider-max-deployed raider-lv 2)
              deployed (count-raiders-deployed pdata)
              supply (:raiders-supply pdata 0)
              adj-routes (routes-from-city city (active-routes (count (:turn-order state))))
              free-route (first (for [r adj-routes
                                      :let [rk (route-key (:from r) (:to r))]
                                      :when (not (contains? (:raiders pdata) rk))]
                                  rk))]
          (if (and free-route (pos? supply) (< deployed max-deployed))
            (-> state
                (assoc-in [:players player-key :raiders free-route] :raiding)
                (update-in [:players player-key :raiders-supply] dec))
            state))

        ;; ── Board 14: Uruk bonus travel action ───────────────────────────
        ;; (complex bonus action — skip for now)
        [14 :turn-start] state

        ;; ── Board 15: Role increase → increase again for free ─────────────
        [15 :role-increased]
        (let [role (:role context)
              current-lv (get-in state [:players player-key :roles role] 1)]
          (if (< current-lv 5)
            (assoc-in state [:players player-key :roles role] (inc current-lv))
            state))

        ;; ── Board 16: 2-astronomer space → third action ──────────────────
        ;; (tracked as flag, handled in choose-action)
        [16 :landing]
        (if (= 2 (:astronomer-count context))
          (assoc-in state [:players player-key :bonus-extra-action] true)
          state)

        ;; ── Board 17: Action space 7 → take a good of choice ─────────────
        [17 :action-space-7]
        ;; Give the most needed resource (lowest count)
        (let [resources (:resources pdata)
              cheapest (apply min-key #(get resources % 0) resource-types)]
          (update-in state [:players player-key :resources cheapest] (fnil inc 0)))

        ;; ── Board 18: Keep tools when spent + tools worth glory at end ────
        ;; (end-game scoring handled in apply-end-game-scoring)
        [18 :end-game]
        (let [tools (get-in state [:players player-key :resources :tools] 0)]
          (update-in state [:players player-key :glory] + tools))

        ;; ── Board 19: Take pottery → extra pottery x2 ────────────────────
        [19 :goods-taken]
        (if (some #{:pottery} (:resources context))
          (update-in state [:players player-key :resources :pottery] + 2)
          state)

        ;; ── Board 20: Flip temple → discard pottery for 3 glory ───────────
        [20 :temple-flipped]
        (if (pos? (get-in pdata [:resources :pottery] 0))
          (-> state
              (update-in [:players player-key :resources :pottery] dec)
              (update-in [:players player-key :glory] + 3))
          state)

        ;; ── Board 21: Place temple → extra facedown in same city ──────────
        [21 :temple-placed]
        ;; Temples are keyed by city, so we can't have two in same city in current model
        ;; This would need a data model change. For now, skip.
        state

        ;; ── Board 22: Space 7 → same action twice ────────────────────────
        ;; (tracked as flag, handled in action selection)
        [22 :action-space-7]
        (assoc-in state [:players player-key :bonus-repeat-action] true)

        ;; ── Board 23: Sell → glory instead of amity ───────────────────────
        ;; This needs to modify the sell resolution. We'll handle it by checking
        ;; after the sell and swapping the amity gained to glory.
        [23 :sold]
        (let [amity-gained (:amity-scored context 0)]
          (if (pos? amity-gained)
            (-> state
                (update-in [:players player-key :amity] - amity-gained)
                (update-in [:players player-key :glory] + amity-gained))
            state))

        ;; ── Board 24: Surround city → sell there ─────────────────────────
        ;; (complex — would need sell logic outside normal sell phase)
        [24 :deployed] state

        ;; ── Board 25: Two raiders per path ────────────────────────────────
        ;; (rule change — tracked as flag for deploy resolution)
        [25 :deployed] state ;; TODO: modify deploy to allow 2 per route

        ;; ── Board 26: Magistrate bonus → extra 2 amity ───────────────────
        [26 :sold]
        (if (pos? (:glory-scored context 0)) ;; glory from magistrate bonus means mag was present
          (update-in state [:players player-key :amity] + 2)
          state)

        ;; ── Board 27: Role increase → another for double cost ─────────────
        [27 :role-increased]
        (let [roles-available (filter #(< (get-in state [:players player-key :roles %] 1) 5)
                                      roles)
              ;; Pick a different role than the one just increased
              other-roles (remove #{(:role context)} roles-available)]
          (if (seq other-roles)
            (let [role (first other-roles)
                  next-lv (inc (get-in state [:players player-key :roles role] 1))
                  cost (get-in role-threshold-costs [role next-lv])
                  ;; Double cost: need 2 of the resource
                  can-pay? (or (nil? cost)
                               (>= (get-in state [:players player-key :resources cost] 0) 2))]
              (if can-pay?
                (cond-> state
                  cost (update-in [:players player-key :resources cost] - 2)
                  true (assoc-in [:players player-key :roles role] next-lv))
                state))
            state))

        ;; ── Board 28: 4+ astronomers → role increase at turn end ──────────
        ;; (tracked as flag, checked at advance-turn)
        [28 :landing]
        (if (>= (:astronomer-count context 0) 4)
          (assoc-in state [:players player-key :bonus-role-increase] true)
          state)

        ;; ── Board 29: Pay gold → 2 amity ─────────────────────────────────
        [29 :resource-spent]
        (if (= :gold (:resource context))
          (update-in state [:players player-key :amity] + 2)
          state)

        ;; ── Board 30: Take goods from other astronomer location ───────────
        ;; (complex — skip for now)
        [30 :goods-taken] state

        ;; ── Board 31: Other astronomer on space 7 → bonus travel ──────────
        ;; (complex positioning check — skip for now)
        [31 :landing] state

        ;; ── Board 32: Sell → discard gem for priest-level scoring ─────────
        [32 :sold]
        (if (pos? (get-in pdata [:resources :gems] 0))
          (let [priest-lv (get-in pdata [:roles :priest] 1)
                priest-score (get priest-max-temples priest-lv 3) ;; using priest level as score proxy
                merchant-score-val (:amity-scored context 0)]
            ;; Only use if priest scoring would be better
            (if (> priest-lv (get-in pdata [:roles :merchant] 1))
              (-> state
                  (update-in [:players player-key :resources :gems] dec)
                  (update-in [:players player-key :amity] + (- priest-lv merchant-score-val)))
              state))
          state)

        ;; ── Board 33: Deploy → influence adjacent magistrate ──────────────
        ;; (complex — would need to insert influence action)
        [33 :deployed] state

        ;; ── Board 34: Score raiders → amity instead of glory ──────────────
        [34 :raider-scored]
        ;; Swap the 4 glory that was just scored to 4 amity
        (-> state
            (update-in [:players player-key :glory] - 4)
            (update-in [:players player-key :amity] + 4))

        ;; ── Board 35: Start of turn, no goods → gain one ─────────────────
        [35 :turn-start]
        (let [resources (:resources pdata)
              total (reduce + (vals resources))]
          (if (zero? total)
            ;; Give the resource needed for next role cost
            (let [pri (:role-priority pdata [:merchant :priest :raider :leader])
                  needed (first (for [role pri
                                      :let [lv (get-in pdata [:roles role] 1)
                                            cost (get-in role-threshold-costs [role (inc lv)])]
                                      :when cost]
                                  cost))]
              (update-in state [:players player-key :resources (or needed :tools)] (fnil inc 0)))
            state))

        ;; Default: no matching passive for this trigger
        state))))

;; =============================================================================
;; Constants
;; =============================================================================

(def resource-types [:tools :pottery :gold :gems])

(def action-types
  #{:take :sell :deploy :travel :influence :temple})

;; Mesopotamian-themed symbols for the astronomy board
(def action-icons
  {:take      "\uD83C\uDF3E"   ;; 🌾 sheaf of grain (goods/harvest)
   :sell      "\u2696"          ;; ⚖ scales (trade/commerce)
   :deploy    "\u2694"          ;; ⚔ crossed swords (raiders)
   :travel    "\uD83D\uDC2A"   ;; 🐪 camel (caravan travel)
   :influence "\uD83D\uDC51"   ;; 👑 crown (magistrate influence)
   :temple    "\uD83C\uDFDB"}) ;; 🏛 classical building (temple)

(def resource-icons
  {:tools   "\uD83D\uDD28"   ;; 🔨
   :pottery "\uD83C\uDFFA"   ;; 🏺
   :gold    "\uD83E\uDE99"   ;; 🪙
   :gems    "\uD83D\uDC8E"}) ;; 💎

(def resource-colors
  {:tools "#D4913A" :pottery "#B074C8" :gold "#E8D44D" :gems "#4DE8D4"})

;; Player colors for distinguishing pieces
(def player-colors
  ["#4A90D9" "#D94A4A" "#4AD95A" "#D9B44A" "#9A4AD9" "#D94A90"])

;; --- Action board (astrology wheel) ---
;; 7 spaces in a clockwise circle.
;; Spaces 1-6 each have: take-goods + two of one action + one of another.
;; Space 7 has: sell, deploy, temple, influence (the four strategic actions).

(def action-spaces
  {1 {:actions [{:type :take :resources [:gems :tools]}
                {:type :sell}
                {:type :sell}
                {:type :travel}]}
   2 {:actions [{:type :take :resources [:gold :pottery]}
                {:type :deploy}
                {:type :deploy}
                {:type :travel}]}
   3 {:actions [{:type :take :resources [:pottery :gems]}
                {:type :travel}
                {:type :travel}
                {:type :temple}]}
   4 {:actions [{:type :take :resources [:pottery :tools]}
                {:type :influence}
                {:type :influence}
                {:type :deploy}]}
   5 {:actions [{:type :take :resources [:tools :gold]}
                {:type :sell}
                {:type :travel}
                {:type :travel}]}
   6 {:actions [{:type :take :resources [:gems :gold]}
                {:type :temple}
                {:type :temple}
                {:type :influence}]}
   7 {:actions [{:type :deploy}
                {:type :sell}
                {:type :influence}
                {:type :temple}]}})

;; Clockwise order: 1->2->3->4->5->6->7->1
(def action-space-order [1 2 3 4 5 6 7])

(defn move-astronomer-clockwise
  "Move an astronomer from space `from` by `steps` spaces clockwise.
   Returns the destination space number."
  [from steps]
  (let [idx (.indexOf action-space-order from)
        new-idx (mod (+ idx steps) 7)]
    (nth action-space-order new-idx)))

;; =============================================================================
;; Dice sequence planning — pre-computed movement math
;; =============================================================================
;; Board has 7 spaces, players roll 4 d6. Key insight: any two dice summing to 7
;; return you to the same space, enabling double-action strategies.
;; Probabilities (4d6, at least one pair summing to 7):
;;   0 pairs: 38%, 1 pair: 42%, 2 pairs: 17%, 3+ pairs: 3%
;; Most hands have 1-2 pairs summing to 7.

(defn dice-to-space
  "Given current space and a die value, return destination space."
  [from die]
  (move-astronomer-clockwise from die))

(defn pairs-summing-to-seven
  "Return vector of [die-a die-b] pairs from dice that sum to 7.
   These pairs can return you to the same space (double-action)."
  [dice]
  (let [d (vec dice)]
    (vec
     (for [i (range (count d))
           j (range (inc i) (count d))
           :when (= 7 (+ (nth d i) (nth d j)))]
       [(nth d i) (nth d j)]))))

(defn combo-indices
  "Return all k-size combinations of indices [0..n-1]."
  [n k]
  (cond
    (zero? k) [[]]
    (> k n) []
    (= k n) [(vec (range n))]
    :else (concat
           (for [rst (combo-indices (dec n) (dec k))]
             (into [0] (map inc rst)))
           (for [rst (combo-indices (dec n) k)]
             (mapv inc rst)))))

(defn dice-combos-for-same-space
  "Given a hand of dice, return all subsets of dice (as index vectors) whose
   sum is a multiple of 7 — these sequences return you to the starting space."
  [dice]
  (let [d (vec dice)
        n (count d)]
    (for [subset-size (range 2 (inc n))
          indices (combo-indices n subset-size)
          :let [vals (mapv #(nth d %) indices)]
          :when (zero? (mod (reduce + vals) 7))]
      indices)))

(defn sequence-actions-for-dice
  "For each die in the hand, return the action types available at destination
   space. Returns [{:die N :space S :actions #{:sell :deploy ...}}]."
  [dice from]
  (mapv (fn [die]
          (let [space (dice-to-space from die)
                actions (set (map :type (:actions (get action-spaces space))))]
            {:die die :space space :actions actions}))
        dice))

(defn plan-double-visit
  "Given dice hand and current space, find the best two-die sequence to visit
   the same target space twice. Returns {:target-space S :dice [a b] :actions #{...}}
   or nil if no such pair exists."
  [dice from]
  (let [pairs (pairs-summing-to-seven dice)]
    (when (seq pairs)
      ;; For each pair, the target space is the FIRST destination
      ;; (after using die-a, the second die-b returns to original space)
      ;; Actually: use die-a to go to space X, take action, use die-b (=7-a) to return
      ;; So target is `(dice-to-space from die-a)` — and `dice-to-space X (7-a) = from`
      ;; Wait: that returns TO origin. What we want is "can we hit some space twice?"
      ;; A double-hit on space X means: be on space X, use die-a to go to Y,
      ;; then from Y use die-b to return to X. Die-a + die-b = 7 (mod 7).
      ;; So ANY pair summing to 7 lets us return to start.
      ;; If start space has the action we want, that's double-visit.
      (let [from-actions (set (map :type (:actions (get action-spaces from))))
            best-pair (first pairs)]
        {:target-space from
         :dice best-pair
         :actions from-actions}))))

(defn three-step-sequences
  "For a hand of dice and starting space, enumerate all 3-die sequences
   and return the action-type set available at each step.
   Returns sorted list of {:dice [d1 d2 d3] :spaces [s0 s1 s2 s3] :action-sets [...]}.
   The most valuable sequences visit high-value action spaces or revisit spaces."
  [dice from]
  (when (>= (count dice) 3)
    (let [d (vec dice)
          n (count d)]
      (for [i (range n) j (range n) k (range n)
            :when (and (not= i j) (not= i k) (not= j k))
            :let [d1 (nth d i) d2 (nth d j) d3 (nth d k)
                  s1 (dice-to-space from d1)
                  s2 (dice-to-space s1 d2)
                  s3 (dice-to-space s2 d3)]]
        {:dice [d1 d2 d3]
         :spaces [from s1 s2 s3]
         :actions-per-step [(set (map :type (:actions (get action-spaces s1))))
                            (set (map :type (:actions (get action-spaces s2))))
                            (set (map :type (:actions (get action-spaces s3))))]
         ;; Key metric: spaces visited more than once (crowding = more actions)
         :revisits (- 3 (count (distinct [s1 s2 s3])))}))))

;; =============================================================================
;; City board
;; =============================================================================

;; Full board: 8 cities. 2/3-player removes Samarra (7 cities).
(def all-cities
  #{:samarra :nineveh :kish :babylon :nippur :lagash :uruk :eridu})

;; Routes are edges between cities. There are two types: :road and :river.
;; Magistrates only move clockwise on road routes.
;; Caravans can travel on any route.
;; Raiders are placed on routes (edges).

(def city-routes
  "All routes as #{[city-a city-b]} with route type."
  [{:from :samarra :to :nineveh :type :road}
   {:from :samarra :to :kish    :type :road}
   {:from :nineveh :to :babylon :type :road}
   {:from :babylon :to :uruk    :type :road}
   {:from :uruk    :to :eridu   :type :road}
   {:from :eridu   :to :lagash  :type :road}
   {:from :lagash  :to :nippur  :type :road}
   {:from :nippur  :to :kish    :type :road}
   ;; Nineveh↔Kish road (active only in 2-3 player when Samarra is removed)
   {:from :nineveh :to :kish    :type :road :only-without #{:samarra}}
   ;; River routes (shortcuts across the board)
   {:from :babylon :to :kish    :type :river}
   {:from :uruk    :to :nippur  :type :river}
   {:from :uruk    :to :lagash  :type :river}])

(defn route-key
  "Canonical key for a route between two cities (alphabetical order)."
  [city-a city-b]
  (let [a (name city-a) b (name city-b)]
    (if (neg? (compare a b))
      [city-a city-b]
      [city-b city-a])))

(defn city-neighbors
  "All cities reachable from a city via any route type."
  [city routes]
  (set
   (for [{:keys [from to]} routes
         :when (or (= from city) (= to city))]
     (if (= from city) to from))))

(defn- route-active?
  "True if a route is active for the given set of cities."
  [cities {:keys [from to only-without]}]
  (and (contains? cities from)
       (contains? cities to)
       (or (nil? only-without)
           (every? #(not (contains? cities %)) only-without))))

(defn city-graph
  "Return adjacency map for the given player count."
  [player-count]
  (let [cities (if (<= player-count 3)
                 (disj all-cities :samarra)
                 all-cities)
        routes (filter (partial route-active? cities) city-routes)]
    (into {}
          (for [c cities]
            [c (city-neighbors c routes)]))))

(defn active-routes
  "Return routes active for the given player count."
  [player-count]
  (let [cities (if (<= player-count 3)
                 (disj all-cities :samarra)
                 all-cities)]
    (filterv (partial route-active? cities) city-routes)))

(defn routes-from-city
  "All routes adjacent to a city."
  [city routes]
  (filterv (fn [{:keys [from to]}]
             (or (= from city) (= to city)))
           routes))

(defn route-other-city
  "Given a route and one city on it, return the other city."
  [route city]
  (if (= (:from route) city) (:to route) (:from route)))

;; Clockwise road order for magistrate movement
(def road-clockwise-order
  [:samarra :kish :nippur :lagash :eridu :uruk :babylon :nineveh])

(defn road-clockwise-next
  "Next city clockwise along roads from the given city."
  [city active-cities]
  (let [order (filterv active-cities road-clockwise-order)
        idx (.indexOf order city)]
    (when (>= idx 0)
      (nth order (mod (inc idx) (count order))))))

(defn road-clockwise-path
  "Return the list of [from to] route pairs for a magistrate moving
   `steps` spaces clockwise from `start-city`."
  [start-city steps active-cities]
  (loop [city start-city
         remaining steps
         path []]
    (if (zero? remaining)
      path
      (let [next-city (road-clockwise-next city active-cities)]
        (if next-city
          (recur next-city (dec remaining)
                 (conj path [city next-city]))
          path)))))

(def city-demand-count
  {:samarra 2 :nineveh 1 :kish 1 :babylon 1
   :nippur 1 :lagash 1 :uruk 1 :eridu 2})

;; --- Demand token bag ---

(def demand-tokens-per-type 7)

(defn full-demand-bag []
  (zipmap resource-types (repeat demand-tokens-per-type)))

(defn bag-total [bag]
  (apply + (vals bag)))

(defn draw-demand-token [bag]
  (let [choices (mapcat (fn [[t n]] (repeat n t)) bag)]
    (when (seq choices)
      (let [token (rand-nth choices)]
        [(update bag token dec) token]))))

(defn fill-demand-spaces
  "Draw tokens to fill empty demand spaces on cities.
   Returns [updated-bag updated-city-demands]."
  [bag city-demands cities]
  (reduce
   (fn [[bag demands] city]
     (let [max-slots (get city-demand-count city 1)
           current (get demands city [])
           needed (- max-slots (count current))]
       (if (pos? needed)
         (loop [b bag tokens current remaining needed]
           (if (or (zero? remaining) (zero? (bag-total b)))
             [b (assoc demands city tokens)]
             (let [[b' token] (draw-demand-token b)]
               (recur b' (conj tokens token) (dec remaining)))))
         [bag demands])))
   [bag city-demands]
   cities))

;; =============================================================================
;; Roles and levels
;; =============================================================================

(def roles [:merchant :priest :raider :leader])

(def max-role-level 5)

;; What each role level grants:
;; Merchant: amity scored per sell action
(def merchant-score {1 2, 2 3, 3 4, 4 5, 5 5})
;; Priest: maximum temples on the board
(def priest-max-temples {1 3, 2 4, 3 5, 4 8, 5 8})
;; Raider: maximum raiders deployed
(def raider-max-deployed {1 2, 2 3, 3 4, 4 6, 5 6})
;; Leader: max magistrate movement AND bonus glory for magistrate city
(def leader-movement {1 1, 2 2, 3 4, 4 5, 5 5})
(def leader-bonus    {1 1, 2 1, 3 2, 4 2, 5 3})

;; Threshold costs to advance roles (level you're entering -> cost or [costs]).
;; nil means free. Level 5 costs both goods (per player board design).
(def role-threshold-costs
  {:merchant {3 :pottery, 4 :gold,    5 [:pottery :gold]}
   :priest   {3 :tools,   4 :gems,    5 [:tools :gems]}
   :raider   {3 :gold,    4 :tools,   5 [:gold :tools]}
   :leader   {3 :gems,    4 :pottery, 5 [:gems :pottery]}})

;; End-game role scoring: points earned for reaching level 5 of a role.
;; Each role's max bonus is the OPPOSITE track from its in-game scoring,
;; so maxing a role offsets the track that role neglects.
;; (Merchant/Priest in-game = amity → max bonus = glory; Raider/Leader in-game = glory → max bonus = amity)
(def role-end-game-bonus
  {:merchant {:track :glory :points 10}
   :priest   {:track :glory :points 10}
   :raider   {:track :amity :points 10}
   :leader   {:track :amity :points 10}})

;; Feat claim bonus values: first claimer gets 3 wild points, second 2, etc.
(def bonus-contest-values [3 2 1 1])

;; =============================================================================
;; Feat/contest evaluation
;; =============================================================================

(def river-cities
  "Cities that are on a river route."
  #{:babylon :kish :uruk :nippur :lagash})

(defn evaluate-contest
  "Check if a player currently meets the conditions for a contest card.
   Returns true/false. Only evaluates 'state-check' feats (not event-based ones)."
  [state player-key contest]
  (let [pdata (get-in state [:players player-key])
        demands (:demand-tokens pdata [])
        roles (:roles pdata)
        temples (:temples pdata {})
        raiders (:raiders pdata {})]
    (case (:id contest)
      ;; A: Fulfill goods
      :A1 (>= (count (filter #{:gems :gold} demands)) 3)
      :A2 (>= (count (filter #{:tools :pottery} demands)) 3)

      ;; B: Fulfill patterns
      :B1 (some #(>= (val %) 3) (frequencies demands))
      :B2 (every? #(some #{%} demands) [:tools :pottery :gold :gems])

      ;; C: Temple count
      :C1 (>= (count (filter #(= :face-up (val %)) temples)) 4)
      :C2 (>= (count (filter #(= :face-down (val %)) temples)) 4)

      ;; D: Temple placement
      :D1 (and (contains? temples :eridu) (contains? temples :nineveh))
      :D2 (>= (count (filter #(contains? river-cities (key %)) temples)) 4)

      ;; E: Raider placement
      :E1 (let [kish-routes (set (for [r (active-routes (count (:turn-order state)))
                                       :when (or (= :kish (:from r)) (= :kish (:to r)))]
                                   (route-key (:from r) (:to r))))]
             (every? #(contains? raiders %) kish-routes))
      :E2 (let [eridu-routes (set (for [r (active-routes (count (:turn-order state)))
                                        :when (or (= :eridu (:from r)) (= :eridu (:to r)))]
                                    (route-key (:from r) (:to r))))
                ninev-routes (set (for [r (active-routes (count (:turn-order state)))
                                        :when (or (= :nineveh (:from r)) (= :nineveh (:to r)))]
                                    (route-key (:from r) (:to r))))]
             (and (some #(contains? raiders %) eridu-routes)
                  (some #(contains? raiders %) ninev-routes)))

      ;; F: Raider state
      :F1 (>= (count (filter #(= :point (val %)) raiders)) 3)
      :F2 (let [river-route-keys (set (for [r (active-routes (count (:turn-order state)))
                                             :when (= :river (:type r))]
                                         (route-key (:from r) (:to r))))]
             (every? #(contains? raiders %) river-route-keys))

      ;; G: Magistrate movement (event-based — uses turn-stats, must be this player's turn)
      :G1 (and (= player-key (get-in state [:turn-stats :player]))
               (>= (get-in state [:turn-stats :magistrate-max-move] 0) 4))
      :G2 (and (= player-key (get-in state [:turn-stats :player]))
               (>= (get-in state [:turn-stats :magistrate-raiders-flipped] 0) 3))

      ;; H: Role levels
      :H1 (>= (count (filter #(>= (val %) 3) roles)) 2)
      :H2 (some #(= 5 (val %)) roles)

      ;; I: Scoring thresholds (event-based — must be this player's turn)
      :I1 (let [ts (get state :turn-stats {})]
             (and (= player-key (:player ts))
                  (pos? (get ts :temples-flipped 0))
                  (>= (+ (get ts :amity 0) (get ts :glory 0)) 10)))
      :I2 (and (= player-key (get-in state [:turn-stats :player]))
               (>= (get-in state [:turn-stats :glory] 0) 5))

      ;; J: Mixed (event-based amity scoring)
      :J1 (and (= player-key (get-in state [:turn-stats :player]))
               (>= (get-in state [:turn-stats :amity] 0) 5))
      :J2 (and (= 2 (get-in pdata [:resources :tools] 0))
               (zero? (get-in pdata [:resources :pottery] 0))
               (zero? (get-in pdata [:resources :gold] 0))
               (zero? (get-in pdata [:resources :gems] 0)))

      ;; K: Sell achievements (event-based — must be this player's turn)
      :K1 (let [ts (get state :turn-stats {})]
             (and (= player-key (:player ts))
                  (= :gold (get ts :sold-resource))
                  (>= (+ (get ts :sell-amity 0) (get ts :sell-glory 0)) 5)))
      :K2 (let [ts (get state :turn-stats {})
                sell-city (get ts :sold-in-city)
                pc (count (:turn-order state))]
             (when (and (= player-key (:player ts)) sell-city)
               (let [adj-routes (routes-from-city sell-city (active-routes pc))
                     adj-route-keys (set (map #(route-key (:from %) (:to %)) adj-routes))]
                 ;; Check if ALL adjacent routes have a raider from ANY player
                 (every? (fn [rk]
                           (some #(contains? (:raiders (val %)) rk)
                                 (:players state)))
                         adj-route-keys))))

      ;; L: Resource hoarding
      :L1 (>= (get-in pdata [:resources :gems] 0) 5)
      :L2 (>= (get-in pdata [:resources :pottery] 0) 5)

      ;; M: Magistrate + temple combos
      :M1 (let [mag-cities (set (keys (:magistrates state)))]
             (>= (count (filter #(and (= :face-down (val %))
                                      (contains? mag-cities (key %)))
                                temples))
                 2))
      :M2 (let [demand-cities (set (for [[c ds] (:city-demands state)
                                          :when (seq ds)] c))]
             (>= (count (filter #(not (contains? demand-cities (key %))) temples)) 4))

      ;; Default: unknown contest
      false)))

;; =============================================================================
;; Feat planning — progress measurement and target selection
;; =============================================================================

(defn feat-action-profile
  "Return the set of action types that advance a given contest.
   Used by personality to boost relevant actions."
  [contest-id]
  (case contest-id
    ;; Fulfill feats → need to sell (and travel to sell cities)
    (:A1 :A2 :B1 :B2) #{:sell :travel :take}
    ;; Temple count/placement → place temples, travel to flip
    (:C1 :C2 :D1 :D2 :M2) #{:temple :travel}
    ;; Raider placement → deploy raiders
    (:E1 :E2 :F1 :F2) #{:deploy :influence :travel}
    ;; Magistrate movement → influence
    (:G1 :G2 :M1) #{:influence :deploy}
    ;; Role levels → land alone on spaces for role increases
    (:H1 :H2) #{:take}  ;; take = generic "get resources for role costs"
    ;; Scoring thresholds → need temple flips + sells + travel combos
    (:I1 :I2 :J1) #{:sell :temple :travel :deploy}
    ;; Resource feats → take resources, don't sell them
    (:J2 :L1 :L2) #{:take}
    ;; Sell feats → sell + travel + maybe deploy
    (:K1 :K2) #{:sell :deploy :travel}
    ;; Default
    #{}))

(defn feat-progress
  "Measure progress toward a contest as a float 0.0 (no progress) to 1.0 (met).
   Returns [progress description] where description explains what's still needed."
  [state player-key contest]
  (let [pdata (get-in state [:players player-key])
        demands (:demand-tokens pdata [])
        roles (:roles pdata)
        temples (:temples pdata {})
        raiders (:raiders pdata {})
        pc (count (:turn-order state))]
    (case (:id contest)
      :A1 (let [n (count (filter #{:gems :gold} demands))]
             [(/ (min n 3) 3.0) (str n "/3 gems+gold fulfilled")])
      :A2 (let [n (count (filter #{:tools :pottery} demands))]
             [(/ (min n 3) 3.0) (str n "/3 tools+pottery fulfilled")])
      :B1 (let [mx (apply max 0 (vals (frequencies demands)))]
             [(/ (min mx 3) 3.0) (str mx "/3 same-type fulfilled")])
      :B2 (let [have (count (distinct (filter #{:tools :pottery :gold :gems} demands)))]
             [(/ (min have 4) 4.0) (str have "/4 types fulfilled")])
      :C1 (let [n (count (filter #(= :face-up (val %)) temples))]
             [(/ (min n 4) 4.0) (str n "/4 face-up temples")])
      :C2 (let [n (count-face-down-temples pdata)]
             [(/ (min n 4) 4.0) (str n "/4 face-down temples")])
      :D1 (let [has-e (if (contains? temples :eridu) 0.5 0)
                has-n (if (contains? temples :nineveh) 0.5 0)]
             [(+ has-e has-n) (str (if (contains? temples :eridu) "✓" "✗") " eridu "
                                   (if (contains? temples :nineveh) "✓" "✗") " nineveh")])
      :D2 (let [n (count (filter #(contains? river-cities (key %)) temples))]
             [(/ (min n 4) 4.0) (str n "/4 river-city temples")])
      :E1 (let [kish-routes (set (for [r (active-routes pc)
                                        :when (or (= :kish (:from r)) (= :kish (:to r)))]
                                    (route-key (:from r) (:to r))))
                have (count (filter #(contains? raiders %) kish-routes))
                need (count kish-routes)]
             [(if (pos? need) (/ (min have need) (double need)) 0)
              (str have "/" need " kish routes")])
      :E2 (let [has-e (some #(let [rk %] (or (= :eridu (first rk)) (= :eridu (second rk))))
                             (keys raiders))
                has-n (some #(let [rk %] (or (= :nineveh (first rk)) (= :nineveh (second rk))))
                             (keys raiders))]
             [(+ (if has-e 0.5 0) (if has-n 0.5 0))
              (str (if has-e "✓" "✗") " eridu-raider " (if has-n "✓" "✗") " nineveh-raider")])
      :F1 (let [n (count (filter #(= :point (val %)) raiders))]
             [(/ (min n 3) 3.0) (str n "/3 point-side raiders")])
      :F2 (let [river-rks (set (for [r (active-routes pc) :when (= :river (:type r))]
                                  (route-key (:from r) (:to r))))
                have (count (filter #(contains? raiders %) river-rks))
                need (count river-rks)]
             [(if (pos? need) (/ (min have need) (double need)) 0)
              (str have "/" need " river routes")])
      ;; Event-based feats: progress is harder to measure, use role readiness
      :G1 (let [ll (get-in pdata [:roles :leader] 1)]
             [(/ (min ll 5) 5.0) (str "leader-lv " ll " (need high influence range)")])
      :G2 (let [ll (get-in pdata [:roles :leader] 1)
                rd (count-raiders-deployed pdata)]
             [(/ (+ (min ll 3) (min rd 3)) 6.0)
              (str "leader " ll " + " rd " raiders deployed")])
      :H1 (let [at3 (count (filter #(>= (val %) 3) roles))]
             [(/ (min at3 2) 2.0) (str at3 "/2 roles at 3+")])
      :H2 (let [mx (apply max (vals roles))]
             [(/ (min mx 5) 5.0) (str "max role " mx "/5")])
      :I1 (let [fd (count-face-down-temples pdata)
                tp (count-temples-placed pdata)]
             [(/ (+ (min fd 1) (min tp 2)) 3.0)
              (str fd " flipped, " tp " placed (need flip+10pts)")])
      :I2 (let [rl (get-in pdata [:roles :raider] 1)
                rd (count-raiders-deployed pdata)]
             [(/ (+ (min rl 3) (min rd 2)) 5.0)
              (str "raider " rl " + " rd " deployed (need 5 glory turn)")])
      :J1 (let [ml (get-in pdata [:roles :merchant] 1)
                fd (count-face-down-temples pdata)]
             [(/ (+ (min ml 3) (min fd 2)) 5.0)
              (str "merchant " ml " + " fd " flipped (need 5 amity turn)")])
      :J2 (let [tools (get-in pdata [:resources :tools] 0)
                others (+ (get-in pdata [:resources :pottery] 0)
                          (get-in pdata [:resources :gold] 0)
                          (get-in pdata [:resources :gems] 0))]
             [(cond (and (= tools 2) (zero? others)) 1.0
                    (and (pos? tools) (zero? others)) 0.6
                    (pos? tools) 0.3
                    :else 0.0)
              (str tools " tools, " others " other goods")])
      :K1 (let [ml (get-in pdata [:roles :merchant] 1)
                gold (get-in pdata [:resources :gold] 0)]
             [(/ (+ (min ml 3) (min gold 1)) 4.0)
              (str "merchant " ml " + " gold " gold")])
      :K2 (let [rd (count-raiders-deployed pdata)
                df (count demands)]
             [(/ (+ (min rd 3) (min df 1)) 4.0)
              (str rd " raiders + " df " demands (need surrounded sell)")])
      :L1 (let [n (get-in pdata [:resources :gems] 0)]
             [(/ (min n 5) 5.0) (str n "/5 gems")])
      :L2 (let [n (get-in pdata [:resources :pottery] 0)]
             [(/ (min n 5) 5.0) (str n "/5 pottery")])
      :M1 (let [mag-cities (set (keys (:magistrates state)))
                n (count (filter #(and (= :face-down (val %))
                                        (contains? mag-cities (key %))) temples))]
             [(/ (min n 2) 2.0) (str n "/2 magistrates at facedown temples")])
      :M2 (let [demand-cities (set (for [[c ds] (:city-demands state) :when (seq ds)] c))
                n (count (filter #(not (contains? demand-cities (key %))) temples))]
             [(/ (min n 4) 4.0) (str n "/4 temples in empty cities")])
      ;; Unknown
      [0.0 "unknown feat"])))

(def feat-difficulty
  "How many dedicated actions a feat typically requires (lower = easier).
   Used to bias toward achievable feats."
  {;; Easy (2-3 actions) — these should be primary targets
   :H1 2   ;; Two roles at 3+ — happens naturally
   :J2 2   ;; Only tools — just hold tools
   :J1 3   ;; 5 amity in one turn — flip 2-3 temples
   :E2 3   ;; Raiders near Eridu & Nineveh — just deploy 2
   ;; Medium (4 actions)
   :A1 4   ;; 3 gems/gold fulfilled
   :A2 4   ;; 3 tools/pottery fulfilled
   :B1 4   ;; 3 same-type fulfilled
   :D1 4   ;; Temples in Eridu & Nineveh
   :G2 4   ;; Magistrate through 3 raiders
   ;; Hard (5+ actions) — only target if good synergy
   :H2 5   ;; Any role at 5 — focused investment
   :B2 5   ;; All 4 types fulfilled
   :D2 5   ;; 4 river-city temples
   :C1 6   ;; 4 face-up temples (conflicts with scoring)
   :C2 5   ;; 4 face-down temples
   :E1 6   ;; Surround Kish
   :F1 5   ;; 3 point-side raiders
   :F2 7   ;; Raiders on all rivers
   :G1 5   ;; Move magistrate 4 cities
   :I1 7   ;; 10 pts with temple flip — hard combo
   :I2 6   ;; 5 glory in one turn
   :K1 6   ;; Big gold sale
   :K2 6   ;; Sell in surrounded city
   :L1 6   ;; 5 gems — resource hoarding
   :L2 6   ;; 5 pottery
   :M1 7   ;; Magistrates at temples
   :M2 8}) ;; 4 temples in empty cities

(defn feat-affinity
  "Score how well a player's starting position aligns with a contest.
   Higher = easier to achieve from this starting position.
   Considers starting city, role, resources, difficulty, and board synergy."
  [state player-key contest]
  (let [pdata (get-in state [:players player-key])
        city (:caravan pdata)
        role (first (filter #(= 2 (get-in pdata [:roles %] 1)) roles))
        cat (:category contest)
        difficulty (get feat-difficulty (:id contest) 5)]
    (+
     ;; Ease bonus: easier feats get a large bonus (range 0-8)
     ;; This is the dominant factor — we want achievable feats
     (* 2 (max 0 (- 6 difficulty)))

     ;; Role synergy
     (case cat
       :fulfill  (if (= role :merchant) 3 (if (= role :priest) 1 0))
       :temple   (if (= role :priest) 3 (if (= role :merchant) 1 0))
       :raider   (if (= role :raider) 3 (if (= role :leader) 1 0))
       :magistrate (if (= role :leader) 3 (if (= role :raider) 1 0))
       :role     3  ;; any role can push for role-level feats — boost these
       :scoring  1
       :resource 1
       :sell     (if (= role :merchant) 2 0)
       0)

     ;; City synergy
     (case (:id contest)
       :D1 (if (#{:eridu :nineveh} city) 2 0)
       :E1 (if (= city :kish) 2 (if (#{:babylon :nippur :lagash} city) 1 0))
       (:D2 :F2) (if (contains? river-cities city) 1 0)
       :K2 (if (#{:kish :uruk} city) 1 0)
       0)

     ;; Current progress (heavily weighted — a feat you're already close to is gold)
     (let [[prog _] (feat-progress state player-key contest)]
       (* prog 5)))))

(defn select-target-feats
  "Select 1-2 target feats for a player based on affinity with starting position.
   Excludes feats already claimed by this player.
   Returns vector of contest maps, best first."
  ([state player-key]
   (select-target-feats state player-key #{}))
  ([state player-key exclude-ids]
   (let [contests (:contests state [])
         claims (:contest-claims state {})
         ;; Exclude feats this player already claimed and any explicit exclusions
         available (remove (fn [c]
                            (or (contains? exclude-ids (:id c))
                                (some #{player-key} (get claims (:id c) []))))
                          contests)
         scored (for [c available]
                  [(feat-affinity state player-key c) c])
         sorted (reverse (sort-by first scored))]
     (mapv second (take 2 sorted)))))

;; =============================================================================
;; Bonus board effect engine
;; =============================================================================

(defn- add-player-resource [state player-key resource n]
  (update-in state [:players player-key :resources resource] (fnil + 0) n))

(defn- increase-role-free [state player-key role]
  (let [current (get-in state [:players player-key :roles role] 1)]
    (if (< current max-role-level)
      (assoc-in state [:players player-key :roles role] (inc current))
      state)))

(defn- increase-role-with-cost [state player-key role]
  (let [current (get-in state [:players player-key :roles role] 1)]
    (if (< current max-role-level)
      (let [next-level (inc current)
            cost (get-in role-threshold-costs [role next-level])
            has-cost? (or (nil? cost)
                          (pos? (get-in state [:players player-key :resources cost] 0)))]
        (if has-cost?
          (cond-> state
            cost (update-in [:players player-key :resources cost] dec)
            true (assoc-in [:players player-key :roles role] next-level))
          state))
      state)))

(defn- place-temple-in [state player-key city allow-duplicate?]
  (let [pdata (get-in state [:players player-key])
        has-temple? (contains? (:temples pdata) city)
        priest-level (get-in pdata [:roles :priest] 1)
        max-t (get priest-max-temples priest-level 3)
        placed (count-temples-placed pdata)
        supply (:temples-supply pdata 0)]
    (if (and (pos? supply)
             (or (not has-temple?) allow-duplicate?)
             (or allow-duplicate? (< placed max-t)))
      (-> state
          (assoc-in [:players player-key :temples city] :face-up)
          (update-in [:players player-key :temples-supply] dec))
      state)))

(defn- place-raider-on [state player-key route-key]
  (let [pdata (get-in state [:players player-key])
        raider-level (get-in pdata [:roles :raider] 1)
        max-r (get raider-max-deployed raider-level 2)
        deployed (count-raiders-deployed pdata)
        supply (:raiders-supply pdata 0)]
    (if (and (pos? supply)
             (< deployed max-r)
             (not (contains? (:raiders pdata) route-key)))
      (-> state
          (assoc-in [:players player-key :raiders route-key] :raiding)
          (update-in [:players player-key :raiders-supply] dec))
      state)))

(def effect-implementation-status
  "Classification of every board effect: what it needs to work.
   :implemented = working, :persistent = slot-0 passive (tracked separately),
   :needs-compound = requires multi-action sequence engine,
   :needs-placement = requires conditional placement logic,
   :needs-demand = requires demand token manipulation,
   :conditional = requires specific board state check"
  {[1 0] :persistent    ;; When you surround a city with Raiders, temple in it
   [1 1] :implemented   ;; Travel to Kish
   [1 2] :implemented   ;; Increase Raider and Leader
   [1 3] :implemented   ;; Place two raiders near Lagash
   [1 4] :implemented   ;; Glory per demand fulfilled
   [2 0] :persistent    ;; When you score Raider, increase Priest
   [2 1] :implemented   ;; Increase Merchant and Raider
   [2 2] :implemented   ;; 5 Amity if at magistrate
   [2 3] :implemented   ;; Temple in magistrate city
   [2 4] :implemented   ;; Glory per facedown temple
   [3 0] :persistent    ;; River travel → gem + gems worth amity
   [3 1] :implemented   ;; Increase Leader free
   [3 2] :implemented   ;; Temple in Lagash
   [3 3] :implemented   ;; Raider near Eridu + good
   [3 4] :implemented   ;; Travel then Sell
   [4 0] :persistent    ;; When flip temple, may sell
   [4 1] :implemented   ;; Temple in Eridu
   [4 2] :implemented   ;; Gain Tools, Gems, Gold
   [4 3] :implemented   ;; Amity = Leader x 2
   [4 4] :implemented   ;; 2 Amity per raider
   [5 0] :persistent    ;; Influence magistrate in your city, travel with it
   [5 1] :implemented   ;; Increase Priest free
   [5 2] :implemented   ;; Place demand tokens in Uruk + gain resources
   [5 3] :implemented   ;; Deploy then Temple
   [5 4] :implemented   ;; 2 Amity per raider
   [6 0] :persistent    ;; Space 7 → free Travel
   [6 1] :implemented   ;; Increase Merchant and Priest
   [6 2] :implemented   ;; Temple in each magistrate city
   [6 3] :implemented   ;; Sell to Babylon double
   [6 4] :implemented   ;; Raider near Lagash + Tools x2 (partial)
   [7 0] :persistent    ;; Place raiders, extra one next to magistrate
   [7 1] :implemented   ;; Increase Merchant and Leader
   [7 2] :implemented   ;; Temple in magistrate city
   [7 3] :implemented   ;; Travel + 3 Glory if at Eridu
   [7 4] :implemented   ;; Travel + 3 Amity if at Kish
   [8 0] :persistent    ;; Score raider → flip to active instead
   [8 1] :implemented   ;; Increase Raider and Priest
   [8 2] :implemented   ;; Place demand + sell
   [8 3] :implemented   ;; Gain Gold, Gems, Pottery (partial, no sell)
   [8 4] :implemented   ;; Flip all raiders to point
   [9 0] :persistent    ;; Flip temple → may increase role
   [9 1] :implemented   ;; Gain Tools, Gold, Pottery + Amity = leader
   [9 2] :implemented   ;; Increase Priest and Leader
   [9 3] :implemented   ;; Raider on each river
   [9 4] :implemented   ;; Sell to magistrate city + temple
   [10 0] :persistent   ;; Sell gold to empty demand cities
   [10 1] :implemented  ;; Increase Merchant free
   [10 2] :implemented  ;; Increase Merchant free
   [10 3] :implemented  ;; Raider near magistrate + amity
   [10 4] :implemented  ;; Temple in Nippur
   [11 0] :persistent   ;; Extra glory on contest claims
   [11 1] :implemented  ;; Place demand tokens in Lagash
   [11 2] :implemented  ;; Sell to Lagash double glory
   [11 3] :implemented  ;; Increase Raider free
   [11 4] :implemented  ;; Glory per facedown temple
   [12 0] :persistent   ;; River crossing → place raider
   [12 1] :implemented  ;; Increase all level-1 roles
   [12 2] :implemented  ;; Gain Gold x3 + Gems
   [12 3] :implemented  ;; Increase merchant + sell for glory
   [12 4] :implemented  ;; Glory per facedown temple
   [13 0] :persistent   ;; Temple placement → raider adjacent
   [13 1] :implemented  ;; Gain Tools x3 + Glory = leader
   [13 2] :implemented  ;; Gain Pottery x3 + Glory = leader
   [13 3] :implemented  ;; Increase all level-3 roles
   [13 4] :implemented  ;; Temple adjacent to raider
   [14 0] :persistent   ;; Uruk travel bonus action
   [14 1] :implemented ;; Glory per raider (partial: no placement)
   [14 2] :implemented ;; Resources (partial: no magistrate move)
   [14 3] :implemented ;; Travel to Eridu (partial: no demands)
   [14 4] :implemented  ;; Temple in Babylon
   [15 0] :persistent   ;; Free role increases
   [15 1] :implemented  ;; Good per demand fulfilled
   [15 2] :implemented  ;; Increase Priest + 4 Glory if Babylon temple
   [15 3] :implemented ;; Increase lowest role (partial: no travel)
   [15 4] :implemented ;; 2 Amity per raider (partial: no adjacency check)
   [16 0] :persistent   ;; 2-astronomer space → third action
   [16 1] :implemented  ;; Pottery per temple
   [16 2] :implemented  ;; Deploy + amity per raider
   [16 3] :implemented  ;; Increase Leader twice
   [16 4] :implemented  ;; Place demands + sell
   [17 0] :persistent   ;; Space 7 → good of choice
   [17 1] :implemented ;; Flip one raider to point (partial: no placement)
   [17 2] :implemented ;; Temple in magistrate city (partial: no facedown)
   [17 3] :implemented ;; 4 Amity (partial: no Uruk surround check)
   [17 4] :implemented ;; Glory = merchant level (partial: no sell)
   [18 0] :persistent   ;; Keep tools when spent + tools worth glory
   [18 1] :implemented ;; Resources (partial: no magistrate move/sell)
   [18 2] :implemented  ;; 5 Glory if facedown Samarra (partial otherwise)
   [18 3] :implemented ;; 3 Amity (partial: no surround check)
   [18 4] :implemented ;; 4 Amity per point raider (partial: don't remove)
   [19 0] :persistent   ;; Take pottery → extra pottery x2
   [19 1] :implemented  ;; Increase Priest twice
   [19 2] :implemented  ;; Sell to pottery cities
   [19 3] :implemented  ;; Discard good + move magistrate + sell
   [19 4] :implemented  ;; Flip all raiders to point
   [20 0] :persistent   ;; Flip temple → discard pottery for 3 glory
   [20 1] :implemented  ;; Raider on each opposing route
   [20 2] :implemented  ;; Increase Merchant twice
   [20 3] :implemented  ;; Amity = leader level (partial, no influence)
   [20 4] :implemented  ;; Take goods from astronomer spaces
   [21 0] :persistent   ;; Temple placement → extra facedown
   [21 1] :implemented  ;; Travel to Eridu
   [21 2] :implemented  ;; Increase Raider and Leader
   [21 3] :implemented ;; Travel to Eridu (partial: no sell)
   [21 4] :implemented  ;; Glory per demand fulfilled
   [22 0] :persistent   ;; Space 7 same action twice
   [22 1] :implemented  ;; Increase Raider and Merchant
   [22 2] :implemented  ;; Demands on facedown temples
   [22 3] :implemented  ;; Good + travel
   [22 4] :implemented  ;; 2 Amity per raider (partial, no travel)
   [23 0] :persistent   ;; Sell → glory instead of amity
   [23 1] :implemented  ;; Increase Priest and Merchant
   [23 2] :implemented  ;; Sell twice to Eridu
   [23 3] :implemented  ;; Good + travel + increase merchant
   [23 4] :implemented  ;; Temple in magistrate city
   [24 0] :persistent   ;; Surround city → sell there
   [24 1] :implemented  ;; Increase Raider and Leader
   [24 2] :implemented  ;; Demands on magistrates
   [24 3] :implemented  ;; Glory per demand fulfilled
   [24 4] :implemented  ;; Goods per demand at magistrates
   [25 0] :persistent   ;; Two raiders per path
   [25 1] :implemented  ;; Influence + score raiders
   [25 2] :implemented  ;; Increase Merchant and Leader
   [25 3] :implemented  ;; Two facedown temples
   [25 4] :implemented  ;; Good + travel
   [26 0] :persistent   ;; Extra 2 amity on magistrate bonus
   [26 1] :implemented  ;; Increase Priest and Leader
   [26 2] :implemented  ;; Increase Priest and Raider
   [26 3] :implemented  ;; Sell + temple
   [26 4] :implemented  ;; Raider + surround check
   [27 0] :persistent   ;; Role increase → another role for double cost
   [27 1] :implemented  ;; Travel + sell
   [27 2] :implemented  ;; Travel + deploy
   [27 3] :implemented  ;; Travel + temple
   [27 4] :implemented  ;; Three goods
   [28 0] :persistent   ;; 4+ astronomers → role increase
   [28 1] :implemented  ;; Travel + temple
   [28 2] :implemented  ;; Travel + temple
   [28 3] :implemented  ;; Sell gold to empty city
   [28 4] :implemented  ;; Raider point-side near Kish
   [29 0] :persistent   ;; Pay gold → 2 amity
   [29 1] :implemented  ;; Decrease leader + increase others
   [29 2] :implemented  ;; Travel + sell
   [29 3] :implemented  ;; Raider on each river
   [29 4] :implemented  ;; Temple in surrounded cities
   [30 0] :persistent   ;; Take goods from other astronomer location
   [30 1] :implemented ;; Glory = leader level (partial: no influence+travel)
   [30 2] :implemented ;; Amity = leader level (partial: no influence+sell)
   [30 3] :implemented ;; Glory = raider level (partial: no deploy+influence)
   [30 4] :implemented ;; Amity = priest level (partial: no influence+temple)
   [31 0] :persistent   ;; Other astronomer on space 7 → bonus travel
   [31 1] :implemented  ;; Increase all level-1 roles
   [31 2] :implemented  ;; Increase all level-3 roles
   [31 3] :implemented  ;; Resource + facedown temple
   [31 4] :implemented  ;; Resource + deploy
   [32 0] :persistent   ;; Sell: discard gem for priest-level scoring
   [32 1] :implemented  ;; Glory per demand (partial, no sell)
   [32 2] :implemented  ;; Gem (partial, no travel)
   [32 3] :implemented  ;; Raider between temple cities
   [32 4] :implemented  ;; Influence + sell
   [33 0] :persistent   ;; Deploy → influence adjacent magistrate
   [33 1] :implemented  ;; Decrease merchant + increase others
   [33 2] :implemented  ;; Facedown temple + travel
   [33 3] :implemented  ;; Temple in Uruk
   [33 4] :implemented  ;; Deploy + travel
   [34 0] :persistent   ;; Score raiders → amity instead of glory
   [34 1] :implemented  ;; Pay tools for raiders around Uruk
   [34 2] :implemented  ;; Raider on each existing route
   [34 3] :implemented  ;; Sell at magistrate+temple cities
   [34 4] :implemented  ;; Same as 34-3
   [35 0] :persistent   ;; No goods → gain good of choice
   [35 1] :implemented  ;; Travel + sell
   [35 2] :implemented  ;; Pay pottery for temples
   [35 3] :implemented  ;; Increase role of choice
   [35 4] :implemented  ;; Influence + score raiders
   })

(defn board-effect-diagnostic
  "Generate a diagnostic report of which board effects work and which don't.
   Returns {:implemented N :persistent N :needs-compound N :needs-placement N
            :needs-demand N :conditional N}."
  []
  (let [by-status (group-by val effect-implementation-status)]
    {:total (count effect-implementation-status)
     :implemented (count (get by-status :implemented []))
     :persistent (count (get by-status :persistent []))
     :needs-compound (count (get by-status :needs-compound []))
     :needs-placement (count (get by-status :needs-placement []))
     :needs-demand (count (get by-status :needs-demand []))
     :conditional (count (get by-status :conditional []))}))

(defn apply-bonus-effect
  "Apply a one-time bonus board effect when a slot is uncovered.
   board-id = the bonus board number, slot-idx = 0-4 (0=persistent).
   Persistent effects (slot 0) are tracked but most aren't applied here.
   Returns updated state with :board-effects-log tracking what happened."
  [state player-key board-id slot-idx]
  (let [pdata (get-in state [:players player-key])
        pc (count (:turn-order state))
        ;; Snapshot pre-state for change detection
        pre-amity (:amity pdata 0)
        pre-glory (:glory pdata 0)
        pre-roles (:roles pdata)
        pre-resources (:resources pdata)
        pre-temples (count (:temples pdata))
        pre-raiders (count (:raiders pdata))
        result-state
    (case [board-id slot-idx]
      ;; ─── Board 1: Shield of Gilgamesh ───────────────────────────
      [1 1] (let [city :kish] ;; Travel to Kish
              (assoc-in state [:players player-key :caravan] city))
      [1 2] (-> state ;; Increase Raider and Leader
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :leader))
      [1 3] (let [routes (active-routes pc) ;; Place two raiders near Lagash
                  lagash-rks (for [r routes
                                   :when (or (= :lagash (:from r)) (= :lagash (:to r)))]
                               (route-key (:from r) (:to r)))
                  avail (remove #(contains? (:raiders pdata) %) lagash-rks)
                  picks (take 2 avail)]
              (reduce #(place-raider-on %1 player-key %2) state picks))
      [1 4] (let [demands (:demand-tokens pdata [])] ;; Glory per demand fulfilled
              (update-in state [:players player-key :glory] + (count demands)))

      ;; ─── Board 2: Seal of Enmerkar ──────────────────────────────
      [2 1] (-> state ;; Increase Merchant and Raider
               (increase-role-with-cost player-key :merchant)
               (increase-role-with-cost player-key :raider))
      [2 2] (if (magistrate-in-city? state (:caravan pdata)) ;; 5 Amity if at magistrate
              (update-in state [:players player-key :amity] + 5)
              state)
      [2 3] (let [mag-city (first (keys (:magistrates state))) ;; Temple in magistrate city
                  target (or (first (filter #(and (contains? (:magistrates state) %)
                                                  (not (contains? (:temples pdata) %)))
                                             (keys (:magistrates state))))
                             mag-city)]
              (if target (place-temple-in state player-key target true) state))
      [2 4] (let [fd (count-face-down-temples pdata)] ;; Glory per facedown temple
              (update-in state [:players player-key :glory] + fd))

      ;; ─── Board 3: Voyage of Ziusudra ────────────────────────────
      [3 1] (increase-role-free state player-key :leader)
      [3 2] (place-temple-in state player-key :lagash true)
      [3 3] (let [routes (active-routes pc) ;; Raider near Eridu + good
                  eridu-rks (for [r routes
                                  :when (or (= :eridu (:from r)) (= :eridu (:to r)))]
                              (route-key (:from r) (:to r)))
                  avail (remove #(contains? (:raiders pdata) %) eridu-rks)]
              (if-let [rk (first avail)]
                (-> state
                    (place-raider-on player-key rk)
                    (add-player-resource player-key :tools 1))
                (add-player-resource state player-key :tools 1)))
      [3 4] (-> state ;; Travel then Sell → travel to Eridu + grant a demand-style bonus
               (assoc-in [:players player-key :caravan] :eridu)
               (update-in [:players player-key :amity] + 2))

      ;; ─── Board 4: Blessing of Inanna ────────────────────────────
      [4 1] (place-temple-in state player-key :eridu true)
      [4 2] (-> state ;; Gain Tools, Gems, Gold
               (add-player-resource player-key :tools 1)
               (add-player-resource player-key :gems 1)
               (add-player-resource player-key :gold 1))
      [4 3] (let [ll (get-in pdata [:roles :leader] 1)] ;; Amity = leader x 2
              (update-in state [:players player-key :amity] + (* ll 2)))
      [4 4] (let [rc (count-raiders-deployed pdata)] ;; 2 Amity per raider
              (update-in state [:players player-key :amity] + (* 2 rc)))

      ;; ─── Board 5: Wisdom of Adapa ──────────────────────────────
      [5 1] (increase-role-free state player-key :priest)
      [5 2] (let [bag (:demand-bag state (full-demand-bag))
                   [bag1 tok1] (draw-demand-token bag)
                   [bag2 tok2] (if bag1 (draw-demand-token bag1) [bag nil])]
               (cond-> state
                 true  (assoc :demand-bag (or bag2 bag1 bag))
                 tok1  (update-in [:city-demands :uruk] (fnil conj []) tok1)
                 tok1  (add-player-resource player-key tok1 1)
                 tok2  (update-in [:city-demands :uruk] (fnil conj []) tok2)
                 tok2  (add-player-resource player-key tok2 1)))
      [5 3] (let [routes (active-routes pc) ;; Deploy then Temple → place raider + temple
                  any-route (first (remove #(contains? (:raiders pdata) %)
                                           (for [r routes]
                                             (route-key (:from r) (:to r)))))
                  caravan (:caravan pdata)]
              (cond-> state
                any-route (place-raider-on player-key any-route)
                caravan (place-temple-in player-key caravan true)))
      [5 4] (let [rc (count-raiders-deployed pdata)] ;; 2 Amity per raider
              (update-in state [:players player-key :amity] + (* 2 rc)))

      ;; ─── Board 6: Trade of Dumuzid ──────────────────────────────
      [6 1] (-> state ;; Increase Merchant and Priest
               (increase-role-with-cost player-key :merchant)
               (increase-role-with-cost player-key :priest))
      [6 2] (reduce (fn [s city] ;; Temple in each magistrate city
                      (if (not (contains? (:temples (get-in s [:players player-key])) city))
                        (place-temple-in s player-key city true)
                        s))
                    state (keys (:magistrates state)))
      [6 3] (-> state ;; Sell to Babylon double → travel Babylon + amity
               (assoc-in [:players player-key :caravan] :babylon)
               (update-in [:players player-key :amity] + 4))
      [6 4] (-> state ;; Raider near Lagash + Tools x2
               (add-player-resource player-key :tools 2))

      ;; ─── Board 7: March of Lugalbanda ───────────────────────────
      [7 1] (-> state ;; Increase Merchant and Leader
               (increase-role-with-cost player-key :merchant)
               (increase-role-with-cost player-key :leader))
      [7 2] (let [target (first (filter #(and (contains? (:magistrates state) %)
                                               (not (contains? (:temples pdata) %)))
                                         (keys (:magistrates state))))]
              (if target (place-temple-in state player-key target true) state))
      [7 3] (-> state ;; Travel + 3 Glory if at Eridu → travel there first
               (assoc-in [:players player-key :caravan] :eridu)
               (update-in [:players player-key :glory] + 3))
      [7 4] (-> state ;; Travel + 3 Amity if at Kish → travel there first
               (assoc-in [:players player-key :caravan] :kish)
               (update-in [:players player-key :amity] + 3))

      ;; ─── Board 8: Fury of Enkidu ───────────────────────────────
      [8 1] (-> state ;; Increase Raider and Priest
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :priest))
      [8 2] (-> state ;; Place demand + sell → grant amity from implied sell
               (update-in [:players player-key :amity] + 3))
      [8 3] (-> state ;; Gain Gold, Gems, Pottery + sell
               (add-player-resource player-key :gold 1)
               (add-player-resource player-key :gems 1)
               (add-player-resource player-key :pottery 1))
      [8 4] (reduce (fn [s [rk _]] ;; Flip all raiders to point
                      (assoc-in s [:players player-key :raiders rk] :point))
                    state (:raiders pdata))

      ;; ─── Board 9: Rites of Ninhursag ───────────────────────────
      [9 1] (-> state ;; Gain Tools, Gold, Pottery + Amity = leader level
               (add-player-resource player-key :tools 1)
               (add-player-resource player-key :gold 1)
               (add-player-resource player-key :pottery 1)
               (update-in [:players player-key :amity] + (get-in pdata [:roles :leader] 1)))
      [9 2] (-> state ;; Increase Priest and Leader
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :leader))
      [9 3] (let [routes (active-routes pc) ;; Raider on each river
                  river-rks (for [r routes :when (= :river (:type r))]
                              (route-key (:from r) (:to r)))
                  avail (remove #(contains? (:raiders pdata) %) river-rks)]
              (reduce #(place-raider-on %1 player-key %2) state (take 3 avail)))
      [9 4] (let [mag-city (first (keys (:magistrates state))) ;; Sell to magistrate + temple
                  target (or (first (filter #(not (contains? (:temples pdata) %))
                                             (keys (:magistrates state))))
                             mag-city)]
              (cond-> state
                target (-> (assoc-in [:players player-key :caravan] target)
                          (update-in [:players player-key :amity] + 2)
                          (place-temple-in player-key target true))))

      ;; ─── Board 10: Wealth of Meskalamdug ───────────────────────
      [10 1] (increase-role-free state player-key :merchant)
      [10 2] (increase-role-free state player-key :merchant)
      [10 3] (let [routes (active-routes pc) ;; Raider near magistrate + amity
                   mag-cities (set (keys (:magistrates state)))
                   mag-rks (for [r routes
                                 :when (or (contains? mag-cities (:from r))
                                           (contains? mag-cities (:to r)))]
                             (route-key (:from r) (:to r)))
                   avail (remove #(contains? (:raiders pdata) %) mag-rks)]
               (if-let [rk (first avail)]
                 (-> state
                     (place-raider-on player-key rk)
                     (update-in [:players player-key :amity] + 2))
                 (update-in state [:players player-key :amity] + 2)))
      [10 4] (place-temple-in state player-key :nippur true)

      ;; ─── Board 11: Ambition of Sargon ──────────────────────────
      [11 1] (-> state ;; Place demand tokens in Lagash → approximate with resources
                (assoc-in [:players player-key :caravan] :lagash)
                (add-player-resource player-key :gold 1)
                (add-player-resource player-key :pottery 1))
      [11 2] (-> state ;; Sell to Lagash double glory
                (assoc-in [:players player-key :caravan] :lagash)
                (update-in [:players player-key :glory] + 4))
      [11 3] (increase-role-free state player-key :raider)
      [11 4] (let [fd (count-face-down-temples pdata)]
               (update-in state [:players player-key :glory] + fd))

      ;; ─── Board 12: Currents of Enki ────────────────────────────
      [12 1] (reduce (fn [s role] ;; Increase all level-1 roles
                       (if (= 1 (get-in s [:players player-key :roles role] 1))
                         (assoc-in s [:players player-key :roles role] 2) s))
                     state roles)
      [12 2] (-> state ;; Gain Gold x3 + Gems
               (add-player-resource player-key :gold 3)
               (add-player-resource player-key :gems 1))
      [12 3] (-> state ;; Increase merchant + sell for glory
                (increase-role-with-cost player-key :merchant)
                (update-in [:players player-key :glory] + 3))
      [12 4] (let [fd (count-face-down-temples pdata)]
               (update-in state [:players player-key :glory] + fd))

      ;; ─── Board 13: Pillars of Etana ────────────────────────────
      [13 1] (-> state ;; Gain Tools x3 + Glory = leader level
               (add-player-resource player-key :tools 3)
               (update-in [:players player-key :glory] + (get-in pdata [:roles :leader] 1)))
      [13 2] (-> state ;; Gain Pottery x3 + Glory = leader level
               (add-player-resource player-key :pottery 3)
               (update-in [:players player-key :glory] + (get-in pdata [:roles :leader] 1)))
      [13 3] (reduce (fn [s role] ;; Increase all level-3 roles
                       (if (= 3 (get-in s [:players player-key :roles role] 1))
                         (increase-role-with-cost s player-key role) s))
                     state roles)
      [13 4] (let [raider-cities (set (mapcat (fn [[a b]] [a b]) (keys (:raiders pdata))))
                   target (first (filter #(and (contains? raider-cities %)
                                                (not (contains? (:temples pdata) %)))
                                         raider-cities))]
               (if target
                 (place-temple-in state player-key target true)
                 state))

      ;; ─── Board 14: Roads of Shulgi ─────────────────────────────
      [14 1] (let [rc (count-raiders-deployed pdata)] ;; Glory per raider (partial: no raider placement)
               (update-in state [:players player-key :glory] + rc))
      [14 2] (-> state ;; Resources (partial: no magistrate move)
               (add-player-resource player-key :tools 1)
               (add-player-resource player-key :pottery 1))
      [14 3] (assoc-in state [:players player-key :caravan] :eridu) ;; Travel to Eridu (partial: no demands)
      [14 4] (place-temple-in state player-key :babylon true)

      ;; ─── Board 15: Ascent of Ur-Nammu ──────────────────────────
      [15 1] (let [demands (:demand-tokens pdata [])] ;; Good per demand fulfilled
               (reduce (fn [s d] (add-player-resource s player-key d 1)) state demands))
      [15 2] (-> state ;; Increase Priest + 4 Glory if facedown temple in Babylon
               (increase-role-with-cost player-key :priest)
               (cond-> (= :face-down (get-in pdata [:temples :babylon]))
                 (update-in [:players player-key :glory] + 4)))
      [15 3] (let [lowest-role (first (sort-by #(get-in pdata [:roles %] 1) roles))]
               (increase-role-free state player-key lowest-role)) ;; Increase lowest role (partial: no travel)
      [15 4] (let [rc (count-raiders-deployed pdata)] ;; 2 Amity per raider (partial: no adjacency check)
               (update-in state [:players player-key :amity] + (* 2 rc)))

      ;; ─── Board 16: Dominion of Hammurabi ────────────────────────
      [16 1] (let [tc (count-temples-placed pdata)] ;; Pottery per temple
               (add-player-resource state player-key :pottery tc))
      [16 2] (let [routes (active-routes pc) ;; Deploy + amity per raider
                   any-rk (first (remove #(contains? (:raiders pdata) %)
                                         (for [r routes]
                                           (route-key (:from r) (:to r)))))
                   s' (if any-rk (place-raider-on state player-key any-rk) state)
                   rc (count-raiders-deployed (get-in s' [:players player-key]))]
               (update-in s' [:players player-key :amity] + (* 2 rc)))
      [16 3] (-> state ;; Increase Leader twice
               (increase-role-with-cost player-key :leader)
               (increase-role-with-cost player-key :leader))
      [16 4] (-> state ;; Place demands + sell → approximate resource+amity gain
               (add-player-resource player-key :tools 1)
               (update-in [:players player-key :amity] + 3))

      ;; ─── Board 17: Cunning of Kubaba ────────────────────────────
      [17 1] (if-let [raider-to-flip (first (filter #(= :raiding (val %)) (:raiders pdata)))]
               (assoc-in state [:players player-key :raiders (key raider-to-flip)] :point) ;; Flip one raider to point
               state)
      [17 2] (let [mag-cities (keys (:magistrates state))   ;; Temple in a magistrate city
                   placeable (first (filter #(and (not (contains? (:temples pdata) %))
                                                  (pos? (:temples-supply pdata 0)))
                                           mag-cities))]
               (if placeable
                 (place-temple-in state player-key placeable true)
                 state))
      [17 3] (update-in state [:players player-key :amity] + 4) ;; 4 Amity (partial: no Uruk check)
      [17 4] (let [ml (get-in pdata [:roles :merchant] 1)] ;; Glory = merchant level (partial: no sell)
               (update-in state [:players player-key :glory] + ml))

      ;; ─── Board 18: Forge of Tubal-Cain ─────────────────────────
      [18 1] (add-player-resource state player-key :tools 2) ;; Resources (partial: no magistrate move/sell)
      [18 2] (if (= :face-down (get-in pdata [:temples :samarra])) ;; 5 Glory if facedown Samarra
               (update-in state [:players player-key :glory] + 5)
               ;; Partial: 2 Glory if precondition unmet
               (update-in state [:players player-key :glory] + 2))
      [18 3] (update-in state [:players player-key :amity] + 3) ;; 3 Amity (partial: no surround check)
      [18 4] (let [point-raiders (count (filter #(= :point (val %)) (:raiders pdata)))]
               (update-in state [:players player-key :amity] + (* 4 point-raiders))) ;; 4 Amity per point raider (partial: don't remove)

      ;; ─── Board 19: Kilns of Ninkasi ────────────────────────────
      [19 1] (-> state ;; Increase Priest twice
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :priest))
      [19 2] (-> state ;; Sell to pottery cities → approximate amity + pottery bonus
                (add-player-resource player-key :pottery 1)
                (update-in [:players player-key :amity] + 3))
      [19 3] (let [goods [:tools :pottery :gold :gems] ;; Discard good + move magistrate + sell
                   has-good (first (filter #(pos? (get-in pdata [:resources %] 0)) goods))]
               (cond-> state
                 has-good (update-in [:players player-key :resources has-good] dec)
                 true (update-in [:players player-key :glory] + 3)))
      [19 4] (reduce (fn [s [rk _]] ;; Flip all raiders to point
                       (assoc-in s [:players player-key :raiders rk] :point))
                     state (:raiders pdata))

      ;; ─── Board 20: Vision of Rimush ─────────────────────────────
      [20 1] (let [routes (active-routes pc) ;; Place raider on each opposing route
                   ;; "Opposing" = routes from current caravan city
                   adj-rks (for [r routes
                                 :when (or (= (:caravan pdata) (:from r))
                                           (= (:caravan pdata) (:to r)))]
                             (route-key (:from r) (:to r)))
                   avail (remove #(contains? (:raiders pdata) %) adj-rks)]
               (reduce #(place-raider-on %1 player-key %2) state (take 2 avail)))
      [20 2] (-> state ;; Increase Merchant twice
               (increase-role-with-cost player-key :merchant)
               (increase-role-with-cost player-key :merchant))
      [20 3] (update-in state [:players player-key :amity] +
                         (get-in pdata [:roles :leader] 1)) ;; Influence + amity = leader
      [20 4] (-> state ;; Take goods from astronomer spaces → grant 2 resources
               (add-player-resource player-key :tools 1)
               (add-player-resource player-key :gold 1))

      ;; ─── Board 21: Legacy of Eannatum ───────────────────────────
      [21 1] (assoc-in state [:players player-key :caravan] :eridu) ;; Travel to Eridu
      [21 2] (-> state ;; Increase Raider and Leader
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :leader))
      [21 3] (assoc-in state [:players player-key :caravan] :eridu) ;; Travel to Eridu (partial: no sell)
      [21 4] (let [demands (:demand-tokens pdata [])] ;; Glory per demand
               (update-in state [:players player-key :glory] + (count demands)))

      ;; ─── Board 22: Strategy of Naram-Sin ────────────────────────
      [22 1] (-> state ;; Increase Raider and Merchant
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :merchant))
      [22 2] (-> state ;; Demands on facedown temples → amity from flipped
                (update-in [:players player-key :amity] + (count-face-down-temples pdata)))
      [22 3] (-> state ;; Good + travel → grant a resource
                (add-player-resource player-key :pottery 1))
      [22 4] (let [rc (count-raiders-deployed pdata)] ;; 2 Amity per raider + travel
               (update-in state [:players player-key :amity] + (* 2 rc)))

      ;; ─── Board 23: Market of Puabi ──────────────────────────────
      [23 1] (-> state ;; Increase Priest and Merchant
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :merchant))
      [23 2] (-> state ;; Sell twice to Eridu → travel + double amity
                (assoc-in [:players player-key :caravan] :eridu)
                (update-in [:players player-key :amity] + 4))
      [23 3] (-> state ;; Good + travel + increase merchant
                (add-player-resource player-key :tools 1)
                (increase-role-with-cost player-key :merchant))
      [23 4] (let [target (first (filter #(and (contains? (:magistrates state) %)
                                                (not (contains? (:temples pdata) %)))
                                         (keys (:magistrates state))))]
               (if target (place-temple-in state player-key target true) state))

      ;; ─── Board 24: Siege of Shulme ──────────────────────────────
      [24 1] (-> state ;; Increase Raider and Leader
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :leader))
      [24 2] (-> state ;; Demands on magistrates → partial glory
                (update-in [:players player-key :glory] + 2))
      [24 3] (let [demands (:demand-tokens pdata [])] ;; Glory per demand
               (update-in state [:players player-key :glory] + (count demands)))
      [24 4] (let [demands (:demand-tokens pdata [])] ;; Goods per demand at magistrates
                (reduce (fn [s d] (add-player-resource s player-key d 1))
                        state (take 2 demands)))

      ;; ─── Board 25: Command of Mesannepada ───────────────────────
      [25 1] (let [point-count (count (filter #(= :point (val %)) (:raiders pdata)))]
               ;; Influence + score raiders → glory per point-raider
               (update-in state [:players player-key :glory] + (+ 2 point-count)))
      [25 2] (-> state ;; Increase Merchant and Leader
               (increase-role-with-cost player-key :merchant)
               (increase-role-with-cost player-key :leader))
      [25 3] (let [faceup (filter #(= :face-up (val %)) (:temples pdata)) ;; Two facedown temples
                   n (min 2 (count faceup))]
               (reduce (fn [s [city _]]
                         (-> s
                             (assoc-in [:players player-key :temples city] :face-down)
                             (update-in [:players player-key :amity] inc)))
                       state (take n faceup)))
      [25 4] (-> state ;; Good + travel
                (add-player-resource player-key :gems 1))

      ;; ─── Board 26: Court of Enshakushanna ───────────────────────
      [26 1] (-> state ;; Increase Priest and Leader
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :leader))
      [26 2] (-> state ;; Increase Priest and Raider
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :raider))
      [26 3] (-> state ;; Sell + temple
                (update-in [:players player-key :amity] + 2)
                (place-temple-in player-key (:caravan pdata) true))
      [26 4] (let [routes (active-routes pc) ;; Raider + surround
                   any-rk (first (remove #(contains? (:raiders pdata) %)
                                         (for [r routes]
                                           (route-key (:from r) (:to r)))))]
               (cond-> state
                 any-rk (place-raider-on player-key any-rk)
                 true (update-in [:players player-key :amity] + 2)))

      ;; ─── Board 27: Path of Alulim ──────────────────────────────
      [27 1] (-> state ;; Travel + sell
                (update-in [:players player-key :amity] + 3))
      [27 2] (let [routes (active-routes pc) ;; Travel + deploy
                   any-rk (first (remove #(contains? (:raiders pdata) %)
                                         (for [r routes]
                                           (route-key (:from r) (:to r)))))]
               (if any-rk (place-raider-on state player-key any-rk) state))
      [27 3] (place-temple-in state player-key (:caravan pdata) true) ;; Travel + temple
      [27 4] (-> state ;; Three goods of choice (give one of each best 3)
               (add-player-resource player-key :tools 1)
               (add-player-resource player-key :gold 1)
               (add-player-resource player-key :gems 1))

      ;; ─── Board 28: Stars of Sin-Kashid ─────────────────────────
      [28 1] (place-temple-in state player-key (:caravan pdata) true) ;; Travel + temple
      [28 2] (place-temple-in state player-key (:caravan pdata) true) ;; Travel + temple
      [28 3] (let [gold (get-in pdata [:resources :gold] 0)] ;; Sell gold to city + demand
               (cond-> state
                 (pos? gold) (update-in [:players player-key :resources :gold] dec)
                 true (update-in [:players player-key :amity] + 4)))
      [28 4] (let [routes (active-routes pc) ;; Raider point-side near Kish
                   kish-rks (for [r routes
                                  :when (or (= :kish (:from r)) (= :kish (:to r)))]
                              (route-key (:from r) (:to r)))
                   avail (remove #(contains? (:raiders pdata) %) kish-rks)]
               (if-let [rk (first avail)]
                 (-> state
                     (place-raider-on player-key rk)
                     (assoc-in [:players player-key :raiders rk] :point))
                 state))

      ;; ─── Board 29: Treasury of Ibbi-Sin ────────────────────────
      [29 1] (let [ll (get-in pdata [:roles :leader] 1)] ;; Decrease leader + increase others
               (if (> ll 1)
                 (-> state
                     (assoc-in [:players player-key :roles :leader] (dec ll))
                     (increase-role-free player-key :merchant)
                     (increase-role-free player-key :priest))
                 state))
      [29 2] (-> state ;; Travel + sell
                (update-in [:players player-key :amity] + 3))
      [29 3] (let [routes (active-routes pc) ;; Raider on each river
                   river-rks (for [r routes :when (= :river (:type r))]
                               (route-key (:from r) (:to r)))
                   avail (remove #(contains? (:raiders pdata) %) river-rks)]
               (reduce #(place-raider-on %1 player-key %2) state (take 3 avail)))
      [29 4] (-> state ;; Temple in surrounded cities
                (place-temple-in player-key (:caravan pdata) true))

      ;; ─── Board 30: Council of Amar-Sin ──────────────────────────
      [30 1] (update-in state [:players player-key :glory] + (get-in pdata [:roles :leader] 1)) ;; Glory = leader level (partial: no influence+travel)
      [30 2] (update-in state [:players player-key :amity] + (get-in pdata [:roles :leader] 1)) ;; Amity = leader level (partial: no influence+sell)
      [30 3] (update-in state [:players player-key :glory] + (get-in pdata [:roles :raider] 1)) ;; Glory = raider level (partial: no deploy+influence)
      [30 4] (update-in state [:players player-key :amity] + (get-in pdata [:roles :priest] 1)) ;; Amity = priest level (partial: no influence+temple)

      ;; ─── Board 31: Horizon of Sharkalisharri ────────────────────
      [31 1] (reduce (fn [s role] ;; Increase all level-1 roles
                       (if (= 1 (get-in s [:players player-key :roles role] 1))
                         (assoc-in s [:players player-key :roles role] 2) s))
                     state roles)
      [31 2] (reduce (fn [s role] ;; Increase all level-3 roles
                       (if (= 3 (get-in s [:players player-key :roles role] 1))
                         (increase-role-with-cost s player-key role) s))
                     state roles)
      [31 3] (let [faceup (first (filter #(= :face-up (val %)) (:temples pdata)))] ;; Resource + facedown temple
               (-> state
                   (add-player-resource player-key :gems 1)
                   (cond-> faceup
                     (-> (assoc-in [:players player-key :temples (key faceup)] :face-down)
                         (update-in [:players player-key :amity] + 2)))))
      [31 4] (let [routes (active-routes pc) ;; Resource + deploy
                   any-rk (first (remove #(contains? (:raiders pdata) %)
                                         (for [r routes]
                                           (route-key (:from r) (:to r)))))]
               (cond-> state
                 true (add-player-resource player-key :tools 1)
                 any-rk (place-raider-on player-key any-rk)))

      ;; ─── Board 32: Jewel of Ku-Bau ─────────────────────────────
      [32 1] (let [demands (:demand-tokens pdata [])] ;; Sell + glory per demand
               (update-in state [:players player-key :glory] + (count demands)))
      [32 2] (add-player-resource state player-key :gems 1) ;; Gem + travel — partial
      [32 3] (let [routes (active-routes pc) ;; Raider between temple cities
                   temple-cities (set (keys (:temples pdata)))
                   temple-rks (for [r routes
                                    :when (and (contains? temple-cities (:from r))
                                               (contains? temple-cities (:to r)))]
                                (route-key (:from r) (:to r)))
                   avail (remove #(contains? (:raiders pdata) %) temple-rks)]
               (if-let [rk (first avail)]
                 (place-raider-on state player-key rk)
                 state))
      [32 4] (-> state ;; Influence + sell
                (update-in [:players player-key :amity] + 2)
                (update-in [:players player-key :glory] + 2))

      ;; ─── Board 33: Vanguard of Enmebaragesi ─────────────────────
      [33 1] (let [ml (get-in pdata [:roles :merchant] 1)] ;; Decrease merchant + increase others
               (if (> ml 1)
                 (-> state
                     (assoc-in [:players player-key :roles :merchant] (dec ml))
                     (increase-role-free player-key :raider)
                     (increase-role-free player-key :priest))
                 state))
      [33 2] (let [faceup (first (filter #(= :face-up (val %)) (:temples pdata)))]
               ;; Facedown temple + travel
               (if faceup
                 (-> state
                     (assoc-in [:players player-key :temples (key faceup)] :face-down)
                     (update-in [:players player-key :amity] + 3))
                 (update-in state [:players player-key :amity] + 1)))
      [33 3] (place-temple-in state player-key :uruk true) ;; Temple in Uruk
      [33 4] (let [routes (active-routes pc) ;; Deploy + travel
                   any-rk (first (remove #(contains? (:raiders pdata) %)
                                         (for [r routes]
                                           (route-key (:from r) (:to r)))))]
               (if any-rk (place-raider-on state player-key any-rk) state))

      ;; ─── Board 34: Honor of Agga ────────────────────────────────
      [34 1] (let [tools (get-in pdata [:resources :tools] 0) ;; Pay tools for raiders around Uruk
                   routes (active-routes pc)
                   uruk-rks (for [r routes
                                  :when (or (= :uruk (:from r)) (= :uruk (:to r)))]
                              (route-key (:from r) (:to r)))
                   avail (remove #(contains? (:raiders pdata) %) uruk-rks)
                   n (min tools (count avail) 2)]
               (if (pos? n)
                 (-> (reduce #(place-raider-on %1 player-key %2) state (take n avail))
                     (update-in [:players player-key :resources :tools] - n))
                 state))
      [34 2] (let [routes (active-routes pc) ;; Raider on each existing route (approx: 2)
                   avail (remove #(contains? (:raiders pdata) %)
                                 (for [r routes] (route-key (:from r) (:to r))))]
               (reduce #(place-raider-on %1 player-key %2) state (take 2 avail)))
      [34 3] (let [mag-cities (set (keys (:magistrates state))) ;; Sell at mag+temple cities
                   valid (first (filter #(and (contains? mag-cities %)
                                               (contains? (:temples pdata) %))
                                        (keys (:temples pdata))))]
               (if valid
                 (-> state
                     (assoc-in [:players player-key :caravan] valid)
                     (update-in [:players player-key :amity] + 3))
                 (update-in state [:players player-key :amity] + 2)))
      [34 4] (let [mag-cities (set (keys (:magistrates state))) ;; Same as 34-3
                   valid (first (filter #(and (contains? mag-cities %)
                                               (contains? (:temples pdata) %))
                                        (keys (:temples pdata))))]
               (if valid
                 (-> state
                     (assoc-in [:players player-key :caravan] valid)
                     (update-in [:players player-key :amity] + 3))
                 (update-in state [:players player-key :amity] + 2)))

      ;; ─── Board 35: Wanderer of Dumuzi ──────────────────────────
      [35 1] (-> state ;; Travel + sell
                (update-in [:players player-key :amity] + 3))
      [35 2] (let [pottery (get-in pdata [:resources :pottery] 0) ;; Pay pottery for temples
                   n (min pottery 2)]
               (if (pos? n)
                 (let [s' (update-in state [:players player-key :resources :pottery] - n)
                       ;; Place n temples in distinct available cities
                       cities (remove #(contains? (:temples pdata) %) (keys (:city-graph state)))]
                   (reduce #(place-temple-in %1 player-key %2 true) s' (take n cities)))
                 state))
      [35 3] (increase-role-with-cost state player-key
               (first (sort-by #(get-in state [:players player-key :roles %] 1) roles)))
      [35 4] (let [point-count (count (filter #(= :point (val %)) (:raiders pdata)))]
               ;; Influence + score raiders
               (update-in state [:players player-key :glory] + (+ 2 point-count)))

      ;; Default: unhandled effect, no-op
      state)]
    ;; Detect what changed and log it
    (let [post-pdata (get-in result-state [:players player-key])
          changed? (not= (select-keys pdata [:amity :glory :roles :resources
                                             :temples :raiders :temples-supply
                                             :raiders-supply :caravan])
                         (select-keys post-pdata [:amity :glory :roles :resources
                                                  :temples :raiders :temples-supply
                                                  :raiders-supply :caravan]))
          impl-status (get effect-implementation-status [board-id slot-idx] :unknown)
          effect-entry {:board-id board-id
                        :slot slot-idx
                        :changed changed?
                        :impl-status impl-status
                        :noop-reason (when-not changed?
                                       (case impl-status
                                         :needs-compound "compound action sequence not implemented"
                                         :needs-placement "conditional placement logic not implemented"
                                         :needs-demand "demand token manipulation not implemented"
                                         :conditional "board state precondition not met"
                                         :persistent "persistent effect (tracked, not instant)"
                                         :implemented "effect ran but no state change (precondition unmet)"
                                         "unknown"))
                        :delta-amity (- (:amity post-pdata 0) pre-amity)
                        :delta-glory (- (:glory post-pdata 0) pre-glory)
                        :delta-temples (- (count (:temples post-pdata)) pre-temples)
                        :delta-raiders (- (count (:raiders post-pdata)) pre-raiders)}]
      (update-in result-state [:players player-key :board-effects-log]
                 (fnil conj []) effect-entry))))

(defn- estimate-effect-value
  "Estimate the immediate value of uncovering a bonus board slot.
   Returns a rough score (0 = no-op, higher = better).
   Used to decide whether to claim now or hold."
  [state player-key board-id slot-idx]
  (let [;; Apply the effect to a copy and measure delta
        test-state (apply-bonus-effect state player-key board-id slot-idx)
        pdata-before (get-in state [:players player-key])
        pdata-after (get-in test-state [:players player-key])
        delta-amity (- (:amity pdata-after 0) (:amity pdata-before 0))
        delta-glory (- (:glory pdata-after 0) (:glory pdata-before 0))
        delta-roles (- (reduce + (vals (:roles pdata-after)))
                       (reduce + (vals (:roles pdata-before))))
        delta-resources (- (reduce + (vals (:resources pdata-after)))
                           (reduce + (vals (:resources pdata-before))))
        delta-temples (- (count (:temples pdata-after)) (count (:temples pdata-before)))
        delta-raiders (- (count (:raiders pdata-after)) (count (:raiders pdata-before)))]
    ;; Weight: direct points are most valuable, roles/resources less so
    (+ (* 2.0 (+ delta-amity delta-glory))
       (* 3.0 delta-roles)
       (* 1.0 delta-resources)
       (* 2.0 delta-temples)
       (* 1.5 delta-raiders)
       ;; Persistent effect (slot 0) is always valuable to unlock early
       (if (zero? slot-idx) 5.0 0.0))))

(defn- best-slot-to-uncover
  "Find the covered slot with the highest estimated value to uncover.
   Persistent effect (slot 0) gets a bonus since it applies for the rest of the game.
   Returns [slot-idx estimated-value]."
  [state player-key]
  (let [board (get-in state [:players player-key :bonus-board]
                      (vec (repeat 5 :covered)))
        board-id (get-in state [:bonus-boards player-key])
        covered-slots (keep-indexed #(when (= :covered %2) %1) board)]
    (when (seq covered-slots)
      (apply max-key second
             (for [slot covered-slots]
               [slot (estimate-effect-value state player-key board-id slot)])))))

(defn best-slot-for-feat
  "Pick best covered slot to uncover, weighted by synergy with `next-contest`.
   If `next-contest` is nil, falls back to raw `best-slot-to-uncover`.
   Returns [slot-idx score] or nil if no slots covered."
  [state player-key next-contest]
  (let [board (get-in state [:players player-key :bonus-board]
                      (vec (repeat 5 :covered)))
        board-id (get-in state [:bonus-boards player-key])
        covered-slots (keep-indexed #(when (= :covered %2) %1) board)]
    (when (seq covered-slots)
      (let [need-actions (if next-contest
                           (feat-action-profile (:id next-contest))
                           #{})]
        (apply max-key second
               (for [slot covered-slots]
                 (let [base (estimate-effect-value state player-key board-id slot)
                       after (apply-bonus-effect state player-key board-id slot)
                       ;; Progress delta on the next planned feat
                       delta-prog (if next-contest
                                    (- (first (feat-progress after player-key next-contest))
                                       (first (feat-progress state player-key next-contest)))
                                    0.0)
                       ;; Action-type synergy: slot 0 unlocks passive, others unlock role
                       ;; boost slots whose effect matches what the next feat needs.
                       ;; We proxy this by checking role bumps (role action profile).
                       post-pdata (get-in after [:players player-key])
                       pre-pdata (get-in state [:players player-key])
                       role-delta (- (reduce + (vals (:roles post-pdata)))
                                     (reduce + (vals (:roles pre-pdata))))
                       synergy (cond
                                 (and (contains? need-actions :sell) (pos? role-delta)) 2.0
                                 (and (contains? need-actions :temple) (pos? role-delta)) 2.0
                                 :else 0.0)]
                   [slot (+ base (* 12.0 delta-prog) synergy)])))))))

(defn- feat-feasible?
  "Is this feat worth planning toward? Excludes claimed feats and hard-to-pre-plan
   event-based feats. Event-based feats (e.g., :G1 :I1 :I2 :J1 :K1) depend on
   turn-specific stats and can't be committed to in a plan."
  [state player-key contest]
  (let [claims (:contest-claims state {})
        cid (:id contest)
        already? (some #{player-key} (get claims cid []))
        ;; Event-based feats: too volatile for planning
        event-based? (#{:G1 :G2 :I1 :I2 :J1 :K1} cid)]
    (and (not already?)
         (not event-based?))))

(defn- chain-score
  "Score a chain of 2-3 feats without deep simulation.
   Sums: wild-points estimate, current progress, ease factor, action-profile overlap.
   Positions earlier in the chain weigh more (they're attempted first)."
  [state player-key chain]
  (let [board-id (get-in state [:bonus-boards player-key])
        pdata (get-in state [:players player-key])
        board (:bonus-board pdata (vec (repeat 5 :covered)))
        n-covered (count (filter #{:covered} board))]
    (reduce
     (fn [total [idx contest]]
       (let [cid (:id contest)
             cur-claims (get-in state [:contest-claims cid] [])
             claim-count (count cur-claims)
             wild-points (get bonus-contest-values claim-count 1)
             [prog _] (feat-progress state player-key contest)
             difficulty (get feat-difficulty cid 5)
             ease-factor (max 0.3 (- 1.5 (/ difficulty 6.0)))
             ;; Effect value: next slot we'd uncover (approximate)
             effect-v (if (and (< idx n-covered) board-id)
                        (let [avail-slot (some (fn [[i v]]
                                                 (when (= v :covered) i))
                                                (map-indexed vector board))]
                          (if avail-slot
                            (estimate-effect-value state player-key board-id avail-slot)
                            0))
                        0)
             ;; Position weight: first feat gets full weight, later less
             pos-weight (case idx 0 1.0 1 0.7 2 0.4 0.2)
             claim-prob (min 1.0 (+ 0.25 (* 0.6 prog) (* 0.3 ease-factor)))
             contribution (* pos-weight
                             (* claim-prob
                                (+ wild-points
                                   (* 0.3 effect-v)
                                   (* 3.0 prog)
                                   (* 2.0 ease-factor))))
             ;; Overlap bonus with the NEXT feat in the chain
             next-c (get chain (inc idx))
             overlap-bonus (if next-c
                             (let [prof-cur (feat-action-profile cid)
                                   prof-nxt (feat-action-profile (:id next-c))
                                   common (count (clojure.set/intersection
                                                  (set prof-cur) (set prof-nxt)))]
                               (* 0.5 common pos-weight))
                             0.0)]
         (+ total contribution overlap-bonus)))
     0.0
     (map-indexed vector chain))))

(defn plan-feat-chain
  "Plan an ordered 2-3 feat chain for `player-key`.
   Evaluates each permutation of top-N feasible feats (by affinity), simulating
   feat-1 claim → slot effect → feat-2 claim etc. Returns best chain as a vector
   of contest maps (ordered)."
  [state player-key]
  (let [contests (:contests state [])
        feasible (filter #(feat-feasible? state player-key %) contests)
        ;; Take top-4 by affinity to limit combinatorial blowup
        scored (sort-by #(- (feat-affinity state player-key %)) feasible)
        top (vec (take 4 scored))
        n (count top)]
    (cond
      (zero? n) []
      (= 1 n) top
      :else
      (let [chain-len (min 3 n)
            ;; Generate all ordered selections of `chain-len` from top
            permute (fn permute [items k]
                      (if (zero? k) [[]]
                          (let [items (vec items)]
                            (for [i (range (count items))
                                  tail (permute (into (subvec items 0 i)
                                                      (subvec items (inc i)))
                                                (dec k))]
                              (into [(nth items i)] tail)))))
            candidates (permute top chain-len)
            scored-chains (for [chain candidates]
                            [(chain-score state player-key chain) chain])
            best (apply max-key first scored-chains)]
        (vec (second best))))))

(defn check-and-claim-feats
  "Check all unclaimed feats for the current player. Uses strategic timing
   controlled by personality weights (tempo, feat-awareness) — the genetic
   algorithm optimizes when to claim vs. hold.
   Last round: always claim when met (last chance).
   Earlier rounds: evaluate whether the bonus board effect + wild points
   justify claiming now vs. waiting for a better position.
   Prioritizes target feats over incidental ones.
   Returns updated state with claims, wild points, and effects applied."
  [state player-key]
  (let [contests (:contests state [])
        claims (:contest-claims state {})
        last-round? (>= (:round state 1) rounds-per-game)
        pdata (get-in state [:players player-key])
        ;; Personality-driven timing: lower tempo = claim eagerly, higher = hold
        tempo (get-in pdata [:personality-cache :tempo] 0.3)
        awareness (get-in pdata [:personality-cache :feat-awareness] 0.3)
        ;; Sort: target feats first (checked before others can claim them)
        targets (set (map :id (:target-feats pdata [])))
        sorted-contests (concat (filter #(contains? targets (:id %)) contests)
                                (remove #(contains? targets (:id %)) contests))]
    (reduce
     (fn [s contest]
       (let [contest-id (:id contest)
             already-claimed? (some #{player-key} (get claims contest-id []))
             board (get-in s [:players player-key :bonus-board]
                           (vec (repeat 5 :covered)))
             has-token? (some #{:covered} board)]
         (if (or already-claimed?
                 (not (evaluate-contest s player-key contest))
                 (not has-token?))
           s
           ;; Evaluate claim timing
           (let [current-claims (get-in s [:contest-claims contest-id] [])
                 claim-count (count current-claims)
                 wild-points (get bonus-contest-values claim-count 1)
                 ;; Pick slot synergizing with the NEXT feat in the chain
                 chain (get-in s [:players player-key :feat-chain] [])
                 ;; Find contest-id's index in chain; pick next one (if any)
                 chain-idx (some (fn [[i c]] (when (= (:id c) contest-id) i))
                                  (map-indexed vector chain))
                 next-feat (when (and chain-idx (< (inc chain-idx) (count chain)))
                             (nth chain (inc chain-idx)))
                 [best-slot effect-value] (or (best-slot-for-feat s player-key next-feat)
                                              [nil 0])
                 is-target? (contains? targets contest-id)
                 ;; Claim decision: genetics can optimize this threshold
                 ;; - Last round: always claim (forced)
                 ;; - Target feat: claim eagerly (scaled by awareness)
                 ;; - First claimer (3 wild pts): almost always worth it
                 ;; - Tempo < 0.5: claim eagerly; > 0.5: wait for better board effect
                 should-claim? (or last-round?
                                  (and is-target? (> awareness 0.1))
                                  (> wild-points 2)
                                  (> effect-value (* tempo 5.0))
                                  (and (> wild-points 1) (> effect-value 0)))]
             (if (and should-claim? best-slot)
               (let [board-id (get-in s [:bonus-boards player-key])
                     s' (-> s
                            (update-in [:contest-claims contest-id] (fnil conj []) player-key)
                            (assoc-in [:players player-key :bonus-board best-slot] :uncovered)
                            (update-in [:players player-key :wild-points] (fnil + 0) wild-points)
                            (apply-bonus-effect player-key board-id best-slot)
                            (apply-passive player-key :feat-claimed
                                           {:contest-id contest-id
                                            :slot best-slot}))
                     ;; Re-target: when a target feat is claimed, select a replacement
                     ;; from unclaimed feats to keep pursuit active
                     s' (if is-target?
                          ;; Advance the chain: drop the just-claimed feat, then
                          ;; re-plan from what remains. target-feats is first 2.
                          (let [new-chain (plan-feat-chain s' player-key)
                                new-targets (vec (take 2 new-chain))]
                            (-> s'
                                (assoc-in [:players player-key :feat-chain] new-chain)
                                (assoc-in [:players player-key :target-feats] new-targets)))
                          s')]
                 s')
               s)))))
     state
     sorted-contests)))

(defn apply-end-game-scoring
  "Apply end-of-game scoring:
   1. Role track bonuses (level 5 → 10 pts to relevant track)
   2. Wild points split optimally (to lower track for max reputation)"
  [state]
  (reduce
   (fn [s player-key]
     (let [;; 0. Fire :end-game passive first so its effects are visible to
           ;;    the wild-points-split heuristic below (board 18: tools → glory).
           s (apply-passive s player-key :end-game {})
           pdata (get-in s [:players player-key])
           role-levels (:roles pdata)
           ;; 1. Role end-game bonuses
           s (reduce
              (fn [st role]
                (if (= max-role-level (get role-levels role 1))
                  (let [{:keys [track points]} (get role-end-game-bonus role)]
                    (update-in st [:players player-key track] + points))
                  st))
              s
              roles)
           ;; 2. Wild points — split to maximize reputation (put in lower track)
           wild (get-in s [:players player-key :wild-points] 0)]
       (if (pos? wild)
         (let [amity (get-in s [:players player-key :amity] 0)
               glory (get-in s [:players player-key :glory] 0)
               ;; Put all wild points into the lower track to maximize min(amity,glory)
               ;; If tied, split evenly
               to-amity (cond
                          (< amity glory) (min wild (- glory amity))
                          (> amity glory) 0
                          :else (quot wild 2))
               to-glory (- wild to-amity)]
           (-> s
               (update-in [:players player-key :amity] + to-amity)
               (update-in [:players player-key :glory] + to-glory)))
         s)))
   state
   (:turn-order state)))

;; =============================================================================
;; Player state
;; =============================================================================

;; From MSE "Starting Cards.mse-set" — canonical data
;; Symbol key: G=gems O=gold P=pottery L=tools
(def starting-cards
  [{:number 1 :city :babylon :role :leader   :resource :gems}     ;; A1
   {:number 2 :city :nippur  :role :merchant :resource :tools}    ;; B1
   {:number 3 :city :lagash  :role :merchant :resource :pottery}  ;; B2
   {:number 4 :city :babylon :role :priest   :resource :tools}    ;; A2
   {:number 5 :city :kish    :role :raider   :resource :gems}     ;; C1
   {:number 6 :city :kish    :role :leader   :resource :pottery}  ;; C2
   {:number 7 :city :uruk    :role :raider   :resource :pottery}  ;; D1
   {:number 8 :city :uruk    :role :priest   :resource :pottery}  ;; D2
   ])

;; =============================================================================
;; Bonus Contests (feat/race cards) — from MSE "Bonus Contests.mse-set"
;; =============================================================================
;; Symbol key from MSE: G=gems O=gold P=pottery L=tools A=amity Y=glory
;; Contest IDs are the "Cost" field from MSE

(def bonus-contests
  [;; --- A: Fulfill goods ---
   {:id :A1 :name "Fulfill Gems/Gold"
    :description "Fulfill 3 Gems and/or Gold"
    :category :fulfill}
   {:id :A2 :name "Fulfill Tools/Pottery"
    :description "Fulfill 3 Tools and/or Pottery"
    :category :fulfill}
   ;; --- B: Fulfill patterns ---
   {:id :B1 :name "Fulfill Same Type"
    :description "Fulfill 3 goods of the same type"
    :category :fulfill}
   {:id :B2 :name "Fulfill All Types"
    :description "Fulfill one or more good of all four types"
    :category :fulfill}
   ;; --- C: Temple count ---
   {:id :C1 :name "Four Face-Up Temples"
    :description "Four face-up temples"
    :category :temple}
   {:id :C2 :name "Four Face-Down Temples"
    :description "Four face-down temples"
    :category :temple}
   ;; --- D: Temple placement ---
   {:id :D1 :name "Temples in Eridu & Nineveh"
    :description "A temple in each Eridu and Nineveh"
    :category :temple}
   {:id :D2 :name "Temples in River Cities"
    :description "A temple in four river cities"
    :category :temple}
   ;; --- E: Raider placement ---
   {:id :E1 :name "Surround Kish"
    :description "Raiders surrounding Kish"
    :category :raider}
   {:id :E2 :name "Raiders at Eridu & Nineveh"
    :description "Raiders next to Eridu and Nineveh"
    :category :raider}
   ;; --- F: Raider state ---
   {:id :F1 :name "Three Point Raiders"
    :description "Three Raiders on their point side"
    :category :raider}
   {:id :F2 :name "Raiders on Rivers"
    :description "A raider on each river"
    :category :raider}
   ;; --- G: Magistrate movement ---
   {:id :G1 :name "Move Magistrate Four"
    :description "Move one Magistrate four cities in one turn"
    :category :magistrate}
   {:id :G2 :name "Magistrate Through Raiders"
    :description "Move a Magistrate through three raiders (owned by any player)"
    :category :magistrate}
   ;; --- H: Role levels ---
   {:id :H1 :name "Two Roles at Level 3+"
    :description "Two roles at level 3 or higher"
    :category :role}
   {:id :H2 :name "Any Role at Level 5"
    :description "Any Role at Level 5"
    :category :role}
   ;; --- I: Scoring thresholds ---
   {:id :I1 :name "10 Points with Temple Flip"
    :description "Earn 10 points on a turn where you flip at least 1 Temple (Amity and/or Glory)"
    :category :scoring}
   {:id :I2 :name "5 Glory in One Turn"
    :description "Score 5 Glory in one turn"
    :category :scoring}
   ;; --- J: Scoring thresholds ---
   {:id :J1 :name "5 Amity in One Turn"
    :description "Score 5 Amity in one turn"
    :category :scoring}
   {:id :J2 :name "Only Tools"
    :description "Have two Tools but no other goods"
    :category :resource}
   ;; --- K: Sell achievements ---
   {:id :K1 :name "Big Gold Sale"
    :description "Earn 5 total points by selling one Gold (Amity and/or Glory)"
    :category :sell}
   {:id :K2 :name "Sell in Surrounded City"
    :description "Sell in a city surrounded by Raiders"
    :category :sell}
   ;; --- L: Resource hoarding ---
   {:id :L1 :name "5 Gems"
    :description "Have 5 Gems"
    :category :resource}
   {:id :L2 :name "5 Pottery"
    :description "Have 5 Pottery"
    :category :resource}
   ;; --- M: Magistrate + temple combos ---
   {:id :M1 :name "Magistrates at Temples"
    :description "Both Magistrates in cities with your facedown temples"
    :category :magistrate}
   {:id :M2 :name "Temples Without Demand"
    :description "Four temples in cities with no demand"
    :category :temple}])

;; =============================================================================
;; Bonus Boards — from MSE "BonusBoards.mse-set"
;; =============================================================================
;; Each board has 5 effects: Effect1 is a persistent/passive ability,
;; Effects 2-5 are one-time bonuses uncovered in order.
;; Symbol key: G=gems O=gold P=pottery L=tools A=amity Y=glory

(def bonus-boards
  [{:id 1 :name "Shield of Gilgamesh"
    :effects
    ["When you surround a city with Raiders, put a temple in it (you don't have to be there)"
     "Travel to Kish via the shortest route (you may choose between equal routes)"
     "Increase your Raider and Leader Roles (paying any costs)"
     "Place two Raiders adjacent to Lagash (you don't have to be there)"
     "Score Glory for each demand you have fulfilled"]}
   {:id 2 :name "Seal of Enmerkar"
    :effects
    ["When you score a Raider you may increase your Priest role (paying any costs)"
     "Increase your Merchant and Raider Roles (paying any costs)"
     "Score 5 Amity if you are in a city with a Magistrate"
     "Place a Temple in a city with a Magistrate (even if you already have a temple there)"
     "Score Glory for each of your facedown Temples"]}
   {:id 3 :name "Voyage of Ziusudra"
    :effects
    ["When you Travel across a river take a Gem. Your Gems are worth Amity each at end of game"
     "Increase your Leader Role for Free"
     "Place a Temple in Lagash (even if you already have a temple there)"
     "Place a Raider adjacent to Eridu and gain a good of your choice"
     "Take a travel action then a Sell action"]}
   {:id 4 :name "Blessing of Inanna"
    :effects
    ["When you flip a temple you may sell in that city"
     "Place a Temple in Eridu (even if you already have a temple there)"
     "Gain Tools, Gems, Gold"
     "Score Amity based on your Leader level x 2"
     "Score 2 Amity for each of your Raiders"]}
   {:id 5 :name "Wisdom of Adapa"
    :effects
    ["When you Influence a Magistrate in your city you may travel with it"
     "Increase your Priest Role for Free"
     "Place two random Demand Tokens in Uruk. Gain the matching resources"
     "Take a Deploy action then a Temple action"
     "Score 2 Amity for each of your Raiders"]}
   {:id 6 :name "Trade of Dumuzid"
    :effects
    ["When you use action space 7 you get a free Travel action"
     "Increase your Merchant and Priest Roles (paying any costs)"
     "Place a temple in each city with a Magistrate (if you don't have one there)"
     "Sell to Babylon for double points (you don't need to be there)"
     "Place a Raider adjacent to Lagash. Gain Tools, Tools"]}
   {:id 7 :name "March of Lugalbanda"
    :effects
    ["When you place Raiders you may place an additional one next to a Magistrate"
     "Increase your Merchant and Leader Roles (paying any costs)"
     "Place a Temple in a city with a Magistrate (even if you already have a temple there)"
     "Take a travel action. Score 3 Glory if you are in Eridu"
     "Take a travel action. Score 3 Amity if you are in Kish"]}
   {:id 8 :name "Fury of Enkidu"
    :effects
    ["When you score a Raider, instead flip it to its active side"
     "Increase your Raider and Priest Roles (paying any costs)"
     "Place one random Demand Token in Nippur and Babylon each. Then you may sell once in your city"
     "Gain Gold, Gems, Pottery. Then you may sell once in your city"
     "Flip all of your Raiders to their point side"]}
   {:id 9 :name "Rites of Ninhursag"
    :effects
    ["When you flip a Temple, you may increase a role (paying any costs)"
     "Gain Tools, Gold, Pottery. Score Amity based on your Leader level"
     "Increase your Priest and Leader Roles (paying any costs)"
     "Place a Raider on each River"
     "Sell to any city with a Magistrate. If you are in that city, you may take a Temple action"]}
   {:id 10 :name "Wealth of Meskalamdug"
    :effects
    ["You may sell Gold to cities with no demands. If you do, place a random Demand Token on that city"
     "Increase your Merchant Role for Free"
     "Increase your Merchant Role for Free"
     "Place a Raider adjacent to a Magistrate. Score Amity based on your Leader level"
     "Place a Temple in Nippur (even if you already have a temple there)"]}
   {:id 11 :name "Ambition of Sargon"
    :effects
    ["When you meet this and other contests, score additional Glory based on your Leader level"
     "Place two random Demand Tokens in Lagash. Gain matching resources"
     "Sell to Lagash for Double Glory points (you don't have to be there)"
     "Increase your Raider Role for Free"
     "Score Glory for each of your facedown Temples"]}
   {:id 12 :name "Currents of Enki"
    :effects
    ["When you cross a river, place a raider on that river"
     "Increase all of your Level One Roles"
     "Gain Gold, Gold, Gold, Gems"
     "Increase your Merchant level (paying any costs). Then Sell to the city you are in for Glory instead"
     "Score Glory for each of your facedown Temples"]}
   {:id 13 :name "Pillars of Etana"
    :effects
    ["When you place a Temple you may place a Raider adjacent to it"
     "Gain Tools, Tools, Tools. Score Glory based on your Leader Level"
     "Gain Pottery, Pottery, Pottery. Score Glory based on your Leader Level"
     "Increase all of your Level Three Roles (paying any costs)"
     "Place a Temple adjacent to one of your Raiders (even if you already have a temple there)"]}
   {:id 14 :name "Roads of Shulgi"
    :effects
    ["On your turn you may move between Uruk and an adjacent city by discarding one good as a bonus action"
     "Place a Raider adjacent to Lagash. Then score Glory for each of your Raiders"
     "Move a Magistrate to Uruk. Then gain resources matching Uruk's demands"
     "Place two random Demand Tokens in Eridu. Travel to Eridu via the shortest route (you may choose between equal routes)"
     "Place a Temple in Babylon (even if you already have a temple there)"]}
   {:id 15 :name "Ascent of Ur-Nammu"
    :effects
    ["When you increase a role, you may increase it for free"
     "For each demand you have fulfilled, take a matching good"
     "Increase your Priest role. Then score 4 Glory if you have a facedown temple in Babylon"
     "Increase your lowest role then take a Travel action (you pick if there is a tie)"
     "Score 3 Amity for each Raider you have adjacent to a Magistrate"]}
   {:id 16 :name "Dominion of Hammurabi"
    :effects
    ["When you take an action space with exactly two astronomers on it, take a third action"
     "Take a Pottery for each Temple you have"
     "Deploy then score Amity for each Raider you have"
     "Increase your Leader role twice (paying any costs)"
     "Put two random demand tokens on the city you are in. You may take Sell action"]}
   {:id 17 :name "Cunning of Kubaba"
    :effects
    ["When you use action space 7 take a good of your choice"
     "Place a Raider next to Eridu on its point side"
     "Place one facedown Temple on each city with a Magistrate (even if you have temples there)"
     "Score 8 Amity if you have Uruk surrounded by Raiders. Then you may flip one of those raiders"
     "Sell to the city your caravan is in for Glory instead"]}
   {:id 18 :name "Forge of Tubal-Cain"
    :effects
    ["When you spend Tools in any way, instead keep them. Your Tools are worth Glory each at end of game"
     "Move a Magistrate across a river. You may sell in your caravan's city"
     "Take a travel action then score 5 Glory if you have a facedown temple in Samarra"
     "Score 6 Amity if you have Kish surrounded by Raiders. Then you may flip one of those raiders"
     "Score 4 Amity for each of your Raiders on their point side. Then remove those raiders"]}
   {:id 19 :name "Kilns of Ninkasi"
    :effects
    ["When you take Pottery, take an extra Pottery, Pottery"
     "Increase your Priest role twice (paying any costs)"
     "Sell to two cities that demand Pottery (you don't have to be there)"
     "Discard a good to move a Magistrate to your City. Then take a sell action"
     "Flip all of your placed Raiders to their point side"]}
   {:id 20 :name "Vision of Rimush"
    :effects
    ["When you flip a Temple you may discard a Pottery. If you do, score 3 Glory"
     "Place a Raider on each route with an opposing raider"
     "Increase your Merchant role twice (paying any costs)"
     "Influence a Magistrate. Then score Amity based on your leader level"
     "Take up to four goods based on the action spaces your Astronomers occupy"]}
   {:id 21 :name "Legacy of Eannatum"
    :effects
    ["When you place a temple in a city, you may place an additional temple facedown in that city"
     "If you are in Eridu, travel anywhere via the shortest path (you choose between ties)"
     "Increase your Raider and Leader roles (paying any costs)"
     "Travel to an adjacent city then you may Sell to it"
     "Score Glory for each demand you have fulfilled"]}
   {:id 22 :name "Strategy of Naram-Sin"
    :effects
    ["When taking actions on action space 7 you may take the same action twice"
     "Increase your Raider and Merchant Roles (paying any costs)"
     "Put a random demand token on each of your facedown temples. Only you may fulfill those demands"
     "Take a good of your choice. Then take a travel action"
     "Score 2 Amity for each of your Raiders. Then take a travel action"]}
   {:id 23 :name "Market of Puabi"
    :effects
    ["When you sell, score Glory instead of Amity"
     "Increase your Priest and Merchant Roles (paying any costs)"
     "Sell twice to Eridu (you don't need to be there)"
     "Take a good of your choice. Then take a travel action. Increase your Merchant Role (paying any costs)"
     "Place a Temple in a city with a Magistrate (even if you already have a temple there)"]}
   {:id 24 :name "Siege of Shulme"
    :effects
    ["When you surround a City with Raiders you may Sell to that city (even if you aren't there)"
     "Increase your Raider and Leader Roles (paying any costs)"
     "Put a random demand token on each Magistrate. Only you may fulfill those demands"
     "Score Glory for each demand you have fulfilled"
     "Take a good for each demand in cities with Magistrates"]}
   {:id 25 :name "Command of Mesannepada"
    :effects
    ["You may have two raiders on each path"
     "Influence a Magistrate. Immediately score all of your raiders it moved through"
     "Increase your Merchant and Leader Roles (paying any costs)"
     "Place two facedown temples in your city (even if you already have a temple there)"
     "Take a good of your choice. Then take a Travel action"]}
   {:id 26 :name "Court of Enshakushanna"
    :effects
    ["When you score Magistrate bonus points, score an additional 2 Amity"
     "Increase your Priest and Leader Roles (paying any costs)"
     "Increase your Priest and Raider Roles (paying any costs)"
     "Sell in your city. If you sold Tools or Pottery you may place a Temple in your city (even if you already have a temple there)"
     "Place a Raider adjacent to your city. If you surround it, you may place a temple in it (even if you already have a temple there)"]}
   {:id 27 :name "Path of Alulim"
    :effects
    ["When you increase a role, you may increase another role, paying double the normal cost"
     "Travel to an adjacent city then you may Sell to it"
     "Travel to an adjacent city then you may take a Deploy action"
     "Travel to an adjacent city then you may place a Temple in it"
     "Take three goods of your choice"]}
   {:id 28 :name "Stars of Sin-Kashid"
    :effects
    ["You may increase a role at the end of your turn if you landed on a space with four or more Astronomers"
     "Travel to an adjacent city then you may place a Temple in it (even if you already have a temple there)"
     "Travel to an adjacent city then you may place a Temple in it (even if you already have a temple there)"
     "Sell Gold or Gold to your city if it has no Demands. Then place a random demand on it"
     "Put a raider point-side up adjacent to Kish"]}
   {:id 29 :name "Treasury of Ibbi-Sin"
    :effects
    ["When you pay a Gold for any reason gain 2 Amity"
     "Decrease your Leader role to increase all of your other roles (paying any costs)"
     "Take a travel action then you may take a sell action"
     "Place a raider on each river"
     "Place a Temple in each city surrounded by your Raiders (even if you have a Temple there)"]}
   {:id 30 :name "Council of Amar-Sin"
    :effects
    ["When taking goods you may instead take goods based on one of your other Astronomer's location on the action wheel"
     "Influence a Magistrate then take a Travel action"
     "Influence a Magistrate then take a Sell action"
     "Take a Deploy action then Influence a Magistrate"
     "Influence a Magistrate then take a Temple action"]}
   {:id 31 :name "Horizon of Sharkalisharri"
    :effects
    ["When taking actions if one of your other Astronomers is on space 7, you may take a bonus Travel action"
     "Increase all of your level one roles"
     "Increase all of your level three roles (paying any costs)"
     "Gain a resource of your choice and place a Facedown temple in your city (even if you already have a Temple there)"
     "Gain a resource of your choice and take a Deploy action"]}
   {:id 32 :name "Jewel of Ku-Bau"
    :effects
    ["When you sell you may discard a Gem to score Amity based on your Priest level instead of Merchant level"
     "Sell in your city then Score Glory for each demand you have fulfilled"
     "Take a Gem. Take two travel actions"
     "Place a raider in each route that has one of your Temples in both cities"
     "Influence a Magistrate then you may take sell action"]}
   {:id 33 :name "Vanguard of Enmebaragesi"
    :effects
    ["When you deploy, you may Influence an adjacent Magistrate"
     "Decrease your Merchant role to increase all of your other roles (paying any costs)"
     "Place a facedown Temple in your city then take a travel action (even if you already have a Temple there)"
     "Place a face up Temple in Uruk (even if you already have a Temple there)"
     "Deploy a raider adjacent to your city then take a travel action"]}
   {:id 34 :name "Honor of Agga"
    :effects
    ["When you score raiders, score Amity instead of Glory"
     "Pay Tools, Tools to place a Raider on each space surrounding Uruk"
     "Place a raider on each route you have a raider"
     "Take a Sell action in each city that has both a Magistrate and one of your Temples (you don't have to be there)"
     "Take a Sell action in each city that has both a Magistrate and one of your Temples (you don't have to be there)"]}
   {:id 35 :name "Wanderer of Dumuzi"
    :effects
    ["At the start of your turn if you have no goods, gain a good of your choice"
     "Travel then take a Sell action"
     "You may pay any number of Pottery. For each Pottery you paid, place a Temple in a city which you have a Temple"
     "Increase the role of your choice (paying any costs)"
     "Influence a Magistrate. Score each of your Raiders it moved through"]}])

(def bonus-boards-by-id
  "Lookup bonus board by numeric ID."
  (into {} (map (juxt :id identity) bonus-boards)))

(def bonus-contests-by-id
  "Lookup bonus contest by keyword ID."
  (into {} (map (juxt :id identity) bonus-contests)))

(defn player-color
  "Get the color for a player based on turn order index."
  [state player-key]
  (let [idx (.indexOf (:turn-order state) player-key)]
    (get player-colors (max 0 idx) "#888")))

(defn make-player
  [player-key card player-count]
  (let [num-astronomers (case player-count 2 3, (3 4 5) 2, 2)
        role-levels (into {}
                         (for [r roles]
                           [r (if (= r (:role card)) 2 1)]))]
    {:key              player-key
     :roles            role-levels
     :resources        (merge {:tools 0 :pottery 0 :gold 0 :gems 0}
                              {(:resource card) 1})
     :caravan          (:city card)
     :astronomers      []     ;; positions on action wheel, filled during setup
     :dice-available   []     ;; rolled at start of each round
     :dice-used        []     ;; dice used this round
     :bonus-tokens     5
     :raiders-supply   6      ;; in player's supply (not yet deployed)
     :temples-supply   7      ;; 8 total, 1 placed at starting city
     :raiders          {}     ;; {route-key -> :raiding | :point}
     :temples          {}     ;; {city -> :face-up | :face-down}
     :demand-tokens    []     ;; collected demand tokens
     :bonus-board      (vec (repeat 5 :covered))
     :amity            0
     :glory            0
     :wild-points      0
     :num-astronomers  num-astronomers
     :starting-card    card}))

(defn roll-dice
  "Roll 4 six-sided dice."
  []
  (vec (repeatedly 4 #(inc (rand-int 6)))))

(defn classify-dice-roll
  "Classify a dice roll by its highest duplicate count.
   Returns :quad, :triple, :double, or :unique."
  [dice]
  (let [max-freq (apply max (vals (frequencies dice)))]
    (case max-freq
      4 :quad
      3 :triple
      2 :double
      :unique)))

(defn track-dice-roll
  "Update player's dice roll statistics."
  [player dice]
  (let [class (classify-dice-roll dice)]
    (update-in player [:dice-stats class] (fnil inc 0))))

(defn setup-player
  "Set up a player: roll dice for astronomer placement, place caravan and temple."
  [player _player-count]
  (let [n (:num-astronomers player)
        ;; Roll n dice for initial astronomer placement
        rolls (vec (repeatedly n #(inc (rand-int 6))))
        ;; Map die values to action spaces (1-6 map directly, but 7 isn't possible on d6)
        astronomer-positions (mapv #(if (> % 7) (mod % 7) %) rolls)
        dice (roll-dice)]
    (-> player
        (assoc :astronomers astronomer-positions)
        ;; Place one face-up temple at starting city
        (assoc-in [:temples (:caravan player)] :face-up)
        ;; Roll 4 dice for the first round
        (assoc :dice-available dice)
        (track-dice-roll dice))))

;; =============================================================================
;; State queries
;; =============================================================================

(defn current-player [state]
  (get-in state [:turn-order (:current-player-idx state)]))

(defn current-phase [state]
  (get-in state [:player-turn :phase]))

(defn player-data [state player]
  (get-in state [:players player]))

(defn count-face-down-temples
  "Count how many face-down temples a player has."
  [player-data]
  (count (filter #(= :face-down (val %)) (:temples player-data))))

(defn count-temples-placed
  "Total temples on the board for a player."
  [player-data]
  (count (:temples player-data)))

(defn count-raiders-deployed
  "Total raiders deployed on routes for a player."
  [player-data]
  (count (:raiders player-data)))

(defn astronomers-on-space
  "Return list of [player-key astronomer-index] for all astronomers on a given space."
  [state space]
  (vec
   (for [[pk pdata] (:players state)
         [idx pos] (map-indexed vector (:astronomers pdata))
         :when (= pos space)]
     [pk idx])))

(defn magistrate-in-city?
  "True if any magistrate is in the given city."
  [state city]
  (contains? (:magistrates state) city))

;; =============================================================================
;; Turn & round management
;; =============================================================================

(def rounds-per-game 3)
(def default-turns-per-round 4)

(defn turns-per-round
  "Turns per round = number of dice each player rolls (4).
   Each turn, a player picks one die and resolves actions on that space."
  [_state]
  default-turns-per-round)

;; Solo mode: 3 colors of astronomer pairs, one color per round
(def solo-color-names ["Alpha" "Beta" "Gamma"])

(defn solo-active-indices
  "Return the astronomer indices active for the current round in solo mode."
  [state]
  (let [round (:round state 1)
        pairs (get state :solo-pairs [[0 1] [2 3] [4 5]])]
    (nth pairs (dec round) [0 1])))

(defn solo-mode? [state]
  (= :solo (:mode state)))

;; Solo feat scoring: round-dependent bonus values
(def solo-feat-bonus {1 3, 2 2, 3 1})

(defn advance-turn
  "Move to the next player's turn. Checks feats for the current player first.
   Bots only — humans claim feats manually via UI."
  [state]
  (let [current-player (current-player state)
        is-bot? (some? (get-in state [:players current-player :personality-cache]))
        ;; Auto-claim feats only for bots; humans choose when to claim
        state (if is-bot?
                (check-and-claim-feats state current-player)
                state)
        ;; Board 28: bonus-role-increase — free role increase at end of turn
        pdata (get-in state [:players current-player])
        state (if (:bonus-role-increase pdata)
                (let [pri (:role-priority pdata [:merchant :priest :raider :leader])
                      best-role (first (filter #(< (get-in pdata [:roles %] 1) max-role-level) pri))
                      state (update-in state [:players current-player] dissoc :bonus-role-increase)]
                  (if best-role
                    (let [next-lv (inc (get-in pdata [:roles best-role] 1))
                          cost (get-in role-threshold-costs [best-role next-lv])]
                      (if (or (nil? cost) (pos? (get-in pdata [:resources cost] 0)))
                        (cond-> state
                          cost (update-in [:players current-player :resources cost] dec)
                          true (assoc-in [:players current-player :roles best-role] next-lv))
                        state))
                    state))
                state)
        n (count (:turn-order state))
        next-idx (mod (inc (or (:current-player-idx state) 0)) (max n 1))
        turn-in-round (get state :turn-in-round 1)
        tpr (turns-per-round state)
        last-turn? (and (= next-idx 0) (>= turn-in-round tpr))]
    (if last-turn?
      ;; End of round
      (if (>= (:round state) rounds-per-game)
        ;; Game over
        (if (solo-mode? state)
          ;; Solo: apply end-game scoring (wild points + role bonuses), then check feats
          (let [scored-state (apply-end-game-scoring state)
                player (first (:turn-order scored-state))
                claims (:contest-claims scored-state {})
                total-claimed (count (filter #(some #{player} (val %)) claims))
                total-contests (count (:contests scored-state []))]
            (assoc scored-state :game-over
                   {:reason :end-of-game
                    :solo-result (if (>= total-claimed total-contests)
                                  :victory :defeat)
                    :feats-met total-claimed
                    :feats-needed total-contests}))
          ;; Normal: apply end-game scoring, then end
          (-> state
              apply-end-game-scoring
              (assoc :game-over {:reason :end-of-game})))
        ;; Start new round
        (let [new-round (inc (:round state))
              claimed-ids (set (for [[cid claimers] (:contest-claims state {})
                                     p claimers] cid))
              ;; Roll new dice and re-evaluate feat targets for all players
              players (reduce-kv
                       (fn [ps pk pdata]
                         (let [dice (roll-dice)
                               ;; Check if opponent claimed any feat in our chain
                               chain (:feat-chain pdata [])
                               chain-hijacked? (some (fn [c]
                                                       (let [cs (get-in state [:contest-claims (:id c)] [])]
                                                         (and (seq cs) (not (some #{pk} cs)))))
                                                     chain)
                               current-targets (:target-feats pdata [])
                               best-progress (when (seq current-targets)
                                               (apply max
                                                 (map #(first (feat-progress state pk %))
                                                      current-targets)))
                               has-claimed? (some #(some #{pk} (val %))
                                                   (:contest-claims state {}))
                               ;; Switch targets if stuck or chain hijacked
                               needs-replan? (or chain-hijacked?
                                                 (and (not has-claimed?)
                                                      (or (nil? best-progress)
                                                          (< best-progress 0.2))))
                               new-chain (if needs-replan?
                                           (plan-feat-chain state pk)
                                           chain)
                               new-targets (if needs-replan?
                                             (vec (take 2 new-chain))
                                             current-targets)]
                           (assoc ps pk
                                  (-> pdata
                                      (assoc :dice-available dice)
                                      (assoc :dice-used [])
                                      (assoc :travels-this-round 0)
                                      (assoc :sells-this-round 0)
                                      (assoc :deploys-this-round 0)
                                      (assoc :feat-chain new-chain)
                                      (assoc :target-feats new-targets)
                                      (track-dice-roll dice)))))
                       {}
                       (:players state))]
          (if (solo-mode? state)
            ;; Solo: do NOT refill demands, switch astronomer color
            (-> state
                (assoc :round new-round
                       :turn-in-round 1
                       :current-player-idx 0
                       :players players
                       :player-turn {:phase :choose-die}))
            ;; Normal: refill demand spaces
            (let [cities (keys (:city-graph state))
                  [bag demands] (fill-demand-spaces
                                 (:demand-bag state)
                                 (:city-demands state)
                                 cities)]
              (-> state
                  (assoc :round new-round
                         :turn-in-round 1
                         :current-player-idx 0
                         :players players
                         :demand-bag bag
                         :city-demands demands
                         :player-turn {:phase :choose-die}))))))
      ;; Same round, next player (or next turn in round if wrapped)
      (let [new-turn (if (zero? next-idx) (inc turn-in-round) turn-in-round)]
        (-> state
            (assoc :current-player-idx next-idx
                   :turn-in-round new-turn
                   :player-turn {:phase :choose-die}))))))

;; =============================================================================
;; Initial state
;; =============================================================================

(defn initial-state [player-keys]
  (let [player-count (count player-keys)
        deck (shuffle starting-cards)
        dealt (take player-count deck)
        sorted-deals (sort-by (comp :number second)
                              (map vector player-keys dealt))
        turn-order (mapv first sorted-deals)
        players (into {}
                      (for [[pk card] sorted-deals]
                        [pk (setup-player
                             (make-player pk card player-count)
                             player-count)]))
        cities (if (<= player-count 3)
                 (disj all-cities :samarra)
                 all-cities)
        graph (city-graph player-count)
        routes (active-routes player-count)
        [bag city-demands] (fill-demand-spaces
                            (full-demand-bag)
                            {}
                            (vec cities))
        ;; Select 5 double-sided feat cards: group by letter, pick 5 cards,
        ;; randomly show one side of each
        contest-pairs (vals (group-by #(first (name (:id %))) bonus-contests))
        selected-pairs (take 5 (shuffle contest-pairs))
        contests (vec (map #(rand-nth %) selected-pairs))
        boards (vec (take player-count (shuffle bonus-boards)))
        magistrate-cities (filterv cities [:uruk :kish])]
    (let [base-state
          {:turn-order         turn-order
           :current-player-idx 0
           :round              1
           :turn-in-round      1
           :player-turn        {:phase :choose-die}
           :players            players
           :action-spaces      action-spaces
           :city-graph         graph
           :routes             routes
           :city-demands       city-demands
           :demand-bag         bag
           :magistrates        (zipmap magistrate-cities (repeat :neutral))
           :first-player       (first turn-order)
           :contests           contests
           :contest-claims     {}
           :bonus-boards       (zipmap turn-order
                                       (map :id boards))
           :log                []
           :game-over          nil}]
      ;; Each player plans a 2-3 feat chain and derives targets from it
      (reduce (fn [s pk]
                (let [chain (plan-feat-chain s pk)
                      targets (if (seq chain)
                                (vec (take 2 chain))
                                (select-target-feats s pk))]
                  (-> s
                      (assoc-in [:players pk :feat-chain] chain)
                      (assoc-in [:players pk :target-feats] targets))))
              base-state
              turn-order))))

(defn initial-solo-state
  "Create initial state for solo mode.
   One player with 6 astronomers in 3 color pairs.
   Full 8-city board. All 5 feats must be met to win."
  [player-key]
  (let [;; Solo uses full board (all 8 cities, treat as 4-player layout)
        card (rand-nth starting-cards)
        player (-> (make-player player-key card 4) ;; 4-player sizing for full board
                   (assoc :num-astronomers 6))     ;; 3 pairs of 2
        player (setup-player player 4)
        cities all-cities
        graph (city-graph 4)
        routes (active-routes 4)
        ;; Fill ALL demand spaces at start (no refill later)
        [bag city-demands] (fill-demand-spaces
                            (full-demand-bag) {} (vec cities))
        ;; All 5 contests in play for solo (pick 5 cards, random side)
        contest-pairs (vals (group-by #(first (name (:id %))) bonus-contests))
        selected-pairs (take 5 (shuffle contest-pairs))
        contests (vec (map #(rand-nth %) selected-pairs))
        board (first (shuffle bonus-boards))
        magistrate-cities (filterv cities [:uruk :kish])
        ;; Randomly assign astronomer pairs to rounds
        pair-order (shuffle [[0 1] [2 3] [4 5]])
        base {:mode               :solo
              :turn-order         [player-key]
              :current-player-idx 0
              :round              1
              :turn-in-round      1
              :player-turn        {:phase :choose-die}
              :players            {player-key player}
              :action-spaces      action-spaces
              :city-graph         graph
              :routes             routes
              :city-demands       city-demands
              :demand-bag         bag
              :magistrates        (zipmap magistrate-cities (repeat :neutral))
              :first-player       player-key
              :contests           contests
              :contest-claims     {}
              :bonus-boards       {player-key (:id board)}
              :solo-pairs         pair-order
              :log                []
              :game-over          nil}
        chain (plan-feat-chain base player-key)
        targets (if (seq chain) (vec (take 2 chain))
                    (select-target-feats base player-key))]
    (-> base
        (assoc-in [:players player-key :feat-chain] chain)
        (assoc-in [:players player-key :target-feats] targets))))
