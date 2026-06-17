(ns eridu.game
  (:require [clojure.set]
            [eridu.bonus :as bonus]
            [eridu.cards :as cards]))

;; Forward declarations for functions used by feat evaluation and bonus board effects
(declare count-temples-placed count-face-down-temples count-raiders-deployed
         raiders-on raider-on-route? all-raider-states total-raiders
         count-raiders-with-status place-one-raider flip-raiders-on-route-to-point
         flip-one-raider-to-point score-one-point-raider
         board-routes
         temples-at has-temple? all-temple-states temple-cities add-temple flip-one-temple
         magistrate-in-city? magistrate-cities routes-from-city current-player player-data
         rounds-per-game advance-turn
         ;; Helpers used by apply-passive slot-0 dispatch (defined later in file)
         sell-good-in-city bonus-sell-in bonus-influence perform-influence road-clockwise-next
         ;; Travel resolution (FIX 1: moved here from choice.cljc; used by
         ;; apply-passive board-5 and the apply-bonus-dispatch travel arms)
         add-log add-amity add-glory add-resource spend-resource
         travel-to-city shortest-city-path bonus-travel-to
         visit-temples-on-travel flip-enemy-raiders-on-route score-own-raider-on-route
         city-has-own-face-up-temple? leader-bonus
         ;; Constants used by apply-passive and bonus board effects
         role-threshold-costs merchant-score raider-max-deployed priest-max-temples
         resource-types roles max-role-level active-routes route-key segment-route-key
         eligible-cities-for-filter)

;; =============================================================================
;; Canonical protected-phase sets
;; =============================================================================

(def human-protected-phases
  "Phases where human players make meaningful choices. Used for live play and replay."
  #{:choose-die :choose-astronomer :choose-action :choose-role-increase
    :resolve-sell :resolve-temple :resolve-deploy
    :resolve-travel :travel-continue :resolve-influence
    :turn-complete :game-over})

(def bot-protected-phases
  "Phases where bots pause for broadcast visibility."
  #{:choose-die :choose-action :resolve-landing :game-over})

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

(defn- apply-passive-dispatch
  "Dispatch passive effect by [board-id trigger-type].
   Returns updated state."
  [state player-key board-id pdata trigger-type context]
  (case [board-id trigger-type]

        ;; ── Board 1: When you surround a city with Raiders, temple in it ──
        ;; (checked after deploy — context has :city if surrounded)
        [1 :deployed]
        (if-let [surrounded-city (:surrounded-city context)]
          (let [pdata (get-in state [:players player-key])]
            (if (and (not (has-temple? pdata surrounded-city))
                     (pos? (:temples-supply pdata 0)))
              (add-temple state player-key surrounded-city :face-up)
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

        ;; ── Board 3: Gems worth Amity each at end of game ─────────────────
        [3 :end-game]
        (update-in state [:players player-key :amity]
                   + (get-in state [:players player-key :resources :gems] 0))

        ;; ── Board 4: When you flip a temple, may sell in that city ────────
        [4 :temple-flipped]
        (let [city (:city context)
              demands (get-in state [:city-demands city] [])
              resources (:resources pdata)
              sellable (first (filter #(pos? (get resources % 0)) demands))]
          (if sellable
            (sell-good-in-city state player-key city sellable)
            state))

        ;; ── Board 5: Influence magistrate in your city → travel with it ──
        ;; Fired by resolve-influence-choices with {:from <mag-origin> :to <mag-dest>}.
        ;; If the magistrate was pushed out of the city the caravan is standing in,
        ;; the player MAY travel along with it — auto-applied (free, beneficial).
        [5 :magistrate-moved]
        (let [from (:from context)
              to (:to context)
              caravan (get-in state [:players player-key :caravan])]
          (if (and from to (= from caravan))
            ;; FIX 1: travel WITH the magistrate via real travel resolution
            ;; (raider/temple/river effects fire), not a caravan teleport.
            (bonus-travel-to state player-key to)
            state))

        ;; ── Board 6: Action space 7 → free Travel action ─────────────────
        ;; (tracked via flag, actual travel handled by choice.cljc)
        [6 :action-space-7]
        (assoc-in state [:players player-key :pending-free-travel] true)

        ;; ── Board 7: When you deploy, extra raider next to Magistrate ─────
        [7 :deployed]
        (let [mag-cities (magistrate-cities state)
              pdata (get-in state [:players player-key])
              raider-lv (get-in pdata [:roles :raider] 1)
              max-deployed (get raider-max-deployed raider-lv 2)
              deployed (count-raiders-deployed pdata)
              supply (:raiders-supply pdata 0)]
          (if (and (pos? supply) (< deployed max-deployed))
            ;; Find a route adjacent to a magistrate that we don't have a raider on
            (let [routes (for [mc mag-cities
                               r (routes-from-city mc (board-routes state))
                               :let [rk (segment-route-key r)]
                               :when (not (raider-on-route? (:raiders pdata) rk))]
                           rk)]
              (if (seq routes)
                (-> state
                    (place-one-raider player-key (first routes) :raiding)
                    (update-in [:players player-key :raiders-supply] dec))
                state))
            state))

        ;; ── Board 8: Score raider → flip to active instead of removing ────
        ;; Set flag so score-own-raider-on-route flips to :raiding instead of removing.
        [8 :raider-scored]
        (assoc-in state [:players player-key :keep-scored-raider] true)

        ;; ── Board 9: Flip temple → increase a role (player choice) ────────
        [9 :temple-flipped]
        (let [upgradeable (filter #(< (get-in pdata [:roles %] 1) 5) roles)
              affordable  (filter (fn [r]
                                    (let [next-lv (inc (get-in pdata [:roles r] 1))
                                          cost (get-in role-threshold-costs [r next-lv])]
                                      (or (nil? cost)
                                          (pos? (get-in pdata [:resources cost] 0)))))
                                  upgradeable)]
          (if (seq affordable)
            (assoc-in state [:players player-key :passive-choice-needed]
                      {:type :pick-role :board-id 9
                       :prompt "Flip temple: increase a role"
                       :options (vec affordable)})
            state))

        ;; ── Board 10: Sell gold to empty demand cities ────────────────────
        ;; Action-choice rule change: the extra "sell gold to a demand-free
        ;; city" option lives in choice/resolve-sell-choices (:sell-gold-empty),
        ;; not in this post-event dispatch. No-op here.
        [10 :sold] state

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
                   (not (raider-on-route? (:raiders pdata) rk)))
            (-> state
                (place-one-raider player-key rk :raiding)
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
              adj-routes (routes-from-city city (board-routes state))
              free-route (first (for [r adj-routes
                                      :let [rk (segment-route-key r)]
                                      :when (not (raider-on-route? (:raiders pdata) rk))]
                                  rk))]
          (if (and free-route (pos? supply) (< deployed max-deployed))
            (-> state
                (place-one-raider player-key free-route :raiding)
                (update-in [:players player-key :raiders-supply] dec))
            state))

        ;; ── Board 14: Uruk bonus travel action ───────────────────────────
        ;; Action-choice rule change: the [:uruk-move dest] bonus moves are
        ;; offered in choice/choose-action-choices (gated on :used-uruk-travel,
        ;; cleared at turn start), not via this trigger. No-op here.
        [14 :turn-start] state

        ;; ── Board 15: Role increase → ignore threshold costs ───────────────
        ;; Set flag so choose-role-increase-choices offers roles without cost check.
        [15 :role-increased]
        (assoc-in state [:players player-key :free-role-increase] true)

        ;; ── Board 16: 2-astronomer space → third action ──────────────────
        ;; (tracked as flag, handled in choose-action)
        [16 :landing]
        (if (= 2 (:astronomer-count context))
          (assoc-in state [:players player-key :bonus-extra-action] true)
          state)

        ;; ── Board 17: Action space 7 → take a good of choice ─────────────
        [17 :action-space-7]
        (assoc-in state [:players player-key :passive-choice-needed]
                  {:type :pick-resource :board-id 17
                   :prompt "Action space 7: take a good of choice"})

        ;; ── Board 18: Keep tools when spent + tools worth glory at end ────
        [18 :end-game]
        (let [tools (get-in state [:players player-key :resources :tools] 0)]
          (update-in state [:players player-key :glory] + tools))

        ;; ── Board 18: Tools never consumed — refund tool when spent ───────
        [18 :resource-spent]
        (if (= :tools (:resource context))
          (update-in state [:players player-key :resources :tools] (fnil inc 0))
          state)

        ;; ── Board 19: Take pottery → extra pottery x2 ────────────────────
        [19 :goods-taken]
        (if (some #{:pottery} (:resources context))
          (update-in state [:players player-key :resources :pottery] + 2)
          state)

        ;; ── Board 20: Flip temple → discard pottery for 3 glory ───────────
        [20 :temple-flipped]
        (if (pos? (get-in pdata [:resources :pottery] 0))
          (assoc-in state [:players player-key :passive-choice-needed]
                    {:type :yes-no :board-id 20
                     :prompt "Discard pottery for 3 glory?"})
          state)

        ;; ── Board 21: Place temple → extra facedown in same city ──────────
        ;; Multi-temple model: genuinely add a 2nd (facedown) temple in the city
        ;; that triggered this placement. NOTE: add-temple itself fires
        ;; :temple-placed; with this board active that would recurse forever, so
        ;; we conj/dec directly here (no re-trigger).
        [21 :temple-placed]
        (let [city (:city context)]
          (if (and city (pos? (:temples-supply pdata 0)))
            (-> state
                (update-in [:players player-key :temples city] (fnil conj []) :face-down)
                (update-in [:players player-key :temples-supply] dec))
            state))

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
        ;; When a deploy surrounds a city, you MAY sell to it even if not present.
        ;; Auto-applied (beneficial); bonus-sell-in no-ops if no good matches a
        ;; demand there, which is fine. No travel.
        [24 :deployed]
        (if-let [surrounded-city (:surrounded-city context)]
          (bonus-sell-in state player-key surrounded-city)
          state)

        ;; ── Board 25: Two raiders per path ────────────────────────────────
        ;; Set flag so resolve-deploy-choices allows placing on occupied routes.
        [25 :deployed]
        (assoc-in state [:players player-key :allow-double-raiders] true)

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
              other-roles (remove #{(:role context)} roles-available)
              ;; Only offer roles the player can afford at double cost
              affordable (filter (fn [r]
                                   (let [next-lv (inc (get-in state [:players player-key :roles r] 1))
                                         cost (get-in role-threshold-costs [r next-lv])]
                                     (or (nil? cost)
                                         (>= (get-in state [:players player-key :resources cost] 0) 2))))
                                 other-roles)]
          (if (seq affordable)
            (assoc-in state [:players player-key :passive-choice-needed]
                      {:type :pick-role :board-id 27
                       :prompt "Increase another role for double cost"
                       :options (vec affordable)
                       :context {:double-cost true}})
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
        ;; Action-choice rule change: the [:alt-take space] options are offered
        ;; in choice/resolve-take-choices, not via this post-event trigger.
        ;; No-op here.
        [30 :goods-taken] state

        ;; ── Board 31: Other astronomer on space 7 → bonus travel ──────────
        ;; On landing, if one of your OTHER astronomers is on space 7, you MAY
        ;; take a bonus Travel action. Count astronomers on space 7; the one we
        ;; just landed (when :space is 7) is the "current" action, not an "other".
        ;; Auto-grant via :pending-free-travel (same flag board 6 uses; consumed
        ;; in choice.cljc's resolve-landing-choices as an injected free travel).
        [31 :landing]
        (let [astros (get-in state [:players player-key :astronomers] [])
              on-7 (count (filter #(= 7 %) astros))
              others-on-7 (if (= 7 (:space context)) (dec on-7) on-7)]
          (if (>= others-on-7 1)
            (assoc-in state [:players player-key :pending-free-travel] true)
            state))

        ;; ── Board 32: Sell → discard gem for priest-level scoring ─────────
        [32 :sold]
        (if (pos? (get-in pdata [:resources :gems] 0))
          (let [priest-lv (get-in pdata [:roles :priest] 1)
                merchant-lv (get-in pdata [:roles :merchant] 1)]
            (if (> priest-lv merchant-lv)
              (assoc-in state [:players player-key :passive-choice-needed]
                        {:type :yes-no :board-id 32
                         :prompt (str "Discard gem for priest-level scoring? (priest "
                                      priest-lv " vs merchant " merchant-lv ")")
                         :context {:amity-scored (:amity-scored context 0)}})
              state))
          state)

        ;; ── Board 33: Deploy → influence adjacent magistrate ──────────────
        ;; When you deploy, you MAY Influence a magistrate sitting in either
        ;; endpoint city of the route you just deployed on.
        ;; HEURISTIC: auto-influence exactly 1 step clockwise (the minimal free
        ;; beneficial default for the "may" — it flips raiders on the single route
        ;; the magistrate crosses). If multiple magistrates are on endpoints, the
        ;; first found is influenced. If none, no-op.
        [33 :deployed]
        (let [rk (:route context)
              [c1 c2] (when (and rk (vector? rk)) rk)
              endpoints #{c1 c2}
              active-cities (set (keys (:city-graph state)))
              mag-entry (first (filter (fn [[_ city]] (contains? endpoints city))
                                       (:magistrates state)))]
          (if mag-entry
            (let [[mag-id mag-city] mag-entry
                  dest (road-clockwise-next mag-city active-cities)]
              (if dest
                (perform-influence state player-key mag-id dest 1)
                state))
            state))

        ;; ── Board 34: Score raiders → amity instead of glory ──────────────
        ;; Set flag so score-own-raider-on-route adds amity instead of glory.
        [34 :raider-scored]
        (assoc-in state [:players player-key :raider-score-amity] true)

        ;; ── Board 35: Start of turn, no goods → gain good of choice ───────
        [35 :turn-start]
        (let [resources (:resources pdata)
              total (reduce + (vals resources))]
          (if (zero? total)
            (assoc-in state [:players player-key :passive-choice-needed]
                      {:type :pick-resource :board-id 35
                       :prompt "No goods — gain a good of choice"})
            state))

        ;; Default: no matching passive for this trigger
        state))

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
          state (update-in state [:players player-key :passive-triggers-log]
                           (fnil conj [])
                           {:round (:round state 1)
                            :board-id board-id
                            :trigger trigger-type})
          pdata (get-in state [:players player-key])]
      (apply-passive-dispatch state player-key board-id pdata trigger-type context))))

(defn apply-passive-choice
  "Resolve a pending passive choice. Called when the player picks an option.
   choice-val: keyword (resource or role) or boolean (for yes/no).
   Returns updated state with :passive-choice-needed removed."
  [state player-key choice-val]
  (let [pending (get-in state [:players player-key :passive-choice-needed])
        board-id (:board-id pending)
        pdata (get-in state [:players player-key])
        state (update-in state [:players player-key] dissoc :passive-choice-needed)]
    (if-not pending
      state
      (case board-id
        ;; Board 9: increase chosen role (pay cost)
        9 (let [role choice-val
                next-lv (inc (get-in pdata [:roles role] 1))
                cost (get-in role-threshold-costs [role next-lv])]
            (cond-> state
              cost (update-in [:players player-key :resources cost] dec)
              true (assoc-in [:players player-key :roles role] next-lv)))

        ;; Board 17: gain chosen resource
        17 (update-in state [:players player-key :resources choice-val] (fnil inc 0))

        ;; Board 20: yes → discard pottery for 3 glory; no → nothing
        20 (if (= choice-val :yes)
             (-> state
                 (update-in [:players player-key :resources :pottery] dec)
                 (update-in [:players player-key :glory] + 3))
             state)

        ;; Board 27: increase chosen role at double cost
        27 (let [role choice-val
                 next-lv (inc (get-in pdata [:roles role] 1))
                 cost (get-in role-threshold-costs [role next-lv])]
             (cond-> state
               cost (update-in [:players player-key :resources cost] - 2)
               true (assoc-in [:players player-key :roles role] next-lv)))

        ;; Board 32: yes → discard gem, add priest-level amity minus merchant amity
        ;; QA lesson 7 fix: priest-amity is (merchant-score priest-lv), NOT the
        ;; raw level number. Old code under-scored by 1 in 6 of 10 valid cases.
        32 (if (= choice-val :yes)
             (let [priest-lv          (get-in pdata [:roles :priest] 1)
                   priest-amity       (get merchant-score priest-lv 2)
                   merchant-score-val (get-in pending [:context :amity-scored] 0)]
               (-> state
                   (update-in [:players player-key :resources :gems] dec)
                   (update-in [:players player-key :amity] + (- priest-amity merchant-score-val))))
             state)

        ;; Board 35: gain chosen resource
        35 (update-in state [:players player-key :resources choice-val] (fnil inc 0))

        ;; Unknown board — just clear the flag
        state))))

;; =============================================================================
;; Temple accessors — MULTI-temple data model
;; =============================================================================
;; (:temples pdata) is {city -> [face-state ...]}: a VECTOR of temple states per
;; city. A city key is present IFF it holds >= 1 temple; an empty vector is never
;; stored (the last temple removed dissocs the city). Most cities hold one temple.
;; ALL temple reads/writes must route through these helpers — never touch the
;; {city -> face-state} shape directly.

(defn temples-at
  "Vector of face-states for the player's temples in `city` (empty if none)."
  [pdata city]
  (get-in pdata [:temples city] []))

(defn has-temple?
  "True iff the player holds >= 1 temple in `city`."
  [pdata city]
  (boolean (seq (temples-at pdata city))))

(defn all-temple-states
  "Seq of every temple face-state across all of the player's cities."
  [pdata]
  (mapcat val (:temples pdata)))

(defn temple-cities
  "Seq of cities in which the player holds >= 1 temple."
  [pdata]
  (keys (:temples pdata)))

(defn add-temple
  "Place a temple of `face` (:face-up | :face-down) into `city`: conj onto the
   city's vector, decrement :temples-supply, and fire the :temple-placed passive.
   conj NEVER overwrites, so the old re-assoc/double-charge bug cannot recur.
   No-op (returns state) when the player has no temple in supply."
  [state player-key city face]
  (let [pdata (get-in state [:players player-key])]
    (if (pos? (:temples-supply pdata 0))
      (-> state
          (update-in [:players player-key :temples city] (fnil conj []) face)
          (update-in [:players player-key :temples-supply] dec)
          (apply-passive player-key :temple-placed {:city city}))
      state)))

(defn flip-one-temple
  "Replace ONE :face-up with :face-down in `city`'s temple vector (the temple
   FLIP action — a face-up temple scores when its caravan visits). No-op if the
   city holds no face-up temple."
  [state player-key city]
  (let [v   (vec (temples-at (get-in state [:players player-key]) city))
        idx (.indexOf v :face-up)]
    (if (neg? idx)
      state
      (assoc-in state [:players player-key :temples city] (assoc v idx :face-down)))))

;; =============================================================================
;; Raider accessors (multi-raider-per-route model)
;; :raiders is {route-key [status ...]} where status ∈ {:raiding :point} — a
;; VECTOR of 0..N raiders per (player, route). Mirrors the temple-vector model.
;; Some callers store NON-CANONICAL [from to] keys (perform-influence relies on
;; this); these helpers do NOT canonicalize — callers keep their existing keys.
;; =============================================================================

(defn raiders-on
  "Vector of raider statuses the player holds on `rk` (empty if none)."
  [raiders-map rk]
  (get raiders-map rk []))

(defn raider-on-route?
  "True iff the player holds >= 1 raider on `rk`."
  [raiders-map rk]
  (boolean (seq (raiders-on raiders-map rk))))

(defn all-raider-states
  "Seq of every raider status across all of the player's routes."
  [raiders-map]
  (mapcat val raiders-map))

(defn total-raiders
  "Total number of raiders on the board for a player (sum of vector lengths)."
  [raiders-map]
  (count (all-raider-states raiders-map)))

(defn count-raiders-with-status
  "Number of the player's raiders (across all routes) currently in `status`."
  [raiders-map status]
  (count (filter #{status} (all-raider-states raiders-map))))

(defn place-one-raider
  "conj a `status` (default :raiding) raider onto `rk`'s vector. Never overwrites
   existing raiders on the route — the multi-raider analog of assoc-in a single
   value. Does NOT touch supply (callers manage supply)."
  ([state player-key rk] (place-one-raider state player-key rk :raiding))
  ([state player-key rk status]
   (update-in state [:players player-key :raiders rk] (fnil conj []) status)))

(defn flip-raiders-on-route-to-point
  "Flip EVERY :raiding status to :point in `rk`'s vector for `player-key`.
   Returns [state n] where n is how many were flipped. No-op (n=0) if none."
  [state player-key rk]
  (let [v (raiders-on (get-in state [:players player-key :raiders]) rk)
        n (count (filter #{:raiding} v))]
    (if (zero? n)
      [state 0]
      [(assoc-in state [:players player-key :raiders rk]
                 (mapv #(if (= % :raiding) :point %) v))
       n])))

(defn flip-one-raider-to-point
  "Flip ONE :raiding status to :point in `rk`'s vector for `player-key` (used
   when a placement effect wants the raider it just placed to land point-side).
   No-op if the route holds no :raiding raider."
  [state player-key rk]
  (let [v   (vec (raiders-on (get-in state [:players player-key :raiders]) rk))
        idx (.indexOf v :raiding)]
    (if (neg? idx)
      state
      (assoc-in state [:players player-key :raiders rk] (assoc v idx :point)))))

(defn score-one-point-raider
  "Remove ONE :point raider from `rk`'s vector (returning it to supply) and
   increment :raiders-supply. Preserves any other raiders on the route. If the
   route ends up empty, the key is dissociated. No-op if no :point raider there."
  [state player-key rk]
  (let [v   (vec (raiders-on (get-in state [:players player-key :raiders]) rk))
        idx (.indexOf v :point)]
    (if (neg? idx)
      state
      (let [v' (into (subvec v 0 idx) (subvec v (inc idx)))]
        (-> (if (seq v')
              (assoc-in state [:players player-key :raiders rk] v')
              (update-in state [:players player-key :raiders] dissoc rk))
            (update-in [:players player-key :raiders-supply] inc))))))

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

(defn space-take-resources
  "Resources granted by the :take action on `space` (nil if the space has no
   :take action, e.g. space 7). Used by board 30 (Council of Amar-Sin) to take
   goods based on another of the player's astronomers' wheel positions."
  [space]
  (some (fn [action]
          (when (= :take (:type action)) (:resources action)))
        (:actions (get action-spaces space))))

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

(defn segment-route-key
  "Extract canonical route-key from a route segment {:from :to :type}."
  [route-segment]
  (route-key (:from route-segment) (:to route-segment)))

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

(defn board-routes
  "The game's ACTUAL active routes — the board the game was built on. Stored in
   state at init as (active-routes player-count). Use this for contest evaluation
   and surround checks instead of recomputing (active-routes (count turn-order)):
   turn-order shrinks on resign and excludes non-human seats in solo-vs-AI, so a
   recompute can build a DIFFERENT topology (e.g. Kish↔Nineveh in a ≤3p recompute
   vs Kish↔Samarra on the real ≥4p board), breaking E1/E2/F2 and surround logic.
   Falls back to a recompute only if :routes is absent (minimal hand-built states)."
  [state]
  (or (:routes state)
      (active-routes (count (:turn-order state)))))

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

;; =============================================================================
;; FIX 1: Travel resolution primitives (moved here from eridu.choice).
;; These live in game.cljc so BOTH the real travel action (choice.cljc, via thin
;; re-exports) AND bonus board effects (apply-bonus-dispatch, board-5 passive)
;; resolve travel the SAME way — every traversed hop fires the real side-effects
;; (own point-raider pickup+score, enemy-raider flip, temple visits, :river-crossed
;; passive). choice.cljc re-exports these so all existing callers are unchanged.
;; =============================================================================

;; ── Trivial state helpers ──────────────────────────────────────────────────

(defn add-resource [state player resource n]
  (update-in state [:players player :resources resource] + n))

(defn spend-resource [state player resource]
  (-> state
      (update-in [:players player :resources resource] dec)
      ;; Passive trigger: resource-spent (board 29: gold → +2 amity)
      (apply-passive player :resource-spent {:resource resource})))

(defn add-amity [state player n]
  (-> state
      (update-in [:players player :amity] + n)
      (update-in [:turn-stats :amity] (fnil + 0) n)))

(defn add-glory [state player n]
  (-> state
      (update-in [:players player :glory] + n)
      (update-in [:turn-stats :glory] (fnil + 0) n)))

(defn add-log
  "Append a log entry to the game state."
  [state entry]
  (update state :log (fnil conj [])
          (merge {:round  (:round state 1)
                  :turn   (:turn-in-round state 1)
                  :player (current-player state)}
                 entry)))

;; ── Travel side-effects ──────────────────────────────────────────────────────

(defn visit-temples-on-travel
  "When caravan enters a city with a face-up temple, flip it and score amity."
  [state player city]
  (let [pdata (player-data state player)]
    (if (city-has-own-face-up-temple? pdata city)
      (let [;; Flip ONE face-up temple in this city to face-down
            state (-> state
                      (flip-one-temple player city)
                      (update-in [:turn-stats :temples-flipped] (fnil inc 0)))
            face-down-count (inc (count-face-down-temples pdata))
            ;; Score amity = number of face-down temples
            state (add-amity state player face-down-count)
            ;; Magistrate bonus
            leader-level (get-in state [:players player :roles :leader] 1)
            has-magistrate? (magistrate-in-city? state city)
            glory-bonus (if has-magistrate? (get leader-bonus leader-level 0) 0)]
        (-> (cond-> state
              (pos? glory-bonus) (add-glory player glory-bonus))
            (add-log {:type :temple-visit
                      :message (str "Visited temple in " (clojure.string/capitalize (name city))
                                    " — flipped face-down → +" face-down-count " Amity"
                                    " (" face-down-count " face-down temples)"
                                    (when (pos? glory-bonus)
                                      (str ", +" glory-bonus " Glory"
                                           " (Leader lv" leader-level " magistrate bonus)")))
                      :city city :amity face-down-count :glory glory-bonus})
            ;; Passive triggers: boards 4 (sell), 9 (role+), 20 (pottery→glory)
            (apply-passive player :temple-flipped {:city city})))
      state)))

(defn flip-enemy-raiders-on-route
  "When caravan travels a route, flip ALL opposing raiders on it to :point side
   (one flip per raider — a route may hold several)."
  [state player route-key]
  (reduce-kv
   (fn [s pk pdata]
     (if (and (not= pk player)
              (some #{:raiding} (raiders-on (:raiders pdata) route-key)))
       (let [[s' n] (flip-raiders-on-route-to-point s pk route-key)]
         (add-log s' {:type :raider-flip
                      :message (str "Flipped " n " of " pk "'s raider(s) on "
                                    (clojure.string/capitalize (name (first route-key))) "—"
                                    (clojure.string/capitalize (name (second route-key)))
                                    " to point side (caravan passed)")
                      :owner pk :route route-key}))
       s))
   state
   (:players state)))

(defn score-own-raider-on-route
  "When caravan travels a route holding own :point raider, score ONE of them
   (4 glory) per traversal: remove that single raider (return to supply),
   preserving any other raiders the player has on the route."
  [state player route-key]
  (let [has-point? (some #{:point} (raiders-on (get-in state [:players player :raiders]) route-key))]
    (if has-point?
      (let [;; Fire passive first to set flags (e.g. board 8 :keep-scored-raider)
            state (apply-passive state player :raider-scored {:route route-key})
            keep? (get-in state [:players player :keep-scored-raider])
            amity-instead? (get-in state [:players player :raider-score-amity])
            state (if keep?
                    ;; Board 8: flip ONE back to :raiding instead of removing
                    (let [v   (vec (raiders-on (get-in state [:players player :raiders]) route-key))
                          idx (.indexOf v :point)]
                      (-> state
                          (cond-> (not (neg? idx))
                            (assoc-in [:players player :raiders route-key]
                                      (assoc v idx :raiding)))
                          (update-in [:players player] dissoc :keep-scored-raider)))
                    ;; Normal: remove ONE point raider, return it to supply (others stay)
                    (score-one-point-raider state player route-key))
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
                                    (clojure.string/capitalize (name (first route-key))) "—"
                                    (clojure.string/capitalize (name (second route-key)))
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
        rk (route-key current-city destination)
        ;; Check if this is a river route
        is-river? (some #(and (= :river (:type %))
                              (= rk (route-key (:from %) (:to %))))
                        (:routes state))]
    (-> state
        (assoc-in [:players player :caravan] destination)
        (update-in [:players player :travels-this-round] (fnil inc 0))
        (update-in [:players player :total-travels] (fnil inc 0))
        (add-log {:type :travel
                  :message (str "Traveled from " (clojure.string/capitalize (name current-city))
                                " to " (clojure.string/capitalize (name destination)))
                  :from current-city :to destination})
        (flip-enemy-raiders-on-route player rk)
        (score-own-raider-on-route player rk)
        (visit-temples-on-travel player destination)
        ;; Passive trigger: river crossing (boards 3, 12)
        (cond-> is-river? (apply-passive player :river-crossed {:route rk})))))

(defn shortest-city-path
  "BFS over (:city-graph state) from src to dest. Returns the ordered hop list
   EXCLUDING src (i.e. each intermediate city then dest), or nil if unreachable.
   Deterministic: neighbours are enqueued in sorted order so equal-length paths
   are tie-broken stably. (No general pathfinder existed before — road-clockwise-path
   is magistrate-/road-only and ignores river edges.)"
  [state src dest]
  (let [graph (:city-graph state)]
    (cond
      (= src dest) []
      (nil? (get graph src)) nil
      :else
      ;; Level-order BFS over `frontier` (a vector — portable across CLJ/CLJS,
      ;; unlike clojure.lang.PersistentQueue which is JVM-only). Each level's
      ;; neighbours are expanded in sorted order so equal-length paths tie-break
      ;; stably. `seen` maps each city to its predecessor (src -> nil) for path
      ;; reconstruction.
      (loop [frontier [src]
             seen {src nil}]
        (if (empty? frontier)
          nil
          (if (some #(= dest %) frontier)
            ;; Reconstruct path from dest back to src, then drop src
            (loop [c dest acc ()]
              (if (nil? c)
                (vec (rest acc))
                (recur (get seen c) (conj acc c))))
            (let [[frontier' seen']
                  (reduce
                   (fn [[fr sn] city]
                     (reduce
                      (fn [[fr2 sn2] n]
                        (if (contains? sn2 n)
                          [fr2 sn2]
                          [(conj fr2 n) (assoc sn2 n city)]))
                      [fr sn]
                      (sort-by name (get graph city))))
                   [[] seen]
                   frontier)]
              (recur frontier' seen'))))))))

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

(defn perform-influence
  "Execute a full influence action: move magistrate from current city to dest,
   tracing the clockwise path, flipping all raiders on routes passed through.
   Returns updated state. Used by both resolve-influence and bonus board effects."
  [state player-key mag-id dest steps]
  (let [mag-city (get-in state [:magistrates mag-id])
        active-cities (set (keys (:city-graph state)))
        path (road-clockwise-path mag-city steps active-cities)
        ;; Update magistrate position
        state (assoc-in state [:magistrates mag-id] dest)
        ;; Flip raiders on each route the magistrate passes through
        ;; Must check BOTH orderings since raider keys may not be canonical
        state (reduce
               (fn [s [from to]]
                 (let [rk (route-key from to)]
                   (reduce-kv
                    (fn [s2 pk pdata]
                      ;; Check all key orderings since raider keys may not be canonical;
                      ;; flip EVERY :raiding raider on the matched route (a route may
                      ;; hold several).
                      (let [raiders (:raiders pdata)
                            match (cond
                                    (some #{:raiding} (raiders-on raiders rk)) rk
                                    (some #{:raiding} (raiders-on raiders [from to])) [from to]
                                    (some #{:raiding} (raiders-on raiders [to from])) [to from]
                                    :else nil)]
                        (if match
                          (first (flip-raiders-on-route-to-point s2 pk match))
                          s2)))
                    s (:players s))))
               state path)]
    state))

;; =============================================================================
;; FIX 2 (typed-movement class): magistrate movement that honors route :type.
;; perform-influence above is ROAD-only (road-clockwise-path has no river edges),
;; so a "move a Magistrate across a river" effect must flip the raider on the
;; RIVER edge and fire :river-crossed — not flip a road raider. We model the
;; river hop as the single river-typed edge [mag-city dest] using the same
;; magistrate-flip semantics (:raiding → :point, both key orderings).
;; =============================================================================

(defn river-edge?
  "True if [a b] is an active RIVER-typed route in this state's board."
  [state a b]
  (let [rk (route-key a b)]
    (boolean
     (some #(and (= :river (:type %))
                 (= rk (route-key (:from %) (:to %))))
           (board-routes state)))))

(defn magistrate-river-destinations
  "Cities reachable by a single RIVER edge from a city currently holding a
   magistrate — the legal targets of Board 18 #1 ('Move a Magistrate across a
   river'). Returns a distinct vector of destination cities."
  [state]
  (let [mag-cities (magistrate-cities state)]
    (->> (board-routes state)
         (filter #(= :river (:type %)))
         (mapcat (fn [{:keys [from to]}]
                   (cond-> []
                     (contains? mag-cities from) (conj to)
                     (contains? mag-cities to)   (conj from))))
         distinct
         vec)))

(defn perform-river-influence
  "Move a magistrate ACROSS A RIVER edge to `dest`. Picks a magistrate that sits
   on a city sharing a river edge with `dest`, moves it there, flips the raider on
   that single river edge (both key orderings) to :point, and fires the
   :river-crossed passive for `player-key`. No-op (returns state unchanged) when
   no magistrate is one river edge from `dest`. Mirrors perform-influence's flip
   semantics but for the typed (river) edge instead of a road-clockwise path."
  [state player-key dest]
  (let [mag-entry (->> (:magistrates state)
                       (filter (fn [[_ mag-city]]
                                 (river-edge? state mag-city dest)))
                       first)]
    (if-not mag-entry
      state
      (let [[mag-id mag-city] mag-entry
            rk (route-key mag-city dest)
            ;; Move the magistrate across the river
            state (assoc-in state [:magistrates mag-id] dest)
            ;; Flip the raider on the river edge (both orderings, like perform-influence)
            state (reduce-kv
                   (fn [s pk pdata]
                     (let [raiders (:raiders pdata)
                           match (cond
                                   (some #{:raiding} (raiders-on raiders rk)) rk
                                   (some #{:raiding} (raiders-on raiders [mag-city dest])) [mag-city dest]
                                   (some #{:raiding} (raiders-on raiders [dest mag-city])) [dest mag-city]
                                   :else nil)]
                       (if match
                         (first (flip-raiders-on-route-to-point s pk match))
                         s)))
                   state (:players state))]
        ;; Fire the river-crossing passive (boards 3, 12) — the typed edge was a river
        (apply-passive state player-key :river-crossed {:route rk})))))

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

      ;; C: Temple count (across all cities; a city may hold more than one)
      :C1 (>= (count (filter #{:face-up}   (all-temple-states pdata))) 4)
      :C2 (>= (count (filter #{:face-down} (all-temple-states pdata))) 4)

      ;; D: Temple placement
      :D1 (and (has-temple? pdata :eridu) (has-temple? pdata :nineveh))
      ;; "A temple in four river CITIES" — count distinct river cities (keys),
      ;; NOT total temples (a city may now hold more than one).
      :D2 (>= (count (filter #(contains? river-cities (key %)) temples)) 4)

      ;; E: Raider placement
      :E1 (let [kish-routes (set (for [r (board-routes state)
                                       :when (or (= :kish (:from r)) (= :kish (:to r)))]
                                   (segment-route-key r)))]
             (every? #(raider-on-route? raiders %) kish-routes))
      :E2 (let [eridu-routes (set (for [r (board-routes state)
                                        :when (or (= :eridu (:from r)) (= :eridu (:to r)))]
                                    (segment-route-key r)))
                ninev-routes (set (for [r (board-routes state)
                                        :when (or (= :nineveh (:from r)) (= :nineveh (:to r)))]
                                    (segment-route-key r)))]
             (and (some #(raider-on-route? raiders %) eridu-routes)
                  (some #(raider-on-route? raiders %) ninev-routes)))

      ;; F: Raider state
      :F1 (>= (count-raiders-with-status raiders :point) 3)
      :F2 (let [river-route-keys (set (for [r (board-routes state)
                                             :when (= :river (:type r))]
                                         (segment-route-key r)))]
             (every? #(raider-on-route? raiders %) river-route-keys))

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
      :K2 (boolean
           ;; (boolean ...) wrap added in QA lesson 5: the docstring promises
           ;; true/false strictly, but `when` leaks nil when preconditions fail.
           (let [ts (get state :turn-stats {})
                 sell-city (get ts :sold-in-city)
                 pc (count (:turn-order state))]
             (when (and (= player-key (:player ts)) sell-city)
               (let [adj-routes (routes-from-city sell-city (board-routes state))
                     adj-route-keys (set (map segment-route-key adj-routes))]
                 ;; Check if ALL adjacent routes have YOUR raider
                 (every? #(raider-on-route? raiders %) adj-route-keys)))))

      ;; L: Resource hoarding
      :L1 (>= (get-in pdata [:resources :gems] 0) 5)
      :L2 (>= (get-in pdata [:resources :pottery] 0) 5)

      ;; M: Magistrate + temple combos
      :M1 (let [facedown-cities (set (map key (filter #(some #{:face-down} (val %)) temples)))]
             ;; Every magistrate must be in a city with a facedown temple
             (every? #(contains? facedown-cities (val %))
                     (:magistrates state)))
      :M2 (let [demand-cities (set (for [[c ds] (:city-demands state)
                                          :when (seq ds)] c))]
             (>= (count (mapcat val (filter #(not (contains? demand-cities (key %))) temples))) 4))

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
      :C1 (let [n (count (filter #{:face-up} (all-temple-states pdata)))]
             [(/ (min n 4) 4.0) (str n "/4 face-up temples")])
      :C2 (let [n (count-face-down-temples pdata)]
             [(/ (min n 4) 4.0) (str n "/4 face-down temples")])
      :D1 (let [has-e (if (has-temple? pdata :eridu) 0.5 0)
                has-n (if (has-temple? pdata :nineveh) 0.5 0)]
             [(+ has-e has-n) (str (if (has-temple? pdata :eridu) "✓" "✗") " eridu "
                                   (if (has-temple? pdata :nineveh) "✓" "✗") " nineveh")])
      :D2 (let [n (count (filter #(contains? river-cities (key %)) temples))]
             [(/ (min n 4) 4.0) (str n "/4 river-city temples")])
      :E1 (let [kish-routes (set (for [r (board-routes state)
                                        :when (or (= :kish (:from r)) (= :kish (:to r)))]
                                    (segment-route-key r)))
                have (count (filter #(raider-on-route? raiders %) kish-routes))
                need (count kish-routes)]
             [(if (pos? need) (/ (min have need) (double need)) 0)
              (str have "/" need " kish routes")])
      :E2 (let [has-e (some #(let [rk %] (or (= :eridu (first rk)) (= :eridu (second rk))))
                             (keys raiders))
                has-n (some #(let [rk %] (or (= :nineveh (first rk)) (= :nineveh (second rk))))
                             (keys raiders))]
             [(+ (if has-e 0.5 0) (if has-n 0.5 0))
              (str (if has-e "✓" "✗") " eridu-raider " (if has-n "✓" "✗") " nineveh-raider")])
      :F1 (let [n (count-raiders-with-status raiders :point)]
             [(/ (min n 3) 3.0) (str n "/3 point-side raiders")])
      :F2 (let [river-rks (set (for [r (board-routes state) :when (= :river (:type r))]
                                  (segment-route-key r)))
                have (count (filter #(raider-on-route? raiders %) river-rks))
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
      :M1 (let [mag-cities (magistrate-cities state)
                n (count (filter #(and (some #{:face-down} (val %))
                                        (contains? mag-cities (key %))) temples))]
             [(/ (min n 2) 2.0) (str n "/2 magistrates at facedown temples")])
      :M2 (let [demand-cities (set (for [[c ds] (:city-demands state) :when (seq ds)] c))
                n (count (mapcat val (filter #(not (contains? demand-cities (key %))) temples)))]
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
        priest-level (get-in pdata [:roles :priest] 1)
        max-t (get priest-max-temples priest-level 3)
        placed (count-temples-placed pdata)
        supply (:temples-supply pdata 0)]
    (if (and (pos? supply)
             ;; Multi-temple model: a city's temples are a VECTOR, so conj never
             ;; overwrites and the old re-assoc/double-charge bug cannot recur.
             ;; allow-duplicate? = true (bonus-board placements) permits a SECOND
             ;; temple in a city you already hold; the normal action passes false
             ;; and stays 1/city. allow-duplicate? also bypasses the soft max-t cap.
             (or (not (has-temple? pdata city)) allow-duplicate?)
             (or allow-duplicate? (< placed max-t)))
      ;; add-temple conj's :face-up, decs supply, and fires the :temple-placed
      ;; passive (boards 13 "raider adjacent", 21 "extra facedown").
      (add-temple state player-key city :face-up)
      state)))

(defn- place-raider-on
  "Place ONE :raiding raider on `route-key` (conj — never overwrites), gated by
   supply and the raider-level max-deployed cap. By default keeps the classic
   one-raider-per-route rule (no-op if the route already holds this player's
   raider); pass allow-stack? true to permit a 2nd raider on an occupied route
   (board 25 / [34 2])."
  ([state player-key route-key] (place-raider-on state player-key route-key false))
  ([state player-key route-key allow-stack?]
   (let [pdata (get-in state [:players player-key])
         raider-level (get-in pdata [:roles :raider] 1)
         max-r (get raider-max-deployed raider-level 2)
         deployed (count-raiders-deployed pdata)
         supply (:raiders-supply pdata 0)]
     (if (and (pos? supply)
              (< deployed max-r)
              (or allow-stack?
                  (not (raider-on-route? (:raiders pdata) route-key))))
       (-> state
           (place-one-raider player-key route-key :raiding)
           (update-in [:players player-key :raiders-supply] dec))
       state))))


;; =============================================================================
;; Bonus classification re-exports (moved to eridu.bonus in QA lesson 11)
;; =============================================================================
;; Kept here as defs so existing callers (game/effect-implementation-status,
;; game/bonus-needs-choice?, game/board-effect-diagnostic) still resolve.
;; New code should require eridu.bonus directly.

(def effect-implementation-status bonus/effect-implementation-status)
(def board-effect-diagnostic      bonus/board-effect-diagnostic)
(def bonus-needs-choice?          bonus/bonus-needs-choice?)

(defn- bonus-trace-snapshot
  "Capture the player-state slice plus selected world-level fields that
   bonus coverage tracing diffs against. World-level fields (:magistrates)
   let the oracle resolve fns like at-magistrate? without needing the full
   game state."
  [state player-key]
  (let [pdata (get-in state [:players player-key])]
    {:amity          (:amity pdata 0)
     :glory          (:glory pdata 0)
     :roles          (:roles pdata)
     :resources      (:resources pdata)
     :temples        (:temples pdata {})
     :raiders        (:raiders pdata {})
     :caravan        (:caravan pdata)
     :temples-supply (:temples-supply pdata 0)
     :raiders-supply (:raiders-supply pdata 0)
     :demand-tokens  (:demand-tokens pdata [])
     :magistrates    (magistrate-cities state)}))

(defn- record-coverage-trace
  "Append a coverage-trace record to (:coverage-traces state). Caller must
   only invoke when (:coverage-trace? state) is truthy."
  [state player-key board-id slot-idx pre-snapshot post-snapshot choice-value]
  (let [delta-roles (into {}
                          (for [role roles
                                :let [a (get-in pre-snapshot  [:roles role] 1)
                                      b (get-in post-snapshot [:roles role] 1)]
                                :when (not= a b)]
                            [role (- b a)]))
        delta-resources (into {}
                              (for [res [:tools :pottery :gold :gems]
                                    :let [a (get-in pre-snapshot  [:resources res] 0)
                                          b (get-in post-snapshot [:resources res] 0)]
                                    :when (not= a b)]
                                [res (- b a)]))
        record {:board-id        board-id
                :slot-idx        slot-idx
                :player          player-key
                :pre-snapshot    pre-snapshot
                :post-snapshot   post-snapshot
                :choice-value    choice-value
                :round           (:round state 1)
                :turn            (:turn-in-round state 1)
                :delta-amity     (- (:amity post-snapshot 0) (:amity pre-snapshot 0))
                :delta-glory     (- (:glory post-snapshot 0) (:glory pre-snapshot 0))
                :delta-roles     delta-roles
                :delta-resources delta-resources
                :delta-temples   (- (count (all-temple-states post-snapshot)) (count (all-temple-states pre-snapshot)))
                :delta-raiders   (- (total-raiders (:raiders post-snapshot)) (total-raiders (:raiders pre-snapshot)))
                :impl-status     (get effect-implementation-status [board-id slot-idx] :unknown)}]
    (update state :coverage-traces (fnil conj []) record)))

;; =============================================================================
;; Bonus interactive-action helpers (lifted out of apply-bonus-with-choice in
;; the dual-path unification — QA lesson 11b). These take `player-key` so the
;; single choice-aware apply-bonus-dispatch can drive both the bot (auto) arm
;; and the human/UI arm through one code path.
;; =============================================================================

(declare apply-bonus-dispatch)

(defn- bonus-influence
  "Full influence action for a bonus effect: trace the clockwise path from the
   first magistrate to `dest`, flip raiders passed through, respecting leader
   movement. Returns updated state."
  [s player-key dest]
  (let [active-cities (set (keys (:city-graph s)))
        mag-id (ffirst (:magistrates s))]
   (if-not (and mag-id (contains? active-cities dest))
    s  ;; guard: no magistrate present, or non-city dest (e.g. a wrong-typed choice)
    (let [leader-lv (get-in s [:players player-key :roles :leader] 1)
        max-move (get leader-movement leader-lv 1)
        mag-city (get-in s [:magistrates mag-id])
        ;; Find the steps needed to reach dest (up to max-move)
        steps (or (some (fn [n]
                          (let [path (road-clockwise-path mag-city n active-cities)]
                            (when (and (seq path) (= dest (second (last path))))
                              n)))
                        (range 1 (inc max-move)))
                  1)]
    (perform-influence s player-key mag-id dest steps)))))

(defn- splice-first
  "Remove the first occurrence of `x` from vector `ds` (order-preserving)."
  [ds x]
  (let [v (vec ds) idx (.indexOf v x)]
    (if (neg? idx) v (into (subvec v 0 idx) (subvec v (inc idx))))))

(defn fulfillable-goods
  "Goods the player can fulfill by selling in `city`: the city's OPEN demands
   plus the player's own OWNER-RESTRICTED demands placed there (`:owned-demands`,
   from boards 22/24). Other players never see another's owned-demands, so
   'only you may fulfill those demands' is enforced automatically."
  [state player-key city]
  (concat (get-in state [:city-demands city] [])
          (get-in state [:players player-key :owned-demands city] [])))

(defn consume-demand
  "Remove one fulfilled `good` token at `city`: from the player's owner-restricted
   demands FIRST (if present there), else from the city's open demands."
  [state player-key city good]
  (let [owned (get-in state [:players player-key :owned-demands city] [])]
    (if (>= (.indexOf (vec owned) good) 0)
      (update-in state [:players player-key :owned-demands city] splice-first good)
      (update-in state [:city-demands city] splice-first good))))

(defn- sell-good-in-city
  "Sell one `good` matching a demand in `city`: dec the good, consume the demand
   token (owned-restricted first, else open), conj the token onto the player's
   :demand-tokens, and add merchant-level amity. Assumes `good` is present and
   matches a live (open or owned) demand. Callers handle glory/turn-stats/logging."
  [state player-key city good]
  (let [merchant-lv (get-in state [:players player-key :roles :merchant] 1)
        amity-score (get merchant-score merchant-lv 2)]
    (-> state
        (update-in [:players player-key :resources good] dec)
        (consume-demand player-key city good)
        (update-in [:players player-key :demand-tokens] conj good)
        (update-in [:players player-key :amity] + amity-score))))

(defn- bonus-sell-in
  "Resolve a single Sell in `city`: spend one good matching a demand there,
   move the token to the player's demand-tokens, score merchant-level amity,
   AND the magistrate glory bonus at the SELL city. A sell-at-a-distance still
   earns the magistrate bonus — it is tied to where the sell happens (`city`),
   NOT the caravan's location (designer-confirmed)."
  [s player-key city]
  (let [resources (get-in s [:players player-key :resources])
        sellable (first (filter #(pos? (get resources % 0))
                                (fulfillable-goods s player-key city)))]
    (if sellable
      (let [s (sell-good-in-city s player-key city sellable)
            has-mag (some #{city} (vals (:magistrates s)))
            leader-lv (get-in s [:players player-key :roles :leader] 1)
            glory (if has-mag (get leader-bonus leader-lv 0) 0)]
        (cond-> s
          (pos? glory) (update-in [:players player-key :glory] + glory)))
      s)))

(defn- sell-for-glory-in
  "Like bonus-sell-in but the merchant reward is scored as GLORY instead of
   amity — for cards that say 'Sell ... for Glory instead'. Spends a matching
   good, consumes the demand token, and adds merchant-level glory (* `mult`, for
   'double Glory' cards) + the magistrate glory bonus at the sell city. No-op
   when nothing is sellable."
  ([s player-key city] (sell-for-glory-in s player-key city 1))
  ([s player-key city mult]
   (let [resources (get-in s [:players player-key :resources])
        sellable (first (filter #(pos? (get resources % 0))
                                (fulfillable-goods s player-key city)))]
    (if sellable
      (let [merchant-lv (get-in s [:players player-key :roles :merchant] 1)
            glory (* mult (get merchant-score merchant-lv 2))
            has-mag (some #{city} (vals (:magistrates s)))
            leader-lv (get-in s [:players player-key :roles :leader] 1)
            mag-glory (if has-mag (get leader-bonus leader-lv 0) 0)]
        (-> s
            (update-in [:players player-key :resources sellable] dec)
            (consume-demand player-key city sellable)
            (update-in [:players player-key :demand-tokens] conj sellable)
            (update-in [:players player-key :glory] + (+ glory mag-glory))))
      s))))

(defn- place-demand-tokens
  "Draw `n` random demand tokens from the bag and place them onto `city`'s demand
   vector. Returns state with :demand-bag advanced. No-op draws on an empty bag."
  [s city n]
  (let [bag0 (:demand-bag s (full-demand-bag))]
    (loop [s (assoc s :demand-bag bag0) i 0]
      (if (>= i n)
        s
        (let [drawn (draw-demand-token (:demand-bag s))]
          (if-not drawn
            s
            (let [[bag' tok] drawn]
              (recur (-> s
                         (assoc :demand-bag bag')
                         (update-in [:city-demands city] (fnil conj []) tok))
                     (inc i)))))))))

(defn- place-owned-demand
  "Draw one random demand token onto the player's OWNER-RESTRICTED demands at
   `city` (only that player can later fulfill it). No-op on an empty bag."
  [s player-key city]
  (if-let [[bag' tok] (draw-demand-token (:demand-bag s (full-demand-bag)))]
    (-> s
        (assoc :demand-bag bag')
        (update-in [:players player-key :owned-demands city] (fnil conj []) tok))
    s))

(defn- cities-surrounded-by
  "Set of cities the player has fully SURROUNDED — every board route adjacent to
   the city holds one of `player-key`'s raiders. Mirrors
   choice/city-surrounded-by-player? but lives here so apply-bonus-dispatch can
   use it without the choice→game require cycle. Cities with no adjacent active
   routes (e.g. samarra in ≤3p) are never surrounded."
  [state player-key]
  (let [raiders (get-in state [:players player-key :raiders])]
    (set
     (for [city all-cities
           :let [adj-rks (map segment-route-key
                              (routes-from-city city (board-routes state)))]
           :when (and (seq adj-rks)
                      (every? #(raider-on-route? raiders %) adj-rks))]
       city))))

(defn- touches?
  "True if route `r` has `city` as either endpoint."
  [city r]
  (or (= city (:from r)) (= city (:to r))))

(defn- free-routes
  "Route-keys (in board order) of routes satisfying `pred` (a route map) that the
   player does not already hold a raider on. Uses board-routes (the STORED board)
   so placements are topology-stable under turn-order/board mismatch (solo-vs-AI,
   resign) — NOT (active-routes (count turn-order)). pred takes a route map."
  [pdata state pred]
  (->> (board-routes state)
       (filter pred)
       (map segment-route-key)
       (remove #(raider-on-route? (:raiders pdata) %))))

(defn- default-magistrate-target
  "Prefer a magistrate city where the player has no temple, else any magistrate
   city. nil only when there are no magistrate cities at all."
  [state pdata]
  (or (first (filter #(not (has-temple? pdata %)) (magistrate-cities state)))
      (first (magistrate-cities state))))

(defn- bonus-deploy-near
  "Place a raider on the first available route adjacent to `city`."
  [s player-key city]
  (let [pd (get-in s [:players player-key])
        adj (routes-from-city city (:routes s))
        avail (remove #(raider-on-route? (:raiders pd) %)
                      (map segment-route-key adj))
        rk (first avail)]
    (if rk (place-raider-on s player-key rk) s)))

(defn bonus-travel-to
  "FIX 1: Move the player's caravan to `dest` via REAL travel resolution.
   No-op when dest is nil or already the caravan's city. Otherwise BFS the
   shortest path (over :city-graph, road + river edges) and reduce travel-to-city
   over every hop, so each traversed route fires the genuine side-effects:
   own point-raider pickup+score, enemy-raider flips, temple visits, and the
   :river-crossed passive. A single adjacent hop fires exactly one travel-to-city.
   Replaces the old caravan teleport (assoc-in […:caravan] dest) used throughout
   apply-bonus-dispatch and the board-5 :magistrate-moved passive."
  [s player-key dest]
  (let [caravan (get-in s [:players player-key :caravan])]
    (if (or (nil? dest) (= dest caravan))
      s
      (if-let [hops (shortest-city-path s caravan dest)]
        (reduce (fn [st hop] (travel-to-city st player-key hop)) s hops)
        ;; Unreachable via the graph (e.g. a hand-built state with no :city-graph,
        ;; or a non-adjacent target with no path): fall back to a direct single hop
        ;; so the destination is still honored and its route effects still fire.
        (travel-to-city s player-key dest)))))

(defn apply-bonus-with-choice
  "Human/UI arm for a bonus effect that needs a player choice. A thin wrapper
   over the single, choice-aware apply-bonus-dispatch — this collapses the old
   divergent hand-table (which grouped slots wrongly and gave humans a broken
   effect for ~15 slots) into the same code path the bot uses.
   `choice-value` is the player's selection (resource/role keyword, or city).
   When (:coverage-trace? state) is on, appends a record to :coverage-traces."
  [state player-key board-id slot-idx choice-value]
  (let [pre-snapshot (bonus-trace-snapshot state player-key)
        pdata        (get-in state [:players player-key])
        pc           (count (:turn-order state))
        result-state (apply-bonus-dispatch state player-key pdata pc
                                           board-id slot-idx choice-value)]
    (cond-> result-state
      (:coverage-trace? state)
      (record-coverage-trace player-key board-id slot-idx
                             pre-snapshot
                             (bonus-trace-snapshot result-state player-key)
                             choice-value))))

(defn- apply-bonus-dispatch
  "Unified, choice-aware dispatch for a bonus board instant effect, keyed by
   [board-id slot-idx]. Drives BOTH the bot/auto arm (choice nil) and the
   human/UI arm (apply-bonus-with-choice forwards the player's `choice`).

   For every slot that bonus-needs-choice? marks interactive, the arm uses
   (or choice <auto-default>) for its target/pick and performs the FAITHFUL
   card effect — eliminating the old dual-path divergence where humans got a
   different (broken) effect than the bot. Non-interactive slots are unchanged.

   `choice` is the player's selection (resource/role keyword, or a city);
   nil for the auto arm."
  [state player-key pdata pc board-id slot-idx & [choice]]
  (case [board-id slot-idx]
      ;; ─── Board 1: Shield of Gilgamesh ───────────────────────────
      [1 1] (bonus-travel-to state player-key :kish) ;; Travel to Kish
      [1 2] (-> state ;; Increase Raider and Leader
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :leader))
      [1 3] (let [avail (free-routes pdata state #(touches? :lagash %)) ;; Place two raiders near Lagash
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
      ;; Temple in magistrate city (choice = which magistrate city; default =
      ;; first magistrate city without your temple, else any magistrate city).
      [2 3] (let [target (or choice (default-magistrate-target state pdata))]
              (if target (place-temple-in state player-key target true) state))
      [2 4] (let [fd (count-face-down-temples pdata)] ;; Glory per facedown temple
              (update-in state [:players player-key :glory] + fd))

      ;; ─── Board 3: Voyage of Ziusudra ────────────────────────────
      [3 1] (increase-role-free state player-key :leader)
      [3 2] (place-temple-in state player-key :lagash true)
      ;; "Place a Raider adjacent to Eridu and gain a good of your choice."
      ;; FAITHFUL (Bucket A dual-path): place the raider near Eridu AND grant the
      ;; chosen good. Old human arm dropped the raider; old auto arm forced :tools.
      [3 3] (let [avail (free-routes pdata state #(touches? :eridu %))
                  good  (or choice :tools)]
              (if-let [rk (first avail)]
                (-> state
                    (place-raider-on player-key rk)
                    (add-player-resource player-key good 1))
                (add-player-resource state player-key good 1)))
      ;; "Take a travel action then a Sell action." choice = adjacent
      ;; destination (default = stay at caravan); travel there, then sell at the
      ;; post-travel caravan city.
      [3 4] (let [dest (or choice (:caravan pdata))]
              (cond-> state
                dest (-> (bonus-travel-to player-key dest)
                         (bonus-sell-in player-key dest))))

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
      ;; "Take a Deploy action then a Temple action." No city pick — deploy on
      ;; any open route + temple in the caravan city (FAITHFUL; the old human arm
      ;; inserted a spurious travel and dropped the temple). choice ignored.
      [5 3] (let [any-route (first (free-routes pdata state (constantly true)))
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
                      (if (not (has-temple? (get-in s [:players player-key]) city))
                        (place-temple-in s player-key city true)
                        s))
                    state (magistrate-cities state))
      ;; "Sell to Babylon for double points (you don't need to be there)."
      ;; FAITHFUL: do NOT move the caravan — resolve TWO real sells in Babylon
      ;; (double points), each scoring merchant amity + magistrate glory. No-ops
      ;; per sell when nothing sellable.
      [6 3] (-> state
                (bonus-sell-in player-key :babylon)
                (bonus-sell-in player-key :babylon))
      [6 4] (-> state ;; Place a Raider adjacent to Lagash. Gain Tools, Tools
               (bonus-deploy-near player-key :lagash)
               (add-player-resource player-key :tools 2))

      ;; ─── Board 7: March of Lugalbanda ───────────────────────────
      [7 1] (-> state ;; Increase Merchant and Leader
               (increase-role-with-cost player-key :merchant)
               (increase-role-with-cost player-key :leader))
      ;; "Place a Temple in a city with a Magistrate (even if you already have a
      ;; temple there)." Unified: place in (or choice <first magistrate city>).
      ;; allow-duplicate? true → genuinely places a 2nd temple in the multi-temple
      ;; model when you already hold one there.
      [7 2] (let [target (or choice (first (magistrate-cities state)))]
              (if target (place-temple-in state player-key target true) state))
      ;; "Take a travel action. Score 3 Glory if you are in Eridu." FAITHFUL
      ;; (Bucket A): travel to (or choice :eridu); score 3 glory only if you end
      ;; in Eridu. (Old human arm scored nothing.)
      [7 3] (let [dest (or choice :eridu)]
              (cond-> (bonus-travel-to state player-key dest)
                (= dest :eridu) (update-in [:players player-key :glory] + 3)))
      ;; "Take a travel action. Score 3 Amity if you are in Kish." FAITHFUL.
      [7 4] (let [dest (or choice :kish)]
              (cond-> (bonus-travel-to state player-key dest)
                (= dest :kish) (update-in [:players player-key :amity] + 3)))

      ;; ─── Board 8: Fury of Enkidu ───────────────────────────────
      [8 1] (-> state ;; Increase Raider and Priest
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :priest))
      ;; "Place one random Demand Token in Nippur and Babylon each. Then you may
      ;; sell once in your city." FAITHFUL: draw a token into each city, then
      ;; bonus-sell-in your caravan city (no-ops if nothing sellable). Replaces
      ;; the old flat +3-amity proxy.
      [8 2] (-> state
               (place-demand-tokens :nippur 1)
               (place-demand-tokens :babylon 1)
               (bonus-sell-in player-key (:caravan pdata)))
      ;; "Gain Gold, Gems, Pottery. Then you may sell once in your city."
      ;; FAITHFUL: keep the three resource gains, THEN resolve one real sell in
      ;; the caravan city (a freshly-gained good can satisfy a demand there).
      [8 3] (-> state ;; Gain Gold, Gems, Pottery + sell
               (add-player-resource player-key :gold 1)
               (add-player-resource player-key :gems 1)
               (add-player-resource player-key :pottery 1)
               (bonus-sell-in player-key (:caravan pdata)))
      [8 4] (reduce (fn [s rk] ;; Flip all raiders to point (every raider on every route)
                      (first (flip-raiders-on-route-to-point s player-key rk)))
                    state (keys (:raiders pdata)))

      ;; ─── Board 9: Rites of Ninhursag ───────────────────────────
      [9 1] (-> state ;; Gain Tools, Gold, Pottery + Amity = leader level
               (add-player-resource player-key :tools 1)
               (add-player-resource player-key :gold 1)
               (add-player-resource player-key :pottery 1)
               (update-in [:players player-key :amity] + (get-in pdata [:roles :leader] 1)))
      [9 2] (-> state ;; Increase Priest and Leader
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :leader))
      [9 3] (let [avail (free-routes pdata state #(= :river (:type %)))] ;; Raider on each river
              (reduce #(place-raider-on %1 player-key %2) state (take 3 avail)))
      ;; "Sell to any city with a Magistrate. If you are in that city, you may
      ;; take a Temple action." FAITHFUL: NO teleport — resolve a real sell in
      ;; (or choice <first magistrate city without your temple>); the Temple
      ;; action fires ONLY if your caravan is already in that city.
      [9 4] (let [target (or choice (default-magistrate-target state pdata))]
              (cond-> state
                target (bonus-sell-in player-key target)
                (and target (= (:caravan pdata) target))
                (place-temple-in player-key target true)))

      ;; ─── Board 10: Wealth of Meskalamdug ───────────────────────
      [10 1] (increase-role-free state player-key :merchant)
      [10 2] (increase-role-free state player-key :merchant)
      ;; "Place a Raider adjacent to a Magistrate. Score Amity based on your
      ;; Leader level." FAITHFUL (Bucket B): amity = LEADER LEVEL, not flat +2.
      [10 3] (let [leader-lv (get-in pdata [:roles :leader] 1)
                   mag-cities (magistrate-cities state)
                   avail (free-routes pdata pc
                                      #(or (contains? mag-cities (:from %))
                                           (contains? mag-cities (:to %))))]
               (if-let [rk (first avail)]
                 (-> state
                     (place-raider-on player-key rk)
                     (update-in [:players player-key :amity] + leader-lv))
                 (update-in state [:players player-key :amity] + leader-lv)))
      [10 4] (place-temple-in state player-key :nippur true)

      ;; ─── Board 11: Ambition of Sargon ──────────────────────────
      ;; "Place two random Demand Tokens in Lagash. Gain matching resources."
      ;; FAITHFUL (Bucket B): NO caravan move (this used to bonus-travel-to and
      ;; teleport the player to Lagash — designer-reported bug). Draw two demand
      ;; tokens into Lagash and grant the player the matching goods, at a distance.
      [11 1] (let [d1 (draw-demand-token (:demand-bag state))
                   [bag1 t1] (or d1 [(:demand-bag state) nil])
                   d2 (when t1 (draw-demand-token bag1))
                   [bag2 t2] (or d2 [bag1 nil])
                   tokens (remove nil? [t1 t2])]
               (reduce (fn [s tok]
                         (-> s
                             (update-in [:city-demands :lagash] (fnil conj []) tok)
                             (add-player-resource player-key tok 1)))
                       (assoc state :demand-bag bag2)
                       tokens))
      ;; "Sell to Lagash for Double Glory points (you don't have to be there)."
      ;; FAITHFUL: do NOT move the caravan — resolve TWO real sells in Lagash that
      ;; score GLORY (double glory), each also earning the magistrate glory bonus.
      ;; No-ops per sell when nothing sellable.
      ;; "Sell to Lagash for Double Glory points (you don't have to be there)."
      ;; Designer-confirmed: ONE sell at Lagash scoring 2x merchant glory (one
      ;; good + one token), no caravan move.
      [11 2] (sell-for-glory-in state player-key :lagash 2)
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
      ;; "Increase your Merchant level (paying any costs). Then Sell to the city
      ;; you are IN for Glory instead." FAITHFUL: increase merchant with cost,
      ;; then resolve a REAL sell in the CURRENT (caravan) city scoring GLORY.
      ;; No city pick; no-ops if nothing sellable there.
      [12 3] (-> state
                (increase-role-with-cost player-key :merchant)
                (sell-for-glory-in player-key (:caravan pdata)))
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
      ;; "Place a Temple adjacent to one of your Raiders." choice = chosen city;
      ;; default = first raider-adjacent city. place-temple-in fires the
      ;; :temple-placed passive (board 13 slot-0 "raider adjacent").
      [13 4] (if-let [target (or choice
                                 (first (distinct (mapcat (fn [[a b]] [a b])
                                                          (keys (:raiders pdata))))))]
               (place-temple-in state player-key target true)
               state)

      ;; ─── Board 14: Roads of Shulgi ─────────────────────────────
      [14 1] (let [;; Place raider adjacent to Lagash first
                    adj (for [r (board-routes state)
                              :when (or (= :lagash (:from r)) (= :lagash (:to r)))]
                          (segment-route-key r))
                    avail (remove #(raider-on-route? (:raiders pdata) %) adj)
                    s (if-let [rk (first avail)]
                        (place-raider-on state player-key rk)
                        state)
                    ;; Then score glory for each raider (including the new one)
                    rc (count-raiders-deployed (get-in s [:players player-key]))]
                (update-in s [:players player-key :glory] + rc))
      ;; "Move a Magistrate to Uruk. Then gain resources matching Uruk's demands."
      ;; FAITHFUL: influence a magistrate to :uruk (real move + raider flips), then
      ;; gain one good per demand token currently in Uruk. (Old arm: flat
      ;; +1 tools/pottery with no magistrate move.)
      [14 2] (let [s (bonus-influence state player-key :uruk)
                   demands (get-in s [:city-demands :uruk] [])]
               (reduce (fn [st d] (add-player-resource st player-key d 1)) s demands))
      [14 3] (let [bag (:demand-bag state (full-demand-bag))  ;; Place 2 demand tokens in Eridu + travel there
                    [bag1 tok1] (draw-demand-token bag)
                    [bag2 tok2] (if bag1 (draw-demand-token bag1) [bag nil])]
                (cond-> state
                  true  (assoc :demand-bag (or bag2 bag1 bag))
                  tok1  (update-in [:city-demands :eridu] (fnil conj []) tok1)
                  tok2  (update-in [:city-demands :eridu] (fnil conj []) tok2)
                  true  (bonus-travel-to player-key :eridu)))
      [14 4] (place-temple-in state player-key :babylon true)

      ;; ─── Board 15: Ascent of Ur-Nammu ──────────────────────────
      [15 1] (let [demands (:demand-tokens pdata [])] ;; Good per demand fulfilled
               (reduce (fn [s d] (add-player-resource s player-key d 1)) state demands))
      [15 2] (-> state ;; Increase Priest + 4 Glory if facedown temple in Babylon
               (increase-role-with-cost player-key :priest)
               (cond-> (some #{:face-down} (temples-at pdata :babylon))
                 (update-in [:players player-key :glory] + 4)))
      ;; "Increase your LOWEST role then take a Travel action (you pick if there
      ;; is a TIE)." FAITHFUL (Bucket A): increase the lowest role WITH cost.
      ;; choice only resolves a tie among the lowest roles; if the supplied
      ;; choice is one of the (tied) lowest roles use it, else use the computed
      ;; lowest. (Travel dropped — disclosed stub. Old auto arm increased FREE;
      ;; old human arm let you pick ANY role.)
      ;; "Increase your lowest role then take a Travel action." The travel is a
      ;; same-turn free travel (board-6 mechanism); offered after the increase
      ;; (works for a human's mid-turn claim; a bot's turn-end claim drops it).
      [15 3] (let [min-lv (apply min (map #(get-in pdata [:roles %] 1) roles))
                   lowest-set (set (filter #(= min-lv (get-in pdata [:roles %] 1)) roles))
                   target (if (contains? lowest-set choice)
                            choice
                            (first (filter lowest-set roles)))]
               (-> (increase-role-with-cost state player-key target)
                   (assoc-in [:players player-key :pending-free-travel] true)))
      [15 4] (let [routes (board-routes state)
                   route-by-key (into {} (for [r routes]
                                           [(segment-route-key r) r]))
                   mag-cities (magistrate-cities state)
                   adj-count (reduce (fn [n rk]
                                       (let [r (get route-by-key rk)]
                                         (if (and r (or (contains? mag-cities (:from r))
                                                        (contains? mag-cities (:to r))))
                                           (inc n) n)))
                                     0 (keys (:raiders pdata)))]
               (update-in state [:players player-key :amity] + (* 3 adj-count)))

      ;; ─── Board 16: Dominion of Hammurabi ────────────────────────
      [16 1] (let [tc (count-temples-placed pdata)] ;; Pottery per temple
               (add-player-resource state player-key :pottery tc))
      [16 2] (let [any-rk (first (free-routes pdata state (constantly true))) ;; Deploy + amity per raider
                   s' (if any-rk (place-raider-on state player-key any-rk) state)
                   rc (count-raiders-deployed (get-in s' [:players player-key]))]
               (update-in s' [:players player-key :amity] + (* 2 rc)))
      [16 3] (-> state ;; Increase Leader twice
               (increase-role-with-cost player-key :leader)
               (increase-role-with-cost player-key :leader))
      ;; "Put two random demand tokens on the city you are in. You may take Sell
      ;; action." FAITHFUL: draw two tokens onto the caravan city, then
      ;; bonus-sell-in that same city (no-ops if nothing sellable). Replaces the
      ;; old flat +1-tools/+3-amity proxy.
      [16 4] (let [caravan (:caravan pdata)]
               (-> state
                   (place-demand-tokens caravan 2)
                   (bonus-sell-in player-key caravan)))

      ;; ─── Board 17: Cunning of Kubaba ────────────────────────────
      ;; "Place a Raider next to Eridu on its point side." PLACE (not flip): put a
      ;; raider on a free route touching Eridu, point-side up (mirrors [28 4]/Kish).
      ;; (De-tagged from :pick-resource in bonus.cljc — no player choice needed.)
      [17 1] (let [avail (free-routes pdata state #(touches? :eridu %))]
               (if-let [rk (first avail)]
                 (-> state
                     (place-raider-on player-key rk)
                     ;; flip the just-placed raider to point side (don't clobber the vector)
                     (flip-one-raider-to-point player-key rk))
                 state))
      [17 2] (reduce (fn [s city] ;; Facedown temple in EACH magistrate city
                       ;; "even if you already have temples there" → genuinely conj
                       ;; a facedown temple (supply-gated inside add-temple).
                       (add-temple s player-key city :face-down))
                     state (magistrate-cities state))
      ;; "Score 8 Amity if you have Uruk surrounded by Raiders. Then you may flip
      ;; one of those raiders." Flip is beneficial (a :point raider can be scored
      ;; by travelling through), so auto-flip one surrounding :raiding raider.
      [17 3] (let [adj-rks (set (map segment-route-key
                                      (routes-from-city :uruk (board-routes state))))
                   player-rks (set (keys (:raiders pdata)))]
               (if (and (seq adj-rks) (every? player-rks adj-rks))
                 (let [s (update-in state [:players player-key :amity] + 8)
                       flip-rk (first (filter #(some #{:raiding}
                                                     (raiders-on (get-in s [:players player-key :raiders]) %))
                                              adj-rks))]
                   (if flip-rk (flip-one-raider-to-point s player-key flip-rk) s))
                 state))
      ;; "Sell to the city your caravan is in for Glory instead." FAITHFUL:
      ;; resolve a REAL sell in the caravan's city, scoring GLORY (+ magistrate
      ;; glory bonus). No-ops if nothing sellable there.
      [17 4] (sell-for-glory-in state player-key (:caravan pdata))

      ;; ─── Board 18: Forge of Tubal-Cain ─────────────────────────
      ;; "Move a Magistrate across a river. You may sell in your caravan's city."
      ;; FIX 2 (typed-movement): the move is across a RIVER edge, so use
      ;; perform-river-influence (flip the river-edge raider + fire :river-crossed),
      ;; NOT bonus-influence (road-clockwise, which flips the wrong raider and
      ;; never crosses a river). choice = a city one river edge from a magistrate
      ;; (descriptor filter :magistrate-river); default = first such city. Then
      ;; sell in the caravan's city. (Old arm: road influence + +2-tools stub.)
      [18 1] (let [dest (or choice (first (magistrate-river-destinations state)))
                   s (cond-> state dest (perform-river-influence player-key dest))]
               (bonus-sell-in s player-key (:caravan pdata)))
      ;; "Take a travel action then score 5 Glory IF you have a facedown temple
      ;; in Samarra." FAITHFUL (Bucket B): grant 0 glory when the condition is
      ;; unmet (was a spurious +2). choice = travel destination (no score effect).
      [18 2] (let [s (if choice
                       (bonus-travel-to state player-key choice)
                       state)]
               (if (some #{:face-down} (temples-at pdata :samarra))
                 (update-in s [:players player-key :glory] + 5)
                 s))
      ;; "Score 6 Amity if you have Kish surrounded by Raiders. Then you may flip
      ;; one of those raiders." Auto-flip one surrounding :raiding raider (beneficial).
      [18 3] (let [adj-rks (set (map segment-route-key
                                      (routes-from-city :kish (board-routes state))))
                   player-rks (set (keys (:raiders pdata)))]
               (if (and (seq adj-rks) (every? player-rks adj-rks))
                 (let [s (update-in state [:players player-key :amity] + 6)
                       flip-rk (first (filter #(some #{:raiding}
                                                     (raiders-on (get-in s [:players player-key :raiders]) %))
                                              adj-rks))]
                   (if flip-rk (flip-one-raider-to-point s player-key flip-rk) s))
                 state))
      [18 4] (let [raiders (:raiders pdata)
                   n       (count-raiders-with-status raiders :point)
                   ;; Drop EVERY :point status across every route, returning each to
                   ;; supply; keep any :raiding raiders (a route may be left non-empty).
                   raiders' (into {}
                                  (keep (fn [[rk v]]
                                          (let [kept (vec (remove #{:point} v))]
                                            (when (seq kept) [rk kept])))
                                        raiders))]
               ;; QA lesson 8: faithful "score then remove" per card text.
               ;;   "Score 4 A for each of your Raiders on their point side.
               ;;    Then remove those raiders."
               (-> state
                   (update-in [:players player-key :amity] + (* 4 n))
                   (assoc-in [:players player-key :raiders] raiders')
                   (update-in [:players player-key :raiders-supply] (fnil + 0) n))) ;; 4 Amity per point raider, then remove

      ;; ─── Board 19: Kilns of Ninkasi ────────────────────────────
      [19 1] (-> state ;; Increase Priest twice
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :priest))
      ;; "Sell to two cities that demand Pottery (you don't have to be there)."
      ;; No move: take the first two cities whose live demands include :pottery
      ;; and resolve a real sell in each (bonus-sell-in no-ops if nothing sellable).
      [19 2] (let [pottery-cities (->> (:city-demands state)
                                       (filter (fn [[_ ds]] (some #{:pottery} ds)))
                                       (map first)
                                       (take 2))]
               (reduce #(bonus-sell-in %1 player-key %2) state pottery-cities))
      ;; "Discard a good to move a Magistrate to your City. Then take a sell
      ;; action." choice = good to discard (default = first good held). Then
      ;; influence magistrate to caravan + sell there.
      [19 3] (let [goods [:tools :pottery :gold :gems]
                   caravan (:caravan pdata)
                   discard (or (when (pos? (get-in pdata [:resources choice] 0)) choice)
                               (first (filter #(pos? (get-in pdata [:resources %] 0)) goods)))]
               (cond-> state
                 discard (update-in [:players player-key :resources discard] dec)
                 true (bonus-influence player-key caravan)
                 true (bonus-sell-in player-key caravan)))
      [19 4] (reduce (fn [s rk] ;; Flip all raiders to point (every raider on every route)
                       (first (flip-raiders-on-route-to-point s player-key rk)))
                     state (keys (:raiders pdata)))

      ;; ─── Board 20: Vision of Rimush ─────────────────────────────
      ;; "Place a Raider on each route with an OPPOSING raider." FAITHFUL
      ;; (Bucket B): target = routes that already carry another player's raider
      ;; (and not one of ours), not routes-from-caravan.
      [20 1] (let [routes (board-routes state)
                   my-rks (set (keys (:raiders pdata)))
                   opposing-rks (for [r routes
                                      :let [rk (segment-route-key r)]
                                      :when (and (not (contains? my-rks rk))
                                                 (some (fn [[pk pd]]
                                                         (and (not= pk player-key)
                                                              (raider-on-route? (:raiders pd) rk)))
                                                       (:players state)))]
                                  rk)]
               (reduce #(place-raider-on %1 player-key %2) state opposing-rks))
      [20 2] (-> state ;; Increase Merchant twice
               (increase-role-with-cost player-key :merchant)
               (increase-role-with-cost player-key :merchant))
      ;; "Influence a Magistrate. Then score Amity based on your leader level."
      ;; FAITHFUL (Bucket A/B): do the influence THEN score leader-level amity
      ;; (NOT a sell). choice = magistrate destination (default = first mag city).
      [20 3] (let [dest (or choice (first (magistrate-cities state)))
                   leader-lv (get-in pdata [:roles :leader] 1)]
               (-> (cond-> state dest (bonus-influence player-key dest))
                   (update-in [:players player-key :amity] + leader-lv)))
      ;; "Take up to four goods based on the action spaces your Astronomers
      ;; occupy." FAITHFUL: for each space an astronomer sits on, collect that
      ;; space's :take goods (space-take-resources; nil for space 7), then take
      ;; up to four of those goods. (Old arm gave a flat tools+gold.)
      [20 4] (let [astros (:astronomers pdata [])
                   goods (->> astros
                              (mapcat space-take-resources)
                              (remove nil?)
                              (take 4))]
               (reduce (fn [s g] (add-player-resource s player-key g 1)) state goods))

      ;; ─── Board 21: Legacy of Eannatum ───────────────────────────
      ;; "If you are in Eridu, travel anywhere via the shortest path." FAITHFUL:
      ;; GATE on the caravan being in Eridu (else no-op). choice = destination;
      ;; bot default = a meaningful far city (any active city other than Eridu —
      ;; the current city would be a no-op travel), preferring Babylon.
      [21 1] (if (= :eridu (:caravan pdata))
               (let [dest (or choice
                              (some #{:babylon} (keys (:city-graph state)))
                              (first (remove #{:eridu} (keys (:city-graph state)))))]
                 (bonus-travel-to state player-key dest))
               state)
      [21 2] (-> state ;; Increase Raider and Leader
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :leader))
      ;; "Travel to an adjacent city then you may Sell to it." choice = adjacent
      ;; destination (default :eridu); travel there + sell.
      [21 3] (let [dest (or choice :eridu)]
               (-> state
                   (bonus-travel-to player-key dest)
                   (bonus-sell-in player-key dest)))
      [21 4] (let [demands (:demand-tokens pdata [])] ;; Glory per demand
               (update-in state [:players player-key :glory] + (count demands)))

      ;; ─── Board 22: Strategy of Naram-Sin ────────────────────────
      [22 1] (-> state ;; Increase Raider and Merchant
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :merchant))
      ;; "Put a random demand token on each of your facedown temples. Only you
      ;; may fulfill those demands." Owner-restricted demand per face-down temple
      ;; city (only this player can sell them, via fulfillable-goods).
      [22 2] (reduce (fn [s city] (place-owned-demand s player-key city))
                     state
                     (for [[c states] (:temples pdata)
                           :when (some #{:face-down} states)] c))
      ;; "Take a good of your choice. Then take a travel action." choice =
      ;; resource (default :pottery); the travel half is dropped (disclosed).
      [22 3] (add-player-resource state player-key (or choice :pottery) 1)
      ;; "Score 2 Amity for each of your Raiders. Then take a travel action."
      ;; FAITHFUL (Bucket A): score 2-amity-per-raider (the primary effect) THEN
      ;; travel to (or choice caravan). (Old human arm dropped the amity entirely.)
      [22 4] (let [rc (count-raiders-deployed pdata)
                   dest (or choice (:caravan pdata))]
               (cond-> (update-in state [:players player-key :amity] + (* 2 rc))
                 dest (bonus-travel-to player-key dest)))

      ;; ─── Board 23: Market of Puabi ──────────────────────────────
      [23 1] (-> state ;; Increase Priest and Merchant
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :merchant))
      ;; "Sell twice to Eridu (you don't need to be there)." No move: resolve two
      ;; real sells at :eridu (each no-ops if nothing matching is sellable).
      [23 2] (-> state
                (bonus-sell-in player-key :eridu)
                (bonus-sell-in player-key :eridu))
      ;; "Take a good of your choice. Then take a travel action. Increase your
      ;; Merchant Role (paying any costs)." FAITHFUL (Bucket A): grant the chosen
      ;; good AND increase merchant with cost. (Old human arm dropped the merchant
      ;; increase.) Travel half dropped (disclosed).
      [23 3] (-> state
                (add-player-resource player-key (or choice :tools) 1)
                (increase-role-with-cost player-key :merchant))
      ;; "Place a Temple in a city with a Magistrate (even if you already have a
      ;; temple there)." choice = magistrate city; default prefers a mag city w/o
      ;; your temple, else falls back to the first mag city (allow-duplicate? true
      ;; genuinely places a 2nd temple there in the multi-temple model).
      [23 4] (let [target (or choice (default-magistrate-target state pdata))]
               (if target (place-temple-in state player-key target true) state))

      ;; ─── Board 24: Siege of Shulme ──────────────────────────────
      [24 1] (-> state ;; Increase Raider and Leader
               (increase-role-with-cost player-key :raider)
               (increase-role-with-cost player-key :leader))
      ;; "Put a random demand token on each Magistrate. Only you may fulfill
      ;; those demands." Owner-restricted demand per magistrate city.
      [24 2] (reduce (fn [s city] (place-owned-demand s player-key city))
                     state
                     (magistrate-cities state))
      [24 3] (let [demands (:demand-tokens pdata [])] ;; Glory per demand
               (update-in state [:players player-key :glory] + (count demands)))
      ;; "Take a good for each demand in cities with Magistrates." FAITHFUL: one
      ;; good per demand TOKEN sitting in any magistrate city (the board's
      ;; :city-demands, NOT the player's own fulfilled :demand-tokens), no cap.
      [24 4] (let [mag-cities (magistrate-cities state)
                   demands (mapcat #(get-in state [:city-demands %] []) mag-cities)]
                (reduce (fn [s d] (add-player-resource s player-key d 1))
                        state demands))

      ;; ─── Board 25: Command of Mesannepada ───────────────────────
      ;; "Influence a Magistrate. Immediately score all of your raiders it moved
      ;; through" (GLORY). FAITHFUL: run the influence, then score 1 Glory for
      ;; each of YOUR raiders the magistrate actually moved through — i.e. those
      ;; flipped :raiding→:point by perform-influence (post-:point keys minus the
      ;; keys already :point before). choice = magistrate destination.
      ;; (Old arm hardcoded (+ 2 point-count), which counted pre-existing points
      ;; and added a phantom +2.)
      [25 1] (let [dest (or choice (first (magistrate-cities state)))
                   pre-points (count-raiders-with-status (:raiders pdata) :point)
                   s (cond-> state dest (bonus-influence player-key dest))
                   post-points (count-raiders-with-status
                                (get-in s [:players player-key :raiders]) :point)
                   flipped (max 0 (- post-points pre-points))]
               (update-in s [:players player-key :glory] + flipped))
      [25 2] (-> state ;; Increase Merchant and Leader
               (increase-role-with-cost player-key :merchant)
               (increase-role-with-cost player-key :leader))
      ;; "Place two facedown temples in your city (even if you already have a
      ;; temple there)." Multi-temple model: genuinely conj two facedown temples
      ;; into the caravan city (supply-gated inside add-temple).
      [25 3] (let [city (:caravan pdata)]
               (if city
                 (-> state
                     (add-temple player-key city :face-down)
                     (add-temple player-key city :face-down))
                 state))
      ;; "Take a good of your choice. Then take a Travel action." Grant the CHOSEN
      ;; good (was hardcoded :gems, ignoring the player's pick — a dual-path bug).
      [25 4] (add-player-resource state player-key (or choice :gems) 1)

      ;; ─── Board 26: Court of Enshakushanna ───────────────────────
      [26 1] (-> state ;; Increase Priest and Leader
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :leader))
      [26 2] (-> state ;; Increase Priest and Raider
               (increase-role-with-cost player-key :priest)
               (increase-role-with-cost player-key :raider))
      ;; "Sell in your city. If you sold Tools or Pottery you may place a Temple
      ;; in your city (even if you already have one there)." FAITHFUL: determine
      ;; WHICH good bonus-sell-in will spend (same selection rule it uses: first
      ;; demand at the city the player can satisfy), resolve the real sell, then
      ;; gate the temple on that good being tools/pottery.
      [26 3] (let [city     (:caravan pdata)
                   demands  (get-in state [:city-demands city] [])
                   res      (get-in pdata [:resources])
                   sold     (first (filter #(pos? (get res % 0)) demands))]
               (cond-> (bonus-sell-in state player-key city)
                 (contains? #{:tools :pottery} sold)
                 (place-temple-in player-key city true)))
      ;; "Place a Raider adjacent to your city. If you surround it, you may place a
      ;; temple in it (even if you already have a temple there)." Deploy on a free
      ;; route adjacent to the caravan city; if that completes the surround
      ;; (recomputed from the POST-deploy state), drop a duplicate-allowed temple.
      [26 4] (let [city (:caravan pdata)
                   s    (bonus-deploy-near state player-key city)]
               (cond-> s
                 (and city (contains? (cities-surrounded-by s player-key) city))
                 (place-temple-in player-key city true)))

      ;; ─── Board 27: Path of Alulim ──────────────────────────────
      ;; "Travel to an adjacent city then you may Sell to it." choice = adjacent
      ;; destination; travel + sell.
      [27 1] (let [dest (or choice (:caravan pdata))]
               (cond-> state
                 dest (-> (bonus-travel-to player-key dest)
                          (bonus-sell-in player-key dest))))
      ;; "Travel to an adjacent city then you may take a Deploy action."
      [27 2] (let [dest (or choice (:caravan pdata))]
               (cond-> state
                 dest (-> (bonus-travel-to player-key dest)
                          (bonus-deploy-near player-key dest))))
      ;; "Travel to an adjacent city then you may place a Temple in it."
      [27 3] (let [dest (or choice (:caravan pdata))]
               (cond-> state
                 dest (-> (bonus-travel-to player-key dest)
                          (place-temple-in player-key dest true))))
      ;; "Take three goods of your choice." choice = one resource per call
      ;; (the WS layer prompts 3×); auto default grants tools/gold/gems.
      [27 4] (if choice
               (add-player-resource state player-key choice 1)
               (-> state
                   (add-player-resource player-key :tools 1)
                   (add-player-resource player-key :gold 1)
                   (add-player-resource player-key :gems 1)))

      ;; ─── Board 28: Stars of Sin-Kashid ─────────────────────────
      ;; "Travel to an adjacent city then place a Temple in it." choice = dest.
      [28 1] (let [dest (or choice (:caravan pdata))]
               (cond-> state
                 dest (-> (bonus-travel-to player-key dest)
                          (place-temple-in player-key dest true))))
      [28 2] (let [dest (or choice (:caravan pdata))]
               (cond-> state
                 dest (-> (bonus-travel-to player-key dest)
                          (place-temple-in player-key dest true))))
      ;; "Sell Gold to your city if it has no Demands. Then place a random demand
      ;; on it." FAITHFUL mirror of the board-10 :sell-gold-empty option, at the
      ;; caravan city: only when the city has NO demands AND you hold gold — spend
      ;; the gold, score merchant-level amity, then draw one random demand onto it.
      [28 3] (let [city    (:caravan pdata)
                   gold    (get-in pdata [:resources :gold] 0)
                   demands (get-in state [:city-demands city] [])]
               (if (and city (pos? gold) (empty? demands))
                 (let [merchant-lv (get-in pdata [:roles :merchant] 1)
                       amity       (get merchant-score merchant-lv 2)
                       bag         (:demand-bag state (full-demand-bag))
                       [bag' token] (draw-demand-token bag)]
                   (-> state
                       (update-in [:players player-key :resources :gold] dec)
                       (update-in [:players player-key :amity] + amity)
                       (cond-> bag'  (assoc :demand-bag bag'))
                       (cond-> token (update-in [:city-demands city] (fnil conj []) token))))
                 state))
      [28 4] (let [avail (free-routes pdata state #(touches? :kish %))] ;; Raider point-side near Kish
               (if-let [rk (first avail)]
                 (-> state
                     (place-raider-on player-key rk)
                     ;; flip the just-placed raider to point side (don't clobber the vector)
                     (flip-one-raider-to-point player-key rk))
                 state))

      ;; ─── Board 29: Treasury of Ibbi-Sin ────────────────────────
      ;; "Decrease your Leader role to increase ALL of your OTHER roles." FAITHFUL
      ;; (Bucket B): other roles are merchant, priest AND raider — add the raider.
      [29 1] (let [ll (get-in pdata [:roles :leader] 1)]
               (if (> ll 1)
                 (-> state
                     (assoc-in [:players player-key :roles :leader] (dec ll))
                     (increase-role-free player-key :merchant)
                     (increase-role-free player-key :priest)
                     (increase-role-free player-key :raider))
                 state))
      ;; "Take a travel action then you may take a sell action." choice = adjacent
      ;; destination (default = stay at caravan); travel there, then sell at the
      ;; post-travel caravan city.
      [29 2] (let [dest (or choice (:caravan pdata))]
               (cond-> state
                 dest (-> (bonus-travel-to player-key dest)
                          (bonus-sell-in player-key dest))))
      [29 3] (let [avail (free-routes pdata state #(= :river (:type %)))] ;; Raider on each river
               (reduce #(place-raider-on %1 player-key %2) state (take 3 avail)))
      ;; "Place a Temple in each city surrounded by your Raiders (even if you have
      ;; a Temple there)." Iterate EVERY city whose adjacent board-routes ALL hold
      ;; one of the player's raiders; drop a duplicate-allowed temple in each.
      [29 4] (reduce #(place-temple-in %1 player-key %2 true)
                     state
                     (cities-surrounded-by state player-key))

      ;; ─── Board 30: Council of Amar-Sin ──────────────────────────
      ;; "Influence a Magistrate then take a Travel action." choice = mag dest;
      ;; the travel is a same-turn free travel (board-6 mechanism), queued after
      ;; the influence.
      [30 1] (let [dest (or choice (first (magistrate-cities state)))]
               (-> (cond-> state dest (bonus-influence player-key dest))
                   (assoc-in [:players player-key :pending-free-travel] true)))
      ;; "Influence a Magistrate then take a Sell action." choice = mag dest;
      ;; influence + sell in that city.
      [30 2] (let [dest (or choice (first (magistrate-cities state)))]
               (cond-> state
                 dest (-> (bonus-influence player-key dest)
                          (bonus-sell-in player-key dest))))
      ;; "Take a Deploy action then Influence a Magistrate." choice = mag dest;
      ;; deploy near caravan + influence.
      [30 3] (let [dest (or choice (first (magistrate-cities state)))]
               (cond-> (bonus-deploy-near state player-key (:caravan pdata))
                 dest (bonus-influence player-key dest)))
      ;; "Influence a Magistrate then take a Temple action." choice = mag dest;
      ;; influence + temple in that city.
      [30 4] (let [dest (or choice (first (magistrate-cities state)))]
               (cond-> state
                 dest (-> (bonus-influence player-key dest)
                          (place-temple-in player-key dest true))))

      ;; ─── Board 31: Horizon of Sharkalisharri ────────────────────
      [31 1] (reduce (fn [s role] ;; Increase all level-1 roles
                       (if (= 1 (get-in s [:players player-key :roles role] 1))
                         (assoc-in s [:players player-key :roles role] 2) s))
                     state roles)
      [31 2] (reduce (fn [s role] ;; Increase all level-3 roles
                       (if (= 3 (get-in s [:players player-key :roles role] 1))
                         (increase-role-with-cost s player-key role) s))
                     state roles)
      ;; "Gain a resource of your CHOICE and place a Facedown temple in your city
      ;; (even if you already have one)." FAITHFUL (Bucket A): grant the chosen
      ;; resource AND genuinely conj a facedown temple into the caravan city.
      ;; choice = resource (default :gems).
      [31 3] (let [city (:caravan pdata)]
               (cond-> (add-player-resource state player-key (or choice :gems) 1)
                 city (add-temple player-key city :face-down)))
      ;; "Gain a resource of your choice and take a Deploy action." FAITHFUL
      ;; (Bucket A): grant the chosen resource AND deploy. (Old human arm dropped
      ;; the deploy; old auto arm forced :tools.) choice = resource (default :tools).
      [31 4] (let [any-rk (first (free-routes pdata state (constantly true)))]
               (cond-> state
                 true (add-player-resource player-key (or choice :tools) 1)
                 any-rk (place-raider-on player-key any-rk)))

      ;; ─── Board 32: Jewel of Ku-Bau ─────────────────────────────
      ;; "Sell in your city then Score Glory for each demand you have fulfilled."
      ;; FAITHFUL: resolve the real sell FIRST (so it counts toward the tally),
      ;; then score 1 Glory per fulfilled demand (the player's :demand-tokens).
      [32 1] (let [s (bonus-sell-in state player-key (:caravan pdata))
                   fulfilled (count (get-in s [:players player-key :demand-tokens] []))]
               (update-in s [:players player-key :glory] + fulfilled))
      ;; "Take a Gem. Take two travel actions." FAITHFUL (Bucket A): grant the gem
      ;; AND travel to (or choice caravan). (Old auto arm gave the gem but no
      ;; travel; old human arm travelled but dropped the gem.) Second travel dropped.
      ;; "Take a Gem. Take two travel actions." Gem + first travel to the chosen
      ;; adjacent city, then a second same-turn free travel (board-6 mechanism).
      [32 2] (let [dest (or choice (:caravan pdata))]
               (-> (add-player-resource state player-key :gems 1)
                   (cond-> dest (bonus-travel-to player-key dest))
                   (assoc-in [:players player-key :pending-free-travel] true)))
      ;; "Place a raider in each route that has one of your Temples in both cities."
      ;; Place on EVERY eligible free route (both endpoints hold your temple), not
      ;; just the first. place-raider-on no-ops past the raider-supply / max cap.
      [32 3] (let [t-cities (set (temple-cities pdata))
                   avail (free-routes pdata state
                                      #(and (contains? t-cities (:from %))
                                            (contains? t-cities (:to %))))]
               (reduce #(place-raider-on %1 player-key %2) state avail))
      ;; "Influence a Magistrate then you may take sell action." choice = mag
      ;; dest; influence + sell in that city.
      [32 4] (let [dest (or choice (first (magistrate-cities state)))]
               (cond-> state
                 dest (-> (bonus-influence player-key dest)
                          (bonus-sell-in player-key dest))))

      ;; ─── Board 33: Vanguard of Enmebaragesi ─────────────────────
      ;; "Decrease your Merchant role to increase ALL of your OTHER roles."
      ;; FAITHFUL (Bucket B): other roles are leader, priest AND raider — add leader.
      [33 1] (let [ml (get-in pdata [:roles :merchant] 1)]
               (if (> ml 1)
                 (-> state
                     (assoc-in [:players player-key :roles :merchant] (dec ml))
                     (increase-role-free player-key :raider)
                     (increase-role-free player-key :priest)
                     (increase-role-free player-key :leader))
                 state))
      ;; "Place a facedown Temple in your city then take a travel action (even if
      ;; you already have a Temple there)." Multi-temple model: genuinely conj a
      ;; facedown temple into the (pre-travel) caravan city, THEN travel to
      ;; (or choice caravan).
      [33 2] (let [temple-city (:caravan pdata)
                   dest (or choice (:caravan pdata))
                   s (cond-> state
                       temple-city (add-temple player-key temple-city :face-down))]
               (cond-> s
                 dest (bonus-travel-to player-key dest)))
      [33 3] (place-temple-in state player-key :uruk true) ;; Temple in Uruk
      ;; "Deploy a raider adjacent to your city then take a travel action."
      ;; choice = travel destination; deploy near caravan + travel.
      [33 4] (let [dest (or choice (:caravan pdata))
                   s (bonus-deploy-near state player-key (:caravan pdata))]
               (cond-> s
                 dest (bonus-travel-to player-key dest)))

      ;; ─── Board 34: Honor of Agga ────────────────────────────────
      ;; "Pay Tools, Tools to place a Raider on each space surrounding Uruk."
      ;; FAITHFUL (Bucket B): place a raider on EVERY available route around Uruk
      ;; (Uruk has up to 4) for a fixed 2 tools — not capped at 2 raiders.
      [34 1] (let [tools (get-in pdata [:resources :tools] 0)
                   avail (free-routes pdata state #(touches? :uruk %))]
               (if (and (>= tools 2) (seq avail))
                 (-> (reduce #(place-raider-on %1 player-key %2) state avail)
                     (update-in [:players player-key :resources :tools] - 2))
                 state))
      ;; "Place a Raider on each route you already have a Raider." FAITHFUL
      ;; (multi-raider model): for EVERY route the player currently holds >= 1
      ;; raider, conj one ADDITIONAL :raiding raider (allow-stack), supply/cap
      ;; permitting. Snapshot the occupied routes first so the newly-added raiders
      ;; don't recursively beget more.
      [34 2] (let [occupied (vec (keys (:raiders pdata)))]
               (reduce #(place-raider-on %1 player-key %2 true) state occupied))
      ;; Board 34 #4 / #5 — "Take a Sell action in each city with a Magistrate +
      ;; your Temple (you don't have to be there)". choice = one such city (the WS
      ;; layer multi-picks); sell there. Auto default sells in each qualifying city.
      [34 3] (if choice
               (bonus-sell-in state player-key choice)
               (let [mag-cities (magistrate-cities state)
                     qualifying (filter #(contains? mag-cities %) (temple-cities pdata))]
                 (reduce #(bonus-sell-in %1 player-key %2) state qualifying)))
      [34 4] (if choice
               (bonus-sell-in state player-key choice)
               (let [mag-cities (magistrate-cities state)
                     qualifying (filter #(contains? mag-cities %) (temple-cities pdata))]
                 (reduce #(bonus-sell-in %1 player-key %2) state qualifying)))

      ;; ─── Board 35: Wanderer of Dumuzi ──────────────────────────
      ;; "Travel then take a Sell action." choice = destination; travel + sell.
      [35 1] (let [dest (or choice (:caravan pdata))]
               (cond-> state
                 dest (-> (bonus-travel-to player-key dest)
                          (bonus-sell-in player-key dest))))
      ;; "Pay any number of Pottery. For each Pottery you paid, place a Temple in a
      ;; city which you already have a Temple." Multi-temple model: genuinely add
      ;; a 2nd (face-up) temple into a city you already hold, one per pottery paid
      ;; (capped by temple supply). Was: placed in temple-LESS cities (wrong).
      [35 2] (let [pottery (get-in pdata [:resources :pottery] 0)
                   owned   (vec (temple-cities pdata))
                   supply  (:temples-supply pdata 0)
                   n       (min pottery supply)]
               (if (and (pos? n) (seq owned))
                 (let [s' (update-in state [:players player-key :resources :pottery] - n)
                       ;; Round-robin over owned cities (a city may end with several).
                       targets (take n (cycle owned))]
                   (reduce #(place-temple-in %1 player-key %2 true) s' targets))
                 state))
      ;; "Increase the role of your choice (paying any costs)." choice = role
      ;; (default = lowest role).
      [35 3] (increase-role-with-cost state player-key
               (or choice
                   (first (sort-by #(get-in state [:players player-key :roles %] 1) roles))))
      ;; "Influence a Magistrate. Score each of your Raiders it moved through."
      ;; FAITHFUL: run the influence, then score 1 Glory for each of YOUR raiders
      ;; the magistrate actually moved through — those flipped :raiding→:point by
      ;; perform-influence (post-:point keys minus the keys already :point before).
      ;; choice = magistrate destination. (Old arm hardcoded (+ 2 point-count),
      ;; counting pre-existing points and adding a phantom +2.)
      [35 4] (let [dest (or choice (first (magistrate-cities state)))
                   pre-points (count-raiders-with-status (:raiders pdata) :point)
                   s (cond-> state dest (bonus-influence player-key dest))
                   post-points (count-raiders-with-status
                                (get-in s [:players player-key :raiders]) :point)
                   flipped (max 0 (- post-points pre-points))]
               (update-in s [:players player-key :glory] + flipped))

      ;; Default: unhandled effect, no-op
      state))

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
        pre-temples (count (all-temple-states pdata))
        pre-raiders (total-raiders (:raiders pdata))
        pre-trace-snapshot (bonus-trace-snapshot state player-key)
        result-state (apply-bonus-dispatch state player-key pdata pc board-id slot-idx)]
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
                        :delta-temples (- (count (all-temple-states post-pdata)) pre-temples)
                        :delta-raiders (- (total-raiders (:raiders post-pdata)) pre-raiders)}]
      (let [logged-state (update-in result-state [:players player-key :board-effects-log]
                                    (fnil conj []) effect-entry)]
        (cond-> logged-state
          (:coverage-trace? state)
          (record-coverage-trace player-key board-id slot-idx
                                 pre-trace-snapshot
                                 (bonus-trace-snapshot logged-state player-key)
                                 nil))))))

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
        delta-temples (- (count (all-temple-states pdata-after)) (count (all-temple-states pdata-before)))
        delta-raiders (- (total-raiders (:raiders pdata-after)) (total-raiders (:raiders pdata-before)))]
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

;; =============================================================================
;; Unified contest-claim primitive (bot + human share ONE path)
;; =============================================================================

(defn- bot-bonus-city-score
  "Heuristic value of a city as a bonus-effect target: demands, a magistrate,
   and your own temple all help. The engine's default agent pick for the bot."
  [state player-key city]
  (let [pdata (get-in state [:players player-key])]
    (+ (count (get-in state [:city-demands city] []))
       (if (magistrate-in-city? state city) 5 0)
       (if (city-has-own-face-up-temple? pdata city) 4 0)
       (if (has-temple? pdata city) 2 0))))

(defn- bot-needed-resource
  "A resource the bot wants to GAIN: one matching an un-maxed role's next
   threshold cost, else :gold (versatile)."
  [state player-key]
  (let [pdata (get-in state [:players player-key])]
    (or (first (for [role roles
                     :let [lvl (get-in pdata [:roles role] 1)
                           cost (get-in role-threshold-costs [role (inc lvl)])]
                     :when (and cost (< lvl max-role-level) (keyword? cost))]
                 cost))
        :gold)))

(defn- bot-bonus-picks
  "Choice value(s) a BOT supplies for an interactive bonus slot, scored from
   public state. Returns a seq of picks (one, or several for :multi / :count)."
  [state player-key desc]
  (case (:type desc)
    :pick-resource (repeat (get desc :count 1) (bot-needed-resource state player-key))
    :pick-role     (let [pdata (get-in state [:players player-key])
                         opts (filter #(< (get-in pdata [:roles %] 1) max-role-level) roles)]
                     [(or (first (sort-by #(get-in pdata [:roles %] 1) opts)) (first roles))])
    :pick-city     (let [elig (eligible-cities-for-filter state player-key (:filter desc))]
                     (cond
                       (empty? elig)  []
                       (:multi desc)  (vec elig)
                       :else          [(apply max-key
                                              #(bot-bonus-city-score state player-key %)
                                              elig)]))
    []))

(defn bot-resolve-bonus
  "Resolve a freshly-uncovered bonus slot for a BOT through the SAME dispatch the
   human UI uses: an interactive slot gets a scored pick applied via
   apply-bonus-with-choice (multi-pick folds over every eligible target); a
   non-interactive slot uses the auto arm. Replaces the bot's old
   apply-bonus-effect(nil auto-default) so bot and human cannot drift."
  [state player-key board-id slot]
  (if-let [desc (bonus-needs-choice? board-id slot)]
    (let [picks (bot-bonus-picks state player-key desc)]
      (if (seq picks)
        (reduce (fn [s pick] (apply-bonus-with-choice s player-key board-id slot pick))
                state picks)
        ;; No eligible target: still fire the arm ONCE so a choice-INDEPENDENT
        ;; rider (e.g. [20 3] "Influence a Magistrate, THEN score amity by leader
        ;; level") happens; the choice-dependent part no-ops via (or choice default).
        (apply-bonus-effect state player-key board-id slot)))
    (apply-bonus-effect state player-key board-id slot)))

(defn apply-feat-claim!
  "THE single contest-claim primitive — called by BOTH the bot
   (check-and-claim-feats) and the human (handle-claim-feat!). Records the claim,
   uncovers `slot`, adds `wild-points`, resolves the slot's bonus via `bonus-fn`
   (state->state: the bot passes a scored apply-bonus-with-choice; the human
   passes their UI pick or `identity` to defer), then fires the :feat-claimed
   passive. Centralizing it makes the two paths structurally unable to drift —
   the old duplicated split is what let the human-passive bug exist."
  [state player-key contest-id slot wild-points bonus-fn]
  (-> state
      (update-in [:contest-claims contest-id] (fnil conj []) player-key)
      (assoc-in [:players player-key :bonus-board slot] :uncovered)
      (update-in [:players player-key :wild-points] (fnil + 0) wild-points)
      (bonus-fn)
      (apply-passive player-key :feat-claimed {:contest-id contest-id :slot slot})))

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
                     ;; Shared claim primitive: the bot resolves the uncovered
                     ;; slot's bonus through bot-resolve-bonus (a scored pick via
                     ;; apply-bonus-with-choice) — the SAME dispatch the human UI
                     ;; uses, instead of the old apply-bonus-effect nil-default.
                     s' (apply-feat-claim! s player-key contest-id best-slot wild-points
                                           #(bot-resolve-bonus % player-key board-id best-slot))
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
;; Card data re-exports (moved to eridu.cards in QA lesson 10)
;; =============================================================================
;; Kept here as defs so existing callers (game/bonus-boards-by-id etc.) still
;; resolve. New code should require eridu.cards directly.

(def starting-cards       cards/starting-cards)
(def bonus-contests       cards/bonus-contests)
(def bonus-boards         cards/bonus-boards)
(def bonus-boards-by-id   cards/bonus-boards-by-id)
(def bonus-contests-by-id cards/bonus-contests-by-id)

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
     :raiders          {}     ;; {route-key -> [(:raiding | :point) ...]} (multi-raider-per-route)
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
        ;; Place one face-up temple at starting city (vector — multi-temple model)
        (assoc-in [:temples (:caravan player)] [:face-up])
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
  "Count how many face-down temples a player has (across all cities)."
  [player-data]
  (count (filter #{:face-down} (all-temple-states player-data))))

(defn count-temples-placed
  "Total temples on the board for a player (a city may hold more than one)."
  [player-data]
  (count (all-temple-states player-data)))

(defn count-raiders-deployed
  "Total raiders deployed on routes for a player (sum across all routes)."
  [player-data]
  (total-raiders (:raiders player-data)))

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
  (some #{city} (vals (:magistrates state))))

(defn magistrate-cities
  "Set of cities currently hosting magistrates."
  [state]
  (set (vals (:magistrates state {}))))

(defn magistrate-and-my-temple-cities
  "Cities where this player has a temple AND a magistrate is present.
   Used by Board 34 #4/#5: take a sell action in each such city."
  [state player-key]
  (let [mag (magistrate-cities state)
        my-temples (set (temple-cities (get-in state [:players player-key])))]
    (vec (filter mag my-temples))))

(defn cities-adjacent-to-my-raiders
  "Cities at either endpoint of a route the player has a Raider on — the legal
   targets for Board 13 #4 ('place a Temple adjacent to one of your Raiders')."
  [state player-key]
  (->> (keys (get-in state [:players player-key :raiders] {}))
       (mapcat (fn [[a b]] [a b]))
       distinct
       vec))

(defn eligible-cities-for-filter
  "FIX 3: legal target cities for a :pick-city bonus `filter`, computed
   server-side so the WS layer surfaces a concrete list for EVERY state-dependent
   filter (was only :magistrate-and-my-temple + :adjacent-to-raider — leaving
   e.g. board-13 #4 / board-18 #1 with an empty/absent picker). Returns a vector
   (possibly empty); nil for filters with no state-dependent target set."
  [state player-key filter]
  (case filter
    :magistrate               (vec (magistrate-cities state))
    :adjacent                 (vec (get-in state [:city-graph
                                                  (get-in state [:players player-key :caravan])]))
    :any                      (vec (keys (:city-graph state)))
    :adjacent-to-raider       (cities-adjacent-to-my-raiders state player-key)
    :magistrate-and-my-temple (magistrate-and-my-temple-cities state player-key)
    :magistrate-river         (magistrate-river-destinations state)
    nil))

;; --- State-query helpers shared by personality.cljc and eridu_ws.clj ---
;; These were duplicated until lesson 4 of the QA pass. Bodies are unchanged
;; from the originals.

(defn space-action-types
  "Set of action types available on the action-board space."
  [space-id]
  (set (map :type (:actions (get action-spaces space-id)))))

(defn space-gives-resources
  "Resources granted by the take action on this space (or nil if none)."
  [space-id]
  (some :resources (:actions (get action-spaces space-id))))

(defn has-resource-excess?
  "True if the player has more than 2 of any resource in the given set."
  [pdata resources]
  (some #(> (get-in pdata [:resources %] 0) 2) resources))

(defn city-has-sellable-demand?
  "True if the city has a demand the player can currently fulfill (open demands
   or the player's own owner-restricted demands there)."
  [state player city]
  (let [resources (get-in state [:players player :resources])]
    (some #(pos? (get resources % 0)) (fulfillable-goods state player city))))

(defn city-has-own-face-up-temple?
  "True if the player has at least one face-up temple in the given city."
  [pdata city]
  (boolean (some #{:face-up} (temples-at pdata city))))

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
        ;; Clear an unused free-travel grant (boards 6/31) so it can't leak into
        ;; the next turn — the bonus travel is a same-turn opportunity only.
        state (update-in state [:players current-player] dissoc :pending-free-travel)
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

(defn- build-base-state
  "Shared setup for both normal and solo game modes.
   board-count: number of bonus boards to select (= player count for normal, 1 for solo).
   cities: set of cities in play.
   graph, routes: pre-computed board topology."
  [cities graph routes board-count]
  (let [[bag city-demands] (fill-demand-spaces
                            (full-demand-bag) {} (vec cities))
        contest-pairs (vals (group-by #(first (name (:id %))) bonus-contests))
        selected-pairs (take 5 (shuffle contest-pairs))
        contests (vec (map #(rand-nth %) selected-pairs))
        boards (vec (take board-count (shuffle bonus-boards)))
        mag-cities (filterv cities [:uruk :kish])]
    {:contests      contests
     :boards        boards
     :city-demands  city-demands
     :demand-bag    bag
     :graph         graph
     :routes        routes
     :mag-cities    mag-cities}))

(defn- assign-feat-chains
  "Each player plans a 2-3 feat chain and derives targets from it."
  [base-state turn-order]
  (reduce (fn [s pk]
            (let [chain (plan-feat-chain s pk)
                  targets (if (seq chain)
                            (vec (take 2 chain))
                            (select-target-feats s pk))]
              (-> s
                  (assoc-in [:players pk :feat-chain] chain)
                  (assoc-in [:players pk :target-feats] targets))))
          base-state
          turn-order))

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
        {:keys [contests boards city-demands demand-bag graph routes mag-cities]}
        (build-base-state cities (city-graph player-count) (active-routes player-count) player-count)]
    (assign-feat-chains
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
      :demand-bag         demand-bag
      :magistrates        {:mag-0 (first mag-cities)
                           :mag-1 (second mag-cities)}
      :first-player       (first turn-order)
      :contests           contests
      :contest-claims     {}
      :bonus-boards       (zipmap turn-order (map :id boards))
      :log                []
      :game-over          nil}
     turn-order)))

(defn initial-solo-state
  "Create initial state for solo mode.
   One player with 6 astronomers in 3 color pairs.
   Full 8-city board. All 5 feats must be met to win."
  [player-key]
  (let [card (rand-nth starting-cards)
        player (-> (make-player player-key card 4)
                   (assoc :num-astronomers 6))
        player (setup-player player 4)
        {:keys [contests boards city-demands demand-bag graph routes mag-cities]}
        (build-base-state all-cities (city-graph 4) (active-routes 4) 1)
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
              :demand-bag         demand-bag
              :magistrates        {:mag-0 (first mag-cities)
                                   :mag-1 (second mag-cities)}
              :first-player       player-key
              :contests           contests
              :contest-claims     {}
              :bonus-boards       {player-key (:id (first boards))}
              :solo-pairs         pair-order
              :log                []
              :game-over          nil}]
    (assign-feat-chains base [player-key])))
