(ns eridu.personality
  "AI personality system for Eridu. Each personality is a weight vector
   that parameterizes the agent-step decision function."
  (:require
   [eridu.game :as game]
   [eridu.choice :as choice]))

;; =============================================================================
;; Personality schema — a weight vector that controls AI behavior
;; =============================================================================

(def default-weights
  "Baseline neutral personality — all weights at moderate values."
  {:name            "Default"
   ;; Action priorities (0-2 scale, higher = more preferred)
   :sell-weight      1.0
   :temple-weight    1.0
   :deploy-weight    1.0
   :influence-weight 1.0
   :travel-weight    0.5
   :take-weight      1.0
   ;; Role increase priority order (vector of keywords)
   :role-priority    [:merchant :priest :raider :leader]
   ;; Strategic weights (0-1 scale)
   :track-balance    0.5    ;; 0=maximize stronger track, 1=strictly balance
   :early-role-bias  0.7    ;; how much to prefer role increases in early game
   :chain-weight     0.5    ;; how much to weigh dice chaining lookahead
   :contest-focus    0.3    ;; how much to steer toward contest conditions
   :resource-hoard   0.3    ;; tendency to avoid spending resources
   ;; Resource value adjustments
   :excess-penalty   3.0    ;; penalty for taking resources already >2
   ;; Tactical modifiers
   :temple-in-demand-city 1.5  ;; prefer temples in cities with demands
   :deploy-near-opponents 1.5  ;; prefer raiders near opponent caravans
   :travel-for-temple     2.0  ;; bonus for traveling to flip own temple
   :travel-for-sell       1.8  ;; bonus for traveling to sellable city
   :influence-flip-raider 2.5  ;; bonus for influence that flips own raider
   })

;; =============================================================================
;; Hand-designed archetypes
;; =============================================================================

(def archetype-merchant
  "The Merchant — sell-heavy, amity-focused, goods accumulation."
  (merge default-weights
         {:name            "The Merchant"
          :sell-weight      2.0
          :temple-weight    1.2
          :deploy-weight    0.4
          :influence-weight 0.6
          :travel-weight    0.8
          :role-priority    [:merchant :priest :leader :raider]
          :track-balance    0.3   ;; lean into amity
          :early-role-bias  0.8
          :travel-for-sell  2.5}))

(def archetype-warlord
  "The Warlord — deploy-heavy, glory-focused, aggressive raider placement."
  (merge default-weights
         {:name            "The Warlord"
          :sell-weight      0.5
          :temple-weight    0.4
          :deploy-weight    2.0
          :influence-weight 1.5
          :travel-weight    0.6
          :role-priority    [:raider :leader :merchant :priest]
          :track-balance    0.3
          :deploy-near-opponents 2.5
          :influence-flip-raider 3.0}))

(def archetype-pilgrim
  "The Pilgrim — temple + travel focused, priest/merchant, balanced scoring."
  (merge default-weights
         {:name            "The Pilgrim"
          :sell-weight      1.0
          :temple-weight    2.0
          :deploy-weight    0.3
          :influence-weight 0.8
          :travel-weight    1.5
          :role-priority    [:priest :merchant :leader :raider]
          :track-balance    0.7
          :early-role-bias  0.9
          :travel-for-temple 3.0}))

(def archetype-diplomat
  "The Diplomat — influence-heavy, leader/merchant, magistrate bonus exploitation."
  (merge default-weights
         {:name            "The Diplomat"
          :sell-weight      1.2
          :temple-weight    1.0
          :deploy-weight    0.8
          :influence-weight 2.0
          :travel-weight    0.7
          :role-priority    [:leader :merchant :priest :raider]
          :track-balance    0.6
          :influence-flip-raider 2.0}))

(def archetype-scholar
  "The Scholar — role-maxing, contest-focused, balanced across all actions."
  (merge default-weights
         {:name            "The Scholar"
          :sell-weight      1.0
          :temple-weight    1.0
          :deploy-weight    1.0
          :influence-weight 1.0
          :travel-weight    0.8
          :role-priority    [:priest :raider :merchant :leader]
          :track-balance    0.8
          :early-role-bias  1.0
          :contest-focus    0.8}))

(def archetype-opportunist
  "The Opportunist — reactive, high chain-weight, exploits board state."
  (merge default-weights
         {:name            "The Opportunist"
          :sell-weight      1.3
          :temple-weight    1.0
          :deploy-weight    1.2
          :influence-weight 1.2
          :travel-weight    1.0
          :role-priority    [:merchant :raider :priest :leader]
          :track-balance    0.5
          :chain-weight     1.0
          :contest-focus    0.5
          :travel-for-sell  2.2
          :travel-for-temple 2.2}))

(def archetypes
  [archetype-merchant archetype-warlord archetype-pilgrim
   archetype-diplomat archetype-scholar archetype-opportunist])

;; =============================================================================
;; Weight-based decision engine
;; =============================================================================

(defn- game-progress [state]
  (let [round (:round state 1)
        turn  (:turn-in-round state 1)]
    (/ (+ (* (dec round) game/turns-per-round) (dec turn))
       (* game/rounds-per-game game/turns-per-round))))

(defn- space-action-types [space-id]
  (set (map :type (:actions (get game/action-spaces space-id)))))

(defn- space-gives-resources [space-id]
  (some :resources (:actions (get game/action-spaces space-id))))

(defn- has-resource-excess? [pdata resources]
  (some #(> (get-in pdata [:resources %] 0) 2) resources))

(defn- city-has-sellable-demand? [state player city]
  (let [pdata (game/player-data state player)
        demands (get-in state [:city-demands city] [])
        resources (:resources pdata)]
    (some #(pos? (get resources % 0)) demands)))

(defn- city-has-own-face-up-temple? [pdata city]
  (= :face-up (get-in pdata [:temples city])))

(defn- weighted-action-priority
  "Build action priority map from personality weights and game state."
  [weights state player pdata]
  (let [caravan-city (:caravan pdata)
        can-sell (city-has-sellable-demand? state player caravan-city)
        has-face-up (city-has-own-face-up-temple? pdata caravan-city)
        amity (:amity pdata 0)
        glory (:glory pdata 0)
        balance (:track-balance weights 0.5)
        ;; Lower track influences weight direction
        amity-need (if (<= amity glory)
                     (+ 1.0 (* balance 0.5))
                     (- 1.0 (* balance 0.3)))
        glory-need (if (< glory amity)
                     (+ 1.0 (* balance 0.5))
                     (- 1.0 (* balance 0.3)))]
    {:take     (* (:take-weight weights 1.0) 1.0)
     :sell     (* (:sell-weight weights 1.0) amity-need (if can-sell 1.5 0.5))
     :temple   (* (:temple-weight weights 1.0) amity-need)
     :deploy   (* (:deploy-weight weights 1.0) glory-need)
     :influence (* (:influence-weight weights 1.0) glory-need)
     :travel   (* (:travel-weight weights 0.5)
                  (cond
                    has-face-up (* (:travel-for-temple weights 2.0) 1.0)
                    can-sell    0.3 ;; already here, sell instead
                    :else       0.6))}))

(defn- chain-score
  "Evaluate dice chaining potential for a die choice."
  [weights state player pdata dest remaining-dice]
  (if (or (empty? remaining-dice) (< (:chain-weight weights 0.5) 0.1))
    0
    (let [types (space-action-types dest)
          astro-positions (:astronomers pdata)
          scores
          (for [d remaining-dice
                pos astro-positions
                :let [other-dest (game/move-astronomer-clockwise pos d)
                      other-types (space-action-types other-dest)
                      combo
                      (cond-> 0
                        (and (contains? types :travel) (contains? other-types :sell)) (+ 4)
                        (and (contains? types :sell) (contains? other-types :travel)) (+ 3)
                        (and (contains? types :deploy) (contains? other-types :influence)) (+ 5)
                        (and (contains? types :influence) (contains? other-types :deploy)) (+ 4)
                        (and (contains? types :temple) (contains? other-types :travel)) (+ 4))]]
            combo)]
      (* (:chain-weight weights 0.5) (if (seq scores) (apply max scores) 0)))))

(defn personality-step
  "Pick a choice for a bot with the given personality weights.
   Returns [choice-key next-state] or nil."
  [state weights]
  (let [[phase choices] (choice/find-state-raw state)]
    (when (and (not= phase :game-over) (seq choices))
      (let [player (game/current-player state)
            pdata  (game/player-data state player)
            progress (game-progress state)
            amity (:amity pdata 0)
            glory (:glory pdata 0)
            lower-track (if (<= amity glory) :amity :glory)

            pick
            (case phase
              ;; ── Die selection with chaining ─────────────────────────────
              :choose-die
              (let [dice (get pdata :dice-available [])
                    astro-pos (:astronomers pdata)
                    scored
                    (for [idx (range (count dice))
                          :let [die-val (nth dice idx)
                                remaining (into (subvec dice 0 idx)
                                                (subvec dice (inc idx)))
                                per-astro
                                (for [apos astro-pos
                                      :let [dest (game/move-astronomer-clockwise apos die-val)
                                            on-space (count (game/astronomers-on-space state dest))
                                            will-be-alone (= on-space 0)
                                            space-res (space-gives-resources dest)
                                            res-pen (if (and space-res (has-resource-excess? pdata space-res))
                                                      (- (:excess-penalty weights 3.0))
                                                      0)
                                            ch-score (chain-score weights state player pdata dest remaining)
                                            early-bias (:early-role-bias weights 0.7)]]
                                  (+ res-pen ch-score
                                     (if (< progress 0.4)
                                       (if will-be-alone (* 10 early-bias) (+ 2 on-space))
                                       (+ (* on-space 5) (if will-be-alone 1 0)))))
                                best (apply max per-astro)]]
                      [best idx])]
                (if (seq scored)
                  (second (last (sort scored)))
                  0))

              ;; ── Astronomer selection ────────────────────────────────────
              :choose-astronomer
              (let [die-val (get-in state [:player-turn :die-value])
                    astro-pos (:astronomers pdata)
                    action-pri (weighted-action-priority weights state player pdata)
                    scored
                    (for [idx (range (count astro-pos))
                          :when (contains? choices idx)
                          :let [pos (nth astro-pos idx)
                                dest (game/move-astronomer-clockwise pos die-val)
                                on-space (count (game/astronomers-on-space state dest))
                                will-be-alone (= on-space 0)
                                types (space-action-types dest)
                                ;; Sum action priorities for actions on this space
                                action-val (reduce + (map #(get action-pri % 0) types))
                                space-res (space-gives-resources dest)
                                res-pen (if (and space-res (has-resource-excess? pdata space-res))
                                          (- (:excess-penalty weights 3.0)) 0)]]
                      [(+ res-pen action-val
                          (if (< progress 0.4)
                            (if will-be-alone (* 10 (:early-role-bias weights 0.7))
                                (+ 2 on-space))
                            (+ (* on-space 5) (if will-be-alone 1 0))))
                       idx])]
                (if (seq scored)
                  (second (last (sort scored)))
                  (first (keys choices))))

              ;; ── Landing resolution ─────────────────────────────────────
              :resolve-landing
              (cond
                (and (contains? choices :begin)
                     (> progress (* 0.4 (:early-role-bias weights 0.7))))
                :begin
                (contains? choices :increase-role) :increase-role
                (contains? choices :begin) :begin
                :else (first (keys choices)))

              ;; ── Role increase ──────────────────────────────────────────
              :choose-role-increase
              (if (> (count choices) 1)
                (let [role-choices (dissoc choices :skip)
                      role-levels (:roles pdata)
                      priority-order (:role-priority weights [:merchant :priest :raider :leader])
                      scored (for [role (keys role-choices)
                                   :when (keyword? role)
                                   :let [pri-idx (.indexOf priority-order role)
                                         pri (if (neg? pri-idx) 99 pri-idx)
                                         level (get role-levels role 1)]]
                               [(+ (* pri 3) level) role])]
                  (if (seq scored)
                    (second (first (sort scored)))
                    (or (first (keys role-choices)) :skip)))
                (first (keys choices)))

              ;; ── Action selection ────────────────────────────────────────
              :choose-action
              (if (contains? choices :done)
                :done
                (let [space (get-in state [:player-turn :space])
                      action-pri (weighted-action-priority weights state player pdata)
                      action-choices (dissoc choices :done)
                      scored
                      (for [[idx _] action-choices
                            :let [action (nth (:actions (get game/action-spaces space)) idx)
                                  atype (:type action)
                                  base-pri (get action-pri atype 1.0)
                                  res-pen (if (and (= atype :take)
                                                   (:resources action)
                                                   (has-resource-excess? pdata (:resources action)))
                                            (- (:excess-penalty weights 3.0))
                                            0)]]
                        [(+ base-pri res-pen) idx])]
                  (if (seq scored)
                    (second (last (sort scored)))
                    (first (keys choices)))))

              ;; ── Sell ────────────────────────────────────────────────────
              :resolve-sell
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (let [resources (:resources pdata)]
                    (apply max-key #(get resources % 0) (keys non-skip)))
                  :skip))

              ;; ── Temple ──────────────────────────────────────────────────
              :resolve-temple
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (let [scored (for [city (keys non-skip)
                                    :let [demands (count (get-in state [:city-demands city] []))
                                          has-mag (if (game/magistrate-in-city? state city) 1 0)]]
                                 [(+ (* demands (:temple-in-demand-city weights 1.5))
                                     (* has-mag 3))
                                  city])]
                    (if (seq scored)
                      (second (last (sort scored)))
                      (first (keys non-skip))))
                  :skip))

              ;; ── Deploy ──────────────────────────────────────────────────
              :resolve-deploy
              (let [non-skip (dissoc choices :skip :done)]
                (if (seq non-skip)
                  (let [scored (for [rk (keys non-skip)
                                    :let [[c1 c2] rk
                                          near-opp (count
                                                    (for [[pk pd] (:players state)
                                                          :when (not= pk player)
                                                          :when (or (= (:caravan pd) c1)
                                                                    (= (:caravan pd) c2))]
                                                      pk))
                                          d1 (count (get-in state [:city-demands c1] []))
                                          d2 (count (get-in state [:city-demands c2] []))]]
                                 [(+ d1 d2 (* near-opp (:deploy-near-opponents weights 1.5)))
                                  rk])]
                    (if (seq scored)
                      (second (last (sort scored)))
                      (first (keys non-skip))))
                  (or (:done choices) (first (keys choices)))))

              ;; ── Travel ──────────────────────────────────────────────────
              :resolve-travel
              (let [non-skip (dissoc choices :skip)
                    caravan-city (:caravan pdata)]
                (if (seq non-skip)
                  (let [scored
                        (for [dest (keys non-skip)
                              :let [has-temple (city-has-own-face-up-temple? pdata dest)
                                    can-sell (city-has-sellable-demand? state player dest)
                                    has-mag (game/magistrate-in-city? state dest)
                                    rk (game/route-key caravan-city dest)
                                    own-point (= :point (get-in pdata [:raiders rk]))]]
                          [(+ (if has-temple (* (:travel-for-temple weights 2.0) 5) 0)
                              (if can-sell (* (:travel-for-sell weights 1.8) 4) 0)
                              (if own-point 7 0)
                              (if has-mag 3 0))
                           dest])
                        best (last (sort scored))]
                    (if (and best (pos? (first best)))
                      (second best)
                      (if (contains? choices :skip) :skip (second (first (sort scored))))))
                  :skip))

              ;; ── Travel continue ─────────────────────────────────────────
              :travel-continue :done

              ;; ── Influence ───────────────────────────────────────────────
              :resolve-influence
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (let [scored
                        (for [[k _next-s] non-skip
                              :let [dest (when (vector? k) (second k))
                                    near-own-point
                                    (when dest
                                      (some (fn [[rk rs]]
                                              (and (= rs :point)
                                                   (or (= dest (first rk))
                                                       (= dest (second rk)))))
                                            (:raiders pdata)))
                                    has-temple (and dest (contains? (:temples pdata) dest))
                                    has-demands (and dest (seq (get-in state [:city-demands dest] [])))]]
                          [(+ (if near-own-point (* (:influence-flip-raider weights 2.5) 4) 0)
                              (if has-temple 5 0)
                              (if has-demands 3 0))
                           k])]
                    (if (seq scored)
                      (second (last (sort scored)))
                      (first (keys non-skip))))
                  :skip))

              :resolve-take :done
              (first (keys choices)))]

        (when-let [next-s (get choices pick)]
          [pick next-s])))))

;; =============================================================================
;; Random personality generation (for genetic algorithm)
;; =============================================================================

(defn random-personality
  "Generate a random personality by perturbing default weights."
  ([] (random-personality "Random"))
  ([name-prefix]
   (let [id (subs (str (java.util.UUID/randomUUID)) 0 8)]
     (merge default-weights
            {:name (str name-prefix "-" id)
             :sell-weight      (+ 0.2 (rand 1.8))
             :temple-weight    (+ 0.2 (rand 1.8))
             :deploy-weight    (+ 0.2 (rand 1.8))
             :influence-weight (+ 0.2 (rand 1.8))
             :travel-weight    (+ 0.1 (rand 1.4))
             :take-weight      (+ 0.5 (rand 1.0))
             :role-priority    (shuffle [:merchant :priest :raider :leader])
             :track-balance    (rand)
             :early-role-bias  (+ 0.3 (rand 0.7))
             :chain-weight     (rand)
             :contest-focus    (rand)
             :resource-hoard   (rand)
             :excess-penalty   (+ 1.0 (rand 5.0))
             :temple-in-demand-city (+ 0.5 (rand 2.5))
             :deploy-near-opponents (+ 0.5 (rand 2.5))
             :travel-for-temple     (+ 0.5 (rand 3.5))
             :travel-for-sell       (+ 0.5 (rand 3.0))
             :influence-flip-raider (+ 0.5 (rand 3.5))}))))

(defn mutate-personality
  "Mutate a personality by tweaking random weights."
  [personality mutation-rate]
  (let [numeric-keys [:sell-weight :temple-weight :deploy-weight :influence-weight
                       :travel-weight :take-weight :track-balance :early-role-bias
                       :chain-weight :contest-focus :resource-hoard :excess-penalty
                       :temple-in-demand-city :deploy-near-opponents
                       :travel-for-temple :travel-for-sell :influence-flip-raider]
        mutated (reduce (fn [p k]
                          (if (< (rand) mutation-rate)
                            (let [v (get p k 1.0)
                                  delta (* v (- (rand 0.6) 0.3))] ;; ±30%
                              (assoc p k (max 0.05 (+ v delta))))
                            p))
                        personality
                        numeric-keys)
        ;; Occasionally swap role priority
        mutated (if (< (rand) (* mutation-rate 0.5))
                  (let [rp (:role-priority mutated)
                        i (rand-int (count rp))
                        j (rand-int (count rp))
                        swapped (assoc (vec rp) i (nth rp j) j (nth rp i))]
                    (assoc mutated :role-priority swapped))
                  mutated)]
    (assoc mutated :name (str (:name personality) "-m"))))

(defn crossover
  "Create a child personality from two parents via uniform crossover."
  [parent-a parent-b]
  (let [numeric-keys [:sell-weight :temple-weight :deploy-weight :influence-weight
                       :travel-weight :take-weight :track-balance :early-role-bias
                       :chain-weight :contest-focus :resource-hoard :excess-penalty
                       :temple-in-demand-city :deploy-near-opponents
                       :travel-for-temple :travel-for-sell :influence-flip-raider]
        child (reduce (fn [c k]
                        (assoc c k (if (< (rand) 0.5)
                                     (get parent-a k 1.0)
                                     (get parent-b k 1.0))))
                      parent-a
                      numeric-keys)
        child (assoc child :role-priority
                     (if (< (rand) 0.5)
                       (:role-priority parent-a)
                       (:role-priority parent-b)))
        id (subs (str (java.util.UUID/randomUUID)) 0 8)]
    (assoc child :name (str "Gen-" id))))
