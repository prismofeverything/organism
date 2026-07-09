(ns eridu.decision
  "Unified decision-making algorithm for Eridu bots.

   The bot is a *pure decision-making module*: it scores the SAME presented
   choices a human sees (the `choices-map` from `eridu.choice/find-state-raw`)
   and never reaches into engine internals to interpret a choice. The
   GA-evolved personality weight vector is an INPUT to this algorithm, not the
   scorer itself.

   The contract of `decide`, at every decision point, is:
     a. read the presented choices  (`[phase choices] = find-state-raw`),
     b. extract contextual features per choice — features come from the
        presented `(choice-key, next-state)` pair (the next-state is the
        consequence the human would see) plus public game state,
     c. process the personality weights THROUGH those features, applying
        contextual adjustments that re-weigh aspects by game state
        (`turn-context`), and
     d. return the chosen `[choice-key next-state]`.

   Genetics stay in `eridu.personality` (the weight schema, archetypes, and GA
   operators). This namespace is how those genetics are *expressed*."
  (:require
   [clojure.set]
   [eridu.game :as game]
   [eridu.choice :as choice]))

;; =============================================================================
;; Shared state-derived helpers (context inputs — public game state only)
;; =============================================================================

(defn- game-progress [state]
  (let [round (:round state 1)
        turn  (:turn-in-round state 1)
        tpr   (game/turns-per-round state)]
    (/ (+ (* (dec round) tpr) (dec turn))
       (* game/rounds-per-game tpr))))

(defn- feat-needs-resource?
  "True if any planned-chain feat benefits from holding onto this resource."
  [pdata resource]
  (let [targets (or (seq (:feat-chain pdata))
                    (:target-feats pdata []))]
    (some (fn [c]
            (case (:id c)
              (:A1 :K1) (#{:gems :gold} resource)
              :A2 (#{:tools :pottery} resource)
              (:B1 :B2) true  ;; any resource helps fulfills
              :J2 (= :tools resource)
              :L1 (= :gems resource)
              :L2 (= :pottery resource)
              nil))
          targets)))

(defn- feat-action-boost
  "Calculate per-action-type bonus from the planned feat chain.
   Returns a map of action-type → bonus value.
   Prioritizes the CURRENT (first unachieved) feat in the chain. Once a feat
   is claimed, focus shifts to the next feat. Deeper feats in the chain still
   get a weaker boost so bots do early prep.
   Higher feat-awareness personality weight → stronger steering."
  [weights state player pdata]
  (let [awareness (:feat-awareness weights 0.3)
        feat-seq (:feat-sequence weights 0.4)
        closure-urgency (:feat-closure-urgency weights 0.5)
        ;; Opponent-feat-race urgency: contests are claimed automatically and
        ;; the first claimer gets 3 wild points vs 2/1/1 for the rest, but the
        ;; bot is otherwise blind to the race. When an opponent is close to
        ;; claiming a contest we also pursue, scale up the boost so we move to
        ;; claim first. NEUTRAL and cheap at default — at 0.0 the opponent
        ;; feat-progress scan is skipped and the multiplier is exactly 1.0.
        race-urgency (:feat-race-urgency weights 0.0)
        claims (:contest-claims state {})
        already-claimed? (fn [cid] (some #{player} (get claims cid [])))
        ;; Use feat-chain if present, else fall back to target-feats
        full-chain (or (seq (:feat-chain pdata))
                       (:target-feats pdata []))
        ;; Drop feats already claimed — focus on the next unachieved one
        active-chain (vec (remove #(already-claimed? (:id %)) full-chain))
        boosts (reduce
                (fn [m [idx contest]]
                  (let [[progress _] (game/feat-progress state player contest)
                        actions (game/feat-action-profile (:id contest))
                        ;; Base boost: stronger when far from goal
                        base-scale (* awareness (- 1.5 progress) 3.0)
                        ;; Closure urgency for near-complete feats
                        closure-bonus (if (> progress 0.6)
                                        (* closure-urgency progress 6.0)
                                        0)
                        ;; Chain position weighting: #0 (current) gets biggest boost.
                        ;; feat-seq controls spread: low = laser-focus on current.
                        pos-multiplier (case idx
                                         0 (+ 1.0 (- 1.0 feat-seq))  ;; current: 1.0-2.0
                                         1 (+ 0.3 (* feat-seq 0.5))   ;; next: 0.3-0.8
                                         2 (* feat-seq 0.3)            ;; far: 0-0.3
                                         (* feat-seq 0.1))
                        ;; Race multiplier: how close is the nearest opponent to
                        ;; claiming THIS contest? At race-urgency 0 the scan is
                        ;; skipped and the multiplier is exactly 1.0 (neutral).
                        opp-progress (if (pos? race-urgency)
                                       (apply max 0.0
                                              (for [[pk _] (:players state)
                                                    :when (not= pk player)]
                                                (first (game/feat-progress state pk contest))))
                                       0.0)
                        race-mult (+ 1.0 (* race-urgency opp-progress))
                        scale (* (+ base-scale closure-bonus) pos-multiplier race-mult)]
                    (reduce (fn [m2 a] (update m2 a (fnil + 0) scale))
                            m actions)))
                {}
                (map-indexed vector active-chain))]
    boosts))

(defn- weighted-action-priority
  "Build action priority map from personality weights and game state.
   Incorporates role-action coupling: higher role levels boost corresponding actions."
  [weights state player pdata]
  (let [caravan-city (:caravan pdata)
        can-sell (game/city-has-sellable-demand? state player caravan-city)
        has-face-up (game/city-has-own-face-up-temple? pdata caravan-city)
        amity (:amity pdata 0)
        glory (:glory pdata 0)
        progress (game-progress state)
        balance (:track-balance weights 0.5)
        ;; Lower track influences weight direction
        amity-need (if (<= amity glory)
                     (+ 1.0 (* balance 0.5))
                     (- 1.0 (* balance 0.3)))
        glory-need (if (< glory amity)
                     (+ 1.0 (* balance 0.5))
                     (- 1.0 (* balance 0.3)))
        ;; ── Role-action coupling ──────────────────────────────────
        ;; Higher role levels boost the corresponding action.
        ;; Capped at its documented 1.0 ceiling so a mutation-drifted weight
        ;; (the clamp leak let it reach 1.63) can't blow the sell/temple/
        ;; deploy/influence bonuses past their designed +1.0-at-level-5 range.
        coupling (min 1.0 (:role-action-coupling weights 0.5))
        merchant-lv (get-in pdata [:roles :merchant] 1)
        priest-lv   (get-in pdata [:roles :priest] 1)
        raider-lv   (get-in pdata [:roles :raider] 1)
        leader-lv   (get-in pdata [:roles :leader] 1)
        ;; Bonus = coupling * (level - 1) * 0.25 → at coupling=1, level 5 adds +1.0
        sell-role-bonus     (* coupling (dec merchant-lv) 0.25)
        temple-role-bonus   (* coupling (dec priest-lv) 0.25)
        deploy-role-bonus   (* coupling (dec raider-lv) 0.25)
        influence-role-bonus (* coupling (dec leader-lv) 0.25)
        ;; ── Score balance targeting ───────────────────────────────
        ;; Adjusts amity/glory need based on explicit target ratio
        target (:score-balance-target weights 0.5)
        total-score (+ amity glory 1)  ;; +1 to avoid div by 0
        current-ratio (/ (double amity) total-score)
        ;; If target=0.5 (balanced), boost whichever is lower
        ;; If target=0.7 (amity-heavy), boost amity actions
        amity-target-adj (if (< current-ratio target) 0.3 -0.1)
        glory-target-adj (if (> current-ratio target) 0.3 -0.1)
        ;; ── Feat planning boost ──────────────────────────────────
        feat-boost (feat-action-boost weights state player pdata)
        fb (fn [action-type] (get feat-boost action-type 0))
        ;; ── Glory path influence ────────────────────────────────
        ;; Low glory-path = prefer raider-based glory (deploy+influence+travel)
        ;; High glory-path = prefer role-5 endgame bonuses (role increases)
        glory-path (:glory-path weights 0.5)
        raider-glory-boost (max 0 (* (- 1.0 glory-path) 1.5))  ;; 0-1.5 bonus for raider strategies
        ;; ── Glory floor: emergency boost when glory is dangerously low ──
        ;; Moderate boosts — avoid overwhelming normal priorities
        glory-emergency (cond
                          (and (> progress 0.5) (zero? glory) (>= amity 3)) 3.0
                          (and (> progress 0.3) (zero? glory)) 2.0
                          (and (< glory 2) (>= amity 4)) 1.5
                          :else 0)
        ;; ── Amity floor: symmetric ──
        amity-emergency (cond
                          (and (> progress 0.5) (zero? amity) (>= glory 3)) 3.0
                          (and (> progress 0.3) (zero? amity)) 2.0
                          (and (< amity 2) (>= glory 4)) 1.5
                          :else 0)
        ;; ── Feat rush: boost feat-related actions ──────────────
        feat-rush (:feat-rush weights 0.3)
        feat-rush-bonus (* feat-rush 0.5)
        ;; ── Task 2: Travel logic improvements ───────────────────
        ;; Reachable sale: check cities within 1-2 hops for sellable demands
        graph (:city-graph state)
        neighbors-1 (get graph caravan-city #{})
        neighbors-2 (set (mapcat #(get graph % #{}) neighbors-1))
        ;; Only boost for 1-hop reachable sale (not 2-hop, which is almost always true)
        reachable-sale-1hop? (some (fn [c]
                                     (and (not= c caravan-city)
                                          (game/city-has-sellable-demand? state player c)))
                                   neighbors-1)
        reachable-sale? (or reachable-sale-1hop?
                            (some (fn [c]
                                    (and (not= c caravan-city)
                                         (game/city-has-sellable-demand? state player c)))
                                  neighbors-2))
        travel-reachable-sale-boost (cond reachable-sale-1hop? 3
                                          reachable-sale? 1
                                          :else 0)
        ;; Point-side raider on adjacent route: traveling through scores 4 glory
        adjacent-point-raider?
        (some (fn [dest]
                (some #{:point} (game/raiders-on (:raiders pdata)
                                                 (game/route-key caravan-city dest))))
              neighbors-1)
        travel-point-raider-boost (if adjacent-point-raider? 6 0)
        ;; Unflipped temples: face-up temples that need travel to flip
        face-up-temple-count (count (filter #{:face-up} (game/all-temple-states pdata)))
        travel-unflipped-boost (if (>= face-up-temple-count 2) 4 0)
        ;; ── Task 4: Amity-glory gap closing ─────────────────────
        gap (Math/abs (- amity glory))
        gap-multiplier (cond (>= gap 6) 2.0 (>= gap 4) 1.0 :else 0.0)
        ;; glory-generating actions: deploy, influence, travel
        ;; amity-generating actions: sell, temple, travel
        gap-deploy-boost    (if (> amity glory) (* 3 gap-multiplier) 0)
        gap-influence-boost (if (> amity glory) (* 3 gap-multiplier) 0)
        gap-sell-boost      (if (> glory amity) (* 3 gap-multiplier) 0)
        gap-temple-boost    (if (> glory amity) (* 3 gap-multiplier) 0)
        gap-travel-boost    (if (pos? gap-multiplier) (* 2 gap-multiplier) 0)
        ;; ── Task 3: Round-level action budget ───────────────────
        sells-so-far (get pdata :sells-this-round 0)
        deploys-so-far (get pdata :deploys-this-round 0)
        min-sells (:min-sells-per-round weights 1)
        min-deploys (:min-deploys-per-round weights 0)
        turn-in-round (:turn-in-round state 1)
        sell-budget-boost (if (and (> turn-in-round 2)
                                   (< sells-so-far min-sells))
                            (cond can-sell 5    ;; sellable right here
                                  reachable-sale-1hop? 3  ;; 1 hop away
                                  :else 0)
                            0)
        deploy-budget-boost (if (and (> turn-in-round 2)
                                     (< deploys-so-far min-deploys))
                              3 0)
        ;; ── Standing awareness: race the reputation leader ──────────
        ;; reputation = min(amity,glory). When BEHIND the field's leader,
        ;; add urgency to whichever track is currently binding (the one
        ;; that raises our score). NEUTRAL at default — exactly 0 when
        ;; :standing-awareness is absent/0.0, so existing bots are unchanged.
        standing (:standing-awareness weights 0.0)
        my-rep (min amity glory)
        opp-rep (apply max 0
                       (for [[pk pd] (:players state)
                             :when (not= pk player)]
                         (min (:amity pd 0) (:glory pd 0))))
        behind (max 0 (- opp-rep my-rep))
        standing-amity (if (<= amity glory) (* standing behind 0.4) 0)
        standing-glory (if (< glory amity) (* standing behind 0.4) 0)
        ;; ── Supply conservation: husband finite raider/temple stock ──
        ;; raiders-supply/temples-supply are finite; a high weight makes the
        ;; bot hold its last unit in reserve. NEUTRAL at default (0.0 → 0).
        supply-cons (:supply-conservation weights 0.0)
        raiders-left (:raiders-supply pdata 0)
        temples-left (:temples-supply pdata 0)
        deploy-supply-pen (if (<= raiders-left 1) (* supply-cons -2.0) 0)
        temple-supply-pen (if (<= temples-left 1) (* supply-cons -1.5) 0)
        ;; ── Temple engine: build a wide base for compounding flips ──
        ;; A flip scores amity = face-down count, so each placed temple grows
        ;; the value of every later flip. Reward growing the base (the more
        ;; you hold, the more the next placement is worth). NEUTRAL at 0.0.
        temple-engine (:temple-engine weights 0.0)
        own-temple-count (count (game/all-temple-states pdata))
        temple-engine-place (* temple-engine (inc own-temple-count) 1.0)]
    {:take     (+ (* (:take-weight weights 1.0) 1.0) (fb :take))
     :sell     (+ (* (+ (:sell-weight weights 1.0) sell-role-bonus)
                    (+ amity-need amity-target-adj)
                    (if can-sell 1.5 0.5)
                    ;; Sell threshold: boost selling when merchant level >= threshold
                    (if (>= merchant-lv (:sell-threshold weights 2)) 1.5 0.7))
                  amity-emergency
                  gap-sell-boost
                  sell-budget-boost
                  standing-amity
                  (fb :sell)
                  (* feat-rush-bonus (fb :sell)))
     :temple   (+ (* (+ (:temple-weight weights 1.0) temple-role-bonus)
                    (+ amity-need amity-target-adj))
                  amity-emergency
                  gap-temple-boost
                  standing-amity
                  temple-supply-pen
                  temple-engine-place
                  (fb :temple)
                  (* feat-rush-bonus (fb :temple)))
     :deploy   (+ (* (+ (:deploy-weight weights 1.0) deploy-role-bonus)
                    (+ glory-need glory-target-adj))
                  raider-glory-boost
                  glory-emergency
                  gap-deploy-boost
                  deploy-budget-boost
                  standing-glory
                  deploy-supply-pen
                  ;; Minimum deploy floor: raiders needed for glory generation
                  (let [rc (game/count-raiders-deployed pdata)]
                    (cond
                      (and (zero? rc) (zero? glory) (> progress 0.2)) 3.0
                      (zero? rc) 2.0
                      (and (< rc 2) (zero? glory)) 1.5
                      :else 0))
                  (fb :deploy)
                  (* feat-rush-bonus (fb :deploy)))
     :influence (+ (* (+ (:influence-weight weights 1.0) influence-role-bonus)
                     (+ glory-need glory-target-adj))
                   raider-glory-boost
                   glory-emergency
                   gap-influence-boost
                   standing-glory
                   ;; Leader level boost for influence: higher leader = more effective influence
                   (* (dec leader-lv) 0.15)
                   ;; Boost influence when player has raiding-side raiders that need flipping
                   (let [raiding-count (game/count-raiders-with-status (:raiders pdata) :raiding)]
                     (if (pos? raiding-count)
                       (+ (* raiding-count 1.0)
                          ;; Extra urgency when glory is 0 and raiders need flipping
                          (if (zero? glory) 3.0 0))
                       0))
                   (fb :influence)
                   (* feat-rush-bonus (fb :influence)))
     :travel   (+ (* (:travel-weight weights 0.5)
                    (cond
                      has-face-up (* (:travel-for-temple weights 2.0) 1.0)
                      can-sell    0.3 ;; already here, sell instead
                      :else       0.6))
                  ;; Glory emergency also boosts travel (travel through point raiders = 4 glory)
                  (* glory-emergency 0.5)
                  ;; Boost travel when player has point-side raiders to score
                  (let [point-count (game/count-raiders-with-status (:raiders pdata) :point)]
                    (if (pos? point-count)
                      (+ (* point-count 2.0)
                         ;; Extra urgency when glory is 0 and we have scorable raiders
                         (if (zero? glory) 4.0 0))
                      0))
                  ;; Travel improvements: reachable sale, point raiders, unflipped temples
                  travel-reachable-sale-boost
                  travel-point-raider-boost
                  travel-unflipped-boost
                  gap-travel-boost
                  (fb :travel))}))

(defn- needed-resources
  "Return set of resources needed for the next role threshold costs."
  [pdata]
  (set
   (for [role game/roles
         :let [current (get-in pdata [:roles role] 1)
               next-level (inc current)
               cost (get-in game/role-threshold-costs [role next-level])]
         :when (and cost (< current game/max-role-level))]
     cost)))

(defn- resource-planning-bonus
  "Bonus for a space that gives resources needed for role advancement."
  [weights pdata space-resources]
  (if (nil? space-resources)
    0
    (let [needed (needed-resources pdata)
          planning (:resource-planning weights 0.5)]
      (* planning 2.0
         (count (filter needed space-resources))))))

;; =============================================================================
;; Opponent-awareness helpers
;; =============================================================================

(defn- opponent-max-role
  "Return the max level of `role` among all opponents."
  [state player role]
  (apply max 0
         (for [[pk pd] (:players state)
               :when (not= pk player)]
           (get-in pd [:roles role] 1))))

(defn- opponent-near-city?
  "True if any opponent's caravan is within `dist` hops of city."
  [state player city dist]
  (let [graph (:city-graph state)]
    (some (fn [[pk pd]]
            (when (not= pk player)
              (loop [frontier #{(:caravan pd)} visited #{} d 0]
                (cond
                  (> d dist) false
                  (contains? frontier city) true
                  :else (let [next-f (set (mapcat #(get graph % #{}) frontier))]
                          (recur (clojure.set/difference next-f visited)
                                 (into visited frontier)
                                 (inc d)))))))
          (:players state))))

(defn- opponent-has-resource-for-demand?
  "True if any opponent has a resource matching a demand in `city` and is within 2 hops."
  [state player city]
  (let [demands (set (get-in state [:city-demands city] []))]
    (when (seq demands)
      (some (fn [[pk pd]]
              (when (not= pk player)
                (and (some #(pos? (get-in pd [:resources %] 0)) demands)
                     (opponent-near-city? state player city 2))))
            (:players state)))))

(defn- opponent-temples-in-city
  "Count of opponents who have a temple in the given city."
  [state player city]
  (count (for [[pk pd] (:players state)
               :when (and (not= pk player)
                          (game/has-temple? pd city))]
           pk)))

(defn- opponent-has-unflipped-temple?
  "True if any opponent has a face-up temple in this city."
  [state player city]
  (some (fn [[pk pd]]
          (and (not= pk player)
               (game/city-has-own-face-up-temple? pd city)))
        (:players state)))

(defn- chain-score
  "Evaluate dice chaining potential for a die choice.
   Considers: action synergy combos AND same-space revisits (dice summing to 7)."
  [weights state player pdata dest remaining-dice die-val]
  (if (or (empty? remaining-dice) (< (:chain-weight weights 0.5) 0.1))
    0
    (let [types (game/space-action-types dest)
          astro-positions (:astronomers pdata)
          ;; ── Same-space revisit bonus ─────────────────────────────
          ;; If any remaining die = 7 - die-val, we can revisit `dest`
          ;; This doubles the action value of this space
          can-revisit? (some #(= 7 (+ die-val %)) remaining-dice)
          ;; Role-action alignment for revisit value
          ml (get-in pdata [:roles :merchant] 1)
          pl (get-in pdata [:roles :priest] 1)
          rl (get-in pdata [:roles :raider] 1)
          ll (get-in pdata [:roles :leader] 1)
          ;; Higher role = more value from that action type
          revisit-value (cond-> 0
                          (and can-revisit? (contains? types :sell))     (+ (* 2 ml))
                          (and can-revisit? (contains? types :temple))   (+ (* 2 pl))
                          (and can-revisit? (contains? types :deploy))   (+ (* 2 rl))
                          (and can-revisit? (contains? types :influence)) (+ (* 2 ll))
                          ;; Base revisit bonus — more actions per space is always good
                          can-revisit? (+ 5))
          ;; ── Action-combo chaining ────────────────────────────────
          combo-scores
          (for [d remaining-dice
                pos astro-positions
                :let [other-dest (game/move-astronomer-clockwise pos d)
                      other-types (game/space-action-types other-dest)
                      combo
                      (cond-> 0
                        (and (contains? types :travel) (contains? other-types :sell)) (+ 4)
                        (and (contains? types :sell) (contains? other-types :travel)) (+ 3)
                        (and (contains? types :deploy) (contains? other-types :influence)) (+ 5)
                        (and (contains? types :influence) (contains? other-types :deploy)) (+ 4)
                        (and (contains? types :temple) (contains? other-types :travel)) (+ 4)
                        ;; Deploy → Travel (through point raider) = glory
                        (and (contains? types :deploy) (contains? other-types :travel)) (+ 3)
                        ;; Temple → Travel (flip for amity) = amity
                        (and (contains? types :travel) (contains? other-types :temple)) (+ 3))]]
            combo)
          combo-max (if (seq combo-scores) (apply max combo-scores) 0)]
      (* (:chain-weight weights 0.5) (+ combo-max revisit-value)))))

;; =============================================================================
;; Feat forecasting — potential-based reward shaping (the horizon fix)
;;
;; The rest of `decide` is a GREEDY per-turn scorer: it ranks the *type* of an
;; action and its immediate, this-turn consequence. That is structurally blind
;; to any feat whose payoff is BACKLOADED several turns out — the bot sees no
;; gradient, so it never sets up the multi-turn play. This is a general horizon
;; gap across the whole feat layer, not a quirk of any one feat.
;;
;; The fix (Arimaa goal-distance / AlphaGo "value the resulting position, not
;; the move label" / classical reward-shaping): define a POTENTIAL function Φ
;; over the player's planned feat chain and score each presented choice by the
;; realized change in potential, ΔΦ = Φ(next-state) − Φ(state). Because ΔΦ is
;; read off the SAME `next-state` a human would see, it is a true 1-ply forecast
;; of how much a choice advances the plan — the slope a greedy scorer can't feel.
;; Potential-based shaping (Ng et al.) adds gradient without changing the optimal
;; policy. A no-op / precondition-dead resolution yields ΔΦ = 0, so this also
;; subsumes the "dead action looks live" representation gap for free.
;; =============================================================================

(defn- feat-value
  "Grounded payoff estimate for claiming `contest` from `state`. Mirrors the
   planner's `game/chain-score` ingredients (claim-order wild points + an ease
   factor from feat-difficulty), discounted by single-turn-burst claimability so
   phantom event-feats (J1/G2-style) don't dominate the forecast."
  [state contest]
  (let [cid (:id contest)
        claim-count (count (get-in state [:contest-claims cid] []))
        wild (get game/bonus-contest-values claim-count 1)
        difficulty (get game/feat-difficulty cid 5)
        ease (max 0.3 (- 1.5 (/ difficulty 6.0)))
        claimability (get game/event-feat-claimability cid 1.0)]
    (* claimability (+ wild ease))))

(defn- feat-potential
  "Forecasting potential Φ for `player` in `state`: the expected realized value of
   the player's targeted feat chain, as Σ value(f)·progress(f)·position-weight
   over still-unclaimed targets. Evaluated on a presented next-state and differenced
   against the current state, this is the gradient toward backloaded feats that the
   greedy per-turn scorer is blind to. Earlier chain positions weigh more (they're
   attempted first) — matching the planner's own ordering."
  [state player pdata]
  (let [claims (:contest-claims state {})
        already? (fn [cid] (some #{player} (get claims cid [])))
        chain (or (seq (:feat-chain pdata)) (:target-feats pdata []))
        active (remove #(already? (:id %)) chain)]
    (reduce (fn [acc [idx contest]]
              (let [[prog _] (game/feat-progress state player contest)
                    pos-w (case idx 0 1.0 1 0.7 2 0.4 0.2)]
                (+ acc (* (feat-value state contest) prog pos-w))))
            0.0
            (map-indexed vector active))))

;; =============================================================================
;; The decision-making algorithm
;; =============================================================================

(defn- choice-action
  "Feature extraction (stage b): the action a `:choose-action` choice performs,
   read from the PRESENTED choice's next-state — NOT from engine internals.
   `choice.cljc` stores the chosen action verbatim at [:player-turn :action]
   when it builds each numeric action choice, so this is the same descriptor
   the engine resolved, with no leak of `game/action-spaces`/`[:player-turn :space]`."
  [next-state]
  (get-in next-state [:player-turn :action]))

(defn decide
  "Pick a choice for a bot with the given personality weights.

   Pure decision-making module — see the namespace docstring for the
   read-presented-choices → per-choice-features → context-modulated-scoring →
   argmax contract. Returns [choice-key next-state] or nil.

   `weights` is the GA-evolved personality weight vector (an input, not the
   scorer). Equivalent across the bot (simulate/evolve/socket) and any
   human-assist caller because it scores the same `find-state-raw` choice set."
  [state weights]
  ;; (a) read the presented choices — the SAME map a human is shown.
  (let [[phase choices] (choice/find-state-raw state)]
    (when (and (not= phase :game-over) (seq choices))
      ;; (c) build the per-turn modulation context from public game state.
      (let [player (game/current-player state)
            pdata  (game/player-data state player)
            ;; ── Feat forecasting (horizon fix) ──────────────────────────
            ;; ΔΦ between a presented next-state and now is a 1-ply forecast of
            ;; how much a choice advances the planned feat chain. `fl` returns
            ;; that shaped reward for a candidate's next-state; at the neutral
            ;; default weight 0.0 it is exactly 0.0 (existing bots unchanged).
            lookahead (:feat-lookahead weights 0.0)
            phi0 (if (pos? lookahead) (feat-potential state player pdata) 0.0)
            fl (fn [next-s]
                 (if (and (pos? lookahead) (map? next-s))
                   (* lookahead
                      (- (feat-potential next-s player (game/player-data next-s player))
                         phi0))
                   0.0))
            progress (game-progress state)
            amity (:amity pdata 0)
            glory (:glory pdata 0)
            lower-track (if (<= amity glory) :amity :glory)

            ;; Temporal context
            round (:round state 1)
            turn-in-round (:turn-in-round state 1)
            tpr (game/turns-per-round state)
            first-turn? (and (= 1 round) (= 1 turn-in-round))
            last-turn-of-round? (>= turn-in-round tpr)
            round-end-bias (:round-end-scoring weights 0.6)

            ;; (b)+(d) extract features per presented choice, score, argmax.
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
                                            space-res (game/space-gives-resources dest)
                                            res-pen (if (and space-res (game/has-resource-excess? pdata space-res))
                                                      (- (:excess-penalty weights 3.0))
                                                      0)
                                            ch-score (chain-score weights state player pdata dest remaining die-val)
                                            early-bias (:early-role-bias weights 0.7)
                                            res-plan (resource-planning-bonus weights pdata space-res)]]
                                  (+ res-pen ch-score res-plan
                                     (cond
                                       first-turn?
                                       (if will-be-alone
                                         (* 10 (+ early-bias (* (- 1 (:first-turn-aggression weights 0.3)) 0.5)))
                                         (+ 2 on-space))
                                       (< progress 0.4)
                                       (if will-be-alone (* 10 early-bias) (+ 2 on-space))
                                       :else
                                       (+ (* on-space 5) (if will-be-alone 1 0)))
                                     ;; Last turn of round: favor scoring spaces
                                     (if last-turn-of-round?
                                       (* round-end-bias (if (> on-space 1) 3 -1))
                                       0)))
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
                                types (game/space-action-types dest)
                                ;; Sum action priorities for actions on this space
                                action-val (reduce + (map #(get action-pri % 0) types))
                                space-res (game/space-gives-resources dest)
                                res-pen (if (and space-res (game/has-resource-excess? pdata space-res))
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
              ;; Landing: prefer begin (actions) over role increase for scoring
              ;; Always prefer role increase early game to build up levels
              :resolve-landing
              (cond
                ;; Early game: prefer role increase to build levels
                (and (contains? choices :increase-role) (< progress 0.4))
                :increase-role
                ;; Mid-late game: prefer actions for scoring
                (contains? choices :begin) :begin
                (contains? choices :increase-role) :increase-role
                :else (first (keys choices)))

              ;; ── Role increase ──────────────────────────────────────────
              :choose-role-increase
              (if (> (count choices) 1)
                (let [role-choices (dissoc choices :skip)
                      role-levels (:roles pdata)
                      priority-order (:role-priority weights [:merchant :priest :raider :leader])
                      endgame-push (:endgame-role-push weights 0.5)
                      competitive (:competitive-roles weights 0.5)
                      late-game? (> progress 0.6)
                      ;; Glory deficit: boost raider/leader roles when glory is critically low
                      glory-deficit? (and (< glory 2) (>= amity 3))
                      ;; Feat-aware role targeting
                      target-ids (set (map :id (:target-feats pdata [])))
                      wants-h1? (contains? target-ids :H1)
                      wants-h2? (contains? target-ids :H2)
                      roles-at-3 (count (filter #(>= (val %) 3) role-levels))
                      ;; Temple engine: priest level raises the temple cap
                      ;; (priest-max-temples {3 5, 4 8}), so a committed temple
                      ;; player must level priest to place a wide base. Neutral 0.
                      temple-engine (:temple-engine weights 0.0)
                      scored (for [role (keys role-choices)
                                   :when (keyword? role)
                                   :let [pri-idx (.indexOf priority-order role)
                                         pri (if (neg? pri-idx) 99 pri-idx)
                                         level (get role-levels role 1)
                                         ;; lower score = picked first; boost priest
                                         ;; up to level 4 (the 8-temple cap)
                                         te-role-adj (if (and (= role :priest) (< level 4))
                                                       (* (- temple-engine) 7.0)
                                                       0)
                                         ;; Finish a near-max role (L4→5 = +10 to a
                                         ;; track). Fire regardless of game phase
                                         ;; (the old late-game?>0.6 gate meant the
                                         ;; bot rarely harvested role-5s — they hit
                                         ;; in ~7% of games), stronger past 40%.
                                         near-max-bonus (cond
                                                          (and (>= level 4) (> progress 0.4))
                                                          (* endgame-push -12)
                                                          (>= level 4)
                                                          (* endgame-push -8)
                                                          :else 0)
                                         ;; Glory deficit: strongly prefer raider/leader
                                         glory-adj (if glory-deficit?
                                                     (case role
                                                       :raider -8
                                                       :leader -6
                                                       0)
                                                     0)
                                         ;; Feat targeting: H1 needs 2 roles at 3+
                                         feat-role-adj
                                         (cond
                                           ;; H1: boost the role closest to 3 (if we need more at 3)
                                           (and wants-h1? (< roles-at-3 2) (< level 3))
                                           (* -8 (/ level 3.0))  ;; closer to 3 = bigger boost
                                           ;; H2: boost the highest role toward 5
                                           (and wants-h2? (>= level 3))
                                           (* -6 (/ level 5.0))
                                           :else 0)
                                         ;; Competitive: react to opponent role levels
                                         opp-max (opponent-max-role state player role)
                                         compete-adj (cond
                                                       (> opp-max level)
                                                       (if (> competitive 0.5)
                                                         (* (- competitive 0.5) -6)
                                                         (* (- 0.5 competitive) 3))
                                                       (> level opp-max)
                                                       (if (> competitive 0.5) -1 0)
                                                       :else 0)]]
                               ;; Forecast: a role bump that advances the feat plan
                               ;; lowers the sort key (picked first). Sign-flipped
                               ;; because lower = preferred here.
                               [(+ (* pri 3) level near-max-bonus glory-adj
                                   feat-role-adj compete-adj te-role-adj
                                   (- (fl (get role-choices role)))) role])]
                  (if (seq scored)
                    (second (first (sort scored)))
                    ;; Always pick a role — never skip, there's no downside
                    (first (keys role-choices))))
                ;; Single choice: take whatever is offered (role or skip if truly nothing)
                (let [non-skip (first (filter #(not= :skip %) (keys choices)))]
                  (or non-skip (first (keys choices)))))

              ;; ── Action selection ────────────────────────────────────────
              :choose-action
              (cond
                ;; Only auto-done when :done is the ONLY choice (no real actions left)
                (= #{:done} (set (keys choices)))
                :done
                ;; Free travel from bonus board: take it if travel priority is decent
                (and (contains? choices :free-travel)
                     (= (count choices) 2) ;; free-travel + done
                     )
                :free-travel

                :else
                (let [action-pri (weighted-action-priority weights state player pdata)
                      action-choices (dissoc choices :done :free-travel)
                      ;; Hard override: if glory=0, force glory-generating actions
                      ;; Scale with game progress — stronger as game advances
                      glory-override?
                      (or (and (zero? glory) (>= amity 3) (> progress 0.25))
                          (and (zero? glory) (> progress 0.4)))
                      ;; Symmetric: if amity=0, force amity-generating actions
                      amity-override?
                      (or (and (zero? amity) (>= glory 3) (> progress 0.25))
                          (and (zero? amity) (> progress 0.4)))
                      point-raider-count (game/count-raiders-with-status (:raiders pdata) :point)
                      raiding-count (game/count-raiders-with-status (:raiders pdata) :raiding)
                      final-round? (>= (:round state 1) game/rounds-per-game)
                      final-turn? (and final-round?
                                       (>= (:turn-in-round state 1)
                                            (dec (game/turns-per-round state))))
                      ;; Travel pacing: boost/penalize travel based on round quota
                      travels-so-far (get-in pdata [:travels-this-round] 0)
                      min-travels (:min-travels-per-round weights 1)
                      max-travels (:max-travels-per-round weights 3)
                      scored
                      (for [[idx next-s] action-choices
                            :when (number? idx)
                            ;; (b) feature: the action this presented choice performs,
                            ;; read from the choice's next-state — no engine internals.
                            :let [action (choice-action next-s)
                                  atype (:type action)
                                  base-pri (get action-pri atype 1.0)
                                  res-pen (if (and (= atype :take)
                                                   (:resources action)
                                                   (game/has-resource-excess? pdata (:resources action)))
                                            (- (:excess-penalty weights 3.0))
                                            0)
                                  ;; Travel pacing adjustment
                                  travel-adj
                                  (if (= atype :travel)
                                    (cond
                                      ;; Below minimum: strong boost to travel
                                      (< travels-so-far min-travels) 5
                                      ;; At or above maximum: penalize travel
                                      (>= travels-so-far max-travels) -6
                                      :else 0)
                                    0)
                                  ;; Last-turn adjustments: prefer scoring, avoid setup
                                  endgame-adj
                                  (if final-turn?
                                    (case atype
                                      :sell     5    ;; always good to sell
                                      :travel   4    ;; travel to flip temples / score raiders
                                      :take    -3    ;; taking goods on last turn is wasteful
                                      :temple  -5    ;; can't flip a temple you just placed
                                      :deploy  -5    ;; can't score a raider you just deployed
                                      :influence -2  ;; only good if it flips own raiders
                                      0)
                                    (if final-round?
                                      (case atype
                                        :sell    3
                                        :travel  2
                                        :take   -1
                                        :deploy -2
                                        0)
                                      0))
                                  ;; Glory override: hard boost for glory actions
                                  glory-adj
                                  (if glory-override?
                                    (case atype
                                      :deploy   (if (pos? point-raider-count) 0  ;; have point raiders, don't deploy more
                                                  10)  ;; need to deploy first
                                      :influence (if (pos? raiding-count) 12 0)  ;; flip raiders to point
                                      :travel   (if (pos? point-raider-count) 15 ;; travel through point raiders
                                                  2) ;; travel might flip opponents' raiders
                                      :sell     -3  ;; avoid amity when glory=0
                                      :temple   -3  ;; avoid amity when glory=0
                                      0)
                                    0)
                                  ;; Amity override: hard boost for amity actions when amity=0
                                  amity-adj
                                  (if amity-override?
                                    (case atype
                                      :sell    12  ;; selling generates amity
                                      :temple   8  ;; temples generate amity when flipped
                                      :travel   5  ;; travel to sell cities
                                      :deploy  -3  ;; avoid glory when amity=0
                                      :influence -3
                                      0)
                                    0)]]
                        [(+ base-pri res-pen travel-adj endgame-adj glory-adj amity-adj
                            (fl next-s)) idx])]
                  (let [best-idx (if (seq scored)
                                   (second (last (sort scored)))
                                   nil)
                        ;; Consider free-travel as an option
                        free-travel-score (when (contains? choices :free-travel)
                                           (get action-pri :travel 1.0))]
                    (cond
                      ;; If free-travel is available and scores well, take it
                      (and free-travel-score best-idx
                           (> free-travel-score (first (last (sort scored)))))
                      :free-travel
                      ;; Normal best action
                      best-idx best-idx
                      ;; Fallback
                      :else (first (keys choices))))))

              ;; ── Sell ────────────────────────────────────────────────────
              :resolve-sell
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (let [resources (:resources pdata)
                        sell-urg (:sell-urgency weights 0.5)
                        caravan-city (:caravan pdata)
                        awareness (:feat-awareness weights 0.3)]
                    ;; Prefer selling goods we have most of, penalize feat-needed resources
                    (apply max-key
                           (fn [demand]
                             (+ (get resources demand 0)
                                (if (and (> sell-urg 0.3)
                                         (opponent-has-resource-for-demand? state player caravan-city))
                                  (* sell-urg 3) 0)
                                ;; Protect resources needed for target feats
                                (if (and (> awareness 0.2)
                                         (feat-needs-resource? pdata demand))
                                  (* awareness -5) 0)
                                ;; Forecast: prefer the sale that most advances the plan
                                (fl (get non-skip demand))))
                           (keys non-skip)))
                  :skip))

              ;; ── Temple ──────────────────────────────────────────────────
              :resolve-temple
              (let [non-skip (dissoc choices :skip)
                    final-round? (>= (:round state 1) game/rounds-per-game)
                    final-turn? (and final-round?
                                     (>= (:turn-in-round state 1)
                                          (dec (game/turns-per-round state))))]
                ;; Skip temple placement on final turn — no time to flip for points
                (if (or (empty? non-skip) final-turn?)
                  :skip
                  (let [river-pref (:temple-river-pref weights 0.5)
                        eridu-pref (:eridu-focus weights 0.3)
                        temple-comp (:temple-competition weights 0.5)
                        ;; ── Cluster-aware placement (temple engine) ──────────
                        ;; A tight cluster of temples lets one double-move flip
                        ;; several at once, so reward placing ADJACENT to your
                        ;; own temples. Neutral at temple-engine 0.0.
                        temple-engine (:temple-engine weights 0.0)
                        graph (:city-graph state)
                        own-temple-cities (set (keys (:temples pdata)))
                        scored (for [city (keys non-skip)
                                    :let [demands (count (get-in state [:city-demands city] []))
                                          has-mag (game/magistrate-in-city? state city)
                                          mag-bonus (if has-mag 8 0)
                                          on-route (if (= city (:caravan pdata)) 2 0)
                                          late-penalty (if final-round? -3 0)
                                          ;; River city bonus
                                          river-bonus (if (contains? game/river-cities city)
                                                        (* river-pref 3) 0)
                                          ;; Eridu focus
                                          eridu-bonus (if (= city :eridu) (* eridu-pref 5) 0)
                                          ;; Opponent temple grouping
                                          opp-temples (opponent-temples-in-city state player city)
                                          group-adj (if (pos? opp-temples)
                                                      (* (- temple-comp 0.5) 4) ;; positive=group, negative=avoid
                                                      0)
                                          ;; Cluster bonus: graph-neighbors that hold my temples
                                          cluster-adj (count (filter #(contains? (get graph city #{}) %)
                                                                     own-temple-cities))
                                          cluster-bonus (* temple-engine cluster-adj 3.0)]]
                                 [(+ (* demands (:temple-in-demand-city weights 1.5))
                                     mag-bonus on-route late-penalty
                                     river-bonus eridu-bonus group-adj
                                     cluster-bonus
                                     (fl (get non-skip city)))
                                  city])]
                    (if (seq scored)
                      (second (last (sort scored)))
                      :skip))))

              ;; ── Deploy ──────────────────────────────────────────────────
              :resolve-deploy
              (let [non-skip (dissoc choices :skip :done)
                    mag-pref (:raider-magistrate-pref weights 0.5)
                    final-round? (>= (:round state 1) game/rounds-per-game)
                    ;; Allow deploy on final round if glory is critically low
                    glory-critical? (and (zero? glory) (>= amity 3))]
                (if (and (seq non-skip)
                         (or (not final-round?) glory-critical?)) ;; Deploy on final round if glory=0
                  (let [scored (for [rk (keys non-skip)
                                    :let [[c1 c2] rk
                                          near-opp (count
                                                    (for [[pk pd] (:players state)
                                                          :when (not= pk player)
                                                          :when (or (= (:caravan pd) c1)
                                                                    (= (:caravan pd) c2))]
                                                      pk))
                                          ;; Magistrate nearby = magistrate can flip to point
                                          near-mag (if (or (game/magistrate-in-city? state c1)
                                                           (game/magistrate-in-city? state c2))
                                                     (* mag-pref 4) 0)
                                          ;; On player's likely travel path
                                          near-own (if (or (= c1 (:caravan pdata))
                                                           (= c2 (:caravan pdata))) 3 0)
                                          d1 (count (get-in state [:city-demands c1] []))
                                          d2 (count (get-in state [:city-demands c2] []))]]
                                 [(+ d1 d2
                                     (* near-opp (:deploy-near-opponents weights 1.5))
                                     near-mag near-own
                                     ;; Eridu focus
                                     (if (or (= c1 :eridu) (= c2 :eridu))
                                       (* (:eridu-focus weights 0.3) 3) 0)
                                     ;; Raider aggression: group on opponent routes
                                     (let [opp-raiders (count (for [[pk pd] (:players state)
                                                                    :when (not= pk player)
                                                                    :when (game/raider-on-route? (:raiders pd) rk)]
                                                               pk))]
                                       (* (:raider-aggression weights 0.5) opp-raiders 2))
                                     (fl (get non-skip rk)))
                                  rk])]
                    (if (seq scored)
                      (second (last (sort scored)))
                      (first (keys non-skip))))
                  (or (:done choices) (first (keys choices)))))

              ;; ── Travel ──────────────────────────────────────────────────
              :resolve-travel
              (let [non-skip (dissoc choices :skip)
                    caravan-city (:caravan pdata)
                    temple-states (game/all-temple-states pdata)
                    face-up-count (count (filter #{:face-up} temple-states))
                    ;; A flip scores amity = face-down count, so the marginal
                    ;; flip is worth (face-down + 1). temple-engine prices that
                    ;; compounding so the base is worth flipping out. Neutral at 0.
                    temple-engine (:temple-engine weights 0.0)
                    face-down-count (count (filter #{:face-down} temple-states))
                    flip-threshold (:temple-flip-threshold weights 2)
                    unflipped-urgency (if (>= face-up-count flip-threshold)
                                        (* (:travel-for-temple weights 2.0) 3.0)
                                        0)
                    ;; Last-turn logic: MUST score, don't waste travel
                    final-turn? (and (>= (:round state 1) game/rounds-per-game)
                                     (>= (:turn-in-round state 1)
                                          (dec (game/turns-per-round state))))]
                (if (seq non-skip)
                  (let [;; Pre-compute point-raider routes for multi-hop planning
                        point-raider-routes (set (for [[rk rs] (:raiders pdata)
                                                       :when (some #{:point} rs)] rk))
                        scored
                        (for [dest (keys non-skip)
                              :let [has-temple (game/city-has-own-face-up-temple? pdata dest)
                                    can-sell (game/city-has-sellable-demand? state player dest)
                                    has-mag (game/magistrate-in-city? state dest)
                                    rk (game/route-key caravan-city dest)
                                    own-point (some #{:point} (game/raiders-on (:raiders pdata) rk))
                                    ;; 1-hop lookahead: does dest have a neighbor with point raider?
                                    near-point-raider
                                    (when (and (not own-point) (seq point-raider-routes))
                                      (let [dest-neighbors (get-in state [:city-graph dest])]
                                        (some (fn [n]
                                                (contains? point-raider-routes
                                                           (game/route-key dest n)))
                                              dest-neighbors)))
                                    ;; Enemy raider that would get flipped to point
                                    enemy-raider-risk
                                    (some (fn [[pk pd]]
                                            (and (not= pk player)
                                                 (some #{:raiding} (game/raiders-on (:raiders pd) rk))))
                                          (:players state))
                                    ;; Prioritize: temple+magistrate, sellable+magistrate
                                    temple-mag-bonus (if (and has-temple has-mag) 8 0)
                                    sell-mag-bonus (if (and can-sell has-mag) 6 0)]]
                          [(+ (if has-temple (+ (* (:travel-for-temple weights 2.0) 5)
                                               unflipped-urgency
                                               (* temple-engine (inc face-down-count) 1.0)) 0)
                              (if can-sell (* (:travel-for-sell weights 1.8) 4) 0)
                              ;; OWN POINT RAIDER = instant 4 glory, very high priority
                              (if own-point 15 0)
                              ;; Near a point raider = next travel can score it
                              (if near-point-raider 8 0)
                              (if has-mag 3 0)
                              temple-mag-bonus
                              sell-mag-bonus
                              ;; Avoid flipping enemy raiders to point (helps them score)
                              (if enemy-raider-risk
                                (* (:avoid-enemy-flip weights 0.5) -4) 0)
                              ;; Base travel value — always better than nothing
                              1.0
                              (fl (get non-skip dest)))
                           dest])
                        best (last (sort scored))]
                    ;; Always travel somewhere — don't waste the action
                    (second best))
                  :skip))

              ;; ── Travel continue ─────────────────────────────────────────
              :travel-continue
              (let [resources (:resources pdata)
                    total-resources (reduce + (vals resources))
                    temple-states (game/all-temple-states pdata)
                    face-up-count (count (filter #{:face-up} temple-states))
                    ;; Compounding flip value (a flip scores amity = face-down
                    ;; count) + double-flip chaining: pay a good to keep moving
                    ;; when it flips a temple here AND lets the next hop flip
                    ;; another. Neutral at temple-engine 0.0.
                    temple-engine (:temple-engine weights 0.0)
                    face-down-count (count (filter #{:face-down} temple-states))
                    willingness (:resource-to-move weights 0.3)
                    ;; Check if nearby cities have good destinations
                    current-city (:caravan pdata)
                    neighbors (get-in state [:city-graph current-city])
                    best-nearby-score
                    (apply max 0
                           (for [dest neighbors
                                 :let [rk (game/route-key current-city dest)
                                       dest-neighbors (get-in state [:city-graph dest] #{})
                                       flip-here (game/city-has-own-face-up-temple? pdata dest)
                                       flip-next (some #(game/city-has-own-face-up-temple? pdata %)
                                                       dest-neighbors)]]
                             (+ (if flip-here 8 0)
                                ;; compounding: this flip is worth ~face-down+1
                                (if flip-here (* temple-engine (inc face-down-count) 1.0) 0)
                                ;; double-flip chain: another own temple one hop on
                                (if (and flip-here flip-next) (* temple-engine 10.0) 0)
                                (if (game/city-has-sellable-demand? state player dest) 6 0)
                                ;; Point raider = instant 4 glory, always worth paying to reach
                                (if (some #{:point} (game/raiders-on (:raiders pdata) rk)) 15 0)
                                (if (game/magistrate-in-city? state dest) 3 0)
                                ;; 2-hop lookahead: sellable city one more hop away
                                (if (some #(game/city-has-sellable-demand? state player %) dest-neighbors) 2 0)
                                ;; 2-hop lookahead: point raider one more hop away
                                (if (some (fn [n] (some #{:point} (game/raiders-on (:raiders pdata) (game/route-key dest n))))
                                          dest-neighbors) 5 0))))
                    should-continue? (and (pos? total-resources)
                                          (or (> best-nearby-score 5)
                                              (and (>= face-up-count 2)
                                                   (> willingness 0.2))
                                              (> (* willingness total-resources) 2)))]
                (if (and should-continue? (not (contains? choices :done)))
                  ;; Pick the cheapest resource to discard
                  (let [spendable (for [r game/resource-types
                                        :when (and (pos? (get resources r 0))
                                                   (contains? choices r))]
                                    r)]
                    (if (seq spendable)
                      ;; Spend the resource we have the most of
                      (apply max-key #(get resources % 0) spendable)
                      :done))
                  :done))

              ;; ── Influence ───────────────────────────────────────────────
              :resolve-influence
              (let [non-skip (dissoc choices :skip)
                    final-round? (>= (:round state 1) game/rounds-per-game)
                    mag-setup (:magistrate-setup weights 0.5)
                    mag-denial (:magistrate-denial weights 0.3)
                    ;; Temple engine: co-locating a magistrate with your own
                    ;; temple sets up the flip's magistrate glory AND the M1
                    ;; "magistrates at temples" contest. Neutral at 0.0.
                    temple-engine (:temple-engine weights 0.0)]
                ;; Last round: only influence if it flips own raiders (immediate glory)
                (if (seq non-skip)
                  (let [scored
                        (for [[k next-s] non-skip
                              :let [dest (when (vector? k) (second k))
                                    steps (when (vector? k) (nth k 2 1))
                                    near-own-point
                                    (when dest
                                      (some (fn [[rk rs]]
                                              (and (some #{:raiding} rs)
                                                   (or (= dest (first rk))
                                                       (= dest (second rk)))))
                                            (:raiders pdata)))
                                    ;; Own raiders that would flip to point on this path
                                    own-raiders-flipped
                                    (when dest
                                      (game/count-raiders-with-status (:raiders pdata) :raiding))
                                    has-temple (and dest (game/city-has-own-face-up-temple? pdata dest))
                                    has-demands (and dest (seq (get-in state [:city-demands dest] [])))
                                    can-sell-there (and dest
                                                       (game/city-has-sellable-demand? state player dest))
                                    ;; Setup bonus: move magistrate toward own temples/demands
                                    setup-bonus (* mag-setup
                                                  (+ (if has-temple 5 0)
                                                     (if can-sell-there 4 0)
                                                     (if has-demands 2 0)))
                                    ;; Denial: penalize destinations that help opponents
                                    opp-can-sell (when dest (opponent-has-resource-for-demand? state player dest))
                                    opp-temple (when dest (opponent-has-unflipped-temple? state player dest))
                                    denial-adj (* mag-denial
                                                 (+ (if opp-can-sell -4 0)   ;; avoid helping opp sell
                                                    (if opp-temple -3 0)))]] ;; avoid helping opp flip
                          [(+ (if near-own-point (* (:influence-flip-raider weights 2.5) 4) 0)
                              (if has-temple 7 0)
                              (if can-sell-there 6 0)
                              setup-bonus
                              ;; temple-engine: drive the magistrate onto my temple
                              (if has-temple (* temple-engine 6.0) 0)
                              denial-adj
                              ;; On final round, strongly penalize influence that doesn't score
                              (if (and final-round? (not near-own-point) (not has-temple))
                                -15 0)
                              (fl next-s))
                           k])]
                    (if (seq scored)
                      (let [best (last (sort scored))]
                        (if (and final-round? (neg? (first best)))
                          :skip ;; Skip influence on final round if nothing scores
                          (second best)))
                      (first (keys non-skip))))
                  :skip))

              :resolve-take :done
              (first (keys choices)))]

        ;; (d) return the chosen [choice-key next-state].
        (when-let [next-s (get choices pick)]
          [pick next-s])))))
