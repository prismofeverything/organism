(ns eridu.personality
  "AI personality GENETICS for Eridu. Each personality is a weight vector — the
   GA-evolved representation. The weights are the genome; they are NOT the
   scorer. They are consumed by the unified decision-making algorithm in
   `eridu.decision`, which is how these genetics are expressed at every
   decision point.

   This namespace owns: the weight schema (`default-weights`), the hand-designed
   archetypes, and the GA operators (random/mutate/crossover). The decision
   logic lives in `eridu.decision`; `personality-step` here is a thin delegate
   kept for the existing bot/simulate/replay/socket call sites."
  (:require
   [eridu.decision :as decision]))

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
   ;; ── New coherence dimensions ──────────────────────────────────
   ;; Role-action coupling: role investment boosts corresponding actions
   ;; At 1.0, merchant-5 gives +1.0 sell bonus; at 0, roles don't affect actions
   :role-action-coupling  0.5
   ;; Resource planning: prioritize taking resources needed for next role cost
   :resource-planning     0.5
   ;; Score balance target: 0=all amity, 1=all glory, 0.5=balanced
   :score-balance-target  0.5
   ;; Feat awareness: steer toward fulfilling feats in play
   :feat-awareness        0.3
   ;; Tempo: 0=claim feats immediately, 1=hold for better timing
   :tempo                 0.3
   ;; Endgame role push: how much to prioritize roles in round 3
   :endgame-role-push     0.5
   ;; ── Travel and positioning dimensions ─────────────────────────
   ;; Min travel actions per round: below this, boost travel priority
   :min-travels-per-round 1
   ;; Max travel actions per round: above this, penalize travel priority
   :max-travels-per-round 3
   ;; Resource-to-move: willingness to pay a resource to travel further
   :resource-to-move      0.5
   ;; Avoid flipping enemy raiders to their point side when traveling
   :avoid-enemy-flip      0.5
   ;; Priority for picking up point-side raiders when near deploy limit
   :raider-pickup-priority 0.5
   ;; ── Role-specific thresholds ─────────────────────────────────
   ;; Merchant: level at which selling becomes high priority (1-5)
   :sell-threshold         2
   ;; Priest: face-up temple count before aggressively traveling to flip (1-4)
   :temple-flip-threshold  2
   ;; Raider: preference for placing on magistrate-adjacent routes vs opponent-adjacent
   ;; 0 = near opponents, 1 = near magistrate paths (for magistrate-flip scoring)
   :raider-magistrate-pref 0.5
   ;; Leader: how much to move magistrate toward own temples/demands for setup
   :magistrate-setup       0.5
   ;; ── Bonus board strategy ─────────────────────────────────────
   ;; 0 = always uncover passive first, 1 = always pick best one-time effect
   :prefer-onetime-bonus   0.3
   ;; ── Positional preferences ───────────────────────────────────
   ;; Preference for river-adjacent cities when placing temples
   :temple-river-pref      0.5
   ;; Preference for Eridu specifically (temples + raiders near it)
   :eridu-focus            0.3
   ;; ── Temporal preferences ─────────────────────────────────────
   ;; First turn of game: 0=conservative setup, 1=aggressive early scoring
   :first-turn-aggression  0.3
   ;; Last turn of round: 0=setup for next round, 1=score now
   :round-end-scoring      0.6
   ;; ── Opponent-reactive traits ─────────────────────────────────
   ;; Competitive nature: 0=avoid contested roles, 1=race opponents for them
   :competitive-roles      0.5
   ;; Group raiders with opponents (0=avoid, 1=stack on same routes)
   :raider-aggression      0.5
   ;; Magistrate denial: move magistrate AWAY from opponents who could benefit
   :magistrate-denial      0.3
   ;; Temple grouping: 0=avoid opponent temple cities, 1=place in same cities
   :temple-competition     0.5
   ;; Sell urgency when opponent has same goods and is nearby
   :sell-urgency           0.5
   ;; Feat response: 0=abandon feats opponents claimed, 1=still pursue for 2nd/3rd place
   :feat-persistence       0.5
   ;; ── Strategic pathing dimensions ─────────────────────────────
   ;; Glory path: 0=glory through raiders (deploy+influence+travel), 1=through role-5 end-game bonuses
   :glory-path             0.5
   ;; Board exploitation: 0=ignore board effects, 1=heavily prioritize uncovering board slots
   :board-exploitation     0.5
   ;; Feat rush: 0=slow steady build, 1=rush to claim feats as fast as possible
   :feat-rush              0.3
   ;; Feat sequence priority: 0=focus all effort on first feat before considering 2nd,
   ;; 1=spread effort across both target feats simultaneously for rapid chaining
   :feat-sequence          0.4
   ;; Feat closure depth: how many turns of lookahead to weight when close to completing a feat
   ;; 0=no special urgency, 1=heavily prioritize near-complete feats
   :feat-closure-urgency   0.5
   ;; ── Round-level action budget ────────────────────────────
   ;; Minimum sells per round: if below this, boost sell priority
   :min-sells-per-round    1
   ;; Minimum deploys per round: if below this, boost deploy priority
   :min-deploys-per-round  0
   ;; ── Game-relative dimensions (NEUTRAL at 0.0 — GA discovers value) ──
   ;; Standing awareness: 0=play own win condition in isolation,
   ;; >0=add catch-up urgency on the binding track when behind the field's
   ;; reputation leader. Fills the "bot plays solitaire" gap.
   :standing-awareness     0.0
   ;; Supply conservation: 0=spend raiders/temples freely,
   ;; >0=husband the last unit of finite raider/temple stock for reserve.
   :supply-conservation    0.0
   ;; Feat-race urgency: 0=ignore opponents' contest progress,
   ;; >0=rush to claim a contest first (3 wild pts) when an opponent is also
   ;; close to it. Fills the highest-leverage opponent-blindness gap.
   :feat-race-urgency      0.0
   ;; Temple engine: 0=price temple placement/flips flat (the myopic default,
   ;; which under-builds the base), >0=value building a wide temple base and
   ;; flipping it late, reflecting the COMPOUNDING payoff (a flip scores
   ;; amity = your face-down count). The fix for the priest-path bot blind spot.
   :temple-engine          0.0
   ;; ── Feat-chain planning (default = prior hardcoded chain-score weights, so
   ;;    behavior is unchanged at default and the GA tunes the trade-off) ──
   ;; Synergy: how much to favor a feat that SHARES setup with the partner feat
   ;; (overlapping action profile) — one investment cashes two feats.
   :feat-synergy           0.5
   ;; Bonus foresight: how much the bonus-board slot a claim would UNLOCK pulls
   ;; the bot toward that feat (predictive "once I claim this I'll be better at X").
   :bonus-foresight        0.3
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
          :travel-for-sell  2.5
          :min-travels-per-round 1
          :max-travels-per-round 2   ;; merchants sell, don't wander
          :role-action-coupling  0.8  ;; merchant level → more selling
          :resource-planning     0.7  ;; plan resources for role costs
          :score-balance-target  0.65 ;; amity-leaning
          :feat-awareness        0.4
          :endgame-role-push     0.6
          :glory-path            0.8  ;; merchants prefer role-5 glory
          :board-exploitation    0.4
          :feat-rush             0.3
          :feat-sequence          0.3  ;; merchants focus one feat at a time
          :feat-closure-urgency   0.6}))

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
          :influence-flip-raider 3.0
          :min-travels-per-round 0   ;; warlords don't need to travel
          :max-travels-per-round 2
          :role-action-coupling  0.9  ;; raider level → more deploying
          :resource-planning     0.3  ;; less concerned with role resources
          :score-balance-target  0.35 ;; glory-leaning
          :feat-awareness        0.2
          :endgame-role-push     0.7
          :glory-path            0.1  ;; warlords score glory through raiders
          :board-exploitation    0.5
          :feat-rush             0.2
          :feat-sequence          0.3
          :feat-closure-urgency   0.4}))

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
          :travel-for-temple 3.0
          :min-travels-per-round 2   ;; pilgrims MUST travel
          :max-travels-per-round 4
          :role-action-coupling  0.7  ;; priest level → more temples
          :resource-planning     0.6
          :score-balance-target  0.55 ;; slightly amity
          :feat-awareness        0.5  ;; temple feats are common
          :endgame-role-push     0.5
          :glory-path            0.6  ;; pilgrims mix both
          :board-exploitation    0.6
          :feat-rush             0.4
          :feat-sequence          0.5  ;; pilgrims can work on both
          :feat-closure-urgency   0.6}))

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
          :influence-flip-raider 2.0
          :role-action-coupling  0.8  ;; leader level → more influence
          :resource-planning     0.5
          :score-balance-target  0.4  ;; glory-leaning (magistrate bonus)
          :feat-awareness        0.6  ;; magistrate feats
          :endgame-role-push     0.8
          :glory-path            0.4  ;; diplomats use magistrate for glory
          :board-exploitation    0.5
          :feat-rush             0.5
          :feat-sequence          0.6  ;; diplomats chain feats well
          :feat-closure-urgency   0.7}))

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
          :contest-focus    0.8
          :role-action-coupling  0.6
          :resource-planning     0.8  ;; needs resources for multi-role advancement
          :score-balance-target  0.5  ;; perfectly balanced
          :feat-awareness        0.8  ;; contest-focused
          :tempo                 0.5  ;; moderate timing
          :endgame-role-push     0.9
          :glory-path            0.9  ;; scholars get glory from role-5
          :board-exploitation    0.7
          :feat-rush             0.6
          :feat-sequence          0.8  ;; scholars pursue multiple feats
          :feat-closure-urgency   0.8}))

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
          :travel-for-temple 2.2
          :role-action-coupling  0.4  ;; less tied to roles, more reactive
          :resource-planning     0.3
          :score-balance-target  0.5
          :feat-awareness        0.5
          :tempo                 0.2  ;; claim feats quickly
          :endgame-role-push     0.4
          :glory-path            0.3  ;; opportunists use whatever works
          :board-exploitation    0.8  ;; exploit board effects
          :feat-rush             0.7  ;; rush feats
          :feat-sequence          0.7  ;; oppportunist chains feats
          :feat-closure-urgency   0.5
          }))

(def archetypes
  [archetype-merchant archetype-warlord archetype-pilgrim
   archetype-diplomat archetype-scholar archetype-opportunist])

;; =============================================================================
;; Decision engine — delegated to eridu.decision
;; =============================================================================
;;
;; The weight-based scoring used to live here as one big `case phase`. It now
;; lives in `eridu.decision/decide`, the single unified decision-making module:
;; it scores the SAME presented choices a human sees (no engine internals) and
;; processes these (unchanged) weights through per-choice features with
;; game-state context modulation. `personality-step` stays as the public entry
;; point so existing callers (simulate, evolve, the live socket, the offline
;; cljs bot, diagnose/replay) need no change.

(defn personality-step
  "Pick a choice for a bot with the given personality weights.
   Returns [choice-key next-state] or nil. Delegates to `eridu.decision/decide`."
  [state weights]
  (decision/decide state weights))

;; =============================================================================
;; Random personality generation (for genetic algorithm)
;; =============================================================================

(defn random-personality
  "Generate a random personality by perturbing default weights."
  ([] (random-personality "Random"))
  ([name-prefix]
   (let [id (subs (str (random-uuid)) 0 8)]
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
             :influence-flip-raider (+ 0.5 (rand 3.5))
             ;; New coherence dimensions
             :role-action-coupling  (rand)
             :resource-planning     (rand)
             :score-balance-target  (+ 0.2 (rand 0.6))  ;; 0.2-0.8 range
             :feat-awareness        (rand)
             :tempo                 (rand)
             :endgame-role-push     (rand)
             ;; Travel and positioning
             :min-travels-per-round (rand-int 3)        ;; 0-2
             :max-travels-per-round (+ 1 (rand-int 4))  ;; 1-4
             :resource-to-move      (rand)
             :avoid-enemy-flip      (rand)
             :raider-pickup-priority (rand)
             ;; Role thresholds
             :sell-threshold         (+ 1 (rand-int 4))
             :temple-flip-threshold  (+ 1 (rand-int 3))
             :raider-magistrate-pref (rand)
             :magistrate-setup       (rand)
             :prefer-onetime-bonus   (rand)
             ;; Positional
             :temple-river-pref      (rand)
             :eridu-focus            (rand)
             ;; Temporal
             :first-turn-aggression  (rand)
             :round-end-scoring      (+ 0.3 (rand 0.7))
             ;; Opponent-reactive
             :competitive-roles      (rand)
             :raider-aggression      (rand)
             :magistrate-denial      (rand)
             :temple-competition     (rand)
             :sell-urgency           (rand)
             :feat-persistence       (rand)
             ;; Strategic pathing
             :glory-path             (rand)
             :board-exploitation     (rand)
             :feat-rush              (rand)
             :feat-sequence          (rand)
             :feat-closure-urgency   (rand)
             ;; Round budget
             :min-sells-per-round    (rand-int 3)         ;; 0-2
             :min-deploys-per-round  (rand-int 2)         ;; 0-1
             ;; Game-relative dimensions (explore the full 0-1 range)
             :standing-awareness     (rand)
             :supply-conservation    (rand)
             :feat-race-urgency      (rand)
             :temple-engine          (rand)
             :feat-synergy           (rand)
             :bonus-foresight        (rand)
             }))))

(def weight-bounds
  "Per-key [lo hi] (optionally :int) clamps for mutation, matching the ranges
   random-personality draws from. Without an UPPER clamp, mutate-personality's
   ±30% multiplicative deltas let weights run away (observed: take-weight 11.4,
   role-action-coupling 1.63) far outside their design ranges — noise the
   (collapsed) fitness signal failed to punish. Clamping keeps the genome in a
   meaningful, comparable space."
  {:sell-weight [0.05 2.5]   :temple-weight [0.05 2.5]
   :deploy-weight [0.05 2.5] :influence-weight [0.05 2.5]
   :travel-weight [0.05 2.0] :take-weight [0.3 2.0]
   :track-balance [0.0 1.0]  :early-role-bias [0.1 1.2]
   :chain-weight [0.0 1.5]   :contest-focus [0.0 1.0]
   :resource-hoard [0.0 1.0] :excess-penalty [0.5 6.5]
   :temple-in-demand-city [0.3 3.5] :deploy-near-opponents [0.3 3.5]
   :travel-for-temple [0.3 4.5] :travel-for-sell [0.3 4.0]
   :influence-flip-raider [0.3 4.5]
   :role-action-coupling [0.0 1.0]  :resource-planning [0.0 1.0]
   :score-balance-target [0.15 0.85] :feat-awareness [0.0 1.0]
   :tempo [0.0 1.0] :endgame-role-push [0.0 1.0]
   :min-travels-per-round [0 3 :int] :max-travels-per-round [1 5 :int]
   :resource-to-move [0.0 1.0] :avoid-enemy-flip [0.0 1.0]
   :raider-pickup-priority [0.0 1.0]
   :sell-threshold [1 5 :int] :temple-flip-threshold [1 4 :int]
   :raider-magistrate-pref [0.0 1.0] :magistrate-setup [0.0 1.0]
   :prefer-onetime-bonus [0.0 1.0] :temple-river-pref [0.0 1.0]
   :eridu-focus [0.0 1.0] :first-turn-aggression [0.0 1.0]
   :round-end-scoring [0.1 1.2]
   :competitive-roles [0.0 1.0] :raider-aggression [0.0 1.0]
   :magistrate-denial [0.0 1.0] :temple-competition [0.0 1.0]
   :sell-urgency [0.0 1.0] :feat-persistence [0.0 1.0]
   :glory-path [0.0 1.0] :board-exploitation [0.0 1.0]
   :feat-rush [0.0 1.0] :feat-sequence [0.0 1.0]
   :feat-closure-urgency [0.0 1.0]
   :min-sells-per-round [0 3 :int] :min-deploys-per-round [0 2 :int]
   :standing-awareness [0.0 1.0] :supply-conservation [0.0 1.0]
   :feat-race-urgency [0.0 1.0] :temple-engine [0.0 1.0]
   :feat-synergy [0.0 1.5] :bonus-foresight [0.0 1.0]})

(defn clamp-weight
  "Clamp a mutated weight value to its design [lo hi] range (the fix for the
   runaway-upper-bound leak). Keys without bounds fall back to the legacy 0.05
   floor. Integer-typed keys keep a float value here — the consuming code uses
   them only in numeric comparisons, so a drifted 2.3 behaves like its floor;
   what matters is that the upper bound is now enforced."
  [k v]
  (if-let [[lo hi] (get weight-bounds k)]
    (max lo (min hi v))
    (max 0.05 v)))

(defn mutate-personality
  "Mutate a personality by tweaking random weights."
  [personality mutation-rate]
  (let [numeric-keys [:sell-weight :temple-weight :deploy-weight :influence-weight
                       :travel-weight :take-weight :track-balance :early-role-bias
                       :chain-weight :contest-focus :resource-hoard :excess-penalty
                       :temple-in-demand-city :deploy-near-opponents
                       :travel-for-temple :travel-for-sell :influence-flip-raider
                       :role-action-coupling :resource-planning :score-balance-target
                       :feat-awareness :tempo :endgame-role-push
                       :min-travels-per-round :max-travels-per-round
                       :resource-to-move :avoid-enemy-flip :raider-pickup-priority
                       :sell-threshold :temple-flip-threshold :raider-magistrate-pref
                       :magistrate-setup :prefer-onetime-bonus
                       :temple-river-pref :eridu-focus
                       :first-turn-aggression :round-end-scoring
                       :competitive-roles :raider-aggression :magistrate-denial
                       :temple-competition :sell-urgency :feat-persistence]
        numeric-keys (concat numeric-keys [:glory-path :board-exploitation :feat-rush
                                           :feat-sequence :feat-closure-urgency
                                           :min-sells-per-round :min-deploys-per-round
                                           :standing-awareness :supply-conservation
                                           :feat-race-urgency :temple-engine
                                           :feat-synergy :bonus-foresight])
        mutated (reduce (fn [p k]
                          (if (< (rand) mutation-rate)
                            (let [v (get p k 1.0)
                                  delta (* v (- (rand 0.6) 0.3))] ;; ±30%
                              (assoc p k (clamp-weight k (+ v delta))))
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
                       :travel-for-temple :travel-for-sell :influence-flip-raider
                       :role-action-coupling :resource-planning :score-balance-target
                       :feat-awareness :tempo :endgame-role-push
                       :min-travels-per-round :max-travels-per-round
                       :glory-path :board-exploitation :feat-rush
                       :feat-sequence :feat-closure-urgency
                       :min-sells-per-round :min-deploys-per-round
                       :standing-awareness :supply-conservation
                       :feat-race-urgency :temple-engine
                       :feat-synergy :bonus-foresight]
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
        id (subs (str (random-uuid)) 0 8)]
    (assoc child :name (str "Gen-" id))))
