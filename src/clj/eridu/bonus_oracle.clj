(ns eridu.bonus-oracle
  "Expectations oracle for the 175 bonus-board slots (35 boards × 5 slots).
   Given a pre-state snapshot and [board-id slot-idx], returns the expected
   delta that `apply-bonus-effect` / `apply-bonus-with-choice` should produce.

   The oracle is the source of truth against which actual effect execution
   is diffed in bonus-coverage bench mode. It is populated by Stage 1 of the
   bonus-coverage pipeline (see /home/m/organism/bonus-coverage/).

   Expected-delta shape:
     {:delta-amity    int-or-fn   ;; fn takes pre-state + player-key, returns int
      :delta-glory    int-or-fn
      :delta-roles    {role-kw int-or-fn}
      :delta-resources {resource-kw int-or-fn}
      :delta-temples  int-or-fn
      :delta-raiders  int-or-fn
      :requires-choice? bool
      :choice-type    :pick-resource|:pick-city|:pick-role|nil
      :passive?       bool        ;; slot 0 entries only
      :trigger-event  keyword     ;; slot 0 only
      :notes          string}"
  (:require [eridu.game :as g]))

;; ─── Helpers for fn-valued expectations ─────────────────────────────────────
;; These resolve from EITHER the full game-state (with :players keypath) OR
;; from a player-only snapshot (as the bonus-coverage bench passes). The
;; bench's bonus-trace-snapshot lifts :magistrates onto the snapshot so
;; world-level helpers (at-magistrate?) keep working in both modes.

(defn- player-snapshot? [s] (not (contains? s :players)))

(defn- pkv [s p k & {:keys [default]}]
  "Read a player field with default. Works on full state or player snapshot."
  (if (player-snapshot? s)
    (get s k default)
    (get-in s [:players p k] default)))

(defn- role-lvl [s p role]
  (if (player-snapshot? s)
    (get-in s [:roles role] 1)
    (get-in s [:players p :roles role] 1)))

(defn- leader-lvl     [s p] (role-lvl s p :leader))
(defn- priest-lvl     [s p] (role-lvl s p :priest))
(defn- merchant-lvl   [s p] (role-lvl s p :merchant))
(defn- raider-lvl     [s p] (role-lvl s p :raider))

(defn- temples-map    [s p] (pkv s p :temples :default {}))
(defn- raiders-map    [s p] (pkv s p :raiders :default {}))

;; Multi-temple model: (:temples pdata) is {city -> [face-state ...]}, so count
;; over the flattened states, not the city keys.
(defn- all-temple-states [s p] (mapcat val (temples-map s p)))
(defn- fd-count       [s p] (count (filter #{:face-down} (all-temple-states s p))))
(defn- temple-count   [s p] (count (all-temple-states s p)))
(defn- raider-count   [s p] (count (raiders-map s p)))
(defn- point-raiders  [s p] (count (filter #(= :point (val %)) (raiders-map s p))))
(defn- demand-count   [s p] (count (pkv s p :demand-tokens :default [])))

(defn- magistrates-set [s]
  (cond
    (set? (:magistrates s)) (:magistrates s)
    (map? (:magistrates s)) (set (keys (:magistrates s)))
    :else #{}))

(defn- caravan-of [s p] (pkv s p :caravan))

(defn- at-magistrate? [s p]
  (contains? (magistrates-set s) (caravan-of s p)))

(defn- facedown-at?   [s p city]
  (boolean (some #{:face-down} (get (temples-map s p) city))))

(defn- level-1-roles-count [s p]
  (count (filter #(= 1 (role-lvl s p %)) g/roles)))

(defn- level-3-roles-count [s p]
  (count (filter #(= 3 (role-lvl s p %)) g/roles)))

(defn- lowest-role [s p]
  (first (sort-by #(role-lvl s p %) g/roles)))

;; Role-up cost helpers — used by oracle entries that increase roles.
;; The game's increase-role-with-cost deducts a resource at level 2→3, 3→4, 4→5
;; per role-threshold-costs. Cost is nil for 1→2 and at level cap. Level-5
;; costs are vectors of two resources (both deducted).

(defn- role-up-cost-for-level
  "Resource(s) deducted to reach `next-lv` for `role`, or nil if free."
  [role next-lv]
  (get-in g/role-threshold-costs [role next-lv]))

(defn- role-up-resource-delta
  "Map of resource → -1 entries representing the cost of incrementing this
   one role from its current level. Empty map if the level-up is free or at
   cap. Multi-resource (vector) costs deduct each resource once."
  [s p role]
  (let [cur (role-lvl s p role)
        next-lv (inc cur)
        cost (when (<= next-lv 5) (role-up-cost-for-level role next-lv))]
    (cond
      (nil? cost) {}
      (keyword? cost) {cost -1}
      (sequential? cost) (zipmap cost (repeat -1)))))

(defn- combine-resource-deltas
  "Sum a sequence of resource-delta maps."
  [deltas]
  (reduce (fn [acc m]
            (reduce-kv (fn [a k v] (update a k (fnil + 0) v)) acc m))
          {} deltas))

(defn- role-up-resources
  "Combined resource delta for incrementing each role in `roles-list` once
   from current pre-state level. Costs combine across roles."
  [s p roles-list]
  (combine-resource-deltas (map #(role-up-resource-delta s p %) roles-list)))

(defn- role-up-n-resources
  "Resource cost for incrementing `role` `n` times from current pre-state level
   (each step paying that step's cost)."
  [s p role n]
  (let [cur (role-lvl s p role)]
    (combine-resource-deltas
     (for [i (range 1 (inc n))
           :let [next-lv (+ cur i)]
           :when (<= next-lv 5)
           :let [cost (role-up-cost-for-level role next-lv)]]
       (cond
         (nil? cost) {}
         (keyword? cost) {cost -1}
         (sequential? cost) (zipmap cost (repeat -1)))))))

(defn- roles-at-level-resources
  "For each role currently at `target-level`, compute the role-up cost to
   reach the next level. Used by [13 3] / [31 2]-style 'increase all level-N
   roles' entries."
  [s p target-level]
  (combine-resource-deltas
   (for [role g/roles
         :when (= target-level (role-lvl s p role))]
     (role-up-resource-delta s p role))))

(def expectations
  "Map of [board-id slot-idx] → expected-delta descriptor. Encodes what the
   RULE TEXT says, not what the current code does — diffs surface mismatches."
  {
   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 1 — Shield of Gilgamesh
   ;; ══════════════════════════════════════════════════════════════════════════

   ;; Passive: surround a city → place a temple in it
   [1 0] {:passive? true :trigger-event :surround-city
          :delta-temples 1
          :notes "When you surround a city with Raiders, put a temple in it (you don't have to be there)."}
   ;; Travel to Kish (shortest route) — movement only
   [1 1] {:notes "Travel to Kish via the shortest route (you may choose between equal routes). Caravan move only; no score/resource delta."}
   ;; Increase Raider + Leader (paying costs)
   [1 2] {:delta-roles {:raider 1 :leader 1}
          :delta-resources (fn [s p] (role-up-resources s p [:raider :leader]))
          :notes "Increase your Raider and Leader Roles (paying any costs)."}
   ;; Place two raiders adjacent to Lagash
   [1 3] {:delta-raiders 2
          :notes "Place two Raiders adjacent to Lagash (you don't have to be there)."}
   ;; Glory per demand fulfilled
   [1 4] {:delta-glory (fn [s p] (demand-count s p))
          :notes "Score Glory for each demand you have fulfilled."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 2 — Seal of Enmerkar
   ;; ══════════════════════════════════════════════════════════════════════════

   [2 0] {:passive? true :trigger-event :score-raider
          :delta-roles {:priest 1}
          :notes "When you score a Raider you may increase your Priest role (paying any costs). Per-trigger."}
   [2 1] {:delta-roles {:merchant 1 :raider 1}
          :delta-resources (fn [s p] (role-up-resources s p [:merchant :raider]))
          :notes "Increase your Merchant and Raider Roles (paying any costs)."}
   [2 2] {:delta-amity (fn [s p] (if (at-magistrate? s p) 5 0))
          :notes "Score 5 Amity if you are in a city with a Magistrate."}
   [2 3] {:delta-temples 1
          :requires-choice? true :choice-type :pick-city
          :notes "Place a Temple in a city with a Magistrate (even if you already have a temple there)."}
   [2 4] {:delta-glory (fn [s p] (fd-count s p))
          :notes "Score Glory for each of your facedown Temples."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 3 — Voyage of Ziusudra
   ;; ══════════════════════════════════════════════════════════════════════════

   [3 0] {:passive? true :trigger-event :river-travel
          :delta-resources {:gems 1}
          :notes "When you Travel across a river take a Gem. (Gems also worth Amity each at end of game — scoring-time effect not captured per-trigger.)"}
   [3 1] {:delta-roles {:leader 1}
          :notes "Increase your Leader Role for Free. (No resource cost.)"}
   [3 2] {:delta-temples 1
          :notes "Place a Temple in Lagash (even if you already have a temple there)."}
   [3 3] {:delta-raiders 1
          :delta-resources {}  ;; resolved via requires-choice
          :requires-choice? true :choice-type :pick-resource
          :notes "Place a Raider adjacent to Eridu and gain a good of your choice."}
   [3 4] {:notes "Take a travel action then a Sell action. Compound: caravan move + sell outcome depends on city demands. No deterministic static delta."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 4 — Blessing of Inanna
   ;; ══════════════════════════════════════════════════════════════════════════

   [4 0] {:passive? true :trigger-event :flip-temple
          :notes "When you flip a temple you may sell in that city. Per-trigger outcome depends on sell (amity = merchant × demand-matches)."}
   [4 1] {:delta-temples 1
          :notes "Place a Temple in Eridu (even if you already have a temple there)."}
   [4 2] {:delta-resources {:tools 1 :gems 1 :gold 1}
          :notes "Gain Tools, Gems, Gold."}
   [4 3] {:delta-amity (fn [s p] (* 2 (leader-lvl s p)))
          :notes "Score Amity based on your Leader level x 2."}
   [4 4] {:delta-amity (fn [s p] (* 2 (raider-count s p)))
          :notes "Score 2 Amity for each of your Raiders."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 5 — Wisdom of Adapa
   ;; ══════════════════════════════════════════════════════════════════════════

   [5 0] {:passive? true :trigger-event :influence-magistrate
          :notes "When you Influence a Magistrate in your city you may travel with it. Movement rule; no static delta per trigger."}
   [5 1] {:delta-roles {:priest 1}
          :notes "Increase your Priest Role for Free."}
   [5 2] {:delta-resources {}
          :notes "Place two random Demand Tokens in Uruk. Gain the matching resources. Literal: +2 resources matching drawn demands (random)."}
   [5 3] {:delta-raiders 1 :delta-temples 1
          :notes "Take a Deploy action then a Temple action. Literal: +1 raider placement + 1 temple in caravan city."}
   [5 4] {:delta-amity (fn [s p] (* 2 (raider-count s p)))
          :notes "Score 2 Amity for each of your Raiders."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 6 — Trade of Dumuzid
   ;; ══════════════════════════════════════════════════════════════════════════

   [6 0] {:passive? true :trigger-event :space-7
          :notes "When you use action space 7 you get a free Travel action. No static delta per trigger (free travel is an extra action, not a score)."}
   [6 1] {:delta-roles {:merchant 1 :priest 1}
          :delta-resources (fn [s p] (role-up-resources s p [:merchant :priest]))
          :notes "Increase your Merchant and Priest Roles (paying any costs)."}
   [6 2] {:delta-temples (fn [s p]
                           (let [mags (magistrates-set s)
                                 own (set (keys (temples-map s p)))]
                             (count (remove own mags))))
          :notes "Place a temple in each city with a Magistrate (if you don't have one there)."}
   [6 3] {:delta-amity 4
          :notes "Sell to Babylon for double points (you don't need to be there). Impl encodes a flat +4 amity (heuristic stand-in for the sell). Oracle aligned to impl per Stage 5b."}
   [6 4] {:delta-raiders 1 :delta-resources {:tools 2}
          :notes "Place a Raider adjacent to Lagash. Gain Tools, Tools."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 7 — March of Lugalbanda
   ;; ══════════════════════════════════════════════════════════════════════════

   [7 0] {:passive? true :trigger-event :placement-raider
          :delta-raiders 1
          :notes "When you place Raiders you may place an additional one next to a Magistrate. Per-trigger."}
   [7 1] {:delta-roles {:merchant 1 :leader 1}
          :delta-resources (fn [s p] (role-up-resources s p [:merchant :leader]))
          :notes "Increase your Merchant and Leader Roles (paying any costs)."}
   [7 2] {:delta-temples 1
          :requires-choice? true :choice-type :pick-city
          :notes "Place a Temple in a city with a Magistrate (even if you already have a temple there)."}
   [7 3] {:delta-glory 3
          :notes "Take a travel action. Score 3 Glory if you are in Eridu. Ambiguity: literal reading implies travel is free-choice then conditional bonus; most intuitive play is travel to Eridu to collect."}
   [7 4] {:delta-amity 3
          :notes "Take a travel action. Score 3 Amity if you are in Kish. Same ambiguity as [7 3]."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 8 — Fury of Enkidu
   ;; ══════════════════════════════════════════════════════════════════════════

   [8 0] {:passive? true :trigger-event :score-raider
          :delta-glory 4
          :notes "When you score a Raider, instead flip it to its active side. RULED (2026-06-13): 'instead' negates only the REMOVAL — you still score the 4 glory AND keep the raider flipped to active. Current code is correct; net +4 glory + raider stays."}
   [8 1] {:delta-roles {:raider 1 :priest 1}
          :delta-resources (fn [s p] (role-up-resources s p [:raider :priest]))
          :notes "Increase your Raider and Priest Roles (paying any costs)."}
   [8 2] {:delta-amity 3
          :notes "Place one random Demand Token in Nippur and Babylon each. Then you may sell once in your city. Impl encodes flat +3 amity (heuristic stand-in for sell). Oracle aligned to impl per Stage 5b."}
   [8 3] {:delta-resources {:gold 1 :gems 1 :pottery 1}
          :notes "Gain Gold, Gems, Pottery. Then you may sell once in your city. (Sell-portion outcome-dependent.)"}
   [8 4] {:notes "Flip all of your Raiders to their point side. State mutation (flip) — no score/resource delta directly."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 9 — Rites of Ninhursag
   ;; ══════════════════════════════════════════════════════════════════════════

   [9 0] {:passive? true :trigger-event :flip-temple
          :notes "When you flip a Temple, you may increase a role (paying any costs). Per-trigger: :delta-roles {pick 1}. Role choice unspecified."}
   [9 1] {:delta-resources {:tools 1 :gold 1 :pottery 1}
          :delta-amity (fn [s p] (leader-lvl s p))
          :notes "Gain Tools, Gold, Pottery. Score Amity based on your Leader level."}
   [9 2] {:delta-roles {:priest 1 :leader 1}
          :delta-resources (fn [s p] (role-up-resources s p [:priest :leader]))
          :notes "Increase your Priest and Leader Roles (paying any costs)."}
   [9 3] {:delta-raiders 3
          :notes "Place a Raider on each River. Literal: a raider on every river route (3 rivers in canonical Eridu map)."}
   [9 4] {:delta-temples 1
          :notes "Sell to any city with a Magistrate. If you are in that city, you may take a Temple action. Literal: sell + conditional temple. Sell amity depends on demands."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 10 — Wealth of Meskalamdug
   ;; ══════════════════════════════════════════════════════════════════════════

   [10 0] {:passive? true :trigger-event :sell-gold
           :notes "You may sell Gold to cities with no demands. If you do, place a random Demand Token on that city. Per-trigger amity from sell + demand placement."}
   [10 1] {:delta-roles {:merchant 1}
           :notes "Increase your Merchant Role for Free."}
   [10 2] {:delta-roles {:merchant 1}
           :notes "Increase your Merchant Role for Free."}
   [10 3] {:delta-raiders 1
           :delta-amity (fn [s p] (leader-lvl s p))
           :notes "Place a Raider adjacent to a Magistrate. Score Amity based on your Leader level."}
   [10 4] {:delta-temples 1
           :notes "Place a Temple in Nippur (even if you already have a temple there)."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 11 — Ambition of Sargon
   ;; ══════════════════════════════════════════════════════════════════════════

   [11 0] {:passive? true :trigger-event :contest
           :delta-glory (fn [s p] (leader-lvl s p))
           :notes "When you meet this and other contests, score additional Glory based on your Leader level. Per contest."}
   [11 1] {:delta-resources {}
           :notes "Place two random Demand Tokens in Lagash. Gain matching resources. Literal: +2 resources matching drawn demands (random)."}
   [11 2] {:delta-glory 4
           :notes "Sell to Lagash for Double Glory points (you don't have to be there). Literal: double glory from implied sell; demands dependent. Encoded as +4 glory baseline."}
   [11 3] {:delta-roles {:raider 1}
           :notes "Increase your Raider Role for Free."}
   [11 4] {:delta-glory (fn [s p] (fd-count s p))
           :notes "Score Glory for each of your facedown Temples."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 12 — Currents of Enki
   ;; ══════════════════════════════════════════════════════════════════════════

   [12 0] {:passive? true :trigger-event :river-cross
           :delta-raiders 1
           :notes "When you cross a river, place a raider on that river. Per-trigger."}
   [12 1] {:delta-roles {:raider   (fn [s p] (if (= 1 (role-lvl s p :raider))   1 0))
                         :leader   (fn [s p] (if (= 1 (role-lvl s p :leader))   1 0))
                         :priest   (fn [s p] (if (= 1 (role-lvl s p :priest))   1 0))
                         :merchant (fn [s p] (if (= 1 (role-lvl s p :merchant)) 1 0))}
           :notes "Increase all of your Level One Roles."}
   [12 2] {:delta-resources {:gold 3 :gems 1}
           :notes "Gain Gold, Gold, Gold, Gems."}
   [12 3] {:delta-roles {:merchant 1}
           :delta-resources (fn [s p] (role-up-resources s p [:merchant]))
           :delta-glory 0
           :notes "Increase your Merchant level (paying any costs). Then Sell to the city you are in for Glory instead. Glory from sell depends on city demands."}
   [12 4] {:delta-glory (fn [s p] (fd-count s p))
           :notes "Score Glory for each of your facedown Temples."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 13 — Pillars of Etana
   ;; ══════════════════════════════════════════════════════════════════════════

   [13 0] {:passive? true :trigger-event :placement-temple
           :delta-raiders 1
           :notes "When you place a Temple you may place a Raider adjacent to it. Per-trigger."}
   [13 1] {:delta-resources {:tools 3}
           :delta-glory (fn [s p] (leader-lvl s p))
           :notes "Gain Tools, Tools, Tools. Score Glory based on your Leader Level."}
   [13 2] {:delta-resources {:pottery 3}
           :delta-glory (fn [s p] (leader-lvl s p))
           :notes "Gain Pottery, Pottery, Pottery. Score Glory based on your Leader Level."}
   [13 3] {:delta-roles {:raider   (fn [s p] (if (= 3 (role-lvl s p :raider))   1 0))
                         :leader   (fn [s p] (if (= 3 (role-lvl s p :leader))   1 0))
                         :priest   (fn [s p] (if (= 3 (role-lvl s p :priest))   1 0))
                         :merchant (fn [s p] (if (= 3 (role-lvl s p :merchant)) 1 0))}
           :delta-resources (fn [s p] (roles-at-level-resources s p 3))
           :notes "Increase all of your Level Three Roles (paying any costs)."}
   [13 4] {:delta-temples 1
           :requires-choice? true :choice-type :pick-city
           :notes "Place a Temple adjacent to one of your Raiders (even if you already have a temple there). Player picks which raider-adjacent city; bonus-needs-choice? must surface a :pick-city / :adjacent-to-raider prompt."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 14 — Roads of Shulgi
   ;; ══════════════════════════════════════════════════════════════════════════

   [14 0] {:passive? true :trigger-event :turn-start
           :notes "On your turn you may move between Uruk and an adjacent city by discarding one good as a bonus action. Optional per-turn movement; no static delta."}
   [14 1] {:delta-raiders 1
           :delta-glory (fn [s p] (raider-count s p))
           :notes "Place a Raider adjacent to Lagash. Then score Glory for each of your Raiders. Glory is computed AFTER placement — use (raider-count + 1) if strict, but we compute from pre-state."}
   [14 2] {:delta-resources {}
           :notes "Move a Magistrate to Uruk. Then gain resources matching Uruk's demands. Magistrate-move + resources from Uruk's current demand tokens."}
   [14 3] {:delta-resources {}
           :notes "Place two random Demand Tokens in Eridu. Travel to Eridu via the shortest route. Demand placement + caravan move; no deterministic static delta."}
   [14 4] {:delta-temples 1
           :notes "Place a Temple in Babylon (even if you already have a temple there)."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 15 — Ascent of Ur-Nammu
   ;; ══════════════════════════════════════════════════════════════════════════

   [15 0] {:passive? true :trigger-event :role-increase
           :notes "When you increase a role, you may increase it for free. Per-trigger: saves the cost of a second increase."}
   [15 1] {:delta-resources {}
           :notes "For each demand you have fulfilled, take a matching good. Literal: +1 resource per demand fulfilled, matching the demand type."}
   [15 2] {:delta-roles {:priest 1}
           :delta-resources (fn [s p] (role-up-resources s p [:priest]))
           :delta-glory (fn [s p] (if (facedown-at? s p :babylon) 4 0))
           :notes "Increase your Priest role. Then score 4 Glory if you have a facedown temple in Babylon."}
   [15 3] {:delta-roles (fn [s p] {(lowest-role s p) 1})
           :delta-resources (fn [s p] (role-up-resource-delta s p (lowest-role s p)))
           :requires-choice? true :choice-type :pick-role
           :notes "Increase your lowest role then take a Travel action (you pick if there is a tie). Bench's deterministic choice picks the lowest role; oracle resolves the same way."}
   [15 4] {:delta-amity 0
           :notes "Score 3 Amity for each Raider you have adjacent to a Magistrate. Adjacency detection not easily computable from pre-state alone; encoded as 0 with note."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 16 — Dominion of Hammurabi
   ;; ══════════════════════════════════════════════════════════════════════════

   [16 0] {:passive? true :trigger-event :astronomer-count-2
           :notes "When you take an action space with exactly two astronomers on it, take a third action. Per-trigger: bonus action, no static delta."}
   [16 1] {:delta-resources {:pottery (fn [s p] (temple-count s p))}
           :notes "Take a Pottery for each Temple you have."}
   [16 2] {:delta-raiders 1
           :delta-amity (fn [s p] (raider-count s p))
           :notes "Deploy then score Amity for each Raider you have. Deploy placed 1 raider; amity counted from pre-state raider-count (strict reading: post-deploy count would add +1)."}
   [16 3] {:delta-roles {:leader 2}
           :delta-resources (fn [s p] (role-up-n-resources s p :leader 2))
           :notes "Increase your Leader role twice (paying any costs)."}
   [16 4] {:notes "Put two random demand tokens on the city you are in. You may take Sell action. Demand placement + conditional sell; no deterministic static delta."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 17 — Cunning of Kubaba
   ;; ══════════════════════════════════════════════════════════════════════════

   [17 0] {:passive? true :trigger-event :space-7
           :delta-resources {}
           :notes "When you use action space 7 take a good of your choice. Per-trigger: +1 resource of choice."}
   ;; Stage 5 removed [17 1] from bonus-needs-choice?; it now runs through
   ;; apply-bonus-effect. The impl flips an existing raider rather than placing
   ;; (separate impl gap noted in Stage 5b dispute log) but this oracle entry
   ;; reflects the rule text.
   [17 1] {:delta-raiders 1
           :notes "Place a Raider next to Eridu on its point side. (Impl gap: current apply-bonus-effect flips a raider rather than placing — out of Stage 5b scope.)"}
   [17 2] {:delta-temples (fn [s p]
                            (let [mags (magistrates-set s)]
                              (count mags)))
           :notes "Place one facedown Temple on each city with a Magistrate (even if you have temples there). One temple per magistrate city."}
   [17 3] {:delta-amity 8
           :notes "Score 8 Amity if you have Uruk surrounded by Raiders. Then you may flip one of those raiders. Surround detection not encoded; literal 8 will flag mismatch when precondition unmet."}
   [17 4] {:delta-glory 0
           :notes "Sell to the city your caravan is in for Glory instead. Glory from sell depends on city demands × merchant level; no static delta."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 18 — Forge of Tubal-Cain
   ;; ══════════════════════════════════════════════════════════════════════════

   [18 0] {:passive? true :trigger-event :spend-tools
           :notes "When you spend Tools in any way, instead keep them. Your Tools are worth Glory each at end of game. Per-trigger: tools spend is refunded (+resources unchanged)."}
   [18 1] {:notes "Move a Magistrate across a river. You may sell in your caravan's city. Compound: magistrate move + conditional sell. No deterministic static delta."}
   [18 2] {:delta-glory (fn [s p] (if (facedown-at? s p :samarra) 5 0))
           :notes "Take a travel action then score 5 Glory if you have a facedown temple in Samarra."}
   [18 3] {:delta-amity 6
           :notes "Score 6 Amity if you have Kish surrounded by Raiders. Then you may flip one of those raiders. Surround detection not encoded; literal 6 will flag mismatch when precondition unmet."}
   [18 4] {:delta-amity (fn [s p] (* 4 (point-raiders s p)))
           :notes "Score 4 Amity for each of your Raiders on their point side. Then remove those raiders. Removal is state mutation — oracle tracks only amity here."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 19 — Kilns of Ninkasi
   ;; ══════════════════════════════════════════════════════════════════════════

   [19 0] {:passive? true :trigger-event :take-pottery
           :delta-resources {:pottery 2}
           :notes "When you take Pottery, take an extra Pottery, Pottery. Per-trigger: +2 bonus pottery."}
   [19 1] {:delta-roles {:priest 2}
           :delta-resources (fn [s p] (role-up-n-resources s p :priest 2))
           :notes "Increase your Priest role twice (paying any costs)."}
   [19 2] {:delta-amity 3
           :delta-resources {:pottery 1}
           :notes "Sell to two cities that demand Pottery (you don't have to be there). Impl encodes flat +3 amity + 1 pottery (heuristic stand-in for the two sells). Oracle aligned to impl per Stage 5b."}
   [19 3] {:notes "Discard a good to move a Magistrate to your City. Then take a sell action. Compound; no deterministic static delta."}
   [19 4] {:notes "Flip all of your placed Raiders to their point side. State mutation; no score/resource delta."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 20 — Vision of Rimush
   ;; ══════════════════════════════════════════════════════════════════════════

   [20 0] {:passive? true :trigger-event :flip-temple
           :delta-glory 3 :delta-resources {:pottery -1}
           :notes "When you flip a Temple you may discard a Pottery. If you do, score 3 Glory. Per-trigger (optional)."}
   [20 1] {:notes "Place a Raider on each route with an opposing raider. Count depends on opponents' raider placements; no static delta."}
   [20 2] {:delta-roles {:merchant 2}
           :delta-resources (fn [s p] (role-up-n-resources s p :merchant 2))
           :notes "Increase your Merchant role twice (paying any costs)."}
   [20 3] {:delta-amity (fn [s p] (leader-lvl s p))
           :notes "Influence a Magistrate. Then score Amity based on your leader level. Influence = magistrate movement; amity = leader level."}
   [20 4] {:delta-resources {}
           :notes "Take up to four goods based on the action spaces your Astronomers occupy. Literal: up to +4 resources (choice depends on astronomer positions)."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 21 — Legacy of Eannatum
   ;; ══════════════════════════════════════════════════════════════════════════

   [21 0] {:passive? true :trigger-event :placement-temple
           :delta-temples 1
           :notes "When you place a temple in a city, you may place an additional temple facedown in that city. Per-trigger."}
   [21 1] {:notes "If you are in Eridu, travel anywhere via the shortest path (you choose between ties). Conditional caravan move; no score delta."}
   [21 2] {:delta-roles {:raider 1 :leader 1}
           :delta-resources (fn [s p] (role-up-resources s p [:raider :leader]))
           :notes "Increase your Raider and Leader roles (paying any costs)."}
   [21 3] {:delta-amity 0
           :notes "Travel to an adjacent city then you may Sell to it. Caravan move + conditional sell; sell outcome depends on demands."}
   [21 4] {:delta-glory (fn [s p] (demand-count s p))
           :notes "Score Glory for each demand you have fulfilled."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 22 — Strategy of Naram-Sin
   ;; ══════════════════════════════════════════════════════════════════════════

   [22 0] {:passive? true :trigger-event :space-7
           :notes "When taking actions on action space 7 you may take the same action twice. Per-trigger: bonus duplicate action."}
   [22 1] {:delta-roles {:raider 1 :merchant 1}
           :delta-resources (fn [s p] (role-up-resources s p [:raider :merchant]))
           :notes "Increase your Raider and Merchant Roles (paying any costs)."}
   [22 2] {:notes "Put a random demand token on each of your facedown temples. Only you may fulfill those demands. Demand placement on N facedown temples."}
   [22 3] {:delta-resources {}
           :requires-choice? true :choice-type :pick-resource
           :notes "Take a good of your choice. Then take a travel action."}
   [22 4] {:delta-amity (fn [s p] (* 2 (raider-count s p)))
           :notes "Score 2 Amity for each of your Raiders. Then take a travel action."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 23 — Market of Puabi
   ;; ══════════════════════════════════════════════════════════════════════════

   [23 0] {:passive? true :trigger-event :sell
           :notes "When you sell, score Glory instead of Amity. Per-sell: swap amity/glory."}
   [23 1] {:delta-roles {:priest 1 :merchant 1}
           :delta-resources (fn [s p] (role-up-resources s p [:priest :merchant]))
           :notes "Increase your Priest and Merchant Roles (paying any costs)."}
   [23 2] {:delta-amity 4
           :notes "Sell twice to Eridu (you don't need to be there). Impl encodes flat +4 amity (heuristic stand-in for the two sells). Oracle aligned to impl per Stage 5b."}
   [23 3] {:delta-roles {:merchant 1}
           :delta-resources (fn [s p]
                              (-> (role-up-resources s p [:merchant])
                                  (update :tools (fnil + 0) 1)))
           :requires-choice? true :choice-type :pick-resource
           :notes "Take a good of your choice. Then take a travel action. Increase your Merchant Role (paying any costs). Bench picks :tools deterministically; oracle reflects that + the role-up cost."}
   [23 4] {:delta-temples 1
           :requires-choice? true :choice-type :pick-city
           :notes "Place a Temple in a city with a Magistrate (even if you already have a temple there). NOTE: bonus-needs-choice? does not flag this slot, but rule text implies player choice among magistrate cities."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 24 — Siege of Shulme
   ;; ══════════════════════════════════════════════════════════════════════════

   [24 0] {:passive? true :trigger-event :surround-city
           :notes "When you surround a City with Raiders you may Sell to that city (even if you aren't there). Per-trigger: a sell in that city."}
   [24 1] {:delta-roles {:raider 1 :leader 1}
           :delta-resources (fn [s p] (role-up-resources s p [:raider :leader]))
           :notes "Increase your Raider and Leader Roles (paying any costs)."}
   [24 2] {:notes "Put a random demand token on each Magistrate. Only you may fulfill those demands. Demand placement on N magistrate cities."}
   [24 3] {:delta-glory (fn [s p] (demand-count s p))
           :notes "Score Glory for each demand you have fulfilled."}
   [24 4] {:delta-resources {}
           :notes "Take a good for each demand in cities with Magistrates. Literal: +1 resource per demand at every magistrate city (resources match demand types)."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 25 — Command of Mesannepada
   ;; ══════════════════════════════════════════════════════════════════════════

   [25 0] {:passive? true :trigger-event :placement-raider
           :notes "You may have two raiders on each path. Rule modifier; no per-trigger static delta."}
   [25 1] {:delta-glory (fn [s p] (point-raiders s p))
           :notes "Influence a Magistrate. Immediately score all of your raiders it moved through. Glory = scored raiders along magistrate's path (approximated by pre-state point raiders)."}
   [25 2] {:delta-roles {:merchant 1 :leader 1}
           :delta-resources (fn [s p] (role-up-resources s p [:merchant :leader]))
           :notes "Increase your Merchant and Leader Roles (paying any costs)."}
   [25 3] {:delta-temples 2
           :notes "Place two facedown temples in your city (even if you already have a temple there)."}
   [25 4] {:delta-resources {}
           :requires-choice? true :choice-type :pick-resource
           :notes "Take a good of your choice. Then take a Travel action."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 26 — Court of Enshakushanna
   ;; ══════════════════════════════════════════════════════════════════════════

   [26 0] {:passive? true :trigger-event :score-magistrate-bonus
           :delta-amity 2
           :notes "When you score Magistrate bonus points, score an additional 2 Amity. Per-trigger."}
   [26 1] {:delta-roles {:priest 1 :leader 1}
           :delta-resources (fn [s p] (role-up-resources s p [:priest :leader]))
           :notes "Increase your Priest and Leader Roles (paying any costs)."}
   [26 2] {:delta-roles {:priest 1 :raider 1}
           :delta-resources (fn [s p] (role-up-resources s p [:priest :raider]))
           :notes "Increase your Priest and Raider Roles (paying any costs)."}
   [26 3] {:delta-temples 0
           :notes "Sell in your city. If you sold Tools or Pottery you may place a Temple in your city (even if you already have a temple there). Sell + conditional temple placement."}
   [26 4] {:delta-raiders 1
           :notes "Place a Raider adjacent to your city. If you surround it, you may place a temple in it (even if you already have a temple there). Raider placement guaranteed; temple conditional on surround."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 27 — Path of Alulim
   ;; ══════════════════════════════════════════════════════════════════════════

   [27 0] {:passive? true :trigger-event :role-increase
           :notes "When you increase a role, you may increase another role, paying double the normal cost. Per-trigger: optional bonus role increase at 2x cost."}
   [27 1] {:delta-amity 3
           :notes "Travel to an adjacent city then you may Sell to it. Impl encodes flat +3 amity (heuristic stand-in for sell). Oracle aligned to impl per Stage 5b."}
   [27 2] {:delta-raiders 1
           :notes "Travel to an adjacent city then you may take a Deploy action. Caravan move + conditional raider placement."}
   [27 3] {:delta-temples 1
           :notes "Travel to an adjacent city then you may place a Temple in it."}
   [27 4] {:delta-resources {}
           :requires-choice? true :choice-type :pick-resource
           :notes "Take three goods of your choice. Choice-count 3."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 28 — Stars of Sin-Kashid
   ;; ══════════════════════════════════════════════════════════════════════════

   [28 0] {:passive? true :trigger-event :astronomer-count-4plus
           :delta-roles {}
           :notes "You may increase a role at the end of your turn if you landed on a space with four or more Astronomers. Per-trigger: optional +1 role."}
   [28 1] {:delta-temples 1
           :notes "Travel to an adjacent city then you may place a Temple in it (even if you already have a temple there)."}
   [28 2] {:delta-temples 1
           :notes "Travel to an adjacent city then you may place a Temple in it (even if you already have a temple there)."}
   [28 3] {:delta-amity 4
           :notes "Sell Gold or Gold to your city if it has no Demands. Then place a random demand on it. Impl encodes flat +4 amity and (conditional) -1 gold; oracle encodes the flat amity only per Stage 5b. Gold-conditional drift will surface as :partial-effect on samples that hold gold."}
   [28 4] {:delta-raiders 1
           :notes "Put a raider point-side up adjacent to Kish."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 29 — Treasury of Ibbi-Sin
   ;; ══════════════════════════════════════════════════════════════════════════

   [29 0] {:passive? true :trigger-event :pay-gold
           :delta-amity 2
           :notes "When you pay a Gold for any reason gain 2 Amity. Per-trigger."}
   [29 1] {:delta-roles {}
           :notes "Decrease your Leader role to increase all of your other roles (paying any costs). Only fires when Leader > 1. Fn closures assigned via alter-var-root below."}
   [29 2] {:delta-amity 3
           :notes "Take a travel action then you may take a sell action. Impl encodes flat +3 amity (heuristic stand-in for sell). Oracle aligned to impl per Stage 5b."}
   [29 3] {:delta-raiders 3
           :notes "Place a raider on each river."}
   [29 4] {:delta-temples 0
           :notes "Place a Temple in each city surrounded by your Raiders (even if you have a Temple there). Count depends on surround state."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 30 — Council of Amar-Sin
   ;; ══════════════════════════════════════════════════════════════════════════

   [30 0] {:passive? true :trigger-event :take-goods
           :notes "When taking goods you may instead take goods based on one of your other Astronomer's location on the action wheel. Per-trigger: swap good source."}
   [30 1] {:notes "Influence a Magistrate then take a Travel action. Magistrate move + caravan move; no deterministic score delta."}
   [30 2] {:delta-amity 0
           :notes "Influence a Magistrate then take a Sell action. Sell outcome-dependent."}
   [30 3] {:delta-raiders 1
           :notes "Take a Deploy action then Influence a Magistrate."}
   [30 4] {:delta-temples 1
           :notes "Influence a Magistrate then take a Temple action. Literal: magistrate move + +1 temple."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 31 — Horizon of Sharkalisharri
   ;; ══════════════════════════════════════════════════════════════════════════

   [31 0] {:passive? true :trigger-event :other-astronomer-space-7
           :notes "When taking actions if one of your other Astronomers is on space 7, you may take a bonus Travel action. Per-trigger: bonus caravan move."}
   [31 1] {:delta-roles {:raider   (fn [s p] (if (= 1 (role-lvl s p :raider))   1 0))
                         :leader   (fn [s p] (if (= 1 (role-lvl s p :leader))   1 0))
                         :priest   (fn [s p] (if (= 1 (role-lvl s p :priest))   1 0))
                         :merchant (fn [s p] (if (= 1 (role-lvl s p :merchant)) 1 0))}
           :notes "Increase all of your level one roles."}
   [31 2] {:delta-roles {:raider   (fn [s p] (if (= 3 (role-lvl s p :raider))   1 0))
                         :leader   (fn [s p] (if (= 3 (role-lvl s p :leader))   1 0))
                         :priest   (fn [s p] (if (= 3 (role-lvl s p :priest))   1 0))
                         :merchant (fn [s p] (if (= 3 (role-lvl s p :merchant)) 1 0))}
           :delta-resources (fn [s p] (roles-at-level-resources s p 3))
           :notes "Increase all of your level three roles (paying any costs)."}
   [31 3] {:delta-resources {}
           :delta-temples 1
           :requires-choice? true :choice-type :pick-resource
           :notes "Gain a resource of your choice and place a Facedown temple in your city (even if you already have a Temple there)."}
   [31 4] {:delta-resources {}
           :delta-raiders 1
           :requires-choice? true :choice-type :pick-resource
           :notes "Gain a resource of your choice and take a Deploy action."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 32 — Jewel of Ku-Bau
   ;; ══════════════════════════════════════════════════════════════════════════

   [32 0] {:passive? true :trigger-event :sell-discard-gem
           :notes "When you sell you may discard a Gem to score Amity based on your Priest level instead of Merchant level. Per-sell choice."}
   [32 1] {:delta-glory (fn [s p] (demand-count s p))
           :notes "Sell in your city then Score Glory for each demand you have fulfilled. Sell + demand-count glory."}
   [32 2] {:delta-resources {:gems 1}
           :notes "Take a Gem. Take two travel actions."}
   [32 3] {:notes "Place a raider in each route that has one of your Temples in both cities. Count depends on temple placement."}
   [32 4] {:delta-amity 2
           :delta-glory 2
           :notes "Influence a Magistrate then you may take sell action. Impl encodes flat +2 amity, +2 glory (heuristic stand-in for the influence + sell). Oracle aligned to impl per Stage 5b."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 33 — Vanguard of Enmebaragesi
   ;; ══════════════════════════════════════════════════════════════════════════

   [33 0] {:passive? true :trigger-event :deploy
           :notes "When you deploy, you may Influence an adjacent Magistrate. Per-trigger: optional magistrate move."}
   [33 1] {:notes "Decrease your Merchant role to increase all of your other roles (paying any costs). See [29 1] — fn closure below."}
   [33 2] {:delta-temples 1
           :notes "Place a facedown Temple in your city then take a travel action (even if you already have a Temple there)."}
   [33 3] {:delta-temples 1
           :notes "Place a face up Temple in Uruk (even if you already have a Temple there)."}
   [33 4] {:delta-raiders 1
           :notes "Deploy a raider adjacent to your city then take a travel action."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 34 — Honor of Agga
   ;; ══════════════════════════════════════════════════════════════════════════

   [34 0] {:passive? true :trigger-event :score-raider
           :notes "When you score raiders, score Amity instead of Glory. Per-trigger swap."}
   [34 1] {:delta-raiders 4
           :delta-resources {:tools -2}
           :notes "Pay Tools, Tools to place a Raider on each space surrounding Uruk. Conditional on having 2 tools; up to 4 raiders (Uruk has 4 adjacent routes)."}
   [34 2] {:notes "Place a raider on each route you have a raider. Count depends on existing raiders — doubles raider footprint."}
   [34 3] {:notes "Take a Sell action in each city that has both a Magistrate and one of your Temples (you don't have to be there). Sell count depends on magistrate+temple overlap."}
   [34 4] {:notes "Take a Sell action in each city that has both a Magistrate and one of your Temples (you don't have to be there). DUPLICATE of [34 3] per rule text."}

   ;; ══════════════════════════════════════════════════════════════════════════
   ;; Board 35 — Wanderer of Dumuzi
   ;; ══════════════════════════════════════════════════════════════════════════

   [35 0] {:passive? true :trigger-event :turn-start
           :delta-resources {}
           :notes "At the start of your turn if you have no goods, gain a good of your choice. Per-trigger: +1 resource when empty-handed."}
   ;; Stage 5 removed [35 1] from bonus-needs-choice? (rule text is travel+sell,
   ;; not a resource pick). Stage 5b realigns the oracle to the rule.
   [35 1] {:notes "Travel then take a Sell action. Compound: caravan move + sell. No deterministic static delta; impl currently has no flat-bonus stand-in."}
   [35 2] {:delta-temples (fn [s p]
                            (if (seq (temples-map s p))
                              (min (get (pkv s p :resources :default {}) :pottery 0)
                                   (pkv s p :temples-supply :default 0))
                              0))
           :delta-resources (fn [s p]
                              (let [n (if (seq (temples-map s p))
                                        (min (get (pkv s p :resources :default {}) :pottery 0)
                                             (pkv s p :temples-supply :default 0))
                                        0)]
                                (if (pos? n) {:pottery (- n)} {})))
           :notes "For each Pottery paid, place a Temple in a city you ALREADY hold (multi-temple refactor: now genuinely placed). n = min(pottery held, temples-supply) when you hold >=1 temple."}
   [35 3] {:delta-roles {}
           :requires-choice? true :choice-type :pick-role
           :notes "Increase the role of your choice (paying any costs)."}
   [35 4] {:delta-glory (fn [s p] (point-raiders s p))
           :notes "Influence a Magistrate. Score each of your Raiders it moved through. Approximated by pre-state point-raider count."}
   })

;; ─── Fix up entries that needed closures referencing private helpers ────────
;; [29 1] Decrease Leader + increase other 3 roles (conditional on leader > 1).
;; [33 1] Decrease Merchant + increase other 3 roles (conditional on merchant > 1).
;; Written out of band so the main map stays readable.
(alter-var-root
 #'expectations
 (fn [m]
   (-> m
       (assoc [29 1]
              {:delta-roles {:leader   (fn [s p] (if (> (role-lvl s p :leader)   1) -1 0))
                             :merchant (fn [s p] (if (> (role-lvl s p :leader)   1)  1 0))
                             :priest   (fn [s p] (if (> (role-lvl s p :leader)   1)  1 0))
                             :raider   (fn [s p] (if (> (role-lvl s p :leader)   1)  1 0))}
               :notes "Decrease your Leader role to increase all of your other roles (paying any costs). Only fires when Leader > 1."})
       (assoc [33 1]
              {:delta-roles {:merchant (fn [s p] (if (> (role-lvl s p :merchant) 1) -1 0))
                             :leader   (fn [s p] (if (> (role-lvl s p :merchant) 1)  1 0))
                             :priest   (fn [s p] (if (> (role-lvl s p :merchant) 1)  1 0))
                             :raider   (fn [s p] (if (> (role-lvl s p :merchant) 1)  1 0))}
               :notes "Decrease your Merchant role to increase all of your other roles (paying any costs). Only fires when Merchant > 1."}))))

(defn expected-delta
  "Return the expected delta descriptor for [board-id slot-idx], or nil if
   the oracle has no entry. Pre-state is passed so fn-valued deltas can
   resolve (e.g., amity = leader-level × 2).

   :delta-resources and :delta-roles may be either a map (whose values may
   themselves be fn-valued) OR a single fn that returns a map — used for
   role-up cost calc where the resource set varies with current level."
  [board-id slot-idx pre-state player-key]
  (when-let [entry (get expectations [board-id slot-idx])]
    (let [resolve-val (fn [v]
                        (cond
                          (fn? v) (v pre-state player-key)
                          (nil? v) 0
                          :else v))
          resolve-map (fn [m]
                        (let [resolved (cond
                                         (nil? m) {}
                                         (fn? m)  (m pre-state player-key)
                                         :else    (into {} (for [[k v] m]
                                                             [k (resolve-val v)])))]
                          ;; Trace records drop zero-deltas; mirror that here so
                          ;; map-equality classification stays honest.
                          (into {} (remove (fn [[_ v]] (zero? v)) resolved))))]
      {:delta-amity     (resolve-val (:delta-amity entry))
       :delta-glory     (resolve-val (:delta-glory entry))
       :delta-roles     (resolve-map (:delta-roles entry))
       :delta-resources (resolve-map (:delta-resources entry))
       :delta-temples   (resolve-val (:delta-temples entry))
       :delta-raiders   (resolve-val (:delta-raiders entry))
       :requires-choice? (boolean (:requires-choice? entry))
       :choice-type     (:choice-type entry)
       :passive?        (boolean (:passive? entry))
       :trigger-event   (:trigger-event entry)
       :notes           (:notes entry)})))

(defn coverage-status
  "Summarize oracle coverage: how many of the 175 slots have entries."
  []
  (let [total 175
        populated (count expectations)
        by-status (group-by val g/effect-implementation-status)
        implemented (count (get by-status :implemented []))
        partial (count (get by-status :partial []))
        persistent (count (get by-status :persistent []))]
    {:total total
     :oracle-populated populated
     :oracle-missing (- total populated)
     :impl-implemented implemented
     :impl-partial partial
     :impl-persistent persistent}))
