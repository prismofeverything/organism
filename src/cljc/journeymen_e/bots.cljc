(ns journeymen-e.bots
  "Journeymen E sim bots: a PURE weighted selector over enumerate-moves (one
   engine path — bots are just players with an automatic picker). Move-level
   feature scoring (the POLICY, Groups A/C/D) + a forward-looking POTENTIAL layer
   (the VALUE, Group B — potential-based shaping, NO tree search; playbook §2/§4)
   + deterministic seeded jitter.

   The genome (~42 genes, `default-genome`/`genome-bounds`) is GA-evolved by
   src/clj/journeymen_e/bench.clj against the frozen `reference-panel`. Every gene
   is an interpretable weight on a feature and every gene is WIRED (see the
   `gene-wiring` test — a dead gene is a bug, playbook §3B). Spec:
   game-ideas/journeymen/journeymen-e-genome.md (Groups A/B/C/D)."
  (:require [journeymen-e.game :as g]))

;; ── genome ────────────────────────────────────────────────────────────────────
;; Group A = static evaluator (policy); Group B = horizon/potential (value);
;; Group C = opponent/meta; Group D = guild/ability (features read the seated
;; guild's identity, so the SAME numbers play blacksmith and goldsmith
;; differently). Defaults reproduce the pre-genome hand-coded constants so the
;; live UI bot is unchanged until the GA supersedes this.
(def default-genome
  {:name "Default"
   ;; Group A — core loop
   :sell-value 3.0 :craft-value 1.6 :token-need 0.8 :worker-value 0.7
   :move-toward-sale 1.2 :move-toward-action 0.5
   ;; Group A — engine loop
   :shop-value 2.0 :shop-demand-fit 1.2 :spread-vs-stack 1.0 :shop-cost-aversion 0.15
   :coin-value 0.35 :coin-anxiety 2.0 :recipe-value 0.8 :slot-discipline 1.0
   :glut-shop-bias 0.0            ; §Provisional: the fn adds a 0.35 BASE FLOOR; this dials on top
   ;; Group A — compounding loop
   :atelier-value 1.8 :master-value 2.5 :master-race-urgency 0.3 :display-value 3.0
   :skilled-value 0.9 :skilled-timing 1.0 :bonus-recipe-value 0.8
   ;; Group A — clock / tracks
   :track-own 1.5 :clock-control 0.8 :clock-dump-bias 0.5 :track-hold 0.4
   ;; Group B — horizon / potential (the fix for "display too late" + monoculture)
   :foresight-dividend 1.0 :foresight-medallion 0.8 :foresight-skill-spread 0.6
   :foresight-track 0.5 :foresight-endgame 1.0 :horizon-depth 1.0
   ;; Group C — opponent / meta
   :opponent-denial 0.5 :worker-target 8 :jitter 0.2
   ;; Group D — guild / ability
   :theme-focus 0.8 :passive-lean 0.8 :home-worker-affinity 0.5
   :ability-econ-pref 1.0 :ability-display-pref 1.0 :ability-tempo-pref 1.0
   :ability-followthrough 1.0
   ;; Group E — NEW-EFFECT levers (2026-07-05 guild/recipe rework; the multi-agent
   ;; review found every new mechanic was a FLAT constant → un-tunable. These make
   ;; each one an evolvable weight.)
   :free-atelier-recipe 1.2       ; value the grey-grey→free-atelier recipe (horizon-shaped)
   :endgame-coin-value 0.5        ; with gold-kicker: coins → end-game points → hoard, don't spend
   :worker-econ 0.3               ; grey-swap / coin-per-good / grey-sweep worker plays
   :lowest-target 0.5             ; choose-lowest bias: theme/medallion vs raw grid
   :offtheme-medallion-pull 1.0   ; guild-conditional: when theme has NO medallion, chase the medallion tracks
   :foresight-clock-feed 0.5      ; value a display on a track that will still receive many feeds (luxury farm)
   :route-efficiency 0.3})        ; positioning: move so my NEXT goals are few cart-moves away (tight loops)

(def genome-bounds
  "Per-key [lo hi] (some :int) clamps (green/Eridu runaway-clamp lesson). Bounds
   per the genome doc's Group A/B/C/D tables."
  {:sell-value [0.5 4]   :craft-value [0 3]     :token-need [0 2]    :worker-value [0 2]
   :move-toward-sale [0 3] :move-toward-action [0 2]
   :shop-value [0 3]     :shop-demand-fit [0 2.5] :spread-vs-stack [-1 2]
   :shop-cost-aversion [0 1] :coin-value [0 1.5] :coin-anxiety [0 3] :recipe-value [0 2]
   :slot-discipline [0 2] :glut-shop-bias [0 1.5]
   :atelier-value [0 3]  :master-value [0 3]    :master-race-urgency [0 2]
   :display-value [0 4]  :skilled-value [0 2]   :skilled-timing [0 2] :bonus-recipe-value [0 2]
   :track-own [0 3]      :clock-control [0 3]   :clock-dump-bias [0 2] :track-hold [0 1.5]
   :foresight-dividend [0 3] :foresight-medallion [0 3] :foresight-skill-spread [0 2]
   :foresight-track [0 2] :foresight-endgame [0 2] :horizon-depth [0 2]
   :opponent-denial [0 2] :worker-target [4 12 :int] :jitter [0 0.5]
   :theme-focus [0 2.5]  :passive-lean [0 2]    :home-worker-affinity [0 1.5]
   :ability-econ-pref [0 2] :ability-display-pref [0 2] :ability-tempo-pref [0 2]
   :ability-followthrough [0 2]
   :free-atelier-recipe [0 3] :endgame-coin-value [0 2] :worker-econ [0 2]
   :lowest-target [0 1] :offtheme-medallion-pull [0 2] :foresight-clock-feed [0 2]
   :route-efficiency [0 2]})

(def genome-keys (vec (keys genome-bounds)))

;; ability TYPE tags (genome doc §4b): the :pick-ability score reads the matching
;; ability-<type>-pref gene, so a genome's atelier-unlock preference is evolvable.
(def ability-type
  {:shop-refund :econ :recipe-coinback :econ :gold-kicker :econ :cheap-adjacent :econ
   :shop-payout :econ :optional-sale-coin :econ
   :display-up-innovation :display :display-gain-luxury :display :display-at-shops :display
   :grey-sweep :tempo :two-free-shops :tempo :skilled-affinity :tempo})

;; ── archetypes (the FROZEN reference panel — never evolved) ───────────────────
;; The 6 named corners + the 2 panel adversaries the GA must BEAT (Dividend-farmer,
;; Denier) + Harvester and Themer so every diversity region is seeded.
(def archetypes
  [(merge default-genome {:name "Stallkeeper" :shop-value 3.2 :shop-demand-fit 2.2 :sell-value 3.4 :atelier-value 1.2})
   (merge default-genome {:name "Craftlord"  :master-value 3.0 :display-value 4.0 :atelier-value 2.6 :craft-value 2.0 :sell-value 2.4 :foresight-dividend 2.0})
   (merge default-genome {:name "Banker"     :coin-value 1.2 :recipe-value 1.4 :sell-value 2.6 :token-need 1.2 :coin-anxiety 2.6})
   (merge default-genome {:name "Sprinter"   :clock-control 2.5 :sell-value 3.6 :track-own 2.5 :craft-value 1.9 :foresight-track 1.5})
   (merge default-genome {:name "Generalist"})
   (merge default-genome {:name "Recluse"    :shop-value 1.0 :craft-value 2.4 :token-need 1.5 :sell-value 3.2})
   ;; panel adversaries + region seeds
   (merge default-genome {:name "Dividend-farmer" :foresight-dividend 3.0 :horizon-depth 2.0
                          :ability-display-pref 2.0 :display-value 4.0 :atelier-value 2.6 :master-value 3.0})
   (merge default-genome {:name "Denier"     :opponent-denial 2.0 :master-race-urgency 2.0 :foresight-dividend 1.0})
   (merge default-genome {:name "Harvester"  :glut-shop-bias 1.5 :worker-value 1.8 :shop-value 2.6 :worker-target 12})
   (merge default-genome {:name "Themer"     :theme-focus 2.5 :passive-lean 2.0 :home-worker-affinity 1.5
                          :foresight-medallion 1.6})])

(def reference-panel
  "FROZEN adversarial panel for the bench (never evolved). Candidate plays seat 0;
   these fill the rest by seed. Fitness = absolute score vs THESE, not self-play."
  (vec archetypes))

;; ── shared feature helpers ────────────────────────────────────────────────────
(defn- sale-value [state p goods]
  (reduce + 0 (for [[gd n] goods]
                (* n (g/grid-value (:guild p) gd (get-in p [:skills gd]))))))
(defn- leading? [state pid]
  (let [scores (map :score (:players state))]
    (= (:score (nth (:players state) pid)) (apply max scores))))
(defn- own-displays-on [state pid gd]
  (count (filter #(= pid (:pid %)) (get-in state [:displays gd] []))))
(defn- opp-displays-on [state pid gd]
  (count (filter #(not= pid (:pid %)) (get-in state [:displays gd] []))))
(defn- demand-fit
  "How well a node's demand matches this player's strong skills + held tokens."
  [state p node]
  (let [{:keys [req opt]} (:demand (nth g/spaces node))]
    (+ (get-in p [:skills req]) (* 0.5 (get-in p [:skills (or opt req)]))
       (get-in p [:tokens req] 0))))
(defn- moves-to
  "Directed distance in CART MOVES from node a to node b over the two-track ROAD
   graph (g/road-distance); each move covers 1-2 roads, so moves = ceil(rd/2).
   4 = unreachable fallback (can't happen)."
  [state a b]
  (if-let [d (g/road-distance state a b)] (quot (inc d) 2) 4))
(defn- nearest-sale-distance
  "Directed moves from `dest` to the closest node where I could SELL. 4 = nothing."
  [state p dest]
  (let [pid (:id p)
        sellable (for [n (range (count g/spaces))
                       :when (and (g/player-shop-at? state pid n)
                                  (g/demand-met? (get-in g/spaces [n :demand]) (:tokens p)))]
                   n)]
    (if (seq sellable) (apply min (map #(moves-to state dest %) sellable)) 4)))

;; ── route-distance matrix (Mohammad 2026-07-05): the full pairwise shortest-route
;; in CART MOVES for THIS game's wedge configuration, so routing weights adapt to how
;; the wedges shuffled (a space that's 1 move away in one config is 3 in another) ──
(def ^:private route-cache (atom {}))
(defn move-matrix
  "{[a b] cart-moves} for every ordered pair, memoised on the road graph (:adj)."
  [state]
  (let [adj (:adj state) n (count g/spaces)]
    (or (@route-cache adj)
        (let [m (into {} (for [a (range n) b (range n)] [[a b] (moves-to state a b)]))]
          (swap! route-cache assoc adj m) m))))
(defn- mmoves [state a b] (get (move-matrix state) [a b] 4))
(defn- goal-nodes
  "The spaces I'll want to reach NEXT — my sale outlets (shops) + my display venue
   (an atelier while I hold a mastercraft). Positioning near these = a tight loop."
  [state p]
  (let [pid (:id p)]
    (into (vec (for [n (range (count g/spaces)) :when (g/player-shop-at? state pid n)] n))
          (when (pos? (:mastercrafts-built p))
            (for [n (range (count g/spaces)) :when (g/player-atelier-at? state pid n)] n)))))

(defn pay-preference
  "PAYMENT-CHOICE heuristic (green 2026-06-29 rule): avoid spending SKILLED tokens
   (persistent value) UNLESS a matching-colour REGULAR worker is pooled at the
   node (the spend-grab makes it free value: prefer it). Scaled by :skilled-timing
   (promoted constant: the GA dials how deliberately to trigger the ★ spend-grab)."
  [state p node pay genome]
  (* (:skilled-timing genome 1.0)
     (reduce + 0.0
             (for [w pay :when (g/skilled? w)]
               (if (some #(and (not (g/skilled? %)) (= (g/base-color w) %))
                         (get-in state [:board-workers node] []))
                 0.5 -0.7)))))

(defn- home-white-count
  "How many of `pay` are home-colour or white workers (cheap-for-this-guild)."
  [p pay]
  (count (filter #(let [c (g/base-color %)] (or (= c (g/home-color p)) (= c :white))) pay)))

;; ── Group B: the horizon / potential layer (φ — value, not policy) ────────────
(defn- rounds-remaining
  "Cheap horizon proxy: the game ends when a 2nd track hits threshold (then one
   more round, canon R2-H). Estimate from the token gap of the two FULLEST tracks
   (~1 tracked sale/turn). Floored at 1; forced to 1 once :ending fired. This is
   what makes an EARLY display high-value: rounds-remaining is large early, so its
   whole future dividend stream is priced in NOW (fixes 'displays too late')."
  [state]
  (if (:ending state)
    1
    (let [gaps (sort (for [[g n] (:tracks state)] (max 0 (- (g/track-threshold g) n))))
          need (+ (first gaps) (or (second gaps) 0))]
      (max 1 (quot (+ need 3) 2)))))
(defn- own-max-skill [p] (apply max (map (:skills p) g/dpi)))
(defn- medallion-signal
  "Forward value of raising `good`'s skill by 1: 1.0 if it LANDS on a ◆ medallion
   cell (unlocks a master recipe → the master→display→dividend chain), 0.4 if it
   reaches ONE BELOW a medallion. 0 otherwise (reads g/guilds :medallions)."
  [p good]
  (let [meds (get-in g/guilds [(:guild p) :medallions])
        nx (inc (get-in p [:skills good] 1))]
    (cond (contains? meds [good nx]) 1.0
          (contains? meds [good (inc nx)]) 0.4
          :else 0.0)))
(defn- spread-signal
  "How much raising `good` escapes durability monoculture = its gap below my TOP
   skill (a laggard track is worth climbing — opens more demands + medallions)."
  [p good]
  (max 0 (- (own-max-skill p) (get-in p [:skills good] 1))))

;; ── glut-shop-bias (economy hygiene; Mohammad 2026-07-04) ─────────────────────
(defn glut-shop-bias
  "Workers are CONSERVED (24 + skilled) and spent workers drop where activated, so
   a high-traffic space FLOODS while the rest starves — a dead pool nobody
   harvests. Bias shop placement toward the excess-over-average a space holds,
   strongest when it has NO shops (placing one taps the dead pool). BASE FLOOR
   0.35 (always-on hygiene — no evolved lineage can abandon it) + the :glut-shop-bias
   genetic dial. The horizon layer + GA should breed a labour-harvester on top."
  [state node genome]
  (let [bw (:board-workers state)
        total (reduce + 0 (map count (vals bw)))
        here (count (get bw node []))
        excess (max 0.0 (- here (/ total 8.0)))
        dead-pool? (empty? (get-in state [:board-shops node]))
        strength (+ 0.35 (:glut-shop-bias genome 0.0))]
    (* strength excess (if dead-pool? 1.0 0.25))))

;; ── score-move — the choice surface valuation ─────────────────────────────────
(declare pickup-score)
(defn score-move [state genome move]
  (let [p (g/current-player state) pid (:id p)
        hd (:horizon-depth genome 1.0)
        rr (rounds-remaining state)
        theme (get-in g/guilds [(:guild p) :theme])
        ;; coin-poverty pressure (empty purse → luxury/coin moves urgent), dialed
        ;; by :coin-anxiety (promoted constant, was the hand-coded 2.0/1.2).
        poor (max 0.0 (/ (- 4 (:coins p)) 4.0))]
    (case (:type move)
      :place-cart 0.5
      :move-cart
      (let [dest (:dest move)
            d (nearest-sale-distance state p dest)
            act (:action (nth g/spaces dest))
            act-useful (case act
                         :shop (if (pos? (:shops-left p)) 1 0)
                         (:atelier-pts :atelier-coin)
                         (cond (and (pos? (:ateliers-left p))
                                    (or (pos? (:master-recipes p))
                                        (pos? (:mastercrafts-built p)))) 2.0
                               (pos? (:ateliers-left p)) 1
                               :else 0.2)
                         (:recipe-coin :recipe-worker) 0.5
                         :skilled 0.4)
            ;; FORESIGHT-DIVIDEND (routing): heading to MY atelier with a masterwork OR a
            ;; master-recipe in hand advances the display → dividend stream (ungated per review)
            route-div (if (and (or (pos? (:mastercrafts-built p)) (pos? (:master-recipes p)))
                               (g/player-atelier-at? state pid dest))
                        (* hd (:foresight-dividend genome 0.0) rr)
                        0.0)
            ;; OFF-THEME MEDALLION routing: a dest whose sale climbs a MEDALLION track for
            ;; this guild — extra pull when the THEME track carries no medallion (blacksmith)
            dest-good (get-in g/spaces [dest :demand :req])
            theme-medalled? (some #(= theme (first %)) (get-in g/guilds [(:guild p) :medallions]))
            route-med (if (keyword? dest-good)
                        (* hd (medallion-signal p dest-good)
                           (+ (:foresight-medallion genome 0.0)
                              (if theme-medalled? 0.0 (:offtheme-medallion-pull genome 0.0))))
                        0.0)
            ;; ROUTE-EFFICIENCY: using the pairwise cart-moves matrix for THIS wedge
            ;; config, reward a dest that leaves my next goals few moves away (tight loop)
            goals (goal-nodes state p)
            onward (if (seq goals) (apply min (map #(mmoves state dest %) goals)) 2)
            route-eff (* (:route-efficiency genome 0.0) (- 2 onward))]
        (+ (* (:move-toward-sale genome) (- 3 d))
           (* (:move-toward-action genome) act-useful)
           route-div route-med route-eff
           (* 0.2 (count (get-in state [:board-workers dest])))))
      :pickup       (pickup-score state p genome (:color move) 0.0)
      :pickup-adjacent (pickup-score state p genome (:color move) -0.3)
      :skip-pickup 0.0
      :sell
      (let [goods (:goods move) v (sale-value state p goods)
            t (:track move)
            total-tok (reduce + 0 (vals goods))
            own (if t (own-displays-on state pid t) 0)
            fill (if t (get-in state [:tracks t]) 0)
            clock-urge (if (leading? state pid) (:clock-control genome) (- (* 0.3 (:clock-control genome))))
            lux-coins (* (get goods :luxury 0)
                         (or (g/grid-value (:guild p) :luxury (get-in p [:skills :luxury] 1)) 0))
            ;; FORESIGHT: selling levels each sold good's skill → medallion + spread
            fmed (* hd (:foresight-medallion genome 0.0)
                    (reduce + 0.0 (map #(medallion-signal p %) (keys goods))))
            fspread (* hd (:foresight-skill-spread genome 0.0)
                       (reduce + 0.0 (map #(spread-signal p %) (keys goods))))
            ;; FORESIGHT-TRACK: leading → push toward the end; trailing → don't
            ;; hand the game its 2nd filled track
            ftrack (if t
                     (* hd (:foresight-track genome 0.0)
                        (cond (and (>= (inc fill) (g/track-threshold t)) (not (leading? state pid))) -1.0
                              (leading? state pid) 0.5 :else 0.0))
                     0.0)
            fend (if (:ending state) (* hd (:foresight-endgame genome 0.0) total-tok) 0.0)
            ;; Group D — theme (jeweler=generalist → spread instead) + passive
            theme-term (cond
                         (= theme :generalist) (* (:theme-focus genome 0.0)
                                                  (reduce + 0.0 (map #(spread-signal p %) (keys goods))))
                         (contains? goods theme) (* (:theme-focus genome 0.0) (get goods theme))
                         :else 0.0)
            passive-term (* (:passive-lean genome 0.0)
                            ;; luxury-sale-point steers the tracked-copy CHOICE toward luxury
                            (+ (if (and (g/unlocked? p :luxury-sale-point) (= t :luxury)) 1 0)
                               (if (and (g/unlocked? p :innovation-boost) (:boost move)) 1 0)
                               (if (and (g/unlocked? p :sell-three-luxury) (>= total-tok 3)) 1 0)))
            ;; Group C — opponent-denial: avoid feeding a track opponents out-display me
            denial (if (and t (> (opp-displays-on state pid t) own))
                     (- (* (:opponent-denial genome 0.0) (- (opp-displays-on state pid t) own)))
                     0.0)
            ;; clock-dump-bias: burn the tracked copy onto a good I DON'T farm =
            ;; one far below my top skill (protect my key good's supply; the doc's
            ;; evolvable "protect vs dump", vs today's everyone-dumps-luxury)
            dump (if t (* (:clock-dump-bias genome 0.0) (spread-signal p t) 0.3) 0.0)
            ;; track-hold: seed the EMPTIEST track (spread the clock) rather than
            ;; rush a nearly-full one — the counterweight to clock-control
            hold (if t (* (:track-hold genome) (- 1.0 (/ (double fill) 6.0))) 0.0)
            ;; OFF-THEME MEDALLION (blacksmith feed-back 2026-07-05): when this guild's
            ;; medallions sit OFF its theme, add extra pull to sell (=climb) an off-theme
            ;; medallion good — the diversify-for-medallions line the myopia underrated
            theme-medalled? (some #(= theme (first %)) (get-in g/guilds [(:guild p) :medallions]))
            offtheme-med (if theme-medalled? 0.0
                           (* hd (:offtheme-medallion-pull genome 0.0)
                              (reduce + 0.0 (for [g (keys goods)
                                                  :when (some #(= g (first %)) (get-in g/guilds [(:guild p) :medallions]))]
                                              (medallion-signal p g)))))]
        (+ (* (:sell-value genome) v)
           (* (:track-own genome) own)
           (if t (* clock-urge (/ (inc fill) 6.0)) 0.0)
           (* (:coin-anxiety genome) poor (get goods :luxury 0))
           (* (:coin-value genome) lux-coins)
           (if (:boost move) 0.6 0.0)
           hold offtheme-med
           fmed fspread ftrack fend theme-term passive-term denial dump))
      :display
      (let [t (:track move)
            room (- (+ (g/track-threshold t) 2) (get-in state [:tracks t]))
            own (own-displays-on state pid t)
            ;; immediate grid score of this display (per-track — steers WHICH track)
            pts (or (g/grid-value (:guild p) t (get-in p [:skills t] 1)) 0)
            ;; FORESIGHT-DIVIDEND: the whole future stream = rounds-left × (displays
            ;; on this track incl. this one). This is what makes an EARLY display,
            ;; not a late one, the high-value move (subsumes the old flat pull).
            fdiv (* hd (:foresight-dividend genome 0.0) rr (inc own))
            fmed (* hd (:foresight-medallion genome 0.0)
                    (+ (if (g/unlocked? p :display-skill-first) (medallion-signal p t) 0.0)
                       (if (g/unlocked? p :display-up-innovation) (medallion-signal p :innovation) 0.0)))
            fend (if (:ending state) (* hd (:foresight-endgame genome 0.0) 1.0) 0.0)
            passive-term (* (:passive-lean genome 0.0)
                            (+ (if (g/unlocked? p :display-skill-first) 1 0)
                               (if (g/unlocked? p :display-up-innovation) 1 0)
                               (if (g/unlocked? p :display-gain-luxury) 1 0)))
            follow (if (g/unlocked? p :display-at-shops) (:ability-followthrough genome 0.0) 0.0)
            theme-term (if (= t theme) (:theme-focus genome 0.0) 0.0)
            ;; FORESIGHT-CLOCK-FEED: a display on a track that will still receive many
            ;; feeds (a fresh/luxury track) harvests future dividends (the luxury-farm line)
            fclock (* (:foresight-clock-feed genome 0.0)
                      (max 0 (- (+ (g/track-threshold t) 2) (get-in state [:tracks t] 0))))]
        (+ (* (:display-value genome) pts) (* 0.6 room) (* 0.4 own)
           fdiv fmed fend passive-term follow theme-term fclock))
      :action-shop
      (if (:place? move)
        (let [node (:node move)
              lux? (= :luxury (get-in g/spaces [node :demand :req]))
              already? (g/player-shop-at? state pid node)
              others? (some #(and (not= pid %) (seq (get-in state [:board-shops node %])))
                            (map :id (:players state)))
              follow (if (g/unlocked? p :shop-refund) (:ability-followthrough genome 0.0) 0.0)]
          (+ (:shop-value genome) (* (:shop-demand-fit genome) (* 0.2 (demand-fit state p node)))
             (* (:coin-anxiety genome) poor (if lux? 1 0))
             ;; spread-vs-stack: >0 widen to new spaces, <0 stack (was ±1.0/0.6)
             (if already? (- (:spread-vs-stack genome)) (:spread-vs-stack genome))
             (glut-shop-bias state node genome)
             (if others? (* 0.3 (:opponent-denial genome 0.0)) 0.0)   ; contest a shared space
             follow
             ;; FORESIGHT-ENDGAME: late building can't pay off — don't invest now
             (if (:ending state) (- (* hd (:foresight-endgame genome 0.0) 2.0)) 0.0)
             (- (* (:shop-cost-aversion genome) (g/build-cost state p)))))
        (* 0.4 (:worker-value genome)))
      :shop-worker (+ (:worker-value genome)
                      ;; grey-sweep: taking a grey vacuums the whole grey pool here
                      (if (and (g/unlocked? p :grey-sweep) (= :grey (g/base-color (:color move))))
                        (* (:worker-econ genome 0.0)
                           (count (filter #(= :grey (g/base-color %))
                                          (get-in state [:board-workers (:from move)]))))
                        0.0))
      :skip-shop-worker 0.0
      :action-atelier
      (+ ;; atelier-value, weighted by the demand-fit of the basic being upgraded
         ;; (steers WHICH shop becomes the venue) + coin bonus (:atelier-coin = +3c)
         (* (:atelier-value genome) (+ 1.0 (* 0.1 (demand-fit state p (:basic-node move)))))
         (* (:coin-value genome) (if (= :atelier-coin (:kind move)) 3 1))
         ;; FORESIGHT-DIVIDEND: the venue unlocks the display chain (was 1.5×)
         (* hd (:foresight-dividend genome 0.0) rr (+ (:master-recipes p) (:mastercrafts-built p)))
         (* 0.4 (:opponent-denial genome 0.0))   ; ateliers are a contested slot
         (if (:ending state) (- (* hd (:foresight-endgame genome 0.0) 2.0)) 0.0))
      :action-recipe
      (+ (- (:recipe-value genome) (if (= (:pay move) :coins) 0.2 0.1))
         ;; coin-value: paying COINS for a recipe spends precious purse (a hoarder
         ;; prefers the worker-pay option or skips) — the coin-in-hand standing value
         (if (= (:pay move) :coins)
           ;; coinback atelier makes the net coin cost 1 (2 out, 1 back); with gold-kicker
           ;; every coin is a future point, so spending hurts more (hoard incentive)
           (- (* (+ (:coin-value genome) (if (g/unlocked? p :gold-kicker) (:endgame-coin-value genome 0.0) 0.0))
                 (if (g/unlocked? p :recipe-coinback) 1 2)))
           0.0)
         ;; SLOT discipline (promoted constant): prefer EMPTY slots, place chains
         ;; right of a craftable non-chain. Scaled by :slot-discipline.
         (* (:slot-discipline genome 1.0)
            (let [j (:slot move)
                  slots (:recipe-slots p)
                  taken (nth (:recipe-market state) (:idx move))
                  covered (get slots j)
                  left (when (and j (pos? j)) (get slots (dec j)))]
              (+ (cond (nil? covered) 0.6
                       (contains? (:flags covered #{}) :chain) -0.2
                       :else -0.9)
                 (if (and (contains? (:flags taken #{}) :chain)
                          left (not (contains? (:flags left #{}) :chain)))
                   0.5 0.0))))
         (if (and (= (:pay move) :worker) (:color move))
           (let [c (:color move)]
             (cond (g/skilled? c) -0.6
                   (= c :grey) 0.2
                   (#{(g/home-color p) :white} c) -0.3
                   :else 0.1))
           0.0))
      ;; skilled-value, weighted toward a HOME-colour skilled worker (steers WHICH
      ;; colour to claim — home colour is the cheapest persistent fuel)
      :action-skilled (* (:skilled-value genome)
                         (+ 1.0 (if (= (:color move) (g/home-color p)) 0.5 0.0)))
      :skip-main -0.2
      (:craft :fire-chain)
      (let [r (get-in p [:recipe-slots (:slot move)])
            flags (:flags r #{})
            outs (:outputs r)
            need (reduce + 0 (for [gd outs]
                               (* (:token-need genome)
                                  (g/grid-value (:guild p) gd (get-in p [:skills gd])))))
            lux-out (count (filter #{:luxury} outs))
            node (get-in p [:carts (:active-cart state)])
            fmed (* hd (:foresight-medallion genome 0.0)
                    (reduce + 0.0 (map #(medallion-signal p %) outs)))
            fspread (* hd (:foresight-skill-spread genome 0.0)
                       (reduce + 0.0 (map #(spread-signal p %) outs)))
            theme-term (if (some #{theme} outs) (* (:theme-focus genome 0.0) (count (filter #{theme} outs))) 0.0)
            hwa (* (:home-worker-affinity genome 0.0) (home-white-count p (:pay move)))
            ;; NEW flag cards — all genome-weighted now (was flat constants). free-atelier
            ;; is horizon-shaped (dividend-engine unlock) and worthless without a basic to upgrade.
            has-basic? (some (fn [[_ m]] (some #(= :basic (:level %)) (get m pid))) (:board-shops state))
            flag-bonus (+ (if (and (contains? flags :free-atelier) has-basic?)
                            (* (:free-atelier-recipe genome 0.0)
                               (+ 1.0 (* hd (:foresight-dividend genome 0.0) rr))) 0.0)
                          (if (contains? flags :coin-per-good)
                            (* (:worker-econ genome 0.0)
                               (count (filter (fn [[g c]] (and (pos? c) (#{:durability :precision :innovation} g)))
                                              (:tokens p)))) 0.0)
                          (if (contains? flags :grey-swap) (:worker-econ genome 0.0) 0.0))]
        (+ (:craft-value genome) (* 0.3 need) (* (:coin-anxiety genome) poor lux-out)
           (if (some flags [:bonus-skill :lowest]) (:bonus-recipe-value genome) 0.0)
           (pay-preference state p node (:pay move) genome)
           fmed fspread theme-term hwa flag-bonus))
      :decline-chain 0.0
      :craft-master
      (+ (:master-value genome)
         (* (:master-race-urgency genome) (- 3 (count (get g/master-cost (:id move) []))))
         (pay-preference state p (get-in p [:carts (:active-cart state)]) (:pay move) genome)
         ;; FORESIGHT-DIVIDEND: a masterwork is a future display stream
         (* hd (:foresight-dividend genome 0.0) rr 1.0)
         (* (:opponent-denial genome 0.0) 1.0)   ; claiming REMOVES the token for everyone
         (* (:home-worker-affinity genome 0.0) (home-white-count p (:pay move))))
      :place-free-shop
      (+ (* 0.8 (:shop-value genome)) (* 0.2 (demand-fit state p (:node move))))
      :skip-free-shop 0.0
      :pick-ability
      ;; genome-weighted by ability TYPE (§4b) + situational kicker
      (let [typ (ability-type (:id move))
            pref (case typ
                   :econ (:ability-econ-pref genome 1.0)
                   :display (:ability-display-pref genome 1.0)
                   :tempo (:ability-tempo-pref genome 1.0)
                   1.0)
            kicker (case (:id move)
                     ;; gold-kicker's value = your coins become end-game points → weight
                     ;; by the evolvable endgame-coin-value (the hoard-to-win incentive)
                     :gold-kicker (* (+ 0.25 (:endgame-coin-value genome 0.0)) (:coins p))
                     :grey-sweep (* (:worker-econ genome 0.0) 2.0)
                     :shop-payout (* 0.4 (g/total-player-shops state pid))
                     :two-free-shops (* (:free-atelier-recipe genome 0.0) (:shop-value genome))
                     :display-at-shops (* 0.4 (+ (:master-recipes p) (:mastercrafts-built p)))
                     0.3)]
        (+ pref kicker))
      :skip-ability 0.0
      ;; :lowest tie — prefer raising the THEME skill, else the one with the best next-rank grid
      :choose-lowest
      ;; bias toward the THEME skill and skills NEAR a medallion cell, tunable
      (let [g (:skill move) rank (inc (get-in p [:skills g]))]
        (+ (* (:lowest-target genome 0.0) (if (= g theme) 1.0 0.0))
           (* (:lowest-target genome 0.0) (medallion-signal p g))
           (* 0.1 (or (g/grid-value (:guild p) g rank) 0))))
      ;; a FREE atelier upgrade — a display venue + an ability unlock (evolvable)
      :free-atelier (+ (* (:free-atelier-recipe genome 0.0) (:shop-value genome))
                       (* hd (:foresight-dividend genome 0.0) rr)
                       (* 0.2 (demand-fit state p (:basic-node move)))
                       (:ability-followthrough genome 0.0))
      :skip-free-atelier 0.0
      :skip-craft 0.0
      0.0)))

(defn- pickup-score
  "Worker acquisition value: :worker-value, dialed by :worker-target (stockpile
   discipline — below target keeps grabbing, at/above it discourages hoarding) and
   :home-worker-affinity (a home/white worker is cheap fuel for this guild).
   `base-adj` offsets the pay-1-coin adjacent variant (-0.3)."
  [state p genome color base-adj]
  (let [have (count (:workers p))
        target (:worker-target genome 8)
        ;; below target: keep grabbing; at/above: a real hoarding penalty (spend down)
        stock (if (< have target) 0.4 -1.0)
        c (g/base-color color)
        affin (if (or (= c (g/home-color p)) (= c :white)) (:home-worker-affinity genome 0.0) 0.0)
        follow (if (and (neg? base-adj) (g/unlocked? p :cheap-adjacent))
                 (:ability-followthrough genome 0.0) 0.0)]   ; cheap-adjacent → route adjacents
    (+ (:worker-value genome) base-adj stock affin follow)))

;; ── diversity region tag (genome doc §5) ──────────────────────────────────────
(defn- gnorm
  "Gene value normalised to [0,1] by its bounds — so levers built from
   different-scaled genes are COMPARABLE (else the high-default crafter lever
   swamps every genome → 1-region collapse, the GA's monoculture trap)."
  [gm k]
  (let [[lo hi] (get genome-bounds k [0 1])]
    (if (> hi lo) (max 0.0 (min 1.0 (/ (- (get gm k lo) lo) (double (- hi lo))))) 0.0)))

(defn region-of
  "Coarse strategy tag by DOMINANT lever (each lever = MEAN of its genes'
   bound-normalised values, so all levers live on [0,1]), for diversity-aware
   selection. §5: stall / crafter / banker / sprinter / denier / spreader /
   themer / harvester."
  [gm]
  (let [m (fn [& ks] (/ (reduce + (map #(gnorm gm %) ks)) (count ks)))
        levers {:stall     (m :shop-value :sell-value)
                :crafter   (m :master-value :display-value :foresight-dividend)
                :banker    (m :coin-value :coin-anxiety :endgame-coin-value)
                :sprinter  (m :clock-control :foresight-track)
                :denier    (m :opponent-denial)
                :spreader  (m :foresight-skill-spread :foresight-medallion)
                :themer    (m :theme-focus :passive-lean)
                :harvester (m :glut-shop-bias :worker-econ)
                :ability   (m :ability-econ-pref :ability-display-pref :ability-tempo-pref
                              :ability-followthrough :free-atelier-recipe)}]
    (key (apply max-key val levers))))

(defn decide [state genome]
  (let [moves (g/enumerate-moves state)]
    (when (seq moves)
      (let [seed (+ (* 131 (long (:turn state 0))) (long (:current state 0)) 17)
            jit (:jitter genome 0.2)]
        (->> moves
             (map-indexed (fn [i m]
                            [(+ (score-move state genome m)
                                ;; seeded jitter, magnitude = :jitter gene (was /500 const)
                                (* jit (/ (double (mod (g/prng-next (+ seed (* 7 i))) 97)) 97.0)))
                             m]))
             (apply max-key first)
             second)))))

(defn play-game-seats
  "Seat i uses (nth genomes i). Returns the final state."
  [seed picks genomes]
  (loop [st (g/new-game seed picks) guard 0]
    (if (or (g/game-over? st) (> guard 100000))
      st
      (let [gm (nth genomes (mod (:current st) (count genomes)))]
        (recur (g/apply-move st (decide st gm)) (inc guard))))))
(defn play-game
  ([seed picks] (play-game seed picks default-genome))
  ([seed picks genome]
   (if (vector? genome)
     (play-game-seats seed picks genome)
     (play-game-seats seed picks (vec (repeat (count picks) genome))))))
