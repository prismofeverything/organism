(ns journeymen-e.game
  "Journeymen E — the action+demand restructure. SIM v1 engine.
   Spec: game-ideas/journeymen/journeymen-e.md (round 3, Mohammad-annotated canon +
   §Provisional build decisions). ONE engine path: enumerate-moves + apply-move for
   bots and any future UI alike. v3 (2026-07-02 playtest feedback): green's real
   recipe deck (eras A→B→C, chains/free-shops/skill flags adapted), the shared
   master-pool tableau (build = a race for a specific token), colour-constrained
   shop-worker pickups (:take sets), and guild passives + atelier abilities
   (E-adapted, §Provisional). v4 (2026-07-03): the designer's location↔action↔
   demand PAIRING TABLE — see `spaces` and `wedges` (no-outer Academy wedge; the
   6 D/P/I demands are the complete directed-K3 pair set). Remaining
   simplification: skilled workers keep green's marked spend-grab semantics."
  (:require [clojure.string :as str]))

;; ── deterministic PRNG (Park–Miller, as base journeymen) ─────────────────────
(defn prng-next [s] (mod (* s 48271) 2147483647))
(defn vec-index-of
  "Portable first-index of x in vector v, or -1 (cljc: no JVM .indexOf hinting)."
  [v x]
  (loop [i 0] (cond (>= i (count v)) -1
                    (= (nth v i) x) i
                    :else (recur (inc i)))))
(defn remove-n
  "Remove up to n occurrences of x from vector v."
  [v x n]
  (loop [v (vec v) k n]
    (if (zero? k) v
        (let [i (vec-index-of v x)]
          (if (neg? i) v
              (recur (vec (concat (subvec v 0 i) (subvec v (inc i)))) (dec k)))))))
(defn- seeded-shuffle [seed xs]
  (loop [v (vec xs) i (dec (count v)) s (inc (mod (long seed) 2147483646))]
    (if (<= i 0) v
        (let [s' (prng-next s) j (mod s' (inc i))]
          (recur (assoc v i (v j) j (v i)) (dec i) s')))))

;; ── goods / skills / workers ─────────────────────────────────────────────────
(def good-types [:durability :precision :innovation :luxury])
(def dpi [:durability :precision :innovation])
(def primary-colors [:black :blue :yellow :red])
;; SKILLED workers are persistent MARKED tokens (:skilled-black etc — green
;; semantics): they pay and are taken as their base colour; when SPENT they drop
;; still marked and the spender grabs a matching-colour REGULAR pooled there.
(def skilled-of {:black :skilled-black :blue :skilled-blue
                 :yellow :skilled-yellow :red :skilled-red})
(def base-of {:skilled-black :black :skilled-blue :blue
              :skilled-yellow :yellow :skilled-red :red})
(defn skilled? [w] (contains? base-of w))
(defn base-color [w] (get base-of w w))
(def good-supply {:durability 10 :precision 10 :innovation 10 :luxury 12})
(def track-threshold {:durability 5 :precision 5 :innovation 5 :luxury 6})
(def track-overfill 2)          ; a filled track accepts +2 more, then sales stop tracking

;; ── guilds (grids from the C boards; medallions §Provisional) ─────────────────
;; :passive is ALWAYS ON; :ateliers are 3 abilities unlocked ONE PER ATELIER BUILT
;; (player's choice via the :ability pending step). All effects are §Provisional
;; E-adaptations of green's guild boards (ids kept stable for cross-referencing);
;; green's :gain-master-recipe atelier is GONE in E (medallions are the only source),
;; so each guild gets a third E-specific ability instead.
(def guilds
  {:blacksmith {:color :black  :theme :durability :pay-rule :primary
                :grid {:durability [2 3 4 5 7 8] :precision [1 2 3 4 5 6]
                       :innovation [1 2 3 4 5 6] :luxury [2 2 2 3 4 5]}
                :medallions #{[:precision 3] [:innovation 4] [:innovation 6]}
                :passive {:id :display-skill-first
                          :text "When you DISPLAY a mastercraft, raise that track's skill +1 BEFORE scoring the display."}
                :ateliers [{:id :optional-sale-coin
                            :text "When you sell, if you sold an OPTIONAL good (any not required by the space): +1 coin."}
                           {:id :grey-sweep
                            :text "When you pick up a grey worker from a space, take ALL grey workers from that space."}
                           {:id :shop-refund
                            :text "+2 coins whenever you place a basic shop (incl. Parks)."}]}
   :alchemist  {:color :blue   :theme :innovation :pay-rule :primary
                :grid {:durability [1 2 3 4 4 5] :precision [1 2 3 3 4 5]
                       :innovation [0 2 4 5 7 9] :luxury [2 2 2 3 4 7]}
                :medallions #{[:innovation 2] [:durability 4] [:precision 4]}
                :passive {:id :innovation-boost
                          :text "When selling, you may discard a held Innovation token: each OTHER good TYPE sold (not Innovation) levels its skill a SECOND time."}
                :ateliers [{:id :two-free-shops
                            :text "On unlocking: immediately place up to 2 shops FREE (you choose the spaces)."}
                           {:id :display-up-innovation
                            :text "When you display a mastercraft: +1 Innovation skill."}
                           {:id :recipe-coinback
                            :text "After taking a recipe (either recipe space): +1 coin."}]}
   :goldsmith  {:color :yellow :theme :luxury :pay-rule :primary
                :grid {:durability [1 2 3 4 5 6] :precision [1 2 4 5 6 7]
                       :innovation [1 2 4 5 6 7] :luxury [2 3 3 4 4 5]}
                ;; medallions moved OFF luxury (2026-07-05): displaying on innovation/
                ;; durability (non-luxury) double-hits display-gain-luxury, and higher-tier
                ;; recipes favour non-luxury goods anyway
                :medallions #{[:innovation 5] [:durability 3] [:precision 4]}
                :passive {:id :luxury-sale-point
                          :text "Selling ≥1 Luxury also scores +1 point."}
                :ateliers [{:id :display-gain-luxury
                            :text "When you display a NON-Luxury mastercraft: gain 1 Luxury token from the market pool (if available)."}
                           {:id :gold-kicker
                            :text "At GAME END: score points equal to your leftover coins."}
                           {:id :cheap-adjacent
                            :text "The adjacent-space worker pickup is FREE (no 1-coin fee)."}]}
   :jeweler    {:color :red    :theme :generalist :pay-rule :primary
                :grid {:durability [2 3 3 4 5 6] :precision [2 3 3 4 5 6]
                       :innovation [2 3 3 4 5 6] :luxury [2 2 2 3 4 4]}
                :medallions #{[:durability 4] [:precision 4] [:innovation 4]}
                :passive {:id :sell-three-luxury
                          :text "Fulfilling a sell with ≥3 total tokens: gain 1 Luxury token from the pool afterward (if available)."}
                :ateliers [{:id :display-at-shops
                            :text "You may DISPLAY at any space where you own ANY shop (not just ateliers)."}
                           {:id :skilled-affinity
                            :text "Claiming a skilled worker also takes a matching regular worker from your space (if present)."}
                           {:id :shop-payout
                            :text "On unlocking: immediately gain 1 coin for each shop you own."}]}})
;; PAYMENT-RULE HOOK (Mohammad 2026-07-05): the 4 base guilds are home = a PRIMARY
;; colour (:pay-rule :primary → the white↔home / grey↔off-colour rules in pays?).
;; The planned white/grey/green/purple guilds will carry a DIFFERENT :pay-rule; the
;; hook is wired here + in pays? so those slot in later without touching call sites.
(defn guild-pay-rule [guild] (get-in guilds [guild :pay-rule] :primary))
(defn grid-value [guild skill rank] (get-in guilds [guild :grid skill (dec rank)]))
(defn home-color [p] (get-in guilds [(:guild p) :color]))
(defn unlocked?
  "True when ability `id` is live for player p: the guild PASSIVE (always on) or an
   atelier ability the player unlocked (green's unlocked? pattern)."
  [p id]
  (or (= id (get-in guilds [(:guild p) :passive :id]))
      (contains? (:abilities p #{}) id)))

;; ── the 8 spaces — THE DESIGNER'S PAIRING TABLE (2026-07-03, canon) ───────────
;; Each row pairs location ↔ demand ↔ action (+ :take colours on the 3
;; shop-action spaces = the COLOURS the shop-worker follow-up may take; it
;; constrains by the ACTION space, not where the worker sits; skilled tokens
;; count as their base colour). Demand shapes:
;;   {:req X :opt Y}   required good first, the optional may ride along
;;   {:req :luxury}    luxury + ≤1 each of D/P/I optional (canon)
(def spaces
  [{:name "Academy"  :demand {:req :innovation :opt :precision}  :action :recipe-worker} ; 0 (inner of the NO-OUTER wedge)
   {:name "Docks"    :demand {:req :precision  :opt :innovation} :action :shop :take #{:black :blue}}   ; 1
   {:name "Parks"    :demand {:req :innovation :opt :durability} :action :shop :take #{:white :grey}
    ;; PARKS BACKSTOP (Mohammad 2026-07-04): a coin-FREE shop path. Building a shop
    ;; here costs 2 GREY workers (flat, regardless of shop count), which DROP on
    ;; Parks (recycle, like a recipe's spent workers) — so coin-starvation stops
    ;; forcing everyone onto a luxury stall for the coin tap.
    :shop-cost {:workers [:grey :grey]}}   ; 2
   {:name "Temple"   :demand {:req :precision  :opt :durability} :action :atelier-pts}   ; 3
   {:name "Manor"    :demand {:req :luxury}                      :action :recipe-coin}   ; 4
   {:name "Barracks" :demand {:req :durability :opt :precision}  :action :shop :take #{:red :yellow}}   ; 5
   {:name "Castle"   :demand {:req :luxury}                      :action :atelier-coin}  ; 6
   {:name "Tavern"   :demand {:req :durability :opt :innovation} :action :skilled}])     ; 7
;; NOTE: the 6 D/P/I demands are the COMPLETE set of ordered pairs of {D,P,I}
;; (Academy I→P, Docks P→I, Parks I→D, Temple P→D, Barracks D→P, Tavern D→I),
;; the directed K3 — plus Manor + Castle on Luxury. (Tavern was briefly a
;; both-required "D+I" node; designer corrected it to D→I 2026-07-03, which
;; restores the full pair set.)

(defn demand-met?
  "≥1 held token of the demand's required good?"
  [demand tokens] (pos? (get tokens (:req demand) 0)))

;; ── the wedges: the designer's table (2026-07-03) on green's two-track model ──
;; 6 wedges, order-shuffled per game (canon R2-B). New in the pairing table:
;;   • W-A has an INNER ONLY (the Academy) and NO OUTER, marked switch. The
;;     outer ring SKIPS that wedge on a fast lane (prev outer → next outer in
;;     ONE road) but BRANCHES into the Academy: prev outer → Academy → next
;;     outer — a detour pocket costing one extra stop.
;;   • Docks and Barracks are OUTER-ONLY wedges (no inner, NOT switches).
;; INNER-TRACK READING (⚑ flagged): green gives EVERY wedge an inner-rep
;; (:switch wedges use their outer node). Here only wedges WITH an inner-rep
;; join the inner ring — Academy (real inner; its branch edges are what make
;; it a switch), Parks, Manor (:switch — on both rings exactly as in green)
;; and Castle. The ring connects consecutive inner-reps in wedge order,
;; SKIPPING the two outer-only wedges (the designer marked only Academy and
;; Manor "switch", so Docks/Barracks do not carry the inner track).
(def wedges
  [{:inner 0 :switch true}            ; W-A Academy — INNER ONLY (no outer), the switch pocket
   {:outer 1}                         ; W-B Docks — outer-only
   {:outer 3 :inner 2}                ; W-C Temple / Parks
   {:outer 4 :inner :switch}          ; W-D Manor — switch (tracks connect at its outer)
   {:outer 5}                         ; W-E Barracks — outer-only
   {:outer 7 :inner 6 :cross true}])  ; W-F Tavern / Castle — THE cross

(defn- build-board
  "Wedge ORDER is the only shuffle (canon R2-B). Green's two-track directed
   roads, extended for the designer's 2026-07-03 table. Roads (:adj):
     • outer ring: consecutive OUTERS in wedge order — the no-outer Academy
       wedge is SKIPPED (its neighbours connect directly: the fast lane)
     • Academy branch: prev outer → Academy and Academy → next outer (the
       one-extra-stop detour; with the inner ring this makes Academy a switch)
     • inner ring: consecutive INNER-REPS in wedge order (a :switch wedge's
       rep is its OUTER node; outer-only wedges have NO rep and are skipped)
     • the :cross wedge adds inner → outer (the tracks cross there)"
  [seed]
  (let [ws (vec (seeded-shuffle seed wedges))
        n (count ws)
        inner-rep (fn [w] (if (= :switch (:inner w)) (:outer w) (:inner w)))
        ring (fn [adj nodes]
               (reduce (fn [m k]
                         (update m (nth nodes k) (fnil conj #{})
                                 (nth nodes (mod (inc k) (count nodes)))))
                       adj (range (count nodes))))
        ;; exactly ONE wedge lacks an outer, so its immediate ring neighbours
        ;; always carry outers — prev/next need no search
        ai (first (filter #(nil? (:outer (ws %))) (range n)))
        academy (:inner (ws ai))
        prev-o (:outer (ws (mod (+ ai (dec n)) n)))
        next-o (:outer (ws (mod (inc ai) n)))
        adj (-> (zipmap (range (count spaces)) (repeat #{}))
                (ring (vec (keep :outer ws)))       ; the outer fast lane
                (ring (vec (keep inner-rep ws)))    ; the inner ring
                (update prev-o (fnil conj #{}) academy)   ; branch in
                (update academy (fnil conj #{}) next-o))  ; rejoin
        adj (reduce (fn [m w]
                      (if (:cross w)
                        (update m (:inner w) (fnil conj #{}) (:outer w))
                        m))
                    adj ws)]
    {:wedge-order ws :adj (into {} (for [[k v] adj] [k (disj v k)]))}))

(defn fwd
  "Directed road targets one road forward from n (green's fwd)."
  [state n] (get-in state [:adj n] #{}))
(defn undirected-neighbors
  "Road neighbours of n ignoring direction — green's undirected-neighbors; used
   ONLY for the pay-1-coin adjacent worker pickup."
  [state n]
  (into (fwd state n) (for [[k v] (:adj state) :when (contains? v n)] k)))
(defn reachable
  "DIRECTED movement targets from n: every space 1 or 2 ROADS forward along the
   directed road graph (green's move legality, no Guildhall stop in E)."
  [state n]
  (let [f1 (fwd state n)]
    (disj (into f1 (mapcat #(fwd state %) f1)) n)))
(defn road-distance
  "Shortest DIRECTED road count from a to b (BFS over :adj; small graph). nil if
   unreachable (can't happen on a legal board, but stay total)."
  [state a b]
  (if (= a b) 0
      (loop [frontier #{a} seen #{a} d 0]
        (cond (contains? frontier b) d
              (empty? frontier) nil
              (> d 8) nil
              :else (let [nxt (reduce into #{} (map #(fwd state %) frontier))]
                      (recur (into #{} (remove seen) nxt) (into seen nxt) (inc d)))))))

;; ── recipes: base 4 + green's REAL deck (D page 12, ported 2026-07-02) ───────
(def base-recipes
  [{:id :base-d :inputs [:white :black]  :outputs [:durability]}
   {:id :base-p :inputs [:white :red]    :outputs [:precision]}
   {:id :base-i :inputs [:white :blue]   :outputs [:innovation]}
   {:id :base-l :inputs [:grey :yellow]  :outputs [:luxury]}])

;; Green's transcribed recipe cards (journeymen/game.cljc recipe-cards), adapted
;; for E: :one-time → :once?; the other flags live in :flags —
;;   :bonus-skill  on craft, +1 level of the output good's skill
;;   :lowest       on craft, +1 level of your LOWEST skill; on a TIE the player
;;                 CHOOSES which tied skill (the :choose-lowest pending step)
;;   :free-shop    on craft, place a basic shop FREE anywhere with room (the
;;                 :free-shop pending step; skippable)
;;   :free-atelier on craft, upgrade one of YOUR basics to an atelier FREE (no coin)
;;                 and unlock an ability — the :free-atelier pending step
;;   :coin-per-good on craft, +1 coin per DISTINCT NON-LUXURY good held (D/P/I, max 3)
;;   :grey-swap    on craft, the workers you SPENT leave the game (skilled → back to
;;                 the skilled pool) and 2 fresh GREY drop on the craft space instead
;;   :chain        §Provisional E-ADAPTATION: green fires chains off the recipe to
;;                 their LEFT (slot order); E holds recipes as a flat list, so a
;;                 :chain card is never craftable standalone — instead, after any
;;                 successful :craft, every held+payable chain is OFFERED once
;;                 (the :chain pending step: fire-chain / decline-chain).
;; RECIPE DECAY — 4 STACKS (Mohammad 2026-07-07, test version; full rationale in
;; game-ideas/journeymen/journeymen-e-recipe-decay.md). Dealt A→B→C→D; picking the
;; first C recipe clears leftover A, the first D clears leftover B (sliding window).
(def recipe-cards
  {;; A — starters: early production + skill-builders
   :a [{:inputs [:black :black :grey]   :outputs [:durability :durability]}
       {:inputs [:red :red :grey]       :outputs [:precision :precision]}
       {:inputs [:blue :blue :grey]     :outputs [:innovation :innovation]}
       {:inputs [:black :white] :outputs [:durability]  :flags #{:bonus-skill} :once? true}
       {:inputs [:red :white]   :outputs [:precision]   :flags #{:bonus-skill} :once? true}
       {:inputs [:blue :white]  :outputs [:innovation]  :flags #{:bonus-skill} :once? true}
       {:inputs [:white]        :outputs [] :flags #{:chain :lowest} :once? true}]
   ;; B — luxury, single-good chains, shop utility
   :b [{:inputs [:yellow :grey]         :outputs [:luxury] :flags #{:free-shop} :once? true}
       {:inputs [:yellow :yellow :grey] :outputs [:luxury :luxury]}
       {:inputs [:yellow]               :outputs [:luxury] :flags #{:chain}}
       {:inputs [:grey]                 :outputs [] :flags #{:chain :free-shop} :once? true}
       {:inputs [:black]        :outputs [:durability] :flags #{:chain}}
       {:inputs [:red]          :outputs [:precision]  :flags #{:chain}}
       {:inputs [:blue]         :outputs [:innovation] :flags #{:chain}}
       {:inputs [:grey]         :outputs [] :flags #{:chain :free-shop}}
       ;; grey-swap moved DOWN to B (2026-07-08) — introduce it early enough to shape play
       {:inputs [:grey :grey]   :outputs [] :flags #{:grey-swap} :once? true}]
   ;; C — dual-good production + lowest utility + coin + the free-atelier opener
   :c [{:inputs [:white :grey]  :outputs [] :flags #{:chain :lowest}}
       {:inputs [:white :grey]  :outputs [] :flags #{:chain :lowest}}
       {:inputs [:black :red]   :outputs [:durability :precision]  :once? true}
       {:inputs [:red :blue]    :outputs [:precision :innovation]  :once? true}
       {:inputs [:black :blue]  :outputs [:durability :innovation] :once? true}
       ;; coin-per-good now REPEATABLE (2026-07-08 — a mid-game recurring faucet, not one-shot)
       {:inputs [:grey]         :outputs [] :flags #{:coin-per-good}}
       {:inputs [:yellow]       :outputs [:luxury :luxury]}
       ;; free-atelier moved DOWN to C (2026-07-08) — the engine-opener needs to arrive earlier
       {:inputs [:grey :grey]   :outputs [] :flags #{:free-atelier} :once? true}]
   ;; D — the strongest / most complex (latest): triples + one-of-each
   :d [{:inputs [:black :white :grey]  :outputs [:durability :durability :durability]}
       {:inputs [:red :white :grey]    :outputs [:precision :precision :precision]}
       {:inputs [:blue :white :grey]   :outputs [:innovation :innovation :innovation]}
       {:inputs [:blue :red :black]    :outputs [:innovation :precision :durability]}
       {:inputs [:white :grey]         :outputs [:innovation :precision :durability] :flags #{:chain}}]})
(defn- gen-deck
  "Era order A→B→C, shuffled WITHIN each era (green's gen-recipe-deck; fixed
   per-era seed offsets instead of (hash era) — hash isn't CLJ/CLJS-portable)."
  [seed]
  (vec (for [[k era] (map-indexed vector [:a :b :c :d])
             [i card] (map-indexed vector
                                   (seeded-shuffle (+ seed (* 31 (inc k))) (recipe-cards era)))]
         (assoc card :id (keyword (str (name era) i)) :era era))))

;; ── master pool: the shared masterwork tableau (green's race pool) ────────────
;; Green's 14 master-recipe worker costs (master-recipe-costs, transcribed from
;; D-hires-masterwork-02.png). A visible shared list: BUILDING a mastercraft now
;; targets a specific token; claiming REMOVES it for everyone (a race). You still
;; need a medallion master-recipe claim to build one.
(def master-pool
  (vec (map-indexed (fn [i c] {:id (keyword (str "mw" i)) :cost c})
                    [[:blue :red :yellow] [:white :white] [:grey :grey] [:grey :grey]
                     [:blue :red :yellow] [:red :red :grey] [:blue :grey] [:black :grey]
                     [:yellow :grey] [:grey :grey] [:white :white] [:yellow :blue]
                     [:grey :grey] [:red :black]])))
(def master-cost (into {} (map (juxt :id :cost)) master-pool))

;; ── payment (green's 2026-06-29 PAYMENT CHOICE: payment-options + spend-exact) ─
(defn- pays? [home req w opts]
  (let [w (base-color w)]
    (case (:pay-rule opts :primary)
      ;; the 4 base guilds: home is a PRIMARY colour — white↔home interchangeable,
      ;; grey pays a grey slot OR any off-colour primary.
      :primary
      (cond (= req :any) true
            (= req w) true
            (or (= req home) (= req :white)) (boolean (#{home :white} w))
            (= req :grey) (or (= w :grey) (boolean (some #{w} (remove #{home} primary-colors))))
            :else false)
      ;; TODO white/grey/green/purple guild rules slot in here (different conversions)
      false)))
(defn- remove-one [v x]
  (let [i (vec-index-of (vec v) x)]
    (if (neg? i) (vec v) (into (subvec (vec v) 0 i) (subvec (vec v) (inc i))))))
(defn payment-options
  "Green's payment-options: every DISTINCT way to pay `reqs` from `pool` for a
   `home`-colour guild — each a sorted spent-multiset of ACTUAL pool tokens.
   Surfaces the strategic worker choices (home vs white, grey vs an off-colour,
   REGULAR vs a SKILLED token): when more than one exists the player picks —
   each is a separate move. [] if unpayable. Bounded by recipe size (≤3 reqs).
   opts threads per-player payment abilities (currently {:grey-any bool})."
  ([home pool reqs] (payment-options home pool reqs {}))
  ([home pool reqs opts]
   (->> (letfn [(go [reqs pool]
                  (if (empty? reqs) (list ())
                      (for [w (distinct (filter #(pays? home (first reqs) % opts) pool))
                            rst (go (rest reqs) (remove-one pool w))]
                        (cons w rst))))]
          (go reqs pool))
        (map #(vec (sort-by str %)))
        distinct vec)))
(defn spend-exact
  "Green's spend-exact: remove the exact multiset `pay` from `pool`; nil if the
   pool lacks a token `pay` calls for (a stale/illegal payment)."
  [pool pay]
  (reduce (fn [rem w] (when (and rem (some #{w} rem)) (remove-one rem w))) (vec pool) pay))
(defn- pay-opts [p] {:pay-rule (guild-pay-rule (:guild p))})

;; ── state ─────────────────────────────────────────────────────────────────────
;; Atelier cost — CANON (Mohammad 2026-07-02): "flat 4 gold, always — the scaled cost
;; is upfront with the shop." Ateliers still count as placed shops for the ramp
;; (total-player-shops counts every entry, basic or atelier).
(def atelier-cost 4)

;; ── shop-location blocking (green CHANGE 4, ported 2026-07-03) ───────────────
;; For low player counts, BLOCK shop-capacity slots at random spaces to tighten
;; the board. Green's numbers: 2p → 12 blocks, 3p → 4, 4p → 0; at most 2 blocks
;; per space; deterministic via the seeded PRNG so games replay. E node capacity
;; = 4 minus that space's blocked count.
(def blocks-for-players {2 12 3 4 4 0})
(defn- compute-blocks
  "{node count} of blocked capacity for `n` players, drawn from `seed` (green's
   compute-blocks; E has no Guildhall to exclude — all 8 spaces are candidates)."
  [seed n]
  (let [total (get blocks-for-players n 0)
        slots (seeded-shuffle (+ (long seed) 313) (vec (mapcat #(repeat 2 %) (range (count spaces)))))]
    (reduce (fn [m node] (update m node (fnil inc 0))) {} (take total slots))))

(defn- new-player [i guild]
  {:id i :guild guild :coins (+ 2 i) :score 0   ; green's seat-indexed start: 2 + seat
   :workers [] :tokens {} :master-recipes 0 :mastercrafts-built 0
   ;; 8 recipe SLOTS = green's 4-row × 2-col board, interleaved even=BASE,
   ;; odd=ACQUIRED. The 4 base recipes seed the even slots; ALL 8 are coverable
   ;; (covering a base recipe is a real trade-off). Chains fire by slot
   ;; adjacency: slot k+1 fires off crafting slot k.
   :recipe-slots (vec (mapcat (fn [r] [r nil]) base-recipes))
   :skills {:durability 1 :precision 1 :innovation 1 :luxury 1}
   :medallions-hit #{} :goods-sold #{} :sold-at #{} :abilities #{}
   :carts [nil nil] :shops-left 6 :ateliers-left 3})

(defn new-game [seed guild-picks]
  {:pre [(<= 2 (count guild-picks) 4) (every? guilds guild-picks)]}
  (let [board (build-board seed)
        deck (gen-deck (+ seed 7))
        ;; seed 3 workers per space from a 24-worker bag (4 per color incl. white/grey)
        bag (seeded-shuffle (+ seed 13)
                            (vec (mapcat #(repeat 4 %) (conj primary-colors :white :grey))))
        board-workers (into {} (map-indexed (fn [i _] [i (vec (take 3 (drop (* 3 i) bag)))])
                                            spaces))]
    (merge board
           {:seed seed :phase :setup :step :place-cart
            :setup-left (* 2 (count guild-picks))
            :turn 0 :round 0 :current 0 :active-cart nil
            :players (vec (map-indexed new-player guild-picks))
            :blocked (compute-blocks seed (count guild-picks))   ; green CHANGE 4: 2p→12, 3p→4, 4p→0
            :board-workers board-workers :board-shops {}
            :goods good-supply :tracks {:durability 0 :precision 0 :innovation 0 :luxury 0}
            :displays {}            ; good -> [{:pid n}]  (stacking dividends)
            :skilled-pool 2         ; waves remaining (each wave = 1 per color, tracked simply)
            :skilled-wave (zipmap primary-colors (repeat 1))
            :master-pool master-pool   ; the shared masterwork tableau (a race)
            :recipe-market (vec (take 4 deck)) :recipe-deck (vec (drop 4 deck))
            :ending false :final-round nil :log []})))

(defn game-over? [state] (= :over (:phase state)))
(defn current-player [state] (nth (:players state) (:current state)))
(defn final-scores [state] (into {} (for [p (:players state)] [(:id p) (:score p)])))
(defn winner [state] (key (apply max-key val (final-scores state))))

;; ── shops ─────────────────────────────────────────────────────────────────────
(def shop-cap 4)
(defn- node-entries [state node] (apply concat (vals (get-in state [:board-shops node] {}))))
(defn player-entries [state pid node] (get-in state [:board-shops node pid] []))
(defn player-shop-at? [state pid node] (boolean (seq (player-entries state pid node))))
(defn player-atelier-at? [state pid node]
  (boolean (some #(= :atelier (:level %)) (player-entries state pid node))))
(defn total-player-shops [state pid]
  (reduce + 0 (for [[_ m] (:board-shops state)] (count (get m pid)))))
(defn build-cost [state p] (max 1 (total-player-shops state (:id p))))
(defn node-capacity
  "Max basic shops a space holds: 4 minus its blocked count (green CHANGE 4)."
  [state node] (- shop-cap (get-in state [:blocked node] 0)))
(defn- node-room? [state node]
  (< (count (filter #(= :basic (:level %)) (node-entries state node))) (node-capacity state node)))
(defn- shop-nodes [state pid]
  (for [[node m] (:board-shops state) :when (seq (get m pid))] node))
;; A space with a :shop-cost {:workers [...]} is built by paying WORKERS not coins
;; (Parks backstop, Mohammad 2026-07-04; extensible to Temple's atelier later).
(defn shop-worker-cost [node] (get-in spaces [node :shop-cost :workers]))
(defn- can-pay-workers? [p wcost]
  (let [have (frequencies (map base-color (:workers p)))]
    (every? (fn [[c n]] (>= (get have c 0) n)) (frequencies (map base-color wcost)))))
(defn can-build-shop-at?
  "Room at t + payable. The COST is a property of the ACTION space (the cart's
   node), not the target: the Parks action pays 2 GREY and places a shop ANYWHERE
   (Mohammad bug-fix 2026-07-04: '2 grey to place ANYWHERE when you take the Parks
   action, not 2 grey to place on Parks specifically'). Every other shop action is
   the coin ramp, also anywhere."
  [state p action-node t]
  (and (pos? (:shops-left p)) (node-room? state t)
       (if-let [wc (shop-worker-cost action-node)]
         (can-pay-workers? p wc)
         (>= (:coins p) (build-cost state p)))))
;; UI helper: why can't I build at t from action-node? (nil = I can)
(defn shop-build-blocker [state p action-node t]
  (cond (not (pos? (:shops-left p)))            :no-shops-left
        (not (node-room? state t))              :space-full
        (shop-worker-cost action-node)
        (when-not (can-pay-workers? p (shop-worker-cost action-node)) :need-2-grey)
        (< (:coins p) (build-cost state p))     :need-coins))

;; ── logging ───────────────────────────────────────────────────────────────────
(defn- logln [state text]
  (update state :log conj {:turn (:turn state) :player (:current state)
                           :guild (:guild (current-player state)) :text text}))

;; ── tracks, dividends, clock ─────────────────────────────────────────────────
(defn- track-open? [state g]
  (< (get-in state [:tracks g]) (+ (track-threshold g) track-overfill)))
(defn- feed-track
  "Place one sold token of good g on its track: leaves circulation, advances the
   clock, pays every mastercraft displayed there +1pt +1coin (they stack)."
  [state g]
  (let [state (update-in state [:tracks g] inc)
        divs (get-in state [:displays g] [])]
    (reduce (fn [s {:keys [pid]}]
              (-> s
                  (update-in [:players pid :score] inc)
                  (update-in [:players pid :coins] inc)
                  (logln (str "dividend: P" pid " +1pt +1coin (" (name g) " track)"))))
            state divs)))
(defn- check-clock
  "End trigger: when the 2nd track reaches threshold, finish the round then play
   ONE more full round (canon R2-H)."
  [state]
  (let [filled (count (filter (fn [[g n]] (>= n (track-threshold g))) (:tracks state)))]
    (if (and (>= filled 2) (not (:ending state)))
      (-> state (assoc :ending true) (logln "CLOCK: second track filled — final round after this one"))
      state)))

;; ── selling ───────────────────────────────────────────────────────────────────
(defn- sale-payout
  "Pay out `goods` map {good n} at grid value per token; D/P/I -> points, Lux -> coins.
   Levels each distinct good type once (rank cap 6)."
  [state pid goods]
  (reduce
   (fn [s [g n]]
     (let [p (get-in s [:players pid])
           v (* n (grid-value (:guild p) g (get-in p [:skills g])))
           s (if (= g :luxury)
               (update-in s [:players pid :coins] + v)
               (update-in s [:players pid :score] + v))]
       (-> s
           (update-in [:players pid :skills g] #(min 6 (inc %)))
           (update-in [:players pid :goods-sold] conj g))))
   state goods))
(defn- raise-skill
  "+1 level of skill sk for pid (cap 6), logging `reason`. Callers that can hit a
   medallion cell should follow with award-medallions."
  [state pid sk reason]
  (let [cur (get-in state [:players pid :skills sk])]
    (if (>= cur 6)
      state
      (-> state
          (assoc-in [:players pid :skills sk] (inc cur))
          (logln (str reason ": " (name sk) " skill -> r" (inc cur)))))))
(defn- raise-lowest-skill
  "Green's :lowest flag — +1 the single LOWEST skill; ties break to the FIRST in
   good-types order (deterministic)."
  [state pid reason]
  (let [sks (get-in state [:players pid :skills])
        lo (apply min (map sks good-types))
        sk (first (filter #(= lo (sks %)) good-types))]
    (raise-skill state pid sk reason)))
(defn- award-medallions
  "Reaching a medallion grid cell grants +1 master recipe (once per cell). Master
   recipes come ONLY from the board (canon)."
  [state pid]
  (let [p (get-in state [:players pid])
        cells (get-in guilds [(:guild p) :medallions])]
    (reduce (fn [s [skill rank :as cell]]
              (if (and (>= (get-in s [:players pid :skills skill]) rank)
                       (not (contains? (get-in s [:players pid :medallions-hit]) cell)))
                (-> s
                    (update-in [:players pid :medallions-hit] conj cell)
                    (update-in [:players pid :master-recipes] inc)
                    (logln (str "medallion " (name skill) " r" rank " -> master recipe")))
                s))
            state cells)))
(defn- apply-sell
  "goods = {good n}; track = the chosen sold good to place on its track (or nil if
   no track is open for any sold good). Other tokens return to the market pool.
   boost? = alchemist :innovation-boost passive: discard 1 held Innovation token
   (returned to the pool) for a SECOND skill level per good type sold."
  [state pid node goods track boost?]
  (let [p0 (get-in state [:players pid])
        state (reduce (fn [s [g n]] (update-in s [:players pid :tokens g] - n)) state goods)
        ;; innovation-boost: the discarded token leaves the hand for the pool
        state (if boost?
                (-> state
                    (update-in [:players pid :tokens :innovation] dec)
                    (update-in [:goods :innovation] inc)
                    (logln "discards an Innovation token (boost: each sold type levels twice)"))
                state)
        state (sale-payout state pid goods)
        state (if boost?
                ;; only OTHER good types (not Innovation itself) level a second time
                (reduce (fn [s g] (raise-skill s pid g "innovation-boost"))
                        state (remove #{:innovation} (keys goods)))
                state)
        state (award-medallions state pid)
        ;; return all but the tracked copy to the pool
        state (reduce (fn [s [g n]]
                        (let [ret (if (= g track) (dec n) n)]
                          (update-in s [:goods g] + (max 0 ret))))
                      state goods)
        state (update-in state [:players pid :sold-at] conj node)
        state (if track (feed-track state track) state)
        ;; blacksmith :optional-sale-coin atelier: sold ≥1 OPTIONAL good (any good
        ;; that isn't this space's REQUIRED good) → +1 coin
        state (if (and (unlocked? p0 :optional-sale-coin)
                       (let [req (:req (:demand (nth spaces node)))]
                         (some #(not= % req) (keys goods))))
                (-> state (update-in [:players pid :coins] inc)
                    (logln "optional-good sale: +1 coin"))
                state)
        ;; goldsmith passive: any luxury sold also scores a point
        state (if (and (unlocked? p0 :luxury-sale-point) (pos? (get goods :luxury 0)))
                (-> state (update-in [:players pid :score] inc)
                    (logln "luxury sale: +1 point"))
                state)
        ;; jeweler passive: a ≥3-token sale nets a luxury token from the pool
        state (if (and (unlocked? p0 :sell-three-luxury)
                       (>= (reduce + (vals goods)) 3)
                       (pos? (get-in state [:goods :luxury])))
                (-> state (update-in [:goods :luxury] dec)
                    (update-in [:players pid :tokens :luxury] (fnil inc 0))
                    (logln "3+ token sale: gains a Luxury token from the pool"))
                state)
        state (update-in state [:players pid :tokens]
                         (fn [t] (into {} (remove (comp zero? val) t))))]
    (-> state (check-clock)
        (logln (str "sells " (str/join "+" (map (fn [[g n]] (str n " " (name g))) goods))
                    (when track (str " (tracked: " (name track) ")")))))))

;; ── crafting ──────────────────────────────────────────────────────────────────
(defn- pool-covers? [state outputs]
  (every? (fn [[g n]] (>= (get-in state [:goods g] 0) n)) (frequencies outputs)))
(defn- drop-workers
  "Spent workers pool at `node`. A SKILLED token drops still marked, and the
   spender immediately grabs a matching-colour REGULAR already pooled there
   (green spend-grab rule). pid optional for callers with no grab (setup etc)."
  [state node spent & [pid]]
  (reduce
   (fn [s w]
     (let [s (update-in s [:board-workers node] (fnil conj []) w)]
       (if (and pid (skilled? w))
         (let [b (base-color w)
               pool (vec (get-in s [:board-workers node]))
               i (vec-index-of pool b)]
           (if (neg? i)
             s
             (-> s
                 (assoc-in [:board-workers node]
                           (vec (concat (subvec pool 0 i) (subvec pool (inc i)))))
                 (update-in [:players pid :workers] conj b)
                 (logln (str "skilled " (name b) " dropped — grabs a regular " (name b) " there")))))
         s)))
   state spent))
(defn- spend-pay
  "Remove the exact chosen payment multiset from pid's hand (spend-exact)."
  [state pid pay]
  (update-in state [:players pid :workers] #(vec (spend-exact % pay))))
(defn- apply-craft
  "Craft the recipe in SLOT j (green's craft-token shape): spend the CHOSEN
   payment (`pay`, an exact multiset), drop workers at node, grant outputs from
   the pool, then the card's flag effects (:bonus-skill / :lowest — both can hit
   medallion cells; :free-shop queues on :pending-free-shops; :once? COVERS the
   slot — it empties). Used by :craft AND the chain cascade (:fire-chain)."
  [state pid node j pay]
  (let [recipe (get-in state [:players pid :recipe-slots j])
        flags (:flags recipe #{})
        state (spend-pay state pid pay)
        ;; :grey-swap — the spent workers LEAVE THE GAME (a skilled token returns to
        ;; the skilled reserve instead); 2 fresh grey drop on the space (vs the usual recycle)
        state (if (contains? flags :grey-swap)
                (-> (reduce (fn [s w] (if (skilled? w)
                                        (update-in s [:skilled-wave (base-color w)] (fnil inc 0))
                                        s))
                            state pay)
                    (update-in [:board-workers node] (fnil into []) [:grey :grey])
                    (logln "grey-swap: spent workers leave the game (skilled → reserve); 2 grey drop here"))
                (drop-workers state node pay pid))
        state (reduce (fn [s g] (-> s (update-in [:goods g] dec)
                                    (update-in [:players pid :tokens g] (fnil inc 0))))
                      state (:outputs recipe))
        state (if (:once? recipe)
                (assoc-in state [:players pid :recipe-slots j] nil)
                state)
        state (if (contains? flags :bonus-skill)
                (raise-skill state pid (first (:outputs recipe)) "bonus-skill")
                state)
        ;; :lowest — +1 the lowest skill; a TIE defers to the player's choice (:choose-lowest)
        state (if (contains? flags :lowest)
                (let [sks (get-in state [:players pid :skills])
                      lo (apply min (map sks good-types))
                      tied (filterv #(= lo (sks %)) good-types)]
                  (if (> (count tied) 1)
                    (update state :pending-lowest (fnil inc 0))
                    (raise-skill state pid (first tied) "lowest-skill card")))
                state)
        state (if (some flags [:bonus-skill :lowest])
                (award-medallions state pid)
                state)
        ;; :coin-per-good — +1 coin per DISTINCT NON-LUXURY good held (D/P/I, cap 3).
        ;; Excludes luxury (2026-07-08) so it's a HARD LUXURY ALTERNATIVE: coins off a
        ;; diverse D/P/I spread instead of off luxury sales.
        state (if (contains? flags :coin-per-good)
                (let [n (count (filter (fn [[g c]] (and (pos? c) (some #{g} dpi)))
                                       (get-in state [:players pid :tokens])))]
                  (-> state (update-in [:players pid :coins] + n)
                      (logln (str "coin-per-good: +" n " coins (per distinct D/P/I)"))))
                state)
        ;; :free-atelier — queue a FREE basic→atelier upgrade (player picks the basic)
        state (if (contains? flags :free-atelier)
                (update state :pending-free-atelier (fnil inc 0))
                state)
        state (if (contains? flags :free-shop)
                (update state :pending-free-shops (fnil inc 0))
                state)]
    (logln state (str "crafts slot " (inc j) " — "
                      (if (seq (:outputs recipe))
                        (str/join "+" (map name (:outputs recipe)))
                        "(no goods — flag card)")))))

;; ── the after-craft cascade: green's slot-adjacency chains, then free shops ───
(defn payable-chain-at?
  "Green's payable-chain-at?: is slot k a :chain recipe pid can FIRE right now —
   payable from the pool AND the market can supply its outputs? (Chains fire
   ONLY off crafting the slot to their LEFT; never standalone.)"
  [state pid k]
  (let [p (get-in state [:players pid])
        card (get-in p [:recipe-slots k])]
    (boolean (and card (contains? (:flags card #{}) :chain)
                  (pool-covers? state (:outputs card))
                  (seq (payment-options (home-color p) (:workers p) (:inputs card)
                                        (pay-opts p)))))))
(declare end-turn)
(defn- finish-craft-phase
  "After the chain cascade (or a skip/decline): resolve queued PLAYER-CHOICE
   effects in order — lowest-skill ties (:choose-lowest), free atelier upgrades
   (:free-atelier), free shops (:free-shop) — then end the turn."
  [state]
  (let [state (dissoc state :pending-chain)]
    (cond
      (pos? (:pending-lowest state 0))       (assoc state :step :choose-lowest)
      (pos? (:pending-free-atelier state 0)) (assoc state :step :free-atelier)
      (pos? (:pending-free-shops state 0))   (assoc state :step :free-shop)
      :else (end-turn state))))
(defn- free-shops-done
  "After the last queued free shop resolves: two-free-shops (unlocked BEFORE the
   craft) returns to :craft so you still get your craft; recipe :free-shop cards
   (queued during/after the craft) continue the normal craft-phase tail."
  [state]
  (let [ret (:free-shop-return state)
        state (dissoc state :pending-free-shops :free-shop-return)]
    (if (= ret :craft)
      (assoc state :step :craft)
      (finish-craft-phase state))))
(defn- resolve-after-craft
  "Green's maybe-offer-chain + resolution order: after crafting/firing slot j,
   OFFER slot j+1 as an opt-in chain if it's a payable :chain (the cascade moves
   RIGHTWARD, one slot at a time). When the cascade ends, finish-craft-phase
   drains the queued player-choice effects."
  [state pid node j]
  (let [k (inc j)]
    (if (payable-chain-at? state pid k)
      (assoc state :pending-chain {:next k :node node} :step :chain)
      (finish-craft-phase state))))

;; ── enumerate-moves (THE choice surface — bots and humans alike) ──────────────
(defn- distinct-colors [ws] (distinct ws))
(defn enumerate-moves [state]
  (let [pid (:current state) p (current-player state)]
    (case (:phase state)
      :setup
      ;; place carts alternately; the FIRST cart also auto-places a free starting shop
      (for [n (range (count spaces))] {:type :place-cart :node n})
      :play
      (let [ci (:active-cart state)
            node (when ci (get-in p [:carts ci]))]
        (case (:step state)
          :move
          (for [c [0 1] :let [start (get-in p [:carts c])]
                dest (reachable state start)]
            {:type :move-cart :cart c :dest dest})
          :pickup
          (concat
           (for [col (distinct-colors (get-in state [:board-workers node]))]
             {:type :pickup :color col})
           ;; goldsmith :cheap-adjacent atelier: the 1-coin fee is waived.
           ;; adjacency = UNDIRECTED road neighbours (green's pay-1 pickup rule)
           (when (or (pos? (:coins p)) (unlocked? p :cheap-adjacent))
             (for [adj (undirected-neighbors state node)
                   col (distinct-colors (get-in state [:board-workers adj]))]
               {:type :pickup-adjacent :node adj :color col}))
           [{:type :skip-pickup}])
          :main
          (let [{:keys [req opt] :as demand} (:demand (nth spaces node))
                act (:action (nth spaces node))
                tokens (:tokens p)
                ;; SELL (needs your shop here; required before optional — canon Q2)
                sells (when (and (player-shop-at? state pid node) (demand-met? demand tokens))
                        (let [goods (cond-> {req (get tokens req)}
                                      (and opt (pos? (get tokens opt 0)))
                                      (assoc opt (get tokens opt))
                                      ;; luxury spaces: +≤1 each of D/P/I (canon)
                                      (= req :luxury)
                                      (into (for [g dpi :when (pos? (get tokens g 0))] [g 1])))
                              track-choices (filter #(track-open? state %) (keys goods))
                              base (if (seq track-choices)
                                     (for [t track-choices] {:type :sell :goods goods :track t})
                                     [{:type :sell :goods goods :track nil}])
                              ;; alchemist :innovation-boost passive: only when an
                              ;; Innovation token is held BEYOND the sale itself
                              ;; offer only when a spare Innovation is held AND at least
                              ;; one NON-Innovation good is in the sale (else it does nothing)
                              boost? (and (unlocked? p :innovation-boost)
                                          (> (get tokens :innovation 0)
                                             (get goods :innovation 0))
                                          (seq (remove #{:innovation} (keys goods))))]
                          (concat base (when boost? (map #(assoc % :boost true) base)))))
                ;; DISPLAY a built mastercraft instead of selling — at your atelier
                ;; (jeweler :display-at-shops atelier: ANY of your shops works)
                displays (when (and (pos? (:mastercrafts-built p))
                                    (or (player-atelier-at? state pid node)
                                        (and (unlocked? p :display-at-shops)
                                             (player-shop-at? state pid node))))
                           (for [g good-types] {:type :display :track g}))
                actions
                (case act
                  :shop
                  ;; The shop ACTION places a basic at ANY space with room (Mohammad
                  ;; 2026-07-02: "not allowing me to place them elsewhere" was a bug —
                  ;; shops must be able to cover every demand). The worker pickup is a
                  ;; FOLLOW-UP step (:shop-worker) so it can draw from the JUST-placed
                  ;; shop too (canon R2-C: you get both; without placing you still get
                  ;; a worker from one of your shops).
                  ;; place at any space you can pay for from THIS action (coin ramp;
                  ;; the Parks grey backstop only via Parks' own action — bug-fix)
                  (concat
                   (for [t (range (count spaces)) :when (can-build-shop-at? state p node t)]
                     {:type :action-shop :place? true :node t})
                   [{:type :action-shop :place? false}])
                  (:atelier-pts :atelier-coin)
                  (when (and (pos? (:ateliers-left p))
                             (>= (:coins p) atelier-cost))
                    (for [[n2 m] (:board-shops state)
                          :when (some #(= :basic (:level %)) (get m pid))]
                      {:type :action-atelier :kind act :basic-node n2}))
                  :recipe-coin
                  ;; take-recipe carries the TARGET SLOT (green's recipe-pickup-moves:
                  ;; a taken recipe may go into ANY slot — empty OR covering one
                  ;; already there, base recipes included; slot position matters
                  ;; because a :chain stacks onto the recipe to its LEFT).
                  (when (>= (:coins p) 2)
                    (for [[i r] (map-indexed vector (:recipe-market state)) :when r
                          j (range (count (:recipe-slots p)))]
                      {:type :action-recipe :idx i :slot j :pay :coins}))
                  :recipe-worker
                  (when (seq (:workers p))
                    (for [[i r] (map-indexed vector (:recipe-market state)) :when r
                          j (range (count (:recipe-slots p)))
                          col (distinct (:workers p))]
                      {:type :action-recipe :idx i :slot j :pay :worker :color col}))
                  :skilled
                  (for [[col n] (:skilled-wave state) :when (pos? n)]
                    {:type :action-skilled :color col}))]
            (concat sells displays actions [{:type :skip-main}]))
          :shop-worker
          ;; take a worker from ANY space where you own a shop (incl. one just
          ;; placed) — but ONLY of the ACTION space's :take colours (the space
          ;; where the cart activated the shop action, green's Academy/Docks/Parks
          ;; pattern; skilled tokens count as their base colour). Nil-tolerant for
          ;; pre-:take saves.
          (let [tk (get-in spaces [(:shop-action-node state) :take])]
            (concat
             (for [n2 (shop-nodes state pid)
                   col (distinct (get-in state [:board-workers n2]))
                   :when (or (nil? tk) (contains? tk (base-color col)))]
               {:type :shop-worker :from n2 :color col})
             [{:type :skip-shop-worker}]))
          :craft
          (let [home (home-color p) opts (pay-opts p)
                ;; green's enum-craft: craft ANY recipe SLOT you hold, one move per
                ;; DISTINCT payment (payment choice, 2026-06-29). Chain cards are
                ;; NEVER standalone crafts — they only fire via the slot cascade.
                crafts (for [[j r] (map-indexed vector (:recipe-slots p))
                             :when (and r (not (contains? (:flags r #{}) :chain))
                                        (pool-covers? state (:outputs r)))
                             pay (payment-options home (:workers p) (:inputs r) opts)]
                         {:type :craft :slot j :pay pay})
                ;; mastercraft build TARGETS a specific master-pool token (one move
                ;; per payable token × distinct payment): needs a medallion claim
                master (when (pos? (:master-recipes p))
                         (for [t (:master-pool state)
                               pay (payment-options home (:workers p) (:cost t) opts)]
                           {:type :craft-master :id (:id t) :pay pay}))]
            (concat crafts master [{:type :skip-craft}]))
          :free-shop
          ;; queued :free-shop cards resolve here (one placement per queued shop):
          ;; place a basic FREE anywhere with room, or forgo it (always skippable)
          (concat
           (when (pos? (:shops-left p))
             (for [t (range (count spaces)) :when (node-room? state t)]
               {:type :place-free-shop :node t}))
           [{:type :skip-free-shop}])
          :choose-lowest
          ;; a :lowest card tied — pick which tied-lowest skill to raise
          (let [sks (:skills p) lo (apply min (map sks good-types))]
            (for [g good-types :when (= lo (sks g))] {:type :choose-lowest :skill g}))
          :free-atelier
          ;; :free-atelier recipe — upgrade one of YOUR basics (free), or forgo it
          (concat
           (when (pos? (:ateliers-left p))
             (for [n2 (distinct (for [[n2 m] (:board-shops state)
                                      :when (some #(= :basic (:level %)) (get m pid))] n2))]
               {:type :free-atelier :basic-node n2}))
           [{:type :skip-free-atelier}])
          :chain
          ;; green's enum-chain: the OFFERED chain is the single slot (:next) —
          ;; fire it (one move per distinct payment) or decline. Opt-in, always.
          (let [{:keys [next]} (:pending-chain state)
                card (get-in p [:recipe-slots next])]
            (concat
             (for [pay (payment-options (home-color p) (:workers p) (:inputs card)
                                        (pay-opts p))]
               {:type :fire-chain :slot next :pay pay})
             [{:type :decline-chain}]))
          :ability
          ;; building an atelier unlocks ONE guild ability — the player's choice
          (let [locked (remove (:abilities p #{})
                               (map :id (get-in guilds [(:guild p) :ateliers])))]
            (if (seq locked)
              (for [id locked] {:type :pick-ability :id id})
              [{:type :skip-ability}]))))   ; defensive: can't happen (3 builds / 3 abilities)
      :over [])))

;; ── apply-move ────────────────────────────────────────────────────────────────
(defn- endgame-scoring
  "Fires as the game ends: goldsmith :gold-kicker scores points equal to leftover coins."
  [state]
  (reduce (fn [s p]
            (if (unlocked? p :gold-kicker)
              (-> s (update-in [:players (:id p) :score] + (:coins p))
                  (logln (str "gold kicker (end): P" (:id p) " +" (:coins p) " points (leftover coins)")))
              s))
          state (:players state)))
(defn- end-turn [state]
  (let [n (count (:players state)) nxt (mod (inc (:current state)) n)
        state (assoc state :current nxt :step :move :active-cart nil
                     :turn (inc (:turn state)))]
    (if-not (zero? nxt)
      state
      (let [state (update state :round inc)]
        (cond
          ;; final round finished -> over (endgame scoring fires first)
          (and (:final-round state) (> (:round state) (:final-round state)))
          (-> state endgame-scoring (assoc :phase :over))
          ;; clock fired earlier this round -> the NEXT round is the last (canon R2-H)
          (and (:ending state) (nil? (:final-round state)))
          (assoc state :final-round (:round state))
          ;; §Provisional SAFETY BACKSTOP (not canon): a wedged game (weak bots /
          ;; starved economy) force-ends rather than looping forever.
          (> (:round state) 120)
          (-> state (assoc :ending true :final-round (:round state))
              (logln "SAFETY: round 120 backstop — forcing the final round"))
          :else state)))))

(defn- take-from-node [state node color]
  (update-in state [:board-workers node]
             (fn [ws] (let [i (vec-index-of (vec ws) color)]
                        (vec (concat (subvec (vec ws) 0 i) (subvec (vec ws) (inc i))))))))
(defn- pickup-into-hand
  "Take `color` from `node` into pid's hand and log it. Blacksmith :grey-sweep:
   picking up a GREY sweeps ALL grey workers at that space into the hand."
  [state pid p node color]
  (if (and (unlocked? p :grey-sweep) (= :grey (base-color color)))
    (let [ws (vec (get-in state [:board-workers node]))
          greys (filterv #(= :grey (base-color %)) ws)]
      (-> state
          (assoc-in [:board-workers node] (filterv #(not= :grey (base-color %)) ws))
          (update-in [:players pid :workers] into greys)
          (logln (str "grey sweep: takes " (count greys) " grey workers from space " node))))
    (-> state
        (take-from-node node color)
        (update-in [:players pid :workers] conj color)
        (logln (str "takes a " (name (base-color color)) " worker from space " node)))))

(defn apply-move [state move]
  (let [pid (:current state) p (current-player state)]
    (case (:type move)
      :place-cart
      (let [ci (if (nil? (get-in p [:carts 0])) 0 1)
            n (:node move)
            state (assoc-in state [:players pid :carts ci] n)
            ;; green's setup: EVERY cart placement builds a free basic shop at the
            ;; cart's node if there's room (green :place-cart applies the build)
            state (if (and (node-room? state n) (pos? (:shops-left p)))
                    (-> state
                        (update-in [:board-shops n pid] (fnil conj []) {:level :basic})
                        (update-in [:players pid :shops-left] dec))
                    state)
            left (dec (:setup-left state))]
        (if (zero? left)
          (assoc state :setup-left 0 :phase :play :step :move :current 0)
          (assoc state :setup-left left
                 :current (mod (inc pid) (count (:players state))))))

      :move-cart
      (-> state
          (assoc-in [:players pid :carts (:cart move)] (:dest move))
          (assoc :active-cart (:cart move) :step :pickup))

      :pickup
      (let [node (get-in p [:carts (:active-cart state)])]
        (-> (pickup-into-hand state pid p node (:color move))
            (assoc :step :main)))
      :pickup-adjacent
      (-> (pickup-into-hand state pid p (:node move) (:color move))
          ;; goldsmith :cheap-adjacent atelier waives the 1-coin fee
          (cond-> (not (unlocked? p :cheap-adjacent))
            (update-in [:players pid :coins] dec))
          (assoc :step :main))
      :skip-pickup (assoc state :step :main)

      :sell
      (let [node (get-in p [:carts (:active-cart state)])]
        (-> (apply-sell state pid node (:goods move) (:track move) (:boost move))
            (assoc :step :craft)))

      :display
      (let [g (:track move)
            ;; blacksmith passive: track-good skill rises BEFORE the display scores;
            ;; alchemist :display-up-innovation atelier: +1 Innovation on display.
            ;; Both can reach medallion cells, so medallions are checked after.
            state (cond-> state
                    (unlocked? p :display-skill-first)
                    (raise-skill pid g "display (passive: skill first)")
                    (unlocked? p :display-up-innovation)
                    (raise-skill pid :innovation "display atelier ability"))
            state (award-medallions state pid)
            p' (get-in state [:players pid])
            pts (grid-value (:guild p') g (get-in p' [:skills g]))
            ;; DISPLAY TRIGGERS PRIOR DIVIDENDS (Mohammad 2026-07-03): placing a
            ;; mastercraft immediately pays +1pt +1coin to each mastercraft
            ;; ALREADY displayed on this track (not the new one) — "so the gold
            ;; enters circulation, like all other dividends"
            state (reduce (fn [s {:keys [pid]}]
                            (-> s
                                (update-in [:players pid :score] inc)
                                (update-in [:players pid :coins] inc)
                                (logln (str "dividend: P" pid " +1pt +1coin (new display on the "
                                            (name g) " track)"))))
                          state (get-in state [:displays g] []))]
        (-> state
            (update-in [:players pid :mastercrafts-built] dec)
            (update-in [:displays g] (fnil conj []) {:pid pid})
            (update-in [:players pid :score] + pts)
            (logln (str "displays a mastercraft on the " (name g) " track (+" pts "pts, dividends on)"))
            ;; goldsmith :display-gain-luxury atelier — only on a NON-Luxury display
            (cond-> (and (unlocked? p :display-gain-luxury)
                         (not= g :luxury)
                         (pos? (get-in state [:goods :luxury])))
              (-> (update-in [:goods :luxury] dec)
                  (update-in [:players pid :tokens :luxury] (fnil inc 0))
                  (logln "display ability: gains a Luxury token from the pool")))
            (assoc :step :craft)))

      :action-shop
      (let [node (get-in p [:carts (:active-cart state)])
            state (if (:place? move)
                    (let [t (:node move)
                          wcost (shop-worker-cost node)   ; the ACTION space's cost (Parks = 2 grey)
                          coin-cost (when-not wcost (build-cost state p))  ; BEFORE the shop is added (ramp counts it)
                          state (-> state
                                    (update-in [:board-shops t pid] (fnil conj []) {:level :basic})
                                    (update-in [:players pid :shops-left] dec))
                          state (if wcost
                                  ;; PARKS action: pay grey (drop on Parks = the ACTION
                                  ;; node, recycle) and place the shop at target t ANYWHERE.
                                  (-> (reduce (fn [s c]
                                                (-> s (update-in [:players pid :workers] remove-n c 1)
                                                    (update-in [:board-workers node] (fnil conj []) c)))
                                              state wcost)
                                      (logln (str "Parks action: builds a shop at " (:name (nth spaces t))
                                                  " for " (str/join "+" (map name wcost)) " (coin-free)")))
                                  (-> state
                                      (update-in [:players pid :coins] - coin-cost)
                                      (logln (str "places a shop at space " t))))]
                      ;; blacksmith :shop-refund atelier — fires on any basic placement
                      (cond-> state
                        (unlocked? p :shop-refund)
                        (-> (update-in [:players pid :coins] + 2)
                            (logln "shop refund: +2 coins"))))
                    state)]
        ;; remember the ACTION space — its :take set constrains the worker step
        (assoc state :step :shop-worker :shop-action-node node))

      :shop-worker
      (-> (pickup-into-hand state pid p (:from move) (:color move))
          (dissoc :shop-action-node)
          (assoc :step :craft))
      :skip-shop-worker (-> state (dissoc :shop-action-node) (assoc :step :craft))

      :action-atelier
      (let [n2 (:basic-node move)
            cost atelier-cost
            bonus (if (= (:kind move) :atelier-pts) {:score 3 :coins 1} {:score 1 :coins 3})]
        (-> state
            (update-in [:players pid :coins] - cost)
            ;; swap-in-place: ONE basic returns to the player, the atelier takes its spot
            (update-in [:board-shops n2 pid]
                       (fn [es]
                         (let [i (first (keep-indexed
                                         (fn [i e] (when (= :basic (:level e)) i)) es))]
                           (conj (vec (concat (take i es) (drop (inc i) es)))
                                 {:level :atelier}))))
            (update-in [:players pid :shops-left] inc)
            (update-in [:players pid :ateliers-left] dec)
            (update-in [:players pid :score] + (:score bonus))
            (update-in [:players pid :coins] + (:coins bonus))
            (logln (str "upgrades to an atelier at space " n2 " (+" (:score bonus) "pts +"
                        (:coins bonus) "c)"))
            ;; building an atelier UNLOCKS one guild ability — player's choice
            (assoc :step :ability)))

      :pick-ability
      (let [id (:id move)
            ;; where to return once the ability resolves: normally :craft, or the
            ;; craft-phase tail if this atelier came from a :free-atelier recipe
            from-recipe? (boolean (:ability-return state))
            state (-> state
                      (update-in [:players pid :abilities] (fnil conj #{}) id)
                      (logln (str "unlocks guild ability: " (name id)))
                      (dissoc :ability-return))
            ;; ON-UNLOCK immediate effects
            state (case id
                    ;; jeweler shop-payout: +1 coin per shop owned
                    :shop-payout
                    (let [n (total-player-shops state pid)]
                      (-> state (update-in [:players pid :coins] + n)
                          (logln (str "shop payout: +" n " coins (1 per shop owned)"))))
                    ;; alchemist two-free-shops: place up to 2 free shops RIGHT NOW
                    :two-free-shops
                    (-> state (update :pending-free-shops (fnil + 0) 2)
                        (logln "two free shops: place up to 2 now"))
                    state)]
        (cond
          ;; two-free-shops queued → place them IMMEDIATELY, then resume where we'd go
          (pos? (:pending-free-shops state 0))
          (assoc state :free-shop-return (if from-recipe? :tail :craft) :step :free-shop)
          from-recipe? (finish-craft-phase state)
          :else (assoc state :step :craft)))
      :skip-ability (if (:ability-return state)
                      (finish-craft-phase (dissoc state :ability-return))
                      (assoc state :step :craft))

      :action-recipe
      (let [i (:idx move) r (nth (:recipe-market state) i)
            ;; the move carries the CHOSEN slot (green's take-recipe): the taken
            ;; recipe COVERS it — empty or not, base recipes included
            j (:slot move 1)
            state (if (= (:pay move) :coins)
                    (update-in state [:players pid :coins] - 2)
                    (let [w (or (:color move) (first (:workers p)))
                          node (get-in p [:carts (:active-cart state)])
                          k (vec-index-of (vec (:workers p)) w)]
                      (-> state
                          (update-in [:players pid :workers]
                                     (fn [ws] (let [v (vec ws)]
                                                (vec (concat (subvec v 0 k) (subvec v (inc k)))))))
                          (drop-workers node [w] pid))))
            deck (:recipe-deck state)
            fill1 (first deck) fill2 (second deck)]
        (-> state
            (assoc-in [:players pid :recipe-slots j] r)
            ;; RECIPE-MARKET GROWTH (Mohammad 2026-07-05): taking a recipe refills the
            ;; slot AND adds a SECOND card, so the market grows over the game — you can
            ;; dig for the recipe you want, picking up a weaker one or two along the way.
            (assoc-in [:recipe-market i] fill1)
            (cond-> fill2 (update :recipe-market conj fill2))
            (assoc :recipe-deck (vec (drop (if fill2 2 1) deck)))
            ;; alchemist :recipe-coinback atelier: +1 coin after taking a recipe (either space)
            (cond-> (unlocked? p :recipe-coinback)
              (-> (update-in [:players pid :coins] inc)
                  (logln "recipe coinback: +1 coin")))
            ;; RECIPE DECAY (Mohammad 2026-07-07): the FIRST C recipe picked clears any
            ;; leftover A from the market; the first D picked clears leftover B (nil the
            ;; slots — no replacement — the enum/UI skip nils).
            (cond->
             (and (= :c (:era r)) (not (:a-cleared state)))
             (-> (update :recipe-market (fn [m] (mapv #(when-not (= :a (:era %)) %) m)))
                 (assoc :a-cleared true)
                 (logln "recipe decay: leftover A recipes clear away (first C taken)"))
             (and (= :d (:era r)) (not (:b-cleared state)))
             (-> (update :recipe-market (fn [m] (mapv #(when-not (= :b (:era %)) %) m)))
                 (assoc :b-cleared true)
                 (logln "recipe decay: leftover B recipes clear away (first D taken)")))
            (logln (str "takes a recipe into slot " (inc j) " (" (name (:pay move)) ")"))
            (assoc :step :craft)))

      :action-skilled
      (let [node (get-in p [:carts (:active-cart state)])]
        (-> state
            (update-in [:skilled-wave (:color move)] dec)
            (update-in [:players pid :workers] conj (skilled-of (:color move)))
            ;; wave 2 unlocks when wave 1 exhausts (canon R2-D: 2 waves, same cost)
            (cond-> (and (every? zero? (vals (update (:skilled-wave state) (:color move) dec)))
                         (= 2 (:skilled-pool state)))
              (-> (assoc :skilled-pool 1)
                  (assoc :skilled-wave (zipmap primary-colors (repeat 1)))
                  (logln "skilled reserve exhausted — wave 2 opens")))
            (logln (str "claims a skilled " (name (:color move)) " worker"))
            ;; jeweler :skilled-affinity atelier: also grab a matching REGULAR
            ;; worker pooled at the current space (if present)
            (cond-> (and (unlocked? p :skilled-affinity)
                         (not (neg? (vec-index-of
                                     (vec (get-in state [:board-workers node]))
                                     (:color move)))))
              (-> (take-from-node node (:color move))
                  (update-in [:players pid :workers] conj (:color move))
                  (logln (str "skilled affinity: also takes a regular "
                              (name (:color move)) " worker here"))))
            (assoc :step :craft)))

      :skip-main (assoc state :step :craft)

      :craft
      ;; craft BY SLOT with the chosen payment; then green's resolution order:
      ;; offer the chain at slot j+1 → queued free shops → end of turn
      (let [node (get-in p [:carts (:active-cart state)])
            j (:slot move)]
        (-> (apply-craft state pid node j (:pay move))
            (resolve-after-craft pid node j)))
      :craft-master
      ;; targets a SPECIFIC master-pool token: pay ITS cost (chosen payment),
      ;; remove it for everyone
      (let [node (get-in p [:carts (:active-cart state)])
            t (first (filter #(= (:id %) (:id move)) (:master-pool state)))]
        (-> state
            (spend-pay pid (:pay move))
            (drop-workers node (:pay move) pid)
            (update :master-pool (fn [mp] (vec (remove #(= (:id %) (:id t)) mp))))
            (update-in [:players pid :master-recipes] dec)
            (update-in [:players pid :mastercrafts-built] inc)
            (logln (str "builds mastercraft " (name (:id t)) " from the pool"
                        " (unscored until displayed)"))
            finish-craft-phase))

      :place-free-shop
      ;; place ONE queued free shop, drain the counter; more queued → keep placing
      (let [t (:node move)
            state (-> state
                      (update-in [:board-shops t pid] (fnil conj []) {:level :basic})
                      (update-in [:players pid :shops-left] dec)
                      (logln (str "places a FREE shop at space " t " (recipe card)"))
                      ;; blacksmith :shop-refund fires on ANY basic placement
                      (cond-> (unlocked? p :shop-refund)
                        (-> (update-in [:players pid :coins] + 2)
                            (logln "shop refund: +2 coins")))
                      (update :pending-free-shops (fnil dec 0)))]
        (if (pos? (:pending-free-shops state 0))
          state
          (free-shops-done state)))
      :skip-free-shop
      (let [state (update state :pending-free-shops (fnil dec 0))]
        (if (pos? (:pending-free-shops state 0))
          state
          (free-shops-done state)))

      ;; :lowest tie — the player CHOOSES which tied-lowest skill to raise
      :choose-lowest
      (let [state (-> state
                      (raise-skill pid (:skill move) "lowest-skill card (chosen)")
                      (award-medallions pid)
                      (update :pending-lowest (fnil dec 0)))]
        (if (pos? (:pending-lowest state 0))
          (assoc state :step :choose-lowest)
          (finish-craft-phase state)))

      ;; :free-atelier recipe — upgrade a chosen basic to an atelier FREE, then unlock
      :free-atelier
      (let [n2 (:basic-node move)]
        (-> state
            (update-in [:board-shops n2 pid]
                       (fn [es] (let [i (first (keep-indexed
                                                (fn [i e] (when (= :basic (:level e)) i)) es))]
                                  (conj (vec (concat (take i es) (drop (inc i) es)))
                                        {:level :atelier}))))
            (update-in [:players pid :shops-left] inc)
            (update-in [:players pid :ateliers-left] dec)
            (update :pending-free-atelier (fnil dec 0))
            (logln (str "free atelier: upgrades a basic at space " n2 " (recipe, no coin)"))
            (assoc :ability-return true :step :ability)))
      :skip-free-atelier
      (-> state (update :pending-free-atelier (fnil dec 0)) finish-craft-phase)

      :fire-chain
      ;; FIRE the offered chain slot (paying its own chosen cost), then offer the
      ;; NEXT slot rightward (green's cascade)
      (let [node (get-in p [:carts (:active-cart state)])
            k (:slot move)]
        (-> state
            (logln (str "fires the chain in slot " (inc k)))
            (apply-craft pid node k (:pay move))
            (resolve-after-craft pid node k)))
      :decline-chain
      ;; forgo the offered chain (ending the cascade) → drain queued effects, else end
      (finish-craft-phase state)

      :skip-craft (finish-craft-phase state))))
