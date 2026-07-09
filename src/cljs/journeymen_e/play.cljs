(ns journeymen-e.play
  "Journeymen E — playtest UI v6 (2026-07-03 designer pairing table):
   • the NO-OUTER wedge — the Academy card renders at the INNER radius of its
     wedge angle; the road arrows (drawn straight from :adj) show the outer
     fast lane SKIPPING that wedge plus the branch diving to the Academy and
     rejoining the next outer
   • blocked-sale hints; Tavern is the D→I ordered pair
   • v4 save schema (v3 states can't load into the re-paired board)
   v5 (2026-07-03 board pass):
   • OVERLAP RESOLVER — space cards can no longer hide each other's carts and
     workers: an iterative deterministic layout pass separates every card pair
     (verified over all 720 wedge orders), plus a dev-time DOM assertion on
     mount, acting-card z-index priority and hover-raise
   • green's SVG pieces — meeple silhouettes (★ badge for skilled), tinted
     cart tokens with a 1/2 badge, house/atelier shop glyphs
   • take-recipe is a BOARD interaction: click the market CARD, pick a payment
     if several, then click the target SLOT on your recipe board (empty slots
     bright, covering slots orange); Escape / click-elsewhere disarms
   • shop-worker step: 'take from here' labels on every viable source (shops
     AND ateliers), allowed colours shown as meeples in the prompt
   v4 (2026-07-03 polish pass): 2-4 player picker,
   district names on cards, payout previews on sell/craft/display, sell-blocked
   hints, recent-moves strip, header scoreboard, help overlay, guild-tinted
   player boards, responsive board scaling, game-over breakdown.
   v3 (2026-07-02 second feedback round):
   • server-side saves: every human move (and game over) snapshots to
     /journeymen/save/e2.* so finished games stay reviewable from any device
   • one REAL board panel per player (you + bots): passive, ✓/▢ abilities, grid
   • the MASTER POOL tableau (the shared masterwork race) + targeted builds
   • pending-step panels: ability chooser, chain fire/decline, free-shop clicks
   • colour-constrained shop-worker prompts; recipe cards show green's flags
   Every interaction is a move from g/enumerate-moves through g/apply-move —
   the same one engine path as the sim bench. Seat 0 human, seats 1-2 bots."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.reader :as reader]
            [clojure.string :as string]
            [journeymen-e.game :as g]
            [journeymen-e.bots :as bots]))

(defonce app (r/atom {:st nil :hist [] :guild nil :seed nil :mode nil}))

;; ── palette / icons ───────────────────────────────────────────────────────────
(def worker-css {:black "#2b2b2b" :blue "#2b6cb0" :yellow "#d69e2e" :red "#c53030"
                 :white "#f2f2ec" :grey "#8a8a8a"})
(def good-css {:durability "#4a5568" :precision "#b7791f" :innovation "#2f855a" :luxury "#7c3aed"})
(def good-icon {:durability "⛏" :precision "⚙" :innovation "💡" :luxury "💎"})
(def guild-hue {:blacksmith "#2b2b2b" :alchemist "#2b6cb0" :goldsmith "#c99700" :jeweler "#c53030"})
(defn pcolor [st pid] (or (guild-hue (get-in st [:players pid :guild])) "#666"))
(def action-title {:shop "PLACE SHOP" :atelier-pts "ATELIER +3★+1🪙"
                   :atelier-coin "ATELIER +1★+3🪙" :recipe-worker "RECIPE (pay a worker)"
                   :recipe-coin "RECIPE (pay 2🪙)" :skilled "SKILLED WORKER"})
(def action-tip
  {:shop "Place a basic shop at ANY space with room (cost = 1 per piece you have on the board), then take a worker of THIS space's take-colours from one of your shop locations — you get the worker even if you don't place."
   :atelier-pts "Upgrade one of your basics (anywhere) to an atelier for 4🪙. This space's bonus: +3 points +1 coin. Ateliers let you DISPLAY mastercrafts."
   :atelier-coin "Upgrade one of your basics (anywhere) to an atelier for 4🪙. This space's bonus: +1 point +3 coins. Ateliers let you DISPLAY mastercrafts."
   :recipe-worker "Take a face-up recipe from the market, paying 1 worker of your choice."
   :recipe-coin "Take a face-up recipe from the market, paying 2 coins."
   :skilled "Claim a marked skilled worker (★) of your choice from the reserve (2 waves). It pays as its colour; when spent it drops still marked and you grab a matching regular worker there."})

(defn- gname [k] (string/capitalize (name k)))
(defn- wname [w] (if (g/skilled? w) (str "★ skilled " (name (g/base-color w))) (name w)))
(defn- space-name [i] (:name (nth g/spaces i)))

;; ── one-time DOM setup: injected stylesheet + window-width tracking ───────────
;; inline styles can't express :hover / @keyframes, so the button hierarchy
;; (primary/secondary/tertiary), hover affordances and the acting-card pulse
;; live in ONE injected <style> tag (no external assets).
(def ^:private css
  (str
   ".jm-btn{border-radius:8px;cursor:pointer;font-size:13px;font-family:inherit;"
   "transition:filter .12s ease,transform .05s ease;}"
   ".jm-btn:hover{filter:brightness(.93);}"
   ".jm-btn:active{transform:translateY(1px);}"
   ".jm-primary{padding:8px 12px;font-weight:700;}"
   ".jm-secondary{padding:7px 10px;}"
   ".jm-tertiary{padding:6px 10px;color:#666;background:#f6f4ec;border:1px solid #ccc;}"
   ".jm-mini{padding:2px 8px;font-size:12px;}"
   "@keyframes jm-pulse{0%,100%{outline-color:rgba(26,127,55,.75);}"
   "50%{outline-color:rgba(26,127,55,.12);}}"
   ".jm-acting{outline:3px solid rgba(26,127,55,.75);outline-offset:2px;"
   "animation:jm-pulse 1.7s ease-in-out infinite;}"
   ;; hovering any space card raises it above its neighbours (inline z-index
   ;; carries the resting order, so this needs !important)
   ".jm-space:hover{z-index:9 !important;}"))
(defonce ^:private style-el
  (let [el (.createElement js/document "style")]
    (set! (.-textContent el) css)
    (.appendChild (.-head js/document) el)
    el))
(defonce win-w (r/atom (.-innerWidth js/window)))
(defonce ^:private resize-listener
  (do (.addEventListener js/window "resize" #(reset! win-w (.-innerWidth js/window))) true))
;; Escape disarms whatever interaction is armed (take-recipe, craft pay-picker,
;; cart selection, shop/atelier placement) — click-elsewhere disarms too (root)
(defonce ^:private esc-listener
  (do (.addEventListener js/window "keydown"
                         (fn [e] (when (= "Escape" (.-key e))
                                   (swap! app assoc :mode nil))))
      true))
(defonce help? (r/atom false))
(defonce setup-n (r/atom 3))   ; setup-screen player count (2-4), default 3

;; ── persistence / bots / flow ────────────────────────────────────────────────
;; v4 (2026-07-03 designer pairing table): the BOARD changed (no-outer Academy
;; wedge, new demand/action pairings) —
;; a fresh localStorage key so v3 states never load into the new engine
;; (finished v3 games were already server-saved at game over; migrate-finished!
;; still rescues FINISHED v2 games).
(def ^:private schema-ver "journeymen-e/v4")
(def ^:private ls-key schema-ver)
(defn- save! []
  (try (.setItem js/localStorage ls-key
                 (pr-str {:schema schema-ver
                          :st (:st @app) :guild (:guild @app) :seed (:seed @app)
                          :game-key (:game-key @app)}))
       (catch :default _ nil)))
(defn- rand-key
  "Per-game server snapshot key, e4-prefixed (designer pairing-table engine) so E
   games are recognisable next to green's v1.jm-* keys in :journeymen-games."
  []
  (str "e4." (.toString (js/Math.floor (* (js/Math.random) 2176782336)) 36)))
(defn- server-save!
  "Fire-and-forget snapshot POST to the organism server (green's save route; it is
   OUTSIDE wrap-csrf, so no token header). Never throws — with no server the game
   just keeps running on localStorage."
  ([] (server-save! (:game-key @app) (:st @app) (:guild @app) (:seed @app)))
  ([k st guild seed]
   (when (and k st)
     (try
       (-> (js/fetch (str "/journeymen/save/" k)
                     #js {:method "POST"
                          :headers #js {"Content-Type" "application/edn"}
                          :body (pr-str {:schema schema-ver :st st
                                         :guild guild :seed seed})})
           (.catch (fn [_] nil)))
       (catch :default _ nil)))))
(defn- migrate-finished!
  "One-time shim: a FINISHED game stranded in localStorage by pre-server-save
   builds becomes reviewable — upload it under e2.recovered-<seed>, then clear
   the slot so it never re-uploads."
  []
  (try
    (when-let [raw (.getItem js/localStorage "journeymen-e/v2")]
      (let [{:keys [st guild seed]} (reader/read-string raw)]
        (when (and st (g/game-over? st))
          (server-save! (str "e2.recovered-" seed) st guild seed)
          (.removeItem js/localStorage "journeymen-e/v2"))))
    (catch :default _ nil)))
(defn- bot-genome [pid] (nth bots/archetypes (mod (+ pid (or (:seed @app) 0)) (count bots/archetypes))))
(defn- auto-skip
  "While it's the human's turn and the ONLY legal move is a skip, apply it — keeps
   the flow smooth without hiding real choices."
  [st]
  (loop [st st n 0]
    (if (or (g/game-over? st) (pos? (:current st)) (> n 6))
      st
      (let [ms (g/enumerate-moves st)]
        (if (and (= 1 (count ms))
                 (#{:skip-pickup :skip-main :skip-craft :skip-shop-worker
                    :skip-free-shop :decline-chain :skip-ability}
                  (:type (first ms))))
          (recur (g/apply-move st (first ms)) (inc n))
          st)))))
(defn- run-bots! []
  (let [st (:st @app)]
    (when (and st (not (g/game-over? st)) (pos? (:current st)))
      (js/setTimeout
       (fn []
         (let [st (:st @app)]
           (when (and st (not (g/game-over? st)) (pos? (:current st)))
             (let [mv (bots/decide st (bot-genome (:current st)))
                   st' (g/apply-move st mv)
                   st' (if (zero? (:current st')) (auto-skip st') st')]
               (swap! app assoc :st st')
               (save!)
               (when (g/game-over? st') (server-save!))   ; finished games reviewable
               (run-bots!)))))
       220))))
(defn- advance! [st']
  (let [st' (if (zero? (:current st')) (auto-skip st') st')]
    (swap! app (fn [a] (-> a (update :hist conj (:st a)) (assoc :st st' :mode nil))))
    (save!)
    (server-save!)   ; every human move snapshots to the server (fire-and-forget)
    (run-bots!)))
(defn- apply! [mv] (when mv (advance! (g/apply-move (:st @app) mv))))
(defn- undo! []
  (swap! app (fn [a]
               (let [hist (:hist a)
                     back (or (last (filter #(zero? (:current %)) hist)) (last hist))]
                 (if back
                   (assoc a :st back :mode nil
                          :hist (vec (take-while #(not (identical? % back)) hist)))
                   a))))
  (save!))
(defn- new-game!
  "Start a fresh game: the human's guild first, then (dec n-players) DISTINCT
   bot guilds (2-4 players; g/new-game validates the count)."
  ([guild] (new-game! guild 3))
  ([guild n-players]
   (let [seed (js/Math.floor (* (js/Math.random) 1000000))
         others (vec (take (dec n-players)
                           (remove #{guild} [:goldsmith :jeweler :alchemist :blacksmith])))]
     (reset! app {:st (g/new-game seed (into [guild] others))
                  :hist [] :guild guild :seed seed :mode nil :game-key (rand-key)})
     (save!)
     (server-save!))))
(defn- play-again!
  "Back to the setup screen keeping NOTHING — clears the localStorage slot too so
   restore! can't resurrect the finished game."
  []
  (try (.removeItem js/localStorage ls-key) (catch :default _ nil))
  (reset! app {:st nil :hist [] :mode nil}))
(defn- legal [] (when-let [st (:st @app)] (when (zero? (:current st)) (g/enumerate-moves st))))
(defn- moves-of [t] (filter #(= t (:type %)) (legal)))

;; ── board geometry: green's TWO-TRACK directed rondel ────────────────────────
;; Spaces sit at their wedge's angle: OUTER track at the large radius, INNER at
;; the small one (:switch wedges and the two outer-only wedges have no inner
;; card; the NO-OUTER wedge has only its INNER card — the Academy). The ROADS
;; (directed edges, :adj) are drawn as straight arrowed lines, so the outer
;; fast lane visibly skips the Academy wedge while the branch arrows dive to
;; the Academy card and rejoin the next outer. Carts move 1–2 roads.
(def CX 430) (def CY 385) (def W 860) (def H 780)
(def R-OUTER 312) (def R-INNER 144)
(defn- wedge-angle [i] (* (/ js/Math.PI 180) (- (* i 60) 90)))   ; wedge 0 at top, clockwise
(defn- pos [i r] (let [a (wedge-angle i)] [(+ CX (* r (js/Math.cos a))) (+ CY (* r (js/Math.sin a)))]))

;; ── OVERLAP RESOLVER (2026-07-03, game-breaking fix) ─────────────────────────
;; Raw wedge geometry CAN overlap cards: a diagonal wedge's inner card sits only
;; dx≈145 from its own outer card (< card width), and two inner-track cards on
;; adjacent wedges sit just 144 apart — a cart ended HIDDEN under another card.
;; Fix: treat each card as a fixed-size envelope (CARD-W × CARD-EST-H anchored
;; CARD-TOP above its center), find conflicting pairs and separate them along
;; the cheaper axis. OUTER cards never move (the ring stays readable); INNER
;; cards get pushed (usually toward the center); as an oscillation breaker the
;; movable card(s) shrink a little every 50 iterations. Deterministic (first
;; conflicting pair by node order each step).
;; PAIRING-TABLE RETUNE (2026-07-03): the no-outer wedge makes the Academy a
;; THIRD movable inner card, and orders like Academy-wedge→Temple/Parks chain
;; five cards across one x-range — the old cheap-axis-only push deadlocked.
;; Two added rules (mirrored in scratchpad resolver-sim.js): (1) needs below a
;; half-pixel EPSILON don't count (float-stall guard); (2) a pair re-conflicting
;; on the cheap axis ESCALATES to the other axis every 3rd try, step capped at
;; 48px. Re-verified over ALL 720 wedge permutations: 0 remaining conflicts,
;; ≤193 iterations, min card width 140.
(def CARD-W 156)          ; default card width (the resolver may shrink a card)
(def CARD-EST-H 168)      ; conservative envelope height (border-box; min-height 116)
(def CARD-TOP 62)         ; a card's top edge sits this far above its center point
(def CARD-GAP 8)          ; required clearance between any two card envelopes

(defn- card-needs
  "[need-x need-y]: how much further apart two card envelopes must move, per
   axis, to clear CARD-GAP. The pair CONFLICTS iff BOTH needs are positive."
  [{x1 :x y1 :y w1 :w} {x2 :x y2 :y w2 :w}]
  (let [l1 (- x1 (/ w1 2)) l2 (- x2 (/ w2 2))
        t1 (- y1 CARD-TOP) t2 (- y2 CARD-TOP)
        gx (- (max l1 l2) (min (+ l1 w1) (+ l2 w2)))
        gy (- (max t1 t2) (min (+ t1 CARD-EST-H) (+ t2 CARD-EST-H)))]
    [(- CARD-GAP gx) (- CARD-GAP gy)]))
(defn- clamp-card
  "Keep a pushed card fully inside the board rectangle."
  [{:keys [w] :as c}]
  (-> c
      (update :x #(min (max % (+ (/ w 2) 2)) (- W (/ w 2) 2)))
      (update :y #(min (max % (+ CARD-TOP 2)) (- H (- CARD-EST-H CARD-TOP) 2)))))
(def ^:private CARD-EPS 0.5)   ; sub-half-pixel needs don't count (float-stall guard)
(defn- first-conflict [cards]
  (first (for [i (range (count cards)) j (range (inc i) (count cards))
               :let [[nx ny] (card-needs (cards i) (cards j))]
               :when (and (> nx CARD-EPS) (> ny CARD-EPS))]
           [i j nx ny])))
(defn- resolve-card-layout
  "Iterate until no two card envelopes conflict (see block comment above)."
  [cards]
  (loop [cards cards n 0 tries {}]
    (if-let [[i j nx ny] (when (< n 240) (first-conflict cards))]
      (let [c1 (cards i) c2 (cards j)]
        (if (and (pos? n) (= 49 (mod n 50)))
          ;; oscillation breaker: shrink the movable card(s) of the stuck pair
          (recur (cond-> cards
                   (:inner? c1) (update-in [i :w] #(max 116 (- % 8)))
                   (:inner? c2) (update-in [j :w] #(max 116 (- % 8))))
                 (inc n) tries)
          (let [t (inc (get tries [i j] 0))
                cheap (if (<= nx ny) :x :y)
                ;; a pair stuck on the cheap axis escalates to the OTHER axis
                ;; every 3rd try (cures the 5-card x-chain deadlock the no-outer
                ;; wedge introduced); steps capped at 48px so escalated moves
                ;; integrate instead of slamming into the board edge
                axis (if (zero? (mod t 3)) (if (= cheap :x) :y :x) cheap)
                amt (min (if (= axis :x) nx ny) 48)
                dir (fn [a b] (if (>= (axis a) (axis b)) 1 -1))]
            (recur
             (cond
               (and (:inner? c1) (:inner? c2))
               (-> cards
                   (assoc i (clamp-card (update c1 axis + (* (dir c1 c2) (/ amt 2)))))
                   (assoc j (clamp-card (update c2 axis + (* (dir c2 c1) (/ amt 2))))))
               (:inner? c1) (assoc cards i (clamp-card (update c1 axis + (* (dir c1 c2) amt))))
               (:inner? c2) (assoc cards j (clamp-card (update c2 axis + (* (dir c2 c1) amt))))
               ;; outer–outer pairs can't conflict at this geometry; stay total
               :else (-> cards
                         (update-in [i :w] #(max 116 (- % 8)))
                         (update-in [j :w] #(max 116 (- % 8)))))
             (inc n) (assoc tries [i j] t)))))
      cards)))
(defonce ^:private layout-cache (atom {}))
(defn- board-card-layout
  "node -> {:x :y :w}: the resolved card layout for this game's wedge order.
   Pure function of :wedge-order, so it's computed once and cached."
  [st]
  (let [k (:wedge-order st)]
    (or (get @layout-cache k)
        (let [base (vec (sort-by :node
                                 (mapcat (fn [i]
                                           (let [w (nth k i)
                                                 [ox oy] (pos i R-OUTER)
                                                 [ix iy] (pos i R-INNER)]
                                             ;; the no-outer wedge (Academy) has ONLY an
                                             ;; inner card — it renders at the inner
                                             ;; radius of its wedge angle (2026-07-03)
                                             (cond-> []
                                               (some? (:outer w))
                                               (conj {:node (:outer w) :x ox :y oy
                                                      :w CARD-W :inner? false})
                                               (and (some? (:inner w))
                                                    (not= :switch (:inner w)))
                                               (conj {:node (:inner w) :x ix :y iy
                                                      :w CARD-W :inner? true}))))
                                         (range 6))))
              layout (into {} (map (juxt :node #(select-keys % [:x :y :w])))
                           (resolve-card-layout base))]
          (swap! layout-cache assoc k layout)
          layout))))
(defn- warn-card-overlaps!
  "Dev-time assertion: if any two RENDERED space cards' DOM rects intersect,
   console.warn the pair (the resolver should make this unreachable)."
  []
  (let [els (vec (array-seq (.querySelectorAll js/document ".jm-space")))]
    (doseq [i (range (count els)) j (range (inc i) (count els))]
      (let [r1 (.getBoundingClientRect (nth els i))
            r2 (.getBoundingClientRect (nth els j))]
        (when (and (< (.-left r1) (.-right r2)) (< (.-left r2) (.-right r1))
                   (< (.-top r1) (.-bottom r2)) (< (.-top r2) (.-bottom r1)))
          (js/console.warn "journeymen-e OVERLAP: space cards"
                           (.getAttribute (nth els i) "data-node") "and"
                           (.getAttribute (nth els j) "data-node") "intersect"))))))
(defn- board-mount-check
  "Stable :ref callback — runs the overlap assertion once per board mount."
  [el]
  (when el (js/requestAnimationFrame warn-card-overlaps!)))

(defn- roads-svg
  "The two track guides (faint circles) + every DIRECTED road as an arrowed line
   (trimmed at both ends so the space cards don't swallow the arrowheads)."
  [st posmap]
  [:svg {:width W :height H :style {:position "absolute" :left 0 :top 0 :pointer-events "none"}}
   [:defs
    [:marker {:id "arr" :viewBox "0 0 10 10" :refX 8 :refY 5 :markerWidth 7 :markerHeight 7
              :orient "auto-start-reverse"}
     [:path {:d "M 0 0 L 10 5 L 0 10 z" :fill "#a3802f"}]]]
   ;; track guides: outer + inner (decorative — the ROADS are the truth)
   [:circle {:cx CX :cy CY :r R-OUTER :fill "none" :stroke "#eadfc2" :stroke-width 2
             :stroke-dasharray "5 8"}]
   [:circle {:cx CX :cy CY :r R-INNER :fill "none" :stroke "#eadfc2" :stroke-width 2
             :stroke-dasharray "5 8"}]
   (doall
    (for [[a bs] (:adj st) b bs
          :let [[x1 y1] (posmap a) [x2 y2] (posmap b)
                dx (- x2 x1) dy (- y2 y1)
                d (js/Math.sqrt (+ (* dx dx) (* dy dy)))
                trim (min 86 (max 18 (- (/ d 2) 16)))
                ux (/ dx d) uy (/ dy d)]]
      ^{:key (str a "-" b)}
      [:line {:x1 (+ x1 (* ux trim)) :y1 (+ y1 (* uy trim))
              :x2 (- x2 (* ux trim)) :y2 (- y2 (* uy trim))
              :stroke "#b08d3f" :stroke-width 4 :marker-end "url(#arr)"}]))])

;; ── shared bits: green's SVG pieces, inlined ─────────────────────────────────
;; The icon SVGs under /img/journeymen/icons are single-path game-icons.net
;; silhouettes; inlining the path data (green's meeple-path technique) beats
;; mask-image here — it recolors via :fill and needs no external fetch.
(def ^:private meeple-path
  "M256 54.99c-27 0-46.418 14.287-57.633 32.23-10.03 16.047-14.203 34.66-15.017 50.962-30.608 15.135-64.515 30.394-91.815 45.994-14.32 8.183-26.805 16.414-36.203 25.26C45.934 218.28 39 228.24 39 239.99c0 5 2.44 9.075 5.19 12.065 2.754 2.99 6.054 5.312 9.812 7.48 7.515 4.336 16.99 7.95 27.412 11.076 15.483 4.646 32.823 8.1 47.9 9.577-14.996 25.84-34.953 49.574-52.447 72.315C56.65 378.785 39 403.99 39 431.99c0 4-.044 7.123.31 10.26.355 3.137 1.256 7.053 4.41 10.156 3.155 3.104 7.017 3.938 10.163 4.28 3.146.345 6.315.304 10.38.304h111.542c8.097 0 14.026.492 20.125-3.43 6.1-3.92 8.324-9.275 12.67-17.275l.088-.16.08-.166s9.723-19.77 21.324-39.388c5.8-9.808 12.097-19.576 17.574-26.498 2.74-3.46 5.304-6.204 7.15-7.754.564-.472.82-.56 1.184-.76.363.2.62.288 1.184.76 1.846 1.55 4.41 4.294 7.15 7.754 5.477 6.922 11.774 16.69 17.574 26.498 11.6 19.618 21.324 39.387 21.324 39.387l.08.165.088.16c4.346 8 6.55 13.323 12.61 17.254 6.058 3.93 11.974 3.45 19.957 3.45H448c4 0 7.12.043 10.244-.304 3.123-.347 6.998-1.21 10.12-4.332 3.12-3.122 3.984-6.997 4.33-10.12.348-3.122.306-6.244.306-10.244 0-28-17.65-53.205-37.867-79.488-17.493-22.74-37.45-46.474-52.447-72.315 15.077-1.478 32.417-4.93 47.9-9.576 10.422-3.125 19.897-6.74 27.412-11.075 3.758-2.168 7.058-4.49 9.81-7.48 2.753-2.99 5.192-7.065 5.192-12.065 0-11.75-6.934-21.71-16.332-30.554-9.398-8.846-21.883-17.077-36.203-25.26-27.3-15.6-61.207-30.86-91.815-45.994-.814-16.3-4.988-34.915-15.017-50.96C302.418 69.276 283 54.99 256 54.99z")
(def ^:private cart-path
  "M400 16c-21.335 9.73-58.244 17.34-73.086 48.232-22.36 1.948-72.753 10.673-122.22 40.25-58.098 34.74-116.017 97.417-131.776 213.702l-.48 3.537-2.774 2.25c-30.87 25.002-40.657 38.937-44.416 61.153-3.536 20.9-.72 51.46-.363 101.877H328.36c3.455-16.892 10.44-29.245 12.472-41.568 2.337-14.176.19-29.938-20.812-58.547-43.078-58.683-46.853-129.458-12.916-171.28-8.654-2.765-15.09-6.887-19.458-12.546-6.115-7.924-7.4-17.006-8.57-25.884l17.848-2.352c1.112 8.446 2.38 13.88 4.97 17.237 2.59 3.356 7.31 6.472 19.55 8.46l-.022.128.172-.17 5.998 9.424c19.957 31.358 42.84 51.292 73.332 54.44l6.51.672 1.367 6.4c2.74 12.828 8.626 19.095 15.116 22.238 6.49 3.143 14.225 2.944 20.47.205 9.316-4.086 14.518-11.35 16.7-22.712 2.122-11.05.546-25.834-5.137-42.106-33.538-38.248-44.475-87.277-63.903-128.772-6.055-9.947-12.448-18.518-20.385-24.856C376.808 55.126 386.456 34.852 400 16zM214.068 34.97C179.55 35.06 146.075 43.06 96 58.58c31.146 9.92 70.397 18.9 86.037 39.01 4.463-3.017 8.94-5.88 13.418-8.56 40.51-24.22 80.387-35.286 108.23-40.04-35.854-9.477-63.047-14.094-89.617-14.023zM157.16 96.712c-1.13-.01-2.265-.01-3.402.004-30.353.37-63.1 9.745-96.647 31.283 27.186 3.672 54.67 3.724 72.58 15.398 15.9-17.92 33.144-32.634 50.677-44.668-7.548-1.244-15.292-1.938-23.207-2.017zM368 128a13.214 13.215 0 0 1 13.213 13.215A13.214 13.215 0 0 1 368 154.432a13.214 13.215 0 0 1-13.213-13.217A13.214 13.215 0 0 1 368 128zm-238.906 16.068c-36.395 1.495-68.903 6.53-104.76 24.766 33.236 7.095 50.913 13.507 65.025 33.83 11.522-22.53 25.045-41.93 39.734-58.596zM74.518 201.46C53.53 201.65 36.614 213.14 16 224c27.854 0 46.067 3.862 58.71 12.055 4.33-11.652 9.16-22.615 14.41-32.924-5.12-1.19-9.963-1.71-14.602-1.67zm-.623 36.82c-17.933 5.845-35.452 7.15-54.23 22.284 17.62 4.638 34.79 9.596 41.398 22.034 3.496-15.77 7.814-30.523 12.832-44.32zm370.142 8.57c1.617-.035 3.222.044 4.783.187l-1.64 17.926c-3.928-.36-5.513.416-5.57.465-.058.048-1.035.656-.635 5.886l-17.95 1.372c-.638-8.35 1.297-16.207 6.955-20.997 4.245-3.593 9.206-4.735 14.057-4.84zM52.215 290.723c-10.352.13-23.76 5.646-34.656 12.334 12.173 6.83 12.357 23.472 8.938 37.668 7.3-9.105 16.855-18.323 29.158-28.48 1.016-7.043 2.19-13.9 3.506-20.585-2.082-.67-4.42-.97-6.947-.937z")
(defn- meeple
  "A worker token — green's meeple silhouette tinted by worker colour, with a
   gold ★ badge when skilled. `style` (optional) merges over the wrapper — used
   for the pickup highlighting (glow ring on takeable, dimmed otherwise)."
  [w sz on-click & [style]]
  (let [skilled? (g/skilled? w)]
    [:span {:on-click on-click
            :title (wname w)
            :style (merge {:position "relative" :display "inline-block"
                           :width sz :height sz :border-radius "50%" :line-height 0
                           :cursor (if on-click "pointer" "default")
                           :margin "0 1px" :vertical-align "middle"}
                          style)}
     [:svg {:width sz :height sz :viewBox "0 0 512 512" :style {:display "block"}}
      [:path {:d meeple-path :fill (worker-css (g/base-color w))
              :stroke "#1c1c1c" :stroke-width 16 :stroke-linejoin "round"}]]
     (when skilled?
       [:span {:style {:position "absolute" :top (* sz -0.2) :right (* sz -0.18)
                       :font-size (max 8 (* sz 0.6)) :line-height 1 :color "#f4c430"
                       :text-shadow "0 0 1px #7a5c00, 0 0 1px #7a5c00"
                       :pointer-events "none"}} "★"])]))
(defn- cart-token
  "A cart as green's cart.svg silhouette tinted by player colour, with a small
   1/2 index badge. For the human's carts on a move step it arms cart selection."
  [st pid ci]
  (let [mine? (zero? pid)
        my-move? (and mine? (zero? (:current st)) (= :move (:step st)))
        sel? (and my-move? (= (get-in @app [:mode :kind]) :move)
                  (= (get-in @app [:mode :cart]) ci))]
    [:span {:on-click (when my-move?
                        (fn [e] (.stopPropagation e)
                          (swap! app assoc :mode {:kind :move :cart ci})))
            :title (str "P" pid " cart " (inc ci)
                        (when mine? " — click to move (1–2 roads, follow the arrows)"))
            :style {:position "relative" :display "inline-block" :margin "0 9px 0 0"
                    :cursor (if my-move? "pointer" "default") :line-height 0
                    :filter (when sel?
                              "drop-shadow(0 0 3px #1a7f37) drop-shadow(0 0 2px #1a7f37)")}}
     [:svg {:width 22 :height 22 :viewBox "0 0 512 512" :style {:display "block"}}
      [:path {:d cart-path :fill (pcolor st pid)
              :stroke "#1c1c1c" :stroke-width 10 :stroke-linejoin "round"}]]
     [:span {:style {:position "absolute" :right -6 :top -4 :width 12 :height 12
                     :background "#fffdf4" :border (str "1px solid " (pcolor st pid))
                     :border-radius "50%" :font-size 9 :line-height "12px"
                     :font-weight 800 :text-align "center"
                     :color (pcolor st pid) :pointer-events "none"}}
      (inc ci)]]))
(defn- shop-glyph
  "A BASIC shop is a plain house in the player's colour; an ATELIER is a
   DIFFERENT piece — gold roof, ridge flag and a gold medallion ring with a
   tiny star — so it reads as distinct by shape, not colour alone (green's
   glyph + the E medallion star)."
  [color atelier? size]
  [:svg {:width size :height size :viewBox "0 -16 100 116" :style {:display "block"}}
   (when atelier?
     [:g [:line {:x1 50 :y1 8 :x2 50 :y2 -8 :stroke "#111" :stroke-width 5}]
      [:polygon {:points "50,-8 78,0 50,8" :fill "#e0a000" :stroke "#111"
                 :stroke-width 4 :stroke-linejoin "round"}]])
   [:polygon {:points "50,8 92,42 8,42" :fill (if atelier? "#e0a000" color)
              :stroke "#111" :stroke-width 6 :stroke-linejoin "round"}]
   [:rect {:x 18 :y 42 :width 64 :height 48 :fill color :stroke "#111" :stroke-width 6}]
   (when atelier?
     [:g [:circle {:cx 50 :cy 66 :r 13 :fill "#e0a000" :stroke "#111" :stroke-width 5}]
      [:text {:x 50 :y 73 :text-anchor "middle" :font-size 20 :fill "#111"} "★"]])])
(defn- pay-meeples
  "The worker multiset of one payment option, as meeples."
  [pay sz]
  (into [:span {:style {:display "inline-flex" :gap 2 :align-items "center"}}]
        (for [[j w] (map-indexed vector pay)] ^{:key j} [meeple w sz nil])))

(defn- demand-line [demand]
  (cond
    (= :luxury (:req demand))
    [:span [:b {:style {:color (good-css :luxury)}} "💎 Luxury"]
     [:span {:style {:color "#777" :font-size 11}} " + ≤1 each ⛏⚙💡"]]
    :else
    [:span [:b {:style {:color (good-css (:req demand))}} (good-icon (:req demand)) " " (gname (:req demand))]
     [:span {:style {:color "#999"}} " → "]
     [:span {:style {:color (good-css (:opt demand)) :font-size 12}} (good-icon (:opt demand)) " opt"]]))

;; ── the space card (positioned by the resolved layout) ────────────────────────
(defn- space-card [st i {:keys [x y w]}]
  (let [{:keys [demand action]} (nth g/spaces i)
        mode (:mode @app)
        my? (zero? (:current st))
        setup? (= :setup (:phase st))
        p0 (first (:players st))
        cur-node (when (:active-cart st) (get-in p0 [:carts (:active-cart st)]))
        move-tgts (when (and my? (= :move (:step st)) (= (:kind mode) :move))
                    (set (map :dest (filter #(= (:cart %) (:cart mode)) (moves-of :move-cart)))))
        shop-tgts (when (and my? (= :main (:step st)) (= (:kind mode) :place-shop))
                    (set (map :node (filter :place? (moves-of :action-shop)))))
        atelier-tgts (when (and my? (= :main (:step st)) (= (:kind mode) :atelier))
                       (set (map :basic-node (filter #(= (:atelier-kind mode) (:kind %))
                                                     (moves-of :action-atelier)))))
        free-tgts (when (and my? (= :free-shop (:step st)))
                    (set (map :node (moves-of :place-free-shop))))
        fa-tgts (when (and my? (= :free-atelier (:step st)))
                  (set (map :basic-node (moves-of :free-atelier))))
        worker-src? (and my? (= :shop-worker (:step st))
                         (boolean (some #(= i (:from %)) (moves-of :shop-worker))))
        highlight (cond (and setup? my?) "#1a7f37"
                        (and move-tgts (move-tgts i)) "#1a7f37"
                        (and shop-tgts (shop-tgts i)) "#0b57d0"
                        (and free-tgts (free-tgts i)) "#0b57d0"
                        (and fa-tgts (fa-tgts i)) "#7c3aed"
                        (and atelier-tgts (atelier-tgts i)) "#7c3aed"
                        worker-src? "#b8860b")
        on-card-click
        (cond (and setup? my?) #(apply! {:type :place-cart :node i})
              (and move-tgts (move-tgts i))
              #(apply! (first (filter (fn [m] (and (= (:cart m) (:cart mode)) (= (:dest m) i)))
                                      (moves-of :move-cart))))
              (and shop-tgts (shop-tgts i))
              #(apply! (first (filter (fn [m] (and (:place? m) (= (:node m) i)))
                                      (moves-of :action-shop))))
              (and free-tgts (free-tgts i))
              #(apply! {:type :place-free-shop :node i})
              (and fa-tgts (fa-tgts i))
              #(apply! (first (filter (fn [m] (= i (:basic-node m))) (moves-of :free-atelier))))
              (and atelier-tgts (atelier-tgts i))
              #(apply! (first (filter (fn [m] (and (= (:atelier-kind mode) (:kind m))
                                                   (= (:basic-node m) i)))
                                      (moves-of :action-atelier)))))]
    [:div {:class (str "jm-space" (when (and my? (not setup?) (= i cur-node)) " jm-acting"))
           :data-node i
           :on-click on-card-click
           ;; border-box: :width w IS the full rendered footprint (padding and
           ;; border included) — the resolver's envelope must match the DOM
           :style {:position "absolute" :left (- x (/ w 2)) :top (- y CARD-TOP)
                   :width w :min-height 116 :box-sizing "border-box"
                   :border (str "2px solid " (or highlight (if (= i cur-node) "#555" "#c9bd9c")))
                   :box-shadow (cond highlight (str "0 0 8px " highlight)
                                     (= i cur-node) "0 0 6px #5558")
                   :border-radius 12 :padding "5px 7px" :background "#fffdf4"
                   :cursor (if on-card-click "pointer" "default") :font-size 12
                   ;; the ACTING card always sits on top; highlighted targets
                   ;; above resting cards; hover raises above everything (CSS)
                   :z-index (cond (= i cur-node) 6 highlight 4 :else 2)}}
     ;; :shop-worker step — every viable source (your shop OR ATELIER holding a
     ;; takeable colour) gets a floating label; ateliers explicitly count
     (when worker-src?
       [:div {:style {:position "absolute" :top -17 :left "50%"
                      :transform "translateX(-50%)" :background "#b8860b"
                      :color "#fff" :font-size 10 :font-weight 700
                      :padding "1px 8px" :border-radius 8 :white-space "nowrap"
                      :box-shadow "0 1px 3px #0006" :pointer-events "none"}}
        (str "take from here"
             (when (g/player-atelier-at? st 0 i) " (atelier ✓)"))])
     ;; :place-shop step — show each space's COST, or WHY it can't take a shop
     ;; (bug fix 2026-07-04: coin-clarity + Parks is via its own action only)
     (when (and my? (= :main (:step st)) (= (:kind mode) :place-shop))
       (let [blk (g/shop-build-blocker st p0 cur-node i)
             ;; cost is the ACTION space's cost (Parks action = 2 grey, else coins) —
             ;; the shop places ANYWHERE, so every legal target shows the same cost
             cost-txt (if (g/shop-worker-cost cur-node) "2 grey" (str (g/build-cost st p0) "🪙"))
             [txt bg] (case blk
                        nil [(str "place: " cost-txt " ✓") "#1a7f37"]
                        :need-coins [(str "needs " (g/build-cost st p0) "🪙") "#b3261e"]
                        :need-2-grey ["needs 2 grey" "#b3261e"]
                        :space-full ["full" "#8a7a4a"]
                        :no-shops-left ["no shops left" "#8a7a4a"]
                        ["—" "#8a7a4a"])]
         [:div {:style {:position "absolute" :top -15 :left "50%" :transform "translateX(-50%)"
                        :background bg :color "#fff" :font-size 9 :font-weight 700
                        :padding "1px 6px" :border-radius 7 :white-space "nowrap"
                        :box-shadow "0 1px 2px #0005" :pointer-events "none" :z-index 5}}
          txt]))
     [:div {:style {:display "flex" :justify-content "space-between" :align-items "baseline"}}
      [:span [:b {:style {:font-size 13 :color "#4a4433"}} (space-name i)]
       [:sub {:style {:color "#aaa" :font-size 9 :margin-left 3}} "space " i]]
      (let [cap (g/node-capacity st i)
            entries (apply concat (vals (get-in st [:board-shops i])))
            nb (count (filter #(= :basic (:level %)) entries))
            na (count (filter #(= :atelier (:level %)) entries))]
        [:span {:style {:color "#999" :font-size 10}
                :title (str "Basic-shop slots: " nb "/" cap " (4 per space minus blocked ⛔). "
                            "Ateliers (⚒) are UPGRADES — they DON'T use a basic slot, so a space "
                            "can hold SEVERAL." (when (pos? na) (str " Ateliers here: " na ".")))}
         nb "/" cap "🏠" (when (pos? na) (str " · " na "⚒"))])]
     [:div {:title "SELL here (needs YOUR shop): required good first, optional may ride along"}
      (demand-line demand)]
     [:div {:style {:color "#6a5a2a" :font-size 11 :margin "2px 0" :font-weight 600}
            :title (if-let [tk (:take (nth g/spaces i))]
                     (str (action-tip action) " THIS space's worker pickup is colour-limited: "
                          (string/join "/" (map name (sort tk))) ".")
                     (action-tip action))}
      "▸ " (action-title action)
      (when-let [tk (:take (nth g/spaces i))]
        [:span {:style {:font-weight 400 :color "#999"}}
         " (" (string/join "/" (map name (sort tk))) ")"])]
     ;; explicit SLOT view (bug fix 2026-07-04): basic slots fill then block, and
     ;; ateliers sit in their own gold group (they don't use a basic slot)
     (let [entries (for [[pid es] (sort (get-in st [:board-shops i])) e es] [pid e])
           basics (filter #(= :basic (:level (second %))) entries)
           ateliers (filter #(= :atelier (:level (second %))) entries)
           cap (g/node-capacity st i)
           blocked (get-in st [:blocked i] 0)]
       [:div {:style {:min-height 16 :display "flex" :align-items "center" :gap 2 :flex-wrap "wrap"}}
        (doall
         (concat
          (for [[k [pid _]] (map-indexed vector basics)]
            ^{:key (str "b" k)}
            [:span {:title (str "P" pid " basic shop")} [shop-glyph (pcolor st pid) false 16]])
          (for [k (range (max 0 (- cap (count basics))))]
            ^{:key (str "e" k)}
            [:span {:title "empty basic slot" :style {:display "inline-block" :width 13 :height 13
                                                      :border "1.5px dashed #cbb" :border-radius 3}}])
          (for [k (range blocked)]
            ^{:key (str "x" k)} [:span {:title "blocked slot (setup)" :style {:font-size 12}} "⛔"])))
        (when (seq ateliers)
          [:span {:style {:margin-left 3 :padding-left 4 :border-left "1px solid #e0d8c0"}
                  :title "ateliers (upgrades — display venues)"}
           (doall (for [[k [pid _]] (map-indexed vector ateliers)]
                    ^{:key (str "a" k)} [shop-glyph (pcolor st pid) true 17]))])])
     [:div {:style {:min-height 18}}
      (doall
       (for [[j w] (map-indexed vector (get-in st [:board-workers i]))
             :let [pickup-mv (when (and my? (= :pickup (:step st)))
                               (first (filter #(and (or (and (= :pickup (:type %)) (= i cur-node))
                                                        (and (= :pickup-adjacent (:type %)) (= i (:node %))))
                                                    (= w (:color %)))
                                              (legal))))
                   sw-mv (when (and my? (= :shop-worker (:step st)))
                           (first (filter #(and (= i (:from %)) (= w (:color %)))
                                          (moves-of :shop-worker))))
                   mv (or pickup-mv sw-mv)
                   ;; PICKUP HIGHLIGHTING: during :pickup / :shop-worker, legally
                   ;; takeable workers glow; every non-viable worker on the board
                   ;; dims (incl. wrong :take colours on the shop-worker step)
                   take-step? (and my? (not setup?) (#{:pickup :shop-worker} (:step st)))]]
         ^{:key j}
         [meeple w 15 (when mv #(apply! mv))
          (when take-step?
            (if mv
              {:box-shadow "0 0 0 2.5px #1a7f37, 0 0 9px 2px #1a7f37aa" :cursor "pointer"}
              {:opacity 0.35}))]))]
     [:div {:style {:min-height 22 :padding-top 2}}
      (doall
       (for [[pid p] (map-indexed vector (:players st))
             [ci n] (map-indexed vector (:carts p))
             :when (= n i)]
         ^{:key (str pid ci)}
         [cart-token st pid ci]))]]))

(defn- board
  "The ring board scales to the viewport: transform-scale from the top-left, the
   wrapper sized to the SCALED footprint so layout leaves no gap (no scroll hack)."
  [st]
  (let [layout (board-card-layout st)
        posmap (into {} (map (fn [[n c]] [n [(:x c) (:y c)]])) layout)
        scale (min 1 (max 0.3 (/ (- @win-w 24) W)))]
    [:div {:style {:width (* W scale) :height (* H scale) :margin "0 auto" :max-width "100%"}}
     [:div {:ref board-mount-check
            :style {:position "relative" :width W :height H
                    :transform (str "scale(" scale ")") :transform-origin "top left"}}
      [roads-svg st posmap]
      (doall (for [i (range (count g/spaces))] ^{:key i} [space-card st i (layout i)]))]]))

;; ── tracks + dividends ────────────────────────────────────────────────────────
(defn- tracks-panel [st]
  [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap" :margin "6px 0"}}
   (doall
    (for [gd g/good-types
          :let [n (get-in st [:tracks gd]) th (g/track-threshold gd)
                divs (get-in st [:displays gd] [])]]
      ^{:key gd}
      [:div {:title (str "Sale track: each sale puts ONE sold token here (seller's choice). "
                         "At " th " the track is FULL (2 full tracks end the game; +2 overfill). "
                         "Each ★ mastercraft displayed here earns its owner +1★+1🪙 per token added.")
             :style {:border (if (>= n th) "2px solid #c00" "1px solid #ccc")
                     :border-radius 8 :padding "3px 8px"
                     :background (if (>= n th) "#ffe3e3" "#fff")
                     :box-shadow (when (>= n th) "0 0 6px #c0333340")}}
       [:div {:style {:font-size 12 :font-weight 700 :color (good-css gd)}}
        (good-icon gd) " " (gname gd) " " n "/" th
        (when (>= n th)
          [:span {:style {:background "#c00" :color "#fff" :border-radius 4 :padding "0 5px"
                          :font-size 10 :margin-left 4 :vertical-align "middle"}} "FULL"])]
       [:div (doall (for [i (range (+ th g/track-overfill))]
                      ^{:key i} [:span {:style {:display "inline-block" :width 13 :height 13 :margin 1
                                                :border (if (< i th) "1px solid #999" "1px dashed #c66")
                                                :background (if (< i n) (good-css gd) "#faf8f2")}}]))]
       (when (seq divs)
         [:div {:style {:font-size 11}}
          (doall (for [[i d] (map-indexed vector divs)]
                   ^{:key i} [:span {:style {:color (pcolor st (:pid d)) :font-weight 700 :margin-right 4}}
                              "★P" (:pid d)]))])]))])

;; ── contextual step panels ────────────────────────────────────────────────────
(defn- payout-str
  "'+N★ +M🪙' for a goods map at the player's CURRENT ranks: D/P/I pay points,
   luxury pays coins — the payout preview co-located on every sell/craft button."
  [p goods]
  (let [v (fn [gd n] (* n (g/grid-value (:guild p) gd (get-in p [:skills gd]))))
        pts (reduce + 0 (for [[gd n] goods :when (and (pos? n) (not= gd :luxury))] (v gd n)))
        coins (v :luxury (get goods :luxury 0))]
    (string/join " " (cond-> []
                       (pos? pts) (conj (str "+" pts "★"))
                       (pos? coins) (conj (str "+" coins "🪙"))))))

(defn- sell-buttons [st]
  (let [p (first (:players st))]
    (doall
     (for [[k mv] (map-indexed vector (moves-of :sell))]
       ^{:key k}
       [:button {:class "jm-btn jm-primary"
                 :on-click #(apply! mv)
                 :title (if (:boost mv)
                          "Innovation boost (passive): discard a held 💡 token — each good TYPE sold levels its skill TWICE."
                          "Sell: pays grid value per token, levels each good type once. ONE sold token (your choice) goes on its track.")
                 :style {:border (if (:boost mv) "2px solid #7c3aed" "2px solid #1a7f37")
                         :background (if (:boost mv) "#f4ecff" "#eefaee")}}
        "SELL " (string/join " + " (for [[gd n] (:goods mv)] (str n "×" (good-icon gd))))
        " → " [:b (payout-str p (:goods mv))]
        [:span {:style {:font-weight 400 :font-size 11 :color "#777"}} " (+level)"]
        [:span {:style {:font-weight 400 :font-size 11 :color "#777"}}
         (if (:track mv) (str " · " (good-icon (:track mv)) "→track") " · no track open")]
        (when (:boost mv) " ✦ BOOST (discard 💡)")]))))

(defn- sell-blocked-hint
  "When the :main step offers NO sell, one compact line saying WHY (derived from
   state: your shop here? the required good in hand?) — legibility-is-local."
  [st node]
  (when (empty? (moves-of :sell))
    (let [p (first (:players st))
          demand (get-in g/spaces [node :demand])
          req (:req demand)
          have? (pos? (get-in p [:tokens req] 0))
          shop? (g/player-shop-at? st 0 node)
          lack (str "you lack the required good (" (good-icon req) ")")
          why (cond (and (not shop?) (not have?))
                    (str "no shop of yours here and " lack)
                    (not shop?) "no shop of yours here"
                    (not have?) lack)]
      (when why
        [:div {:style {:font-size 11 :color "#a05a2c" :margin "1px 0 3px"}}
         "No sale possible: " why "."]))))

(defn- main-panel [st]
  (let [p (first (:players st))
        node (get-in p [:carts (:active-cart st)])
        act (:action (nth g/spaces node))
        mode (:mode @app)
        ms (legal)]
    [:div
     [:div {:style {:font-size 12 :color "#666" :margin "2px 0"}}
      "You are at " [:b {:style {:color "#4a4433"}} (space-name node)]
      " (space " node ") — sell here (needs your shop), use the space's action, or skip."]
     [sell-blocked-hint st node]
     [:div {:style {:display "flex" :gap 6 :flex-wrap "wrap"}}
      (sell-buttons st)
      (doall (for [[k mv] (map-indexed vector (moves-of :display))
                   :let [gd (:track mv)
                         pts (g/grid-value (:guild p) gd (get-in p [:skills gd]))]]
               ^{:key (str "d" k)}
               [:button {:class "jm-btn jm-primary"
                         :on-click #(apply! mv)
                         :title "Display a built mastercraft on this track (instead of selling): scores its grid value now, then +1★+1🪙 every time a token lands there."
                         :style {:border "2px solid #7c3aed" :background "#f6eeff"}}
                "DISPLAY ★ → " (good-icon gd) " track"
                [:span {:style {:font-weight 400 :font-size 11 :color "#555"}}
                 " +" pts "★ now, then +1★+1🪙 per feed"]]))
      (case act
        :shop
        (when (some :place? (moves-of :action-shop))
          [:button {:class "jm-btn jm-secondary"
                    :on-click #(swap! app assoc :mode {:kind :place-shop})
                    :title (action-tip :shop)
                    :style {:border "1.5px solid #0b57d0"
                            :background (if (= (:kind mode) :place-shop) "#dbe9ff" "#eef4ff")}}
           (if (= (:kind mode) :place-shop)
             "…click a green space (cost shown on each); red/grey = why not"
             (if (g/shop-worker-cost node)
               "PLACE SHOP anywhere — Parks action: 2 grey (coin-free) + take worker"
               (str "PLACE SHOP (" (g/build-cost st p) "🪙 anywhere) + take worker")))])
        (:atelier-pts :atelier-coin)
        (when (seq (moves-of :action-atelier))
          [:button {:class "jm-btn jm-secondary"
                    :on-click #(swap! app assoc :mode {:kind :atelier :atelier-kind act})
                    :title (action-tip act)
                    :style {:border "1.5px solid #7c3aed"
                            :background (if (= (:kind mode) :atelier) "#efe3ff" "#f6eeff")}}
           (if (= (:kind mode) :atelier)
             "…click one of your highlighted basics to upgrade"
             (str "UPGRADE to atelier (4🪙, " (if (= act :atelier-pts) "+3★+1🪙" "+1★+3🪙") ")"))])
        (:recipe-coin :recipe-worker) nil
        :skilled
        (doall (for [[k mv] (map-indexed vector (moves-of :action-skilled))]
                 ^{:key (str "sk" k)}
                 [:button {:class "jm-btn jm-secondary"
                           :on-click #(apply! mv)
                           :title (action-tip :skilled)
                           :style {:border "1.5px solid #b8860b" :background "#fdf6e3"}}
                  "CLAIM ★ " (gname (:color mv))]))
        nil)
      (when (and (= act :shop) (some #(and (= :action-shop (:type %)) (not (:place? %))) ms))
        [:button {:class "jm-btn jm-secondary"
                  :on-click #(apply! (first (filter (fn [m] (and (= :action-shop (:type m)) (not (:place? m)))) ms)))
                  :title "Skip the placement — you still take a worker from one of your shop locations."
                  :style {:border "1px solid #999" :background "#fff"}}
         "No shop — just take the worker"])
      [:button {:class "jm-btn jm-tertiary"
                :on-click #(apply! {:type :skip-main})}
       "Skip"]]
     (when (#{:recipe-coin :recipe-worker} act)
       [:div {:style {:font-size 12 :color "#0b57d0" :margin-top 4}}
        "⬇ take a recipe: click a recipe CARD in the market below ("
        (if (= act :recipe-coin) "costs coins" "costs 1 worker — you pick which")
        "), then click the SLOT on your recipe board it should go to."])]))

(defn- recipe-flags
  "Flag icons for a recipe card, each tooltipped (legibility-is-local)."
  [rc]
  (let [fl (:flags rc #{})]
    [:span
     (when (contains? fl :chain)
       [:span {:title "CHAIN — never crafted on its own: after each successful craft you may FIRE it (paying its own cost)."} " 🔗"])
     (when (contains? fl :free-shop)
       [:span {:title "On craft: place a basic shop FREE at any space with room."} " 🏠"])
     (when (contains? fl :bonus-skill)
       [:span {:title "On craft: +1 level of the output good's skill."} " ⏫"])
     (when (contains? fl :lowest)
       [:span {:title "On craft: +1 level of your single LOWEST skill (you choose on a tie)."} " ⤒"])
     ;; NEW recipe abilities (2026-07-05)
     (when (contains? fl :free-atelier)
       [:span {:title "On craft: upgrade one of your basic shops to an ATELIER for FREE, then unlock a guild ability."
               :style {:color "#b07f1e"}} " ⚒"])
     (when (contains? fl :coin-per-good)
       [:span {:title "On craft: +1 coin per DISTINCT NON-LUXURY good held (⛏/⚙/💡, up to 3) — a hard luxury alternative."} " 🪙ᐩ"])
     (when (contains? fl :grey-swap)
       [:span {:title "On craft: the workers you spent LEAVE the game (skilled return to the reserve); 2 grey drop on the space instead."} " ♻"])
     (when (:once? rc)
       [:span {:title "One-time: the card is discarded after it's crafted." :style {:color "#a5324a" :font-weight 700}} " ①"])]))

;; ── the recipe SLOT BOARD (green's 4-row × 2-col board, ported 2026-07-03) ────
(defn- recipe-cell
  "One slot of the 8-slot board (interleaved: even j = BASE column, odd =
   ACQUIRED). ALL slots coverable. Human interactions:
     • an armed market take (mode :take-recipe) → click covers THIS slot
     • :craft step → craftable slots glow; click crafts, or opens the inline
       PAY PICKER when several distinct payments exist (green 2026-06-29)
     • :chain step → the OFFERED slot lights purple (fire/decline in the panel)"
  [st p j you?]
  (let [rc (get-in p [:recipe-slots j])
        base? (even? j)
        my-turn? (and you? (zero? (:current st)) (not (g/game-over? st)))
        mode (:mode @app)
        take-mv (when (and my-turn? (= :take-recipe (:kind mode)))
                  (first (filter #(and (= (:idx mode) (:idx %)) (= j (:slot %))
                                       (= (:pay mode) (:pay %))
                                       (= (:color mode) (:color %)))
                                 (moves-of :action-recipe))))
        craft-mvs (when (and my-turn? (= :craft (:step st)))
                    (vec (filter #(= j (:slot %)) (moves-of :craft))))
        chain? (and rc (contains? (:flags rc #{}) :chain))
        offered? (and you? (= :chain (:step st)) (= j (get-in st [:pending-chain :next])))
        multi? (> (count craft-mvs) 1)
        armed? (and (= :craft-pay (:kind mode)) (= j (:slot mode)))
        ;; clicks stopPropagation: the root's click-elsewhere disarm must not
        ;; swallow the craft-pay arming (root handlers run AFTER children)
        click (cond take-mv (fn [e] (.stopPropagation e) (apply! take-mv))
                    (and multi? (not armed?))
                    (fn [e] (.stopPropagation e)
                      (swap! app assoc :mode {:kind :craft-pay :slot j}))
                    (= 1 (count craft-mvs))
                    (fn [e] (.stopPropagation e) (apply! (first craft-mvs)))
                    :else nil)]
    [:div
     [:div {:on-click click
            :title (cond offered? "This chain is OFFERED — fire or decline it in the panel (it pays its own cost)."
                         (and multi? (not armed?)) "Craftable — click to choose WHICH workers to spend (several payment options)."
                         (seq craft-mvs) "Craftable now — click to craft (a 🔗 chain in the NEXT slot is then offered)."
                         (and take-mv rc) "⚠ Click to COVER this recipe with the taken one (replaces it)."
                         take-mv "Click to place the taken recipe in this EMPTY slot."
                         chain? (str "CHAIN — never crafted alone: it fires (opt-in, paying its own cost) when you craft slot " j " to its LEFT.")
                         rc (str "Inputs: " (string/join " + " (map name (:inputs rc)))))
            :style {:display "flex" :align-items "center" :gap 4 :min-height 26
                    :padding "2px 5px" :border-radius 6 :font-size 12
                    ;; take-recipe targets: EMPTY slots bright blue, occupied
                    ;; slots a dimmer orange warning (placing there COVERS)
                    :border (cond offered? "2px solid #7c3aed"
                                  armed? "2px solid #b8860b"
                                  (seq craft-mvs) "2px solid #1a7f37"
                                  (and take-mv rc) "2px dashed #c05621"
                                  take-mv "2px solid #0b57d0"
                                  base? "1px solid #cfe0c8"
                                  :else "1px solid #ddd")
                    :box-shadow (cond offered? "0 0 7px #7c3aed88"
                                      armed? "0 0 6px #b8860b77"
                                      (seq craft-mvs) "0 0 6px #1a7f3777"
                                      (and take-mv rc) "0 0 4px #c0562155"
                                      take-mv "0 0 8px #0b57d088")
                    :background (cond offered? "#f1e6ff" (seq craft-mvs) "#eefaee"
                                      (and take-mv rc) "#fff3e8"
                                      take-mv "#eef4ff" chain? "#f3ecff"
                                      (and rc base?) "#f2f8f0" rc "#fdfbf2" :else "#fafafa")
                    :cursor (when click "pointer")}}
      [:span {:style {:font-size 9 :color "#aaa" :min-width 10}} (inc j)]
      (if rc
        [:span {:style {:display "inline-flex" :align-items "center" :gap 2}}
         (pay-meeples (:inputs rc) 13)
         [:span {:style {:color "#999"}} "→"]
         (if (seq (:outputs rc))
           [:b (string/join "+" (map good-icon (:outputs rc)))]
           [:span {:style {:color "#888" :font-size 11}} "—"])
         [recipe-flags rc]
         (when (and multi? (not armed?))
           [:span {:style {:font-size 10 :color "#b8860b" :font-weight 700 :margin-left 2}}
            (count craft-mvs) " pays ▾"])
         (when offered?
           [:span {:style {:font-size 10 :color "#7c3aed" :font-weight 700}} " 🔗 offered"])
         (when (and take-mv rc)
           [:span {:style {:font-size 10 :color "#c05621" :font-weight 700}} " ⚠ covers"])]
        [:span {:style {:color (if take-mv "#0b57d0" "#bbb")
                        :font-weight (if take-mv 700 400)}}
         (if take-mv "◉ place here" "empty")])]
     ;; the inline PAY PICKER: one row per distinct payment multiset
     (when (and armed? (seq craft-mvs))
       [:div {:on-click (fn [e] (.stopPropagation e))
              :style {:margin "2px 0 3px" :padding 4 :border "1px solid #d8b84a"
                      :border-radius 6 :background "#fffaf0"}}
        [:div {:style {:font-size 10 :color "#7a5c00" :font-weight 700}} "Pay with:"]
        (doall (for [[k mv] (map-indexed vector craft-mvs)]
                 ^{:key k}
                 [:div {:on-click #(apply! mv)
                        :style {:display "flex" :align-items "center" :gap 4 :padding "2px 5px"
                                :margin "2px 0" :cursor "pointer" :border "1px solid #e0cf9a"
                                :border-radius 5 :background "#fff"}}
                  (pay-meeples (:pay mv) 15)
                  [:span {:style {:font-size 10 :color "#888"}}
                   (string/join "+" (map wname (:pay mv)))]]))
        [:div {:style {:font-size 10 :color "#888" :cursor "pointer" :margin-top 2}
               :on-click #(swap! app assoc :mode nil)} "↩ cancel"]])]))

(defn- recipe-slot-board
  "The 8 recipe slots as green's 4×2 grid: BASE column (seeded, coverable) and
   ACQUIRED column. A 🔗 chain fires off crafting the slot to its LEFT (slot k
   offers slot k+1 — the cascade runs rightward through the numbering)."
  [st p you?]
  [:div {:style {:min-width 230 :max-width 300}}
   [:div {:style {:display "flex" :gap 8 :font-size 9 :font-weight 700 :margin-bottom 2}}
    [:div {:style {:flex 1 :color "#5a7a55"}} "BASE (seeded, coverable)"]
    [:div {:style {:flex 1 :color "#6a5a8a"}} "ACQUIRED (🔗 fires off its LEFT ←)"]]
   (into [:div {:style {:display "grid" :grid-template-columns "1fr 1fr" :gap 4}}]
         (mapcat (fn [row]
                   [^{:key (str "b" row)} [recipe-cell st p (* 2 row) you?]
                    ^{:key (str "a" row)} [recipe-cell st p (inc (* 2 row)) you?]])
                 (range 4)))])

(defn- master-build-buttons
  "The mastercraft builds (the shared-pool race): one row per targeted token,
   with a compact PAY PICKER when it can be paid more than one way."
  [st p]
  (let [by-id (group-by :id (moves-of :craft-master))]
    (doall
     (for [[id mvs] (sort by-id)
           :let [cost (get g/master-cost id)]]
       ^{:key id}
       [:span {:style {:display "inline-flex" :align-items "center" :gap 4
                       :border "1.5px solid #7c3aed" :border-radius 8 :padding "3px 7px"
                       :background "#f1e6ff" :font-size 12}}
        [:span {:title (str "Consume a ◆ master recipe + this token's worker cost ("
                            (string/join " + " (map name cost))
                            "). Removes it from the shared pool — a race. Scores NOTHING until displayed.")}
         "BUILD ★ " (name id) " "]
        (if (= 1 (count mvs))
          [:button {:class "jm-btn jm-secondary jm-mini"
                    :on-click #(apply! (first mvs))
                    :style {:border "1px solid #7c3aed" :background "#fff"}}
           "pay " (pay-meeples (:pay (first mvs)) 13)]
          ;; several distinct payments → the compact pay-picker
          (doall (for [[k mv] (map-indexed vector mvs)]
                   ^{:key k}
                   [:button {:class "jm-btn jm-secondary jm-mini"
                             :title (str "pay " (string/join "+" (map wname (:pay mv))))
                             :on-click #(apply! mv)
                             :style {:border "1px solid #7c3aed" :background "#fff"}}
                    (pay-meeples (:pay mv) 13)])))]))))

(defn- craft-panel [st]
  (let [p (first (:players st))
        crafts (moves-of :craft)]
    [:div
     [:div {:style {:font-size 12 :color "#666" :margin "2px 0"}}
      "CRAFT (works anywhere): pay workers, take goods from the market pool. Then the turn ends. "
      (if (seq crafts)
        [:b {:style {:color "#1a7f37"}}
         "Click a glowing recipe on YOUR board below (several payments → a pay picker opens)."]
        [:span {:style {:color "#a05a2c"}} "Nothing craftable (workers or market pool short)."])]
     [:div {:style {:display "flex" :gap 6 :flex-wrap "wrap" :align-items "center"}}
      (master-build-buttons st p)
      [:button {:class "jm-btn jm-primary"
                :on-click #(apply! {:type :skip-craft})
                :style {:border "2px solid #333" :background "#333" :color "#fff"}}
       "End turn ▸"]]]))

(defn- ability-panel
  "The :ability pending step — building an atelier unlocked ONE guild ability;
   pick which (a real choice, never auto-skipped)."
  [st]
  (let [p (first (:players st))
        by-id (into {} (map (juxt :id identity)) (get-in g/guilds [(:guild p) :ateliers]))]
    [:div
     [:div {:style {:font-size 13 :margin-bottom 4}}
      [:b "ATELIER BUILT — "] "unlock ONE of your guild's abilities:"]
     [:div {:style {:display "flex" :gap 6 :flex-wrap "wrap"}}
      (doall
       (for [[k mv] (map-indexed vector (moves-of :pick-ability))
             :let [a (by-id (:id mv))]]
         ^{:key k}
         [:button {:class "jm-btn jm-secondary"
                   :on-click #(apply! mv)
                   :title (:text a)
                   :style {:border "1.5px solid #7c3aed" :background "#f6eeff"
                           :font-size 12 :max-width 240 :text-align "left"}}
          [:div [:b (name (:id mv))]]
          [:div {:style {:color "#666" :font-size 11}} (:text a)]]))]]))

(defn- chain-panel
  "The :chain pending step — green's cascade: the slot to the RIGHT of what you
   just crafted is OFFERED. Fire it (one button per distinct payment — the pay
   picker) or decline. Firing then offers the NEXT slot rightward."
  [st]
  (let [p (first (:players st))
        k (get-in st [:pending-chain :next])
        rc (get-in p [:recipe-slots k])
        fires (moves-of :fire-chain)]
    [:div {:style {:border "1.5px solid #7c3aed" :border-radius 10 :padding "8px 10px"
                   :background "#f6f0ff"}}
     [:div {:style {:font-size 13 :margin-bottom 6}}
      [:b {:style {:color "#6d28d9"}} "🔗 Chain offered"]
      " — the recipe in slot " (inc k) " (glowing on your board) can ride off your craft:"]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap" :align-items "center"}}
      [:span {:style {:font-size 12 :border "1px solid #c9b3f0" :border-radius 8
                      :padding "3px 8px" :background "#fff"}}
       (pay-meeples (:inputs rc) 14) " → "
       (if (seq (:outputs rc)) (string/join "+" (map good-icon (:outputs rc))) "effect")
       [recipe-flags rc]]
      (doall
       (for [[i mv] (map-indexed vector fires)]
         ^{:key i}
         [:button {:class "jm-btn jm-secondary"
                   :on-click #(apply! mv)
                   :title "Fire this chain, paying its own cost. Then the NEXT slot rightward is offered if it too is a payable chain."
                   :style {:border "1.5px solid #7c3aed" :background "#efe3ff" :font-weight 700}}
          "Fire — pay " (pay-meeples (:pay mv) 14)]))
      [:button {:class "jm-btn jm-tertiary jm-mini"
                :on-click #(apply! {:type :decline-chain})
                :title "Skip the chain and finish your turn."}
       "skip ▸"]]]))

(defn- step-panel [st]
  (let [step (:step st)]
    [:div {:style {:border "1px solid #d8d0b8" :border-radius 10 :padding 8 :margin "8px 0"
                   :background "#fcfaf3" :min-height 52}}
     (case step
       :move [:div {:style {:font-size 13}}
              [:b "MOVE: "] "click one of your green cart tokens, then a highlighted space — "
              [:b "1–2 roads along the arrows"] " (two directed tracks; switches connect them)."
              (when-not (:mode @app) [:span {:style {:color "#999"}} " (pick a cart first)"])]
       :pickup [:div {:style {:font-size 13}}
                [:b "PICK UP: "] "click a worker on your space (free) or on a neighbouring space (1🪙), or "
                [:button {:class "jm-btn jm-tertiary jm-mini"
                          :on-click #(apply! {:type :skip-pickup})
                          :style {:margin-left 4}} "skip"]]
       :main [main-panel st]
       :shop-worker
       (let [an (:shop-action-node st)
             tk (get-in g/spaces [an :take])]
         [:div {:style {:font-size 13}}
          [:b "SHOP WORKER: "] "take a worker of "
          (if (seq tk)
            [:span
             (doall (for [c (sort tk)] ^{:key c} [meeple c 16 nil]))
             [:b " " (string/join "/" (map name (sort tk)))]
             (when an [:span {:style {:color "#888"}}
                       " (" (space-name an) "'s colours)"])]
            [:b "any colour"])
          " from any gold-labelled “take from here” space — every space where you own a shop "
          [:b "or atelier"] " counts (ateliers too!). Click the glowing worker there, or "
          [:button {:class "jm-btn jm-tertiary jm-mini"
                    :on-click #(apply! {:type :skip-shop-worker})
                    :style {:margin-left 4}} "skip"]])
       :free-shop [:div {:style {:font-size 13}}
                   [:b "FREE SHOP (recipe card): "]
                   "click any highlighted space to place a basic shop for FREE, or "
                   [:button {:class "jm-btn jm-tertiary jm-mini"
                             :on-click #(apply! {:type :skip-free-shop})
                             :style {:margin-left 4}} "skip"]]
       :choose-lowest
       [:div {:style {:font-size 13}}
        [:b "LOWEST SKILL — tie! "] "you choose which to raise: "
        (doall (for [mv (moves-of :choose-lowest)]
                 ^{:key (:skill mv)}
                 [:button {:class "jm-btn jm-secondary jm-mini"
                           :on-click #(apply! mv) :style {:margin-left 4}}
                  (good-icon (:skill mv)) " " (gname (:skill mv))]))]
       :free-atelier
       [:div {:style {:font-size 13}}
        [:b "FREE ATELIER (recipe): "]
        "click one of your highlighted basic shops to upgrade it FREE (then pick an ability), or "
        [:button {:class "jm-btn jm-tertiary jm-mini"
                  :on-click #(apply! {:type :skip-free-atelier})
                  :style {:margin-left 4}} "skip"]]
       :chain [chain-panel st]
       :ability [ability-panel st]
       :craft [craft-panel st]
       :place-cart [:div {:style {:font-size 13}}
                    [:b "SETUP: "] "click any space to place your "
                    (if (nil? (get-in st [:players 0 :carts 0])) "first" "second")
                    " cart. EACH cart brings a FREE starting shop (if the space has room)."]
       [:div])]))

;; ── my board / opponents / market / log ──────────────────────────────────────
(defn- skill-grid [st p]
  (let [meds (get-in g/guilds [(:guild p) :medallions])]
    [:table {:style {:border-collapse "collapse" :font-size 11}}
     [:tbody
      [:tr [:td ""] (doall (for [rk (range 1 7)] ^{:key rk} [:td {:style {:padding "0 4px" :color "#999"}} "r" rk]))]
      (doall
       (for [sk g/good-types]
         ^{:key sk}
         [:tr
          [:td {:style {:color (good-css sk) :font-weight 700 :padding-right 4}
                :title (str (gname sk) (if (= sk :luxury) " (pays coins)" " (pays points)"))}
           (good-icon sk)]
          (doall
           (for [rk (range 1 7)
                 :let [cur? (= rk (get-in p [:skills sk]))
                       med? (contains? meds [sk rk])
                       hit? (contains? (:medallions-hit p) [sk rk])]]
             ^{:key rk}
             [:td {:title (str "rank " rk " pays " (g/grid-value (:guild p) sk rk)
                               (when med? " · ◆ medallion: reaching this grants a MASTER RECIPE"))
                   :style {:border "1px solid #ddd" :padding "1px 6px" :text-align "center"
                           :background (cond cur? (good-css sk) (and med? hit?) "#eee" med? "#fdf3d0" :else "#fff")
                           :color (if cur? "#fff" "#333") :font-weight (if cur? 700 400)}}
              (g/grid-value (:guild p) sk rk) (when med? [:span {:style {:color "#b8860b"}} "◆"])]))]))]]))

(defn- abilities-block
  "Guild passive (always on) + the 3 atelier abilities with ✓/▢ unlock state,
   every line tooltipped with its full effect text."
  [p]
  (let [gd (g/guilds (:guild p))]
    [:div {:style {:font-size 11 :line-height 1.5}}
     [:div {:title (str "PASSIVE (always on): " (get-in gd [:passive :text]))
            :style {:color "#7c3aed" :font-weight 700}}
      "✦ " (name (get-in gd [:passive :id]))]
     (doall
      (for [a (:ateliers gd)
            :let [on? (contains? (:abilities p #{}) (:id a))]]
        ^{:key (:id a)}
        [:div {:title (str (:text a)
                           (if on? " — UNLOCKED" " — locked: build an atelier and pick it"))
               :style {:color (if on? "#1a7f37" "#999") :font-weight (if on? 700 400)}}
         (if on? "✓ " "▢ ") (name (:id a))]))]))

(defn- player-panel
  "One real board per player — you AND the bots (2026-07-02 feedback): guild,
   passive + abilities, skill grid, workers/tokens/coins/score/claims/built ★."
  [st p you?]
  (let [pid (:id p)
        hue (worker-css (get-in g/guilds [(:guild p) :color]))   ; the guild's tint
        edge (str "1.5px solid " (if you? "#c9b87a" "#ddd"))]
    ;; per-side borders (NOT the `border` shorthand): style maps >8 keys are hash
    ;; maps with arbitrary key order, so a shorthand could clobber the accent bar
    [:div {:style {:border-top edge :border-right edge :border-bottom edge
                   :border-left (str "6px solid " hue)
                   :border-radius 10 :padding 8 :margin "6px 0"
                   :background (if you? "#fffef8" "#fafafa")
                   :box-shadow (when you? "0 1px 6px #0002")
                   :display "flex" :gap 14 :flex-wrap "wrap"}}
     [:div {:style {:min-width 210}}
      [:div {:style {:font-weight 700 :color (pcolor st pid) :font-size (if you? 14 13)}}
       "P" pid " " (gname (:guild p)) (if you? " (you)" " 🤖")
       " — ★" (:score p) " · " (:coins p) "🪙"]
      [:div {:style {:font-size 12 :margin-top 2}}
       "Workers: " (if (seq (:workers p))
                     (doall (for [[j w] (map-indexed vector (:workers p))]
                              ^{:key j} [meeple w (if you? 16 13) nil]))
                     [:span {:style {:color "#999"}} "none"])]
      [:div {:style {:font-size 12}}
       "Goods: "
       (if (seq (:tokens p))
         (doall (for [[gd n] (:tokens p)]
                  ^{:key gd} [:span {:style {:margin-right 8 :color (good-css gd) :font-weight 700}}
                              (good-icon gd) "×" n]))
         [:span {:style {:color "#999"}} (if you? "none — craft some" "none")])]
      [:div {:style {:font-size 12 :margin-top 2}}
       [:span {:title "Master recipes come ONLY from ◆ medallion cells on your grid — spend one (+ a pool token's cost) to BUILD"}
        "Master recipes: " [:b (:master-recipes p)]]
       [:span {:style {:margin-left 10}
               :title "Built mastercrafts score nothing until DISPLAYED at a space where you own an atelier"}
        "Built ★: " [:b (:mastercrafts-built p)]]]
      [:div {:style {:font-size 12}}
       "Shops left: " (:shops-left p) " · Ateliers left: " (:ateliers-left p)]]
     [:div {:title "Recipe board (green's slot board): 8 slots, base recipes seeded left; take-recipe covers a slot of YOUR choice; 🔗 chains fire off the slot to their left."}
      [recipe-slot-board st p you?]]
     [:div {:title "Guild board: passive is always on; each atelier you build unlocks ONE ability of your choice"}
      [abilities-block p]]
     [:div [skill-grid st p]]]))

(defn- master-pool-panel
  "The shared masterwork tableau — the visible list everyone competes for.
   Claimed tokens grey out (removed for everyone: a race)."
  [st]
  (let [live (set (map :id (:master-pool st)))]
    [:div {:style {:margin "6px 0"}}
     [:div {:style {:font-size 12 :font-weight 700 :color "#555"}}
      "MASTER POOL "
      [:span {:style {:font-weight 400 :color "#999"}}
       "(" (count live) "/14 left — BUILD one in your craft step with a ◆ master recipe + its worker cost; claiming removes it for everyone)"]]
     [:div {:style {:display "flex" :gap 5 :flex-wrap "wrap"}}
      (doall
       (for [t g/master-pool
             :let [gone? (not (contains? live (:id t)))]]
         ^{:key (:id t)}
         [:div {:title (if gone?
                         (str (name (:id t)) " — already claimed")
                         (str (name (:id t)) " — cost: "
                              (string/join " + " (map name (:cost t)))))
                :style {:border "1px solid #d0c5a4" :border-radius 8 :padding "3px 6px"
                        :background (if gone? "#eee" "#fdfbf2")
                        :opacity (if gone? 0.4 1) :font-size 11 :text-align "center"}}
          [:div {:style {:font-weight 700 :color "#7c3aed"}}
           "★" (name (:id t)) (when gone? " ✗")]
          [:div (doall (for [[j c] (map-indexed vector (:cost t))]
                         ^{:key j} [meeple c 12 nil]))]]))]]))

(defn- market-panel
  "The recipe market. Takeable cards are DIRECT board clicks (2026-07-03):
   click the CARD to arm it (one payment → armed at once; several → an inline
   payment picker opens on the card), then click the target SLOT on your recipe
   board. Clicking the armed card again, Escape, or clicking elsewhere disarms."
  [st]
  (let [takes (moves-of :action-recipe)
        by-idx (group-by :idx takes)
        mode (:mode @app)
        armed-idx (when (= :take-recipe (:kind mode)) (:idx mode))]
    [:div {:style {:margin "6px 0"}}
     [:div {:style {:font-size 12 :font-weight 700 :color "#555"}}
      "RECIPE MARKET " [:span {:style {:font-weight 400 :color "#999"}}
                        "(" (count (:recipe-deck st)) " in deck · taking a recipe adds 2 — the market GROWS so you can dig)"]
      (cond
        (and armed-idx (nil? (:pay mode)))
        [:span {:style {:color "#0b57d0" :font-weight 700}}
         " — pick HOW to pay on the armed card"]
        armed-idx
        [:span {:style {:color "#0b57d0" :font-weight 700}}
         " — now click a SLOT on your recipe board (bright = empty, orange = covers)"])]
     [:div {:style {:display "flex" :gap 6 :flex-wrap "wrap"}}
      (doall
       (for [[i rc] (map-indexed vector (:recipe-market st)) :when rc
             ;; the take moves fan out over target SLOTS — arm from the CARD,
             ;; pick a payment if several, then choose the slot on the board
             :let [opts (vec (distinct (map #(select-keys % [:pay :color]) (get by-idx i))))
                   armed? (= i armed-idx)]]
         ^{:key i}
         [:div {:on-click (when (seq opts)
                            (fn [e] (.stopPropagation e)
                              (swap! app assoc :mode
                                     (cond armed? nil
                                           (= 1 (count opts))
                                           {:kind :take-recipe :idx i
                                            :pay (:pay (first opts))
                                            :color (:color (first opts))}
                                           :else {:kind :take-recipe :idx i}))))
                :title (cond armed? "Armed — click again (or press Escape) to cancel."
                             (seq opts) "Click to TAKE this recipe — you then click the slot on your board it goes to (the SLOT matters: a 🔗 chain fires off the slot to its LEFT).")
                :style {:border (cond armed? "2px solid #0b57d0"
                                      (seq opts) "2px solid #0b57d0"
                                      :else "1px solid #ccc")
                        :box-shadow (when armed? "0 0 8px #0b57d088")
                        :background (if armed? "#eaf2ff" "#fff")
                        :cursor (when (seq opts) "pointer")
                        :border-radius 8 :padding "4px 8px" :font-size 12}}
          [:div {:title (str "Era " (string/upper-case (name (:era rc :a))) " card")}
           (string/join " + " (map gname (:inputs rc))) " → "
           [:b (if (seq (:outputs rc))
                 (string/join "+" (map good-icon (:outputs rc))) "—")]
           [recipe-flags rc]]
          (when (and (seq opts) (not armed?))
            [:div {:style {:font-size 10 :color "#0b57d0" :font-weight 700 :margin-top 2}}
             "click to take ▾"])
          ;; several distinct payments → the inline payment picker ON the card
          (when (and armed? (nil? (:pay mode)) (> (count opts) 1))
            [:div {:style {:display "flex" :gap 4 :margin-top 3 :flex-wrap "wrap"}}
             (doall (for [[k o] (map-indexed vector opts)]
                      ^{:key k}
                      [:button {:class "jm-btn jm-secondary jm-mini"
                                :on-click (fn [e] (.stopPropagation e)
                                            (swap! app update :mode assoc
                                                   :pay (:pay o) :color (:color o)))
                                :style {:font-size 11 :border "1px solid #0b57d0"
                                        :background "#eef4ff"}}
                       (if (= :coins (:pay o))
                         (str "pay " 2 "🪙")
                         [:span "pay " [meeple (:color o) 13 nil]])]))])
          (when (and armed? (:pay mode))
            [:div {:style {:font-size 10 :color "#0b57d0" :font-weight 700 :margin-top 2}}
             "…now click a SLOT on your board "
             (if (= :coins (:pay mode))
               (str "(paying " 2 "🪙)")
               [:span "(paying " [meeple (:color mode) 12 nil] ")"])])]))]]))

;; ── 🐞 bug report (green's submit-bug! port) ─────────────────────────────────
(defonce bug-ui (r/atom {:open? false :text "" :status nil}))
(defn- submit-bug!
  "POST the pr-str'd {:notes :state :log} payload to /journeymen/bug (the green
   route; fire-and-forget) and surface a sent/failed status."
  [st]
  (swap! bug-ui assoc :status "sending…")
  (try
    (-> (js/fetch "/journeymen/bug"
                  #js {:method "POST"
                       :headers #js {"Content-Type" "application/edn"}
                       :body (pr-str {:notes (:text @bug-ui)
                                      :state (dissoc st :log)
                                      :log (vec (take-last 100 (:log st)))})})
        (.then (fn [resp]
                 (swap! bug-ui assoc :status
                        (if (.-ok resp) "✓ sent — thank you!" "⚠ failed — server said no"))))
        (.catch (fn [_] (swap! bug-ui assoc :status "⚠ failed — no server?"))))
    (catch :default _ (swap! bug-ui assoc :status "⚠ failed"))))
(defn- bug-report [st]
  [:div {:style {:margin-top 8}}
   [:button {:class "jm-btn jm-tertiary jm-mini"
             :on-click #(swap! bug-ui update :open? not)}
    (if (:open? @bug-ui) "Hide bug report" "🐞 Report a bug")]
   (when (:open? @bug-ui)
     [:div {:style {:margin-top 4 :max-width 560}}
      [:div {:style {:font-size 12 :color "#666"}}
       "Describe what went wrong — game state + log attach automatically."]
      [:textarea {:value (:text @bug-ui)
                  :placeholder "What happened? What did you expect?"
                  :on-change #(swap! bug-ui assoc :text (.. % -target -value))
                  :style {:width "100%" :height 80 :margin-top 4 :font-family "inherit"}}]
      [:button {:class "jm-btn jm-mini"
                :style {:padding "5px 12px" :background "#fdd" :border "1px solid #c99"}
                :on-click #(submit-bug! st)} "Submit ▶"]
      (when (:status @bug-ui)
        [:span {:style {:margin-left 8 :font-size 12}} (:status @bug-ui)])])])

(defn- log-panel [st]
  [:div {:style {:font-size 11 :color "#444" :max-height 150 :overflow "auto"
                 :border "1px solid #eee" :padding 6 :margin-top 8 :background "#fff"}}
   (doall (for [[i e] (map-indexed vector (reverse (take-last 30 (:log st))))]
            ^{:key i}
            [:div [:b {:style {:color (pcolor st (:player e))}} "P" (:player e)] " " (:text e)]))])

(defn- game-over-panel [st]
  (let [;; dividend earnings per player, recovered from the log ("dividend: P<id> …")
        divs (frequencies
              (keep #(some->> (:text %) (re-find #"^dividend: P(\d+) ") second js/parseInt)
                    (:log st)))
        w (g/winner st)
        td {:padding "2px 14px 2px 0" :font-size 13}]
    [:div {:style {:border "2px solid #1a7f37" :border-radius 10 :padding "10px 14px"
                   :margin "8px 0" :background "#f0fff0"}}
     [:h3 {:style {:margin "0 0 6px"}} "Game over — "
      (str "P" w " (" (gname (:guild (nth (:players st) w))) ") wins!")]
     [:table {:style {:border-collapse "collapse"}}
      [:tbody
       (doall
        (for [p (sort-by (comp - :score) (:players st))
              :let [pid (:id p)
                    shown (count (filter #(= pid (:pid %)) (mapcat val (:displays st))))
                    dv (get divs pid 0)]]
          ^{:key pid}
          [:tr
           [:td {:style (assoc td :color (pcolor st pid) :font-weight 700)}
            "P" pid " " (gname (:guild p)) (when (zero? pid) " (you)")]
           [:td {:style td} [:b (:score p)] "★"]
           [:td {:style td} (:coins p) "🪙"]
           [:td {:style td} shown "★ displayed"]
           [:td {:style (assoc td :color "#666")}
            (if (pos? dv) (str "+" dv "★ +" dv "🪙 from dividends") "no dividends")]]))]]
     [:div {:style {:font-size 12 :color "#555" :margin-top 6}}
      "Final tracks: "
      (doall (for [gd g/good-types
                   :let [n (get-in st [:tracks gd]) th (g/track-threshold gd)]]
               ^{:key gd}
               [:span {:style {:margin-right 10 :color (good-css gd) :font-weight 600}}
                (good-icon gd) " " n "/" th (when (>= n th) " FULL")]))]
     [:button {:class "jm-btn jm-primary"
               :on-click play-again!
               :style {:margin-top 8 :border "2px solid #1a7f37" :background "#1a7f37"
                       :color "#fff"}}
      "Play again"]]))

;; ── header strips + help overlay ─────────────────────────────────────────────
(defn- scoreboard
  "Compact all-players strip for the header row; the current player highlighted."
  [st]
  [:span {:style {:display "inline-flex" :gap 4 :align-items "baseline" :flex-wrap "wrap"}}
   (doall
    (for [p (:players st)
          :let [cur? (= (:id p) (:current st))]]
      ^{:key (:id p)}
      [:span {:title (str "P" (:id p) " " (gname (:guild p)) (when cur? " — playing now"))
              :style {:color (pcolor st (:id p)) :font-size 12
                      :font-weight (if cur? 800 600)
                      :background (if cur? "#fdf3d0" "transparent")
                      :outline (when cur? "1px solid #d8c88a")
                      :border-radius 6 :padding "1px 6px"}}
       "P" (:id p) " ★" (:score p) " " (:coins p) "🪙"]))])

(defn- recent-strip
  "The last 3 log entries ABOVE the board — bot turns stay visible without
   scrolling down to the full log."
  [st]
  (let [es (take-last 3 (:log st))]
    (when (seq es)
      [:div {:style {:display "flex" :gap 10 :flex-wrap "wrap" :align-items "center"
                     :font-size 11 :color "#555" :margin "2px 0 4px"
                     :border "1px solid #eee" :border-radius 8 :padding "3px 8px"
                     :background "#fffdf6"}}
       (doall
        (for [[i e] (map-indexed vector es)]
          ^{:key i}
          [:span {:style {:white-space "nowrap" :overflow "hidden"
                          :text-overflow "ellipsis" :max-width 350}}
           [:span {:style {:background (pcolor st (:player e)) :color "#fff"
                           :border-radius 4 :padding "0 4px" :font-weight 700
                           :margin-right 3}}
            "P" (:player e)]
           (:text e)]))])))

(defn- help-overlay []
  (when @help?
    [:div {:on-click #(reset! help? false)
           :style {:position "fixed" :inset 0 :background "rgba(40,32,12,.45)" :z-index 50
                   :display "flex" :align-items "flex-start" :justify-content "center"
                   :padding "40px 12px" :overflow "auto"}}
     [:div {:on-click (fn [e] (.stopPropagation e))
            :style {:background "#fffdf4" :border "2px solid #b08d3f" :border-radius 12
                    :padding "14px 18px" :max-width 540 :font-size 13 :line-height 1.55
                    :box-shadow "0 6px 30px #0006"}}
      [:div {:style {:display "flex" :justify-content "space-between" :align-items "baseline"
                     :gap 12}}
       [:b {:style {:font-size 15}} "How a turn works"]
       [:button {:class "jm-btn jm-tertiary jm-mini" :on-click #(reset! help? false)} "✕"]]
      [:ol {:style {:margin "8px 0" :padding-left 20}}
       [:li [:b "MOVE"] " — pick one of your cart tokens, go 1–2 ROADS along the arrows (two directed tracks)."]
       [:li [:b "PICK UP"] " — a worker at your space (free) or a neighbouring one (1🪙)."]
       [:li [:b "SELL / ACTION"] " — sell where you own a shop (required good first, optional rides along), or use the space's action (shop / atelier / recipe / skilled), or DISPLAY a built ★ at your atelier."]
       [:li [:b "CRAFT"] " — pay workers on a recipe for goods tokens (works anywhere); 🔗 chains may cascade. Then the turn ends."]]
      [:p {:style {:margin "6px 0"}}
       [:b "The clock: "] "every sale puts ONE sold token on its good's track. When the 2nd track fills, the current round finishes and ONE final round is played."]
      [:p {:style {:margin "6px 0"}}
       [:b "Dividends: "] "a displayed ★ mastercraft scores its grid value once, then pays its owner +1★+1🪙 every time a token lands on that track."]
      [:p {:style {:margin "6px 0 0"}}
       [:a {:href "/journeymen/rules"} "Full rules →"]]]]))

(defn- setup-screen []
  [:div {:style {:padding 24 :font-family "system-ui, sans-serif" :max-width 640}}
   [:h2 "Journeymen E — playtest"]
   [:div {:style {:margin "10px 0"}}
    [:span {:style {:font-weight 700 :margin-right 8}} "Players:"]
    (doall
     (for [n [2 3 4]]
       ^{:key n}
       [:button {:class "jm-btn jm-secondary"
                 :on-click #(reset! setup-n n)
                 :style {:margin-right 6 :min-width 42
                         :border (if (= n @setup-n) "2px solid #1a7f37" "1px solid #bbb")
                         :background (if (= n @setup-n) "#eefaee" "#fff")
                         :font-weight (if (= n @setup-n) 700 400)}}
        n]))
    [:span {:style {:color "#888" :font-size 12}}
     " you + " (dec @setup-n) " bot" (when (> @setup-n 2) "s")]]
   [:p "Pick your guild:"]
   (into [:div {:style {:display "flex" :gap 10 :flex-wrap "wrap"}}]
         (for [gu [:blacksmith :alchemist :goldsmith :jeweler]]
           [:button {:class "jm-btn jm-primary"
                     :on-click #(new-game! gu @setup-n)
                     :style {:padding "12px 18px" :border-radius 10 :font-size 15
                             :border (str "2px solid " (worker-css (get-in g/guilds [gu :color])))
                             :background "#fffdf4"}}
            (gname gu)]))
   [:p {:style {:color "#888" :font-size 12 :max-width 560}}
    "The board is green's two-track directed rondel — carts follow the road arrows, 1–2 roads per move. "
    "Every space is an action + a demand; you can only SELL where you own a shop. "
    "The game ends when 2 of the 4 sale tracks fill."]])

(defn root []
  (let [{:keys [st]} @app]
    (if-not st
      [setup-screen]
      ;; click-elsewhere disarms an armed take-recipe / craft pay-picker (their
      ;; own targets stopPropagation, so a click landing here means "away")
      [:div {:on-click (fn [_]
                         (when (#{:take-recipe :craft-pay} (get-in @app [:mode :kind]))
                           (swap! app assoc :mode nil)))
             :style {:max-width 1150 :margin "0 auto" :padding 10
                     :font-family "system-ui, sans-serif"}}
       [help-overlay]
       [:div {:style {:display "flex" :gap 10 :align-items "baseline" :flex-wrap "wrap"}}
        [:h2 {:style {:margin "4px 0"}} "Journeymen E"]
        [scoreboard st]
        [:span {:style {:font-size 13 :color "#666"}}
         "round " (:round st)
         (when (:ending st) [:b {:style {:color "#c00"}} " · ⏳ FINAL ROUNDS"])
         (when (= :setup (:phase st)) [:b " · SETUP — click a space to place each of your 2 carts"])]
        [:button {:class "jm-btn jm-tertiary jm-mini" :on-click undo!
                  :style {:margin-left 8}} "↩ Undo"]
        [:button {:class "jm-btn jm-tertiary jm-mini" :on-click play-again!} "New game"]
        [:button {:class "jm-btn jm-tertiary jm-mini" :on-click #(reset! help? true)
                  :title "how a turn works · the clock · dividends"
                  :style {:font-weight 700}} "?"]
        [:a {:href "/journeymen/rules" :style {:font-size 12}} "rules"]]
       (when (g/game-over? st) [game-over-panel st])
       [tracks-panel st]
       (when-not (g/game-over? st)
         (if (zero? (:current st))
           [step-panel st]
           [:div {:style {:border "1px solid #eee" :border-radius 10 :padding 10 :margin "8px 0"
                          :background "#f8f8f8" :color "#888" :font-size 13}}
            "🤖 P" (:current st) " (" (gname (:guild (g/current-player st))) ") is taking their turn…"]))
       [recent-strip st]
       [board st]
       ;; YOUR board, then the surfaces you make CHOICES from (market, master pool),
       ;; THEN opponents — reference sits below the things you interact with, so a
       ;; playing human doesn't scroll past opponent boards to reach their own moves
       [player-panel st (first (:players st)) true]
       [market-panel st]
       [master-pool-panel st]
       (when (seq (rest (:players st)))
         [:div {:style {:font-size 11 :letter-spacing ".08em" :text-transform "uppercase"
                        :color "#b7ad97" :font-weight 700 :margin "14px 0 2px"}} "Opponents"])
       (doall (for [p (rest (:players st))]
                ^{:key (:id p)} [player-panel st p false]))
       [log-panel st]
       [bug-report st]
       [:div {:style {:font-size 11 :color "#aaa" :margin-top 6}}
        "playtest UI v6 · every click = a legal engine move · errors auto-beacon · seed " (:seed @app)
        " · saved to the server as " (or (:game-key @app) "—")]])))

(defn- restore!
  "Resume an in-progress game from localStorage. STRICT about provenance: the
   payload must carry THIS engine's :schema string — a save written by any other
   schema (or a raw pre-schema payload) is left alone and we quietly fall back
   to the setup screen instead of crashing on a stale board shape. (Older keys
   like journeymen-e/v3 aren't even read: ls-key changed with the schema.)"
  []
  (try
    (when-let [raw (.getItem js/localStorage ls-key)]
      (let [{:keys [schema st guild seed game-key]} (reader/read-string raw)]
        (when (and st (= schema schema-ver) (not (g/game-over? st)))
          ;; pre-server-save snapshots have no key — mint one so saves start flowing
          (reset! app {:st st :hist [] :guild guild :seed seed :mode nil
                       :game-key (or game-key (rand-key))})
          (save!)
          (run-bots!))))
    (catch :default _ nil)))

(defn mount-components []
  (rdom/render [root] (.getElementById js/document "journeymen-e")))
(defn init! []
  (migrate-finished!)   ; rescue any finished pre-server-save game FIRST
  (restore!)
  (when-not (:st @app)
    (let [q (js/URLSearchParams. js/window.location.search)
          gu (some-> (.get q "guild") keyword)]
      (when (contains? g/guilds gu) (new-game! gu))))
  (mount-components))
