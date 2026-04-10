(ns journey.bots
  "Per-player journey bot list and flowchart editor.

   Two pages share this single cljs build:
   - bot list (js/isBotList): list saved bots, link to create/edit
   - bot editor (js/isBotEditor): canvas with diagrams, palette, properties panel"
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [ajax.core :refer [POST]]
   [journey.bot-flow :as journey-bf]
   [organism.bot-flow :as organism-bf]
   [organism.ajax :as ajax]))

;; ── Path helpers (read from template JS vars, default to journey) ────────────

(defn- bot-base-path []
  (if (and (exists? js/botBasePath) (not (str/blank? js/botBasePath)))
    js/botBasePath
    "/journey/bots"))

(defn- bot-home-path []
  (if (and (exists? js/botHomePath) (not (str/blank? js/botHomePath)))
    js/botHomePath
    "/journey"))

(defn- bot-game-title []
  (if (and (exists? js/botGameType) (not (str/blank? js/botGameType)))
    (str/upper-case js/botGameType)
    "JOURNEY"))

(defn- game-type []
  (if (and (exists? js/botGameType) (not (str/blank? js/botGameType)))
    js/botGameType
    "journey"))

;; ── Game-specific vocabulary dispatch ────────────────────────────────────────
;; The editor needs tile-spec, all-categories, and default-bot from the
;; correct game's vocabulary.  We dispatch on js/botGameType.

(defn current-vocab []
  (case (game-type)
    "organism" organism-bf/vocab
    journey-bf/vocab))

(defn current-tile-spec [tile]
  (case (game-type)
    "organism" (organism-bf/tile-spec tile)
    (journey-bf/tile-spec tile)))

(defn current-all-categories []
  (case (game-type)
    "organism" (organism-bf/all-categories)
    (journey-bf/all-categories)))

(defn current-default-bot []
  (case (game-type)
    "organism" organism-bf/default-bot
    journey-bf/default-bot))

;; ── State ────────────────────────────────────────────────────────────────────

(defonce bot
  (r/atom nil))

(defonce bot-name
  (r/atom ""))

(defonce bot-owner
  (r/atom nil))

(defonce selected
  (r/atom nil))  ;; {:diagram diagram-name :tile tile-id} or nil

(defonce palette-selection
  (r/atom nil))  ;; {:kind :type :label}

(defonce link-drag
  (r/atom nil))  ;; {:from {:diagram :tile :port} :mx :my} during a port-drag

(defonce tile-drag
  (r/atom nil))  ;; {:diagram :tile :ox :oy} while dragging a tile

(defonce diagram-drag
  (r/atom nil))  ;; {:diagram :start-origin [x y] :mx :my} while dragging a diagram

(defonce save-status
  (r/atom nil))

(defonce next-id-counter
  (r/atom 1000))

(defn fresh-id [prefix]
  (let [n (swap! next-id-counter inc)]
    (keyword (str prefix "-" n))))

;; ── Region geometry ──────────────────────────────────────────────────────────

;; Regular flat-top hexagon.  Circumradius r → width = 2r, height = r√3.
(def hex-r 50)
(def sqrt3 (Math/sqrt 3))
(def tile-w (* 2 hex-r))           ;; 100
(def tile-h (* hex-r sqrt3))       ;; ≈86.6
(def region-pad 30)
(def region-title-h 22)
(def diagram-gap 24)

;; Flat-top hex vertices relative to tile top-left [0,0]:
;;   upper-left:  (r/2,  0)
;;   upper-right: (3r/2, 0)
;;   right:       (2r,   r√3/2)
;;   lower-right: (3r/2, r√3)
;;   lower-left:  (r/2,  r√3)
;;   left:        (0,    r√3/2)
(defn hex-points-str
  "SVG polygon points string for a flat-top hex at absolute position [x y]."
  [x y]
  (let [r   hex-r
        h2  (/ tile-h 2)]
    (str (+ x (* 0.5 r)) "," y " "                ;; upper-left
         (+ x (* 1.5 r)) "," y " "                ;; upper-right
         (+ x (* 2 r))   "," (+ y h2) " "         ;; right
         (+ x (* 1.5 r)) "," (+ y tile-h) " "     ;; lower-right
         (+ x (* 0.5 r)) "," (+ y tile-h) " "     ;; lower-left
         x                "," (+ y h2))))

(defn tile-bounds
  "Compute the [x y w h] of a region from its tiles."
  [diagram]
  (let [tiles (vals (:tiles diagram))
        [base-x base-y] (or (:origin diagram) [0 0])]
    (if (seq tiles)
      (let [xs (map (fn [t] (first (:pos t))) tiles)
            ys (map (fn [t] (second (:pos t))) tiles)
            min-x (apply min xs)
            min-y (apply min ys)
            max-x (apply max xs)
            max-y (apply max ys)
            w (+ (- max-x min-x) tile-w (* 2 region-pad))
            h (+ (- max-y min-y) tile-h (* 2 region-pad) region-title-h)]
        [(- (+ base-x min-x) region-pad)
         (- (+ base-y min-y) region-pad region-title-h)
         w h])
      [base-x base-y 240 (+ region-title-h (* 2 region-pad))])))

(defn rects-collide?
  [[ax ay aw ah] [bx by bw bh]]
  (not (or (<= (+ ax aw) bx)
           (<= (+ bx bw) ax)
           (<= (+ ay ah) by)
           (<= (+ by bh) ay))))

(defn relayout
  "Push diagrams apart to eliminate overlaps. Pushes the later diagram in the
   direction of least overlap (down or right). Iterates until stable."
  [b]
  (let [order (vec (keys (:diagrams b)))
        rects (atom (into {} (map (fn [k]
                                    [k (tile-bounds (get-in b [:diagrams k]))])
                                  order)))]
    (loop [i 0]
      (if (> i 40)
        b
        (let [conflicts? (atom false)]
          (doseq [a order
                  bk order
                  :when (not= a bk)]
            (let [[ax ay aw ah] (get @rects a)
                  [bx by bw bh] (get @rects bk)]
              (when (rects-collide? [ax ay aw ah] [bx by bw bh])
                (reset! conflicts? true)
                ;; Compute overlap in each axis
                (let [overlap-x (min (- (+ ax aw) bx) (- (+ bx bw) ax))
                      overlap-y (min (- (+ ay ah) by) (- (+ by bh) ay))
                      ;; Push the one that is more to the right / below
                      ;; Choose the axis with smaller overlap (less push needed)
                      [target axis]
                      (if (< overlap-y overlap-x)
                        [(if (>= ay by) a bk) :y]
                        [(if (>= ax bx) a bk) :x])]
                  (if (= axis :y)
                    (let [[_ ty _ th] (get @rects (if (= target a) bk a))
                          push (- (+ ty th diagram-gap) (second (get @rects target)))]
                      (swap! rects update-in [target 1] + push))
                    (let [[tx _ tw _] (get @rects (if (= target a) bk a))
                          push (- (+ tx tw diagram-gap) (first (get @rects target)))]
                      (swap! rects update-in [target 0] + push)))))))
          (if @conflicts?
            (recur (inc i))
            ;; Apply new positions back to diagram origins
            (reduce (fn [acc k]
                      (let [[nx ny _ _] (get @rects k)
                            old (get-in acc [:diagrams k])
                            [ox oy _ _] (tile-bounds old)
                            dy (- ny oy)
                            dx (- nx ox)]
                        (-> acc
                            (update-in [:diagrams k]
                                       (fn [d]
                                         (let [[bx by] (or (:origin d) [0 0])]
                                           (assoc d :origin [(+ bx dx) (+ by dy)])))))))
                    b
                    order)))))))

;; ── Hex auto-layout (force-directed) ─────────────────────────────────────────
;; 1) BFS initial placement: true/out → NE, false → SE (diagonal, not straight
;;    down), keeping everything compact.
;; 2) Force-directed simulation in pixel space: spring forces along edges pull
;;    connected tiles together; repulsion prevents overlap.
;; 3) Snap to nearest hex-grid cell.

(def layout-gap 16)
(def grid-r  (+ hex-r layout-gap))
(def grid-dx (* 1.5 grid-r))                   ;; column step ≈99
(def grid-dy (* grid-r sqrt3))                  ;; row step ≈114

;; Ideal edge vectors in pixel space (flat-top hex neighbor directions).
(def ne-step [grid-dx (- (/ grid-dy 2))])       ;; NE neighbor: right & up
(def se-step [grid-dx (/ grid-dy 2)])            ;; SE neighbor: right & down
(def s-step  [0       grid-dy])                  ;; S neighbor:  straight down

(defn- grid->px [[col row]]
  [(* col grid-dx)
   (+ (* row grid-dy) (if (odd? col) (/ grid-dy 2) 0))])

(defn- px->grid
  "Snap pixel coords to nearest hex grid cell."
  [[x y]]
  ;; Try the two nearest columns and pick the one whose center is closest.
  (let [col-f   (/ x grid-dx)
        col-lo  (int (Math/floor col-f))
        col-hi  (inc col-lo)
        best    (fn [c]
                  (let [yo (if (odd? c) (/ grid-dy 2) 0)
                        r  (Math/round (/ (- y yo) grid-dy))
                        [gx gy] (grid->px [c r])
                        d  (+ (* (- x gx) (- x gx)) (* (- y gy) (- y gy)))]
                    {:col c :row r :dist d}))
        a (best col-lo)
        b (best col-hi)]
    (if (< (:dist a) (:dist b))
      [(:col a) (:row a)]
      [(:col b) (:row b)])))

(defn- build-adj [links]
  (reduce (fn [acc l]
            (update acc (get-in l [:from :tile])
                    (fnil conj []) {:tile (get-in l [:to :tile])
                                   :port (get-in l [:from :port])}))
          {} links))

(defn auto-layout-diagram
  "Force-directed hex layout.
   Phase 1: BFS places tiles — true→NE, false→SE (keeps things diagonal).
   Phase 2: Force simulation pulls connected tiles together, pushes
            overlapping tiles apart.
   Phase 3: Snap to hex grid, resolve collisions."
  [diagram]
  (let [tiles    (:tiles diagram)
        links    (:links diagram)
        tile-ids (vec (keys tiles))
        start-id (or (:start-tile diagram)
                     (some (fn [[id t]] (when (= :start (:kind t)) id)) tiles)
                     (first tile-ids))
        adj      (build-adj links)

        ;; Collect edges as [from-id to-id port] for force sim
        edges (vec (for [l links]
                     [(get-in l [:from :tile])
                      (get-in l [:to :tile])
                      (get-in l [:from :port])]))

        ;; ── Phase 1: BFS initial pixel placement ────────────────────────
        ;; For false chains, alternate between S (straight down) and SE
        ;; (down-right) to create a staircase pattern instead of a
        ;; uniform diagonal.
        ;; false-depth tracks how many consecutive false steps led to each tile.
        [pos _ edge-steps]
        (loop [queue       [start-id]
               pos         {start-id [0.0 0.0]}
               seen        #{start-id}
               false-depth {start-id 0}
               edge-steps  {}]  ;; {[from to] → step-vector} for force sim
          (if (empty? queue)
            [pos seen edge-steps]
            (let [u     (first queue)
                  queue (subvec (vec queue) 1)
                  [ux uy] (get pos u [0 0])
                  u-fd  (get false-depth u 0)
                  succs (sort-by #(if (#{:false :b} (:port %)) 1 0)
                                 (get adj u []))
                  [pos' queue' seen' fd' es']
                  (reduce
                   (fn [[p q s fd es] {:keys [tile port]}]
                     (if (or (not (some #{tile} tile-ids)) (s tile))
                       [p q s fd es]
                       (let [down?   (#{:false :b} port)
                             ;; For false: alternate S and SE based on chain depth
                             step    (cond
                                       (not down?)      ne-step
                                       (even? u-fd)     s-step    ;; straight down
                                       :else            se-step)  ;; down-right
                             [dx dy] step
                             nx      (+ ux dx)
                             ny      (+ uy dy)
                             new-fd  (if down? (inc u-fd) 0)]
                         [(assoc p tile [nx ny])
                          (conj q tile)
                          (conj s tile)
                          (assoc fd tile new-fd)
                          (assoc es [u tile] step)])))
                   [pos queue seen false-depth edge-steps]
                   succs)]
              (recur queue' pos' seen' fd' es'))))

        ;; Place disconnected tiles
        max-y (+ 200 (apply max 0 (map second (vals pos))))
        pos (reduce (fn [p id]
                      (if (p id) p
                          (assoc p id [0 max-y])))
                    pos tile-ids)

        ;; ── Phase 2: Force-directed simulation ──────────────────────────
        ;; Spring constant, repulsion strength, damping
        k-spring   0.18
        k-repulse  5000.0
        min-dist   (+ tile-w layout-gap)
        damping    0.82
        iterations 60

        pos
        (loop [p pos, vel (into {} (map #(vector % [0.0 0.0]) tile-ids)), iter 0]
          (if (>= iter iterations)
            p
            (let [;; Compute forces on each tile
                  forces
                  (reduce
                   (fn [f [from-id to-id port]]
                     (let [[x1 y1] (get p from-id [0 0])
                           [x2 y2] (get p to-id [0 0])
                           ;; Use per-edge ideal step if available (staircase-aware),
                           ;; otherwise fall back to port-based default
                           [idx idy] (or (get edge-steps [from-id to-id])
                                        (if (#{:false :b} port) se-step ne-step))
                           dx (- x2 x1)
                           dy (- y2 y1)
                           ex (- dx idx)
                           ey (- dy idy)
                           fx (* k-spring ex)
                           fy (* k-spring ey)]
                       (-> f
                           (update from-id (fn [[ax ay]] [(+ ax fx) (+ ay fy)]))
                           (update to-id   (fn [[ax ay]] [(- ax fx) (- ay fy)])))))
                   (into {} (map #(vector % [0.0 0.0]) tile-ids))
                   edges)

                  ;; Repulsion between all pairs (O(n²) but n is small)
                  forces
                  (reduce
                   (fn [f i]
                     (reduce
                      (fn [f2 j]
                        (let [[x1 y1] (get p i [0 0])
                              [x2 y2] (get p j [0 0])
                              dx (- x1 x2)
                              dy (- y1 y2)
                              d2 (+ (* dx dx) (* dy dy) 1)
                              d  (Math/sqrt d2)]
                          (if (> d (* 3 min-dist))
                            f2  ;; too far, skip
                            (let [force (/ k-repulse d2)
                                  fx (* force (/ dx d))
                                  fy (* force (/ dy d))]
                              (-> f2
                                  (update i (fn [[ax ay]] [(+ ax fx) (+ ay fy)]))
                                  (update j (fn [[ax ay]] [(- ax fx) (- ay fy)])))))))
                      f
                      (subvec tile-ids (inc (.indexOf tile-ids i)))))
                   forces
                   tile-ids)

                  ;; Apply forces with velocity and damping (skip start tile)
                  [p' vel']
                  (reduce
                   (fn [[pp vv] id]
                     (if (= id start-id)
                       [pp vv]
                       (let [[fx fy] (get forces id [0 0])
                             [vx vy] (get vv id [0 0])
                             nvx (* damping (+ vx fx))
                             nvy (* damping (+ vy fy))
                             [px py] (get pp id [0 0])]
                         [(assoc pp id [(+ px nvx) (+ py nvy)])
                          (assoc vv id [nvx nvy])])))
                   [p vel]
                   tile-ids)]
              (recur p' vel' (inc iter)))))

        ;; ── Phase 3: Snap to hex grid ───────────────────────────────────
        ;; Convert pixel positions to grid cells, resolve collisions.
        grid (into {} (map (fn [[id xy]] [id (px->grid xy)]) pos))

        ;; Resolve grid collisions: if two tiles snapped to the same cell,
        ;; shift one down.
        grid
        (reduce
         (fn [g id]
           (let [[c r] (get g id)]
             (if (some (fn [[oid [oc or']]] (and (not= oid id) (= oc c) (= or' r)))
                       g)
               ;; Find free cell nearby
               (loop [dr 1]
                 (if (not (some (fn [[oid [oc or']]]
                                  (and (not= oid id) (= oc c) (= or' (+ r dr))))
                                g))
                   (assoc g id [c (+ r dr)])
                   (recur (inc dr))))
               g)))
         grid tile-ids)

        ;; Convert back to pixels
        start-px (grid->px (get grid start-id [0 0]))
        offset-x (- 40 (first start-px))
        offset-y (- 40 (second start-px))

        new-tiles
        (reduce-kv
         (fn [ts id gc]
           (if-not (contains? ts id) ts
                   (let [[px py] (grid->px gc)]
                     (assoc-in ts [id :pos]
                               [(+ px offset-x) (+ py offset-y)]))))
         tiles grid)]
    (assoc diagram :tiles new-tiles)))

(defn auto-layout!
  "Apply auto-layout to a single diagram and update the bot atom."
  [diagram-name]
  (swap! bot
         (fn [b]
           (relayout
            (update-in b [:diagrams diagram-name] auto-layout-diagram)))))

;; ── Tile/diagram operations ─────────────────────────────────────────────────

(defn diagram-key
  "Convert a user-supplied diagram name to a stable keyword (UPPER snake)."
  [s]
  (-> s str/trim str/lower-case (str/replace #"\s+" "-") keyword))

(defn add-diagram!
  [name]
  (let [k     (diagram-key name)
        next? (contains? (:diagrams @bot) k)]
    (when (and (not next?) (seq (str name)))
      (let [start-id :start
            color    (rand-nth ["#1e3a5a" "#3a1e5a" "#5a1e3a" "#1e5a3a" "#5a3a1e" "#3a5a1e"])
            n-existing (count (:diagrams @bot))
            base-y   (* n-existing 220)]
        (swap! bot
               (fn [b]
                 (relayout
                  (assoc-in b [:diagrams k]
                            {:name name
                             :color color
                             :collapsed? false
                             :origin [40 (+ 40 base-y)]
                             :start-tile start-id
                             :tiles {start-id {:id start-id :kind :start :type :start
                                               :pos [40 80] :params {}}}
                             :links []}))))
        k))))

(defn delete-diagram!
  [diagram-key]
  (when (not= diagram-key (or (:start-diagram @bot) :main))
    (swap! bot
           (fn [b]
             (-> b
                 (update :diagrams dissoc diagram-key)
                 relayout)))))

(defn add-tile-to-diagram!
  "Add a palette tile (:kind :type :label) to a diagram at a position."
  [diagram-name palette-tile [x y]]
  (let [id (fresh-id (name (:type palette-tile)))
        v    (current-vocab)
        spec (case (:kind palette-tile)
               :condition (get-in v [:conditions (:type palette-tile)])
               :logic     (get-in v [:logic (:type palette-tile)])
               :effect    (get-in v [:effects (:type palette-tile)])
               :jump      (get-in v [:effects :jump])
               nil)
        defaults (into {} (map (juxt :key :default) (:params spec)))]
    (swap! bot
           (fn [b]
             (relayout
              (assoc-in b [:diagrams diagram-name :tiles id]
                        {:id id
                         :kind (:kind palette-tile)
                         :type (:type palette-tile)
                         :pos [x y]
                         :params defaults}))))
    (reset! selected {:diagram diagram-name :tile id})
    (reset! palette-selection nil)))

(defn delete-tile!
  [diagram-name tile-id]
  (when-not (= tile-id :start)
    (swap! bot
           (fn [b]
             (-> b
                 (update-in [:diagrams diagram-name :tiles] dissoc tile-id)
                 (update-in [:diagrams diagram-name :links]
                            (fn [ls]
                              (vec (remove
                                    (fn [l]
                                      (or (= tile-id (get-in l [:from :tile]))
                                          (= tile-id (get-in l [:to :tile]))))
                                    ls))))
                 relayout)))
    (reset! selected nil)))

(defn move-tile!
  [diagram-name tile-id [x y]]
  (swap! bot
         (fn [b]
           (-> b
               (assoc-in [:diagrams diagram-name :tiles tile-id :pos] [x y])
               relayout))))

(defn add-link!
  [diagram-name from-id from-port to-id to-port]
  (when (not= from-id to-id)
    (swap! bot
           (fn [b]
             (update-in b [:diagrams diagram-name :links]
                        (fn [ls]
                          (let [stripped (vec (remove
                                               (fn [l]
                                                 (and (= (get-in l [:from :tile]) from-id)
                                                      (= (get-in l [:from :port]) from-port)))
                                               ls))]
                            (conj stripped {:from {:tile from-id :port from-port}
                                            :to   {:tile to-id   :port to-port}}))))))))

(defn delete-link!
  [diagram-name from-id from-port]
  (swap! bot
         (fn [b]
           (update-in b [:diagrams diagram-name :links]
                      (fn [ls]
                        (vec (remove
                              (fn [l]
                                (and (= (get-in l [:from :tile]) from-id)
                                     (= (get-in l [:from :port]) from-port)))
                              ls)))))))

(defn update-tile-param!
  [diagram-name tile-id k v]
  (swap! bot assoc-in [:diagrams diagram-name :tiles tile-id :params k] v))

(defn toggle-collapsed!
  [diagram-name]
  (swap! bot update-in [:diagrams diagram-name :collapsed?] not))

;; ── Geometry helpers for SVG drawing ────────────────────────────────────────

(defn tile-abs-pos
  "Get the absolute SVG coordinate for a tile (origin + relative pos)."
  [diagram tile]
  (let [[ox oy] (or (:origin diagram) [0 0])
        [px py] (:pos tile)]
    [(+ ox px) (+ oy py)]))

(defn output-ports-of [tile]
  (let [spec (current-tile-spec tile)
        out  (or (:outputs spec)
                 (case (:kind tile)
                   :condition [:true :false]
                   :logic     (case (:type tile)
                                :branch [:a :b]
                                [:out])
                   :start     [:out]
                   []))]
    out))

(defn output-port-pos
  "Port position in absolute SVG coords.
   :true/:out/:a → NE edge midpoint
   :false/:b     → bottom flat edge midpoint"
  [diagram tile port]
  (let [[x y] (tile-abs-pos diagram tile)
        r     hex-r
        h4    (/ tile-h 4)]
    (case port
      ;; bottom flat edge midpoint: center of lower-left→lower-right
      (:false :b) [(+ x r) (+ y tile-h)]
      ;; NE edge midpoint: midpoint of upper-right → right vertices
      [(+ x (* 1.75 r)) (+ y h4)])))

(defn input-port-pos
  "Input port: NW edge midpoint (midpoint of upper-left → left vertices)."
  [diagram tile]
  (let [[x y] (tile-abs-pos diagram tile)
        r     hex-r
        h4    (/ tile-h 4)]
    [(+ x (* 0.25 r)) (+ y h4)]))

;; ── Save / load ─────────────────────────────────────────────────────────────

(defn save-bot! []
  (let [n (str/upper-case (str/trim @bot-name))]
    (cond
      (str/blank? n)
      (reset! save-status {:error "Bot name is required"})
      (re-find #"[^A-Z0-9_-]" n)
      (reset! save-status {:error "Bot name must be uppercase letters/digits"})
      (or (not (exists? js/csrfToken)) (str/blank? js/csrfToken))
      (reset! save-status {:error "No CSRF token — try refreshing the page"})
      :else
      (do
        (js/console.log "saving bot" n "csrf-token-length:" (count js/csrfToken))
        (POST (str (str (bot-base-path) "/") n)
          {:params {:name n :description "" :definition (pr-str @bot)}
           :format :transit
           :response-format :transit
           :handler (fn [_]
                      (reset! save-status {:ok (str "saved " n)})
                      (reset! bot-name n))
           :error-handler (fn [err]
                            (reset! save-status {:error (str "Save failed: " (pr-str err))}))})))))


;; ── List page (saved bots) ──────────────────────────────────────────────────

(defonce my-bots-list  (r/atom []))
(defonce all-bots-list (r/atom []))

(def page-bg "#04040E")
(def panel-bg "#0A0E1C")
(def border-c "#1A2A40")
(def text-c "#AABBCC")
(def accent  "#7AAAE0")
(def muted   "#556677")

(def list-card-style
  {:background panel-bg :border (str "1px solid " border-c)
   :border-radius "6px" :padding "12px 16px"
   :font-family "monospace" :color text-c
   :display "flex" :align-items "center" :gap "12px"})

(defn bot-list-page []
  (let [my-set (set (map :name @my-bots-list))]
    [:div {:style {:padding "48px" :background page-bg
                   :min-height "100vh" :color text-c
                   :font-family "monospace"}}
     [:div {:style {:display "flex" :align-items "center" :margin-bottom "24px"
                    :gap "16px"}}
      [:h2 {:style {:color accent :margin 0 :flex 1}} (str (bot-game-title) " — Bots")]
      [:a {:href (bot-home-path) :style {:color muted :text-decoration "none"
                                          :font-size "13px"}} "← home"]]

     [:div {:style {:margin-bottom "16px"}}
      [:a {:href (str (bot-base-path) "/new")
           :style {:display "inline-block"
                   :padding "10px 20px" :background "#10182A"
                   :border (str "1px solid " "#2A4A80") :border-radius "4px"
                   :color accent :text-decoration "none"
                   :font-family "monospace" :font-size "14px"}}
       "+ NEW BOT"]]

     [:h3 {:style {:color accent :font-size "13px" :margin "32px 0 12px"
                   :letter-spacing "2px"}} "YOUR BOTS"]
     (if (seq @my-bots-list)
       [:div {:style {:display "flex" :flex-direction "column" :gap "8px"}}
        (for [b @my-bots-list]
          ^{:key (:name b)}
          [:div {:style list-card-style}
           [:a {:href (str (str (bot-base-path) "/") (:name b))
                :style {:flex 1 :color accent :text-decoration "none"
                        :font-weight "bold"}}
            (:name b)]
           (when (:description b)
             [:span {:style {:color muted :font-size "12px"}}
              (:description b)])
           [:span {:style {:color "#445566" :font-size "11px"}}
            (str "diagrams: "
                 (count (:diagrams (:definition b))))]])]
       [:div {:style {:color muted :font-style "italic"}}
        "no bots yet — create one to design custom behavior"])

     [:h3 {:style {:color accent :font-size "13px" :margin "32px 0 12px"
                   :letter-spacing "2px"}} "OTHER BOTS"]
     (if-let [others (seq (remove #(my-set (:name %)) @all-bots-list))]
       [:div {:style {:display "flex" :flex-direction "column" :gap "8px"}}
        (for [b others]
          ^{:key (:name b)}
          [:div {:style list-card-style}
           [:a {:href (str (str (bot-base-path) "/") (:name b))
                :style {:flex 1 :color "#88AACC" :text-decoration "none"}}
            (:name b)]
           [:span {:style {:color muted :font-size "11px"}}
            (str "by " (or (:owner b) "?"))]])]
       [:div {:style {:color muted :font-style "italic"}}
        "no shared bots from other players"])]))

;; ── Editor: palette ─────────────────────────────────────────────────────────

(defn category-color [cat]
  (case cat
    :conditions "#5a3a1e"
    :logic      "#1e5a3a"
    :effects    "#3a1e5a"
    :best-of    "#1e3a5a"
    :flow       "#5a1e3a"
    "#333"))

(defn palette []
  [:div {:style {:width "200px" :background panel-bg
                 :border-right (str "1px solid " border-c)
                 :overflow-y "auto"
                 :padding "16px 12px"
                 :font-family "monospace"}}
   [:h3 {:style {:color accent :font-size "12px" :margin "0 0 8px"
                 :letter-spacing "2px"}} "TILES"]
   [:div {:style {:font-size "10px" :color muted :margin-bottom "12px"
                  :line-height "1.4"}}
    "click a tile then click on a diagram to add it"]
   (for [{:keys [category tiles]} (current-all-categories)]
     ^{:key category}
     [:div {:style {:margin-bottom "16px"}}
      [:div {:style {:color "#778899" :font-size "10px" :letter-spacing "1.5px"
                     :text-transform "uppercase" :margin-bottom "6px"}}
       (name category)]
      (for [t tiles]
        (let [selected? (and (= (:kind @palette-selection) (:kind t))
                             (= (:type @palette-selection) (:type t)))]
          ^{:key (str (:kind t) "-" (:type t))}
          [:div {:on-click #(reset! palette-selection
                                    (if selected? nil
                                        {:kind (:kind t) :type (:type t) :label (:label t)}))
                 :style {:padding "6px 10px" :margin-bottom "4px"
                         :background (if selected? "#2a4a80"
                                         (category-color category))
                         :border-radius "4px"
                         :cursor "pointer"
                         :color text-c :font-size "12px"
                         :transition "background 0.1s"}
                 :title (or (:description t) "")}
           (:label t)]))])])

;; ── Editor: SVG canvas ──────────────────────────────────────────────────────

(defn- svg-loc
  "Convert a mouse event to SVG viewport coordinates (before pan/zoom)."
  [e]
  (let [svg (or (.. e -currentTarget -ownerSVGElement)
                (.-currentTarget e))
        pt  (.createSVGPoint svg)
        _   (set! (.-x pt) (.-clientX e))
        _   (set! (.-y pt) (.-clientY e))
        ctm (.getScreenCTM svg)
        loc (.matrixTransform pt (.inverse ctm))]
    [(.-x loc) (.-y loc)]))

(declare canvas-pan-x canvas-pan-y canvas-zoom)

(defn- content-loc
  "Convert a mouse event to content-space coordinates (inside pan/zoom transform)."
  [e]
  (let [[sx sy] (svg-loc e)
        z @canvas-zoom]
    [(/ (- sx @canvas-pan-x) z)
     (/ (- sy @canvas-pan-y) z)]))

(defn port-color [port]
  (case port
    :true  "#88CC66"
    :false "#CC6666"
    :a     "#88BBCC"
    :b     "#BB88CC"
    :out   "#AABBCC"
    "#888"))

(defn render-tile [diagram-name diagram tile]
  (let [[x y] (tile-abs-pos diagram tile)
        spec (current-tile-spec tile)
        label (or (:label spec) (name (or (:type tile) :tile)))
        sel? (= @selected {:diagram diagram-name :tile (:id tile)})
        kind-color (case (:kind tile)
                     :condition "#5a3a1e"
                     :logic     "#1e5a3a"
                     :effect    "#3a1e5a"
                     :jump      "#5a1e3a"
                     :start     "#444444"
                     "#222")
        outs (output-ports-of tile)
        [ipx ipy] (input-port-pos diagram tile)
        cx   (+ x (/ tile-w 2))
        cy   (+ y (/ tile-h 2))]
    [:g {:key (str diagram-name "-" (name (:id tile)))}
     ;; hex body
     [:polygon
      {:points (hex-points-str x y)
       :fill kind-color
       :stroke (if sel? "#FFD030" "#555")
       :stroke-width (if sel? 2.5 1)
       :stroke-linejoin "round"
       :on-mouse-down
       (fn [e]
         (.stopPropagation e)
         (let [[mx my] (content-loc e)]
           (reset! selected {:diagram diagram-name :tile (:id tile)})
           (reset! tile-drag {:diagram diagram-name :tile (:id tile)
                              :ox (- mx x) :oy (- my y)})))}]
     ;; label
     [:text
      {:x cx :y (- cy 2)
       :text-anchor "middle"
       :font-family "monospace" :font-size "11"
       :fill "#FFF" :pointer-events "none"}
      label]
     ;; param summary
     (when (seq (:params tile))
       (let [params (:params tile)
             ;; Detect prefer-1/2/3 pattern → show "tower > matrix > foundry"
             prefer? (contains? params :prefer-1)
             txt (if prefer?
                   (str/join " > "
                             (keep #(when-let [v (get params %)]
                                      (name v))
                                   [:prefer-1 :prefer-2 :prefer-3]))
                   (str/join " "
                             (map (fn [[k v]]
                                    (str (name k) "=" (cond (keyword? v) (name v) :else (str v))))
                                  params)))
             txt (if (> (count txt) 22) (str (subs txt 0 20) "..") txt)]
         [:text
          {:x cx :y (+ cy 12)
           :text-anchor "middle"
           :font-family "monospace" :font-size "8"
           :fill "#bbb" :pointer-events "none"}
          txt]))
     ;; input port (NW edge midpoint) — not on start tiles
     (when (not= :start (:kind tile))
       [:circle
        {:cx ipx :cy ipy :r 5
         :fill "#222" :stroke "#888" :stroke-width 1
         :on-mouse-up
         (fn [e]
           (.stopPropagation e)
           (when-let [{:keys [from]} @link-drag]
             (when (= (:diagram from) diagram-name)
               (add-link! diagram-name (:tile from) (:port from)
                          (:id tile) :in))
             (reset! link-drag nil)))}])
     ;; output ports
     (for [port outs]
       (let [[px py] (output-port-pos diagram tile port)
             is-bottom? (contains? #{:false :b} port)]
         ^{:key (str (:id tile) "-" (name port))}
         [:g
          [:circle
           {:cx px :cy py :r 5
            :fill (port-color port) :stroke "#000" :stroke-width 1
            :on-mouse-down
            (fn [e]
              (.stopPropagation e)
              (let [[mx my] (content-loc e)]
                (reset! link-drag
                        {:from {:diagram diagram-name :tile (:id tile) :port port}
                         :mx mx :my my})))
            :on-click
            (fn [e]
              (when (.-shiftKey e)
                (.stopPropagation e)
                (delete-link! diagram-name (:id tile) port)))}]
          ;; port label
          [:text
           {:x (if is-bottom? (+ px 10) (+ px 6))
            :y (if is-bottom? (+ py 2) (- py 6))
            :font-family "monospace" :font-size "8"
            :fill (port-color port) :pointer-events "none"}
           (name port)]]))]))

;; ── Link / wire routing ──────────────────────────────────────────────────────
;; Each port has a "face direction" — the outward normal of its hex edge.
;; We leave a port tangent to that face and arrive at the input tangent to
;; its face.  This keeps wires in the channels between hexes and prevents
;; them from cutting through hex bodies.
;;
;; NE port face direction:  ~(cos30°, -sin30°) = (0.866, -0.5)
;; NW port face direction:  ~(-cos30°, -sin30°) = (-0.866, -0.5)
;; Bottom port face direction: (0, 1)

(def ne-dx  0.866)
(def ne-dy -0.5)
(def nw-dx -0.866)
(def nw-dy -0.5)

(defn render-link [diagram-name diagram link]
  (let [from-tile (get-in diagram [:tiles (get-in link [:from :tile])])
        to-tile   (get-in diagram [:tiles (get-in link [:to :tile])])]
    (when (and from-tile to-tile)
      (let [from-port (get-in link [:from :port])
            [x1 y1]  (output-port-pos diagram from-tile from-port)
            [x2 y2]  (input-port-pos diagram to-tile)
            dist     (Math/sqrt (+ (* (- x2 x1) (- x2 x1))
                                   (* (- y2 y1) (- y2 y1))))
            arm      (max 20 (* 0.35 dist))
            is-down? (contains? #{:false :b} from-port)
            ;; Control points: leave the port along its face normal,
            ;; arrive at the input along its face normal (reversed).
            [c1x c1y c2x c2y]
            (if is-down?
              ;; Bottom port exit: straight down (0, +1)
              ;; NW port arrival: approach from upper-left along NW face outward
              [(+ x1 0)              (+ y1 arm)
               (+ x2 (* nw-dx arm))  (+ y2 (* nw-dy arm))]
              ;; NE port exit: along NE face outward (0.866, -0.5)
              ;; NW port arrival: approach from upper-left along NW face outward
              [(+ x1 (* ne-dx arm))  (+ y1 (* ne-dy arm))
               (+ x2 (* nw-dx arm))  (+ y2 (* nw-dy arm))])
            d     (str "M " x1 " " y1
                       " C " c1x " " c1y " " c2x " " c2y " " x2 " " y2)
            color (port-color from-port)]
        [:path
         {:key (str diagram-name "-link-"
                    (name (get-in link [:from :tile])) "-"
                    (name (get-in link [:from :port])) "-"
                    (name (get-in link [:to :tile])))
          :d d :stroke color :stroke-width 2 :fill "none"
          :on-click (fn [e]
                      (when (.-shiftKey e)
                        (.stopPropagation e)
                        (delete-link! diagram-name
                                      (get-in link [:from :tile])
                                      (get-in link [:from :port]))))}]))))

(def collapsed-w 150)

(defn- start-diagram-drag!
  "Begin dragging a whole diagram by its title bar or background."
  [e diagram-name diagram]
  (.stopPropagation e)
  (let [[mx my] (content-loc e)
        [ox oy] (or (:origin diagram) [0 0])]
    (reset! diagram-drag {:diagram diagram-name
                          :start-origin [ox oy]
                          :mx mx :my my})))

(defn render-diagram [diagram-name diagram]
  (let [[rx ry rw rh] (tile-bounds diagram)
        title (or (:name diagram) (name diagram-name))
        collapsed? (:collapsed? diagram)
        show-w (if collapsed? collapsed-w rw)
        show-h (if collapsed? region-title-h rh)
        color  (or (:color diagram) "#1e3a5a")]
    [:g {:key (str "dg-" (name diagram-name))}
     ;; region background — drag diagram or place palette tile
     [:rect
      {:x rx :y ry :width show-w :height show-h
       :rx 8 :ry 8
       :fill color :fill-opacity 0.18
       :stroke color :stroke-width 1.5
       :style {:cursor (if @palette-selection "crosshair" "grab")}
       :on-mouse-down
       (fn [e]
         (if @palette-selection
           ;; placing a palette tile
           (do (.stopPropagation e)
               (let [[mx my] (content-loc e)
                     [ox oy] (or (:origin diagram) [0 0])]
                 (add-tile-to-diagram! diagram-name @palette-selection
                                      [(- mx ox) (- my oy)])))
           ;; drag the diagram
           (start-diagram-drag! e diagram-name diagram)))}]
     ;; title bar — always drag
     [:rect
      {:x rx :y ry :width show-w :height region-title-h
       :rx 8 :ry 8
       :fill color :fill-opacity 0.55
       :style {:cursor "grab"}
       :on-mouse-down (fn [e] (start-diagram-drag! e diagram-name diagram))}]
     ;; collapse/expand toggle (small triangle at left of title bar)
     [:text
      {:x (+ rx 6) :y (+ ry 16)
       :font-family "monospace" :font-size "11"
       :fill "#FFF" :opacity 0.6
       :style {:cursor "pointer"}
       :on-click (fn [e]
                   (.stopPropagation e)
                   (toggle-collapsed! diagram-name))}
      (if collapsed? "▶" "▼")]
     ;; title text
     [:text
      {:x (+ rx 22) :y (+ ry 15)
       :font-family "monospace" :font-size "12"
       :fill "#FFF" :pointer-events "none"}
      title]
     ;; tiles + links (hidden when collapsed)
     (when-not collapsed?
       [:g
        (for [link (:links diagram)]
          (render-link diagram-name diagram link))
        (for [[_ tile] (:tiles diagram)]
          (render-tile diagram-name diagram tile))])]))

(defonce canvas-pan-x (r/atom 0))
(defonce canvas-pan-y (r/atom 0))
(defonce canvas-zoom  (r/atom 1.0))
(defonce canvas-drag  (r/atom nil))

(def pan-step 80)

(defn canvas []
  (r/with-let
    [on-key
     (fn [e]
       (case (.-key e)
         "ArrowLeft"  (swap! canvas-pan-x + pan-step)
         "ArrowRight" (swap! canvas-pan-x - pan-step)
         "ArrowUp"    (swap! canvas-pan-y + pan-step)
         "ArrowDown"  (swap! canvas-pan-y - pan-step)
         "PageUp"     (swap! canvas-zoom #(min 4.0 (* % 1.2)))
         "PageDown"   (swap! canvas-zoom #(max 0.15 (/ % 1.2)))
         " "          (do (reset! canvas-pan-x 0)
                          (reset! canvas-pan-y 0)
                          (reset! canvas-zoom 1.0))
         nil))
     on-wheel
     (fn [e]
       (.preventDefault e)
       (let [delta (.-deltaY e)]
         (if (neg? delta)
           (swap! canvas-zoom #(min 4.0 (* % 1.1)))
           (swap! canvas-zoom #(max 0.15 (/ % 1.1))))))
     _ (js/document.addEventListener "keydown" on-key)]
    (let [b @bot
          z @canvas-zoom]
      [:div {:style {:flex 1 :background "#04040E"
                     :overflow "hidden"
                     :position "relative"}
             :on-wheel on-wheel}
       [:svg
        {:width "100%" :height "100%"
         :on-mouse-down
         (fn [e]
           (when-not @palette-selection
             (let [[sx sy] (svg-loc e)]
               (reset! canvas-drag {:mx sx :my sy
                                    :px @canvas-pan-x :py @canvas-pan-y}))))
         :on-mouse-move
         (fn [e]
           (cond
             @tile-drag
             (let [[cx cy] (content-loc e)
                   {:keys [diagram tile ox oy]} @tile-drag
                   d (get-in @bot [:diagrams diagram])
                   [orx ory] (or (:origin d) [0 0])
                   nx (- cx ox orx)
                   ny (- cy oy ory)]
               (move-tile! diagram tile [nx ny]))

             @diagram-drag
             (let [[cx cy] (content-loc e)
                   {:keys [diagram start-origin mx my]} @diagram-drag
                   [sx sy] start-origin
                   dx (- cx mx)
                   dy (- cy my)]
               (swap! bot assoc-in [:diagrams diagram :origin]
                      [(+ sx dx) (+ sy dy)]))

             @link-drag
             (let [[cx cy] (content-loc e)]
               (swap! link-drag assoc :mx cx :my cy))

             @canvas-drag
             (let [[sx sy] (svg-loc e)  ;; canvas pan works in viewport space
                   {:keys [mx my px py]} @canvas-drag]
               (reset! canvas-pan-x (+ px (- sx mx)))
               (reset! canvas-pan-y (+ py (- sy my))))))
         :on-mouse-up
         (fn [_]
           (when @diagram-drag
             (swap! bot relayout))
           (reset! tile-drag nil)
           (reset! diagram-drag nil)
           (reset! link-drag nil)
           (reset! canvas-drag nil))
         :style {:cursor (cond @canvas-drag "grabbing"
                               @diagram-drag "grabbing"
                               @palette-selection "crosshair"
                               :else "default")}}
        [:g {:transform (str "translate(" @canvas-pan-x " " @canvas-pan-y ")"
                             " scale(" z ")")}
         (for [[k d] (:diagrams b)]
           ^{:key (name k)} (render-diagram k d))
         ;; in-progress link drag
         (when-let [{:keys [from mx my]} @link-drag]
           (let [d  (get-in b [:diagrams (:diagram from)])
                 t  (get-in d [:tiles (:tile from)])
                 [px py] (output-port-pos d t (:port from))]
             [:line {:x1 px :y1 py :x2 mx :y2 my
                     :stroke (port-color (:port from))
                     :stroke-width 2 :stroke-dasharray "4,3"}]))]]
       ;; zoom indicator + controls
       [:div {:style {:position "absolute" :bottom "12px" :left "12px"
                      :display "flex" :align-items "center" :gap "8px"}}
        [:input {:type "range" :min 15 :max 400 :step 5
                 :value (int (* z 100))
                 :on-change #(reset! canvas-zoom (/ (js/parseInt (.. % -target -value)) 100))
                 :style {:width "90px" :cursor "pointer"}}]
        [:span {:style {:color muted :font-family "monospace" :font-size "10px"
                        :min-width "32px"}}
         (str (int (* z 100)) "%")]]
       ;; status overlay
       [:div {:style {:position "absolute" :bottom "30px" :left "12px"
                      :color muted :font-family "monospace" :font-size "10px"}}
        (cond
          @palette-selection
          (str "click in a diagram to place: " (:label @palette-selection))
          @link-drag
          "drop on input port to link · shift-click port to delete"
          @diagram-drag
          "release to reposition diagram"
          :else
          "arrows: pan · pgup/pgdn: zoom · space: reset · drag titles to move diagrams")]])
    (finally
      (js/document.removeEventListener "keydown" on-key))))

;; ── Editor: properties panel ────────────────────────────────────────────────

(def input-style
  {:background "#0A0E1C" :color "#AACCEE"
   :border "1px solid #2A4A80" :border-radius "4px"
   :padding "6px 10px" :font-family "monospace" :font-size "12px"
   :width "100%"})

(defn render-param [diagram-name tile param-spec]
  (let [k (:key param-spec)
        v (get-in tile [:params k] (:default param-spec))
        on-change (fn [new-v]
                    (update-tile-param! diagram-name (:id tile) k new-v))]
    [:div {:key (name k) :style {:margin-bottom "8px"}}
     [:label {:style {:color muted :font-size "10px" :display "block"
                      :margin-bottom "3px"}}
      (name k)]
     (case (:type param-spec)
       :enum
       [:select {:value (str v)
                 :on-change (fn [e]
                              (let [s (.. e -target -value)]
                                (on-change (keyword (subs s 1)))))
                 :style input-style}
        (for [opt (:options param-spec)]
          ^{:key (str opt)} [:option {:value (str opt)} (name opt)])]

       :int
       [:input {:type "number" :value (or v 0)
                :on-change #(on-change (js/parseInt (.. % -target -value)))
                :style input-style}]

       :float
       [:input {:type "number" :step "0.05" :value (or v 0)
                :on-change #(on-change (js/parseFloat (.. % -target -value)))
                :style input-style}]

       :diagram-ref
       [:select {:value (str v)
                 :on-change (fn [e]
                              (let [s (.. e -target -value)]
                                (on-change (when (seq s) (keyword (subs s 1))))))
                 :style input-style}
        [:option {:value ""} "(none)"]
        (for [k2 (keys (:diagrams @bot))]
          ^{:key (name k2)}
          [:option {:value (str k2)} (name k2)])]

       [:input {:type "text" :value (str (or v ""))
                :on-change #(on-change (.. % -target -value))
                :style input-style}])]))

(defn properties-panel []
  [:div {:style {:width "280px" :background panel-bg
                 :border-left (str "1px solid " border-c)
                 :overflow-y "auto"
                 :padding "16px 14px"
                 :font-family "monospace" :color text-c}}
   ;; bot meta
   [:h3 {:style {:color accent :font-size "12px" :margin "0 0 12px"
                 :letter-spacing "2px"}} "BOT"]
   [:div {:style {:margin-bottom "8px"}}
    [:label {:style {:color muted :font-size "10px" :display "block"
                     :margin-bottom "3px"}} "name (ALL CAPS)"]
    [:input {:type "text" :value @bot-name
             :on-change #(reset! bot-name (str/upper-case (.. % -target -value)))
             :placeholder "ASTRA"
             :style input-style}]]
   [:button {:on-click save-bot!
             :style {:width "100%" :background "#10182A"
                     :border "1px solid #2A4A80" :border-radius "4px"
                     :padding "8px" :color accent :cursor "pointer"
                     :font-family "monospace" :font-size "13px"
                     :margin-bottom "6px"}}
    "SAVE BOT"]
   (when-let [s @save-status]
     [:div {:style {:font-size "11px"
                    :color (if (:error s) "#CC6666" "#88CC66")
                    :margin-bottom "8px"}}
      (or (:error s) (:ok s))])
   [:a {:href (bot-base-path)
        :style {:font-size "11px" :color muted :text-decoration "none"
                :display "block" :margin-bottom "16px"}}
    "← all bots"]

   ;; diagrams
   [:h3 {:style {:color accent :font-size "12px" :margin "16px 0 8px"
                 :letter-spacing "2px"}} "DIAGRAMS"]
   (for [[k d] (:diagrams @bot)]
     ^{:key (name k)}
     [:div {:style {:padding "6px 10px" :margin-bottom "4px"
                    :background (or (:color d) "#1e3a5a")
                    :background-color "transparent"
                    :border (str "1px solid " (or (:color d) "#1e3a5a"))
                    :border-radius "4px" :font-size "12px"
                    :display "flex" :align-items "center" :gap "8px"}}
      [:span {:style {:flex 1 :color text-c}} (or (:name d) (name k))]
      [:button {:on-click #(auto-layout! k)
                :title "auto-layout tiles to minimize edge crossings"
                :style {:background "none" :border "1px solid #2A4A80"
                        :border-radius "3px" :cursor "pointer"
                        :color accent :font-size "10px" :padding "2px 6px"}}
       "layout"]
      (when (and (not= k (or (:start-diagram @bot) :main))
                 (not= k :main))
        [:button {:on-click #(delete-diagram! k)
                  :style {:background "none" :border "none" :cursor "pointer"
                          :color "#886666" :font-size "11px"}}
         "✕"])])
   [:button {:on-click (fn []
                         (let [n (js/prompt "diagram name:")]
                           (when (and n (seq (str/trim n)))
                             (add-diagram! n))))
             :style {:width "100%" :background "transparent"
                     :border (str "1px dashed " muted) :border-radius "4px"
                     :padding "6px" :color muted :cursor "pointer"
                     :font-family "monospace" :font-size "11px"
                     :margin-top "8px"}}
    "+ NEW DIAGRAM"]

   ;; selected tile
   (when-let [{:keys [diagram tile]} @selected]
     (when-let [t (get-in @bot [:diagrams diagram :tiles tile])]
       [:div {:style {:margin-top "20px" :padding-top "16px"
                      :border-top (str "1px solid " border-c)}}
        [:h3 {:style {:color accent :font-size "12px" :margin "0 0 8px"
                      :letter-spacing "2px"}}
         (str "TILE: " (name (or (:type t) :?)))]
        [:div {:style {:color muted :font-size "10px" :margin-bottom "12px"}}
         (or (:description (current-tile-spec t)) "")]
        (let [spec (current-tile-spec t)]
          (for [p (:params spec)]
            ^{:key (name (:key p))}
            [render-param diagram t p]))
        (when (not= :start (:kind t))
          [:button {:on-click #(delete-tile! diagram tile)
                    :style {:width "100%" :background "transparent"
                            :border "1px solid #4A2A2A" :border-radius "4px"
                            :padding "6px" :color "#886666" :cursor "pointer"
                            :font-family "monospace" :font-size "11px"
                            :margin-top "12px"}}
           "DELETE TILE"])]))])

(defn bot-editor-page []
  [:div {:style {:display "flex" :width "100vw" :height "100vh"
                 :background page-bg :overflow "hidden"}}
   [palette]
   [canvas]
   [properties-panel]])

;; ── Page container ──────────────────────────────────────────────────────────

(defn page-container []
  (cond
    (and (exists? js/isBotEditor) js/isBotEditor) [bot-editor-page]
    (and (exists? js/isBotList)   js/isBotList)   [bot-list-page]
    :else [:div]))

;; ── Init ────────────────────────────────────────────────────────────────────

(defn mount-components []
  (rdom/render [#'page-container] (.getElementById js/document "journey")))

(defn- safe-read [s]
  (when (and (string? s) (not (str/blank? s)))
    (try (reader/read-string s) (catch :default _ nil))))

(defn init! []
  (ajax/load-interceptors!)
  (if (and (exists? js/preloadedBot) js/preloadedBot)
    (let [b (safe-read js/preloadedBot)]
      (if (and b (:definition b))
        (do (reset! bot (relayout (:definition b)))
            (reset! bot-name (or (:name b) ""))
            (reset! bot-owner (:owner b)))
        (reset! bot (current-default-bot))))
    (when-not @bot
      (reset! bot (current-default-bot))))
  (when (and (exists? js/preloadedBotName) js/preloadedBotName
             (str/blank? @bot-name))
    (reset! bot-name js/preloadedBotName))
  (when (and (exists? js/myBots) js/myBots)
    (when-let [v (safe-read js/myBots)]
      (reset! my-bots-list v)))
  (when (and (exists? js/allBots) js/allBots)
    (when-let [v (safe-read js/allBots)]
      (reset! all-bots-list v)))
  (mount-components))
