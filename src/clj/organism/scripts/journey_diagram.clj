(ns organism.scripts.journey-diagram
  "Render the JOURNEY action reference sheet to SVG, 10×8in landscape, dark on white.

   Content follows the hand-drawn journey-diagram sketch (v18); the layout and
   visual grammar follow SOL's CHOOSE AN ACTION sheet (banner bars, before →
   conversion → after), since JOURNEY is its sequel and uses SOL's pieces.

   Board pieces come from journey.board — the same primitives the live board
   renders with, recoloured for paper — and the piece symbols from
   journey.pieces, which carries SOL's own artwork.  Numbers are read out of
   journey.game so the sheet cannot drift from the implementation.

   `lein run -m organism.scripts.journey-diagram --pieces` writes the symbol
   sheet on its own, for porting the pieces into the site.

     lein run -m organism.scripts.journey-diagram [out.svg]

   Default output: resources/public/img/journey-diagram.svg

   Anything the sketch shows that the code does not implement is drawn faded,
   dashed and tagged SKETCH ONLY rather than silently dropped or quietly
   promoted into the rules."
  (:require
   [clojure.string :as string]
   [journey.board :as board]
   [journey.game :as game]
   [journey.pieces :as pieces]))

;; ── hiccup → SVG ──────────────────────────────────────────────────────────────
;; journey.board returns reagent-style hiccup: [component & args] vectors and
;; lazy seqs of children.  `expand` resolves the components, `emit` serializes.

(def ^:private skip-attrs
  #{:key :style :on-click :on-mouse-enter :on-mouse-leave :on-mouse-over})

(defn- expand
  [form]
  (cond
    (and (vector? form) (fn? (first form))) (expand (apply (first form) (rest form)))
    (vector? form)                          (mapv expand form)
    (seq? form)                             (doall (map expand form))
    :else                                   form))

(defn- num-str
  [x]
  (if (integer? x)
    (str x)
    (let [r (/ (Math/round (* 100.0 (double x))) 100.0)]
      (if (== r (Math/rint r)) (str (long r)) (str r)))))

(defn- esc
  [s]
  (-> (str s)
      (string/replace "&" "&amp;")
      (string/replace "<" "&lt;")
      (string/replace ">" "&gt;")
      (string/replace "\"" "&quot;")))

(defn- attrs->str
  [attrs]
  (apply str
         (for [[k v] attrs
               :when (and (not (skip-attrs k)) (some? v))]
           (str " " (name k) "=\""
                (esc (cond (keyword? v) (name v)
                           (number? v)  (num-str v)
                           :else        v))
                "\""))))

(defn- emit
  [form]
  (cond
    (nil? form)     ""
    (string? form)  (esc form)
    (number? form)  (num-str form)
    (keyword? form) (esc (name form))
    (vector? form)  (let [[tag & more] form
                          attrs (when (map? (first more)) (first more))
                          kids  (if attrs (rest more) more)
                          inner (apply str (map emit kids))]
                      (if (string/blank? inner)
                        (str "<" (name tag) (attrs->str attrs) "/>")
                        (str "<" (name tag) (attrs->str attrs) ">"
                             inner
                             "</" (name tag) ">")))
    (seq? form)     (apply str (map emit form))
    :else           (esc (str form))))

(defn ->svg
  "Serialize reagent-style hiccup to an SVG string, trimming long floats."
  [form]
  (string/replace (emit (expand form)) #"(\d+\.\d\d)\d+" "$1"))

;; ── print palette ─────────────────────────────────────────────────────────────

(def ^:private paper   "#FFFFFF")
(def ^:private ink     "#15151C")
(def ^:private dim     "#4E4E5A")
(def ^:private faint   "#93939F")
(def ^:private hexline "#7C7C8A")
(def ^:private amber   "#B07800")   ; the Ark, its heading, the flame, choices
(def ^:private cyan    "#16729E")   ; conversion
(def ^:private moss    "#1C7A3E")   ; movement
(def ^:private flare-c "#C42B14")
(def ^:private bag-c   "#6A2FA8")
(def ^:private sans    "'Helvetica Neue',Helvetica,Arial,sans-serif")
(def ^:private mono    "'DejaVu Sans Mono',Menlo,'Courier New',monospace")

(def ^:private you-c   "#2B5FD0")
(def ^:private other-c "#0E7A44")

;; Substituted into journey.board while the sheet renders, so every board shape
;; comes out in paper inks without touching the game's own palette.
(def ^:private print-world-outer
  {:sun "#DE7412" :silver "#8A90A0" :green "#0E7A44"
   :blue "#2B5FD0" :purple "#7B2CB5" :void "#2A2A38"})

(def ^:private print-world-inner
  {:sun "#F0BE16" :silver "#C6CAD4" :green "#5CB584"
   :blue "#86A4EE" :purple "#BE7EE2" :void "#747482"})

;; light tints for the hex ground — the world token carries the real colour
(def ^:private tint
  {:sun "#FCEAD6" :silver "#EDEEF2" :green "#DCF0E5"
   :blue "#E2E8FB" :purple "#EFE1F8" :void "#E5E5EA"})

(def ^:private you   :blue)
(def ^:private other :green)

(def ^:private markers
  {"ink"   dim
   "amber" amber
   "faint" faint
   "you"   you-c
   "other" other-c
   "flare" flare-c
   "bag"   bag-c
   "cyan"  cyan
   "moss"  moss})

;; ── piece symbols ─────────────────────────────────────────────────────────────
;; journey.pieces carries SOL's own artwork at true relative sizes (millimetres),
;; so everything drawn with the same `unit` stays in proportion — the tower
;; stands two and a half foundries tall, as it does on the table.

(def ^:private space-h (* board/hex-size board/sqrt3))  ; a board space, 86.6 units
(def ^:private icon-space 30)                          ; a space, in points, for icons

(defn- piece
  "A piece symbol at `unit`, base at the origin.  `outer` is the scale the
   calling group applies, so the contour lines land near 0.3pt on paper and
   drop out entirely once the symbol is too small to carry them."
  [t unit & [{:keys [ck outer line]}]]
  (let [ck      (or ck you)
        outer   (or outer 1.0)
        printed (* (pieces/height t unit) outer)]
    (pieces/piece t unit {:fill (board/pwo ck)
                          :line (when (>= printed 10) (or line paper))
                          :line-width (/ 0.3 (* unit outer))})))

(defn- level-bars
  "JOURNEY's level platforms, stacked under a piece's base."
  [n ck h]
  (let [bw (* h 0.8) bh (* h 0.1) gap (* h 0.17)]
    (for [i (range n)]
      [:rect {:x (- (/ bw 2.0)) :y (+ (* h 0.05) (* i gap)) :width bw :height bh
              :rx (* h 0.02)
              :fill (board/pwo ck) :stroke (board/pwi ck) :stroke-width (* h 0.035)}])))

(defn- station-icon
  "A station standing on its level platforms, its base on the tile centre."
  [t level ck unit outer]
  (let [h (pieces/height t unit)]
    [:g {:transform (str "translate(0," (num-str (* h 0.2)) ")")}
     (piece t unit {:ck ck :outer outer})
     (level-bars level ck (* h 0.36))]))

;; ── text ──────────────────────────────────────────────────────────────────────

(defn- txt
  [x y s & [{:keys [size fill anchor family weight spacing opacity]}]]
  [:text {:x x :y y
          :fill (or fill ink)
          :font-size (or size 6.4)
          :font-family (or family mono)
          :font-weight weight
          :text-anchor anchor
          :letter-spacing spacing
          :opacity opacity}
   s])

(defn- lines
  "Stack of caption lines from [x y].  \"\" leaves a blank line."
  [x y ls & [{:keys [size fill lead anchor]}]]
  (let [lead (or lead 8.4)]
    (map-indexed
     (fn [i s]
       (when-not (= s "")
         (txt x (+ y (* i lead)) s {:size (or size 6.2) :fill (or fill dim)
                                    :anchor anchor})))
     ls)))

(defn- caps
  [x y s & [{:keys [size fill anchor spacing]}]]
  (txt x y s {:size (or size 5.6) :fill (or fill faint) :family sans
              :weight "600" :spacing (or spacing 0.9) :anchor anchor}))

(defn- banner
  "SOL's label bar: solid ink, paper-coloured caps."
  [x y w s & [{:keys [size]}]]
  [:g
   [:rect {:x x :y y :width w :height 11 :fill ink}]
   (txt (+ x (/ w 2.0)) (+ y 7.7) s
        {:size (or size 6.4) :fill paper :anchor "middle" :spacing 1.3
         :family sans :weight "600"})])

(defn- sketch-tag
  [x y & [{:keys [anchor]}]]
  (txt x y "SKETCH ONLY" {:size 5 :fill faint :spacing 1.1 :anchor anchor}))

;; ── marks ─────────────────────────────────────────────────────────────────────

(defn- move-mark
  [c]
  [:g (for [i (range 3)]
        [:path {:d (str "M " (+ -6 (* i 5)) ",-4 L " (+ -2 (* i 5)) ",0 L "
                        (+ -6 (* i 5)) ",4")
                :fill "none" :stroke c :stroke-width 1.7 :stroke-linecap "round"}])])

(defn- atom-mark
  [c]
  [:g
   [:circle {:cx 0 :cy 0 :r 2.1 :fill c}]
   [:ellipse {:cx 0 :cy 0 :rx 6.4 :ry 2.6 :fill "none" :stroke c :stroke-width 1
              :transform "rotate(32)"}]
   [:ellipse {:cx 0 :cy 0 :rx 6.4 :ry 2.6 :fill "none" :stroke c :stroke-width 1
              :transform "rotate(-32)"}]])

(defn- bolt-mark
  [c]
  [:path {:d "M 2.5,-7 L -4,1.5 L 0,1.5 L -2,7 L 4.5,-1.5 L 0.5,-1.5 Z"
          :fill c :stroke c :stroke-width 0.4}])

(defn- column-head
  [x y s mark-fn mark-c]
  [:g
   [:g {:transform (str "translate(" (+ x 7) "," (- y 5) ")")} (mark-fn mark-c)]
   (txt (+ x 22) y s {:size 15 :family sans :weight "500" :spacing 3.2})])

;; ── arrows ────────────────────────────────────────────────────────────────────

(defn- arrow
  "Arrow from [x1 y1] to [x2 y2].  :bow curves it, :head false leaves a trail."
  [[x1 y1] [x2 y2] & [{:keys [c width bow dash head fat]}]]
  (let [c   (or c "ink")
        len (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2)))
        bow (or bow 0)
        ux  (if (zero? len) 0.0 (/ (- x2 x1) len))
        uy  (if (zero? len) 0.0 (/ (- y2 y1) len))]
    [:path {:d (str "M " (num-str x1) "," (num-str y1)
                    " Q " (num-str (+ (/ (+ x1 x2) 2.0) (* (- uy) bow))) ","
                    (num-str (+ (/ (+ y1 y2) 2.0) (* ux bow))) " "
                    (num-str x2) "," (num-str y2))
            :fill "none"
            :stroke (markers c)
            :stroke-width (or width 1.1)
            :stroke-dasharray dash
            :stroke-linecap "round"
            :marker-end (when (not= head false)
                          (str "url(#ah-" (when fat "fat-") c ")"))}]))

(defn- no-mark
  "Circle-slash: goes no further."
  [x y r]
  [:g {:transform (str "translate(" x "," y ")")}
   [:circle {:cx 0 :cy 0 :r r :fill paper :fill-opacity 0.85
             :stroke flare-c :stroke-width 1.3}]
   [:line {:x1 (* -0.68 r) :y1 (* 0.68 r) :x2 (* 0.68 r) :y2 (* -0.68 r)
           :stroke flare-c :stroke-width 1.3}]])

(defn- sundiver-glyph
  "One sundiver, upright and centred on [0 0], drawn at `unit`."
  [ck unit & [{:keys [outer]}]]
  [:g {:transform (str "translate(0," (num-str (* 0.5 (pieces/height :sundiver unit))) ")")}
   (piece :sundiver unit {:ck ck :outer (or outer 1.0)})])

;; Sundivers fan around the world token as they do on the board, a size up:
;; at print scale the board's own 12px triangles disappear.
(defn- toward-angle
  "Degrees from a tile's centre toward its neighbour in direction d."
  [d]
  (let [[dx dy] (board/hex->pixel d)]
    (* (/ 180 Math/PI) (Math/atan2 dy dx))))

(defn- facing-angle
  "Degrees, from a tile at direction d, back toward the tile it neighbours."
  [d]
  (+ 180 (toward-angle d)))

(defn- tile-sundivers
  "Sundivers standing in the middle of a space."
  [sundivers _angle outer]
  (let [all  (mapcat (fn [[ck n]] (repeat n ck)) sundivers)
        step (* space-h 0.3)
        x0   (* -0.5 (dec (count all)) step)]
    [:g
     (for [[i ck] (map-indexed vector all)]
       [:g {:transform (str "translate(" (num-str (+ x0 (* i step))) ",0)")}
        (sundiver-glyph ck space-h {:outer outer})])]))

(defn- cost
  "\"− sundiver\" cost tag."
  [x y & [{:keys [ck unit]}]]
  (let [ck (or ck you)
        u  (or unit 24)
        h  (pieces/height :sundiver u)]
    [:g {:transform (str "translate(" x "," y ")")}
     (txt (* h -0.9) (* h 0.3) "−" {:size (* h 1.1) :fill (board/pwo ck) :family sans})
     (sundiver-glyph ck u)]))

(defn- dashed-box
  [x y w h & [{:keys [c]}]]
  [:rect {:x x :y y :width w :height h :rx 3
          :fill "none" :stroke (or c faint) :stroke-width 0.6
          :stroke-dasharray "2.5 2"}])

;; ── board hexes ───────────────────────────────────────────────────────────────
;; Flat-top hexes: the six neighbours are up, down and the four diagonals.

(def ^:private up   [0 -1])
(def ^:private up-r [1 -1])
(def ^:private dn-r [1 0])
(def ^:private dn   [0 1])
(def ^:private dn-l [-1 1])
(def ^:private up-l [-1 0])

(defn- tile-hex
  "One board hex at the origin.
     :color / :world? / :beacon / :station {:type :level} / :sundivers / :sd-angle
     :ark and :heading take a heading direction
     :arcs and :gates take [[dir colour-key] …]
     :mark is :target (amber) · :ghost (dashed) · :hatch (unavailable)"
  [{:keys [color world? beacon station sundivers sd-angle ark heading arcs mark]
    ::keys [scale] :or {scale 1.0}}]
  (let [world? (if (nil? world?) (some? color) world?)
        pts    (board/hex-pts-str (- board/hex-size 1))]
    [:g
     [:polygon {:points pts
                :fill (cond (= mark :hatch) "url(#hatch)"
                            color           (tint color)
                            :else           paper)
                :stroke (case mark
                          :target amber
                          :ghost  faint
                          :hatch  faint
                          hexline)
                :stroke-width (if (= mark :target) 7 4)
                :stroke-dasharray (when (and (not= mark :target)
                                             (or (nil? color) (#{:ghost :hatch} mark)))
                                    "9 8")}]
     ;; cipher-match arcs
     (for [[dir ck] arcs
           :let [[ex ey] (board/edge-offset dir)
                 ang     (* (/ 180 Math/PI) (Math/atan2 ey ex))
                 c       (board/pwo ck)]]
       [:path {:d "M 0,-19 A 13,19 0 0,0 0,19 L 0,15 A 9,15 0 0,1 0,-15 Z"
               :transform (str "translate(" (num-str ex) "," (num-str ey) ") "
                               "rotate(" (num-str ang) ")")
               :fill c :stroke c :stroke-width 2}])
     ;; a world already discovered keeps its hex but loses its token — the
     ;; sketch draws the empty socket as a dotted circle
     (if world?
       (board/world-token color)
       (when color
         [:circle {:cx 0 :cy 0 :r 24 :fill "none" :stroke faint
                   :stroke-width 3 :stroke-dasharray "6 7"}]))
     (when beacon
       [:g {:transform "translate(0,-10) scale(1.4)"} (board/beacon-shape beacon)])
     (when station
       [:g {:transform "translate(0,4)"}
        (station-icon (:type station) (:level station 0) (:ck station you)
                      space-h scale)])
     (when ark
       [:g (board/ark-ring) (board/ark-heading-arrow ark)])
     (when heading (board/heading-marker heading))
     (when (seq sundivers) (tile-sundivers sundivers sd-angle scale))]))

(defn- cluster
  "Board hexes as [[q r] spec] pairs, centred on their own centroid at [x y]."
  [x y scale tiles]
  (let [[ox oy] (board/board-centroid tiles)]
    [:g {:transform (str "translate(" (num-str x) "," (num-str y) ") "
                         "scale(" (num-str scale) ")")}
     (for [[pos spec] tiles
           :let [[cx cy] (board/hex->pixel pos)]]
       [:g {:transform (str "translate(" (num-str (- cx ox)) ","
                            (num-str (- cy oy)) ")")}
        (tile-hex (assoc spec ::scale scale))])
     ;; gates last, so no neighbouring space can eat them
     (for [[pos spec] tiles
           :let [[cx cy] (board/hex->pixel pos)]
           [dir ck] (:gates spec)
           :let [[ex ey] (board/edge-offset dir)]]
       [:g {:transform (str "translate(" (num-str (+ (- cx ox) ex)) ","
                            (num-str (+ (- cy oy) ey (* space-h 0.16))) ")")}
        (piece :gate space-h {:ck ck :outer scale})])]))

(defn- hex-at
  "Absolute [x y] of the hex at position d in a cluster placed at [cx cy]."
  [cx cy scale tiles d]
  (let [[ox oy] (board/board-centroid tiles)
        [px py] (board/hex->pixel d)]
    [(+ cx (* scale (- px ox))) (+ cy (* scale (- py oy)))]))

(defn- at
  "Place a board-scale glyph at [x y], scaled."
  [x y scale & body]
  [:g {:transform (str "translate(" (num-str x) "," (num-str y) ") "
                       "scale(" (num-str scale) ")")}
   body])

;; ── glyphs that are not board pieces ──────────────────────────────────────────

(defn- habitat
  [x y r n & [{:keys [ck]}]]
  (let [ck (or ck you)]
    [:g {:transform (str "translate(" x "," y ")")}
     [:circle {:cx 0 :cy 0 :r r :fill paper
               :stroke (board/pwo ck) :stroke-width 1}]
     (let [u    (* r 1.6)
           step (* r 0.62)
           x0   (* -0.5 (dec n) step)]
       (for [i (range n)]
         [:g {:transform (str "translate(" (num-str (+ x0 (* i step))) ",1)")}
          (sundiver-glyph ck u)]))]))

(defn- reserve
  [x y w h]
  [:g {:transform (str "translate(" x "," y ")")}
   [:rect {:x (- (/ w 2.0)) :y (- (/ h 2.0)) :width w :height h :rx 2
           :fill paper :stroke dim :stroke-width 0.9}]
   [:line {:x1 (+ (- (/ w 2.0)) 3) :y1 0 :x2 (- (/ w 2.0) 3) :y2 0
           :stroke faint :stroke-width 0.6}]])

(defn- card
  [x y & [{:keys [suit w h]}]]
  (let [w (or w 12) h (or h 16)
        sc (when suit (get game/suit-colors suit))]
    [:g {:transform (str "translate(" x "," y ")")}
     [:rect {:x (- (/ w 2.0)) :y (- (/ h 2.0)) :width w :height h :rx 1.6
             :fill (or sc paper) :stroke (if sc "#00000055" dim) :stroke-width 0.7}]
     (when suit (board/suit-icon suit paper (* w 0.3)))]))

(defn- bag
  [x y s]
  [:g {:transform (str "translate(" x "," y ") scale(" s ")")}
   [:path {:d (str "M -9,-12 L 9,-12 L 9,-5 C 20,1 22,15 12,21 "
                   "C 4,26 -4,26 -12,21 C -22,15 -20,1 -9,-5 Z")
           :fill paper :stroke bag-c :stroke-width 2 :stroke-linejoin "round"}]
   [:path {:d "M -11,-12 C -6,-18 6,-18 11,-12"
           :fill "none" :stroke bag-c :stroke-width 2 :stroke-linecap "round"}]])

(defn- cipher-rosette
  "The seven cipher slots and the colours registered in each."
  [x y s filled]
  [:g {:transform (str "translate(" x "," y ") scale(" s ")")}
   (for [pos (into [[0 0]] game/hex-directions)
         :let [[q r] pos
               cx    (* board/cipher-hex-size 1.5 q)
               cy    (* board/cipher-hex-size board/sqrt3 (+ (* 0.5 q) r))
               cs    (get filled pos)]]
     [:g {:transform (str "translate(" (num-str cx) "," (num-str cy) ")")}
      [:polygon {:points (board/hex-pts-str (- board/cipher-hex-size 1))
                 :fill (if cs (tint (first cs)) paper)
                 :stroke (if cs amber hexline)
                 :stroke-width (if cs 3 1.8)}]
      (for [c cs]
        [:circle {:cx 0 :cy 0 :r (* board/cipher-hex-size 0.34)
                  :fill (board/pwo c) :stroke (board/pwi c) :stroke-width 1.4}])])])

(defn- mini-board
  "A board reduced to coloured hexes, centred at [x y]; `rad` is the hex radius.
   Entries are [[q r] {:color c :mark m :ark dir :heading dir
                       :sundiver ck :beacon ck}]."
  [x y rad tiles]
  (let [s (/ rad 50.0)
        u (* rad 2.1)
        [ox oy] (board/board-centroid tiles)]
    [:g {:transform (str "translate(" (num-str x) "," (num-str y) ")")}
     (for [[pos spec] tiles
           :let [[px py] (board/hex->pixel pos)
                 {:keys [color mark ark heading sundiver beacon dim?]} spec]]
       [:g {:transform (str "translate(" (num-str (* (- px ox) s)) ","
                            (num-str (* (- py oy) s)) ")")
            :opacity (when dim? 0.4)}
        [:polygon {:points (board/hex-pts-str (* rad 0.94))
                   :fill (if color (tint color) paper)
                   :stroke (case mark :target amber :ghost faint hexline)
                   :stroke-width (if (= mark :target) (* rad 0.28) (* rad 0.1))
                   :stroke-dasharray (when-not color
                                       (str (num-str (* rad 0.42)) " "
                                            (num-str (* rad 0.34))))}]
        (when color
          [:circle {:cx 0 :cy 0 :r (* rad 0.33) :fill (board/pwo color)
                    :stroke (board/pwi color) :stroke-width (* rad 0.06)}])
        (when ark
          [:g {:transform (str "scale(" (num-str (* s 1.45)) ")")}
           (board/ark-ring) (board/ark-heading-arrow ark)])
        (when heading
          [:g {:transform (str "scale(" (num-str (* s 1.3)) ")")}
           (board/heading-marker heading)])
        (when sundiver
          [:g {:transform (str "translate(" (num-str (* rad 0.36)) ","
                               (num-str (* rad 0.2)) ")")}
           (sundiver-glyph sundiver u)])
        (when beacon
          [:g {:transform (str "translate(" (num-str (* rad -0.32)) ","
                               (num-str (* rad -0.2)) ") scale("
                               (num-str (* rad 0.05)) ")")}
           (board/beacon-shape beacon)])])]))

;; ── MOVE ──────────────────────────────────────────────────────────────────────

(defn- panel-move
  [x0 w]
  (let [sc 0.28]
    [:g
     (column-head x0 54 "MOVE" move-mark moss)
     (lines x0 66
            ["SPEND MOVE POINTS: 2 + 1 FOR EVERY"
             "WORLD COLOUR YOU HOLD A STATION ON"]
            {:size 5.4 :lead 7 :fill faint})

     ;; ── LAUNCH ────────────────────────────────────────────────────────────
     (banner x0 84 w "LAUNCH")
     (let [fy    146
           cx    (+ x0 52)
           hx    (+ x0 10)
           tiles [[dn-l  {:mark :hatch}]
                  [dn    {:mark :hatch}]
                  [up-l  {:mark :hatch}]
                  [up    {:color :silver :mark :target}]
                  [dn-r  {:color :sun    :mark :target}]
                  [up-r  {:color :green  :mark :target :heading up-r}]
                  [[0 0] {:color :void :mark :target :ark up-r}]]]
       [:g
        (cluster cx fy sc tiles)
        (habitat hx fy 9 3)
        (caps hx (+ fy 20) "HABITAT" {:size 4.4 :anchor "middle"})
        (for [[d bow] [[[0 0] 2] [up -13] [up-r -21] [dn-r 15]]
              :let [[tx ty] (hex-at cx fy sc tiles d)]]
          (arrow [(+ hx 10) (- fy 1)] [tx ty] {:c "you" :width 1 :bow bow}))
        (lines (+ x0 104) 122
               ["one point each: a"
                "sundiver from your"
                "habitat onto the Ark's"
                "own space, or the"
                "three spaces ahead"
                "of it. the three"
                "behind are shut."]
               {:size 5.8 :lead 7.4})])

     ;; ── FLY ───────────────────────────────────────────────────────────────
     (banner x0 186 w "FLY")
     (let [fy    240
           cx    (+ x0 52)
           tiles [[up-l  {:color :silver}]
                  [up    {:color :silver}]
                  [dn-l  {:color :silver}]
                  [dn    {:color :sun}]
                  [[0 0] {:color :silver :sundivers [[you 1]]}]]
           [ox oy] (hex-at cx fy sc tiles [0 0])]
       [:g
        (cluster cx fy sc tiles)
        (for [d [up-l up dn-l]
              :let [[tx ty] (hex-at cx fy sc tiles d)
                    dx (- tx ox) dy (- ty oy)
                    len (Math/sqrt (+ (* dx dx) (* dy dy)))]]
          (arrow [(+ ox (* dx (/ 9.0 len))) (+ oy (* dy (/ 9.0 len)))] [tx ty]
                 {:c "you" :width 1.4}))
        (lines (+ x0 104) 222
               ["one point a hop, onto"
                "any world of the same"
                "colour. keep hopping"
                "while the points last."
                "a different colour"
                "needs a gate — below."]
               {:size 5.8 :lead 7.4})])

     ;; ── GATE ──────────────────────────────────────────────────────────────
     (banner x0 284 w "GATE")
     (let [gsc  0.3
           fy   330
           bx   (+ x0 40)
           ax   (+ x0 136)
           tiles-before [[dn-r  {:color :green}]
                         [[0 0] {:color :silver :sundivers [[you 1]]}]]
           tiles-after  [[dn-r  {:color :green}]
                         [[0 0] {:color :silver :gates [[dn-r you]]}]]
           [ox oy] (hex-at bx fy gsc tiles-before [0 0])
           [tx ty] (hex-at bx fy gsc tiles-before dn-r)]
       [:g
        (cluster bx fy gsc tiles-before)
        (arrow [(+ ox 5) (+ oy 4)] [tx ty] {:c "flare" :width 1.2})
        (no-mark (/ (+ ox tx) 2.0) (/ (+ oy ty) 2.0) 5)
        [:g {:transform (str "translate(" (+ x0 90) "," fy ")")} (atom-mark cyan)]
        (arrow [(+ x0 98) fy] [(- ax 26) fy] {:c "cyan" :width 0.9 :dash "1.6 2"})
        (cluster ax fy gsc tiles-after)])
     (lines x0 362
            ["a different colour blocks the hop. spend the point"
             "and the sundiver itself instead: it becomes a GATE"
             "on that edge, joining the two worlds both ways for"
             "anyone, and draws you a card. crossing a gate later"
             "costs a point too, and hands its owner a sundiver"
             "— once a turn."]
            {:size 5.8 :lead 7.4})

     ;; ── EXPLORE ───────────────────────────────────────────────────────────
     (banner x0 408 w "EXPLORE")
     (let [fy 442
           esc 0.22
           tiles [[up-r  {:mark :ghost}]
                  [[0 0] {:color :silver :sundivers [[you 1]]}]]
           [ox oy] (hex-at (+ x0 24) fy esc tiles [0 0])
           [tx ty] (hex-at (+ x0 24) fy esc tiles up-r)]
       [:g
        (cluster (+ x0 24) fy esc tiles)
        (arrow [(+ ox 4) (- oy 3)] [tx ty] {:c "you" :width 1.2})
        (bag (+ x0 58) (- fy 4) 0.3)
        (arrow [(+ x0 66) (- fy 14)] [(+ x0 84) (- fy 12)] {:c "bag" :width 1 :bow -4})
        (cluster (+ x0 92) fy esc
                 [[up-r  {:color :sun :sundivers [[you 1]]}]
                  [[0 0] {:color :silver}]])
        (no-mark (+ x0 105) (- fy 15) 4)
        (lines (+ x0 122) 426
               ["empty space draws"
                "a world from the bag"
                (str "— " game/num-worlds-per-color " worlds, "
                     (count game/tile-colors) " colours.")
                "the sundiver lands"
                "there and stops."]
               {:size 5.8 :lead 7.4})])]))

;; ── CONVERT ───────────────────────────────────────────────────────────────────

(defn- convert-row
  "SOL's grammar: the sundivers that build it, dotted paths into the conversion,
   then the same spaces with the station standing on them."
  [x0 w y label sc fig-h before after dirs caption]
  (let [fy (+ y 18 (/ fig-h 2.0))
        bx (+ x0 (* w 0.23))
        mx (+ x0 (* w 0.50))
        ax (+ x0 (* w 0.78))]
    [:g
     (banner x0 y w label)
     (cluster bx fy sc before)
     (cluster ax fy sc after)
     (for [d dirs
           :let [[sx sy] (hex-at bx fy sc before d)
                 bow     (if (< sy fy) -10 10)]]
       (arrow [sx sy] [(- mx 8) fy]
              {:c "cyan" :width 0.9 :dash "1.6 2" :bow bow :head false}))
     [:g {:transform (str "translate(" mx "," fy ")")} (atom-mark cyan)]
     (arrow [(+ mx 8) fy]
            (let [[tx ty] (hex-at ax fy sc after [0 0])] [(- tx 9) ty])
            {:c "cyan" :width 0.9 :dash "1.6 2" :bow -6})
     (lines x0 (+ y 30 fig-h) caption {:size 5.6 :lead 7.2})]))

(defn- panel-convert
  [x0 w]
  (let [sc 0.3
        pattern (fn [dirs t after?]
                  (concat
                   (for [d dirs]
                     [d (if after?
                          {:color :purple}
                          {:color :purple :sundivers [[you 1]]
                           :sd-angle (facing-angle d)})])
                   [[[0 0] (if after?
                             {:color :silver :station {:type t :level 1}}
                             {:color :silver :mark :target})]]))
        row (fn [y label fig-h dirs t caption]
              (convert-row x0 w y label sc fig-h
                           (pattern dirs t false) (pattern dirs t true)
                           dirs caption))]
    [:g
     (column-head x0 54 "CONVERT" atom-mark cyan)
     (lines x0 66
            ["ONE ACTION. THE SUNDIVERS GO BACK"
             "TO YOUR RESERVE, THE STATION TAKES OVER"]
            {:size 5.4 :lead 7 :fill faint})

     (row 84  "FOUNDRY" 44 [dn-r dn-l] :foundry ["two sundivers, 120° apart"])
     (row 178 "MATRIX"  52 [dn-r up-l] :matrix  ["two sundivers, facing"])
     (row 280 "TOWER"   64 [dn-r up dn-l] :tower ["three sundivers, evenly spaced"])

     (banner x0 396 w "LEVEL")
     (at (+ x0 12) 422 1.0 (level-bars 3 you 11))
     (lines (+ x0 26) 420
            ["a station takes its region's level. a"
             "fresh region is one deeper than the"
             "deepest a gate leads out to, up to 3."]
            {:size 5.6 :lead 7.2})
     (lines x0 446
            ["draw that many cards. the new station"
             "activates once, immediately, for free."]
            {:size 5.6 :lead 7.2})]))

;; ── ACTIVATE ──────────────────────────────────────────────────────────────────

(defn- panel-activate
  [x0 w]
  (let [cw  (/ (- w 14) 2.0)
        bx2 (+ x0 cw 14)]
    [:g
     (column-head x0 54 "ACTIVATE" bolt-mark amber)
     (lines x0 66
            ["ONE STATION TYPE PER TURN · EVERY STATION OF THAT TYPE YOU HAVE A"
             "SUNDIVER ON, EACH ONE ONCE · THAT SUNDIVER THEN COMES HOME"]
            {:size 5.4 :lead 7 :fill faint})

     ;; the three types, in proportion, and the sundiver coming home
     (for [[i [t label]] (map-indexed vector [[:foundry "FOUNDRY"] [:matrix "MATRIX"]
                                              [:tower "TOWER"]])
           :let [x (+ x0 26 (* i 50))]]
       [:g
        (at x 128 1.0 (piece t icon-space))
        (caps x 139 label {:fill ink :size 5.2 :anchor "middle"})])
     (cluster (+ x0 192) 116 0.24
              [[[0 0] {:color :silver :station {:type :tower :level 2}
                       :sundivers [[you 1]]}]])
     (arrow [(+ x0 210) 122] [(+ x0 228) 122] {:c "you" :width 1})
     (habitat (+ x0 246) 122 11 3)
     (caps (+ x0 222) 139 "SUNDIVER COMES HOME" {:size 4.8 :anchor "middle"})

     ;; ACTIONS PER STATION — the level is the platforms themselves
     (banner x0 148 w "ACTIONS PER STATION")
     (let [lx  (+ x0 26) ccw (/ (- w 26) 5.0) y0 164 rh 15
           col (fn [i] (+ lx (* i ccw)))]
       [:g
        (caps (+ x0 22) (+ y0 10) "LEVEL" {:anchor "end" :size 5 :fill dim})
        (caps (+ x0 22) (+ y0 rh 11) "BASE" {:anchor "end" :size 5 :fill dim})
        (caps (+ x0 22) (+ y0 (* 2 rh) 11) "BONUS" {:anchor "end" :size 5 :fill dim})
        (for [i [0 1 2 3]
              :let [{:keys [base bonus]} (game/station-action-counts i)]]
          [:g
           [:rect {:x (col i) :y y0 :width ccw :height (+ (* 3 rh) 1)
                   :fill "none" :stroke hexline :stroke-width 0.6}]
           (if (pos? i)
             (at (+ (col i) (/ ccw 2.0)) (+ y0 3) 1.0 (level-bars i you 11))
             (txt (+ (col i) (/ ccw 2.0)) (+ y0 11) "—"
                  {:size 8 :family sans :anchor "middle" :fill faint}))
           (txt (+ (col i) (/ ccw 2.0)) (+ y0 rh 12) (str base)
                {:size 9.5 :family sans :anchor "middle"})
           (txt (+ (col i) (/ ccw 2.0)) (+ y0 (* 2 rh) 12) (str bonus)
                {:size 9.5 :family sans :anchor "middle"
                 :fill (if (zero? bonus) faint ink)})])
        ;; the sketch's fifth depth, which the code never reaches
        [:g {:opacity 0.5}
         (dashed-box (col 4) y0 ccw (+ (* 3 rh) 1))
         (at (+ (col 4) (/ ccw 2.0)) (+ y0 3) 1.0 (level-bars 4 you 11))
         (txt (+ (col 4) (/ ccw 2.0)) (+ y0 rh 12) "5"
              {:size 9.5 :family sans :anchor "middle" :fill dim})
         (txt (+ (col 4) (/ ccw 2.0)) (+ y0 (* 2 rh) 12) "3"
              {:size 9.5 :family sans :anchor "middle" :fill dim})
         (sketch-tag (+ (col 4) (/ ccw 2.0)) (+ y0 (* 3 rh) 11) {:anchor "middle"})]])
     (lines x0 226
            ["BONUS — on your own station, take it or leave it. on someone else's"
             "the owner decides first; you get it only if they pass."]
            {:size 5.6 :lead 7.2})

     ;; ── FOUNDRY · BUILD ───────────────────────────────────────────────────
     (banner x0 246 cw "FOUNDRY · BUILD")
     (let [hy 292]
       [:g
        (at (+ x0 16) (+ hy 8) 1.0 (piece :foundry (* icon-space 0.9)))
        (reserve (+ x0 56) hy 36 16)
        (at (+ x0 48) hy 1.0 (sundiver-glyph you 22))
        (at (+ x0 64) hy 1.0 (sundiver-glyph you 22))
        (caps (+ x0 56) (+ hy 16) "RESERVE" {:size 4.4 :anchor "middle"})
        (arrow [(+ x0 78) (- hy 4)] [(+ x0 104) (- hy 6)] {:c "you" :width 1.4 :bow -4})
        (habitat (+ x0 122) hy 12 2)
        (caps (+ x0 122) (+ hy 19) "HABITAT" {:size 4.4 :anchor "middle"})])
     (lines x0 336
            ["each action moves up to two sundivers from"
             "your reserve into your habitat."]
            {:size 5.6 :lead 7.2})

     ;; ── MATRIX · PLANT ────────────────────────────────────────────────────
     (banner x0 366 cw "MATRIX · PLANT")
     (at (+ x0 12) 412 1.0 (piece :matrix (* icon-space 0.85)))
     (mini-board (+ x0 86) 408 8.4
                 ;; the Ark heads up-right: its own space and the three behind
                 ;; it are open, and so is any world holding your sundiver —
                 ;; except where a beacon already stands, one to a space
                 [[[0 -2]  {:color :void :dim? true}]
                  [[1 -1]  {:color :green :dim? true}]
                  [[1 -2]  {:color :sun :heading up-r :dim? true}]
                  [[-2 0]  {:color :purple :dim? true}]
                  [[-2 1]  {:color :sun :dim? true}]
                  [[2 -2]  {:color :void :dim? true}]
                  [[-1 1]  {:color :blue :dim? true}]
                  [[0 -1]  {:color :silver :mark :target :ark up-r}]
                  [[-1 0]  {:color :purple :beacon you}]
                  [[-1 -1] {:color :green :mark :target}]
                  [[0 0]   {:color :blue :mark :target}]
                  [[2 -1]  {:color :silver :sundiver you :beacon you}]
                  [[1 0]   {:color :purple :mark :target :sundiver you}]])
     (cost (+ x0 20) 438 {:unit 22})
     (lines x0 452
            ["one beacon per action, for one sundiver:"
             "on the Ark's own space, any of the three"
             "spaces behind it, or any world where you"
             "have a sundiver."]
            {:size 5.6 :lead 7.2})

     ;; ── TOWER · STEER & PROPEL ────────────────────────────────────────────
     (banner bx2 246 cw "TOWER · STEER & PROPEL")
     (at (+ bx2 12) 292 1.0 (piece :tower (* icon-space 0.62)))
     (at (+ bx2 30) 274 0.7 (board/captain-flame))
     (caps (+ bx2 18) 304 "TAKE FLAME" {:size 4.4 :anchor "middle" :fill amber})
     (let [tiles [[up    {:mark :target}]
                  [up-r  {:mark :target}]
                  [dn-r  {:mark :target}]
                  [[0 0] {:color :void :ark up-r}]]
           tsc   0.22
           tcx   (+ bx2 66)
           tcy   276]
       [:g
        (cluster tcx tcy tsc tiles)
        ;; three fat arrows: straight, one step left, one step right
        (for [d [up up-r dn-r]
              :let [[tx ty] (hex-at tcx tcy tsc tiles d)
                    [ox oy] (hex-at tcx tcy tsc tiles [0 0])
                    dx (- tx ox) dy (- ty oy)
                    len (Math/sqrt (+ (* dx dx) (* dy dy)))]]
          (arrow [(+ ox (* dx (/ 8.0 len))) (+ oy (* dy (/ 8.0 len)))] [tx ty]
                 {:c "amber" :width 2.4 :fat true}))
        (caps tcx 304 "STEER ±1" {:size 4.4 :anchor "middle"})])
     (cost (+ bx2 100) 278 {:unit 21})
     (let [tiles [[dn-l  {:color :void :world? false}]
                  [up-r  {:color :green :beacon other :heading up-r}]
                  [[0 0] {:color :silver :ark up-r}]]
           psc   0.22
           pcx   (+ bx2 128)
           pcy   276]
       [:g
        (cluster pcx pcy psc tiles)
        (arrow [(- pcx 15) (+ pcy 13)] [(+ pcx 14) (- pcy 11)]
               {:c "amber" :width 2.2 :fat true})
        (caps pcx 304 "PROPEL · DISCOVER" {:size 4.4 :anchor "middle"})])
     (lines bx2 318
            ["each action steers the heading"
             "straight, one step left or one step"
             "right for one sundiver, then the Ark"
             "advances onto the heading token —"
             "it may WRAP. any beacon it reaches"
             "is discovered."]
            {:size 5.6 :lead 7.2})

     ;; ── LAND ──────────────────────────────────────────────────────────────
     (banner bx2 366 cw "LAND")
     (let [tiles [[up-l {:color :void}]
                  [dn-l {:color :green}]
                  [dn   {:color :purple}]
                  [up   {:color :blue}]
                  [up-r {:color :sun}]
                  [dn-r {:color :silver :ark up-l}]   ; the Ark waits outside
                  [[0 0] {:color :silver :mark :target
                          :arcs [[up-l :void] [dn-l :green] [dn :purple]
                                 [up :blue] [up-r :sun]]}]]
           lsc 0.24
           lcx (+ bx2 40)
           lcy 414]
       [:g
        (cluster lcx lcy lsc tiles)
        (let [[ax ay] (hex-at lcx lcy lsc tiles dn-r)
              [tx ty] (hex-at lcx lcy lsc tiles [0 0])
              dx (- tx ax) dy (- ty ay)
              len (Math/sqrt (+ (* dx dx) (* dy dy)))]
          (arrow [(+ ax (* dx (/ 7.0 len))) (+ ay (* dy (/ 7.0 len)))] [tx ty]
                 {:c "amber" :width 2.4 :fat true}))
        (caps lcx 446 "5 OF 6 MATCH" {:size 4.6 :anchor "middle" :fill amber})])
     (lines (+ bx2 76) 392
            ["only during a tower"
             "activation. the Ark may"
             "land when the world's"
             "colour is in the cipher"
             "centre and at least 5 of"
             "its 6 neighbours match"
             "their cipher slots."]
            {:size 5.6 :lead 7.2})
     (lines bx2 452
            ["5 matches: the Ark must be on it. 6: on it"
             "or beside it. landing ends the game — score"
             "every beacon you hold on a matching slot."]
            {:size 5.6 :lead 7.2})]))

;; ── WRAP, along the foot ──────────────────────────────────────────────────────

(defn- panel-wrap
  [x0 y w]
  (let [rad 2.7
        ;; known space runs a long way down-left of the mover, and only a
        ;; little straight down.  Nothing lies north of it, so both wraps come
        ;; out below and neither arc has to cross the board or the other.
        band  (map-indexed (fn [i c] [[(- (inc i)) (inc i)] {:color c}])
                           [:purple :green :sun :silver :blue :void
                            :green :purple :sun :silver])
        body  [[[-1 0]  {:color :green}]   [[-3 2] {:color :sun}]
               [[-5 3]  {:color :blue}]    [[-7 4] {:color :purple}]
               [[-2 2]  {:color :silver}]  [[-4 4] {:color :green}]
               [[-6 5]  {:color :void}]    [[-8 6] {:color :blue}]
               [[0 1]   {:color :sun}]     [[0 2]  {:color :green}]]
        tiles (concat band body
                      [[[1 -1]  {:mark :ghost}]     ; north-east: empty
                       [[0 -1]  {:mark :ghost}]     ; north: empty
                       [[0 9]   {:mark :target}]    ; the minimum, unexplored
                       [[0 0]   {:color :silver :sundiver you :mark :target}]])
        [ox oy] (board/board-centroid tiles)
        sc  (/ rad 50.0)
        bx  (+ x0 128) by (+ y 40)
        at* (fn [pos] (let [[px py] (board/hex->pixel pos)]
                        [(+ bx (* (- px ox) sc)) (+ by (* (- py oy) sc))]))]
    [:g
     (banner x0 y w "WRAP")
     (mini-board bx by rad tiles)
     (let [[mx my] (at* [0 0])
           [ax ay] (at* [-1 1])
           [fx fy] (at* [-10 10])
           [nx ny] (at* [0 9])]
       [:g
        ;; a plain hop to the next world along
        (arrow [mx my] [ax ay] {:c "you" :width 1.3})
        ;; north-east is empty and nothing lies beyond, so it comes out at the
        ;; far end of that axis — both wraps swing wide of the board
        (arrow [mx my] [fx fy] {:c "amber" :width 1.1 :dash "2.5 2" :bow -13})
        ;; north too, and that axis runs out, so the minimum applies
        (arrow [mx my] [nx ny] {:c "amber" :width 1.1 :dash "2.5 2" :bow 11})
        (caps (- fx 2) (+ fy 9) "10 AWAY" {:size 4.2 :fill amber :anchor "middle"})
        (caps (+ nx 8) (+ ny 3) "MIN 9" {:size 4.2 :fill amber})
        (caps (+ mx 9) (- my 5) "HOP" {:size 4.2 :fill (markers "you")})])
     (lines x0 (+ y 22)
            ["at the edge of known"
             "space you may come"
             "out the far side"
             "instead: the far end"
             "of the same axis"
             "behind you, or nine"
             "spaces back, whichever"
             "is further — even into"
             "the unknown."]
            {:size 5.6 :lead 7.2})]))

(defn- panel-cipher
  [x0 y w]
  [:g
   (banner x0 y w "CIPHER")
   (cipher-rosette (+ x0 30) (+ y 44) 0.27
                   {[0 0] [:sun] up [:green] up-r [:blue] dn-r [:silver]})
   (caps (+ x0 30) (+ y 70) "SEVEN SLOTS" {:size 4.4 :anchor "middle"})
   (at (+ x0 62) (+ y 22) 1.0 (sundiver-glyph you 17))
   (arrow [(+ x0 56) (+ y 30)] [(+ x0 44) (+ y 36)] {:c "you" :width 1 :bow 5})
   (lines (+ x0 70) (+ y 26)
          ["last of all, every beacon the Ark"
           "discovered this turn is placed: its"
           "world's colour goes into one of the"
           "seven slots — your choice — with"
           "your beacon on it. free if that"
           "colour is already there, else one"
           "sundiver per colour that is."]
          {:size 5.6 :lead 7.2})])

(defn- panel-draw
  [x0 y w]
  [:g
   (banner x0 y w "DRAW")
   (at (+ x0 6) (+ y 22) 1.0 (level-bars 3 you 11))
   (txt (+ x0 16) (+ y 31) "→" {:size 8 :fill dim :family sans})
   (card (+ x0 32) (+ y 27) {:suit 2})
   (card (+ x0 45) (+ y 27) {:suit 0})
   (at (+ x0 8) (+ y 50) 1.0 (board/suit-icon 4 flare-c 6))
   (arrow [(+ x0 17) (+ y 50)] [(+ x0 31) (+ y 50)] {:c "flare" :width 1})
   (caps (+ x0 35) (+ y 52) "PROPEL" {:size 5.6 :fill flare-c})
   (at (+ x0 8) (+ y 68) 0.85 (board/captain-flame))
   (caps (+ x0 17) (+ y 69) "CAPTAIN DRIFTS" {:size 5.6 :fill ink})
   (lines (+ x0 78) (+ y 22)
          ["cards equal to the levels of the stations you"
           "activated, or one per gate you built. keep a"
           "single card, discard the rest."
           "every flare advances the Ark one space and may"
           (str "DISCOVER. the " game/cards-per-suit "th flare, or an empty bag:")
           "everyone loses."
           "the captain then steers, propels and draws one"
           "more card — a flare propels again."]
          {:size 5.6 :lead 7.2})])

(defn pieces-sheet
  "The piece symbols on their own: <symbol> defs to <use>, over a labelled row.
   They stand in scale with each other, as they do on the table."
  []
  (let [items [[:sundiver "SUNDIVER"] [:gate "GATE"] [:foundry "FOUNDRY"]
               [:matrix "MATRIX"] [:tower "TOWER"]]
        cell  116
        w     (* cell (count items))
        h     230
        u     70
        base  180]
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :width w :height h :viewBox (str "0 0 " w " " h)}
     [:defs
      (for [[t _] items
            :let [pw (pieces/width t 1.0) ph (pieces/height t 1.0)]]
        [:symbol {:id (str "journey-piece-" (name t))
                  :viewBox (str (num-str (/ pw -2.0)) " " (num-str (- ph)) " "
                                (num-str pw) " " (num-str ph))}
         (pieces/piece t 1.0 {:fill "currentColor"})])]
     [:rect {:x 0 :y 0 :width w :height h :fill paper}]
     (txt 12 20 "JOURNEY PIECES — SOL's own symbols, one silhouette + one contour path each"
          {:size 8 :family sans :weight "600" :spacing 0.6})
     (txt 12 32 "journey.pieces · sized against the height of one board space · base on y=0 · centred on x=0"
          {:size 6.4 :fill dim})
     [:line {:x1 12 :y1 base :x2 (- w 12) :y2 base :stroke "#E2E2E8" :stroke-width 0.8}]
     (for [[i [t label]] (map-indexed vector items)
           :let [cx (+ (* i cell) (/ cell 2.0))]]
       [:g
        (at cx base 1.0 (piece t u))
        (at (- cx 30) base 1.0 (piece t (* u 0.34)))
        (caps cx 196 label {:size 6.4 :fill ink :anchor "middle"})
        (caps cx 206 (str (get-in pieces/pieces [t :height]) " × a space")
              {:size 5.4 :fill faint :anchor "middle"})])]))

;; ── the sheet ─────────────────────────────────────────────────────────────────

(def ^:private sheet-w 720)     ; 10in, 72 units to the inch
(def ^:private sheet-h 576)     ; 8in

(defn- defs
  []
  [:defs
   [:pattern {:id "hatch" :width 7 :height 7 :patternUnits "userSpaceOnUse"
              :patternTransform "rotate(45)"}
    [:line {:x1 0 :y1 0 :x2 0 :y2 7 :stroke "#D6D6DC" :stroke-width 3}]]
   (for [[id c] markers]
     [:marker {:id (str "ah-" id) :viewBox "0 0 10 10"
               :refX 8.5 :refY 5 :markerWidth 4.5 :markerHeight 4.5 :orient "auto"}
      [:path {:d "M 0,0 L 10,5 L 0,10 z" :fill c}]])
   ;; heads for the fat arrows, sized down so they stay in proportion
   (for [[id c] markers]
     [:marker {:id (str "ah-fat-" id) :viewBox "0 0 10 10"
               :refX 8.2 :refY 5 :markerWidth 2.3 :markerHeight 2.3 :orient "auto"}
      [:path {:d "M 0,0 L 10,5 L 0,10 z" :fill c}]])])

(defn sheet
  []
  (let [m  18
        mv 200   ; MOVE
        cv 170   ; CONVERT
        ac 294   ; ACTIVATE — the big one
        g  10
        x1 m
        x2 (+ x1 mv g)
        x3 (+ x2 cv g)]
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :width "10in" :height "8in"
           :viewBox (str "0 0 " sheet-w " " sheet-h)}
     (defs)
     [:rect {:x 0 :y 0 :width sheet-w :height sheet-h :fill paper}]
     ;; header
     (txt (/ sheet-w 2.0) 30 "CHOOSE ONE ACTION"
          {:size 12 :family sans :weight "600" :spacing 4.5 :anchor "middle"})
     [:line {:x1 m :y1 26 :x2 264 :y2 26 :stroke ink :stroke-width 1}]
     [:line {:x1 456 :y1 26 :x2 (- sheet-w m) :y2 26 :stroke ink :stroke-width 1}]
     ;; columns
     (panel-move x1 mv)
     (panel-convert x2 cv)
     (panel-activate x3 ac)
     [:line {:x1 (- x2 (/ g 2.0)) :y1 44 :x2 (- x2 (/ g 2.0)) :y2 470
             :stroke "#E2E2E8" :stroke-width 0.8}]
     [:line {:x1 (- x3 (/ g 2.0)) :y1 44 :x2 (- x3 (/ g 2.0)) :y2 470
             :stroke "#E2E2E8" :stroke-width 0.8}]
     ;; the foot: wrapping, then everything that follows the action
     (panel-wrap m 478 206)
     (panel-cipher 234 478 206)
     (panel-draw 450 478 (- sheet-w 450 m))
     [:line {:x1 226 :y1 478 :x2 226 :y2 558 :stroke "#E2E2E8" :stroke-width 0.8}]
     [:line {:x1 442 :y1 478 :x2 442 :y2 558 :stroke "#E2E2E8" :stroke-width 0.8}]
     (txt m 566 "JOURNEY · one action a turn, then draw · after the v18 diagram"
          {:size 5.4 :fill faint :spacing 0.8})
     (txt (- sheet-w m) 566
          (str "faded and dashed = in the sketch, not in the code: "
               "station level 4 · steering by card suit (CW / CCW)")
          {:size 5.4 :fill faint :anchor "end"})]))

(defn -main
  [& args]
  (let [pieces? (= "--pieces" (first args))
        args    (if pieces? (rest args) args)
        out     (or (first args)
                    (if pieces?
                      "resources/public/img/journey-pieces.svg"
                      "resources/public/img/journey-diagram.svg"))]
    ;; Swap the board's screen palette for paper inks while the sheet renders,
    ;; so every board shape below still comes straight from journey.board.
    (with-redefs [board/world-outer    print-world-outer
                  board/world-inner    print-world-inner
                  board/neutral-stroke "#4A4A56"
                  board/captain-fill   "#E8A81C"
                  board/captain-stroke "#7A5200"]
      (spit out (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                     (->svg (if pieces? (pieces-sheet) (sheet))) "\n")))
    (println "wrote" out)
    (System/exit 0)))
