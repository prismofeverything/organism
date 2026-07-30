(ns organism.scripts.journey-diagram
  "Render the JOURNEY action reference sheet to SVG, 10×8in landscape, dark on white.

   Content follows the hand-drawn journey-diagram sketch (v18); the layout and
   visual grammar follow SOL's CHOOSE AN ACTION sheet (banner bars, before →
   conversion → after), since JOURNEY is its sequel and uses SOL's pieces.

   Board pieces come from journey.board — the same primitives the live board
   renders with, recoloured for paper.  The station and gate silhouettes are
   traced from SOL's own piece models (sol-3d-{gate,foundry,node,tower}.stl,
   front elevation, projected and simplified).  Numbers are read out of
   journey.game so the sheet cannot drift from the implementation.

     lein run -m organism.scripts.journey-diagram [out.svg]

   Default output: resources/public/img/journey-diagram.svg

   Anything the sketch shows that the code does not implement is drawn faded,
   dashed and tagged SKETCH ONLY rather than silently dropped or quietly
   promoted into the rules."
  (:require
   [clojure.string :as string]
   [journey.board :as board]
   [journey.game :as game]))

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

;; ── SOL piece silhouettes ─────────────────────────────────────────────────────
;; Front elevations traced from SOL's own piece models, normalised to 22 units
;; tall with the base at y=0 (the piece rises into negative y).

(def ^:private sol-piece
  {:gate    (str "M -7.6,-3.5 L -5.3,-7.6 L -2.9,-17.5 L 0,-22 L 3.5,-15.9 "
                 "L 5.3,-7.6 L 7.6,-3.5 L 7.5,0 L -7.5,0 L -7.6,-3.5 Z")
   :foundry (str "M -10.8,0 L -10.4,-14.2 L -9.5,-17.2 L -7.9,-19.4 L -4.1,-20.3 "
                 "L -1.8,-21.8 L 0.6,-22 L 4.1,-20.3 L 7.2,-19.9 L 8.5,-18.8 "
                 "L 10.6,-13.1 L 10.8,0 L 5,0 L 4.8,-2.7 L 1.7,-1.6 L 1.2,0 "
                 "L -1.3,0 L -1.7,-1.6 L -4.8,-2.7 L -5,0 L -10.8,0 Z")
   :matrix  (str "M -12.2,-0.6 L -9.8,-11.1 L -5.1,-20.9 L -4.1,-22 L -3.7,-20.7 "
                 "L -2.5,-22 L -1.4,-20.7 L 0,-22 L 1.5,-20.7 L 2.5,-22 L 3.8,-20.7 "
                 "L 4.1,-22 L 6.7,-18.3 L 9.3,-12.7 L 11.7,-5 L 12.2,-0.4 L 11.1,0 "
                 "L 8.6,-2.2 L 8.1,-0.1 L 6.2,0 L 4.6,-2.6 L 2.8,-3.6 L 1.1,0 "
                 "L -1.3,-0.1 L -2.8,-3.6 L -4.6,-2.6 L -6.1,0 L -8,-0.1 L -8.6,-2.2 "
                 "L -11,0 L -12.2,-0.6 Z")
   :tower   (str "M -6,0 L -5.5,-3.8 L -4.4,-7.4 L -2.5,-10.3 L -2,-10.3 L -2.2,-8 "
                 "L -1.2,-11 L -1.1,-13.1 L -1.5,-15.2 L -1.8,-16 L -2,-15.3 "
                 "L -2.8,-15.3 L -3.1,-16.5 L -3.2,-18.4 L -2.8,-20.1 L -2,-21.6 "
                 "L -1.4,-21.3 L 0,-22 L 1.4,-21.3 L 2,-21.6 L 3,-19.6 L 3.2,-17.1 "
                 "L 2.8,-15.3 L 2,-15.3 L 1.8,-16 L 1.4,-15 L 1,-12.9 L 1.2,-10.8 "
                 "L 2.2,-8 L 2,-10.3 L 2.5,-10.3 L 4.4,-7.4 L 5.5,-3.8 L 6,0 L -6,0 Z")})

(defn- piece
  "A SOL piece silhouette h units tall, base at the origin."
  [t ck h & [{:keys [stroke-w]}]]
  (let [s (/ h 22.0)]
    [:g {:transform (str "scale(" (num-str s) ")")}
     [:path {:d (sol-piece t)
             :fill (board/pwo ck) :stroke (board/pwi ck)
             :stroke-width (num-str (/ (or stroke-w 1.6) s))}]]))

(defn- level-bars
  "JOURNEY's level platforms, stacked under a piece's base."
  [n ck h]
  (let [bw (* h 0.8) bh (* h 0.1) gap (* h 0.17)]
    (for [i (range n)]
      [:rect {:x (- (/ bw 2.0)) :y (+ (* h 0.05) (* i gap)) :width bw :height bh
              :rx (* h 0.02)
              :fill (board/pwo ck) :stroke (board/pwi ck) :stroke-width (* h 0.035)}])))

(defn- station-icon
  "A station: SOL's piece over its level platforms, centred on [0 0]."
  [t level ck h]
  [:g {:transform (str "translate(0," (num-str (* h (if (pos? level) 0.4 0.5))) ")")}
   (piece t ck h)
   (level-bars level ck h)])

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
  [[x1 y1] [x2 y2] & [{:keys [c width bow dash head]}]]
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
            :marker-end (when (not= head false) (str "url(#ah-" c ")"))}]))

(defn- no-mark
  "Circle-slash: goes no further."
  [x y r]
  [:g {:transform (str "translate(" x "," y ")")}
   [:circle {:cx 0 :cy 0 :r r :fill paper :fill-opacity 0.85
             :stroke flare-c :stroke-width 1.3}]
   [:line {:x1 (* -0.68 r) :y1 (* 0.68 r) :x2 (* 0.68 r) :y2 (* -0.68 r)
           :stroke flare-c :stroke-width 1.3}]])

(defn- sundiver-glyph
  "A single sundiver, nose up."
  [ck s]
  [:g {:transform (str "scale(" s ")")}
   [:polygon {:points (board/tri-pts 0 0 13)
              :fill (board/pwo ck) :stroke (board/pwi ck) :stroke-width 1}]])

;; Sundivers fan around the world token as they do on the board, a size up:
;; at print scale the board's own 12px triangles disappear.
(def ^:private sd-arc-r 25)
(def ^:private sd-size  26)
(def ^:private sd-step  19)

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
  [sundivers angle]
  (let [groups (count sundivers)]
    [:g
     (for [[idx [ck n]] (map-indexed vector sundivers)
           :let [base (+ (or angle board/sundiver-ang-start)
                         (* idx (/ 360.0 (max 1 groups))))
                 half (* 0.5 (dec n) sd-step)]]
       (for [i (range n)
             :let [deg (+ base (- (* i sd-step) half))
                   rad (* deg (/ Math/PI 180))]]
         [:g {:transform (str "translate(" (num-str (* sd-arc-r (Math/cos rad))) ","
                              (num-str (* sd-arc-r (Math/sin rad)))
                              ") rotate(" (num-str (+ 90 deg)) ")")}
          [:polygon {:points (board/tri-pts 0 0 sd-size)
                     :fill (board/pwo ck) :stroke (board/pwi ck)
                     :stroke-width 1.6}]]))]))

(defn- cost
  "\"− sundiver\" cost tag."
  [x y & [{:keys [ck size]}]]
  (let [ck (or ck you)
        s  (or size 0.5)]
    [:g {:transform (str "translate(" x "," y ")")}
     (txt (* s -19) (* s 6) "−" {:size (* s 22) :fill (board/pwo ck) :family sans})
     (sundiver-glyph ck s)]))

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
  [{:keys [color world? beacon station sundivers sd-angle ark heading arcs gates mark]}]
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
        (station-icon (:type station) (:level station 0) (:ck station you) 30)])
     ;; SOL's gate piece stands on the boundary it joins
     (for [[dir ck] gates
           :let [[ex ey] (board/edge-offset dir)]]
       [:g {:transform (str "translate(" (num-str ex) "," (num-str (+ ey 14)) ")")}
        (piece :gate ck 30 {:stroke-w 2})])
     (when ark
       [:g (board/ark-ring) (board/ark-heading-arrow ark)])
     (when heading (board/heading-marker heading))
     (when (seq sundivers) (tile-sundivers sundivers sd-angle))]))

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
        (tile-hex spec)])]))

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
     (let [step (* r 0.44)
           x0   (* -0.5 (dec n) step)]
       (for [i (range n)]
         [:g {:transform (str "translate(" (num-str (+ x0 (* i step))) ",1.5)")}
          (sundiver-glyph ck (* r 0.032))]))]))

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

;; ── MOVE ──────────────────────────────────────────────────────────────────────

(defn- panel-move
  [x0 w]
  (let [sc 0.25]
    [:g
     (column-head x0 54 "MOVE" move-mark moss)
     (lines x0 66
            ["SPEND MOVE POINTS: 2 + 1 FOR EVERY"
             "WORLD COLOUR YOU HOLD A STATION ON"]
            {:size 5.4 :lead 7 :fill faint})

     ;; ── LAUNCH: from the habitat onto the Ark and the three spaces ahead ──
     (banner x0 84 w "LAUNCH")
     (let [fy    138
           cx    (+ x0 54)
           hx    (+ x0 10)
           tiles [[dn-l  {:mark :hatch}]
                  [dn    {:mark :hatch}]
                  [up-l  {:mark :hatch}]
                  [up    {:color :purple :mark :target}]
                  [dn-r  {:color :sun    :mark :target}]
                  [up-r  {:color :green  :mark :target :heading up-r}]
                  [[0 0] {:color :void :mark :target :ark up-r}]]]
       [:g
        (cluster cx fy sc tiles)
        (habitat hx fy 8.5 3)
        (caps hx (+ fy 19) "HABITAT" {:size 4.4 :anchor "middle"})
        (for [[d bow] [[[0 0] 2] [up -12] [up-r -19] [dn-r 14]]
              :let [[tx ty] (hex-at cx fy sc tiles d)]]
          (arrow [(+ hx 10) (- fy 1)] [(- tx 7) ty]
                 {:c "you" :width 0.9 :bow bow}))
        (lines (+ x0 96) 116
               ["one point each: a"
                "sundiver from your"
                "habitat onto the Ark's"
                "own space, or the"
                "three spaces ahead"
                "of it. the three"
                "behind are shut."]
               {:size 5.8 :lead 7.4})])

     ;; ── FLY ───────────────────────────────────────────────────────────────
     (banner x0 184 w "FLY")
     (let [fy    232
           cx    (+ x0 46)
           tiles [[up-l  {:color :purple :sundivers [[you 1]]
                          :sd-angle (facing-angle up-l)}]
                  [up-r  {:color :green}]
                  [dn-r  {:color :green}]
                  [[0 0] {:color :purple :sundivers [[you 1]] :sd-angle 165
                          :gates [[up-r you] [dn-r other]]}]]
           [ax ay] (hex-at cx fy sc tiles up-l)
           [gx gy] (hex-at cx fy sc tiles up-r)
           [dx dy] (hex-at cx fy sc tiles dn-r)
           [ox oy] (hex-at cx fy sc tiles [0 0])]
       [:g
        (cluster cx fy sc tiles)
        (arrow [(- ox 5) (- oy 5)] [(+ ax 5) (+ ay 4)] {:c "you" :width 1 :bow -4})
        (arrow [(+ ox 4) (- oy 6)] [(- gx 4) (+ gy 5)] {:c "amber" :width 1 :bow 4})
        (arrow [(+ ox 6) (+ oy 4)] [(- dx 4) (- dy 5)] {:c "amber" :width 1 :bow -4})
        (caps (+ gx 11) (- gy 4) "GATE" {:size 4.6 :fill amber})
        (caps (+ dx 11) (+ dy 9) "+SUNDIVER" {:size 4.6 :fill amber})])
     (lines x0 270
            ["one point per hop. same colour: hop on."
             "a new colour spends the point and the"
             "sundiver to build a GATE. crossing any"
             "gate costs a point too, and hands its"
             "owner a sundiver — once a turn."]
            {:size 5.8 :lead 7.6})

     ;; ── EXPLORE ───────────────────────────────────────────────────────────
     (banner x0 318 w "EXPLORE")
     (let [fy 356]
       [:g
        (cluster (+ x0 22) fy 0.21
                 [[up-r  {:mark :ghost}]
                  [[0 0] {:color :purple :sundivers [[you 1]]
                          :sd-angle (toward-angle up-r)}]])
        (bag (+ x0 52) (- fy 6) 0.32)
        (arrow [(+ x0 60) (- fy 15)] [(+ x0 78) (- fy 13)] {:c "bag" :width 1 :bow -4})
        (cluster (+ x0 86) fy 0.21
                 [[up-r  {:color :silver :sundivers [[you 1]]
                          :sd-angle (toward-angle up-r)}]
                  [[0 0] {:color :purple}]])
        (no-mark (+ x0 99) (- fy 16) 4)
        (lines (+ x0 112) 342
               ["empty space draws a"
                "world from the bag —"
                (str game/num-worlds-per-color " worlds, "
                     (count game/tile-colors) " colours.")
                "the sundiver lands"
                "there and stops."]
               {:size 5.8 :lead 7.4})])

     ;; ── WRAP ──────────────────────────────────────────────────────────────
     (banner x0 388 w "WRAP")
     (let [fy    428
           wsc   0.18
           wcx   (+ x0 48)
           tiles [[[0 0] {:color :green}]
                  [[1 0] {:color :purple}]
                  [[2 0] {:color :silver :sundivers [[you 1]]
                          :sd-angle (toward-angle dn-r)}]
                  [[3 0] {:mark :ghost}]]
           [fx fy2] (hex-at wcx fy wsc tiles [0 0])
           [sx sy]  (hex-at wcx fy wsc tiles [2 0])
           [gx gy]  (hex-at wcx fy wsc tiles [3 0])]
       [:g
        (cluster wcx fy wsc tiles)
        (arrow [(+ sx 4) (+ sy 4)] [(- gx 4) (- gy 3)] {:c "you" :width 1})
        (caps (+ gx 1) (+ gy 11) "UNKNOWN" {:size 4.2 :fill dim :anchor "middle"})
        (arrow [sx (- sy 9)] [(+ fx 1) (- fy2 8)]
               {:c "amber" :width 1 :dash "2.5 2" :bow 13})
        (lines (+ x0 96) 412
               ["at the edge of known"
                "space: fly on into the"
                "unknown, or come out"
                "at the far end of the"
                "same axis behind you"
                "— nine spaces at least"
                "(dashed)."]
               {:size 5.8 :lead 7.4})])]))

;; ── CONVERT ───────────────────────────────────────────────────────────────────

(defn- convert-row
  "SOL's grammar: the sundivers that build it, dotted paths into the conversion,
   then the same spaces with the station standing on them."
  [x0 w y label sc fig-h before after dirs caption]
  (let [fy (+ y 16 (/ fig-h 2.0))
        bx (+ x0 (* w 0.22))
        mx (+ x0 (* w 0.50))
        ax (+ x0 (* w 0.78))]
    [:g
     (banner x0 y w label)
     (cluster bx fy sc before)
     (cluster ax fy sc after)
     ;; each sundiver's dotted path into the conversion
     (for [d dirs
           :let [[sx sy] (hex-at bx fy sc before d)
                 bow     (if (< sy fy) -10 10)]]
       (arrow [sx sy] [(- mx 8) fy]
              {:c "cyan" :width 0.9 :dash "1.6 2" :bow bow :head false}))
     [:g {:transform (str "translate(" mx "," fy ")")} (atom-mark cyan)]
     ;; and out onto the space it becomes
     (arrow [(+ mx 8) fy]
            (let [[tx ty] (hex-at ax fy sc after [0 0])] [(- tx 9) ty])
            {:c "cyan" :width 0.9 :dash "1.6 2" :bow -6})
     (lines x0 (+ y 28 fig-h) caption {:size 5.6 :lead 7.2})]))

(defn- panel-convert
  [x0 w]
  (let [sc 0.26
        pattern (fn [dirs t after?]
                  (concat
                   (for [d dirs]
                     [d (if after?
                          {:color :purple}
                          {:color :purple :sundivers [[you 1]]
                           :sd-angle (facing-angle d)})])
                   [[[0 0] (if after?
                             {:color :purple :station {:type t :level 1}}
                             {:color :purple :mark :target})]]))
        row (fn [y label fig-h dirs t caption]
              (convert-row x0 w y label sc fig-h
                           (pattern dirs t false) (pattern dirs t true)
                           dirs caption))]
    [:g
     (column-head x0 54 "CONVERT" atom-mark cyan)
     (lines x0 66
            ["ONE ACTION. THE SUNDIVERS GO BACK TO"
             "YOUR RESERVE, THE STATION TAKES OVER"]
            {:size 5.4 :lead 7 :fill faint})

     ;; GATE — the sundiver itself becomes the gate between two colours
     (convert-row
      x0 w 84 "GATE" sc 34
      [[dn-r  {:color :green}]
       [[0 0] {:color :purple :sundivers [[you 1]] :sd-angle (toward-angle dn-r)}]]
      [[dn-r  {:color :green}]
       [[0 0] {:color :purple :gates [[dn-r you]]}]]
      [[0 0]]
      ["two adjacent worlds of different colours,"
       "joined both ways for anyone. costs the"
       "move point too. +1 card."])

     (row 172 "FOUNDRY" 34 [dn-r dn-l] :foundry ["two sundivers, 120° apart"])
     (row 250 "MATRIX"  45 [dn-r up-l] :matrix  ["two sundivers, facing"])
     (row 338 "TOWER"   56 [dn-r up dn-l] :tower ["three sundivers, evenly spaced"])

     (banner x0 436 w "LEVEL")
     (at (+ x0 12) 460 1.0 (level-bars 3 you 11))
     (lines (+ x0 26) 458
            ["a station takes its region's level. a"
             "fresh region is one deeper than the"
             "deepest a gate leads out to, up to 3."]
            {:size 5.6 :lead 7.2})
     (lines x0 484
            ["draw that many cards. the new station"
             "activates once, immediately, for free."]
            {:size 5.6 :lead 7.2})]))

;; ── ACTIVATE ──────────────────────────────────────────────────────────────────

(defn- panel-activate
  [x0 w]
  (let [cw  (/ (- w 10) 2.0)          ; two blocks side by side
        bx2 (+ x0 cw 10)]
    [:g
     (column-head x0 54 "ACTIVATE" bolt-mark amber)
     (lines x0 66
            ["ONE STATION TYPE PER TURN · EVERY STATION OF THAT TYPE YOU HAVE A"
             "SUNDIVER ON, EACH ONE ONCE · THAT SUNDIVER THEN COMES HOME"]
            {:size 5.4 :lead 7 :fill faint})

     ;; the three types, and the sundiver coming home
     (for [[i [t label]] (map-indexed vector [[:foundry "FOUNDRY"] [:matrix "MATRIX"]
                                              [:tower "TOWER"]])
           :let [x (+ x0 26 (* i 48))]]
       [:g
        (at x 108 0.95 (piece t you 24))
        (caps x 119 label {:fill ink :size 5.2 :anchor "middle"})])
     (cluster (+ x0 192) 102 0.22
              [[[0 0] {:color :purple :station {:type :tower :level 2}
                       :sundivers [[you 1]]}]])
     (arrow [(+ x0 206) 102] [(+ x0 222) 102] {:c "you" :width 1})
     (habitat (+ x0 238) 102 10 3)
     (caps (+ x0 216) 119 "SUNDIVER COMES HOME" {:size 4.8 :anchor "middle"})

     ;; ACTIONS PER STATION
     (banner x0 126 w "ACTIONS PER STATION")
     (let [lx  (+ x0 26) ccw (/ (- w 26) 5.0) y0 142 rh 14
           col (fn [i] (+ lx (* i ccw)))]
       [:g
        (caps (+ x0 22) (+ y0 10) "LEVEL" {:anchor "end" :size 5 :fill dim})
        (caps (+ x0 22) (+ y0 rh 10) "BASE" {:anchor "end" :size 5 :fill dim})
        (caps (+ x0 22) (+ y0 (* 2 rh) 10) "BONUS" {:anchor "end" :size 5 :fill dim})
        (for [i [0 1 2 3]
              :let [{:keys [base bonus]} (game/station-action-counts i)]]
          [:g
           [:rect {:x (col i) :y y0 :width ccw :height (* 3 rh)
                   :fill "none" :stroke hexline :stroke-width 0.6}]
           (at (+ (col i) 16) (+ y0 3) 1.0 (level-bars i you 10))
           (txt (+ (col i) 38) (+ y0 11) (str i)
                {:size 8.5 :family sans :anchor "middle"
                 :fill (if (zero? i) faint ink)})
           (txt (+ (col i) (/ ccw 2.0)) (+ y0 rh 11) (str base)
                {:size 9.5 :family sans :anchor "middle"})
           (txt (+ (col i) (/ ccw 2.0)) (+ y0 (* 2 rh) 11) (str bonus)
                {:size 9.5 :family sans :anchor "middle"
                 :fill (if (zero? bonus) faint ink)})])
        ;; the sketch's fifth depth, which the code never reaches
        [:g {:opacity 0.5}
         (dashed-box (col 4) y0 ccw (* 3 rh))
         (txt (+ (col 4) (/ ccw 2.0)) (+ y0 11) "4"
              {:size 8.5 :family sans :anchor "middle" :fill dim})
         (txt (+ (col 4) (/ ccw 2.0)) (+ y0 rh 11) "5"
              {:size 9.5 :family sans :anchor "middle" :fill dim})
         (txt (+ (col 4) (/ ccw 2.0)) (+ y0 (* 2 rh) 11) "3"
              {:size 9.5 :family sans :anchor "middle" :fill dim})
         (sketch-tag (+ (col 4) (/ ccw 2.0)) (+ y0 (* 3 rh) 8) {:anchor "middle"})]])
     (lines x0 198
            ["BONUS — on your own station, take it or leave it. on someone else's"
             "the owner decides first; you get it only if they pass."]
            {:size 5.6 :lead 7.2})

     ;; ── FOUNDRY · BUILD ───────────────────────────────────────────────────
     (banner x0 220 cw "FOUNDRY · BUILD")
     (at (+ x0 14) 258 0.9 (piece :foundry you 22))
     (reserve (+ x0 50) 250 26 16)
     (at (+ x0 50) 250 0.42 (sundiver-glyph you 1.0))
     (caps (+ x0 50) 266 "RESERVE" {:size 4.4 :anchor "middle"})
     (arrow [(+ x0 66) 244] [(+ x0 90) 244] {:c "you" :width 1 :bow -4})
     (habitat (+ x0 106) 250 10 2)
     (caps (+ x0 106) 267 "HABITAT" {:size 4.4 :anchor "middle"})
     (lines x0 282
            ["each action moves up to two sundivers"
             "from your reserve into your habitat."]
            {:size 5.6 :lead 7.2})

     ;; ── MATRIX · PLANT ────────────────────────────────────────────────────
     (banner x0 304 cw "MATRIX · PLANT")
     (at (+ x0 12) 348 0.9 (piece :matrix you 22))
     (cluster (+ x0 54) 344 0.21
              [[up-l  {:mark :ghost}]
               [dn-l  {:mark :ghost}]
               [up    {:color :green}]
               [up-r  {:color :blue}]
               [dn-r  {:color :silver}]
               [dn    {:mark :ghost}]
               [[0 0] {:color :sun :beacon you}]])
     (cost (+ x0 30) 366 {:size 0.3})
     (arrow [(+ x0 76) 330] [(+ x0 92) 330] {:c "you" :width 1 :bow -4})
     (cipher-rosette (+ x0 110) 344 0.27
                     {[0 0] [:sun] up [:green] up-r [:blue] dn-r [:silver]})
     (caps (+ x0 110) 368 "CIPHER" {:size 4.6 :anchor "middle" :fill ink})
     (lines x0 384
            ["one beacon per action, for one sundiver:"
             "on the Ark, the space behind it, either"
             "rear flank, or any world where you have"
             "a sundiver."
             "a discovered beacon puts its world's"
             "colour into the cipher for its owner;"
             "others may join with a beacon of their"
             "own — free, then 1, then 2 …"]
            {:size 5.6 :lead 7.2})

     ;; ── TOWER · STEER → PROPEL → DISCOVER ─────────────────────────────────
     (banner bx2 220 cw "TOWER · STEER & PROPEL")
     (at (+ bx2 14) 262 0.9 (piece :tower you 24))
     (at (+ bx2 32) 250 0.7 (board/captain-flame))
     (caps (+ bx2 20) 276 "TAKE FLAME" {:size 4.4 :anchor "middle" :fill amber})
     (let [tiles [[up    {:mark :target}]
                  [up-r  {:mark :target :heading up-r}]
                  [dn-r  {:mark :target}]
                  [[0 0] {:color :void :ark up-r}]]
           tsc   0.2
           tcx   (+ bx2 62)
           tcy   250]
       [:g
        (cluster tcx tcy tsc tiles)
        (let [[ux uy] (hex-at tcx tcy tsc tiles up)
              [dx dy] (hex-at tcx tcy tsc tiles dn-r)]
          (arrow [(+ dx 9) dy] [(+ ux 9) uy] {:c "amber" :width 1 :bow 9}))
        (caps tcx 276 "STEER ±1" {:size 4.4 :anchor "middle"})])
     (cost (+ bx2 94) 250 {:size 0.28})
     (let [tiles [[dn-l  {:color :void :world? false}]
                  [up-r  {:color :green :beacon other :heading up-r}]
                  [[0 0] {:color :purple :ark up-r}]]
           psc   0.2
           pcx   (+ bx2 124)
           pcy   250]
       [:g
        (cluster pcx pcy psc tiles)
        (arrow [(- pcx 14) (+ pcy 12)] [(+ pcx 13) (- pcy 10)]
               {:c "amber" :width 1.1})
        (caps pcx 276 "PROPEL · DISCOVER" {:size 4.4 :anchor "middle"})])
     (lines bx2 292
            ["each action steers the heading straight,"
             "one step left or right for one sundiver,"
             "then the Ark advances onto the heading"
             "token — it may WRAP. any beacon it"
             "reaches is discovered: that world's"
             "colour enters the cipher."]
            {:size 5.6 :lead 7.2})

     ;; ── LAND ──────────────────────────────────────────────────────────────
     (banner bx2 340 cw "LAND")
     (cluster (+ bx2 30) 384 0.22
              [[up-l {:color :sun}]
               [dn-l {:color :silver}]
               [dn   {:color :green}]
               [up   {:color :blue}]
               [up-r {:color :purple}]
               [dn-r {:mark :ghost}]
               [[0 0] {:color :void :ark up-r
                       :arcs [[up-l :sun] [dn-l :silver] [dn :green]
                              [up :blue] [up-r :purple]]}]])
     (caps (+ bx2 30) 414 "5 OF 6 MATCH" {:size 4.6 :anchor "middle" :fill amber})
     (lines (+ bx2 66) 360
            ["only during a tower"
             "activation. the Ark may"
             "land when the world's"
             "colour is in the cipher"
             "centre and 5 of its 6"
             "neighbours match their"
             "slots (5: the Ark on it;"
             "6: on it or beside it)."]
            {:size 5.6 :lead 7.2})
     (lines bx2 428
            ["landing ends the game — score every beacon"
             "you hold on a matching slot. most wins."]
            {:size 5.6 :lead 7.2})]))

;; ── the band along the foot: what every action ends with ──────────────────────

(defn- panel-draw
  [x0 y w]
  (let [c2 (+ x0 (* w 0.40))
        c3 (+ x0 (* w 0.70))]
    [:g
     (banner x0 y w "DRAW")
     (at (+ x0 8) (+ y 20) 1.0 (level-bars 3 you 11))
     (txt (+ x0 18) (+ y 29) "→" {:size 8 :fill dim :family sans})
     (card (+ x0 34) (+ y 25) {:suit 2})
     (card (+ x0 47) (+ y 25) {:suit 0})
     (lines (+ x0 62) (+ y 20)
            ["cards equal to the levels of the stations you activated,"
             "or one per gate you built."
             "keep a single card and discard the rest."]
            {:size 5.6 :lead 7.2})
     ;; flare
     (at (+ c2 7) (+ y 22) 1.0 (board/suit-icon 4 flare-c 6))
     (arrow [(+ c2 16) (+ y 22)] [(+ c2 30) (+ y 22)] {:c "flare" :width 1})
     (caps (+ c2 34) (+ y 24) "PROPEL" {:size 5.6 :fill flare-c})
     (lines (+ c2 2) (+ y 36)
            ["every flare advances the Ark one space, and"
             (str "may DISCOVER. the " game/cards-per-suit "th flare, or an")
             "empty bag: everyone loses."]
            {:size 5.6 :lead 7.2})
     ;; captain
     (at (+ c3 6) (+ y 23) 0.85 (board/captain-flame))
     (caps (+ c3 15) (+ y 24) "CAPTAIN DRIFTS" {:size 5.6 :fill ink})
     (lines (+ c3 2) (+ y 36)
            ["the captain then steers, propels and draws one"
             "more card — a flare propels again."]
            {:size 5.6 :lead 7.2})]))

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
      [:path {:d "M 0,0 L 10,5 L 0,10 z" :fill c}]])])

(defn sheet
  []
  (let [m  18
        mv 186   ; MOVE
        cv 174   ; CONVERT
        ac 304   ; ACTIVATE — the big one
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
     [:line {:x1 (- x2 (/ g 2.0)) :y1 44 :x2 (- x2 (/ g 2.0)) :y2 494
             :stroke "#E2E2E8" :stroke-width 0.8}]
     [:line {:x1 (- x3 (/ g 2.0)) :y1 44 :x2 (- x3 (/ g 2.0)) :y2 494
             :stroke "#E2E2E8" :stroke-width 0.8}]
     ;; the foot
     (panel-draw m 500 (- sheet-w (* 2 m)))
     (txt m 566 "JOURNEY · one action a turn, then draw · after the v18 diagram"
          {:size 5.4 :fill faint :spacing 0.8})
     (txt (- sheet-w m) 566
          (str "faded and dashed = in the sketch, not in the code: "
               "station level 4 · steering by card suit (CW / CCW)")
          {:size 5.4 :fill faint :anchor "end"})]))

(defn -main
  [& args]
  (let [out (or (first args) "resources/public/img/journey-diagram.svg")]
    ;; Swap the board's screen palette for paper inks while the sheet renders,
    ;; so every board shape below still comes straight from journey.board.
    (with-redefs [board/world-outer    print-world-outer
                  board/world-inner    print-world-inner
                  board/neutral-stroke "#4A4A56"
                  board/captain-fill   "#E8A81C"
                  board/captain-stroke "#7A5200"]
      (spit out (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" (->svg (sheet)) "\n")))
    (println "wrote" out)
    (System/exit 0)))
