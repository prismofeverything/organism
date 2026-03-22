(ns journey.board
  (:require
   [clojure.string :as str]
   [journey.game :as game]))

;; ── Constants ──────────────────────────────────────────────────────────────────

(def ^:const hex-size   50)   ; circumradius (center → corner)
(def ^:const sqrt3      (Math/sqrt 3))
(def ^:const tau        (* 2 Math/PI))
(def ^:const panel-r    65)   ; player panel circle radius

;; ── Player color assignment ───────────────────────────────────────────────────

;; Turn-order index → tile color keyword (sun is NEUTRAL)
(def player-color-keys [:silver :green :blue :purple :void])

(defn build-player-colors
  "Map player-key string → tile color keyword, by position in turn-order."
  [player-order]
  (into {"NEUTRAL" :sun}
        (map-indexed #(vector %2 (nth player-color-keys (mod %1 5))) player-order)))

;; ── Color palette ─────────────────────────────────────────────────────────────

;; Tile hex backgrounds: dark but saturated tints (distinct from player piece colors)
(def tile-bg
  {:sun    "#4A3208" :silver "#18183A" :green  "#122012"
   :blue   "#0E1A3C" :purple "#1C0A30" :void   "#0E0C28"})

(def tile-border
  {:sun    "#B08820" :silver "#5050A0" :green  "#1E6A1E"
   :blue   "#123898" :purple "#4A10A0" :void   "#1C1468"})

;; World token (nested circles on each tile)
(def world-outer
  {:sun    "#A86C04" :silver "#5858A0" :green  "#166016"
   :blue   "#0A3E90" :purple "#4A0E90" :void   "#160A70"})

(def world-inner
  {:sun    "#FFCA18" :silver "#9898C8" :green  "#28A828"
   :blue   "#1468D8" :purple "#7E1EC8" :void   "#2812B0"})

;; Player pieces: same hue, significantly brighter/lighter
(def player-fill
  {:sun    "#FFD030" :silver "#C0C0E4" :green  "#40CC40"
   :blue   "#2898FF" :purple "#B038F0" :void   "#5438D8"})

(def player-stroke
  {:sun    "#FFEE80" :silver "#E0E0FF" :green  "#88FF88"
   :blue   "#88CCFF" :purple "#E090FF" :void   "#8878FF"})

(def neutral-fill   "#8898A8")
(def neutral-stroke "#B0C4CC")
(def captain-fill   "#F09020")
(def captain-stroke "#FFD060")

(defn pf [ck] (get player-fill   ck neutral-fill))
(defn ps [ck] (get player-stroke ck neutral-stroke))

;; ── Hex geometry ──────────────────────────────────────────────────────────────

(defn hex->pixel [[q r]]
  ;; Flat-top orientation: top and bottom edges are horizontal
  [(* hex-size 1.5 q)
   (* hex-size sqrt3 (+ (* 0.5 q) r))])

(defn hex-pts-str
  "SVG polygon points for a flat-top hex at origin with circumradius r."
  [r]
  (->> (range 6)
       (map (fn [i]
              (let [a (* i (/ Math/PI 3))]   ; start at 0° — vertex points right
                (str (* r (Math/cos a)) "," (* r (Math/sin a))))))
       (str/join " ")))

(defn edge-offset
  "Pixel [dx dy] from hex center to midpoint of edge facing hex-direction dir."
  [dir]
  (let [[dx dy] (hex->pixel dir)] [(/ dx 2) (/ dy 2)]))

(defn board-centroid
  "Mean pixel center of all tiles, for board-centering."
  [board]
  (if (empty? board)
    [0 0]
    (let [pxs (map (comp hex->pixel first) board)
          n   (count pxs)]
      [(/ (apply + (map first pxs)) n)
       (/ (apply + (map second pxs)) n)])))

;; ── SVG path helpers ──────────────────────────────────────────────────────────

(defn poly-pts
  "Polygon points string for N-gon at [cx cy] with radius r, start-angle a0."
  [cx cy r n a0]
  (->> (range n)
       (map (fn [i]
              (let [a (+ a0 (* i (/ tau n)))]
                (str (+ cx (* r (Math/cos a)))
                     ","
                     (+ cy (* r (Math/sin a)))))))
       (str/join " ")))

(defn tri-pts
  "Isosceles sundiver triangle at [cx cy] pointing up (angle 0 = north), scaled by s."
  [cx cy s]
  (let [tip  (* s 0.65)
        base (* s 0.42)
        back (* s 0.48)]
    (str cx "," (- cy tip) " "
         (- cx base) "," (+ cy back) " "
         (+ cx base) "," (+ cy back))))

;; ── World token ───────────────────────────────────────────────────────────────

(defn world-token [color-key]
  (let [oc (get world-outer color-key "#505060")
        ic (get world-inner color-key "#808090")]
    [:g
     [:circle {:cx 0 :cy 0 :r 20 :fill oc :stroke "#000" :stroke-width 0.5}]
     [:circle {:cx 0 :cy 0 :r 12 :fill ic}]]))

;; ── Beacon (pentagon) ─────────────────────────────────────────────────────────

(defn beacon-shape [color-key]
  [:polygon
   {:points (poly-pts 0 -24 7 5 (- (/ Math/PI 2)))
    :fill   (pf color-key)
    :stroke (ps color-key)
    :stroke-width 1.2}])

;; ── Station shapes ────────────────────────────────────────────────────────────

(defn foundry-shape [ck]
  ;; Hemisphere: flat bottom at y=5, dome curving up
  [:path {:d "M -11,5 A 11,11 0 0 1 11,5 Z"
          :fill (pf ck) :stroke (ps ck) :stroke-width 1.3}])

(defn matrix-shape [ck]
  ;; Lotus: 6 small circles around a center
  [:g
   (for [i (range 6)
         :let [a  (* i (/ Math/PI 3))
               px (* 5.5 (Math/cos a))
               py (* 5.5 (Math/sin a))]]
     [:circle {:key i :cx px :cy py :r 3.6
               :fill (pf ck) :stroke (ps ck) :stroke-width 0.7 :opacity 0.92}])
   [:circle {:cx 0 :cy 0 :r 3 :fill (pf ck) :stroke (ps ck) :stroke-width 1}]])

(defn tower-shape [ck]
  ;; Flared base + narrow neck + bulbous top
  [:g
   [:polygon {:points "-7,10 7,10 4,4 -4,4"
              :fill (pf ck) :stroke (ps ck) :stroke-width 0.9}]
   [:rect {:x -2.2 :y 0 :width 4.4 :height 4
           :fill (pf ck) :stroke (ps ck) :stroke-width 0.9}]
   [:circle {:cx 0 :cy -4 :r 5.5
             :fill (pf ck) :stroke (ps ck) :stroke-width 1.2}]])

(defn level-badge [n]
  (when (pos? n)
    [:text {:x 0 :y 38
            :text-anchor "middle"
            :fill "#99AABB"
            :font-size "8.5"
            :font-weight "bold"
            :font-family "monospace"}
     (str "L" n)]))

(defn station-shape [station]
  (when station
    (let [{:keys [type color-key level]} station]
      [:g
       (case type
         :foundry [foundry-shape color-key]
         :matrix  [matrix-shape  color-key]
         :tower   [tower-shape   color-key]
         nil)
       [level-badge (or level 0)]])))

;; ── Sundivers ─────────────────────────────────────────────────────────────────

;; ── Sundivers: radial placement around world token ───────────────────────────
;;
;; Each player occupies a sector of the ring around the world token.
;; Their sundivers fan along an arc at arc-r, each triangle pointing outward.
;;
;; SVG rotate(θ) on a triangle drawn pointing up (tip at y=-r):
;;   rotated tip = (r·sin θ, -r·cos θ)  so tip points outward at angle a when θ = 90 + a°

(def ^:const sundiver-arc-r   30)   ; radius of placement ring
(def ^:const sundiver-ang-step 12)  ; degrees between stacked sundivers

(defn sundivers-on-tile [tile player-order player-colors]
  (let [n-players (count player-order)]
    [:g
     (keep-indexed
      (fn [idx player]
        (let [n  (get-in tile [:sundivers player] 0)
              ck (get player-colors player :sun)]
          (when (pos? n)
            (let [fc         (pf ck)
                  sc         (ps ck)
                  base-deg   (* idx (/ 360.0 n-players))
                  show       (min n 5)
                  half-span  (* 0.5 (dec show) sundiver-ang-step)]
              [:g {:key player}
               (for [i (range show)
                     :let [deg (+ base-deg (- (* i sundiver-ang-step) half-span))
                           rad (* deg (/ Math/PI 180))
                           cx  (* sundiver-arc-r (Math/cos rad))
                           cy  (* sundiver-arc-r (Math/sin rad))
                           rot (+ 90 deg)]]
                 [:g {:key i :transform (str "translate(" cx "," cy ") rotate(" rot ")")}
                  [:polygon {:points (tri-pts 0 0 8)
                             :fill fc :stroke sc :stroke-width 0.8}]])
               (when (> n 5)
                 (let [rad (* base-deg (/ Math/PI 180))]
                   [:text {:x (* (+ sundiver-arc-r 14) (Math/cos rad))
                           :y (* (+ sundiver-arc-r 14) (Math/sin rad))
                           :text-anchor "middle" :dominant-baseline "middle"
                           :fill fc :font-size "7" :font-family "monospace"}
                    (str "×" n)]))]))))
      player-order)]))

;; ── Ark (ring) ────────────────────────────────────────────────────────────────

(defn ark-ring []
  [:circle {:cx 0 :cy 0 :r 30
            :fill "none"
            :stroke neutral-stroke
            :stroke-width 5
            :stroke-dasharray "10 3"
            :opacity 0.9}])

;; ── Heading token (directional arrow) ────────────────────────────────────────

(defn heading-marker [heading-dir]
  ;; Arrow pointing in the pixel direction of heading-dir
  (let [[dq dr] heading-dir
        [dx dy] (hex->pixel [dq dr])
        len     (Math/sqrt (+ (* dx dx) (* dy dy)))
        nx      (/ dx len)   ; unit vec toward heading
        ny      (/ dy len)
        tip     13
        base    6
        wing    7
        ;; Arrow vertices
        tx  (* nx tip)
        ty  (* ny tip)
        blx (- (* -1 nx base) (* ny wing))
        bly (+ (* -1 ny base) (* nx wing))
        brx (+ (* -1 nx base) (* ny wing))
        bry (- (* -1 ny base) (* nx wing))]
    [:polygon {:points (str tx "," ty " " blx "," bly " " brx "," bry)
               :fill captain-fill :stroke captain-stroke
               :stroke-width 1.2 :opacity 0.92}]))

;; ── Captain flame ─────────────────────────────────────────────────────────────

(defn captain-flame []
  [:path
   {:d "M 0,-9 C 3,-5 5,-2 3,3 C 1,6 -1,6 -3,3 C -5,-2 -3,-5 0,-9 Z"
    :fill captain-fill :stroke captain-stroke :stroke-width 0.8}])

;; ── Gate indicators ───────────────────────────────────────────────────────────

(defn gate-indicators [state pos player-order player-colors]
  [:g
   (for [player   player-order
         :let     [gates (get-in state [:players player :gates pos] #{})]
         neighbor gates
         :when    (contains? (:board state) neighbor)
         :let     [dir-vec (game/subtract-hex pos neighbor)
                   valid?  (some #(= % dir-vec) game/hex-directions)]
         :when    valid?
         :let     [[ex ey] (edge-offset dir-vec)
                   fc      (pf (get player-colors player :sun))]]
     [:line {:key (str player neighbor)
             :x1 0 :y1 0 :x2 ex :y2 ey
             :stroke fc :stroke-width 2.5 :opacity 0.6}])])

;; ── Cipher-match edge dots ────────────────────────────────────────────────────

(defn cipher-match-dots [state pos color-key]
  (let [center-colors (get-in state [:cipher [0 0] :colors] {})]
    (when (contains? center-colors color-key)
      [:g
       (for [dir   game/hex-directions
             :let  [neighbor-color (get-in state [:board (game/add-hex pos dir) :color])
                    cipher-colors  (get-in state [:cipher dir :colors] {})]
             :when (and neighbor-color (contains? cipher-colors neighbor-color))
             :let  [[ex ey] (edge-offset dir)
                    dot-c   (get world-inner neighbor-color "#FFF")]]
         [:circle {:key (str dir)
                   :cx ex :cy ey :r 4.5
                   :fill dot-c :stroke "#FFF" :stroke-width 0.6 :opacity 0.9}])])))

;; ── Single tile ───────────────────────────────────────────────────────────────

(defn render-tile
  [state pos tile player-order player-colors ark-pos heading-pos heading-dir highlight? on-click]
  (let [[cx cy] (hex->pixel pos)
        color   (:color tile)
        bg      (get tile-bg color "#0A0A12")
        bdr     (get tile-border color "#222")]
    [:g {:key      (str pos)
         :transform (str "translate(" cx "," cy ")")
         :on-click  (when on-click #(on-click pos))
         :style    {:cursor (when on-click "pointer")}}
     ;; Hex background
     [:polygon {:points       (hex-pts-str (- hex-size 1))
                :fill         bg
                :stroke       (if highlight? "#FFD030" bdr)
                :stroke-width (if highlight? 2.5 1)}]
     ;; Highlight glow outer ring
     (when highlight?
       [:polygon {:points (hex-pts-str (- hex-size 1))
                  :fill "none"
                  :stroke "#FFD030"
                  :stroke-width 7
                  :opacity 0.2}])
     ;; Gate lines (under world token)
     [gate-indicators state pos player-order player-colors]
     ;; World token (nested circles)
     [world-token color]
     ;; Beacon (pentagon, shown above world token)
     (when-let [bk (:beacon tile)]
       [beacon-shape (get player-colors bk)])
     ;; Station (foundry / matrix / tower) — shifted down to leave room for beacon
     (when-let [s (:station tile)]
       [:g {:transform "translate(0,14)"}
        [station-shape (assoc s :color-key (get player-colors (:player s) :sun))]])
     ;; Ark ring overlay
     (when (= pos ark-pos) [ark-ring])
     ;; Heading marker overlay
     (when (= pos heading-pos) [heading-marker heading-dir])
     ;; Sundivers (triangles grouped by player)
     [sundivers-on-tile tile player-order player-colors]
     ;; Cipher-match edge dots
     [cipher-match-dots state pos color]]))

;; ── Ghost hex (heading into unexplored space) ────────────────────────────────

(defn ghost-hex [heading-pos heading-dir]
  (let [[cx cy] (hex->pixel heading-pos)]
    [:g {:key      "ghost-hex"
         :transform (str "translate(" cx "," cy ")")}
     [:polygon {:points       (hex-pts-str (- hex-size 1))
                :fill         "none"
                :stroke       captain-fill
                :stroke-width 1.5
                :stroke-dasharray "5 4"
                :opacity      0.4}]
     [heading-marker heading-dir]]))

;; ── Floating ark (when ark is not on any board tile) ─────────────────────────

(defn ark-ghost [ark-pos heading-dir]
  (let [[cx cy] (hex->pixel ark-pos)]
    [:g {:key "ark-ghost"
         :transform (str "translate(" cx "," cy ")")}
     [:polygon {:points       (hex-pts-str (- hex-size 1))
                :fill         "#05050E"
                :stroke       neutral-stroke
                :stroke-width 1.5
                :stroke-dasharray "4 3"
                :opacity      0.55}]
     [ark-ring]
     [heading-marker heading-dir]]))

;; ── Board ─────────────────────────────────────────────────────────────────────

(defn render-board
  [state player-order player-colors pos-highlights on-hex-click]
  (let [board       (:board state)
        ark-pos     (:ark state)
        heading-pos (:heading-token state)
        heading-dir (game/heading-direction state)]
    [:g
     ;; Floating ark when it sits on unexplored space
     (when (and ark-pos (not (contains? board ark-pos)))
       [ark-ghost ark-pos heading-dir])
     ;; Tiles
     (for [[pos tile] board]
       [render-tile state pos tile player-order player-colors ark-pos heading-pos heading-dir
        (contains? pos-highlights pos)
        (when (contains? pos-highlights pos) on-hex-click)])
     ;; Ghost hex at heading-token if unexplored
     (when (and heading-pos (not (contains? board heading-pos)))
       [ghost-hex heading-pos heading-dir])]))

;; ── Cipher display ────────────────────────────────────────────────────────────

(def ^:const cipher-hex-size 26)

(defn cipher-hex [cipher pos]
  (let [[cx cy]  (let [[q r] pos]
                   [(* cipher-hex-size 1.5 q)
                    (* cipher-hex-size sqrt3 (+ (* 0.5 q) r))])
        entry    (get cipher pos {})
        ;; Only show colors where at least one player has placed a beacon
        colors   (keep (fn [[c players]] (when (seq players) c)) (:colors entry))
        bg       (if (= pos [0 0]) "#1A1830" "#0C0C1E")]
    [:g {:key      (str "c" pos)
         :transform (str "translate(" cx "," cy ")")}
     [:polygon {:points (hex-pts-str (- cipher-hex-size 1))
                :fill bg :stroke "#2A2A4A" :stroke-width 1}]
     ;; Color dots
     (let [n (count colors)]
       (for [[i c] (map-indexed vector colors)
             :let  [a   (if (= n 1) 0 (* i (/ tau n)))
                    r   (if (= n 1) 0 (* cipher-hex-size 0.38))
                    icf (get world-inner c "#555")]]
         [:circle {:key (str c i)
                   :cx (* r (Math/cos a))
                   :cy (* r (Math/sin a))
                   :r  (if (= n 1) (* cipher-hex-size 0.35) (* cipher-hex-size 0.22))
                   :fill icf}]))
     (when (= pos [0 0])
       [:text {:x 0 :y 1
               :text-anchor "middle"
               :dominant-baseline "middle"
               :fill "#445" :font-size "7" :font-family "monospace"}
        "C"])]))

(defn render-cipher [cipher]
  [:g
   (for [pos (into [[0 0]] game/hex-directions)]
     [cipher-hex cipher pos])])

;; ── Player area ───────────────────────────────────────────────────────────────

(defn render-player-area
  [state player-key player-colors captain y-offset _panel-w]
  (let [pstate   (get-in state [:players player-key] {})
        hab      (get-in pstate [:habitat :sundivers] 0)
        res      (:reserve pstate {})
        ck       (get player-colors player-key :sun)
        fc       (pf ck)
        sc       (ps ck)
        is-cap   (= player-key captain)
        on-turn? (= player-key (game/current-player state))
        ;; Circle center within this panel's local coords
        cx       panel-r
        cy       panel-r]
    [:g {:key (str "p" player-key)
         :transform (str "translate(0," y-offset ")")}
     ;; Circle backing
     [:circle {:cx cx :cy cy :r panel-r
               :fill (if on-turn? "#161222" "#080814")
               :stroke (if on-turn? fc "#2A2A44")
               :stroke-width (if on-turn? 3 1.5)}]
     ;; Captain flame (top-right quadrant)
     (when is-cap
       [:g {:transform (str "translate(" (+ cx 38) "," (- cy 40) ")")}
        [captain-flame]])
     ;; Player name
     [:text {:x cx :y (- cy 40)
             :text-anchor "middle"
             :fill fc :font-size "11" :font-weight "bold" :font-family "monospace"}
      player-key]
     ;; Habitat sundivers — up to 7 per row, two rows if needed
     [:g {:transform (str "translate(" cx "," (- cy 24) ")")}
      (let [row1 (min hab 7)
            row2 (max 0 (- hab 7))
            row-x (fn [n i] (- (* i 10) (* (dec (max n 1)) 5)))]
        [:g
         ;; First row
         (for [i (range row1)
               :let [ox (row-x row1 i)]]
           [:polygon {:key i :points (tri-pts ox 0 7)
                      :fill fc :stroke sc :stroke-width 0.7}])
         ;; Second row (when hab > 7)
         (for [i (range row2)
               :let [ox (row-x row2 i)]]
           [:polygon {:key (+ 7 i) :points (tri-pts ox 12 7)
                      :fill fc :stroke sc :stroke-width 0.7}])])]
     ;; Move points
     [:text {:x cx :y (+ cy 2)
             :text-anchor "middle"
             :fill "#88AACC" :font-size "9" :font-family "monospace"}
      (str "move " (game/move-points state player-key))]
     ;; Reserve counts — full names, two per line
     [:text {:x cx :y (+ cy 16)
             :text-anchor "middle" :fill "#6A8A6A" :font-size "7" :font-family "monospace"}
      (str "sundivers: " (:sundivers res 0) "  beacons: " (:beacons res 0))]
     [:text {:x cx :y (+ cy 27)
             :text-anchor "middle" :fill "#6A8A6A" :font-size "7" :font-family "monospace"}
      (str "foundries: " (:foundries res 0) "  matrixes: " (:matrixes res 0))]
     [:text {:x cx :y (+ cy 38)
             :text-anchor "middle" :fill "#6A8A6A" :font-size "7" :font-family "monospace"}
      (str "towers: " (:towers res 0) "  gates: " (:gates res 0))]
     ;; Held card
     (when-let [card (get-in pstate [:held-card])]
       [:text {:x cx :y (+ cy 54)
               :text-anchor "middle"
               :fill "#99BBDD" :font-size "8" :font-family "monospace"}
        (str "suit " (:suit card) "  val " (:value card))])]))

;; ── Deterministic starfield ────────────────────────────────────────────────────
;;
;; Four layered passes: dust, small, medium, and rare giants.
;; Stars are generated in a slightly oversized field so the parallax
;; pan never reveals an edge.  pan-x / pan-y are applied at 18% speed.

(defn render-starfield [w h pan-x pan-y zoom]
  (let [;; Extend field beyond viewport to cover parallax drift
        fw    (* w 1.25)
        fh    (* h 1.25)
        ox    (* w -0.125)   ; top-left offset of the extended field
        oy    (* h -0.125)

        frac  (fn [x] (- x (Math/floor x)))
        hash2 (fn [a b] (frac (* (+ (* a 127.1) (* b 311.7)) 43758.5453)))

        ;; 9 cluster focal points in normalised coords
        clusters [[0.12 0.22] [0.71 0.10] [0.44 0.56] [0.87 0.68]
                  [0.29 0.80] [0.59 0.36] [0.08 0.63] [0.52 0.15] [0.35 0.45]]

        ;; Four layers: dust · small · medium · giants
        layers
        [{:n 380 :ax 1.0   :ay 1.0   :bx 0.0   :by 0.0
          :r-base 0.25 :r-range 0.55 :op-base 0.06 :op-range 0.35 :pull 0.28}
         {:n 160 :ax 0.618 :ay 0.732 :bx 0.41  :by 0.19
          :r-base 0.50 :r-range 0.90 :op-base 0.12 :op-range 0.45 :pull 0.38}
         {:n 55  :ax 0.382 :ay 0.905 :bx 0.73  :by 0.57
          :r-base 0.90 :r-range 1.30 :op-base 0.18 :op-range 0.35 :pull 0.50}
         {:n 12  :ax 0.236 :ay 0.541 :bx 0.17  :by 0.84
          :r-base 1.60 :r-range 1.20 :op-base 0.22 :op-range 0.22 :pull 0.60}]

        tints ["#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF"
               "#FFE8C0" "#FFE4B0" "#C8D8FF" "#D0E4FF"
               "#FFFFFF" "#FFF0D8"]

        stars
        (mapcat
         (fn [{:keys [n ax ay bx by r-base r-range op-base op-range pull]}]
           (for [i (range n)]
             (let [u     (frac (* (+ i 0.5) ax))
                   v     (frac (+ (* (+ i 0.5) ay) bx))
                   pull? (< (hash2 i (+ n 1)) pull)
                   [cu cv] (nth clusters (mod i (count clusters)))
                   ;; Stronger cluster pull for denser nebula feel
                   blend (+ 0.45 (* 0.25 (hash2 i (+ n 7))))
                   sx    (if pull? (+ (* u (- 1 blend)) (* cu blend)) u)
                   sy    (if pull? (+ (* v (- 1 blend)) (* cv blend)) v)
                   t     (hash2 i n)
                   s     (hash2 (+ i 13) by)
                   r     (+ r-base (* t r-range))
                   op    (+ op-base (* s op-range))
                   color (nth tints (mod i (count tints)))]
               {:x (+ ox (* sx fw)) :y (+ oy (* sy fh)) :r r :op op :color color})))
         layers)

        ;; Parallax: stars drift at 18% of pan speed and 12% of zoom depth
        ;; Scale is centred on the canvas midpoint so it feels anchored
        tx         (* pan-x 0.18)
        ty         (* pan-y 0.18)
        star-scale (+ 1.0 (* (- zoom 1.0) 0.12))
        cx         (/ w 2)
        cy         (/ h 2)
        ;; CSS translate(px) + scale centred at (cx,cy): shift by (1-s)*centre
        fx         (+ tx (* cx (- 1.0 star-scale)))
        fy         (+ ty (* cy (- 1.0 star-scale)))
        css-tx     (str "translate(" fx "px," fy "px) scale(" star-scale ")")]
    [:g {:style          {:transform  css-tx
                          :transition "transform 0.18s cubic-bezier(0.2,0,0,1)"}
         :pointer-events "none"}
     (map-indexed
      (fn [i {:keys [x y r op color]}]
        [:circle {:key i :cx x :cy y :r r :fill color :opacity op}])
      stars)]))

;; ── SVG defs (filters) ────────────────────────────────────────────────────────

(defn svg-defs []
  [:defs
   [:filter {:id "glow" :x "-30%" :y "-30%" :width "160%" :height "160%"}
    [:feGaussianBlur {:stdDeviation "3" :result "blur"}]
    [:feMerge
     [:feMergeNode {:in "blur"}]
     [:feMergeNode {:in "SourceGraphic"}]]]])

;; ── Full game render ──────────────────────────────────────────────────────────

(def vw 1400)
(def vh 900)
;; Left column: circular player panels (r=65, centered at x=75)
;; Board center shifted right to accommodate left panels
(def board-center-x 780)
(def board-center-y 450)
(def panel-x        10)
(def panel-w        (* 2 panel-r))  ; 130 — diameter
(def panel-h        (* 2 panel-r))  ; 130 — diameter
(def panel-gap      16)
(def cipher-x       1330)
(def cipher-y       150)

(defn render-game
  "Render the full game SVG.
   pos-highlights: set of [q r] positions to highlight as clickable.
   on-hex-click:   fn [pos] called when a highlighted tile is clicked.
   choice-buttons: seq of {:label str :on-click fn} for non-position choices.
   opts (optional map):
     :pan-x :pan-y  — board pan offset in screen pixels (default 0)
     :zoom          — board zoom factor (default 1.0)
     :on-bg-mouse-down — handler for mouse-down on the background (for panning)"
  [state pos-highlights on-hex-click choice-buttons
   & [{:keys [pan-x pan-y zoom on-bg-mouse-down]
       :or   {pan-x 0 pan-y 0 zoom 1.0}}]]
  (let [player-order  (:turn-order state)
        player-colors (build-player-colors player-order)
        captain       (:captain-flame state)
        current       (game/current-player state)
        [bcx bcy]     (board-centroid (:board state))]
    [:svg {:xmlns   "http://www.w3.org/2000/svg"
           :viewBox (str "0 0 " vw " " vh)
           :width   "100%" :height "100%"
           :style   {:display "block" :background "#04040E"}}
     [svg-defs]

     ;; ── Background starfield (pointer-events:none so bg rect receives clicks)
     [render-starfield vw vh pan-x pan-y zoom]

     ;; ── Background rect for pan interaction (sits behind board tiles)
     (when on-bg-mouse-down
       [:rect {:x 0 :y 0 :width vw :height vh
               :fill "transparent"
               :on-mouse-down on-bg-mouse-down
               :style {:cursor "grab"}}])

     ;; ── Board (hex tiles + pieces) — pan/zoom applied here only
     ;; CSS transform so the transition property animates it smoothly
     [:g {:style {:transform  (str "translate(" (+ board-center-x pan-x) "px,"
                                               (+ board-center-y pan-y) "px)"
                                   " scale(" zoom ")"
                                   " translate(" (- bcx) "px," (- bcy) "px)")
                  :transition "transform 0.18s cubic-bezier(0.2,0,0,1)"}}
      [render-board state player-order player-colors pos-highlights on-hex-click]]

     ;; ── Cipher (fixed position, not panned/zoomed)
     [:g {:transform (str "translate(" cipher-x "," cipher-y ")")}
      [:text {:x 0 :y -78
              :text-anchor "middle"
              :fill "#445566"
              :font-size "10" :font-family "monospace" :letter-spacing "2"}
       "CIPHER"]
      [render-cipher (:cipher state)]]

     ;; ── Player panels (fixed, left column)
     [:g {:transform (str "translate(" panel-x ",0)")}
      (map-indexed
       (fn [i player]
         (let [py (+ 10 (* i (+ panel-h panel-gap)))]
           ^{:key player}
           [render-player-area state player player-colors captain py panel-w]))
       player-order)]

     ;; ── Game info (below player circles on the left)
     (let [info-y (+ 10 (* (count player-order) (+ panel-h panel-gap)) panel-h 16)]
       [:g {:transform (str "translate(" (+ panel-x panel-r) "," info-y ")")}
        [:text {:x 0 :y 0
                :text-anchor "middle"
                :fill (pf (get player-colors current :sun))
                :font-size "10" :font-weight "bold" :font-family "monospace"}
         (str "TURN: " current)]
        [:text {:x 0 :y 14
                :text-anchor "middle"
                :fill "#334455"
                :font-size "9" :font-family "monospace"}
         (str "Round " (:round state 0)
              "  Flares " (:flares-drawn state 0) "/13")]])

     ;; ── Choice buttons (non-position choices)
     (when (seq choice-buttons)
       [:g {:transform (str "translate(200," (- vh 60) ")")}
        (map-indexed
         (fn [i {:keys [label on-click]}]
           [:g {:key i
                :transform (str "translate(" (* i 148) ",0)")
                :on-click  on-click
                :style     {:cursor "pointer"}}
            [:rect {:x 0 :y 0 :width 138 :height 38
                    :fill "#10182A" :stroke "#2A4A80"
                    :stroke-width 1.5 :rx 5}]
            [:text {:x 69 :y 23
                    :text-anchor "middle"
                    :fill "#7AAAE0"
                    :font-size "12" :font-family "monospace"}
             label]])
         choice-buttons)])]))
