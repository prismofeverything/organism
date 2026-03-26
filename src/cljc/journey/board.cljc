(ns journey.board
  (:require
   [clojure.string :as str]
   [journey.game :as game]
   #?(:cljs [reagent.core :as r])))

;; ── Constants ──────────────────────────────────────────────────────────────────

(def ^:const hex-size   50)   ; circumradius (center → corner)
(def ^:const sqrt3      (Math/sqrt 3))
(def ^:const tau        (* 2 Math/PI))
(def ^:const panel-r    80)   ; player panel circle radius

;; ── Player color assignment ───────────────────────────────────────────────────

;; Turn-order index → tile color keyword (sun is NEUTRAL)
(def player-color-keys [:silver :green :blue :purple :void])

(defn build-player-colors
  "Map player-key string → tile color keyword, by position in turn-order."
  [player-order]
  (into {"NEUTRAL" :sun}
        (map-indexed #(vector %2 (nth player-color-keys (mod %1 5))) player-order)))

;; ── Color palette ─────────────────────────────────────────────────────────────

;; ;; Tile hex backgrounds: lighter, less-saturated tints
;; (def tile-bg
;;   {:sun    "#382C16" :silver "#28283C" :green  "#1E2E1E"
;;    :blue   "#1A2238" :purple "#251830" :void   "#181828"})

(def tile-border
  {:sun    "#B08820" :silver "#5050A0" :green  "#1E6A1E"
   :blue   "#123898" :purple "#4A10A0" :void   "#1C1468"})

;; Tile hex backgrounds: lighter, less-saturated tints
(def tile-bg
  {:sun    "#d92f11" :silver "#b3b3b3" :green  "#119650"
   :blue   "#8892cb" :purple "#7e24b3" :void   "#000000"})

;; World token (nested circles on each tile)
(def world-outer
  {:sun    "#e89c23" :silver "#e3e3e3" :green  "#116630"
   :blue   "#5562ab" :purple "#ae44e3" :void   "#333333"})

;; World token (nested circles on each tile)
(def world-inner
  {:sun    "#dbeb05" :silver "#838383" :green  "#113610"
   :blue   "#aac2fb" :purple "#de94f3" :void   "#666666"})

;; ;; World token (nested circles on each tile)
;; (def world-outer
;;   {:sun    "#e89c23" :silver "#d3d3d3" :green  "#31b670"
;;    :blue   "#a8b2eb" :purple "#9e44d3" :void   "#333333"})

;; ;; World token (nested circles on each tile)
;; (def world-inner
;;   {:sun    "#dbeb05" :silver "#ffffff" :green  "#71f6b0"
;;    :blue   "#d8e2fb" :purple "#ce74f3" :void   "#666666"})

;; (def world-inner
;;   {:sun    "#FFCA18" :silver "#9898C8" :green  "#28A828"
;;    :blue   "#1468D8" :purple "#7E1EC8" :void   "#2812B0"})

;; ;; Player pieces: same hue, significantly brighter/lighter
;; (def player-fill
;;   {:sun    "#e89c23" :silver "#dbdbdb" :green  "#83c1a4"
;;    :blue   "#aab2e0" :purple "#a76dc9" :void   "#4a4a4a"})

;; Tile hex backgrounds: lighter, less-saturated tints
(def player-fill
  {:sun    "#e93f21" :silver "#c3c3c3" :green  "#42af7d"
   :blue   "#98a2db" :purple "#8e34c3" :void   "#000000"})

;; ;; Player pieces: same hue, significantly brighter/lighter
;; (def player-fill
;;   {:sun    "#FFD030" :silver "#C0C0E4" :green  "#40CC40"
;;    :blue   "#2898FF" :purple "#B038F0" :void   "#5438D8"})

(def player-stroke
  {:sun    "#FFEE80" :silver "#E0E0FF" :green  "#88FF88"
   :blue   "#88CCFF" :purple "#E090FF" :void   "#8878FF"})

(def neutral-fill   "#8898A8")
(def neutral-stroke "#B0C4CC")
(def captain-fill   "#F09020")
(def captain-stroke "#FFD060")

(defn pf  [ck] (get player-fill ck neutral-fill))
(defn ps  [ck] (get player-stroke ck neutral-stroke))
(defn ptb [ck] (get tile-bg ck neutral-fill))

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
     [:circle {:cx 0 :cy 0 :r 24 :fill oc}]
     [:circle {:cx 0 :cy 0 :r 14 :fill ic}]]))

;; ── Beacon (pentagon) ─────────────────────────────────────────────────────────

(defn beacon-shape [color-key]
  ;; Pentagon centered on the tile, overlaid on the world token
  [:polygon
   {:points (poly-pts 0 0 10 5 (- (/ Math/PI 2)))
    :fill   (pf color-key)
    :stroke (ps color-key)
    :stroke-width 1.4
    :opacity 0.92}])

;; ── Station shapes ────────────────────────────────────────────────────────────

(defn foundry-shape [ck]
  ;; Hemisphere: flat bottom at y=5, dome curving up
  [:path {:d "M -11,5 A 11,11 0 0 1 11,5 Z"
          :fill (pf ck) :stroke (ps ck) :stroke-width 1.3}])

(defn matrix-shape [ck]
  ;; Lotus side-view: 5 narrow petal-ellipses fanning from a base
  [:g
   (for [[idx [rot px py]] (map-indexed vector
                             [[-38 -7 2] [-18 -3.5 -1] [0 0 -3] [18 3.5 -1] [38 7 2]])]
     [:ellipse {:key idx :cx px :cy py :rx 2.6 :ry 7.5
                :transform (str "rotate(" rot "," px "," py ")")
                :fill (pf ck) :stroke (ps ck) :stroke-width 0.7 :opacity 0.9}])
   ;; Curved base cup
   [:path {:d "M -9,7 Q 0,4 9,7"
           :fill "none" :stroke (ps ck) :stroke-width 1.2 :opacity 0.7}]])

(defn tower-shape [ck]
  ;; Space needle tower: flat bottom, narrow waist, ridge higher up, round dome top
  [:g
   [:path {:d (str "M -7,10 L 7,10 "          ;; flat bottom
                   "L 4,4 L 2.5,0 "           ;; right side tapers in
                   "L 2,-3 "                   ;; narrow waist
                   "L 7,-5 L 7,-7 "           ;; ridge flares right (higher)
                   "L 2,-8 "                   ;; ridge back to narrow
                   "L 2,-10 "                  ;; neck to dome
                   "L -2,-10 "                 ;; across to left
                   "L -2,-8 "                  ;; left neck
                   "L -7,-7 L -7,-5 "         ;; ridge flares left
                   "L -2,-3 "                  ;; left waist
                   "L -2.5,0 L -4,4 Z")       ;; left side back to base
           :fill (pf ck) :stroke (ps ck) :stroke-width 1.2}
    ;; Round dome on top
    [:circle {:cx 0 :cy -13 :r 5
              :fill (pf ck) :stroke (ps ck) :stroke-width 1.2}]]])

(defn level-platforms
  "Render n flat platform boxes stacked below the station icon.
   Platform i (0=bottom) sits at y=(9 - i*5) within the station group.
   Hex tile boundary ≈43px from center; station group at y=26 → budget of 17px."
  [n ck]
  (when (pos? n)
    (let [ph 4 slot 5]
      [:g
       (map-indexed
        (fn [i _]
          [:rect {:key       i
                  :x         -10
                  :y         (- 9 (* i slot))
                  :width     20
                  :height    ph
                  :rx        2
                  :fill      (pf ck)
                  :stroke    (ps ck)
                  :stroke-width 0.6
                  :opacity   0.55}])
        (range n))])))

(defn station-shape [station]
  (when station
    (let [{:keys [type color-key level]} station
          lv      (or level 0)
          ;; Lift station up so it sits just above the top platform.
          ;; y-lift = -(lv-1)*5 - 1  (negative = upward in SVG)
          y-lift  (if (pos? lv) (- (+ (* (dec lv) 5) 1)) 0)]
      [:g
       [:g {:transform (str "translate(0," y-lift ")")}
        (case type
          :foundry [foundry-shape color-key]
          :matrix  [matrix-shape  color-key]
          :tower   [tower-shape   color-key]
          nil)]
       [level-platforms lv color-key]])))

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

(defn sundivers-on-tile [tile player-order player-colors & [{:keys [highlight-player highlight-color on-sundiver-click]}]]
  (let [n-players (count player-order)]
    [:g
     (keep-indexed
      (fn [idx player]
        (let [n  (get-in tile [:sundivers player] 0)
              ck (get player-colors player :sun)]
          (when (pos? n)
            (let [fc         (pf ck)
                  sc         (ps ck)
                  hl?        (and highlight-player (= player highlight-player))
                  hl-c       (or highlight-color "#FFD030")
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
                 [:g {:key i :transform (str "translate(" cx "," cy ") rotate(" rot ")")
                      :on-click (when (and hl? on-sundiver-click)
                                  (fn [e] (.stopPropagation e) (on-sundiver-click)))
                      :style {:cursor (when (and hl? on-sundiver-click) "pointer")}}
                  (when hl?
                    [:polygon {:points (tri-pts 0 0 13)
                               :fill "none" :stroke hl-c :stroke-width 2.5
                               :opacity 0.7}])
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

;; ── Ark (ring + heading arrow at its edge) ───────────────────────────────────

(defn ark-ring []
  [:circle {:cx 0 :cy 0 :r 30
            :fill "none"
            :stroke neutral-stroke
            :stroke-width 5
            :stroke-dasharray "10 3"
            :opacity 0.9}])

(defn ark-heading-arrow
  "Bird-wing chevron at the ark ring edge pointing in heading-dir.
   Path designed pointing right (+X); rotated to match heading direction."
  [heading-dir]
  (let [[dq dr] heading-dir
        [dx dy] (hex->pixel [dq dr])
        angle   (* (/ 180 Math/PI) (Math/atan2 dy dx))]
    [:g {:transform (str "rotate(" angle ")")}
     [:path
      {:d (str "M 43,0 "
               "C 38,-3 33,-8 28,-12 "   ; right leading edge sweeps back
               "L 23,-15 "               ; right wingtip flares backward-outward
               "C 25,-12 27,-7 28,-2 "   ; right trailing edge curves to notch
               "L 28,2 "                 ; center notch (chevron indent)
               "C 27,7 25,12 23,15 "     ; left trailing edge
               "L 28,12 "               ; left wingtip
               "C 33,8 38,3 43,0 Z")     ; left leading edge back to nose
       :fill         captain-fill
       :stroke       captain-stroke
       :stroke-width 1.0
       :opacity      0.95}]]))

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

(defn render-all-gates
  "Board-level gate ovals rendered after all tile backgrounds.
   Rotated 90° so the oval bridges across the edge between tiles,
   with a background fill that erases the hex gap."
  [state player-order player-colors]
  (let [board (:board state)]
    [:g
     (for [[pos _]  board
           :let     [[cx cy] (hex->pixel pos)]
           player   player-order
           :let     [gates (get-in state [:players player :gates pos] #{})]
           neighbor gates
           ;; Each edge drawn once (from the lexicographically smaller tile)
           :when    (and (contains? board neighbor)
                         (neg? (compare pos neighbor)))
           :let     [dir-vec (game/subtract-hex pos neighbor)
                     valid?  (some #(= % dir-vec) game/hex-directions)]
           :when    valid?
           :let     [[ex ey] (edge-offset dir-vec)
                     gx      (+ cx ex)
                     gy      (+ cy ey)
                     angle   (* (/ 180 Math/PI) (Math/atan2 ey ex))
                     fc      (pf (get player-colors player :sun))
                     sc      (ps (get player-colors player :sun))]]
       [:rect {:key       (str pos player neighbor)
               :x        (- gx 8)  :y (- gy 13)
               :width     16  :height 26
               :rx        3   :ry 3
               :transform (str "rotate(" angle "," gx "," gy ")")
               :fill      fc
               :stroke    sc
               :stroke-width 1.2}])]))

;; ── Cipher-match edge dots ────────────────────────────────────────────────────

;; When this tile's color is in the cipher center AND a neighbor's color matches
;; the cipher position in that direction, render a half-oval arc at the shared edge.
;; Path (pre-transform, outward = +x after rotate(angle)):
;;   M 0,-13  A 8,13 0 0,0  0,13  Z
;; Half-annulus that wraps around the gate oval (rx=8, ry=13) like a halo ring.
;;
;; Path construction (in local pre-transform space, x≤0 = inward toward tile center):
;;   Outer half (CCW, sweep=0): M 0,-16  A 11,16 0 0,0  0,16
;;   Connect flat edges:        L 0,13
;;   Inner half (CW,  sweep=1): A  8,13 0 0,1  0,-13   ← reversed = nonzero hole
;;   Z (close flat edge back to outer top)
;;
;; With nonzero winding the inner oval region is excluded, leaving a 3px ring.
;; Rendered below render-all-gates so the gate oval sits on top, masking the inner edge.
;; Two such rings from adjacent tiles form a complete annulus around the gate.
(defn cipher-match-dots [state pos color-key]
  (when (game/cipher-color-active? state [0 0] color-key)
    [:g
     (for [dir   game/hex-directions
             :let  [neighbor-color (get-in state [:board (game/add-hex pos dir) :color])]
             :when (and neighbor-color (game/cipher-color-active? state dir neighbor-color))
             :let  [[ex ey] (edge-offset dir)
                    angle   (* (/ 180 Math/PI) (Math/atan2 ey ex))
                    arc-c   (get world-outer neighbor-color "#FFF")]]
         [:path {:key          (str dir)
                 ;; Half-annulus wrapping around the gate rectangle (16×26 → half-w=8, half-h=13)
                 ;; Outer: rx=13, ry=19 (well outside gate), Inner: rx=9, ry=15 (just inside gate edge)
                 :d            "M 0,-19 A 13,19 0 0,0 0,19 L 0,15 A 9,15 0 0,1 0,-15 Z"
                 :transform    (str "translate(" ex "," ey ") rotate(" angle ")")
                 :fill         arc-c
                 :stroke       arc-c
                 :stroke-width 1.5
                 :opacity      0.8}])]))

;; ── Single tile ───────────────────────────────────────────────────────────────

(defn- render-tile*
  "Inner tile render. cipher-dots is the pre-computed cipher-match-dots hiccup (or nil).
   highlight? can be :gold (launch/destination), :chosen (selected sundiver), or nil.
   sundiver-hl is {:highlight-player p :highlight-color c} or nil.
   match-count is the number of cipher direction matches (0-6)."
  [pos tile cipher-dots player-order player-colors ark-pos heading-dir highlight? on-click sundiver-hl match-count]
  (let [[cx cy] (hex->pixel pos)
        color   (:color tile)
        bg      (get tile-bg color "#141420")
        hl-color (case highlight?
                   :gold   "#FFD030"
                   :fly    "#30C8FF"
                   :chosen "#30FF90"
                   nil)
        landable-5? (= match-count 5)
        landable-6? (>= match-count 6)]
    [:g {:key      (str pos)
         :transform (str "translate(" cx "," cy ")")
         :on-click  (when on-click #(on-click pos))
         :style    {:cursor (when on-click "pointer")}}
     ;; 6-match: outer golden halo
     (when landable-6?
       [:polygon {:points (hex-pts-str (+ hex-size 4))
                  :fill "none"
                  :stroke "#FFD030"
                  :stroke-width 4
                  :opacity 0.6}])
     ;; 5-match: white glow ring
     (when landable-5?
       [:polygon {:points (hex-pts-str (+ hex-size 2))
                  :fill "none"
                  :stroke "#FFFFFF"
                  :stroke-width 3
                  :opacity 0.35}])
     ;; Hex background
     [:polygon {:points       (hex-pts-str (- hex-size 1))
                :fill         bg
                :stroke       (cond
                                hl-color      hl-color
                                landable-6?   "#FFD030"
                                landable-5?   "#FFFFFF88"
                                :else         "none")
                :stroke-width (cond
                                hl-color    2.5
                                landable-6? 2
                                landable-5? 1.5
                                :else       0)}]
     ;; 6-match: inner glow fill
     (when landable-6?
       [:polygon {:points (hex-pts-str (- hex-size 2))
                  :fill "#FFD030"
                  :opacity 0.08}])
     ;; Choice highlight glow outer ring
     (when hl-color
       [:polygon {:points (hex-pts-str (- hex-size 1))
                  :fill "none"
                  :stroke hl-color
                  :stroke-width 7
                  :opacity 0.2}])
     ;; World token (nested circles) — hidden once the beacon has been discovered
     (when (:world tile)
       [world-token color])
     ;; Beacon (pentagon) centered on tile, overlaid on world token
     (when-let [bk (:beacon tile)]
       [beacon-shape (get player-colors bk)])
     ;; Station below the world token
     (when-let [s (:station tile)]
       [:g {:transform "translate(0,26)"}
        [station-shape (assoc s :color-key (get player-colors (:player s) :sun))]])
     ;; Ark ring + heading arrow (at ring edge) when Ark is on this tile
     (when (= pos ark-pos)
       [:g [ark-ring] [ark-heading-arrow heading-dir]])
     ;; Sundivers (triangles grouped by player)
     [sundivers-on-tile tile player-order player-colors sundiver-hl]
     ;; Cipher-match edge dots (pre-computed)
     cipher-dots]))

;; In ClojureScript, wrap as a memoized React class component.
;; A tile only re-renders when its data, cipher-dots, highlight, or ark presence changes.
;; Most tiles are unchanged on most steps, so this avoids O(tiles) SVG work per frame.
#?(:cljs
   (def render-tile
     (r/create-class
      {:display-name "render-tile"
       :should-component-update
       (fn [_ old-argv new-argv]
         ;; argv = [component-fn pos tile cipher-dots player-order player-colors
         ;;         ark-pos heading-dir highlight? on-click sundiver-hl match-count]
         (let [pos     (nth old-argv 1)
               old-tile (nth old-argv 2)  new-tile (nth new-argv 2)
               old-cd   (nth old-argv 3)  new-cd   (nth new-argv 3)
               old-ark  (nth old-argv 6)  new-ark  (nth new-argv 6)
               old-hd   (nth old-argv 7)  new-hd   (nth new-argv 7)
               old-hl   (nth old-argv 8)  new-hl   (nth new-argv 8)
               old-shl  (nth old-argv 10 nil) new-shl (nth new-argv 10 nil)
               old-mc   (nth old-argv 11 0)  new-mc  (nth new-argv 11 0)]
           (or (not= old-tile new-tile)
               (not= old-cd   new-cd)
               (not= old-hl   new-hl)
               (not= old-shl  new-shl)
               (not= old-mc   new-mc)
               ;; Ark arrived at or departed from this tile
               (not= (= pos old-ark) (= pos new-ark))
               ;; Heading changed while ark is here
               (and (= pos new-ark) (not= old-hd new-hd)))))
       :reagent-render render-tile*}))
   :clj
   (def render-tile render-tile*))

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
         :transform (str "translate(" cx "," cy ")")
         :style {:pointer-events "none"}}
     [:polygon {:points       (hex-pts-str (- hex-size 1))
                :fill         "#05050E"
                :stroke       neutral-stroke
                :stroke-width 1.5
                :stroke-dasharray "4 3"
                :opacity      0.55}]
     [ark-ring]
     [ark-heading-arrow heading-dir]]))

;; ── Board ─────────────────────────────────────────────────────────────────────

(defn render-board
  [state player-order player-colors pos-highlights on-hex-click
   & [{:keys [fly-highlights chosen-pos active-player conv-groups conv-sundivers pending-convert]}]]
  (let [board       (:board state)
        ark-pos     (:ark state)
        heading-dir (game/heading-direction state)
        fly-hl      (or fly-highlights #{})
        chosen      chosen-pos
        conv-g      (or conv-groups {})
        conv-s      (or conv-sundivers #{})
        conv-targets (set (map first (keys conv-g)))
        pending     pending-convert]
    [:g
     ;; Tiles (backgrounds, world tokens, pieces)
     ;; cipher-dots is pre-computed here so render-tile doesn't depend on full state,
     ;; enabling shouldComponentUpdate to skip tiles whose data hasn't changed.
     (for [[pos tile] board
           :let [cipher-dots (cipher-match-dots state pos (:color tile))
                 is-fly?     (contains? fly-hl pos)
                 is-chosen?  (= pos chosen)
                 is-conv-s?  (contains? conv-s pos)
                 is-conv-t?  (contains? conv-targets pos)
                 hl (cond
                      is-chosen?                     :chosen
                      (contains? pos-highlights pos) :gold
                      :else                          nil)
                 ;; Highlight sundivers: fly-from, chosen, or convert-involved
                 shl (cond
                       is-fly?    {:highlight-player active-player :highlight-color "#FFD030"
                                   :on-sundiver-click (when on-hex-click #(on-hex-click [:fly pos]))}
                       is-chosen? {:highlight-player active-player :highlight-color "#30FF90"}
                       is-conv-s? {:highlight-player active-player :highlight-color "#FFD030"}
                       :else      nil)
                 ;; Hex click: launch/destination (not fly — fly is on the sundiver itself)
                 hex-click (when (or hl is-conv-t?) on-hex-click)
                 ;; Count cipher matches for landing highlight (only if center color active)
                 mc (if (and (:color tile)
                             (game/cipher-color-active? state [0 0] (:color tile)))
                      (game/count-cipher-matches state pos)
                      0)]]
       ^{:key (str pos)}
       [render-tile pos tile cipher-dots player-order player-colors ark-pos heading-dir
        hl
        (or hex-click (when (and is-fly? (not hl)) on-hex-click))
        shl
        mc])
     ;; Ghost hexes for highlighted positions that don't have tiles yet
     (for [pos pos-highlights
           :when (not (contains? board pos))
           :let [[px py] (hex->pixel pos)]]
       [:g {:key       (str "ghost" pos)
            :transform (str "translate(" px "," py ")")
            :on-click  (when on-hex-click #(on-hex-click pos))
            :style     {:cursor "pointer"}}
        [:polygon {:points       (hex-pts-str (- hex-size 1))
                   :fill         "#0A0A1A"
                   :stroke       "#FFD030"
                   :stroke-width 2.5}]
        [:polygon {:points       (hex-pts-str (- hex-size 1))
                   :fill         "none"
                   :stroke       "#FFD030"
                   :stroke-width 7
                   :opacity      0.15}]
        [:text {:x 0 :y 4
                :text-anchor "middle"
                :fill "#FFD03066" :font-size "10" :font-family "monospace"}
         "?"]])
     ;; Ghost station icons for each conversion option, clickable
     ;; When pending-convert is set, show sundiver arrangement options instead
     (if pending
       ;; Show each sundiver arrangement as clickable highlighted sundivers
       (let [{:keys [target options]} pending
             [px py] (hex->pixel target)]
         (for [[i {:keys [choice-key sundivers]}] (map-indexed vector options)]
           [:g {:key (str "conv-opt" i)
                :on-click (when on-hex-click #(on-hex-click choice-key))
                :style {:cursor "pointer"}}
            ;; Draw lines from each sundiver to the target to show the pattern
            (for [[j sv-pos] (map-indexed vector sundivers)
                  :let [[sx sy] (hex->pixel sv-pos)]]
              [:line {:key (str "line" j)
                      :x1 sx :y1 sy :x2 px :y2 py
                      :stroke (nth ["#FFD030" "#30C8FF" "#FF6090"] (mod i 3))
                      :stroke-width 2.5
                      :opacity 0.5
                      :stroke-dasharray "4,4"}])]))
       ;; Normal: show one ghost station per (target, type) group
       (let [by-target (group-by (fn [[[t _] _]] t) conv-g)]
         (for [[target entries] by-target
               :let [[px py] (hex->pixel target)
                     n (count entries)
                     spacing 28
                     x-start (- (* (dec n) spacing 0.5))]]
           [:g {:key (str "conv-group" target)}
            (for [[i [[_ stype] _opts]] (map-indexed vector entries)
                  :let [ox (+ x-start (* i spacing))
                        group-key [target stype]]]
              [:g {:key       (str "conv" i stype)
                   :transform (str "translate(" (+ px ox) "," (+ py 26) ")")
                   :on-click  (when on-hex-click #(on-hex-click group-key))
                   :style     {:cursor "pointer" :opacity 0.6}}
               [:circle {:cx 0 :cy 0 :r 16
                         :fill "none" :stroke "#FFD030" :stroke-width 2 :opacity 0.5}]
               [station-shape {:type stype :color-key (get player-colors active-player :sun) :level 0}]])])))
     ;; Gate ovals in a separate pass — always above all tile backgrounds
     [render-all-gates state player-order player-colors]
     ;; Floating ark on unexplored space — rendered last so it's always on top
     (when (and ark-pos (not (contains? board ark-pos)))
       [ark-ghost ark-pos heading-dir])]))

;; ── Cipher display ────────────────────────────────────────────────────────────

(def ^:const cipher-hex-size 26)

(defn cipher-hex [cipher pos highlight? on-click on-beacon-hover cipher-queue-color]
  (let [[cx cy]  (let [[q r] pos]
                   [(* cipher-hex-size 1.5 q)
                    (* cipher-hex-size sqrt3 (+ (* 0.5 q) r))])
        entry    (get cipher pos {})
        colors   (keep (fn [[c players]] (when (seq players) c)) (:colors entry))
        bg       "#0C0C1E"]
    [:g {:key      (str "c" pos)
         :transform (str "translate(" cx "," cy ")")
         :on-click (when on-click #(on-click pos))
         ;; During cipher placement, hovering a hex shows preview of where matches would appear
         :on-mouse-enter (when (and cipher-queue-color on-beacon-hover)
                           #(on-beacon-hover pos cipher-queue-color))
         :on-mouse-leave (when (and cipher-queue-color on-beacon-hover)
                           #(on-beacon-hover nil nil))
         :style    {:cursor (when (or on-click cipher-queue-color) "pointer")}}
     [:polygon {:points (hex-pts-str (- cipher-hex-size 1))
                :fill bg
                :stroke (if highlight? "#FFD030" "#2A2A4A")
                :stroke-width (if highlight? 2.5 1)}]
     (when highlight?
       [:polygon {:points (hex-pts-str (- cipher-hex-size 1))
                  :fill "none" :stroke "#FFD030" :stroke-width 5 :opacity 0.2}])
     ;; Color beacon dots — with hover support
     (let [n (count colors)]
       (for [[i c] (map-indexed vector colors)
             :let  [a   (if (= n 1) 0 (* i (/ tau n)))
                    r   (if (= n 1) 0 (* cipher-hex-size 0.38))
                    icf (get world-outer c "#555")]]
         [:circle {:key (str c i)
                   :cx (* r (Math/cos a))
                   :cy (* r (Math/sin a))
                   :r  (if (= n 1) (* cipher-hex-size 0.35) (* cipher-hex-size 0.22))
                   :fill icf
                   :style {:cursor "pointer"}
                   :on-mouse-enter (when on-beacon-hover #(on-beacon-hover pos c))
                   :on-mouse-leave (when on-beacon-hover #(on-beacon-hover nil nil))}]))
     (when (= pos [0 0])
       [:text {:x 0 :y 1
               :text-anchor "middle"
               :dominant-baseline "middle"
               :fill "#445" :font-size "7" :font-family "monospace"
               :style {:pointer-events "none"}}
        "C"])]))

(defn render-cipher [cipher & [{:keys [highlights on-click on-beacon-hover expanded? cipher-queue-color]}]]
  (let [s (if expanded? 2.0 1.0)]
    [:g {:transform (when expanded? (str "scale(" s ")"))}
     (for [pos (into [[0 0]] game/hex-directions)
           :let [hl? (and highlights (contains? highlights pos))]]
       [cipher-hex cipher pos hl? (when hl? on-click) on-beacon-hover cipher-queue-color])]))

;; ── Scoring overlay (game-over landing) ──────────────────────────────────────

(defn render-scoring-overlay
  "After a landing game-over, render small player-colored dots on each tile
   that contributed to the score, showing whose beacons counted there."
  [state player-colors]
  (let [landing-pos (get-in state [:game-over :tile])
        board       (:board state)
        cipher      (:cipher state)
        tile-color  (get-in board [landing-pos :color])
        ;; Build map {tile-pos -> [player-key ...]} for all scoring tiles
        center-players (keys (get-in cipher [[0 0] :colors tile-color] {}))
        scored
        (reduce
         (fn [acc dir]
           (let [neighbor (game/add-hex landing-pos dir)
                 n-color  (get-in board [neighbor :color])]
             (if (and n-color (game/cipher-color-active? state dir n-color))
               (let [ps (keys (get-in cipher [dir :colors n-color] {}))]
                 (if (seq ps) (assoc acc neighbor (vec ps)) acc))
               acc)))
         (if (seq center-players) {landing-pos (vec center-players)} {})
         game/hex-directions)]
    [:g
     (for [[pos players] scored
           :let [[px py] (hex->pixel pos)
                 n       (count players)]]
       [:g {:key       (str "score" pos)
            :transform (str "translate(" px "," py ")")}
        ;; Faint ring to mark the tile as a scoring tile
        [:polygon {:points (hex-pts-str (- hex-size 2))
                   :fill "none" :stroke "#FFFFFF" :stroke-width 1.5 :opacity 0.25}]
        ;; One dot per scoring player, arranged in a small ring
        (for [[i player] (map-indexed vector players)
              :let [a  (if (= n 1) (/ Math/PI -2) (+ (/ Math/PI -2) (* i (/ tau n))))
                    r  (if (= n 1) 0 (* hex-size 0.28))
                    fc (ptb (get player-colors player :sun))]]
          [:circle {:key    player
                    :cx     (* r (Math/cos a))
                    :cy     (* r (Math/sin a))
                    :r      (* hex-size 0.10)
                    :fill   fc
                    :stroke "#000" :stroke-width 0.8
                    :opacity 0.95}])])]))

;; ── Suit icons ──────────────────────────────────────────────────────────────
;; Five distinct icons forming a visual family: all geometric, similar weight.
;; Centered at origin; `s` scales the radius.

(defn suit-icon
  "SVG group for a card suit icon centered at origin. s = scale (radius)."
  [suit fill s]
  (case (int suit)
    ;; 0 = Space (purple): ringed planet — circle + tilted ring
    0 [:g
       [:circle {:cx 0 :cy 0 :r (* s 0.45) :fill fill}]
       [:ellipse {:cx 0 :cy 0 :rx (* s 0.9) :ry (* s 0.28)
                  :fill "none" :stroke fill :stroke-width (* s 0.16)
                  :transform "rotate(-25)"}]]
    ;; 1 = Matter (green): pulsating — concentric rings radiating outward
    1 [:g
       [:circle {:cx 0 :cy 0 :r (* s 0.3) :fill fill}]
       [:circle {:cx 0 :cy 0 :r (* s 0.55) :fill "none" :stroke fill :stroke-width (* s 0.12)}]
       [:circle {:cx 0 :cy 0 :r (* s 0.85) :fill "none" :stroke fill :stroke-width (* s 0.08)}]]
    ;; 2 = Time (silver): spiral
    2 (let [points (for [i (range 0 (* 2.5 Math/PI) 0.15)
                         :let [r (* s 0.1 (+ 1 (* 2.2 (/ i (* 2.5 Math/PI)))))
                               x (* r (Math/cos i))
                               y (* r (Math/sin i))]]
                     (str x "," y))]
        [:polyline {:points (str/join " " points)
                    :fill "none" :stroke fill :stroke-width (* s 0.16)
                    :stroke-linecap "round"}])
    ;; 3 = Dyad (blue): two circles on ends of a pole
    3 (let [r (* s 0.25) d (* s 0.6)]
        [:g
         [:line {:x1 (- d) :y1 0 :x2 d :y2 0
                 :stroke fill :stroke-width (* s 0.14)}]
         [:circle {:cx (- d) :cy 0 :r r :fill fill}]
         [:circle {:cx d :cy 0 :r r :fill fill}]])
    ;; 4 = Flare (red): 4-pointed starburst
    4 (let [o (* s 0.9) i (* s 0.28)]
        [:polygon {:points (str "0," (- o) " " i "," (- i)
                            " " o ",0 " i "," i
                            " 0," o " " (- i) "," i
                            " " (- o) ",0 " (- i) "," (- i))
                   :fill fill}])
    ;; Fallback
    [:circle {:cx 0 :cy 0 :r (* s 0.5) :fill fill}]))

;; ── Player area ───────────────────────────────────────────────────────────────

(defn render-player-area
  [state player-key player-colors captain y-offset _panel-w]
  (let [pstate       (get-in state [:players player-key] {})
        hab          (get-in pstate [:habitat :sundivers] 0)
        res          (:reserve pstate {})
        ck           (get player-colors player-key :sun)
        fc           (pf ck)
        sc           (ps ck)
        is-cap       (= player-key captain)
        on-turn?     (= player-key (game/current-player state))
        player-order (:turn-order state)
        n-players    (count player-order)
        pidx         (.indexOf (vec player-order) player-key)
        sundiver-rot (+ 90 (* pidx (/ 360.0 n-players)))
        cx           panel-r
        cy           panel-r]
    [:g {:key (str "p" player-key)
         :transform (str "translate(0," y-offset ")")}
     ;; Circle backing
     [:circle {:cx cx :cy cy :r panel-r
               :fill (if on-turn? "#161222" "#080814")
               :stroke (if on-turn? fc "#2A2A44")
               :stroke-width (if on-turn? 3 1.5)}]
     ;; Captain flame (top-right quadrant)
     (when is-cap
       [:g {:transform (str "translate(" (+ cx 50) "," (- cy 52) ")")}
        [captain-flame]])
     ;; Player name
     [:text {:x cx :y (- cy 52)
             :text-anchor "middle"
             :fill fc :font-size "12" :font-weight "bold" :font-family "monospace"}
      player-key]
     ;; Habitat sundivers — up to 7 per row, rotated to match board direction
     (let [rot  sundiver-rot
           row1 (min hab 7)
           row2 (max 0 (- hab 7))
           row-x (fn [n i] (- (* i 10) (* (dec (max n 1)) 5)))]
       [:g {:transform (str "translate(" cx "," (- cy 34) ")")}
        ;; First row
        (for [i (range row1)
              :let [ox (row-x row1 i)]]
          [:g {:key i :transform (str "translate(" ox ",0) rotate(" rot ")")}
           [:polygon {:points (tri-pts 0 0 7)
                      :fill fc :stroke sc :stroke-width 0.7}]])
        ;; Second row (when hab > 7)
        (for [i (range row2)
              :let [ox (row-x row2 i)]]
          [:g {:key (+ 7 i) :transform (str "translate(" ox ",12) rotate(" rot ")")}
           [:polygon {:points (tri-pts 0 0 7)
                      :fill fc :stroke sc :stroke-width 0.7}]])])
     ;; Move points
     [:text {:x cx :y (- cy 4)
             :text-anchor "middle"
             :fill "#88AACC" :font-size "9" :font-family "monospace"}
      (str "move " (game/move-points state player-key))]
     ;; Reserve counts — icon + number, 3 columns × 2 rows
     (let [items [;; Row 1: sundivers, beacons, gates
                  [{:icon :sundiver :n (:sundivers res 0)}
                   {:icon :beacon   :n (:beacons res 0)}
                   {:icon :gate     :n (:gates res 0)}]
                  ;; Row 2: foundries, matrixes, towers
                  [{:icon :foundry  :n (:foundries res 0)}
                   {:icon :matrix   :n (:matrixes res 0)}
                   {:icon :tower    :n (:towers res 0)}]]
           col-w 36
           x-start (- cx (* col-w 1))]
       [:g {:transform (str "translate(0," (+ cy 12) ")")}
        (for [[row-i row] (map-indexed vector items)
              [col-i {:keys [icon n]}] (map-indexed vector row)
              :let [x (+ x-start (* col-i col-w))
                    y (* row-i 22)]]
          [:g {:key (str icon) :transform (str "translate(" x "," y ")")}
           ;; Icon
           (case icon
             :sundiver [:g {:transform (str "rotate(" sundiver-rot ")")}
                        [:polygon {:points (tri-pts 0 0 5) :fill fc :stroke sc :stroke-width 0.5}]]
             :beacon   [:g {:transform "scale(0.6)"} [beacon-shape fc]]
             :gate     [:ellipse {:cx 0 :cy 0 :rx 5 :ry 3
                                  :fill "none" :stroke fc :stroke-width 1.2}]
             :foundry  [:g {:transform "scale(0.5)"}
                         [station-shape {:type :foundry :color-key ck :level 0}]]
             :matrix   [:g {:transform "scale(0.5)"}
                         [station-shape {:type :matrix :color-key ck :level 0}]]
             :tower    [:g {:transform "scale(0.5)"}
                         [station-shape {:type :tower :color-key ck :level 0}]])
           ;; Number
           [:text {:x 9 :y 4
                   :fill "#6A8A6A" :font-size "9" :font-family "monospace"}
            (str n)]])])
     ;; Held card — small playing card with suit color and icon
     (when-let [card (get-in pstate [:held-card])]
       (let [suit   (:suit card)
             scolor (get game/suit-colors suit "#555")
             cw 22 ch 30
             cx0 (- cx (/ cw 2)) cy0 (+ cy 52)]
         [:g
          [:rect {:x cx0 :y cy0 :width cw :height ch :rx 3
                  :fill scolor :stroke "#FFFFFF33" :stroke-width 0.8}]
          [:g {:transform (str "translate(" cx "," (+ cy0 15) ")")}
           [suit-icon suit "#FFF" 7]]]))]))

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
   & [{:keys [pan-x pan-y zoom on-bg-mouse-down fly-highlights chosen-pos active-player
              conv-groups conv-sundivers pending-convert cipher-highlights cipher-on-click
              cipher-on-beacon-hover cipher-expanded? cipher-on-toggle cipher-hover
              cipher-queue-color]
       :or   {pan-x 0 pan-y 0 zoom 1.0}}]]
  (let [player-order  (:turn-order state)
        player-colors (build-player-colors player-order)
        captain       (:captain-flame state)
        current       (game/current-player state)
        [bcx bcy]     (board-centroid (:board state))
        ;; Compute cipher hover match highlights
        board         (:board state)
        cipher-match-hl
        (when cipher-hover
          (let [{:keys [pos color]} cipher-hover]
            (if (= pos [0 0])
              (set (filter #(= color (:color (get board %))) (keys board)))
              (set (filter #(= color (:color (get board (game/add-hex % pos))))
                           (keys board))))))
        pos-highlights (into (or pos-highlights #{}) (or cipher-match-hl #{}))]
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
      [render-board state player-order player-colors pos-highlights on-hex-click
       {:fly-highlights fly-highlights :chosen-pos chosen-pos :active-player active-player
        :conv-groups conv-groups :conv-sundivers conv-sundivers :pending-convert pending-convert}]
      (when (= :landing (get-in state [:game-over :type]))
        [render-scoring-overlay state player-colors])]

     ;; ── Cipher (fixed position, not panned/zoomed) — click to expand/collapse
     [:g {:transform (str "translate(" cipher-x "," cipher-y ")")
          :on-click (when cipher-on-toggle cipher-on-toggle)
          :style {:cursor "pointer"}}
      [:text {:x 0 :y (if cipher-expanded? -140 -78)
              :text-anchor "middle"
              :fill "#445566"
              :font-size "10" :font-family "monospace" :letter-spacing "2"}
       "CIPHER"]
      [render-cipher (:cipher state) {:highlights cipher-highlights :on-click cipher-on-click
                                       :on-beacon-hover cipher-on-beacon-hover
                                       :expanded? cipher-expanded?
                                       :cipher-queue-color cipher-queue-color}]
      ;; Cipher queue: show pending beacons to place
      (let [queue (get-in state [:player-turn :cipher-queue] [])]
        (when (seq queue)
          [:g {:transform "translate(0,110)"}
           [:text {:x 0 :y -16
                   :text-anchor "middle"
                   :fill "#445566"
                   :font-size "8" :font-family "monospace" :letter-spacing "1"}
            "QUEUE"]
           ;; Arrow pointing right at the first item, from its left side
           (let [first-x (* (- 0 (/ (dec (count queue)) 2.0)) 46)]
             [:path {:d "M 0,-4 L 8,0 L 0,4 Z"
                     :fill "#FFD030" :opacity 0.8
                     :transform (str "translate(" (- first-x 28) ",7)")}])
           (for [[i {:keys [player color]}] (map-indexed vector queue)
                 :let [x (* (- i (/ (dec (count queue)) 2.0)) 46)
                       wc (get world-outer color "#555")
                       ic (get world-inner color "#333")
                       pc (pf (get player-colors player :sun))
                       first? (zero? i)]]
             [:g {:key i :transform (str "translate(" x ",8)")}
              ;; World tile circle (color of the discovered world) — large so color is clear
              [:circle {:cx 0 :cy 0 :r 18
                        :fill wc :stroke (if first? "#FFD030" "#333") :stroke-width (if first? 2.5 1)}]
              [:circle {:cx 0 :cy 0 :r 12 :fill ic}]
              ;; Player beacon pentagon on top
              [:g {:transform "translate(0,-1)"}
               [beacon-shape pc]]])]))]

     ;; ── Player panels (left side, wraps to second column if needed)
     (let [max-h    (- vh 20)
           col-step (+ panel-h panel-gap)
           per-col  (max 1 (int (/ max-h col-step)))]
       [:g {:transform (str "translate(" panel-x ",0)")}
        (map-indexed
         (fn [i player]
           (let [col (quot i per-col)
                 row (mod i per-col)
                 px  (* col (+ panel-w 12))
                 py  (+ 10 (* row col-step))]
             ^{:key player}
             [:g {:transform (str "translate(" px ",0)")}
              [render-player-area state player player-colors captain py panel-w]]))
         player-order)])

     ;; ── Game info (below last column of player panels)
     (let [max-h    (- vh 20)
           col-step (+ panel-h panel-gap)
           per-col  (max 1 (int (/ max-h col-step)))
           last-col-count (let [n (count player-order)] (mod n per-col))
           rows-in-last   (if (zero? last-col-count) per-col last-col-count)
           info-y  (+ 10 (* rows-in-last col-step))]
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
         (str "Round " (:round state 0))]])

     ;; ── Deck + flares (bottom-right corner)
     (let [deck-n  (count (:deck state))
           discard (:discard state)
           top     (peek discard)
           flares  (:flares-drawn state 0)]
       [:g {:transform (str "translate(" (- vw 180) "," (- vh 70) ")")}
        ;; Flare icon + count
        [:g {:transform "translate(0,0)"}
         [:g {:transform "scale(1.8)"}
          [suit-icon 4 "#DD3322" 7]]
         [:text {:x 20 :y 6
                 :fill "#DD3322" :font-size "16" :font-weight "bold" :font-family "monospace"}
          (str flares "/13")]]
        ;; Deck stack
        [:g {:transform "translate(100,0)"}
         (for [i (range (min 3 (max 1 (quot deck-n 10))))]
           [:rect {:key i :x (- i) :y (- 17 i) :width 28 :height 40 :rx 3
                   :fill "#1A1A30" :stroke "#3A3A5A" :stroke-width 0.8}])
         [:text {:x 14 :y 8
                 :text-anchor "middle"
                 :fill "#667788" :font-size "12" :font-weight "bold" :font-family "monospace"}
          (str deck-n)]]
        ;; Top of discard
        (when top
          [:g {:transform "translate(138,0)"}
           [:rect {:x 0 :y -17 :width 28 :height 40 :rx 3
                   :fill (get game/suit-colors (:suit top) "#555")
                   :stroke "#FFFFFF33" :stroke-width 0.8}]
           [:g {:transform "translate(14,6)"}
            [suit-icon (:suit top) "#FFF" 7]]])])

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
         choice-buttons)])

     ;; ── Score banner (game-over landing)
     (when-let [go (:game-over state)]
       (when (= :landing (:type go))
         (let [scores  (:scores go)
               winners (:winners go)
               tile    (:tile go)
               n       (count player-order)
               bw      (* n 130)
               bx      (/ (- vw bw) 2)]
           [:g {:transform (str "translate(" bx "," (- vh 50) ")")}
            [:rect {:x -10 :y -18 :width (+ bw 20) :height 46
                    :fill "#0A0E1C" :stroke "#2A4A80" :stroke-width 1 :rx 6
                    :opacity 0.92}]
            [:text {:x (/ bw 2) :y -4
                    :text-anchor "middle"
                    :fill "#667788" :font-size "9" :font-family "monospace"
                    :letter-spacing "1.5"}
             (str "LANDED AT " (pr-str tile))]
            (map-indexed
             (fn [i player]
               (let [ck (get player-colors player :sun)
                     fc (ptb ck)
                     sc (get scores player 0)
                     win? (some #{player} winners)]
                 [:g {:key player :transform (str "translate(" (* i 130) ",0)")}
                  [:text {:x 65 :y 14
                          :text-anchor "middle"
                          :fill fc :font-size "13" :font-weight (if win? "bold" "normal")
                          :font-family "monospace"}
                   (str player ": " sc (when win? " ★"))]]))
             player-order)])))]))
