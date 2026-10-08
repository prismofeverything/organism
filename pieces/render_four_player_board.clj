(set! *warn-on-reflection* true)
(require '[organism.board :as board] '[organism.game :as game] '[hiccup.core :as up])

(def out-dir (first *command-line-args*))
(def symmetry 4)
(def rings ["A" "B" "C" "D" "E" "F" "G"])
(def players ["orb" "mass" "brone" "laam"])
;; ring palette sampled from organism-five-player.png
(def classic
  {:colors [["A" "#ebe0e4"] ["B" "#b2b1e2"] ["C" "#be97b3"] ["D" "#57adc7"]
            ["E" "#969f41"] ["F" "#5e7236"] ["G" "#531842"]]
   :discs {}
   :players {"orb" "#5ec8ea" "mass" "#e0b8d4" "brone" "#b4be50" "laam" "#7a9450"}})

;; warm core cooling to a deep sea edge; each ring's disc is a darker shade of
;; its spaces so the spaces stand off their own band (classic's outer ring
;; vanishes into its disc), and neighboring rings step in both hue and value
(def tide
  {:colors [["A" "#fff4dc"] ["B" "#ffc857"] ["C" "#f4845f"] ["D" "#d1477a"]
            ["E" "#8a4fbf"] ["F" "#3d7cc9"] ["G" "#1fa39a"]]
   :discs {"B" "#7a5a12" "C" "#6e2f1c" "D" "#5e1736" "E" "#38194f"
           "F" "#142f52" "G" "#0a3d3a"}
   :players {"orb" "#e8f7ff" "mass" "#ffe066" "brone" "#ff9ec4" "laam" "#b9f27c"}})

(def relax-params
  {:iterations 4000   ; spring steps, then `:settle` spacing-only passes
   :settle 300
   :step 0.05
   :apart 2.2         ; non-adjacent pairs pushed out to this many spacings
   :ring-pull 3.0     ; pull toward each ring's mean radius, so rings read round
   :min 1.0})         ; hard floor: no two spaces closer than one spacing

(defn relax
  "Afterpass on the space positions. The square rings put a corner's
   same-ring neighbors ~1.41 spacings away, as far as spaces it does not
   touch, so rings read as concentric squares. Pull adjacent spaces to one
   spacing, push non-adjacent ones apart, round each ring toward a circle,
   and keep the board's 4-fold rotational symmetry exact every step."
  [locations adjacencies center]
  (let [{:keys [iterations settle step apart ring-pull min]} relax-params
        names (vec (sort (keys adjacencies)))
        n (int (count names))
        idx (zipmap names (range))
        [cx cy] (take 2 (get locations center))
        ^doubles xs (double-array (map #(- (first (get locations %)) cx) names))
        ^doubles ys (double-array (map #(- (second (get locations %)) cy) names))
        c (int (idx center))
        spacing (Math/hypot (aget xs (idx ["B" 0])) (aget ys (idx ["B" 0])))
        ^booleans adj (let [a (boolean-array (* n n))]
                        (doseq [[s ns] adjacencies t ns]
                          (aset a (+ (* n (idx s)) (idx t)) true))
                        a)
        ^ints level (int-array (map #(- (int (first (first %))) 65) names))
        ;; the space a quarter turn on: ring k, space i -> i+k
        ^ints nxt (int-array (map (fn [[color i]]
                              (let [k (- (int (first color)) 65)]
                                (if (zero? k) c
                                    (get idx [color (mod (+ i k) (* symmetry k))] (idx [color i])))))
                            names))
        ;; which way a quarter turn goes in screen space
        sgn (let [b0 (idx ["B" 0]) b1 (aget nxt b0)]
              (if (pos? (- (* (aget xs b0) (aget ys b1)) (* (aget ys b0) (aget xs b1)))) 1.0 -1.0))
        ^doubles fx (double-array n) ^doubles fy (double-array n)
        symmetrize!
        (fn []
          (let [^doubles ax (double-array n) ^doubles ay (double-array n)]
            (dotimes [i n]
              (loop [j i r 0 x (aget xs i) y (aget ys i)]
                (if (< r 4)
                  (do (aset ax i (+ (aget ax i) x)) (aset ay i (+ (aget ay i) y))
                      ;; next orbit member, rotated back a quarter turn
                      (let [j' (aget nxt j) rot (inc r)
                            [x' y'] (loop [x (aget xs j') y (aget ys j') t rot]
                                      (if (zero? t) [x y] (recur (* sgn y) (- (* sgn x)) (dec t))))]
                        (recur j' rot x' y')))
                  nil)))
            (dotimes [i n]
              (aset xs i (/ (aget ax i) 4)) (aset ys i (/ (aget ay i) 4)))))
        ;; mirror across the axis through B0: ring k, space i -> -i
        ^ints mirror (int-array (map (fn [[color i]]
                                       (let [k (- (int (first color)) 65)]
                                         (if (zero? k) c
                                             (get idx [color (mod (- i) (* symmetry k))] (idx [color i])))))
                                     names))
        [ux uy] (let [b0 (idx ["B" 0]) d (Math/hypot (aget xs b0) (aget ys b0))]
                  [(/ (aget xs b0) d) (/ (aget ys b0) d)])
        mirror!
        (fn []
          (let [^doubles mx (double-array n) ^doubles my (double-array n)]
            (dotimes [i n]
              (let [j (aget mirror i) x (aget xs j) y (aget ys j)
                    dot (+ (* x ux) (* y uy))]
                (aset mx i (/ (+ (aget xs i) (- (* 2 dot ux) x)) 2))
                (aset my i (/ (+ (aget ys i) (- (* 2 dot uy) y)) 2))))
            (System/arraycopy mx 0 xs 0 n) (System/arraycopy my 0 ys 0 n)))
        ;; every pair moves at once (Jacobi), so update order can't skew the layout
        push-apart!
        (fn []
          (let [^doubles mx (double-array n) ^doubles my (double-array n)]
            (dotimes [i n]
              (dotimes [j n]
                (when (not= i j)
                  (let [dx (- (aget xs i) (aget xs j)) dy (- (aget ys i) (aget ys j))
                        d (Math/hypot dx dy) short (- (* min spacing) d)]
                    (when (pos? short)
                      (let [m (/ (* 0.25 short) d)]
                        (aset mx i (+ (aget mx i) (* m dx))) (aset my i (+ (aget my i) (* m dy)))))))))
            (dotimes [i n]
              (aset xs i (+ (aget xs i) (aget mx i))) (aset ys i (+ (aget ys i) (aget my i)))))
          (aset xs c 0.0) (aset ys c 0.0))]
    (dotimes [it (+ iterations settle)]
      (when (< it iterations)
        (java.util.Arrays/fill fx 0.0) (java.util.Arrays/fill fy 0.0)
        (dotimes [i n]
          (dotimes [j n]
            (when (not= i j)
              (let [dx (- (aget xs i) (aget xs j)) dy (- (aget ys i) (aget ys j))
                    d (Math/hypot dx dy)
                    f (if (aget adj (+ (* n i) j))
                        (- d spacing)
                        (clojure.core/min 0.0 (- d (* apart spacing))))]
                (aset fx i (- (aget fx i) (/ (* f dx) d)))
                (aset fy i (- (aget fy i) (/ (* f dy) d)))))))
        (let [radii (group-by first (map (fn [i] [(aget level i) (Math/hypot (aget xs i) (aget ys i))]) (range n)))
              mean (into {} (map (fn [[k rs]] [k (/ (reduce + (map second rs)) (count rs))]) radii))]
          (dotimes [i n]
            (when-not (= i c)
              (let [r (Math/hypot (aget xs i) (aget ys i))
                    pull (/ (* ring-pull (- r (mean (aget level i)))) r)]
                (aset fx i (- (aget fx i) (* pull (aget xs i))))
                (aset fy i (- (aget fy i) (* pull (aget ys i))))))))
        (dotimes [i n]
          (when-not (= i c)
            (aset xs i (+ (aget xs i) (* step (aget fx i))))
            (aset ys i (+ (aget ys i) (* step (aget fy i)))))))
      (dotimes [_ 3] (push-apart!))
      (symmetrize!)
      (mirror!))
    {:spacing spacing
     :positions (into {} (map (fn [s] [s [(aget xs (idx s)) (aget ys (idx s))]]) names))}))

(def oval-gap
  "Space left between an oval and its same-ring neighbors, in space radii."
  0.15)

(defn oval
  "A space drawn as an oval stretched along its ring, toward its same-ring
   neighbors, so the ring reads as one connected band. Rings have fewer spaces
   than a hex board, so same-ring neighbors sit farther apart than neighbors
   across rings; without the stretch they look unconnected. A space beside a
   notched corner has only one same-ring neighbor and stretches toward it; its
   length is capped by the nearest same-ring space either way (across the
   notch included), so it never runs into one."
  [radius [x y] neighbors ring color]
  (if (empty? neighbors)
    (board/circle [x y radius color])
    (let [dist (fn [[px py]] (Math/hypot (- px x) (- py y)))
          nearest (apply min (map dist (remove #{[x y]} ring)))
          reach (- (/ nearest 2) (* oval-gap radius))
          [[ax ay] [bx by]] (if (= 1 (count neighbors))
                              [[x y] (first neighbors)]
                              neighbors)
          angle (Math/toDegrees (Math/atan2 (- by ay) (- bx ax)))]
      [:ellipse {:cx x :cy y :rx (max reach radius) :ry radius :fill color
                 :transform (str "rotate(" angle " " x " " y ")")}])))

(def relax-once (memoize relax))

(defn relaxed-board
  "Rebuild the board's layout around relaxed positions: oval spaces, and one
   gradient disc per ring sized to that ring's new radius."
  [b g disc-colors]
  (let [{:keys [radius buffer colors]} b
        field (* radius buffer (count colors))
        {:keys [spacing positions]} (relax-once (:locations b) (:adjacencies g) ["A" 0])
        ;; turn the board so its four axes run straight up/down and across
        [bx by] (get positions ["B" 0])
        t (- (/ Math/PI -2) (Math/atan2 by bx))
        positions (into {} (map (fn [[s [x y]]]
                                  [s [(- (* x (Math/cos t)) (* y (Math/sin t)))
                                      (+ (* x (Math/sin t)) (* y (Math/cos t)))]])
                                positions))
        color-of (into {} colors)
        ring-r (into {} (map (fn [[k ps]] [k (/ (reduce + (map (fn [[_ p]] (Math/hypot (first p) (second p))) ps)) (count ps))])
                             (group-by (comp first first) positions)))
        outer (apply max (map (fn [[_ [x y]]] (Math/hypot x y)) positions))
        ;; fit the outer ring where the square board's corners reached
        scale (/ (- field radius (* 0.9 radius buffer)) (+ outer radius))
        at (fn [[x y]] [(+ field (* scale x)) (+ field (* scale y))])
        locations (into {} (map (fn [[s p]] [s (at p)]) positions))
        spaces (for [[s p] locations]
                 (oval radius p
                       (map locations (filter #(= (first %) (first s)) (get-in g [:adjacencies s])))
                       (keep (fn [[t q]] (when (= (first t) (first s)) q)) locations)
                       (color-of (first s))))
        discs (for [[k _] (reverse (rest colors))]
                (board/circle [field field (* scale (+ (ring-r k) (* 0.55 spacing))) (str "url(#" k ")")]))
        background [:g
                    (into [:defs]
                          (for [[k col] (rest colors)]
                            [:radialGradient {:id k}
                             [:stop {:offset "0%" :stop-color "black"}]
                             [:stop {:offset "100%" :stop-color (get disc-colors k col)}]]))
                    (into [:g (board/make-circle (* field 0.93) "#111" [field field])] discs)]]
    (println "relaxed: spacing" spacing "scale" scale)
    (assoc b
           :locations locations
           :field field
           :layout (into [:svg {:width (* 2 field) :height (* 2 field)} background] spaces))))

(defn lattice
  "Real-size overlay: scale the board so the closest two space centers are
   exactly `mm` apart, so a `mm` circle on every space just touches its
   nearest neighbor, and report the board diameter that takes."
  [svg b mm]
  (let [{:keys [locations field]} b
        pts (vals locations)
        nearest (apply min (for [[ax ay] pts [bx by] pts :when (not= [ax ay] [bx by])]
                             (Math/hypot (- ax bx) (- ay by))))
        per-mm (/ nearest mm)
        ;; reach is in px: farthest space center plus one circle radius
        reach (+ (apply max (map (fn [[x y]] (Math/hypot (- x field) (- y field))) pts))
                 (* per-mm (/ mm 2)))
        diameter-mm (/ (* 2 reach) per-mm)
        side (* 2 field)
        text (fn [y size s] [:text {:x field :y y :text-anchor "middle" :font-family "sans-serif"
                                    :font-size (* per-mm size) :fill "#222"} s])]
    (println (format "lattice: %.0f mm circles, %.1f mm board diameter (%.2f in), %.1f mm canvas"
                     (double mm) diameter-mm (/ diameter-mm 25.4) (/ side per-mm)))
    (-> svg
        (update 1 assoc
                :width (format "%.2fmm" (/ side per-mm)) :height (format "%.2fmm" (/ (+ side (* per-mm 30)) per-mm))
                :viewBox (format "0 0 %s %s" side (+ side (* per-mm 30))))
        (conj (into [:g {:fill "none" :stroke "#fff" :stroke-width (* per-mm 0.6)}]
                    (for [[x y] pts] [:circle {:cx x :cy y :r (* per-mm (/ mm 2))}]))
              [:circle {:cx field :cy field :r reach :fill "none" :stroke "#fff"
                        :stroke-width (* per-mm 0.8) :stroke-dasharray (str (* per-mm 4) " " (* per-mm 3))}]
              (text (+ side (* per-mm 12)) 6
                    (format "%d spaces · %.0f mm circles, nearest centers %.0f mm apart"
                            (count pts) (double mm) (double mm)))
              (text (+ side (* per-mm 22)) 6
                    (format "dashed circle: smallest board holding every circle, %.0f mm (%.1f in) across"
                            diameter-mm (/ diameter-mm 25.4)))))))

(defn render [notches? palette file & {:keys [lattice-mm]}]
  (let [radius (board/ring-radius (count rings))
        buffer 2.1
        b (board/build-board symmetry radius buffer (:colors palette) rings players notches?)
        b (assoc b :player-colors (:players palette))
        ;; center each organism on its side: a side is the corner plus five
        ;; spaces, so the middle three are 2-4 past each corner
        side (dec (count rings))
        starting (map-indexed
                  (fn [i player]
                    [player (mapv (fn [k] [(last rings) (+ (* i side) (quot side 2) -1 k)])
                                  (range 3))])
                  players)
        info (game/initial-players starting (repeat 4 5))
        g (game/create-game symmetry rings info 3 notches? {})
        ;; give each starting organism one of each element, like a fresh board
        g (reduce
           (fn [g [player spaces]]
             (reduce (fn [g [space type]]
                       (assoc-in g [:state :elements space]
                                 (game/->Element player 0 type space 1 [])))
                     g (map vector spaces [:eat :grow :move])))
           g starting)
        b (relaxed-board b g (:discs palette))
        svg (board/render-game b g)
        svg (update svg 1 assoc :xmlns "http://www.w3.org/2000/svg"
                    :style "background:#fff")
        svg (if lattice-mm (lattice svg b lattice-mm) svg)]
    (println file "spaces:" (count (:adjacencies g))
             "center adj:" (get (:adjacencies g) ["A" 0]))
    (spit (str out-dir "/" file) (up/html svg))))

(render true classic "organism-four-player.svg")
(render true tide "organism-four-player-37mm.svg" :lattice-mm 37)
