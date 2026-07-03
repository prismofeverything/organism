(ns future.board
  "Topology and rendering geometry for FUTURE.

   The playing surface has two kinds of space:

   * 5 sun wedges       [:sun k]          k in 0..4
   * 5 orbital rings    [:orbit color i]  i in 0..(size-1)
     with Fibonacci sizes silver=5, green=8, blue=13, purple=21, void=34
     for 81 orbital spaces total plus 5 sun wedges → 86 spaces.

   Indexing convention
   -------------------
   * Index 0 of every orbital ring lies on the BEAM (north).
   * Indices increase clockwise in screen-space.
   * Front  = counter-clockwise = decreasing index.
   * Sun wedge 0 is at the top (on beam) and is the SILVER wedge.
     Going CCW from wedge 0 the colors are silver → green → blue → purple → void,
     so the wedge → color map in CW (index) order is
       0 silver, 1 void, 2 purple, 3 blue, 4 green.

   Adjacency
   ---------
   * Within a ring: i ↔ i±1 (mod n)
   * Between adjacent rings: by angular overlap
   * Between a sun wedge and silver: by angular overlap (1:1 since both have 5)
   * Sun wedges are NOT adjacent to each other on the board — you move between
     them only by re-entering from silver.")

;; ── Orbits ──────────────────────────────────────────────────────────────────

(def orbits
  "Inner-to-outer."
  [:silver :green :blue :purple :void])

(def orbit-sizes
  {:silver  5
   :green   8
   :blue   13
   :purple 21
   :void   34})

(def planet-colors orbits)

(def num-wedges 5)

;; ── Colors (rendering) ─────────────────────────────────────────────────────

(def orbit-colors
  {:silver "#bfbfbf"
   :green  "#3fbf4f"
   :blue   "#3f7fdf"
   :purple "#9f3fdf"
   :void   "#1f1f1f"})

(def planet-fill orbit-colors)

(def sun-outer-color "#cf2222")
(def beam-color      "#ffe066")
(def flame-color     "#ff8844")

;; ── Wedges ─────────────────────────────────────────────────────────────────
;; CW (visual / index) order — derived from the rule that placement is CCW.

(def wedge-color
  "Wedge index → color of its inner triangle."
  {0 :silver, 1 :void, 2 :purple, 3 :blue, 4 :green})

(def color->wedge
  (into {} (map (fn [[k v]] [v k]) wedge-color)))

(def wedge-placement-order
  "Order in which the flame-holder and subsequent players seed components on
   the sun (silver first, then CCW around)."
  [:silver :green :blue :purple :void])

;; ── Space identifiers ──────────────────────────────────────────────────────

(defn orbit-space [orbit i] [:orbit orbit i])
(defn sun-space   [k]       [:sun k])

(defn space-type [sid] (first sid))
(defn sun?       [sid] (= :sun   (space-type sid)))
(defn orbital?   [sid] (= :orbit (space-type sid)))

(defn orbit-of    [sid] (when (orbital? sid) (second sid)))
(defn space-index [sid] (when (orbital? sid) (nth sid 2)))
(defn wedge-of    [sid] (when (sun? sid) (second sid)))
(defn space-color [sid]
  (cond
    (sun?     sid) (wedge-color (wedge-of sid))
    (orbital? sid) (orbit-of sid)))

(defn orbit-spaces [orbit]
  (mapv #(orbit-space orbit %) (range (orbit-sizes orbit))))

(defn sun-spaces []
  (mapv sun-space (range num-wedges)))

(defn all-orbital-spaces []
  (vec (mapcat orbit-spaces orbits)))

(defn all-spaces []
  (vec (concat (sun-spaces) (all-orbital-spaces))))

;; ── Direction (front = CCW = decreasing index) ─────────────────────────────

(defn front-space [sid]
  (cond
    (orbital? sid)
    (let [[_ o i] sid n (orbit-sizes o)]
      [:orbit o (mod (dec i) n)])

    (sun? sid)
    (let [[_ k] sid]
      [:sun (mod (dec k) num-wedges)])))

(defn back-space [sid]
  (cond
    (orbital? sid)
    (let [[_ o i] sid n (orbit-sizes o)]
      [:orbit o (mod (inc i) n)])

    (sun? sid)
    (let [[_ k] sid]
      [:sun (mod (inc k) num-wedges)])))

;; ── Beam ───────────────────────────────────────────────────────────────────

(defn beam-space-for-orbit [orbit] (orbit-space orbit 0))

(def beam-orbital-spaces
  (mapv beam-space-for-orbit orbits))

(defn on-beam? [sid]
  (and (orbital? sid) (zero? (space-index sid))))

;; ── Angular geometry (fractions of the circle, CW from north) ─────────────

(defn angular-range
  "[start end) as fractions of full circle. CW from north."
  [sid]
  (cond
    (orbital? sid)
    (let [n (orbit-sizes (orbit-of sid))
          i (space-index sid)]
      [(/ i n) (/ (inc i) n)])

    (sun? sid)
    (let [k (wedge-of sid)]
      [(/ k num-wedges) (/ (inc k) num-wedges)])))

(defn- ranges-overlap?
  [[a0 a1] [b0 b1]]
  (and (< a0 b1) (< b0 a1)))

(defn angular-midpoint [sid]
  (let [[s e] (angular-range sid)] (/ (+ s e) 2.0)))

;; ── Adjacency ──────────────────────────────────────────────────────────────

(defn- adj+ [adj a b]
  (-> adj
      (update a (fnil conj #{}) b)
      (update b (fnil conj #{}) a)))

(defn- ring-adjacency [orbit]
  (let [n (orbit-sizes orbit)]
    (reduce (fn [a i]
              (adj+ a
                    (orbit-space orbit i)
                    (orbit-space orbit (mod (inc i) n))))
            {} (range n))))

(defn- inter-ring-adjacency [inner outer]
  (reduce (fn [a [i j]]
            (if (ranges-overlap?
                  (angular-range (orbit-space inner i))
                  (angular-range (orbit-space outer j)))
              (adj+ a
                    (orbit-space inner i)
                    (orbit-space outer j))
              a))
          {}
          (for [i (range (orbit-sizes inner))
                j (range (orbit-sizes outer))] [i j])))

(defn- sun-silver-adjacency []
  ;; Both rings have 5 spaces → 1:1 by index.
  (reduce (fn [a k]
            (adj+ a (sun-space k) (orbit-space :silver k)))
          {} (range num-wedges)))

(defn build-adjacency []
  (let [adj (reduce (fn [acc o]
                      (merge-with into acc (ring-adjacency o)))
                    {} orbits)
        adj (reduce (fn [acc [in out]]
                      (merge-with into acc (inter-ring-adjacency in out)))
                    adj (partition 2 1 orbits))]
    (merge-with into adj (sun-silver-adjacency))))

;; ── Orbit-neighbor helpers ────────────────────────────────────────────────

(def orbit-index (zipmap orbits (range)))

(defn inner-orbit [orbit]
  (let [i (orbit-index orbit)]
    (when (pos? i) (nth orbits (dec i)))))

(defn outer-orbit [orbit]
  (let [i (orbit-index orbit)]
    (when (< i (dec (count orbits))) (nth orbits (inc i)))))

(defn neighbors [adjacency sid]
  (get adjacency sid #{}))

(defn neighbors-in-orbit
  "Adjacent spaces of `sid` lying in target-orbit (only orbital rings)."
  [adjacency sid target-orbit]
  (->> (neighbors adjacency sid)
       (filterv #(and (orbital? %) (= target-orbit (orbit-of %))))))

(defn frontmost-adjacent-in-orbit
  "Of the neighbors of `sid` in `target-orbit`, the one most CCW from sid."
  [adjacency sid target-orbit]
  (let [candidates (neighbors-in-orbit adjacency sid target-orbit)]
    (when (seq candidates)
      (let [src-mid (angular-midpoint sid)]
        (first
          (sort-by
            (fn [c] (mod (- src-mid (angular-midpoint c)) 1.0))
            candidates))))))

;; ── Rendering ──────────────────────────────────────────────────────────────

(def view-size 800)
(def center   (/ view-size 2.0))

(def sun-inner-r 40.0)   ;; outer edge of inner-color pentagon
(def sun-outer-r 70.0)   ;; outer edge of red ring

(def orbit-radii
  {:silver  [ 75 110]
   :green   [115 155]
   :blue    [160 215]
   :purple  [220 290]
   :void    [295 380]})

(defn polar->xy [angle-rad radius]
  [(+ center (* radius (Math/sin angle-rad)))
   (- center (* radius (Math/cos angle-rad)))])

(defn space-center
  "Cartesian [x y] for the centroid of a space."
  [sid]
  (let [mid (angular-midpoint sid)
        a   (* 2.0 Math/PI mid)
        r   (cond
              (orbital? sid)
              (let [[ri ro] (orbit-radii (orbit-of sid))]
                (/ (+ ri ro) 2.0))

              (sun? sid)
              (/ (+ sun-inner-r sun-outer-r) 2.0))]
    (polar->xy a r)))

(defn arc-path
  "SVG path for an annular sector. start/end as fractions of circle, radii px."
  [start end r-inner r-outer]
  (let [;; Convert fractions to radians from north, CW positive.
        sa (- (* 2.0 Math/PI start) (/ Math/PI 2))
        ea (- (* 2.0 Math/PI end)   (/ Math/PI 2))
        ox1 (+ center (* r-outer (Math/cos sa)))
        oy1 (+ center (* r-outer (Math/sin sa)))
        ox2 (+ center (* r-outer (Math/cos ea)))
        oy2 (+ center (* r-outer (Math/sin ea)))
        ix1 (+ center (* r-inner (Math/cos ea)))
        iy1 (+ center (* r-inner (Math/sin ea)))
        ix2 (+ center (* r-inner (Math/cos sa)))
        iy2 (+ center (* r-inner (Math/sin sa)))
        large (if (> (- end start) 0.5) 1 0)]
    (str "M " ox1 " " oy1
         " A " r-outer " " r-outer " 0 " large " 1 " ox2 " " oy2
         " L " ix1 " " iy1
         " A " r-inner " " r-inner " 0 " large " 0 " ix2 " " iy2
         " Z")))

(defn wedge-triangle-path
  "Triangle path for a sun wedge's inner colored region (from sun center)."
  [k]
  (let [start (/ k num-wedges)
        end   (/ (inc k) num-wedges)
        sa    (- (* 2.0 Math/PI start) (/ Math/PI 2))
        ea    (- (* 2.0 Math/PI end)   (/ Math/PI 2))
        x1    (+ center (* sun-inner-r (Math/cos sa)))
        y1    (+ center (* sun-inner-r (Math/sin sa)))
        x2    (+ center (* sun-inner-r (Math/cos ea)))
        y2    (+ center (* sun-inner-r (Math/sin ea)))]
    (str "M " center " " center
         " L " x1 " " y1
         " L " x2 " " y2
         " Z")))

(defn wedge-outer-path
  "Annular sector path for a sun wedge's outer red region."
  [k]
  (arc-path (/ k num-wedges) (/ (inc k) num-wedges)
            sun-inner-r sun-outer-r))

(defn orbit-space-path [sid]
  (let [[s e] (angular-range sid)
        [ri ro] (orbit-radii (orbit-of sid))]
    (arc-path s e ri ro)))

;; ── Composite board record ────────────────────────────────────────────────

(defn build-board []
  {:adjacency (build-adjacency)
   :spaces    (all-spaces)
   :orbits    (into {} (map (juxt identity orbit-spaces) orbits))
   :wedges    (sun-spaces)})
