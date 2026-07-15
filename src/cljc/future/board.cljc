(ns future.board
  "Board topology, adjacency, angular geometry, and SVG rendering geometry
   for FUTURE.

   Rings (inner → outer):

     [:sun k]           k ∈ 0..4   sun wedge
     [:orbit :silver n] n ∈ 0..4
     [:orbit :green  n] n ∈ 0..7
     [:orbit :blue   n] n ∈ 0..12
     [:orbit :purple n] n ∈ 0..20
     [:orbit :void   n] n ∈ 0..33

   Indexing convention
   -------------------
   * Orbital index 0 is CENTERED on the beam (angular position 0).
   * Sun wedges are OFFSET by half a wedge CCW from the beam — the beam
     runs along the boundary between [:sun 0] (silver) and [:sun 1] (green).
   * Wedge 0 is silver; going CW around the sun the colors are silver →
     green → blue → purple → void (matches rulebook v2 + board image).
   * Index increases CW in screen-space in both rings.
   * Forward = CCW = index decreasing.

   Adjacency
   ---------
   Two spaces are adjacent iff:
     (a) same ring, indices differ by ±1 mod size, OR
     (b) rings are ring-adjacent (per `ring-adjacency`) AND angular
         overlap on the unit circle > `min-overlap`.
   The sun↔silver 1:2 pattern falls out of the offset geometry rather
   than being hard-coded.")

;; ── Rings ──────────────────────────────────────────────────────────────────

(def ring-sizes
  "Number of spaces in each ring."
  {:sun 5 :silver 5 :green 8 :blue 13 :purple 21 :void 34})

(def orbits
  "Orbital rings, inner to outer (excludes the sun)."
  [:silver :green :blue :purple :void])

(def ring-adjacency
  "All rings in radial order (used for cross-ring adjacency)."
  [:sun :silver :green :blue :purple :void])

(def num-wedges 5)

(def wedge-color
  "Canonical sun-wedge index → color mapping (rulebook v2)."
  {0 :silver, 1 :green, 2 :blue, 3 :purple, 4 :void})

(def color->wedge
  "Inverse of wedge-color."
  (into {} (map (fn [[k v]] [v k]) wedge-color)))

(def wedge-placement-order
  "Order of seeded wedges at setup — first player = silver, etc."
  [:silver :green :blue :purple :void])

;; ── Rendering palette ──────────────────────────────────────────────────────

(def orbit-colors
  "Fill color per ring/color keyword."
  {:silver "#bfbfbf"
   :green  "#3fbf4f"
   :blue   "#3f7fdf"
   :purple "#9f3fdf"
   :void   "#1f1f1f"})

(def planet-fill orbit-colors)

(def sun-outer-color "#cf2222")
(def beam-color      "#ffe066")
(def flame-color     "#ff8844")

;; ── Space identifiers ──────────────────────────────────────────────────────

(defn orbit-space
  "Construct an orbital space id."
  [orbit i] [:orbit orbit i])

(defn sun-space
  "Construct a sun wedge space id."
  [k] [:sun k])

(defn space-type
  "Return :orbit or :sun for the space."
  [sid] (first sid))

(defn sun?
  "Is this a sun wedge space?"
  [sid] (= :sun (space-type sid)))

(defn orbital?
  "Is this an orbital space?"
  [sid] (= :orbit (space-type sid)))

(defn orbit-of
  "Return the orbit color of an orbital space, or nil."
  [sid] (when (orbital? sid) (nth sid 1)))

(defn space-index
  "Return the index of an orbital space, or nil."
  [sid] (when (orbital? sid) (nth sid 2)))

(defn wedge-of
  "Return the wedge index of a sun space, or nil."
  [sid] (when (sun? sid) (nth sid 1)))

(defn space-color
  "Return the color associated with a space."
  [sid]
  (cond
    (sun?     sid) (wedge-color (wedge-of sid))
    (orbital? sid) (orbit-of sid)))

(defn orbit-spaces
  "All space ids in a given orbital ring."
  [orbit]
  (mapv #(orbit-space orbit %) (range (ring-sizes orbit))))

(defn sun-spaces
  "All sun wedge space ids."
  []
  (mapv sun-space (range num-wedges)))

(defn all-orbital-spaces
  "Every orbital space id in the board."
  []
  (vec (mapcat orbit-spaces orbits)))

(defn all-spaces
  "Every space id in the board (sun + orbital)."
  []
  (vec (concat (sun-spaces) (all-orbital-spaces))))

;; ── Direction ──────────────────────────────────────────────────────────────

(defn front-space
  "The space one step forward (CCW = index − 1)."
  [sid]
  (cond
    (orbital? sid)
    (let [[_ o i] sid n (ring-sizes o)]
      [:orbit o (mod (dec i) n)])

    (sun? sid)
    (let [[_ k] sid]
      [:sun (mod (dec k) num-wedges)])))

(defn back-space
  "The space one step backward (CW = index + 1)."
  [sid]
  (cond
    (orbital? sid)
    (let [[_ o i] sid n (ring-sizes o)]
      [:orbit o (mod (inc i) n)])

    (sun? sid)
    (let [[_ k] sid]
      [:sun (mod (inc k) num-wedges)])))

;; ── Beam ───────────────────────────────────────────────────────────────────

(defn beam-space-for-orbit
  "Index-0 space for an orbital ring."
  [orbit] (orbit-space orbit 0))

(def beam-orbital-spaces
  "The five index-0 orbital spaces — the beam passes through each."
  (mapv beam-space-for-orbit orbits))

(defn on-beam?
  "True iff sid is a beam-aligned orbital space."
  [sid]
  (and (orbital? sid) (zero? (space-index sid))))

;; ── Angular geometry (fractions of a full turn, 0..1) ─────────────────────

(defn angular-center+half
  "Return [center half-width] on the unit circle for a space."
  [sid]
  (case (first sid)
    :orbit (let [[_ r n] sid size (ring-sizes r)]
             [(mod (/ (double n) size) 1.0) (/ 0.5 size)])
    :sun   (let [[_ k] sid]
             ;; Sun wedge k centered at (k - 0.5)/5. Wedge 0 → -0.1 ≡ 0.9.
             [(mod (/ (- (double k) 0.5) 5) 1.0) 0.1])))

(defn angular-center
  "Center of a space as a fraction of a full turn."
  [sid] (first (angular-center+half sid)))

(defn angular-half-width
  "Half-width of a space as a fraction of a full turn."
  [sid] (second (angular-center+half sid)))

(defn circular-distance
  "Shortest CW/CCW distance on the unit circle, in [0, 0.5]."
  [a b]
  (let [d (mod (- a b) 1.0)]
    (min d (- 1.0 d))))

(defn angular-overlap
  "Amount by which two spaces overlap on the unit circle. Positive =
   overlap; zero = tangential; negative = gap."
  [sid-a sid-b]
  (let [[ca ha] (angular-center+half sid-a)
        [cb hb] (angular-center+half sid-b)]
    (- (+ ha hb) (circular-distance ca cb))))

(def min-overlap
  "Threshold above which cross-ring overlap counts as adjacency."
  (/ 1.0 1000.0))

(defn angular-range
  "[start end) as fractions of a full turn — start may be > end when
   wrapping past 0 (in which case treat as [start, 1) ∪ [0, end))."
  [sid]
  (let [[c h] (angular-center+half sid)
        s (mod (- c h) 1.0)
        e (mod (+ c h) 1.0)]
    [s e]))

(defn angular-midpoint
  "Center of a space as a fraction of a full turn (alias)."
  [sid] (angular-center sid))

;; ── Adjacency ──────────────────────────────────────────────────────────────

(defn- adj+
  "Add undirected edge a↔b to adjacency map."
  [adj a b]
  (-> adj
      (update a (fnil conj #{}) b)
      (update b (fnil conj #{}) a)))

(defn- ring-index
  "Ring-order index of a ring keyword in ring-adjacency."
  [ring]
  (.indexOf ring-adjacency ring))

(defn- inner-ring [ring]
  (let [i (ring-index ring)]
    (when (pos? i) (nth ring-adjacency (dec i)))))

(defn- outer-ring [ring]
  (let [i (ring-index ring)]
    (when (< i (dec (count ring-adjacency))) (nth ring-adjacency (inc i)))))

(defn ring-of
  "Ring keyword for a space (:sun or one of the orbital colors)."
  [sid] (if (sun? sid) :sun (orbit-of sid)))

(defn all-spaces-in-ring
  "Vector of all space ids in a given ring keyword."
  [ring]
  (if (= :sun ring)
    (sun-spaces)
    (orbit-spaces ring)))

(defn inner-orbit
  "Inner orbital ring of an orbital ring, or nil for silver."
  [ring]
  (when (some #{ring} orbits)
    (let [i (.indexOf orbits ring)]
      (when (pos? i) (nth orbits (dec i))))))

(defn outer-orbit
  "Outer orbital ring of an orbital ring, or nil for void."
  [ring]
  (when (some #{ring} orbits)
    (let [i (.indexOf orbits ring)]
      (when (< i (dec (count orbits))) (nth orbits (inc i))))))

(defn- in-ring-adjacency
  "Adjacencies within a single ring."
  [ring]
  (let [n (ring-sizes ring)
        spaces (all-spaces-in-ring ring)]
    (reduce (fn [a i]
              (adj+ a (nth spaces i) (nth spaces (mod (inc i) n))))
            {} (range n))))

(defn- cross-ring-adjacency
  "Overlap-based cross-ring adjacencies for two ring-adjacent rings."
  [inner outer]
  (let [in-sids (all-spaces-in-ring inner)
        out-sids (all-spaces-in-ring outer)]
    (reduce (fn [a [x y]]
              (if (> (angular-overlap x y) min-overlap)
                (adj+ a x y)
                a))
            {}
            (for [x in-sids y out-sids] [x y]))))

(defn build-adjacency
  "Return an adjacency map {sid → #{sid …}}."
  []
  (let [with-in-ring
        (reduce (fn [acc r]
                  (merge-with into acc (in-ring-adjacency r)))
                {} ring-adjacency)]
    (reduce (fn [acc [inner outer]]
              (merge-with into acc (cross-ring-adjacency inner outer)))
            with-in-ring
            (partition 2 1 ring-adjacency))))

(defn neighbors
  "Set of adjacent space ids for sid."
  [adjacency sid] (get adjacency sid #{}))

(defn neighbors-in-ring
  "Neighbors of sid that lie in target-ring."
  [adjacency sid target-ring]
  (filterv #(= target-ring (ring-of %)) (neighbors adjacency sid)))

(defn frontmost-adjacent-in-ring
  "Of `sid`'s neighbors that lie in `target-ring`, the one whose angular
   center has the smallest CCW distance from sid's center. Returns nil if
   none. CCW distance from a to b = (a - b) mod 1."
  [adjacency sid target-ring]
  (let [candidates (neighbors-in-ring adjacency sid target-ring)]
    (when (seq candidates)
      (let [src-c (angular-center sid)]
        (first
          (sort-by
            (fn [c] (mod (- src-c (angular-center c)) 1.0))
            candidates))))))

;; ── Rendering (SVG geometry) ───────────────────────────────────────────────

(def view-size 800)
(def center (/ view-size 2.0))

(def sun-inner-r 40.0)
(def sun-outer-r 70.0)

(def orbit-radii
  "Inner and outer radius of each orbital ring."
  {:silver [ 78 118]
   :green  [122 165]
   :blue   [170 225]
   :purple [230 305]
   :void   [310 385]})

(defn orbit-space-radii
  "Convenience: mean radius of an orbit ring."
  [ring]
  (let [[ri ro] (orbit-radii ring)]
    (/ (+ ri ro) 2.0)))

(defn polar->xy
  "Convert (angle-in-turns, radius) to SVG [x y]. angle 0 = up."
  [angle-turns radius]
  (let [ang (* 2.0 Math/PI angle-turns)]
    [(+ center (* radius (Math/sin ang)))
     (- center (* radius (Math/cos ang)))]))

(defn space-center
  "SVG [x y] centroid of a space."
  [sid]
  (let [c (angular-center sid)
        r (cond
            (orbital? sid) (orbit-space-radii (orbit-of sid))
            (sun? sid)     (/ (+ sun-inner-r sun-outer-r) 2.0))]
    (polar->xy c r)))

(defn arc-path
  "SVG path 'd' for an annular sector from start to end (both fractions of
   a full turn, CW from top) between r-inner and r-outer."
  [start end r-inner r-outer]
  (let [;; angles from top (12 o'clock), CW positive; convert to standard
        ;; math radians (x = cos, y = sin from +x axis) minus π/2
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

(defn- wedge-arc-endpoints
  "Return [start end] as fractions of a full turn for wedge k, where
   wedges are offset half-a-wedge CCW from the beam."
  [k]
  (let [c (mod (/ (- (double k) 0.5) 5) 1.0)
        s (- c 0.1)
        e (+ c 0.1)]
    [s e]))

(defn wedge-triangle-path
  "SVG path for the inner colored triangle of sun wedge k."
  [k]
  (let [[start end] (wedge-arc-endpoints k)
        sa (- (* 2.0 Math/PI start) (/ Math/PI 2))
        ea (- (* 2.0 Math/PI end)   (/ Math/PI 2))
        x1 (+ center (* sun-inner-r (Math/cos sa)))
        y1 (+ center (* sun-inner-r (Math/sin sa)))
        x2 (+ center (* sun-inner-r (Math/cos ea)))
        y2 (+ center (* sun-inner-r (Math/sin ea)))]
    (str "M " center " " center
         " L " x1 " " y1
         " L " x2 " " y2
         " Z")))

(defn wedge-outer-path
  "SVG path for the outer (exhausted) red ring segment of sun wedge k."
  [k]
  (let [[start end] (wedge-arc-endpoints k)]
    (arc-path start end sun-inner-r sun-outer-r)))

(defn orbit-space-path
  "SVG path for a single orbital space."
  [sid]
  (let [c (angular-center sid)
        h (angular-half-width sid)
        [ri ro] (orbit-radii (orbit-of sid))]
    (arc-path (- c h) (+ c h) ri ro)))

;; ── Composite board record ────────────────────────────────────────────────

(defn build-board
  "Return a plain map with the topology fields: :adjacency, :spaces,
   :orbits, :wedges."
  []
  {:adjacency (build-adjacency)
   :spaces    (all-spaces)
   :orbits    (into {} (map (juxt identity orbit-spaces) orbits))
   :wedges    (sun-spaces)})
