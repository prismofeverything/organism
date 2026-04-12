(ns future.board
  "Board topology for Future: concentric Fibonacci rings around a central Sun.

   Orbits (inside → outside):
     silver  5 spaces
     green   8 spaces
     blue   13 spaces
     purple 21 spaces
     void   34 spaces
            ── total 81 spaces

   Each ring's count is the sum of the two preceding rings (Fibonacci).
   Adjacency: within a ring (circular neighbors) and between adjacent rings
   (spaces whose angular extents overlap).

   The Sun sits at the center with a solar network of 5 sections (one per
   orbit color), each having an 'available' and 'exhausted' zone for
   components.

   The Beam is a line from the sun outward, crossing space 0 of each orbit.
   Front = counterclockwise (decreasing index).")

;; ── Constants ───────────────────────────────────────────────────────────────

(def orbits
  "Ordered from innermost to outermost."
  [:silver :green :blue :purple :void])

(def orbits-reversed
  "Outermost to innermost (for setup order)."
  [:void :purple :blue :green :silver])

(def orbit-sizes
  {:silver 5
   :green  8
   :blue   13
   :purple 21
   :void   34})

(def orbit-colors
  {:silver "#999999"
   :green  "#44cc44"
   :blue   "#4488ee"
   :purple "#9944cc"
   :void   "#222222"})

(def sun-color "#cc2222")

;; ── Space identifiers ───────────────────────────────────────────────────────

(defn space-id
  "Canonical identifier for a board space, e.g. [:silver 0]."
  [orbit index]
  [orbit index])

(defn space-orbit [sid] (first sid))
(defn space-index [sid] (second sid))

(defn orbit-spaces
  "All space ids for a given orbit."
  [orbit]
  (let [n (orbit-sizes orbit)]
    (mapv #(space-id orbit %) (range n))))

(defn all-spaces
  "All 81 board spaces, inner to outer."
  []
  (vec (mapcat orbit-spaces orbits)))

;; ── Direction helpers ───────────────────────────────────────────────────────
;; Front = counterclockwise = decreasing index.

(defn front-space
  "The space in front (counterclockwise) of the given space in the same ring."
  [[orbit idx]]
  (let [n (orbit-sizes orbit)]
    [orbit (mod (dec idx) n)]))

(defn back-space
  "The space behind (clockwise) of the given space in the same ring."
  [[orbit idx]]
  (let [n (orbit-sizes orbit)]
    [orbit (mod (inc idx) n)]))

;; ── Beam ────────────────────────────────────────────────────────────────────
;; The beam crosses space index 0 of every orbit.

(def beam-spaces
  "One space per orbit where the beam crosses."
  (mapv #(space-id % 0) orbits))

(defn beam-space
  "The beam space for a given orbit."
  [orbit]
  (space-id orbit 0))

;; ── Angular geometry ────────────────────────────────────────────────────────

(defn- angular-range
  "Returns [start end] as fractions of the full circle [0,1)."
  [index ring-size]
  [(/ index ring-size)
   (/ (inc index) ring-size)])

(defn angular-midpoint
  "Midpoint angle as fraction of full circle."
  [index ring-size]
  (/ (+ index 0.5) ring-size))

(defn- ranges-overlap?
  "Do two angular ranges [a0,a1) and [b0,b1) overlap?"
  [[a0 a1] [b0 b1]]
  (and (< a0 b1) (< b0 a1)))

;; ── Adjacency computation ───────────────────────────────────────────────────

(defn- ring-adjacency
  "Circular adjacency within a single ring."
  [orbit]
  (let [n (orbit-sizes orbit)]
    (into {}
      (for [i (range n)]
        [(space-id orbit i)
         #{(space-id orbit (mod (dec i) n))
           (space-id orbit (mod (inc i) n))}]))))

(defn- inter-ring-adjacency
  "Adjacency between two adjacent rings based on angular overlap."
  [inner-orbit outer-orbit]
  (let [ni (orbit-sizes inner-orbit)
        no (orbit-sizes outer-orbit)]
    (reduce
     (fn [adj i]
       (let [ir (angular-range i ni)]
         (reduce
          (fn [adj j]
            (if (ranges-overlap? ir (angular-range j no))
              (-> adj
                  (update (space-id inner-orbit i)
                          (fnil conj #{}) (space-id outer-orbit j))
                  (update (space-id outer-orbit j)
                          (fnil conj #{}) (space-id inner-orbit i)))
              adj))
          adj (range no))))
     {} (range ni))))

(defn- merge-adjacency [a b]
  (merge-with into a b))

(defn build-adjacency
  "Full adjacency map: {space-id → #{neighbor-ids}}."
  []
  (let [ring-adj (reduce merge-adjacency {}
                         (map ring-adjacency orbits))
        pairs (partition 2 1 orbits)
        inter-adj (reduce merge-adjacency {}
                          (map (fn [[inner outer]]
                                 (inter-ring-adjacency inner outer))
                               pairs))]
    (merge-adjacency ring-adj inter-adj)))

;; ── Orbit neighbor helpers ──────────────────────────────────────────────────

(def orbit-index
  "Map orbit keyword → position in orbits vector."
  (into {} (map-indexed (fn [i o] [o i]) orbits)))

(defn inner-orbit
  "The orbit one ring closer to the sun, or nil."
  [orbit]
  (let [i (orbit-index orbit)]
    (when (pos? i)
      (nth orbits (dec i)))))

(defn outer-orbit
  "The orbit one ring further from the sun, or nil."
  [orbit]
  (let [i (orbit-index orbit)]
    (when (< i (dec (count orbits)))
      (nth orbits (inc i)))))

(defn adjacent-in-ring
  "All neighbors of space-id that are in the specified target-orbit."
  [adjacency space-id target-orbit]
  (filter #(= (space-orbit %) target-orbit)
          (get adjacency space-id #{})))

(defn frontmost-adjacent-in-ring
  "Among the neighbors of space-id in target-orbit, return the one that is
   most counterclockwise (has the smallest angular distance CCW from space-id)."
  [adjacency sid target-orbit]
  (let [candidates (adjacent-in-ring adjacency sid target-orbit)]
    (when (seq candidates)
      ;; Pick the candidate whose midpoint is most counterclockwise from sid's midpoint.
      ;; 'Most counterclockwise' = largest negative angular delta (mod 1).
      ;; Equivalently, we want the candidate with the greatest (mid_candidate - mid_src) mod 1
      ;; when going in the CCW direction — but simpler: just pick the candidate with the
      ;; smallest index, wrapping appropriately.
      ;; For correctness: pick the candidate whose angular midpoint is closest to sid's
      ;; midpoint in the CCW direction.
      (let [[src-orbit src-idx] sid
            src-mid (angular-midpoint src-idx (orbit-sizes src-orbit))
            n-tgt (orbit-sizes target-orbit)]
        (->> candidates
             (sort-by (fn [[_ tidx]]
                        ;; Angular distance going CCW (positive = more CCW)
                        (let [tgt-mid (angular-midpoint tidx n-tgt)
                              delta (- src-mid tgt-mid)]
                          ;; Normalize to [0, 1) — smaller positive = more in front
                          (mod delta 1.0))))
             first)))))

;; ── Board construction ──────────────────────────────────────────────────────

(defn build-board
  "Returns the complete board map."
  []
  (let [spaces (all-spaces)
        adjacency (build-adjacency)
        orbit-map (into {} (map (fn [o] [o (orbit-spaces o)]) orbits))
        orbit-of (into {}
                       (for [o orbits
                             s (orbit-spaces o)]
                         [s o]))]
    {:spaces    spaces
     :adjacency adjacency
     :orbits    orbit-map
     :orbit-of  orbit-of}))

;; ── Geometry for rendering ──────────────────────────────────────────────────

(def orbit-radii
  "Inner and outer radius for each orbit ring (for SVG rendering).
   Sun occupies radius 0-50."
  {:silver [55  95]
   :green  [100 150]
   :blue   [155 215]
   :purple [220 290]
   :void   [295 375]})

(defn space-center-polar
  "Returns [angle-degrees radius] for the center of a space."
  [space-id]
  (let [[orbit idx] space-id
        n (orbit-sizes orbit)
        angle (* 360.0 (/ (+ idx 0.5) n))
        [r-inner r-outer] (orbit-radii orbit)
        r (/ (+ r-inner r-outer) 2.0)]
    [angle r]))

(defn space-center-xy
  "Returns [x y] for the center of a space in SVG coordinates.
   Origin at (400, 400) for an 800x800 viewBox."
  [space-id]
  (let [[angle-deg radius] (space-center-polar space-id)
        angle-rad (* (/ angle-deg 180.0) Math/PI)
        x (+ 400.0 (* radius (Math/sin angle-rad)))
        y (- 400.0 (* radius (Math/cos angle-rad)))]
    [x y]))
