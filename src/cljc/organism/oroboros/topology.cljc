(ns organism.oroboros.topology
  "Board topologies: square grid, hex grid, torus variants.
   Each builder returns {:nodes :adjacencies :coords :grid-size :topology-type}
   with integer node IDs and (row, col) coordinates for SVG rendering.")

;; ── Square grid ──────────────────────────────────────────────────────────────────

(defn build-square
  "4-connected square grid. Optional toroidal wrapping."
  [size wrap?]
  (let [n (* size size)
        nodes (vec (range n))
        adjacencies
        (reduce
         (fn [adj idx]
           (let [r (quot idx size)
                 c (rem idx size)
                 neighbors
                 (for [[dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
                       :let [nr (+ r dr) nc (+ c dc)
                             nr (if wrap? (mod nr size) nr)
                             nc (if wrap? (mod nc size) nc)]
                       :when (and (>= nr 0) (< nr size)
                                  (>= nc 0) (< nc size))]
                   (+ (* nr size) nc))]
             (assoc adj idx (vec (distinct neighbors)))))
         {} nodes)
        coords (reduce
                (fn [m idx]
                  (assoc m idx [(quot idx size) (rem idx size)]))
                {} nodes)]
    {:nodes nodes
     :adjacencies adjacencies
     :coords coords
     :grid-size size
     :topology-type (if wrap? "torus_square" "square")}))

;; ── Hex grid ─────────────────────────────────────────────────────────────────────

(defn- hex-cells
  "Generate all axial hex cells (q, r) within radius."
  [radius]
  (let [r (dec radius)]
    (for [q (range (- r) (inc r))
          row (range (- r) (inc r))
          :when (< (Math/abs (+ q row)) radius)]
      [q row])))

(def ^:private hex-dirs
  [[1 0] [-1 0] [0 1] [0 -1] [1 -1] [-1 1]])

(defn build-hex
  "6-connected hex grid in axial coordinates."
  [radius]
  (let [cells (vec (sort (hex-cells radius)))
        cell->id (into {} (map-indexed (fn [i c] [c i]) cells))
        nodes (vec (range (count cells)))
        adjacencies
        (reduce
         (fn [adj [cell idx]]
           (let [neighbors
                 (for [[dq dr] hex-dirs
                       :let [nc [(+ (first cell) dq) (+ (second cell) dr)]]
                       :when (contains? cell->id nc)]
                   (get cell->id nc))]
             (assoc adj idx (vec neighbors))))
         {} (map vector cells (range)))
        grid-size (dec (* 2 radius))
        coords (reduce
                (fn [m [cell idx]]
                  (let [[q r] cell]
                    (assoc m idx [(+ r (dec radius)) (+ q (dec radius))])))
                {} (map vector cells (range)))]
    {:nodes nodes
     :adjacencies adjacencies
     :coords coords
     :grid-size grid-size
     :topology-type "hex"}))

(defn build-torus-hex
  "Hex grid with toroidal wrapping."
  [radius]
  ;; For rendering purposes, same as hex. Wrapping adds edges between boundary cells.
  (let [base (build-hex radius)
        cells (vec (sort (hex-cells radius)))
        cell->id (into {} (map-indexed (fn [i c] [c i]) cells))
        max-coord (dec radius)
        ;; Add wrap-around adjacencies for boundary cells
        wrap-adj
        (reduce
         (fn [adj [cell idx]]
           (let [extra-neighbors
                 (for [[dq dr] hex-dirs
                       :let [[q r] cell
                             nq (+ q dq) nr (+ r dr)
                             nc [nq nr]]
                       :when (and (not (contains? cell->id nc))
                                  ;; Wrap: find the cell on the opposite boundary
                                  ;; Simple approach: wrap coordinates into range
                                  (let [wq (mod (+ nq radius) (dec (* 2 radius)))
                                        wr (mod (+ nr radius) (dec (* 2 radius)))
                                        wc [(- wq (dec radius)) (- wr (dec radius))]]
                                    (contains? cell->id wc)))]
                   (let [wq (mod (+ (first nc) radius) (dec (* 2 radius)))
                         wr (mod (+ (second nc) radius) (dec (* 2 radius)))
                         wc [(- wq (dec radius)) (- wr (dec radius))]]
                     (get cell->id wc)))]
             (if (seq extra-neighbors)
               (update adj idx (fn [existing] (vec (distinct (concat existing extra-neighbors)))))
               adj)))
         (:adjacencies base) (map vector cells (range)))]
    (assoc base
           :adjacencies wrap-adj
           :topology-type "torus_hex")))

;; ── Factory ──────────────────────────────────────────────────────────────────────

(defn build-topology
  "Build a topology by type string."
  [topology-type size & [symmetry]]
  (case topology-type
    "square"       (build-square size false)
    "torus_square" (build-square size true)
    "hex"          (build-hex size)
    "torus_hex"    (build-torus-hex size)
    ;; Default: square
    (build-square size false)))
