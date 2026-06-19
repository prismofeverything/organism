(ns organism.chroma.engine
  "Pure Chroma game engine — a faithful Clojure port of game-ideas/chroma-core.js.

   This is now the SINGLE source of truth for Chroma game logic (the hand-written
   JS engine is being retired). Humans and bots traverse the same functions:
     enumerate-moves -> the presented choice set
     decide          -> a pure weighted bot selector (genome = input)
     apply-placement -> the one mutator: pull chit from hand, push onto stack
   Board geometry/seeding mirror render_board.py (edge 6 = 91 cells).

   Palette / depth / trimmed-cells are per-game settings carried on the game map
   and bound into dynamic vars around engine calls via `with-config`.

   Cells are [q r] vectors used directly as map keys (no \"q,r\" strings). The RNG
   is a per-turn java.util.Random seeded from (game-seed, turn) — deterministic for
   replay; it intentionally does NOT bit-match mulberry32 (the JS engine is gone, so
   no cross-engine determinism is needed)."
  (:require [clojure.string :as str]))

;; ── geometry ─────────────────────────────────────────────────────────────────

(def ^:const SIZE 1.0)
(def SQRT3 (Math/sqrt 3.0))
(def ^:const N 6)
(def ^:const R (dec N)) ; 5

(defn ax->px [q r]
  [(* SIZE (+ (* SQRT3 q) (* (/ SQRT3 2.0) r)))
   (* SIZE 1.5 r)])

(defn- deg [rad] (/ (* rad 180.0) Math/PI))

(defn px->cell [x y]
  (let [r  (/ y (* 1.5 SIZE))
        q  (/ (- (/ x SIZE) (* (/ SQRT3 2.0) r)) SQRT3)
        cx q, cy (- (- q) r), cz r
        rx (Math/round cx), ry (Math/round cy), rz (Math/round cz)
        dx (Math/abs (- rx cx)), dy (Math/abs (- ry cy)), dz (Math/abs (- rz cz))]
    (cond
      (and (> dx dy) (> dx dz)) [(int (- (- ry) rz)) (int rz)]
      (> dy dz)                 [(int rx) (int rz)]
      :else                     [(int rx) (int (- (- rx) ry))])))

(def cells
  (vec (for [q (range (- R) (inc R))
             r (range (- R) (inc R))
             :when (<= (max (Math/abs q) (Math/abs r) (Math/abs (- (- q) r))) R)]
         [q r])))

(def cell-set (set cells))

(def NEI [[1 0] [1 -1] [0 -1] [-1 0] [-1 1] [0 1]])

(defn- by-angle [coll]
  (->> coll
       (map (fn [c] (let [[x y] (ax->px (first c) (second c))]
                      [(mod (+ (deg (Math/atan2 y x)) 360.0) 360.0) (first c) (second c)])))
       (sort-by first)))

(def ^:private corner-cells
  (filterv (fn [c]
             (let [a (sort [(Math/abs (first c)) (Math/abs (second c))
                            (Math/abs (- (- (first c)) (second c)))])]
               (and (= (nth a 0) 0) (= (nth a 1) R) (= (nth a 2) R))))
           cells))

;; vert: index (angular order) -> corner [q r]
(def ^:private vert
  (into {} (map-indexed (fn [i c] [i [(nth c 1) (nth c 2)]]) (by-angle corner-cells))))

;; NEI sorted by angle (for seeding the six neighbors of the center)
(def ^:private nb-sorted
  (mapv (fn [c] [(nth c 1) (nth c 2)]) (by-angle NEI)))

(defn isCenter? [c] (and (= (first c) 0) (= (second c) 0)))

(def ring
  (into {} (map (fn [c] [c (max (Math/abs (first c)) (Math/abs (second c))
                                (Math/abs (- (- (first c)) (second c))))]) cells)))

(def max-ring (reduce max (vals ring)))

(defn sector [q r]
  (let [[x y] (ax->px q r)
        a (mod (+ (deg (Math/atan2 y x)) 1e-6 360.0) 360.0)]
    (int (Math/floor (/ (mod (+ a 30.0) 360.0) 60.0)))))

;; ── palettes ─────────────────────────────────────────────────────────────────

(def palettes
  {:RYB {:order ["R" "O" "Y" "G" "B" "P"]
         :trans {"R" [0.92 0.30 0.33] "O" [0.95 0.58 0.26] "Y" [0.97 0.92 0.32]
                 "G" [0.30 0.84 0.46] "B" [0.30 0.46 0.93] "P" [0.62 0.34 0.86]}
         :chip {"R" "#e23b3b" "O" "#ef8a2b" "Y" "#f4d030" "G" "#3fae54" "B" "#2f6fd0" "P" "#7a3fb0"}
         :name {"R" "Red" "O" "Orange" "Y" "Yellow" "G" "Green" "B" "Blue" "P" "Purple"}}
   :CMY {:order ["C" "B" "M" "R" "Y" "G"]
         :trans {"C" [0.10 0.90 0.92] "B" [0.12 0.14 0.92] "M" [0.92 0.12 0.90]
                 "R" [0.92 0.13 0.12] "Y" [0.94 0.92 0.12] "G" [0.12 0.90 0.20]}
         :chip {"C" "#22c3d6" "M" "#d63cae" "Y" "#f4d030" "R" "#e23b3b" "G" "#3fae54" "B" "#2f6fd0"}
         :name {"C" "Cyan" "M" "Magenta" "Y" "Yellow" "R" "Red" "G" "Green" "B" "Blue"}}})

(def ^:private trans-fixed {"W" [1.0 1.0 1.0] "K" [0.05 0.05 0.05]})

;; ── per-game config (palette / depth / trimmed cells) ────────────────────────

(def ^:dynamic *pal* (:CMY palettes))
(def ^:dynamic *palette-key* :CMY)
(def ^:dynamic *depth* 3)
(def ^:dynamic *removed* #{})

;; The 9-corner trim (Mohammad 2026-06-14): the live mock's TRIM_CELLS.
(def trim-cells
  #{[5 0] [4 1] [5 -1] [-5 5] [-5 4] [-4 5] [0 -5] [1 -5] [-1 -4]})

(defn order [] (:order *pal*))

;; seed map depends only on the palette key; memoize it.
(def seed-for
  (memoize
   (fn [palette-key]
     (let [pal (palettes palette-key)
           o (:order pal)]
       (as-> {} s
         (assoc s [0 0] "K")
         ;; six neighbours of center -> the six palette colors in angular order
         (reduce (fn [m i] (assoc m (nth nb-sorted i) (nth o i))) s (range 6))
         ;; three edges: walk the segment between two corners, paint the primate
         (reduce
          (fn [m e]
            (let [[ai bi pi] e
                  a (vert ai), b (vert bi), prim (nth o pi)
                  [pax pay] (ax->px (first a) (second a))
                  [pbx pby] (ax->px (first b) (second b))]
              (reduce
               (fn [mm t]
                 (let [f (/ t 600.0)
                       cell (px->cell (+ pax (* (- pbx pax) f)) (+ pay (* (- pby pay) f)))]
                   (if (and (cell-set cell) (not= cell [0 0]))
                     (assoc mm cell prim)
                     mm)))
               m (range 0 601))))
          s [[1 3 2] [3 5 4] [5 1 0]])
         ;; three secondary vertices
         (reduce (fn [m i] (assoc m (vert i) (nth o i))) s [1 3 5]))))))

(defn seed-map [] (seed-for *palette-key*))

(defmacro with-config
  "Bind *pal*/*palette-key*/*depth*/*removed* from a game map G for the body."
  [G & body]
  `(let [g# ~G]
     (binding [*palette-key* (or (:palette g#) :CMY)
               *pal* (palettes (or (:palette g#) :CMY))
               *depth* (or (:depth g#) 3)
               *removed* (or (:removed g#) #{})]
       ~@body)))

(defn removed? [c] (contains? *removed* c))
(defn capacity [c] (if (removed? c) 0 *depth*))

;; ── mixing / classification ──────────────────────────────────────────────────

(def ^:const REL_LUM_MIN 0.14)
(def ^:const PURITY_MAX 0.105)

(defn rel-lum [[r g b]] (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))

(defn trans [ck] (or (get-in *pal* [:trans ck]) (trans-fixed ck) [1.0 1.0 1.0]))

(defn mix-stack [stack]
  (reduce (fn [rgb ck]
            (let [t (trans ck)]
              [(* (nth rgb 0) (nth t 0))
               (* (nth rgb 1) (nth t 1))
               (* (nth rgb 2) (nth t 2))]))
          [1.0 1.0 1.0] stack))

(defn classify [rgb]
  (let [mx (apply max rgb)]
    (cond
      (< (rel-lum rgb) REL_LUM_MIN) "mud"
      (and (> (nth rgb 0) 0.85) (> (nth rgb 1) 0.85) (> (nth rgb 2) 0.85)) "white"
      :else
      (let [nr (mapv #(/ % mx) rgb)
            [best bd]
            (reduce
             (fn [[best bd] c]
               (let [b (get-in *pal* [:trans c])
                     bmx (apply max b)
                     nb (mapv #(/ % bmx) b)
                     d (+ (* (- (nth nr 0) (nth nb 0)) (- (nth nr 0) (nth nb 0)))
                          (* (- (nth nr 1) (nth nb 1)) (- (nth nr 1) (nth nb 1)))
                          (* (- (nth nr 2) (nth nb 2)) (- (nth nr 2) (nth nb 2))))]
                 (if (< d bd) [c d] [best bd])))
             [nil 1e9] (order))]
        (if (> bd PURITY_MAX) "mud" best)))))

(defn classify-independent [stack]
  (let [[ar ag ab]
        (reduce (fn [[ar ag ab] ck]
                  (let [t (trans ck)]
                    [(+ ar (- (Math/log (max (nth t 0) 1e-4))))
                     (+ ag (- (Math/log (max (nth t 1) 1e-4))))
                     (+ ab (- (Math/log (max (nth t 2) 1e-4))))]))
                [0.0 0.0 0.0] stack)]
    (classify [(Math/exp (- ar)) (Math/exp (- ag)) (Math/exp (- ab))])))

(defn result-of [stack]
  (let [rgb (mix-stack stack)]
    {:rgb rgb :color (classify rgb) :check (classify-independent stack)}))

(defn display-rgb [stack]
  (if (empty? stack)
    [245 245 242]
    (let [rgb (mix-stack stack)]
      (mapv #(int (Math/round (double (* % 255)))) rgb))))

;; ── board analysis / scoring ─────────────────────────────────────────────────

(defn board-colors [stacks]
  (reduce (fn [m c]
            (let [st (get stacks c [])]
              (assoc m c (if (seq st) (classify (mix-stack st)) "white"))))
          {} cells))

(defn- flood-sizes
  "Return a seq of region sizes for cells whose color satisfies (pred color)."
  [col pred]
  (loop [todo cells, seen #{}, sizes []]
    (if (empty? todo)
      sizes
      (let [c (first todo)]
        (if (or (seen c) (not (pred (col c))))
          (recur (rest todo) (conj seen c) sizes)
          (let [color (col c)
                [seen2 size]
                (loop [stack [c], seen (conj seen c), size 0]
                  (if (empty? stack)
                    [seen size]
                    (let [cur (peek stack)
                          nbrs (for [d NEI
                                     :let [nk [(+ (first cur) (first d)) (+ (second cur) (second d))]]
                                     :when (and (cell-set nk) (not (seen nk)) (= (col nk) color))]
                                 nk)]
                      (recur (into (pop stack) nbrs) (into seen nbrs) (inc size)))))]
            (recur (rest todo) seen2 (conj sizes size))))))))

(defn largest-regions [stacks]
  (let [col (board-colors stacks)
        palette-colors (set (order))]
    (loop [todo cells, seen #{}, best (zipmap (order) (repeat 0))]
      (if (empty? todo)
        best
        (let [c (first todo)
              color (col c)]
          (if (or (seen c) (not (palette-colors color)))
            (recur (rest todo) (conj seen c) best)
            (let [[seen2 size]
                  (loop [stack [c], seen (conj seen c), size 0]
                    (if (empty? stack)
                      [seen size]
                      (let [cur (peek stack)
                            nbrs (for [d NEI
                                       :let [nk [(+ (first cur) (first d)) (+ (second cur) (second d))]]
                                       :when (and (cell-set nk) (not (seen nk)) (= (col nk) color))]
                                   nk)]
                        (recur (into (pop stack) nbrs)
                               (into seen nbrs)
                               (inc size)))))]
              (recur (rest todo) seen2 (update best color #(max % size))))))))))

(defn largest-two [stacks]
  (let [col (board-colors stacks)
        palette-colors (set (order))
        z (zipmap (order) (repeat 0))]
    (loop [todo cells, seen #{}, first* z, second* z]
      (if (empty? todo)
        {:first first* :second second*}
        (let [c (clojure.core/first todo)
              color (col c)]
          (if (or (seen c) (not (palette-colors color)))
            (recur (rest todo) (conj seen c) first* second*)
            (let [[seen2 size]
                  (loop [stack [c], seen (conj seen c), size 0]
                    (if (empty? stack)
                      [seen size]
                      (let [cur (peek stack)
                            nbrs (for [d NEI
                                       :let [nk [(+ (clojure.core/first cur) (clojure.core/first d))
                                                 (+ (second cur) (second d))]]
                                       :when (and (cell-set nk) (not (seen nk)) (= (col nk) color))]
                                   nk)]
                        (recur (into (pop stack) nbrs) (into seen nbrs) (inc size)))))
                  fc (first* color)]
              (cond
                (> size fc) (recur (rest todo) seen2 (assoc first* color size) (assoc second* color fc))
                (> size (second* color)) (recur (rest todo) seen2 first* (assoc second* color size))
                :else (recur (rest todo) seen2 first* second*)))))))))

(defn mud-regions [stacks]
  (let [col (board-colors stacks)
        total (count (filter #(= (col %) "mud") cells))
        sizes (flood-sizes col #(= % "mud"))
        largest (if (seq sizes) (reduce max sizes) 0)]
    {:total total :largest largest}))

(defn tier [n] (+ (if (>= n 1) 1 0) (if (>= n 4) 1 0) (if (>= n 8) 1 0) (if (>= n 12) 1 0)))

(defn- hand-counts [hand]
  (let [z (zipmap (order) (repeat 0))]
    (reduce (fn [c x] (if (contains? c x) (update c x inc) c)) z hand)))

(defn score-game
  "LIVE op2b scoring + mud v2. Returns one row (map) per player."
  [players stacks]
  (let [reg (largest-regions stacks)
        two (largest-two stacks)
        ord (order)
        np (count players)
        cnts (mapv #(hand-counts (:hand %)) players)
        init (vec (repeat np {:mult 0 :add 0 :colorBreak []}))
        ;; color scoring
        acc (reduce
             (fn [acc color]
               (let [mx (reduce max 0 (map #(get % color 0) cnts))]
                 (if (<= mx 0)
                   acc
                   (let [holders (filterv #(= (get (nth cnts %) color 0) mx) (range np))]
                     (if (= 1 (count holders))
                       (let [w (first holders) v (reg color)]
                         (-> acc
                             (update-in [w :mult] + v)
                             (update-in [w :add] + (tier v))
                             (update-in [w :colorBreak] conj {:color color :value v :tie false})))
                       (reduce (fn [a h]
                                 (let [v (get-in two [:second color])]
                                   (-> a
                                       (update-in [h :mult] + v)
                                       (update-in [h :add] + (tier v))
                                       (update-in [h :colorBreak] conj {:color color :value v :tie true}))))
                               acc holders))))))
             init ord)
        ;; mud scoring v2
        mr (mud-regions stacks)
        min-hand (reduce min (map #(count (:hand %)) players))
        mud-holders (filterv #(= (count (:hand (nth players %))) min-hand) (range np))
        mud-tie (> (count mud-holders) 1)
        mud-score (vec (repeat np 0))
        mud-break (vec (repeat np nil))
        [mud-score mud-break]
        (if-not mud-tie
          [(assoc mud-score (first mud-holders) (:total mr))
           (assoc mud-break (first mud-holders) {:value (:total mr) :tie false})]
          (reduce (fn [[ms mb] h]
                    [(assoc ms h (:largest mr)) (assoc mb h {:value (:largest mr) :tie true})])
                  [mud-score mud-break] mud-holders))]
    (mapv (fn [i]
            (let [p (nth players i)
                  a (nth acc i)
                  cb (:colorBreak a)
                  ms (nth mud-score i)
                  mb (nth mud-break i)
                  scored-types (+ (count (filter #(> (:value %) 0) cb))
                                  (if (and mb (> (:value mb) 0)) 1 0))]
              {:mult (+ (:mult a) ms) :add (:add a) :size (+ (:mult a) ms)
               :regionScore (:mult a) :mudScore ms
               :boardMud (:total mr) :mudLargest (:largest mr)
               :breakdown {:colors cb :mud mb}
               :scoredTypes scored-types
               :distinct (count (distinct (:hand p)))
               :dry (:dry p) :swaps (:swaps p) :handLen (count (:hand p)) :target (:target p)}))
          (range np))))

;; ── color-wheel swaps ────────────────────────────────────────────────────────

(defn wheel-opposite [c]
  (let [o (order) i (.indexOf o c)] (if (neg? i) nil (nth o (mod (+ i 3) 6)))))

(defn wheel-between [a b]
  (let [o (order) i (.indexOf o a) j (.indexOf o b)]
    (if (or (neg? i) (neg? j) (= i j))
      nil
      (let [d (mod (+ (- j i) 6) 6)
            [i d] (if (> d 3) [j (- 6 d)] [i d])]
        (if (not= d 2) nil (nth o (mod (+ i 1) 6)))))))

(defn wheel-neighbors [x]
  (let [o (order) i (.indexOf o x)]
    (if (neg? i) [] [(nth o (mod (+ i 5) 6)) (nth o (mod (+ i 1) 6))])))

;; ── rng ──────────────────────────────────────────────────────────────────────

(defn make-rng ^java.util.Random [seed]
  (java.util.Random. (long seed)))

(defn rnd ^double [^java.util.Random r] (.nextDouble r))

(defn turn-seed [game-seed turn]
  (bit-xor (long game-seed) (unchecked-multiply (long turn) 2654435761)))

;; ── bag / draw ───────────────────────────────────────────────────────────────

(def ^:const MUD_LIMIT 3)
(def ^:const START_HAND 6)
(def ^:const PER_COLOR 30)
(def PRIMARIES ["C" "M" "Y"])
(def SECONDARIES ["R" "G" "B"])

(defn bases-for [n]
  (vec (for [i (range n)] (mod (Math/round (double (/ (* i 6) n))) 6))))

(defn seed-count []
  (let [s (seed-map) z (zipmap (order) (repeat 0))]
    (reduce (fn [m k] (let [c (s k)] (if (contains? m c) (update m c inc) m))) z (keys s))))

(defn bag-total [bag] (reduce + (map #(get bag % 0) (order))))

(defn draw-weighted
  "Draw one color weighted by bag counts. Returns [color new-bag] or [nil bag]."
  [bag ^java.util.Random rng]
  (let [t (bag-total bag)]
    (if (<= t 0)
      [nil bag]
      (let [r (* (rnd rng) t)]
        (loop [ord (order) r r]
          (if (empty? ord)
            [nil bag]
            (let [c (first ord) r (- r (get bag c 0))]
              (if (< r 0)
                [c (update bag c dec)]
                (recur (rest ord) r)))))))))

;; ── moves ────────────────────────────────────────────────────────────────────

(defn wedge-of [p turn] (mod (+ (:base p) turn) 6))

(defn legal-cells [stacks w]
  (vec (for [c cells
             :when (and (not (isCenter? c))
                        (= (sector (first c) (second c)) w)
                        (not (removed? c))
                        (< (count (get stacks c [])) (capacity c)))]
         c)))

(defn label-regions
  "Connected-component labelling. Returns {:col col :lab {cell id} :size {id n}}."
  [stacks]
  (let [col (board-colors stacks)
        palette-colors (set (order))]
    (loop [todo cells, lab {}, size {}, id 0]
      (if (empty? todo)
        {:col col :lab lab :size size}
        (let [c (first todo)]
          (cond
            (contains? lab c) (recur (rest todo) lab size id)
            (not (palette-colors (col c))) (recur (rest todo) (assoc lab c -1) size id)
            :else
            (let [color (col c)
                  [lab2 n]
                  (loop [stack [c], lab (assoc lab c id), n 0]
                    (if (empty? stack)
                      [lab n]
                      (let [cur (peek stack)
                            nbrs (for [d NEI
                                       :let [nk [(+ (first cur) (first d)) (+ (second cur) (second d))]]
                                       :when (and (cell-set nk) (not (contains? lab nk)) (= (col nk) color))]
                                   nk)]
                        (recur (into (pop stack) nbrs)
                               (reduce #(assoc %1 %2 id) lab nbrs)
                               (inc n)))))]
              (recur (rest todo) lab2 (assoc size id n) (inc id)))))))))

(defn majority [hand]
  (let [cnt (reduce (fn [m c] (if (contains? m c) (update m c inc) m))
                    (zipmap (order) (repeat 0)) hand)]
    (reduce (fn [best c] (if (> (cnt c) (cnt best)) c best)) (first (order)) (order))))

;; ── genome ───────────────────────────────────────────────────────────────────

(def genes
  {:cmyFocus [-1 2] :rgbFocus [-1 2] :colorLock [0 1] :mudRush [0 2]
   :blankPriority [-1 2] :capPriority [-1 2] :edgeCenterPref [-1 1]
   :bridgeWeight [0 3] :growRegion [0 2] :targetDraw [0 3] :anyDraw [0 2]
   :spendTargetPen [0 3] :dryAversion [0 3] :mudAversion [0 3]
   :earlySwap [0 2] :lateSwap [0 2] :cycleLock [0 2]})

(def gene-keys (vec (keys genes)))

(defn clamp-gene [k v] (let [[lo hi] (genes k)] (max lo (min hi v))))

(defn random-genome [^java.util.Random rng]
  (into {} (map (fn [k] (let [[lo hi] (genes k)] [k (+ lo (* (- hi lo) (rnd rng)))])) gene-keys)))

(defn default-genome []
  (merge (zipmap gene-keys (repeat 0))
         {:anyDraw 1 :targetDraw 2 :mudAversion 2 :dryAversion 1.5
          :growRegion 1 :spendTargetPen 1.5 :blankPriority 1 :bridgeWeight 1
          :lateSwap 1.2 :earlySwap 0.2}))

(defn- g [genome k] (get genome k 0))

(defn choose-lock-target [hand genome]
  (let [cnt (reduce (fn [m c] (if (contains? m c) (update m c inc) m))
                    (zipmap (order) (repeat 0)) hand)
        prim (set PRIMARIES) sec (set SECONDARIES)]
    (first
     (reduce (fn [[best bv] c]
               (let [s (+ (cnt c)
                          (if (prim c) (g genome :cmyFocus) 0)
                          (if (sec c) (g genome :rgbFocus) 0))]
                 (if (> s bv) [c s] [best bv])))
             [nil -1e9] (order)))))

;; ── presented choice set ─────────────────────────────────────────────────────

(defn enumerate-moves [G pi]
  (let [p (get-in G [:players pi])
        w (wedge-of p (:turn G))
        opts (legal-cells (:stacks G) w)]
    (if (or (empty? opts) (empty? (:hand p)))
      []
      (let [uniq (->> (distinct (:hand p)) (remove #(= % (:lastPlaced p))) vec)]
        (vec (for [c opts chit uniq] {:c c :chit chit :k c}))))))

(defn can-pass [G pi]
  (let [p (get-in G [:players pi])]
    (cond
      (not (:passedLast p)) true
      (zero? (count (:hand p))) true
      (zero? (count (enumerate-moves G pi))) true
      :else false)))

;; ── swaps (decision helpers) ─────────────────────────────────────────────────

(defn game-progress [G] (if (:ending G) 1.0 (min 1.0 (/ (:turn G) 20.0))))
(defn swap-drive [G genome] (let [t (game-progress G)] (+ (* (g genome :earlySwap) (- 1 t)) (* (g genome :lateSwap) t))))

(def ^:const SWAP_THRESH 2.0)

(defn best-swap
  "Best legal swap to gain a NEW in-stock color without dropping a held color to 0."
  [hand reg bag target mudded]
  (let [cnt (reduce (fn [m c] (if (contains? m c) (update m c inc) m))
                    (zipmap (order) (repeat 0)) hand)
        consider
        (fn [best type discards gc]
          (if (or (nil? gc) (not (> (get bag gc 0) 0)) (> (cnt gc) 0))
            best
            (let [after (reduce (fn [a d] (when a (if (> (a d 0) 0) (update a d dec) nil))) cnt discards)]
              (if (or (nil? after) (some #(= 0 (after % 0)) discards))
                best
                (let [gain (+ (get reg gc 0) (if (= gc target) 0.25 0))]
                  (if (and (> gain 0) (or (nil? best) (> gain (:gain best))))
                    {:type type :discards (vec discards) :get gc :gain gain}
                    best))))))]
    (if mudded
      (let [b1 (reduce (fn [b x] (consider b "opposite" [x] (wheel-opposite x))) nil (order))]
        (reduce (fn [b x]
                  (reduce (fn [bb nb] (consider bb "adjacent" [x] nb)) b (wheel-neighbors x)))
                b1 (order)))
      (let [held (filterv #(> (cnt %) 0) (order))
            b1 (reduce (fn [b x] (if (>= (cnt x) 3) (consider b "opposite" [x x] (wheel-opposite x)) b)) nil (order))]
        (reduce (fn [b [i a]]
                  (reduce (fn [bb b2] (consider bb "between" [a b2] (wheel-between a b2)))
                          b (subvec held (inc i))))
                b1 (map-indexed vector held))))))

(defn- distinct-by [f coll]
  (let [seen (volatile! #{})]
    (filterv (fn [x] (let [k (f x)] (when-not (@seen k) (vswap! seen conj k) true))) coll)))

(defn available-swaps
  "Every legal swap available to a hand (for the human UI)."
  [hand bag stacks mudded]
  (let [cnt (reduce (fn [m c] (if (contains? m c) (update m c inc) m))
                    (zipmap (order) (repeat 0)) hand)
        reg (largest-regions stacks)
        mk (fn [type discards gc]
             (when gc
               {:key (str (str/join "+" (sort discards)) ">" gc ":" type)
                :pi 0 :type type :discards (vec discards) :get gc
                :playable (> (get bag gc 0) 0)
                :gain (+ (get reg gc 0) (if (> (cnt gc) 0) 0 0.01))}))
        raw (if mudded
              (concat
               (for [x (order) :when (>= (cnt x) 1)] (mk "opposite" [x] (wheel-opposite x)))
               (for [x (order) :when (>= (cnt x) 1) nb (wheel-neighbors x)] (mk "adjacent" [x] nb)))
              (let [held (filterv #(> (cnt %) 0) (order))]
                (concat
                 (for [x (order) :when (>= (cnt x) 2)] (mk "opposite" [x x] (wheel-opposite x)))
                 (for [[i a] (map-indexed vector held) b (subvec held (inc i))] (mk "between" [a b] (wheel-between a b))))))]
    (->> raw (remove nil?) (distinct-by :key)
         (sort (fn [a b] (let [pd (compare (boolean (:playable b)) (boolean (:playable a)))]
                           (if (zero? pd) (compare (:gain b) (:gain a)) pd))))
         vec)))

;; ── bot selector (pure) ──────────────────────────────────────────────────────

(defn decide
  "Pure weighted bot selector. Returns {:c [q r] :chit color} or nil.
   rng provides the tie-break jitter."
  [G pi ^java.util.Random rng]
  (let [p (get-in G [:players pi])
        genome (:g p)
        moves (enumerate-moves G pi)]
    (if (empty? moves)
      nil
      (let [tgt (if (> (g genome :colorLock) 0.5) (:fixed p) (majority (:hand p)))
            cnt (frequencies (:hand p))
            ord (order)
            tp (.indexOf ord tgt)
            ingredients (set (remove nil? [(wheel-opposite tgt) (nth ord (mod (+ tp 5) 6)) (nth ord (mod (+ tp 1) 6))]))
            drv (swap-drive G genome)
            {:keys [col lab size]} (label-regions (:stacks G))
            bt (bag-total (:bag G))
            prim (set PRIMARIES) sec (set SECONDARIES)]
        (->> moves
             (reduce
              (fn [[best bscore] {:keys [c chit k]}]
                (let [st (get-in G [:stacks k] [])
                      cap (capacity k)
                      rr (classify (mix-stack (conj st chit)))
                      s0 (if (= rr "mud")
                           (+ (* -8 (g genome :mudAversion)) (* 6 (g genome :mudRush)))
                           (let [scarce (/ (get-in G [:bag rr] 0) (max 1 bt))
                                 adj (reduce
                                      (fn [acc d]
                                        (let [nk [(+ (first c) (first d)) (+ (second c) (second d))]]
                                          (if (and (cell-set nk) (= (col nk) rr) (>= (get lab nk -1) 0)
                                                   (not (contains? (:ids acc) (lab nk))))
                                            (-> acc (update :ids conj (lab nk)) (update :count inc)
                                                (update :size + (get size (lab nk) 0)))
                                            acc)))
                                      {:ids #{} :count 0 :size 0} NEI)]
                             (cond-> (g genome :anyDraw)
                               (= rr tgt) (+ (g genome :targetDraw))
                               (= chit tgt) (- (g genome :spendTargetPen))
                               (prim rr) (+ (g genome :cmyFocus))
                               (sec rr) (+ (g genome :rgbFocus))
                               (<= (get-in G [:bag rr] 0) 0) (- (* 3 (g genome :dryAversion)))
                               (and (> (get-in G [:bag rr] 0) 0) (< scarce 0.06)) (- (g genome :dryAversion))
                               (and (= rr tgt) (> (:size adj) 0)) (+ (* (g genome :growRegion) (/ (min (:size adj) 12) 4.0)))
                               (>= (:count adj) 2) (+ (* (g genome :bridgeWeight) (dec (:count adj)))))))
                      s1 (cond-> s0
                           (and (> drv 0.5) (ingredients chit) (<= (get cnt chit 0) 2)) (- (* 0.3 drv))
                           (zero? (count st)) (+ (g genome :blankPriority))
                           (= (count st) (dec cap)) (+ (g genome :capPriority)))
                      s2 (+ s1 (* (g genome :edgeCenterPref) (- (/ (ring k) (double max-ring)) 0.5) 2)
                            (if (and (> (g genome :cycleLock) 0) (= chit (:lastPlaced2 p))) (g genome :cycleLock) 0)
                            (* (rnd rng) 0.01))]
                  (if (> s2 bscore) [{:c c :chit chit} s2] [best bscore])))
              [nil -1e18])
             first)))))

;; ── mutator: apply one placement ─────────────────────────────────────────────

(defn apply-placement
  "THE single mutator. Returns [G' placement-record]."
  [G pi move]
  (let [c (:c move) color (:chit move) k c
        st (get-in G [:stacks k] [])
        prev (if (seq st) (classify (mix-stack st)) "white")
        hand (get-in G [:players pi :hand])
        idx (.indexOf hand color)
        hand' (if (>= idx 0) (vec (concat (subvec hand 0 idx) (subvec hand (inc idx)))) hand)
        p (get-in G [:players pi])
        G' (-> G
               (assoc-in [:players pi :hand] hand')
               (assoc-in [:players pi :lastPlaced2] (:lastPlaced p))
               (assoc-in [:players pi :lastPlaced] color)
               (update-in [:stacks k] (fnil conj []) color))
        rr (result-of (get-in G' [:stacks k]))]
    [G' {:pi pi :k k :c c :color color :prev prev :res (:color rr)
         :drew nil :dry nil :mismatch (not= (:color rr) (:check rr))}]))

;; ── resolve draws ────────────────────────────────────────────────────────────

(defn resolve-draws
  "Phase 2: resolve all draws together. Returns [G' placements']."
  [G placements]
  (let [palette-colors (set (order))
        demand (reduce (fn [m pp] (if (palette-colors (:res pp)) (update m (:res pp) (fnil conj []) pp) m))
                       {} placements)
        ;; first satisfy/deny color draws
        [G2 drew-map dry-map]
        (reduce
         (fn [[G drew dry] [c claim]]
           (let [avail (get-in G [:bag c] 0)]
             (if (>= avail (count claim))
               [(reduce (fn [g pp]
                          (-> g (update-in [:bag c] dec)
                              (update-in [:players (:pi pp) :hand] (fnil conj []) c)))
                        G claim)
                (reduce (fn [d pp] (assoc d (:pi pp) c)) drew claim)
                dry]
               (let [kind (if (and (= avail 0) (= (count claim) 1)) "empty" "conflict")]
                 [(reduce (fn [g pp] (-> g (update-in [:players (:pi pp) :dry] inc)
                                         (update-in [:dryEvents (keyword kind)] (fnil inc 0))))
                          G claim)
                  drew
                  (reduce (fn [d pp] (assoc d (:pi pp) kind)) dry claim)]))))
         [G {} {}] demand)
        ;; mud results
        [G3 dry-map]
        (reduce (fn [[G dry] pp]
                  (if (= (:res pp) "mud")
                    [(-> G (update-in [:players (:pi pp) :dry] inc)
                         (update-in [:dryEvents :mud] (fnil inc 0)))
                     (assoc dry (:pi pp) "mud")]
                    [G dry]))
                [G2 dry-map] placements)
        placements' (mapv (fn [pp] (assoc pp :drew (get drew-map (:pi pp)) :dry (get dry-map (:pi pp)))) placements)]
    [G3 placements']))

;; ── swap decision + resolution ───────────────────────────────────────────────

(defn decide-swap [G pi reg mudded]
  (let [p (get-in G [:players pi]) genome (:g p)]
    (cond
      mudded
      (let [swm (best-swap (:hand p) reg (:bag G) (:target p) true)]
        (when (and swm (>= (:gain swm) 1)) {:pi pi :type (:type swm) :discards (:discards swm) :get (:get swm)}))
      :else
      (let [blocked (and (> (count (:hand p)) 0)
                         (zero? (count (enumerate-moves G pi)))
                         (> (count (legal-cells (:stacks G) (wedge-of p (:turn G)))) 0))]
        (if blocked
          (let [rescue (or (best-swap (:hand p) reg (:bag G) (:target p) false)
                           (let [rc (frequencies (:hand p))]
                             (some (fn [x] (let [opp (wheel-opposite x)]
                                             (when (and (>= (get rc x 0) 2) opp (> (get-in G [:bag opp] 0) 0))
                                               {:type "opposite" :discards [x x] :get opp})))
                                   (order))))]
            (when rescue {:pi pi :type (:type rescue) :discards (:discards rescue) :get (:get rescue)}))
          (when (>= (count (:hand p)) 4)
            (let [drive (swap-drive G genome)]
              (when (> drive 0)
                (let [sw (best-swap (:hand p) reg (:bag G) (:target p) false)]
                  (when (and sw (>= (:gain sw) 1) (>= (* (:gain sw) drive) SWAP_THRESH))
                    {:pi pi :type (:type sw) :discards (:discards sw) :get (:get sw)}))))))))))

(defn resolve-swaps
  "Resolve all swaps together. Returns [G' granted denied]."
  [G plans]
  (let [demand (reduce (fn [m s] (update m (:get s) (fnil conj []) s)) {} plans)]
    (reduce
     (fn [[G granted denied] [c claim]]
       (if (>= (get-in G [:bag c] 0) (count claim))
         (let [G' (reduce
                   (fn [g s]
                     (let [hand (get-in g [:players (:pi s) :hand])
                           hand' (reduce (fn [h d] (let [i (.indexOf h d)]
                                                     (if (>= i 0) (vec (concat (subvec h 0 i) (subvec h (inc i)))) h)))
                                         hand (:discards s))]
                       (-> g
                           (assoc-in [:players (:pi s) :hand] (conj hand' c))
                           (update-in [:bag c] dec)
                           (update-in [:swapEvents (keyword (:type s))] (fnil inc 0))
                           (update-in [:players (:pi s) :swaps] (fnil inc 0))
                           (update-in [:players (:pi s) :discarded] (fnil + 0) (count (:discards s))))))
                   G claim)]
           [G' (into granted claim) denied])
         [(update-in G [:swapEvents :denied] (fnil + 0) (count claim)) granted (into denied claim)]))
     [G [] []] demand)))

;; ── game construction ────────────────────────────────────────────────────────

(defn new-game
  "playerSpecs: [{:isBot bool :g genome-or-nil}]. settings: {:palette :CMY :depth 3
   :removed #{...} :seed int}. Returns a serializable game map."
  [player-specs {:keys [palette depth removed seed] :or {palette :CMY depth 3 removed #{} seed 0}}]
  (binding [*palette-key* palette *pal* (palettes palette) *depth* depth *removed* removed]
    (let [rng (make-rng seed)
          n (count player-specs)
          smap (seed-map)
          stacks (into {} (map (fn [c] [c (if (and (smap c) (not (removed c))) [(smap c)] [])]) cells))
          sc (seed-count)
          bag (into {} (map (fn [c] [c (- PER_COLOR (get sc c 0))]) (order)))
          bs (bases-for n)
          players0 (mapv (fn [spec i]
                           {:g (or (:g spec) (default-genome)) :hand [] :dry 0 :swaps 0 :discarded 0
                            :base (nth bs i) :isBot (boolean (:isBot spec)) :target nil :fixed nil
                            :lastPlaced nil :lastPlaced2 nil :passedLast false
                            :name (or (:name spec) (if (:isBot spec) (str "Bot " (inc i)) (str "Player " (inc i))))})
                         player-specs (range n))
          ;; deal START_HAND, round-robin, weighted draw
          [players bag]
          (reduce (fn [[ps bag] _]
                    (reduce (fn [[ps bag] i]
                              (let [[c bag'] (draw-weighted bag rng)]
                                (if c [(update-in ps [i :hand] conj c) bag'] [ps bag'])))
                            [ps bag] (range n)))
                  [players0 bag] (range START_HAND))
          players (mapv (fn [p] (let [f (choose-lock-target (:hand p) (:g p))]
                                  (assoc p :fixed f :target f)))
                        players)]
      {:palette palette :depth depth :removed removed :seed seed
       :N n :stacks stacks :bag bag :players players :turn 0
       :over false :ending false :finalTurn -1 :moves []
       :dryEvents {:mud 0 :empty 0 :conflict 0}
       :swapEvents {:opposite 0 :between 0 :denied 0}})))

;; ── one fully-autonomous turn (all bots) — used by tests/bench ───────────────

(defn step
  "One autonomous simultaneous turn (all players bots). Returns G'."
  ([G] (step G false))
  ([G record]
   (with-config G
     (let [rng (make-rng (turn-seed (:seed G) (:turn G)))
           ;; placements
           [G1 plc]
           (reduce (fn [[G plc] i]
                     (let [d (decide G i rng)]
                       (if d (let [[G' rec] (apply-placement G i d)] [G' (conj plc rec)]) [G plc])))
                   [G []] (range (:N G)))
           [G2 plc] (resolve-draws G1 plc)
           mudded (set (keep (fn [p] (when (= (:res p) "mud") (:pi p))) plc))
           reg (largest-regions (:stacks G2))
           plans (keep (fn [i] (decide-swap G2 i reg (boolean (mudded i)))) (range (:N G2)))
           [G3 granted _denied] (if (seq plans) (resolve-swaps G2 (vec plans)) [G2 [] []])
           placed (set (map :pi plc))
           G4 (reduce (fn [g j] (assoc-in g [:players j :passedLast] (not (placed j)))) G3 (range (:N G3)))
           G5 (cond-> G4
                record (update :moves conj
                               {:turn (:turn G4)
                                :plays (mapv #(select-keys % [:pi :k :color :prev :res :drew :dry]) plc)
                                :swaps (mapv #(select-keys % [:pi :type :discards :get]) granted)})
                true (update :turn inc))]
       (cond
         (:over G5) G5
         (and (:ending G5) (> (:turn G5) (:finalTurn G5))) (assoc G5 :over true)
         :else
         (let [max-dry (reduce max (map :dry (:players G5)))
               bt (bag-total (:bag G5))]
           (if (and (not (:ending G5)) (or (>= max-dry MUD_LIMIT) (zero? bt)))
             (assoc G5 :ending true :finalTurn (:turn G5))
             G5)))))))
