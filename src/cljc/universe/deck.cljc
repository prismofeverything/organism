(ns universe.deck
  "The UNIVERSE deck and its hand chart.

   Sixty cards on three axes -- 3 colors x 4 shapes x 5 numbers -- so the deck
   is a complete 3x4x5 lattice with every combination appearing exactly once.
   A card is its index into that lattice:

       id = color*20 + shape*5 + number

   which is the order the print pipeline uses (universe/deck.py), so card 17
   means the same card here, in `out/mpc/manifest.csv`, and on the printed
   sheet.

   A hand is five cards, and `chart` is the nineteen kinds of hand ranked by
   counted rarity.  Those counts are exact -- universe/hands.py enumerates all
   C(60,5) = 5,461,512 hands -- and `universe.deck-test` re-enumerates them to
   check this namespace reproduces the chart rather than approximating it.

   Strength runs 0 (dyad, the floor) to 17 (singularity).  There are nineteen
   rows but only eighteen strengths: a color tetrad and a shape split pentad
   are each 1 in 22,756, exactly equally rare, and that is the one place the
   chart ties -- the two shoulders at one height in out/universe-pyramid.svg."
  (:require
   [clojure.string :as string]))

;; ── The lattice ────────────────────────────────────────────────────────────

(def colors
  "Darkest to lightest.  The lightness order is what carries the axis -- read
   as greys these are 66, 119, 182 against paper at 255 -- which is why the
   deck survives any of the three dichromacies."
  [:purple :green :yellow])

(def shapes [:eye :helix :pyramid :star])

(def hex
  {:purple "#651694" :green "#26883F" :yellow "#E6AD24"})

(def card-count 60)
(def all-cards (vec (range card-count)))

(defn color-index  [card] (quot card 20))
(defn shape-index  [card] (quot (mod card 20) 5))
(defn number-index [card] (mod card 5))

(defn color  [card] (nth colors (color-index card)))
(defn shape  [card] (nth shapes (shape-index card)))
(defn number [card] (inc (number-index card)))       ; 1..5, as printed

(def ^:private color-position (zipmap colors (range)))
(def ^:private shape-position (zipmap shapes (range)))

(defn card
  "The card with this color, shape and number (number 1..5, as printed)."
  [color shape number]
  (+ (* 20 (color-position color))
     (* 5 (shape-position shape))
     (dec number)))

(defn describe
  "\"3 purple star\" -- how a card is said out loud."
  [card]
  (str (number card) " " (name (color card)) " " (name (shape card))))

;; ── The chart ──────────────────────────────────────────────────────────────
;;
;; Generated from universe/hands.json (see the scripts under universe/), which
;; is itself the exact enumeration.  Names follow the README's own scheme: a
;; group of matching cards is named for its size, a hand of two groups is
;; split, and the suit grade goes in front.

(def chart
  [{:strength 17 :suit :perfect :pattern [1 1 1 1 1] :name :singularity         :count      12}
   {:strength 16 :suit :color   :pattern [4 1]       :name :color-tetrad        :count     240}
   {:strength 16 :suit :shape   :pattern [3 2]       :name :shape-split-pentad  :count     240}
   {:strength 15 :suit :shape   :pattern [1 1 1 1 1] :name :shape-sequence      :count     960}
   {:strength 14 :suit :shape   :pattern [3 1 1]     :name :shape-triad         :count    1080}
   {:strength 13 :suit :color   :pattern [3 2]       :name :color-split-pentad  :count    1440}
   {:strength 12 :suit :color   :pattern [1 1 1 1 1] :name :color-sequence      :count    3060}
   {:strength 11 :suit :shape   :pattern [2 2 1]     :name :shape-split-tetrad  :count    3240}
   {:strength 10 :suit :mixed   :pattern [5]         :name :pentad              :count    3960}
   {:strength  9 :suit :color   :pattern [3 1 1]     :name :color-triad         :count    5760}
   {:strength  8 :suit :shape   :pattern [2 1 1 1]   :name :shape-dyad          :count    6480}
   {:strength  7 :suit :color   :pattern [2 2 1]     :name :color-split-tetrad  :count   12960}
   {:strength  6 :suit :color   :pattern [2 1 1 1]   :name :color-dyad          :count   23040}
   {:strength  5 :suit :mixed   :pattern [4 1]       :name :tetrad              :count  118560}
   {:strength  4 :suit :mixed   :pattern [1 1 1 1 1] :name :sequence            :count  244800}
   {:strength  3 :suit :mixed   :pattern [3 2]       :name :split-pentad        :count  288720}
   {:strength  2 :suit :mixed   :pattern [3 1 1]     :name :triad               :count  943560}
   {:strength  1 :suit :mixed   :pattern [2 2 1]     :name :split-tetrad        :count 1551960}
   {:strength  0 :suit :mixed   :pattern [2 1 1 1]   :name :dyad                :count 2251440}])

(def total-hands
  "C(60,5).  The chart's counts sum to this, which is what makes it a chart
   and not a list of opinions."
  5461512)

(def ^:private chart-index
  (into {} (map (juxt (juxt :pattern :suit) identity) chart)))

(defn hand-name
  "\"shape split pentad\" -- the keyword said out loud."
  [row]
  (string/replace (name (:name row)) "-" " "))

;; ── Classifying a hand ─────────────────────────────────────────────────────

(defn suit-grade
  "The smallest axis-aligned sub-block of the lattice the hand fits inside:
   one color and one shape (5 cards) is :perfect, one color (20) is :color,
   one shape (15) is :shape, otherwise :mixed.

   Color and shape are incomparable -- neither implies the other -- so this is
   a diamond, not a ladder.  The chart orders the grades by rarity instead."
  [cards]
  (let [one-color? (apply = (map color-index cards))
        one-shape? (apply = (map shape-index cards))]
    (cond
      (and one-color? one-shape?) :perfect
      one-color?                  :color
      one-shape?                  :shape
      :else                       :mixed)))

(defn number-groups
  "The hand's numbers as [size number] pairs, biggest group first and the
   higher number first within a size -- which is the order poker compares
   them in.  Five is high.

   With only five numbers in the deck, five cards with no repeat are already
   the whole run 1-2-3-4-5, so there is no high-card hand to fall through to."
  [cards]
  (->> cards
       (map number)
       frequencies
       (map (fn [[n size]] [size n]))
       (sort #(compare %2 %1))
       vec))

(defn number-pattern
  "Just the group sizes, biggest first -- the key into the chart."
  [cards]
  (mapv first (number-groups cards)))

(defn classify
  "The chart row for five cards."
  [cards]
  (get chart-index [(number-pattern cards) (suit-grade cards)]))

(defn describe-hand
  "The hand said the way a player says it out loud: the chart name, then the
   numbers that actually make it.

     \"dyad of 2s\"                    one pair
     \"split tetrad, 4s and 2s\"       two groups of the same size
     \"split pentad, 3s over 5s\"      two groups of different sizes
     \"color tetrad of 3s\"            the suit grade rides in front
     \"sequence\"                      nothing repeats, so there is nothing to name

   Equal groups are joined with \"and\" and unequal ones with \"over\", so the
   bigger group is never in doubt. Kickers are left out: at a showdown the five
   cards are face up anyway, and with only five numbers in the deck the label
   would be longer than the hand."
  [cards]
  (let [row    (classify cards)
        groups (filterv (fn [[size _]] (> size 1)) (number-groups cards))]
    (case (count groups)
      0 (hand-name row)
      1 (str (hand-name row) " of " (second (first groups)) "s")
      (let [[[size-a number-a] [size-b number-b]] groups]
        (str (hand-name row) ", " number-a "s "
             (if (= size-a size-b) "and" "over") " "
             number-b "s")))))

;; ── Comparing two hands ────────────────────────────────────────────────────

(def ^:private color-rank
  "Purple high, then green, then yellow."
  {0 2, 1 1, 2 0})

(defn- number-key
  "The number groups flattened and padded to a fixed width, so hands compare
   left to right with no dependence on how many groups they have.  Clojure
   compares vectors by length before contents, and a hand with fewer, larger
   groups is a better hand, not a shorter one -- padding is what keeps that
   from being read backwards."
  [cards]
  (vec (take 10 (concat (apply concat (number-groups cards)) (repeat 0)))))

(defn- color-key
  "The five colors, purple first.  Shape is deliberately absent: the deck
   orders its three colors by lightness but gives its four marks no order at
   all, so shape never breaks a tie."
  [cards]
  (vec (sort > (map (comp color-rank color-index) cards))))

(defn value
  "How good five cards are, as something `compare` understands -- bigger is
   better, read left to right:

     1. the chart strength,
     2. the numbers, groups first and five high, exactly as poker reads them,
     3. the colors, purple high.

   Hands equal on all three tie, and the pot splits."
  [cards]
  [(:strength (classify cards))
   (number-key cards)
   (color-key cards)])

(declare best-five)

;; ── Best five of seven ─────────────────────────────────────────────────────
;;
;; The seven-card table deals two to you and five to the board and keeps your
;; best five. That game ranks its hands by a chart of its own, universe/hands7.py:
;; by how rarely seven cards hold each hand, whatever else they hold. Ranking by
;; how often a hand is *kept* instead chases its own tail and buries hands -- a
;; split pentad is common in seven cards, sinks below split tetrad, and every
;; split pentad holds a split tetrad, so nobody would ever keep one.
;;
;; The thirteen colored and shaped rows keep their five-card order exactly; only
;; the plain rows at the foot move, and the one five-card tie comes apart. Two
;; rows can never be anyone's best five: seven cards holding a triad or a split
;; tetrad always hold something better. See out/universe-pyramid-seven.svg.

(def seven-order
  "Strongest first."
  [:singularity :color-tetrad :shape-split-pentad :shape-sequence :shape-triad
   :color-split-pentad :color-sequence :shape-split-tetrad :pentad :color-triad
   :shape-dyad :color-split-tetrad :color-dyad :tetrad :sequence :split-pentad
   :dyad :triad :split-tetrad])

(def ^:private seven-strength
  (zipmap seven-order (range (dec (count seven-order)) -1 -1)))

(def seven-top
  "The strongest seven-card strength, for scaling one to a fraction."
  (dec (count seven-order)))

(def seven-odds
  "How often each row is the best five a seven-card player keeps, as 1 in N.
   Sampled over ten million deals by universe/hands7.py, so the rare end is
   good to a few percent; triad and split tetrad never happen."
  {:singularity 21277 :color-tetrad 1444 :shape-split-pentad 1113
   :shape-sequence 327 :shape-triad 330 :color-split-pentad 200
   :color-sequence 115 :shape-split-tetrad 105 :pentad 85 :color-triad 78
   :shape-dyad 61 :color-split-tetrad 31.4 :color-dyad 22.2 :tetrad 10.1
   :sequence 4.7 :split-pentad 2.6 :dyad 6.6})

(defn seven-row
  "The chart row for the best five of these cards on the seven-card table,
   carrying that table's strength and the five it is made of."
  [cards]
  (when-let [five (best-five cards)]
    (let [row (classify five)]
      (assoc row :strength (seven-strength (:name row)) :best five))))

(defn value-seven
  "`value` for the seven-card table: the same number and color tiebreaks, under
   the seven-card chart's strengths."
  [cards]
  (assoc (value cards) 0 (seven-strength (:name (classify cards)))))

;; The fast path. An equity player values tens of thousands of seven-card
;; deals per decision, each one twenty-one five-card subsets, and the readable
;; `value-seven` -- frequency maps, sorts, vector compares -- is too slow for
;; that. So a five-card hand is scored as a single integer instead, from bit
;; masks and a count per number, packed so that comparing the integers is
;; comparing `value-seven`:
;;
;;   strength (0..18), then the ten number-key digits (base 6), then the five
;;   color-key digits (base 3)
;;
;; which fits in 2^53, so it is exact in a JavaScript number too.
;; universe.seven-test checks the two agree.

(def ^:private card-color  (int-array (map color-index all-cards)))
(def ^:private card-shape  (int-array (map shape-index all-cards)))
(def ^:private card-number (int-array (map number-index all-cards)))

(def ^:private pattern-code
  "Group sizes, biggest first, as base-10 digits: [2 1 1 1] is 21110."
  (fn [pattern]
    (reduce (fn [code size] (+ (* 10 code) size)) 0
            (take 5 (concat pattern (repeat 0))))))

(def ^:private suit-code {:perfect 0 :color 1 :shape 2 :mixed 3})

(def ^:private strength-by-code
  (delay
    (into {} (for [{:keys [pattern suit name]} chart]
               [(+ (* 4 (pattern-code pattern)) (suit-code suit))
                (seven-strength name)]))))

(defn- one-bit? [m] (zero? (bit-and m (dec m))))

(defn seven-code
  "`value-seven` of five cards as one integer: bigger is better, equal ties."
  [a b c d e]
  (let [counts (int-array 5)
        cards [a b c d e]]
    (loop [i 0 colors 0 shapes 0 purple 0 green 0]
      (if (< i 5)
        (let [card (nth cards i)
              color (aget ^ints card-color card)]
          (aset ^ints counts (aget ^ints card-number card)
                (inc (aget ^ints counts (aget ^ints card-number card))))
          (recur (inc i)
                 (bit-or colors (bit-shift-left 1 color))
                 (bit-or shapes (bit-shift-left 1 (aget ^ints card-shape card)))
                 (if (== color 0) (inc purple) purple)
                 (if (== color 1) (inc green) green)))
        (let [suit (cond (and (one-bit? colors) (one-bit? shapes)) 0
                         (one-bit? colors) 1
                         (one-bit? shapes) 2
                         :else 3)
              ;; the sizes present, biggest first, as a pattern code, and the
              ;; number key: [size number] pairs by size then number, five high
              [pattern numbers]
              (loop [size 5 pattern 0 numbers 0 digits 0]
                (if (zero? size)
                  [pattern (loop [numbers numbers digits digits]
                             (if (< digits 10) (recur (* 6 numbers) (inc digits)) numbers))]
                  (let [[pattern numbers digits]
                        (loop [n 4 pattern pattern numbers numbers digits digits]
                          (if (neg? n)
                            [pattern numbers digits]
                            (if (== size (aget ^ints counts n))
                              (recur (dec n) (+ (* 10 pattern) size)
                                     (+ (* 36 numbers) (* 6 size) (inc n)) (+ digits 2))
                              (recur (dec n) pattern numbers digits))))]
                    (recur (dec size) pattern numbers digits))))
              pattern (loop [p pattern] (if (< p 10000) (recur (* 10 p)) p))
              ;; the colors purple first: purple ranks 2, green 1, yellow 0
              color-key (loop [key 0 i 0]
                          (if (< i 5)
                            (recur (+ (* 3 key) (cond (< i purple) 2
                                                      (< i (+ purple green)) 1
                                                      :else 0))
                                   (inc i))
                            key))
              strength (get @strength-by-code (+ (* 4 pattern) suit))]
          (+ (* (+ (* strength 60466176) numbers) 243) color-key))))))

(def ^:private subsets
  "Every five-of-n choice of positions, for n five to seven."
  (into {} (for [n [5 6 7]]
             [n (vec (for [a (range n) b (range (inc a) n) c (range (inc b) n)
                           d (range (inc c) n) e (range (inc d) n)]
                       [a b c d e]))])))

(defn best-code
  "The best `seven-code` among every five of these cards."
  [cards]
  (let [cards (vec cards)]
    (reduce (fn [best [a b c d e]]
              (max best (seven-code (cards a) (cards b) (cards c) (cards d) (cards e))))
            -1 (subsets (count cards)))))

(defn best-five
  "The five of these cards that make the best seven-card-table hand. Five
   cards are already a hand; fewer are not one."
  [cards]
  (when (>= (count cards) 5)
    (let [cards (vec cards)]
      (mapv cards
            (apply max-key
                   (fn [[a b c d e]]
                     (seven-code (cards a) (cards b) (cards c) (cards d) (cards e)))
                   (subsets (count cards)))))))

(defn compare-hands
  "Negative when a loses, positive when a wins, zero when the pot splits."
  [a b]
  (compare (value a) (value b)))

(defn winners
  "The indices of the best hands in `hands`, which is more than one on a split."
  [hands]
  (if (empty? hands)
    []
    (let [values (mapv value hands)
          best   (reduce (fn [a b] (if (neg? (compare a b)) b a)) values)]
      (vec (keep-indexed (fn [i v] (when (= v best) i)) values)))))
