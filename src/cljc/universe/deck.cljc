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
