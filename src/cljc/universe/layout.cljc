(ns universe.layout
  "Where everything on the poker table goes, worked out from the space it has.

   This exists because hand-tuning the table one number at a time does not
   converge: making your own cards bigger pushes the board down, which makes
   the other players' seats collide, which makes their cards smaller, and the
   next fix undoes the last one. So nothing here is a pixel somebody chose.
   There is exactly one free variable -- a scale -- and everything else is
   derived from it and from the area the table was given.

   The model:

     * Every nameplate sits ON the ring. Your cards hang DOWN from yours,
       into the felt toward the board; everybody else carries theirs ABOVE.
     * The board sits at the centre of the ring.
     * The ring's vertical radius is whatever the height allows, and must be
       at least enough for your cards to hang from the top of it to the board.
     * Its horizontal radius is whatever the width allows, and must be at
       least enough for the side seats to clear the board.

   `solve` bisects the scale for the largest set of cards that breaks none of
   that, and returns finished rectangles -- so the renderer places what it is
   told and `universe.layout-test` checks the very same numbers that get drawn."
  )

;; ── The furniture, in pixels ───────────────────────────────────────────────

(def pad         "breathing room at the edge of the table area" 10)
(def plate-h     "a nameplate"                                   30)
(def card-gap    "nameplate to cards"                             8)
(def tail        "status line, bet and hand name, always reserved" 54)
(def margin      "between any two things that must not touch"     14)
(def plate-w     "a nameplate, whatever size its cards are"      150)
(def board-tail  "the pot line and the winner line under the board" 74)
(def card-ratio  "a card is this many times as tall as it is wide" 1.4)

(def proportions
  "Your hand, the board, everybody else. The ratio is the point; the scale is
   whatever the room allows. Your hand reads as the important one without
   dwarfing the board it has to be read against."
  {:yours 2.1 :board 1.5 :others 1.0})

(def rail-width     "the status and chat column down the right" 340)
(def top-gap        "a little air above the table"                16)
(def action-height  "the hand name and the buttons below it"     92)

(defn table-area
  "What is left of a `vw` by `vh` window once the rail, the header and the
   action bar have taken theirs. Kept here rather than in the view so the
   layout tests measure the same table the page draws."
  [vw vh]
  {:w (- vw rail-width 40) :h (- vh top-gap action-height)})

(def ^:private min-scale
  "The floor, not a target. Below this the rosettes stop being countable at a
   glance, which is the one job the middle of a card has -- so rather than
   shrink past it to fit a cramped window, the table keeps this size and grows
   taller, and the page scrolls. Small screen, some scrolling; never a table
   nobody can read."
  50.0)

;; ── Portable maths ─────────────────────────────────────────────────────────

(defn- sin [x] #?(:clj (Math/sin x) :cljs (js/Math.sin x)))
(defn- cos [x] #?(:clj (Math/cos x) :cljs (js/Math.cos x)))
(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))
(defn- round [x] #?(:clj (Math/round (double x)) :cljs (js/Math.round x)))

;; ── Pieces ─────────────────────────────────────────────────────────────────

(defn card-sizes [scale]
  (into {} (map (fn [[k v]] [k (max 1 (round (* v scale)))])) proportions))

(defn seat-box
  "One seat's rectangle, with its nameplate centred on [x y]. `below?` hangs
   the cards under the nameplate instead of above it."
  [x y card-w below?]
  (let [card-h (* card-ratio card-w)
        w      (max (+ (* 2 card-w) 8) plate-w)
        top    (if below?
                 (- y (/ plate-h 2))
                 (- y (/ plate-h 2) card-gap card-h))
        bottom (if below?
                 (+ y (/ plate-h 2) card-gap card-h tail)
                 (+ y (/ plate-h 2) tail))]
    {:x x :y y :below? below? :card card-w
     :left (- x (/ w 2)) :top top :width w :height (- bottom top)
     :right (+ x (/ w 2)) :bottom bottom}))

(defn- geometry
  "Place the ring and the board for one set of card sizes, or nil when the area
   cannot hold them."
  [w h {:keys [yours board others]}]
  (let [board-w (+ (* 3 board) 18)
        board-h (* card-ratio board)
        ;; tall enough for your cards to hang from the top of the ring to the
        ;; board, and for the lowest seat's cards to reach up to it from below
        ry-min  (max (+ (/ board-h 2) margin tail (* card-ratio yours)
                        card-gap (/ plate-h 2))
                     (+ (/ board-h 2) board-tail margin (* card-ratio others)
                        card-gap (/ plate-h 2)))
        ;; take all the height there is rather than the least that works: a
        ;; short ring stacks the two seats on each flank on top of each other
        ry      (/ (- h plate-h tail (* 2 pad)) 2)
        seat-w  (max (+ (* 2 others) 8) plate-w)
        your-w  (max (+ (* 2 yours) 8) plate-w)
        rx-min  (+ (/ board-w 2) (/ seat-w 2) margin)
        rx      (- (/ w 2) pad (/ seat-w 2))
        cy      (+ pad (/ plate-h 2) ry)]
    (when (and (>= ry ry-min) (>= rx rx-min) (<= (/ your-w 2) (- (/ w 2) pad)))
      {:cx (/ w 2) :cy cy :rx rx :ry ry
       :board-w board-w :board-h board-h
       :height (+ cy ry (/ plate-h 2) tail pad)})))

(defn- seats
  "Every seat's rectangle. Index 0 is you, at the top; the rest run clockwise.

   Your cards hang down toward the board and everybody else's sit above their
   nameplate, uniformly. Facing every seat's cards toward the board reads
   better but cannot be done: that rule flips across the horizontal, so the two
   seats either side of it grow their cards straight into each other."
  [{:keys [cx cy rx ry]} n sizes]
  (into [(seat-box cx (- cy ry) (:yours sizes) true)]
        (for [i (range 1 n)
              :let [t (* 2 pi (/ (double i) n))]]
          (seat-box (+ cx (* rx (sin t)))
                    (- cy (* ry (cos t)))
                    (:others sizes)
                    false))))

(defn- board-rect [{:keys [cx cy board-w board-h]}]
  {:left (- cx (/ board-w 2)) :top (- cy (/ board-h 2))
   :right (+ cx (/ board-w 2)) :bottom (+ cy (/ board-h 2) board-tail)})

(defn overlap?
  "Do two rectangles come within `gap` of each other?"
  ([a b] (overlap? a b margin))
  ([a b gap]
   (not (or (<= (+ (:right a) gap) (:left b))
            (<= (+ (:right b) gap) (:left a))
            (<= (+ (:bottom a) gap) (:top b))
            (<= (+ (:bottom b) gap) (:top a))))))

(defn- fits?
  [w h n sizes]
  (when-let [g (geometry w h sizes)]
    (let [ss (seats g n sizes) board (board-rect g)]
      (when (and (every? (fn [s] (and (>= (:left s) -1) (>= (:top s) -1)
                                      (<= (:right s) (inc w))
                                      (<= (:bottom s) (inc (:height g)))))
                         ss)
                 (not-any? (fn [s] (overlap? s board 0)) ss)
                 (every? (fn [[i j]] (not (overlap? (nth ss i) (nth ss j))))
                         (for [i (range n) j (range (inc i) n)] [i j])))
        (assoc g :sizes sizes :seats ss :board board)))))

;; ── The one knob ───────────────────────────────────────────────────────────

(defn- best-fit
  "Bisect the scale for the largest set of cards that fits, or nil."
  [w h n lo]
  (loop [lo lo hi 140.0 best nil steps 0]
    (if (> steps 28)
      best
      (let [mid (/ (+ lo hi) 2)]
        (if-let [got (fits? w h n (card-sizes mid))]
          (recur mid hi got (inc steps))
          (recur lo mid best (inc steps)))))))

(defn- grow-height
  "Keep the cards this size and ask the page for more height until they fit."
  [w h n sizes]
  (loop [h* h steps 0]
    (if-let [got (fits? w h* n sizes)]
      got
      (when (< steps 40) (recur (+ h* 40) (inc steps))))))

(defn solve
  "The largest cards that fit `n` players into a `w` by `h` area, in three
   preferences:

     1. the biggest readable set that fits as it stands;
     2. failing that, the readable floor with more height -- the page scrolls,
        which costs the reader a scroll rather than the cards;
     3. failing even that, below the floor. A narrow window with nine players
        is beaten by its own width: no amount of height separates seats that
        collide side to side, so there the cards do have to give.

   Always returns a layout, with the `:height` it actually needs."
  [w h n]
  (or (best-fit w h n min-scale)
      (grow-height w h n (card-sizes min-scale))
      (best-fit w (+ h 1200) n 14.0)
      (grow-height w h n (card-sizes 14.0))
      ;; nothing is drawable at this width at all
      (assoc (or (geometry (max w 900) (max h 760) (card-sizes min-scale))
                 {:cx (/ w 2) :cy (/ h 2) :rx 1 :ry 1
                  :board-w 1 :board-h 1 :height h})
             :sizes (card-sizes min-scale)
             :seats []
             :board {:left 0 :top 0 :right 0 :bottom 0})))
