(ns organism.play
  (:require
   [clojure.set :as cset]
   [clojure.string :as string]
   [cljs.pprint :refer (pprint)]
   [cljs.reader :as reader]
   [goog.events :as events]
   [goog.history.EventType :as HistoryEventType]
   [reitit.core :as reitit]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.base :as base]
   [organism.game :as game]
   [organism.choice :as choice]
   [organism.board :as board]
   [organism.dom :as dom]
   [organism.ajax :as ajax]
   [organism.components :as components]
   [organism.websockets :as ws])
  (:import goog.History))

(def orange "rgb(225, 195, 61)")

(def ^:private word-pool
  ["amber" "arrow" "blade" "bloom" "briar" "brook" "cedar" "chain" "cliff"
   "coral" "crane" "crown" "dawn" "delta" "drift" "dusk" "echo" "ember"
   "fable" "fern" "flame" "flint" "frost" "gale" "ghost" "gleam" "grove"
   "haven" "hawk" "haze" "helm" "heron" "husk" "ivory" "jade" "knot"
   "larch" "lark" "leaf" "light" "lunar" "marsh" "mist" "moss" "nexus"
   "oak" "onyx" "orbit" "pearl" "pike" "plume" "pulse" "quartz" "raven"
   "reef" "ridge" "river" "root" "rune" "sage" "shard" "shell" "shore"
   "silk" "slate" "solar" "spark" "spire" "star" "steam" "stone" "storm"
   "swift" "thorn" "tide" "torch" "trail" "vale" "vault" "vine" "void"
   "wander" "wave" "weald" "wisp" "wolf" "wren" "zenith"])

(defn- generate-game-key []
  (let [pick #(nth word-pool (rand-int (count word-pool)))]
    (str (pick) "-" (pick) "-" (pick))))

(def font-choice
  "BlinkMacSystemFont,-apple-system,\"Segoe UI\",Roboto,Oxygen,Ubuntu,Cantarell,\"Fira Sans\",\"Droid Sans\",\"Helvetica Neue\",Helvetica,Arial,sans-serif")

(def possible-mutations
  {:COMMUNE "elements are are considered fed and mobile for movement if they are adjacent to at least two other fed elements"
   :PERSIST "elements are not lost to integrity unless the player has no living organisms remaining"
   :RAIN "the top two sides rain an increasing amount of neutral elements down upon your organisms"})


   ;; :BOOST "elements are mobile if they are adjacent to at least one other fed element"
   ;; :EXTRACT "the capturing element takes the food from captured element"
   ;; :ABSORB "any element lost to integrity that captured another element is added to that organism in place of the captured element"
   ;; :SKIP "start with 5 elements instead of 3"

(defn display-mutation
  [mutation-key mutation-description]
  (str
   (name mutation-key)
   " ➞ "
   mutation-description))

(defonce session (r/atom {:page :home}))
(defonce chat (r/atom []))

(defonce history-advance
  (r/atom nil))

(defonce player-order
  (r/atom
   (vec
    (take
     (count board/default-player-order)
     (repeat "")))))

(defonce player-captures-order
  (r/atom
   (vec
    (repeat
     (count board/default-player-order)
     board/default-player-captures))))

(defonce board-invocation
  (r/atom (board/empty-invocation
           (if (and (exists? js/playerKey) (not (empty? js/playerKey)))
             js/playerKey
             "orb"))))

(def empty-game-state
  {:game {}
   :created false
   :player js/playerKey
   :history []
   :cursor nil
   :board {}
   :turn :open
   :choices []})

(defonce game-state
  (r/atom empty-game-state))

(defonce clear-state
  (r/atom (game/initial-state board/default-player-order)))

(def empty-introduction
  {:chosen-space nil
   :chosen-element nil
   :progress {}})

(defonce introduction
  (r/atom empty-introduction))

;; Hover state for the choose-action-type phase: which element-type is hovered,
;; plus the [x y] of the hovered element so we can position the popup.
(defonce action-hover (r/atom nil))

;; Hover state for the choose-action phase: which "from" space is hovered
;; so we can show its destination spaces.
(defonce from-hover (r/atom nil))

;; Separate hover for an individual destination (so we can brighten it)
(defonce dest-hover (r/atom nil))

;; Persistent "space under the pointer" ({:space ... :kind :element|:food}).
;; Unlike action-hover/from-hover this is set on mouseenter and cleared only on
;; a real mouseleave (debounced) — never on click or state update. Each action
;; phase falls back to it so that after committing a choice the element still
;; under the cursor immediately offers its next options (no move-out-and-back-in).
(defonce pointer-space (r/atom nil))

;; In-progress grow payment, or nil. Food is a shared pool across all growers,
;; so the player spends the cost one coin at a time by clicking growers; when the
;; cost is fully paid we commit the matching precomputed variant.
;;   {:dest <space> :cost N :spent {grower-space amount} :variants [{:spent :state} ...]}
(defonce grow-pay (r/atom nil))

;; Pending timeout id for delayed clearing of from-hover on mouseleave
(defonce from-hover-timeout (atom nil))

;; ── Animation state ─────────────────────────────────────────────────────────
;; When a new state arrives, we hold the old state in `displayed-state` and
;; the new one in `target-state`, animating between them via `transition-progress`
;; (0.0 → 1.0). The board renders the old state with an overlay describing the
;; transitions in flight.
(defonce displayed-state    (r/atom nil))
(defonce target-state       (r/atom nil))
(defonce transition-progress (r/atom 1.0))
(defonce transition-token   (atom 0))  ;; cancels older RAF loops when a new transition starts
(defonce transition-duration 240)  ;; ms

(defn- diff-transitions
  "Detect what changed between two game states. Returns a vector of transition
   maps. Types:
   {:type :move            :from space :to space :element element}
   {:type :grow            :to space :element element}
   {:type :lose            :space space :element element}  ;; conflict/integrity
   {:type :circulate       :from space :to space :amount n :element-color c}
   {:type :food-up         :space space :amount n}          ;; e.g. eating
   {:type :food-down       :space space :amount n}
   {:type :free-food-appear :space space :amount n}
   {:type :free-food-vanish :space space :amount n}"
  [from-state to-state]
  (let [from-els   (:elements from-state)
        to-els     (:elements to-state)
        from-food  (:food from-state)
        to-food    (:food to-state)
        from-spaces (set (keys from-els))
        to-spaces   (set (keys to-els))
        new-spaces  (cset/difference to-spaces from-spaces)
        gone-spaces (cset/difference from-spaces to-spaces)
        common-spaces (cset/intersection from-spaces to-spaces)
        ;; Match up moves: a gone-space element matched with a new-space element
        ;; of the same player/organism/type. Each match consumes both.
        [move-pairs unmoved-gone unmoved-new]
        (reduce
         (fn [[pairs gs ns] s]
           (let [el (get from-els s)
                 match (first
                        (filter
                         (fn [ns-space]
                           (let [new-el (get to-els ns-space)]
                             (and new-el
                                  (= (:player el) (:player new-el))
                                  (= (:organism el) (:organism new-el))
                                  (= (:type el) (:type new-el)))))
                         ns))]
             (if match
               [(conj pairs {:from s :to match :element (get to-els match)})
                (disj gs s)
                (disj ns match)]
               [pairs gs ns])))
         [[] gone-spaces new-spaces]
         gone-spaces)
        ;; Food deltas on elements present in both states
        food-changes
        (for [s common-spaces
              :let [old-f (or (:food (get from-els s)) 0)
                    new-f (or (:food (get to-els s)) 0)
                    delta (- new-f old-f)
                    el (get from-els s)]
              :when (not (zero? delta))]
          {:space s :delta delta
           :player (:player el) :organism (:organism el)})
        ups   (vec (filter #(pos? (:delta %)) food-changes))
        downs (vec (filter #(neg? (:delta %)) food-changes))
        ;; Greedy matching of +N/−N pairs within same player+organism => circulate
        [circ-pairs remaining-ups remaining-downs]
        (reduce
         (fn [[circs us ds] u]
           (let [match (first
                        (filter
                         (fn [d]
                           (and (= (:player u)   (:player d))
                                (= (:organism u) (:organism d))
                                (= (:delta u)    (- (:delta d)))))
                         ds))]
             (if match
               [(conj circs {:from (:space match) :to (:space u)
                             :amount (:delta u)})
                (remove #{u} us)
                (remove #{match} ds)]
               [circs us ds])))
         [[] ups downs]
         ups)]
    (vec
     (concat
      ;; Moves
      (for [{:keys [from to element]} move-pairs]
        {:type :move :from from :to to :element element})
      ;; Lost (gone without a move match)
      (for [s unmoved-gone]
        {:type :lose :space s :element (get from-els s)})
      ;; Grown (new without a move match)
      (for [s unmoved-new]
        {:type :grow :to s :element (get to-els s)})
      ;; Circulate pairs (enriched with food counts so the animation can
      ;; compute the exact coin slot the food leaves from and arrives at)
      (for [c circ-pairs]
        (assoc c
               :type :circulate
               :from-food-before (or (:food (get from-els (:from c))) 0)
               :to-food-after    (or (:food (get to-els (:to c))) 0)))
      ;; Remaining food ups
      (for [u remaining-ups]
        {:type :food-up :space (:space u) :amount (:delta u)})
      ;; Remaining food downs
      (for [d remaining-downs]
        {:type :food-down :space (:space d) :amount (- (:delta d))})
      ;; Free food appearing
      (for [s (cset/difference (set (keys to-food)) (set (keys from-food)))
            :let [amt (get to-food s)]]
        {:type :free-food-appear :space s :amount amt})
      ;; Free food vanishing
      (for [s (cset/difference (set (keys from-food)) (set (keys to-food)))
            :let [amt (get from-food s)]]
        {:type :free-food-vanish :space s :amount amt})))))

(defn- during-transition-state
  "Compute a render-time game state: starts from `from-state`, but strips out
   elements and food that are being animated by the overlay so we don't see
   them in both the base and the overlay simultaneously.

   - :move source elements are hidden (overlay draws the ghost mover)
   - :lose elements are hidden (overlay draws the collapse morph)
   - :circulate source food is reduced by the circulated amount (the coin
     is in flight in the overlay)
   - :food-down food is reduced (the coin is shrinking away)
   - :free-food-vanish free food is removed from :food (overlay fades it)"
  [from-state transitions]
  (let [hide-elements
        (set
         (concat
          (keep (fn [{:keys [type from]}] (when (= type :move) from)) transitions)
          (keep (fn [{:keys [type space]}] (when (= type :lose) space)) transitions)))
        hide-free-food
        (set
         (keep (fn [{:keys [type space]}] (when (= type :free-food-vanish) space))
               transitions))
        base (-> from-state
                 (update :elements (fn [els] (reduce dissoc els hide-elements)))
                 (update :food (fn [f] (reduce dissoc f hide-free-food))))]
    (reduce
     (fn [st {:keys [type] :as tr}]
       (case type
         :circulate
         (let [{:keys [from amount]} tr]
           (if (get-in st [:elements from])
             (update-in st [:elements from :food]
                        #(max 0 (- (or % 0) amount)))
             st))
         :food-down
         (let [{:keys [space amount]} tr]
           (if (get-in st [:elements space])
             (update-in st [:elements space :food]
                        #(max 0 (- (or % 0) amount)))
             st))
         st))
     base
     transitions)))

(defn- ease-in-out
  "Smoothstep easing: 3t^2 - 2t^3"
  [t]
  (let [t (max 0.0 (min 1.0 t))]
    (- (* 3 t t) (* 2 t t t))))

(defn- lerp [a b t] (+ a (* (- b a) t)))

;; ── Path morphing for grow animation ────────────────────────────────────────
;; The grow animation uses a dense circular path with the same number of
;; control points as the target element's path, then spirals each point
;; outward to its final destination.

(defn- ra-seq
  "N points from `radial-axis symmetry radius phase` around a full circle."
  [symmetry radius phase]
  (mapv #(board/radial-axis symmetry radius phase %) (range symmetry)))

(defn- element-target-points
  "Return the flat vector of control/vertex points defining the given
   element type's path, centered at origin. Matches the point layout used
   by board/render-eat / render-grow / render-move."
  [type r]
  (case type
    :eat
    (let [symmetry 5
          half (/ 0.5 symmetry)
          outer-radius 1.00
          outer-arc 0.03
          inner-radius 0.5
          inner-arc 0.12
          oc-start (ra-seq symmetry (* r (+ inner-radius half)) (* board/tau -1 (- inner-arc half)))
          oc-end   (ra-seq symmetry (* r outer-radius) (* board/tau -1 outer-arc))
          outer    (ra-seq symmetry r 0)
          ic-start (ra-seq symmetry (* r outer-radius) (* board/tau outer-arc))
          ic-end   (ra-seq symmetry (* r (+ inner-radius half)) (* board/tau (- inner-arc half)))
          inner    (ra-seq symmetry (* inner-radius r) (* board/tau half))]
      (vec (interleave oc-start oc-end outer ic-start ic-end inner)))

    :grow
    (let [symmetry 4
          half (/ 0.5 symmetry)
          outer-radius 1.1
          outer-arc 0.07
          inner-radius 0.5
          inner-arc 0.23
          oc-start (ra-seq symmetry (* r (+ inner-radius 0.3)) (* board/tau -1 (- inner-arc half)))
          oc-end   (ra-seq symmetry (* r outer-radius) (* board/tau -1 outer-arc))
          outer    (ra-seq symmetry r 0)
          ic-start (ra-seq symmetry (* r outer-radius) (* board/tau outer-arc))
          ic-end   (ra-seq symmetry (* r (+ inner-radius 0.3)) (* board/tau (- inner-arc half)))
          inner    (ra-seq symmetry (* inner-radius r) (* board/tau half))]
      (vec (interleave oc-start oc-end outer ic-start ic-end inner)))

    :move
    (let [symmetry 3
          half (/ 0.5 symmetry)
          outer-radius 1.1
          outer-arc 0.07
          mid-radius 1.1
          mid-arc 0.22
          under-radius 0.75
          under-arc 0.13
          inner-radius 0.3
          inner-arc 0.3
          oc-start (ra-seq symmetry (* r (+ inner-radius 0.4)) (* board/tau -1 1.1 (- inner-arc half)))
          oc-end   (ra-seq symmetry (* r outer-radius) (* board/tau -1 outer-arc))
          outer    (ra-seq symmetry r 0)
          mc-start (ra-seq symmetry (* r outer-radius) (* board/tau outer-arc))
          mc-end   (ra-seq symmetry (* r mid-radius) (* board/tau (- mid-arc 0.05)))
          mid      (ra-seq symmetry r (* board/tau mid-arc))
          uc-start (ra-seq symmetry (* r outer-radius 0.9) (* board/tau mid-arc 1.2))
          uc-end   (ra-seq symmetry (* r 1.1 under-radius) (* board/tau (- mid-arc 0.03)))
          under    (ra-seq symmetry (* r under-radius) (* board/tau under-arc))
          ic-start (ra-seq symmetry (* r under-radius 1.2) (* board/tau (- under-arc 0.11)))
          ic-end   (ra-seq symmetry (* r (+ inner-radius 0.5)) (* board/tau (- (* 0.3 inner-arc) half)))
          inner    (ra-seq symmetry (* inner-radius r) (* board/tau (- half 0.09)))]
      (vec (interleave
            oc-start oc-end outer
            mc-start mc-end mid
            uc-start uc-end under
            ic-start ic-end inner)))

    ;; Default (unknown element type) — approximate circle with 24 points
    (ra-seq 24 (* r 0.8) 0)))

(defn- source-circle-points
  "Return n points uniformly distributed on a small circle of radius `radius`
   centered at origin, starting at angle 0 and progressing counter-clockwise."
  [n radius]
  (mapv
   (fn [i]
     (let [theta (* board/tau (/ i n))]
       [(* radius (Math/cos theta))
        (* radius (Math/sin theta))]))
   (range n)))

(defn- short-angle-delta
  "Signed angular delta from `from` to `to`, choosing the shorter of the two
   directions around the circle."
  [from to]
  (let [pi Math/PI
        d (- to from)]
    (cond
      (> d pi)     (- d board/tau)
      (< d (- pi)) (+ d board/tau)
      :else d)))

(defn- spiral-morph
  "Interpolate each source point toward its target along a spiral path.
   At progress 0 each point sits at its source; at progress 1 at its target.
   Extra rotation decays from `spiral-turns * tau` at p=0 to 0 at p=1, giving
   the spiraling-outward effect."
  [source-pts target-pts progress]
  (let [curl-turns (/ 0.2 9)
        extra (* (- 1.0 progress) board/tau curl-turns)]
    (mapv
     (fn [[sx sy] [tx ty]]
       (let [sr (Math/sqrt (+ (* sx sx) (* sy sy)))
             sth (Math/atan2 sy sx)
             tr (Math/sqrt (+ (* tx tx) (* ty ty)))
             tth (Math/atan2 ty tx)
             dth (short-angle-delta sth tth)
             r (+ sr (* (- tr sr) progress))
             th (+ sth (* dth progress) extra)]
         [(* r (Math/cos th)) (* r (Math/sin th))]))
     source-pts
     target-pts)))

(defn- curved-shape? [element-type]
  (contains? #{:eat :grow :move} element-type))

;; ── Path helpers for circulate animation ───────────────────────────────────

(defn- bfs-path
  "Shortest path from `from` to `to` through `valid-spaces` using the
   `adjacencies` map, returning a vector of spaces including endpoints,
   or nil if no path exists."
  [adjacencies valid-spaces from to]
  (let [valid (set valid-spaces)]
    (cond
      (= from to) [from]
      (not (contains? valid from)) nil
      (not (contains? valid to))   nil
      :else
      (loop [queue   #queue [[from [from]]]
             visited #{from}]
        (if (seq queue)
          (let [[space path] (peek queue)
                q            (pop queue)]
            (if (= space to)
              path
              (let [neighbors (->> (get adjacencies space [])
                                   (filter valid)
                                   (remove visited))]
                (recur (into q (map (fn [n] [n (conj path n)]) neighbors))
                       (into visited neighbors)))))
          nil)))))

(defn- chaikin-smooth
  "Run one pass of Chaikin corner-cutting on a polyline, keeping endpoints."
  [points]
  (if (< (count points) 3)
    (vec points)
    (let [first-pt (first points)
          last-pt  (last points)
          middle   (mapcat
                    (fn [[[ax ay] [bx by]]]
                      [[(+ ax (* 0.25 (- bx ax)))
                        (+ ay (* 0.25 (- by ay)))]
                       [(+ ax (* 0.75 (- bx ax)))
                        (+ ay (* 0.75 (- by ay)))]])
                    (partition 2 1 points))]
      (vec (concat [first-pt] middle [last-pt])))))

(defn- segment-length [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1) dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn- sample-along
  "Return the point at fraction `t` (0..1) of the total length of the
   polyline defined by `points`."
  [points t]
  (if (< (count points) 2)
    (first points)
    (let [segs  (mapv (fn [a b] [a b (segment-length a b)])
                      points (rest points))
          total (reduce + (map #(nth % 2) segs))
          target (* t total)]
      (loop [remaining segs
             accum 0.0]
        (if (empty? remaining)
          (last points)
          (let [[a b len] (first remaining)
                next-accum (+ accum len)]
            (if (or (>= next-accum target) (empty? (rest remaining)))
              (let [frac (if (zero? len) 0 (/ (- target accum) len))
                    [ax ay] a
                    [bx by] b]
                [(+ ax (* (- bx ax) frac))
                 (+ ay (* (- by ay) frac))])
              (recur (rest remaining) next-accum))))))))

(defn- organism-space-set
  "Spaces belonging to the same organism (same player+organism) as the
   element at `space` in `state`."
  [state space]
  (let [el (get-in state [:elements space])]
    (when el
      (into #{}
            (comp
             (filter #(and (= (:player el) (:player %))
                           (= (:organism el) (:organism %))))
             (map :space))
            (vals (:elements state))))))

(defn- render-grow-morph
  "Render a path that morphs from a dense circular food blob into the target
   element's shape. Uses two stacked paths with crossfading opacity to blend
   color from food to element."
  [board element [x y] progress]
  (let [{:keys [radius colors player-colors]} board
        food-color (-> colors first last)
        color (get player-colors (:player element))
        bright (board/brighten color 0.2)
        element-type (:type element)
        subradius (* 0.87 radius)
        target-pts (element-target-points element-type subradius)
        n (count target-pts)
        source-radius (* radius 0.22)
        source-pts (source-circle-points n source-radius)
        morphed (spiral-morph source-pts target-pts progress)
        absolute (mapv (fn [[px py]] [(+ x px) (+ y py)]) morphed)
        curve? (curved-shape? element-type)
        path-points (if curve?
                      (partition 3 absolute)
                      absolute)
        base-path (board/make-path bright path-points)
        food-path (update base-path 1 merge
                          {:fill food-color
                           :stroke "#777"
                           :stroke-width (* radius 0.04)
                           :stroke-linejoin "round"})
        element-path (update base-path 1 merge
                             {:fill bright
                              :stroke "#555"
                              :stroke-width (* radius 0.02)
                              :stroke-linejoin "round"})]
    [:g
     [:g {:opacity (- 1.0 progress)} food-path]
     [:g {:opacity progress} element-path]]))

(defn- render-transition
  "Render a single in-flight transition as an SVG group given the board,
   the raw progress (0..1), the transition map, the pre-transition state
   used as the animation baseline, and the adjacencies map (for pathing
   circulate inside the organism)."
  [board from-state adjacencies progress {:keys [type] :as t}]
  (let [{:keys [locations radius colors player-colors]} board
        food-color (-> colors first last)
        food-beam (* radius 0.3)
        food-rad  (* radius 0.2)
        eased (ease-in-out progress)]
    (case type
      :move
      (let [{:keys [from to element]} t
            [fx fy] (get locations from)
            [tx ty] (get locations to)
            x (lerp fx tx eased)
            y (lerp fy ty eased)
            color (get player-colors (:player element))]
        ^{:key [:move from to]}
        [:g
         (board/render-element color food-color [x y] radius element)])

      :lose
      (let [{:keys [space element]} t
            pos (get locations space)]
        ;; Reverse-morph: element path collapses back into a food blob by
        ;; running the grow morph with inverted progress.
        (with-meta
          (render-grow-morph board element pos (- 1.0 eased))
          {:key [:lose space]}))

      :grow
      (let [{:keys [to element]} t
            pos (get locations to)]
        (with-meta
          (render-grow-morph board element pos eased)
          {:key [:grow to]}))

      :circulate
      (let [{:keys [from to from-food-before to-food-after]} t
            ;; Find a path from source to target through the source's own
            ;; organism so the food visibly travels inside it. If no path
            ;; can be found (e.g. missing state), fall back to a straight
            ;; line.
            org-spaces (organism-space-set from-state from)
            space-path (or (bfs-path adjacencies org-spaces from to)
                           [from to])
            ;; Position of the i-th coin among n in the radial food layout,
            ;; matching board/render-food's placement. For a 1-coin layout
            ;; this is directly above the element center.
            food-slot
            (fn [[cx cy] n i]
              (if (or (nil? n) (<= n 0))
                [cx (- cy food-beam)]
                (let [[ox oy] (board/radial-axis n food-beam (* board/tau -0.25) i)]
                  [(+ cx ox) (+ cy oy)])))
            ;; Source coin departs from the slot of the "last" coin in the
            ;; pre-circulate layout (the one the user is removing).
            src-slot (food-slot (get locations from)
                                from-food-before
                                (max 0 (dec (or from-food-before 1))))
            ;; Target coin arrives at the slot of the "last" coin in the
            ;; post-circulate layout (the new one appearing).
            dst-slot (food-slot (get locations to)
                                to-food-after
                                (max 0 (dec (or to-food-after 1))))
            ;; Waypoints through each intermediate element center, using
            ;; the single-coin-above position so the food rides along the
            ;; organism silhouette.
            mid-points (mapv
                        (fn [s]
                          (let [[x y] (get locations s)]
                            [x (- y food-beam)]))
                        (butlast (rest space-path)))
            raw-points (vec (concat [src-slot] mid-points [dst-slot]))
            ;; One or two passes of Chaikin smoothing give a gentle
            ;; curved path instead of a hex-lattice zig-zag.
            smoothed (-> raw-points chaikin-smooth chaikin-smooth)
            [x y] (sample-along smoothed eased)]
        ^{:key [:circ from to]}
        [:g
         [:circle {:cx x :cy y :r food-rad
                   :fill food-color
                   :stroke "#777"
                   :stroke-width (* radius 0.03)}]])

      :food-up
      (let [{:keys [space]} t
            [x y] (get locations space)
            cy (- y (* radius 0.3))
            scale eased]
        ^{:key [:food-up space]}
        [:g
         [:circle {:cx x :cy cy :r (* food-rad scale)
                   :fill food-color
                   :stroke "#777"
                   :stroke-width (* radius 0.03)}]])

      :food-down
      (let [{:keys [space]} t
            [x y] (get locations space)
            cy (- y (* radius 0.3))
            scale (- 1.0 eased)]
        ^{:key [:food-down space]}
        [:g
         [:circle {:cx x :cy cy :r (* food-rad scale)
                   :fill food-color
                   :stroke "#777"
                   :stroke-width (* radius 0.03)}]])

      :free-food-appear
      (let [{:keys [space amount]} t
            [x y] (get locations space)]
        ^{:key [:ff-app space]}
        [:g {:opacity eased}
         (board/render-food [x y] food-beam food-rad food-color amount)])

      :free-food-vanish
      (let [{:keys [space amount]} t
            [x y] (get locations space)]
        ^{:key [:ff-van space]}
        [:g {:opacity (- 1.0 eased)}
         (board/render-food [x y] food-beam food-rad food-color amount)])

      nil)))

(defn- animation-overlay
  "Build an SVG group of all in-flight transitions."
  [board from-state adjacencies transitions progress]
  (when (seq transitions)
    (vec
     (concat
      [:g {:key "anim-overlay" :style {:pointer-events "none"}}]
      (keep (partial render-transition board from-state adjacencies progress)
            transitions)))))

(defn start-transition!
  "Begin animating from the currently displayed state to the new state."
  [new-state]
  ;; If a previous transition is still in flight, snap to its target first
  (when (and @target-state (< @transition-progress 1.0))
    (reset! displayed-state @target-state))
  (when (nil? @displayed-state)
    (reset! displayed-state new-state))
  (reset! target-state new-state)
  (reset! transition-progress 0.0)
  (let [my-token (swap! transition-token inc)
        start-time (js/Date.now)]
    (letfn [(tick []
              (when (= my-token @transition-token)
                (let [elapsed (- (js/Date.now) start-time)
                      p (min 1.0 (/ elapsed transition-duration))]
                  (reset! transition-progress p)
                  (if (< p 1.0)
                    (js/requestAnimationFrame tick)
                    (do
                      (reset! displayed-state @target-state)
                      (reset! transition-progress 1.0))))))]
      (js/requestAnimationFrame tick))))

;; When the user clicks an element at :choose-action and multiple sub-actions
;; are possible (e.g. both grow and circulate), this holds {:space :options}
;; to render the popup.
(defonce action-popup (r/atom nil))

;; Introduce-phase popup hover: which element type is being hovered. Also
;; reused by the choose-action grow popup for the hover-to-pick-type flow.
(defonce intro-hover (r/atom nil))

(defn- cancel-from-hover-clear! []
  (when @from-hover-timeout
    (js/clearTimeout @from-hover-timeout)
    (reset! from-hover-timeout nil)))

(defn- schedule-from-hover-clear! []
  (cancel-from-hover-clear!)
  (reset! from-hover-timeout
          (js/setTimeout
           (fn []
             (reset! from-hover nil)
             (reset! dest-hover nil)
             (reset! intro-hover nil)
             (reset! pointer-space nil)
             (reset! from-hover-timeout nil))
           150)))

(defonce food-source
  (r/atom {}))

(defonce player-games
  (r/atom {}))

(defonce player-preferences
  (r/atom {}))

;; Use shared atom from organism.components
(def create-game-key components/create-game-key)

(defonce observe-games
  (r/atom []))

(defonce player-stats
  (r/atom []))

(declare update-messages!)
(declare apply-invocation!)
(declare connect-create-ws!)

(def max-players 10)

(def highlight-element-stroke {:ratio 0.04 :color "#ccc"})

(defn choose-food-source!
  [space]
  (swap! food-source update space inc))

(defn introduction-complete?
  [{:keys [progress]}]
  (and
   (= 3 (count (set (keys progress))))
   (= 3 (count (set (vals progress))))))

(defn assoc-prop
  [el key value]
  (if (> 1 (count el))
    (if (map? (nth el 1))
      (assoc-in el [1 key] value)
      (vec
       (concat
        [(first el) {key value}]
        (rest el))))
    (vec
     (conj
      (conj
       (rest el)
       {key value})
      (first el)))))

(defn send-state!
  [state complete]
  (ws/send-transit-message!
   {:type "game-state"
    :game state
    :complete complete}))

(defn send-reset!
  [state]
  (ws/send-transit-message!
   {:type "history"
    :game state}))

(defn send-clear!
  []
  (ws/send-transit-message!
   {:type "clear"}))

(defn send-choice!
  [choices match complete]
  (let [choice (get-in choices [match :state])]
    (if choice
      (send-state! choice complete)
      (println "NO CHOICE MATCHING" match))))

(defn send-introduction!
  [choices {:keys [progress] :as intro}]
  ;; if (introduction-complete? intro)
  (send-choice!
   choices
   {:spaces progress
    :organism 0}
   true))

(defn send-create!
  [invocation]
  (reset! board-invocation invocation)
  (apply-invocation! invocation)
  (components/send-create! invocation))

(def send-open-game! components/send-open-game!)

(defn initialize-chat
  [chat message]
  (println "initializing chat" (:chat message))
  (:chat message))

(defn initialize-game
  [game-state {:keys [game invocation player history board witness] :as message}]
  (let [{:keys [ring-count player-count players colors mutations]} invocation
        board (board/generate-board
               colors
               players
               (take ring-count board/total-rings)
               mutations)
        [game turn choices] (choice/find-next-choices game)
        cursor (if (< witness (count history)) witness)]
    (println "initializing game" game)
    (println "initializing board" board)
    (println "turn" turn)
    (println "choices" (count choices))
    {:game game
     :invocation invocation
     :player player
     :history history
     :board board
     :turn turn
     :cursor cursor
     :choices choices}))

(defn update-chat
  [chat message]
  (conj chat message))

(defn update-game
  [game-state message]
  (let [state (:game message)
        current-game (assoc (:game game-state) :state state)
        [final-game turn choices] (choice/find-next-choices current-game)
        game-state (assoc game-state :game final-game)]
    (-> game-state
        (update :history conj (:state final-game))
        (assoc :turn turn)
        (assoc :choices choices))))

(def number->word
  {3 "THREE"
   4 "FOUR"
   5 "FIVE"
   6 "SIX"
   7 "SEVEN"
   8 "EIGHT"})

(defn round-banner
  [color round]
  [:div
   {:style
    {:color "#fff"
     :border-radius "50px"
     :cursor "pointer"
     :background color
     :letter-spacing "8px"
     :font-family font-choice
     :margin "20px 0px"
     :padding "25px 60px"}}
   [:h1
    [:a
     {:style
      {:color "#fff"}
      :href (str js/playerPath "/" js/playerKey)}
     js/playKey]]
   [:h2 "round " (inc round)]])

(defn boundary-inc
  [total n]
  (cond
    (nil? n) nil
    (= n (dec total)) nil
    :else (inc n)))

(defn boundary-dec
  [total n]
  (cond
    (nil? n) (dec total)
    (zero? n) 0
    :else (dec n)))

(defn history-beginning-control
  []
  [:polygon
   {:points "0,5 10,5 10,25 50,0 50,50 10,25 10,45 0,45"
    :style {:fill "hsl(100, 20%, 30%)"}
    :on-click
    (fn [event]
      (swap! game-state assoc :cursor 0))}])

(defn history-back-control
  [cursor total]
  [:polygon
   {:points "70,25 100,5 100,45"
    :style {:fill "hsl(100, 20%, 30%)"}
    :on-click
    (fn [event]
      (swap! game-state update :cursor (partial boundary-dec total)))}])

(def history-interval 300)

(defn clear-history-advance!
  [advance]
  (when advance
    (.clearInterval
     js/window
     advance)
    (reset! history-advance nil)))

(defn set-history-advance!
  [total cursor]
  (if-let [advance @history-advance]
    (clear-history-advance! advance))
  (reset!
   history-advance
   (.setInterval
    js/window
    (fn []
      (let [cursor (:cursor @game-state)]
        (if (>= cursor (dec total))
          (do
            (clear-history-advance! @history-advance)
            (swap! game-state assoc :cursor nil))
          (swap! game-state update :cursor (partial boundary-inc total)))))
    history-interval)))

(defn history-status-display
  [cursor total]
  [:text
   {:x (if cursor "110" "160")
    :y "35"
    :width "80"
    :font-size "1.8em"
    :style
    {:fill "#eee"}
    :on-click
    (fn [event]
      (if-let [advance @history-advance]
        (clear-history-advance! advance)
        (do
          (when (nil? cursor)
            (swap! game-state assoc :cursor 0))
          (set-history-advance! total cursor))))}
   (if cursor
     (str (inc cursor) " / " total)
     (str total))])

(defn history-forward-control
  [total]
  [:polygon
   {:points "250,5 280,25 250,45"
    :style {:fill "hsl(100, 20%, 30%)"}
    :on-click
    (fn [event]
      (swap! game-state update :cursor (partial boundary-inc total)))}])

(defn history-end-control
  [total]
  [:polygon
   {:points "350,5 340,5 340,25 300,0 300,50 340,25 340,45 350,45"
    :style {:fill "hsl(100, 20%, 30%)"}
    :on-click
    (fn [event]
      (clear-history-advance! @history-advance)
      (swap! game-state assoc :cursor nil))}])

(defn history-controls
  [history cursor]
  (let [total (count history)]
    [:div
      {:style
       {:margin "0px 0px 0px 0px"}}
     [:h3 "history"]
     [:svg
      {:width 300
       :height 50
       :style
       {:margin "10px 0px 0px 30px"}}
      [:g
       {:transform "scale(0.6)"}
       [history-beginning-control]
       [history-back-control cursor total]
       [history-status-display cursor total]
       [history-forward-control total]
       [history-end-control total]]]]))

(defn mutation-display
  [color mutation-key]
  ^{:key mutation-key}
  [:h4
   {:style
    {:color color
     :margin-left "20px"}}
   (display-mutation
    mutation-key
    (get possible-mutations mutation-key))])

(defn mutations-display
  [mutations color]
  (let [chosen (map first (filter (fn [[key choice]] choice) mutations))]
    (if-not (empty? chosen)
      [:div
       [:h3 "mutations"]
       [:div
        (map (partial mutation-display color) chosen)]])))

(defn scoreboard
  [turn-order organism-victory colors player-captures mutations state]
  (let [player (get-in state [:player-turn :player])
        player-colors (into {} (map vector turn-order colors))]
    [:div
     [:h3 "score"]
     [:ul
      (let [player-captures (if player-captures player-captures (repeat board/default-player-captures))]
        (for [[player captures color] (map vector turn-order player-captures colors)]
          ^{:key (str player color)}
          [:li
           {:style {:color color}}
           player " - "
           (count (get-in state [:captures player])) " / "
           (if (get mutations :RAIN)
             (let [rain-player (last turn-order)]
               (if (= player rain-player)
                 captures
                 [:span {:style {:font-size "1.5em"}} "∞"]))
             captures)]))]

     [:h4
      {:style
       {:font-size "1.0em"
        :margin "12px 0px 0px 0px"}}
      [:span
       {:style
        {:color (get player-colors player)}}
       (get number->word organism-victory organism-victory)]
      " organisms for victory"]
     [mutations-display mutations (get player-colors player)]]))

(def chat-window 15)

(defn chat-list
  [player-colors chat]
  [:ul
   (let [total (count chat)
         visible (drop (- total chat-window) chat)]
     (for [[i message] (map-indexed vector visible)]
       (let [player (:player message)
             color (get player-colors player "black")]
         ^{:key i}
         [:li
          {:style {:color color}}
          player ": " (:message message)])))])

(defn chat-input
  []
  (if js/playerKey
    (let [value (r/atom nil)]
      (fn []
        [:input.form-control
         {:type :text
          :placeholder "respond"
          :value @value
          :on-change #(reset! value (-> % .-target .-value))
          :on-key-down
          #(when (= (.-keyCode %) 13)
             (ws/send-transit-message!
              {:type "chat"
               :player js/playerKey
               :message @value})
             (reset! value nil))}]))
    []))

(defn description-panel
  [player-color description]
  [:div
   [:h4
    {:style
     {:color (board/brighten player-color 0.3)}}
    description]])

(defn help-panel
  [color]
  [:div
   [:h3 "help"]
   [:div
    {:style
     {:color color
      :font-size "1.2em"
      :letter-spacing "3px"
      :margin "0px 0px 0px 30px"}}
    [:a
     {:href "/img/organism-player-diagram.png"
      :target "_blank"
      :style
      {:color "hsl(250, 30%, 70%)"}}
     "player aid"]
    " | "
    [:a
     {:href "/img/organism-rulebook.pdf"
      :target "_blank"
      :style
      {:color "hsl(130, 30%, 70%)"}}
     "rules"]]])

(defn chat-panel
  [description
   turn-order
   organism-victory
   colors
   player-colors
   player-captures
   mutations
   state
   history
   cursor
   chat]
  (let [player-color (get player-colors (-> state :player-turn :player) (first colors))]
    [:div
     {:style
      {:margin "20px"}}
     [round-banner
      player-color
      (:round state)]
     [:div
      {:style
       {:margin "20px 50px"}}
      [description-panel player-color description]
      [scoreboard turn-order organism-victory colors player-captures mutations state]
      [history-controls history cursor]
      [help-panel player-color]
      [:h3 "discussion"]
      [chat-list player-colors chat]
      [:br]
      [chat-input]]]))

(defn highlight-circle
  [x y radius color on-click]
  (let [highlight-color (board/brighten color 0.3)]
    [:circle
     {:cx x :cy y
      :r (* radius 1.1)
      :stroke highlight-color
      :stroke-width (* 0.19 radius)
      :fill-opacity 0.1
      :fill "white"
      :on-click on-click}]))

(defn focus-circle
  [x y radius color on-click]
  [:circle
   {:cx x :cy y
    :r radius
    :stroke (board/brighten color 0.2)
    :stroke-width (* 0.21 radius)
    :fill (board/brighten color 0.1)
    :on-click on-click}])

(defn highlight-element
  [type food x y radius color stroke on-click]
  (let [g (board/render-element
           (board/brighten color 0.1)
           "white"
           stroke
           [x y]
           radius
           {:type type :food food})]
    (assoc-prop g :on-click on-click)))

(defn render-element
  [type x y radius color food-color on-click]
  (let [g (board/render-element
           color
           food-color
           [x y]
           radius
           {:type type :food 1})]
    (assoc-prop g :on-click on-click)))

(def highlight-factor 0.93)
(def element-highlight-factor 1.0)

(defn create-highlights
  [game board colors turn choices]
  (let [players (:players game)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        highlights
        (base/map-cat
         (fn [[[player {:keys [starting-spaces]}] color]]
           (map
            (fn [space]
              (let [[x y] (get locations space)]
                (highlight-circle
                 x y radius color
                 (fn [event]))))
            starting-spaces))
         (map vector players colors))]
    (into [] (concat [:g] highlights))))

(defn introduce-highlights
  "Click a starting space → popup with available element types appears next
   to it. Pick a type → element placed there. After two are placed, the
   third space gets the remaining type automatically. No left-panel needed."
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        food-color (-> board :colors first last)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        element-radius (* (:radius board) 1)
        popup-radius (* (:radius board) 0.45)
        starting-spaces (get-in game [:players player :starting-spaces])
        {:keys [chosen-space progress]} @introduction
        all-types [:eat :grow :move]
        used-types (set (vals progress))
        available-types (vec (remove used-types all-types))

        ;; Place element at space, auto-completing if only one type remains
        place-at!
        (fn [space type]
          (let [new-progress (assoc progress space type)
                ;; If 2 placed and 1 starting space + 1 type remain, auto-place
                remaining-spaces (remove (set (keys new-progress)) starting-spaces)
                remaining-types (remove (set (vals new-progress)) all-types)
                final-progress
                (if (and (= 1 (count remaining-spaces))
                         (= 1 (count remaining-types)))
                  (assoc new-progress (first remaining-spaces) (first remaining-types))
                  new-progress)
                new-intro (-> @introduction
                              (assoc :chosen-space nil)
                              (assoc :chosen-element nil)
                              (assoc :progress final-progress))]
            (reset! introduction new-intro)
            (when (= (count final-progress) (count starting-spaces))
              (send-introduction! choices new-intro))))

        ;; Render unchosen starting spaces as click targets that brighten
        ;; on hover so the user gets feedback before committing a choice.
        hovered-space @dest-hover
        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 hovered? (= hovered-space space)
                 stroke-c (if hovered?
                            (board/brighten color 0.6)
                            (board/brighten color 0.3))
                 stroke-w (if hovered?
                            (* 0.28 radius)
                            (* 0.19 radius))
                 fill-op (if hovered? 0.22 0.1)
                 r (* radius (if hovered? 1.15 1.1))]
             ^{:key space}
             [:circle
              {:cx x :cy y
               :r r
               :stroke stroke-c
               :stroke-width stroke-w
               :fill-opacity fill-op
               :fill "white"
               :on-mouse-enter (fn [_e] (reset! dest-hover space))
               :on-mouse-leave (fn [_e]
                                 (when (= @dest-hover space)
                                   (reset! dest-hover nil)))
               :on-click (fn [_e]
                           (reset! dest-hover nil)
                           (swap! introduction assoc :chosen-space space))}]))
         (remove
          (set (conj (keys progress) chosen-space))
          starting-spaces))

        ;; Chosen space — show its highlight + a popup with available types
        hovered-type @intro-hover
        chosen-popup
        (when chosen-space
          (let [[x y] (get locations chosen-space)
                n (count available-types)
                ;; Layout the popup in a horizontal row above the space
                spread (* popup-radius 2.4)
                start-x (- x (* spread (/ (dec n) 2.0)))
                offset-y (- y (* (:radius board) 1.6))]
            ^{:key (str "popup-" chosen-space)}
            [:g {:key (str "popup-" chosen-space)}
             ;; Click-elsewhere catcher: highlight on the chosen space cancels
             (focus-circle
              x y radius color
              (fn [_e]
                (reset! intro-hover nil)
                (swap! introduction dissoc :chosen-space)))
             ;; Preview of the hovered type placed at the chosen space
             (when hovered-type
               ^{:key (str "preview-" chosen-space "-" hovered-type)}
               [:g {:style {:pointer-events "none"}
                    :opacity 0.85}
                (board/render-element
                 (board/brighten color 0.2)
                 food-color
                 [x y]
                 element-radius
                 {:type hovered-type :food 0})])
             ;; The popup options
             (for [[i type] (map-indexed vector available-types)
                   :let [px (+ start-x (* i spread))
                         hovered? (= hovered-type type)
                         bg-fill (if hovered?
                                   (board/brighten color 0.25)
                                   (board/brighten color 0.05))
                         bg-stroke (if hovered?
                                     (board/brighten color 0.6)
                                     (board/brighten color 0.4))
                         bg-stroke-w (if hovered?
                                       (* 0.22 popup-radius)
                                       (* 0.15 popup-radius))
                         opt-radius (if hovered?
                                      (* popup-radius 1.08)
                                      popup-radius)]]
               ^{:key (str "popup-" chosen-space "-" type)}
               [:g {:on-click (fn [e]
                                (.stopPropagation e)
                                (reset! intro-hover nil)
                                (place-at! chosen-space type))
                    :on-mouse-enter (fn [_e] (reset! intro-hover type))
                    :on-mouse-leave (fn [_e]
                                      (when (= @intro-hover type)
                                        (reset! intro-hover nil)))}
                ;; Background circle behind the icon
                [:circle {:cx px :cy offset-y :r opt-radius
                          :fill bg-fill
                          :stroke bg-stroke
                          :stroke-width bg-stroke-w
                          :style {:cursor "pointer"}}]
                (render-element
                 type px offset-y (* opt-radius 0.8) color food-color
                 (fn [e]
                   (.stopPropagation e)
                   (reset! intro-hover nil)
                   (place-at! chosen-space type)))])]))

        ;; Render elements placed so far — clickable to remove (revert)
        elements
        (map
         (fn [[space type]]
           (let [[x y] (get locations space)]
             ^{:key space}
             (render-element
              type x y element-radius color food-color
              (fn [_event]
                (swap! introduction
                       (fn [intro]
                         (-> intro
                             (assoc :chosen-space nil)
                             (update :progress dissoc space))))))))
         progress)]
    ^{:key "highlights"}
    [:g (cond-> (concat highlights elements)
          chosen-popup (concat [chosen-popup]))]))

(defn chosen-organism-highlights
  [game board on-click turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) element-highlight-factor)
        elements (game/current-organism-elements game)
        element-stroke highlight-element-stroke
        source @food-source

        highlights
        (mapv
         (fn [{:keys [space organism type food] :as element}]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-element
              type (- food (get source space 0))
              x y radius
              color element-stroke
              (partial on-click element))))
         elements)]
    highlights))

(defn space-highlights
  [game board turn choices spaces on-click]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)

        space-highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 next-state (get-in choices [space :state])]
             ^{:key space}
             (highlight-circle
              x y radius color
              (partial on-click space))))
         spaces)]
    space-highlights))

(defn choose-organism-highlights
  [game board turn choices]
  (let [game (game/find-organisms game) ;; find organisms here to avoid finding for each introduction
        player (game/current-player game)
        organisms (game/player-organisms game player)
        available (keys choices)
        elements (base/map-cat organisms available)
        spaces (map :space elements)
        space-organisms
        (into
         {}
         (map
          (juxt :space :organism)
          elements))

        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (let [organism (get space-organisms space)]
             (send-choice! choices organism true))))]
    
    highlights))

(defn choose-action-type-highlights
  "During :choose-action-type, render a highlight circle around each
   current-organism element. Hovering brightens all elements of that type
   and shows a popup with the type name and number of actions you'll get."
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        ;; The game at :choose-action-type doesn't yet have the current
        ;; organism set in organism-turns. Find the player's first organism
        ;; (find-state already chose it; here we just look it up).
        game-with-orgs (game/find-organisms game)
        organisms (game/player-organisms game-with-orgs player)
        first-org-id (first (keys organisms))
        elements (or (not-empty (game/current-organism-elements game))
                     (get organisms first-org-id)
                     [])
        by-type (group-by :type elements)
        hover (or @action-hover
                  ;; Fallback: right after committing the previous action the
                  ;; cursor hasn't moved but action-hover was cleared — re-derive
                  ;; from pointer-space so the element under the pointer stays
                  ;; immediately actionable.
                  (when-let [hs @pointer-space]
                    (when-let [el (first (filter #(= (:space hs) (:space %)) elements))]
                      (let [[x y] (get locations (:space el))]
                        {:type (:type el) :x x :y y}))))
        hover-type (:type hover)

        ;; A click-target halo per element. Hovered type → brighter halo.
        element-halos
        (mapv
         (fn [{:keys [space type] :as _element}]
           (let [[x y] (get locations space)
                 hovered? (= hover-type type)
                 stroke-c (if hovered?
                            (board/brighten color 0.6)
                            (board/brighten color 0.3))
                 fill-op (if hovered? 0.25 0.10)]
             ^{:key (str "halo-" space)}
             [:circle
              {:cx x :cy y
               :r (* radius 1.15)
               :stroke stroke-c
               :stroke-width (if hovered? (* 0.28 radius) (* 0.19 radius))
               :fill "white"
               :fill-opacity fill-op
               :style {:cursor "pointer"}
               :on-mouse-enter (fn [_e]
                                 (cancel-from-hover-clear!)
                                 (reset! action-hover {:type type :x x :y y})
                                 (reset! pointer-space {:space space :kind :element}))
               :on-mouse-leave (fn [_e]
                                 (reset! action-hover nil)
                                 (schedule-from-hover-clear!))
               :on-click (fn [_e]
                           (reset! action-hover nil)
                           (send-choice! choices type true))}]))
         elements)

        ;; Popup showing "EAT: 3 actions" above the hovered element
        popup
        (when hover
          (let [{:keys [type x y]} hover
                n (count (get by-type type []))
                label (str (clojure.string/upper-case (name type))
                           ": " n " action" (when (not= n 1) "s"))
                box-w 180
                box-h 38
                px (- x (/ box-w 2))
                py (- y (* (:radius board) 2.4))]
            ^{:key "action-hover-popup"}
            [:g {:pointer-events "none"}
             [:rect {:x px :y py :width box-w :height box-h :rx 6
                     :fill "#0A0E1C"
                     :stroke (board/brighten color 0.4)
                     :stroke-width 2
                     :opacity 0.95}]
             [:text {:x (+ px (/ box-w 2)) :y (+ py 25)
                     :text-anchor "middle"
                     :fill "#fff"
                     :font-family "monospace"
                     :font-size 16
                     :letter-spacing "1px"}
              label]]))]
    (cond-> element-halos
      popup (conj popup))))

(defn- compute-from-spaces-and-options
  "Given the post-:choose-action game wrap, return a map
   {space → [{:label ... :destinations [...] :next-state ...
              :sub-options [...]} ...]}
   For grow, the top-level option for each grower has :sub-options listing
   the available element-types as a nested popup."
  [post-action-game-wrap label-prefix]
  (try
    (let [[phase from-choices] (choice/find-state post-action-game-wrap)]
      (cond
        ;; Move/eat/circulate: from-choices is keyed by space directly
        (#{:move-from :eat-to :circulate-from} phase)
        (into {}
              (map
               (fn [space]
                 (let [from-state (get-in from-choices [space :state])
                       from-wrap (assoc post-action-game-wrap :state from-state)
                       dests (try
                               (let [[_ to-choices] (choice/find-state from-wrap)]
                                 (filter vector? (keys to-choices)))
                               (catch :default _ nil))
                       ;; For :eat, suppress the preview entirely when no
                       ;; adjacent space has food — the server auto-advances
                       ;; past :eat-from in that case, so we shouldn't
                       ;; highlight an arbitrary empty source either.
                       dests (if (and (= phase :eat-to) (seq dests))
                               (let [food-map (get-in from-wrap [:state :food] {})
                                     any-food? (some #(pos? (get food-map % 0)) dests)]
                                 (if any-food? dests []))
                               dests)]
                   [space [{:label label-prefix
                            :destinations (or dests [])
                            :next-state from-state}]]))
               (filter vector? (keys from-choices))))

        ;; Grow: top-level option is "GROW", nested sub-options are element types
        (= phase :grow-element)
        (let [type-keys (keys from-choices)
              ;; For each grower space, collect its sub-options (one per type)
              ;; sub-options-by-space: {grower-space [{:label :destinations :next-state} ...]}
              sub-by-space
              (reduce
               (fn [acc type-key]
                 (try
                   (let [type-state (get-in from-choices [type-key :state])
                         type-wrap (assoc post-action-game-wrap :state type-state)
                         [grow-from-phase grow-from-choices] (choice/find-state type-wrap)
                         sub-label (clojure.string/upper-case (name type-key))]
                     (if (= grow-from-phase :grow-from)
                       (reduce
                        (fn [acc contribution]
                          (let [contrib-state (get-in grow-from-choices [contribution :state])
                                contrib-wrap (assoc type-wrap :state contrib-state)
                                [_ to-choices] (choice/find-state contrib-wrap)
                                dests (filter vector? (keys to-choices))
                                sub-opt {:label sub-label
                                         :type type-key
                                         :destinations (or dests [])
                                         :next-state contrib-state}]
                            (reduce
                             (fn [acc space]
                               (update acc space (fnil conj []) sub-opt))
                             acc
                             (keys contribution))))
                        acc
                        (keys grow-from-choices))
                       acc))
                   (catch :default _ acc)))
               {} type-keys)]
          ;; Wrap each grower's sub-options in a single top-level GROW option
          (into {}
                (map
                 (fn [[space subs]]
                   [space [{:label label-prefix
                            :destinations (->> subs
                                                (mapcat :destinations)
                                                distinct
                                                vec)
                            :sub-options subs}]])
                 sub-by-space)))

        :else {}))
    (catch :default _ {})))

(defn- compute-move-options
  "Walk the choice tree from the post-:choose-action game wrap (phase
   :move-from) through :move-from → :move-to to build
     {mover-space {dest-space <committed-state>}}
   so clicking a destination commits the full move in one step."
  [post-action-game-wrap]
  (try
    (let [[phase from-choices] (choice/find-state post-action-game-wrap)]
      (if (not= phase :move-from)
        {}
        (reduce
         (fn [acc mover-space]
           (try
             (let [from-state (get-in from-choices [mover-space :state])
                   from-wrap  (assoc post-action-game-wrap :state from-state)
                   [_ to-choices] (choice/find-state from-wrap)]
               (reduce
                (fn [acc dest-space]
                  (let [committed (get-in to-choices [dest-space :state])]
                    (update acc mover-space (fnil assoc {}) dest-space committed)))
                acc
                (filter vector? (keys to-choices))))
             (catch :default _ acc)))
         {}
         (filter vector? (keys from-choices)))))
    (catch :default _ {})))

(defn- compute-grow-options
  "Walk the game's choice tree from the post-:choose-action game wrap
   (phase :grow-element) through :grow-element → :grow-from → :grow-to to
   build a nested map
     {grower-space {dest-space [{:type <el-type> :next-state <committed>} ...]}}
   where each committed state has :element, :from, and :to already chosen
   so sending it commits the full grow action in one step."
  [post-action-game-wrap]
  (try
    (let [[phase type-choices] (choice/find-state post-action-game-wrap)]
      (if (not= phase :grow-element)
        {}
        (reduce
         (fn [acc type-key]
           (try
             (let [type-state (get-in type-choices [type-key :state])
                   type-wrap  (assoc post-action-game-wrap :state type-state)
                   [_ contrib-choices] (choice/find-state type-wrap)]
               (reduce
                (fn [acc contribution]
                  (try
                    (let [contrib-state (get-in contrib-choices [contribution :state])
                          contrib-wrap  (assoc post-action-game-wrap :state contrib-state)
                          [_ dest-choices] (choice/find-state contrib-wrap)]
                      (reduce
                       (fn [acc dest-space]
                         (let [committed (get-in dest-choices [dest-space :state])]
                           (reduce
                            (fn [acc grower-space]
                              (update-in acc [grower-space dest-space]
                                         (fnil conj [])
                                         {:type type-key
                                          :next-state committed}))
                            acc
                            (keys contribution))))
                       acc
                       (filter vector? (keys dest-choices))))
                    (catch :default _ acc)))
                acc
                (filter map? (keys contrib-choices))))
             (catch :default _ acc)))
         {} (keys type-choices))))
    (catch :default _ {})))

(defn- grow-spent-food
  "Map of {grower-space amount} for elements whose food decreases from
   current-state to next-state — i.e. how much food each space spends for a
   given grow option."
  [current-state next-state]
  (let [nxt (:elements next-state)]
    (into {}
     (keep
      (fn [[space el]]
        (let [spent (- (or (:food el) 0) (or (:food (get nxt space)) 0))]
          (when (pos? spent) [space spent])))
      (:elements current-state)))))

(defn- grow-dest-options
  "All grow variants for `dest-space`, pooled across every grower and grouped by
   element type: {type [{:spent {grower-space amount} :state next-state} ...]}.
   Food is a shared pool, so variants that resolve to the same committed state
   (listed under different growers) are deduped."
  [grow-options current-state dest-space]
  (let [variants (distinct
                  (mapcat #(get-in grow-options [% dest-space])
                          (keys grow-options)))]
    (reduce
     (fn [acc {:keys [type next-state]}]
       (update acc type (fnil conj [])
               {:spent (grow-spent-food current-state next-state)
                :state next-state}))
     {}
     variants)))

(defn- grow-pay-click!
  "Spend one food coin from grower `space` toward the in-progress grow payment.
   Commits the variant matching the chosen distribution once the cost is paid."
  [game space]
  (when-let [{:keys [cost spent variants]} @grow-pay]
    (let [cur (or (:food (get-in game [:state :elements space])) 0)]
      (when (< (get spent space 0) cur)
        (let [spent' (update spent space (fnil inc 0))]
          (if (>= (reduce + 0 (vals spent')) cost)
            (let [variant (first (filter #(= (:spent %) spent') variants))]
              (reset! grow-pay nil)
              (cancel-from-hover-clear!)
              (reset! from-hover nil)
              (reset! dest-hover nil)
              (when variant (send-state! (:state variant) true)))
            (swap! grow-pay assoc :spent spent')))))))

(defn choose-action-highlights
  "During :choose-action, the chosen action type drives one set of clickable
   ELEMENT halos (move/eat/grow targets), and a separate set of FOOD halos
   (small markers on every element with food → click to circulate).

   - Click element halo → execute the action (popup only for grow's
     element-type sub-choice)
   - Click food halo → execute circulate from that element"
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        food-color (-> board :colors first last)
        element-radius (* (:radius board) 1)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        organism-turns (get-in game [:state :player-turn :organism-turns])
        action-type (get-in organism-turns [(dec (count organism-turns)) :choice])
        ;; Action type's flow (move-from / eat-to / grow-element)
        action-game-state (get-in choices [action-type :state])
        action-game (when action-game-state (assoc game :state action-game-state))
        action-from-map (when action-game
                          (compute-from-spaces-and-options
                           action-game (clojure.string/upper-case (name action-type))))
        ;; Pre-computed grow options: {grower {dest [{:type :next-state}]}}
        ;; Only populated when the active action is grow.
        grow-options (when (and (= action-type :grow) action-game)
                       (compute-grow-options action-game))
        ;; Pre-computed move options: {mover {dest <committed-state>}}
        ;; Only populated when the active action is move.
        move-options (when (and (= action-type :move) action-game)
                       (compute-move-options action-game))
        ;; Circulate flow (any element with food)
        circ-game-state (get-in choices [:circulate :state])
        circ-game (when circ-game-state (assoc game :state circ-game-state))
        circ-from-map (when circ-game
                        (compute-from-spaces-and-options circ-game "CIRCULATE"))

        hover (or @from-hover @pointer-space)  ;; {:space [...] :kind :element|:food}
        popup @action-popup
        d-hover @dest-hover
        hovered-grow-type @intro-hover
        paying @grow-pay

        ;; ── Element halo click ──────────────────────────────────────────
        opt->button
        (fn opt->button [space opt]
          {:label (:label opt)
           :type  (:type opt)
           :on-click
           (fn []
             (cond
               (and (seq (:sub-options opt))
                    (= 1 (count (:sub-options opt))))
               (do (reset! action-popup nil)
                   (send-state! (:next-state (first (:sub-options opt))) true))
               (seq (:sub-options opt))
               (reset! action-popup
                       {:space space
                        :options (mapv (partial opt->button space) (:sub-options opt))})
               :else
               (do (reset! action-popup nil)
                   (send-state! (:next-state opt) true))))})

        click-element
        (fn [space]
          (cond
            ;; For grow/move the click on the source element is a no-op:
            ;; the user drives the choice by hovering destinations. Move
            ;; commits on destination click; grow commits on popup click.
            (contains? #{:grow :move} action-type) nil
            :else
            (do
              (reset! from-hover nil)
              (let [opts (get action-from-map space)]
                (cond
                  (empty? opts) nil
                  ;; Single top-level option with no sub-options → execute directly
                  (and (= 1 (count opts))
                       (empty? (:sub-options (first opts))))
                  (send-state! (:next-state (first opts)) true)
                  ;; Single top-level option with exactly one sub-option → execute it
                  (and (= 1 (count opts))
                       (= 1 (count (:sub-options (first opts)))))
                  (send-state! (:next-state (first (:sub-options (first opts)))) true)
                  ;; Single top-level option with multiple sub-options
                  ;; (e.g. GROW with element-type choices) → skip top level and show sub-options
                  (= 1 (count opts))
                  (reset! action-popup
                          {:space space
                           :options (mapv (partial opt->button space)
                                          (:sub-options (first opts)))})
                  ;; Multiple top-level options → top-level popup
                  :else
                  (reset! action-popup
                          {:space space
                           :options (mapv (partial opt->button space) opts)}))))))

        click-food
        (fn [space]
          (reset! from-hover nil)
          (when-let [opt (first (get circ-from-map space))]
            (send-state! (:next-state opt) true)))

        ;; ── Element halos (the action's clickable elements) ────────────
        element-halos
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 hovered? (and (= :element (:kind hover))
                               (= space (:space hover)))
                 stroke-c (if hovered?
                            (board/brighten color 0.6)
                            (board/brighten color 0.3))
                 fill-op (if hovered? 0.25 0.10)]
             ^{:key (str "el-" space)}
             [:circle
              {:cx x :cy y
               :r (* radius 1.15)
               :stroke stroke-c
               :stroke-width (if hovered? (* 0.28 radius) (* 0.19 radius))
               :fill "white"
               :fill-opacity fill-op
               :style {:cursor "pointer"}
               :on-mouse-enter (fn [_e]
                                 (cancel-from-hover-clear!)
                                 (reset! dest-hover nil)
                                 (reset! from-hover {:space space :kind :element})
                                 (reset! pointer-space {:space space :kind :element}))
               :on-mouse-leave (fn [_e] (schedule-from-hover-clear!))
               :on-click (fn [_e] (click-element space))}]))
         (if paying [] (keys action-from-map)))

        ;; ── Food halos (every element with food → circulate sources) ───
        ;; One marker per food coin, at the same radial layout board/render-food
        ;; uses, so every coin on an element is highlighted identically — no coin
        ;; is singled out. Hovering any coin lights the whole element's food.
        food-halos
        (let [food-beam (* element-radius 0.3)]
          (vec
           (mapcat
            (fn [space]
              (let [[x y] (get locations space)
                    food (or (:food (get-in game [:state :elements space])) 0)
                    hovered? (and (= :food (:kind hover))
                                  (= space (:space hover)))
                    enter (fn [_e]
                            (cancel-from-hover-clear!)
                            (reset! dest-hover nil)
                            (reset! from-hover {:space space :kind :food})
                            (reset! pointer-space {:space space :kind :food}))
                    leave (fn [_e] (schedule-from-hover-clear!))
                    click (fn [_e] (click-food space))]
                (for [i (range food)
                      :let [[ox oy] (board/radial-axis food food-beam (* board/tau -0.25) i)]]
                  ^{:key (str "food-" space "-" i)}
                  [:circle
                   {:cx (+ x ox) :cy (+ y oy)
                    :r (* element-radius (if hovered? 0.26 0.22))
                    :fill "#FFD030"
                    :stroke (board/brighten color 0.4)
                    :stroke-width 2.5
                    :fill-opacity (if hovered? 0.95 0.85)
                    :style {:cursor "pointer"}
                    :on-mouse-enter enter
                    :on-mouse-leave leave
                    :on-click click}])))
            (if paying [] (keys circ-from-map)))))

        ;; ── Destination halos when hovering ────────────────────────────
        ;; Element hover → action's destinations
        ;; Food hover    → circulate's destinations
        ;; For grow we use the pre-computed grow-options tree so each dest
        ;; can carry its own set of element-type choices.
        hover-dests
        (when hover
          (let [{:keys [space kind]} hover]
            (cond
              (= :food kind)
              (when circ-from-map
                (->> (get circ-from-map space)
                     (mapcat :destinations)
                     distinct
                     seq))
              (and (= :element kind) (= action-type :grow))
              (when grow-options
                ;; Food is pooled, so every growable destination is reachable no
                ;; matter which grower is hovered — show them all.
                (seq (distinct (mapcat keys (vals grow-options)))))
              (and (= :element kind) (= action-type :move))
              (when move-options
                (seq (keys (get move-options space))))
              (= :element kind)
              (when action-from-map
                (->> (get action-from-map space)
                     (mapcat :destinations)
                     distinct
                     seq)))))
        dest-halos
        (when (and (not paying) (seq hover-dests))
          (let [hovering-mover? (and (= action-type :move)
                                     (= :element (:kind hover)))
                mover-space (when hovering-mover? (:space hover))]
            (mapv
             (fn [space]
               (let [[x y] (get locations space)
                     d-hovered? (= space d-hover)
                     move-commit! (when hovering-mover?
                                    (fn [_e]
                                      (let [committed (get-in move-options [mover-space space])]
                                        (when committed
                                          (cancel-from-hover-clear!)
                                          (reset! intro-hover nil)
                                          (reset! from-hover nil)
                                          (reset! dest-hover nil)
                                          (send-state! committed true)))))]
                 ^{:key (str "dest-" space)}
                 [:circle
                  (cond-> {:cx x :cy y
                           :r (* radius (if d-hovered? 1.15 1.0))
                           :stroke (if d-hovered?
                                     (board/brighten color 0.9)
                                     (board/brighten color 0.7))
                           :stroke-width (if d-hovered? (* 0.28 radius) (* 0.18 radius))
                           :stroke-dasharray (when-not d-hovered? "4,3")
                           :fill (if d-hovered?
                                   (board/brighten color 0.6)
                                   (board/brighten color 0.3))
                           :fill-opacity (if d-hovered? 0.40 0.18)
                           :style {:cursor "pointer"}
                           :on-mouse-enter (fn [_e]
                                             (cancel-from-hover-clear!)
                                             (reset! dest-hover space))
                           :on-mouse-leave (fn [_e]
                                             (schedule-from-hover-clear!))}
                    move-commit! (assoc :on-click move-commit!))]))
             hover-dests)))

        ;; ── Move preview at hovered destination ────────────────────────
        move-preview
        (when (and (= action-type :move)
                   (= :element (:kind hover))
                   d-hover
                   move-options)
          (let [mover-space (:space hover)
                mover-el (get-in game [:state :elements mover-space])]
            (when (and mover-el (get-in move-options [mover-space d-hover]))
              (let [[dx dy] (get locations d-hover)]
                ^{:key (str "move-prev-" d-hover)}
                [:g {:style {:pointer-events "none"} :opacity 0.85}
                 (board/render-element
                  (board/brighten color 0.2)
                  food-color
                  [dx dy]
                  element-radius
                  mover-el)]))))

        ;; ── Inline grow popup at the hovered destination ───────────────
        ;; When hovering a grower and a destination, show element-type
        ;; options above that dest (introduce-style). Hover a type to see
        ;; a preview rendered inside the dest; click a type to commit.
        grow-inline-popup
        (when (and (= action-type :grow)
                   (not paying)
                   (= :element (:kind hover))
                   d-hover
                   grow-options)
          (let [dest-space d-hover
                by-type    (grow-dest-options grow-options (:state game) dest-space)
                types      (vec (sort-by name (keys by-type)))]
            (when (seq types)
              (let [[dx dy] (get locations dest-space)
                    popup-radius (* (:radius board) 0.45)
                    n (count types)
                    spread (* popup-radius 2.4)
                    start-x (- dx (* spread (/ (dec n) 2.0)))
                    offset-y (- dy (* (:radius board) 1.6))]
                ^{:key (str "grow-popup-" dest-space)}
                [:g
                 ;; Invisible "bridge" spanning the destination up through the
                 ;; whole option row, so the cursor never crosses a dead gap on
                 ;; its way to a corner option — otherwise the hover-clear timer
                 ;; fires mid-reach and the popup blinks away before you can pick.
                 [:rect {:x (- start-x popup-radius)
                         :y (- offset-y popup-radius)
                         :width (+ (* spread (dec n)) (* 2 popup-radius))
                         :height (+ (* (:radius board) 1.6) radius popup-radius)
                         :fill "transparent"
                         :on-mouse-enter (fn [_e] (cancel-from-hover-clear!))
                         :on-mouse-leave (fn [_e] (schedule-from-hover-clear!))}]
                 ;; Preview the hovered type rendered at the destination
                 (when hovered-grow-type
                   ^{:key (str "grow-prev-" dest-space "-" hovered-grow-type)}
                   [:g {:style {:pointer-events "none"} :opacity 0.85}
                    (board/render-element
                     (board/brighten color 0.2)
                     food-color
                     [dx dy]
                     element-radius
                     {:type hovered-grow-type :food 0})])
                 (for [[i type] (map-indexed vector types)
                       :let [px (+ start-x (* i spread))
                             variants (get by-type type)
                             cost (reduce + 0 (vals (:spent (first variants))))
                             hovered? (= hovered-grow-type type)
                             bg-fill (if hovered?
                                       (board/brighten color 0.25)
                                       (board/brighten color 0.05))
                             bg-stroke (if hovered?
                                         (board/brighten color 0.6)
                                         (board/brighten color 0.4))
                             bg-stroke-w (if hovered?
                                           (* 0.22 popup-radius)
                                           (* 0.15 popup-radius))
                             opt-radius (if hovered?
                                          (* popup-radius 1.08)
                                          popup-radius)
                             ;; One way to pay → commit immediately. Several ways
                             ;; (food is pooled) → enter coin-by-coin pay mode.
                             choose! (fn [e]
                                       (.stopPropagation e)
                                       (cancel-from-hover-clear!)
                                       (reset! intro-hover nil)
                                       (if (or (zero? cost) (= 1 (count variants)))
                                         (do (reset! from-hover nil)
                                             (reset! dest-hover nil)
                                             (send-state! (:state (first variants)) true))
                                         (reset! grow-pay {:dest dest-space
                                                           :cost cost
                                                           :spent {}
                                                           :variants variants})))]]
                   ^{:key (str "grow-popup-opt-" i "-" type)}
                   [:g {:on-click choose!
                        :on-mouse-enter (fn [_e]
                                          (cancel-from-hover-clear!)
                                          (reset! intro-hover type))
                        :on-mouse-leave (fn [_e]
                                          (schedule-from-hover-clear!))
                        :style {:cursor "pointer"}}
                    [:circle {:cx px :cy offset-y :r opt-radius
                              :fill bg-fill
                              :stroke bg-stroke
                              :stroke-width bg-stroke-w}]
                    (render-element
                     type px offset-y (* opt-radius 0.8) color food-color
                     choose!)
                    ;; cost shown above the icon as that many food tokens
                    [:g {:style {:pointer-events "none"}}
                     (board/render-food
                      [px (- offset-y (* popup-radius 1.9))]
                      (* popup-radius 0.45) (* popup-radius 0.3)
                      food-color cost)]])]))))

        ;; ── Grow payment overlay (coin-by-coin from the shared grower pool) ─
        ;; Click growers to spend their food one coin at a time; once the cost is
        ;; covered we commit the matching variant. Click empty space to cancel.
        pay-overlay
        (when paying
          (let [{:keys [spent variants]} paying
                growers (distinct (mapcat (comp keys :spent) variants))
                food-beam (* element-radius 0.3)
                food-rad  (* element-radius 0.2)]
            ^{:key "grow-pay"}
            [:g
             ;; click empty space to cancel the grow
             [:rect {:x -10000 :y -10000 :width 20000 :height 20000
                     :fill "transparent"
                     :on-click (fn [_e] (reset! grow-pay nil))}]
             (into
              [:g]
              (for [g growers
                    :let [[ex ey] (get locations g)
                          cur (or (:food (get-in game [:state :elements g])) 0)
                          sp  (get spent g 0)
                          payable? (< sp cur)]]
                ^{:key (str "pay-" g)}
                [:g
                 ;; obvious highlight on growers that still have a coin to spend;
                 ;; maxed growers get an invisible click-catcher (so clicking a
                 ;; fully-spent grower doesn't fall through and cancel).
                 [:circle (cond-> {:cx ex :cy ey :r (* element-radius 1.15)
                                   :fill "#FFD030"
                                   :fill-opacity (if payable? 0.14 0.0)
                                   :stroke (if payable? "#FFD030" "none")
                                   :stroke-width (* 0.18 element-radius)}
                            payable? (assoc :style {:cursor "pointer"}
                                            :on-click (fn [e]
                                                        (.stopPropagation e)
                                                        (grow-pay-click! game g))))]
                 (into
                  [:g {:style {:pointer-events "none"}}]
                  (for [i (range sp)
                        :let [[ox oy] (board/radial-axis cur food-beam (* board/tau -0.25) i)]]
                    ^{:key (str "payspent-" g "-" i)}
                    [:circle {:cx (+ ex ox) :cy (+ ey oy) :r (* food-rad 1.7)
                              :fill "none" :stroke "#FFD030"
                              :stroke-width (* food-rad 0.5)}]))]))]))

        ;; ── Popup (grow element-type selection) ─────────────────────────
        ;; If every option carries an :type (element-type keyword), render
        ;; the popup introduce-style: circular background behind the
        ;; element's icon, hover brightens, and a preview element is
        ;; rendered at the chosen grower's space.
        popup-render
        (when popup
          (let [{:keys [space options]} popup
                [x y] (get locations space)
                all-typed? (and (seq options) (every? :type options))
                hovered-type @intro-hover]
            (if all-typed?
              ;; Introduce-style element-icon popup
              (let [popup-radius (* (:radius board) 0.45)
                    n (count options)
                    spread (* popup-radius 2.4)
                    start-x (- x (* spread (/ (dec n) 2.0)))
                    offset-y (- y (* (:radius board) 1.6))]
                ^{:key "action-choice-popup"}
                [:g
                 [:rect {:x -10000 :y -10000 :width 20000 :height 20000
                         :fill "transparent"
                         :on-click (fn [_e]
                                     (reset! intro-hover nil)
                                     (reset! action-popup nil))}]
                 ;; Preview of the hovered type placed at the grower's space
                 (when hovered-type
                   ^{:key (str "preview-" space "-" hovered-type)}
                   [:g {:style {:pointer-events "none"} :opacity 0.85}
                    (board/render-element
                     (board/brighten color 0.2)
                     food-color
                     [x y]
                     element-radius
                     {:type hovered-type :food 0})])
                 (for [[i opt] (map-indexed vector options)
                       :let [px (+ start-x (* i spread))
                             type (:type opt)
                             hovered? (= hovered-type type)
                             bg-fill (if hovered?
                                       (board/brighten color 0.25)
                                       (board/brighten color 0.05))
                             bg-stroke (if hovered?
                                         (board/brighten color 0.6)
                                         (board/brighten color 0.4))
                             bg-stroke-w (if hovered?
                                           (* 0.22 popup-radius)
                                           (* 0.15 popup-radius))
                             opt-radius (if hovered?
                                          (* popup-radius 1.08)
                                          popup-radius)]]
                   ^{:key (str "popup-opt-" i)}
                   [:g {:on-click (fn [e]
                                    (.stopPropagation e)
                                    (reset! intro-hover nil)
                                    ((:on-click opt)))
                        :on-mouse-enter (fn [_e] (reset! intro-hover type))
                        :on-mouse-leave (fn [_e]
                                          (when (= @intro-hover type)
                                            (reset! intro-hover nil)))
                        :style {:cursor "pointer"}}
                    [:circle {:cx px :cy offset-y :r opt-radius
                              :fill bg-fill
                              :stroke bg-stroke
                              :stroke-width bg-stroke-w}]
                    (render-element
                     type px offset-y (* opt-radius 0.8) color food-color
                     (fn [e]
                       (.stopPropagation e)
                       (reset! intro-hover nil)
                       ((:on-click opt))))])])
              ;; Fallback: text-button popup (non-type options)
              (let [n (count options)
                    btn-w 130
                    btn-h 36
                    spread (+ btn-w 10)
                    start-x (- x (* spread (/ (dec n) 2.0)))
                    py (- y (* (:radius board) 2.4))]
                ^{:key "action-choice-popup"}
                [:g
                 [:rect {:x -10000 :y -10000 :width 20000 :height 20000
                         :fill "transparent"
                         :on-click (fn [_e] (reset! action-popup nil))}]
                 (for [[i opt] (map-indexed vector options)
                       :let [bx (- (+ start-x (* i spread)) (/ btn-w 2))]]
                   ^{:key (str "popup-btn-" i)}
                   [:g {:on-click (fn [e]
                                    (.stopPropagation e)
                                    ((:on-click opt)))
                        :style {:cursor "pointer"}}
                    [:rect {:x bx :y py :width btn-w :height btn-h :rx 6
                            :fill "#0A0E1C"
                            :stroke (board/brighten color 0.5)
                            :stroke-width 2}]
                    [:text {:x (+ bx (/ btn-w 2)) :y (+ py 24)
                            :text-anchor "middle"
                            :fill "#fff"
                            :font-family "monospace"
                            :font-size 14
                            :letter-spacing "1px"}
                     (:label opt)]])]))))]
    (vec (concat element-halos
                 food-halos
                 (or dest-halos [])
                 (when move-preview [move-preview])
                 (when grow-inline-popup [grow-inline-popup])
                 (when pay-overlay [pay-overlay])
                 (when popup-render [popup-render])))))

(defn choose-space-highlights
  [game board turn choices]
  (let [spaces (keys choices)
        elements (game/current-organism-elements game)

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [element event]
           (if (get choices (:space element))
             (send-choice! choices (:space element) true)))
         turn choices)
        
        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (send-choice! choices space true)))]
    (concat highlights element-highlights)))

(defn choose-target-highlights
  "Render the destination spaces from `choices` as halos with hover-brightening.
   Used for :move-to, :grow-to, :eat-from, :circulate-to, etc.

   Also renders a lit-up source indicator at the `:from` space of the current
   action so the player keeps visual context of what they're about to direct:
   a glowing food coin for circulate, a glowing element halo for move."
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        spaces (filter vector? (keys choices))
        d-hover @dest-hover
        action (game/get-current-action game)
        action-type (:type action)
        from-space (let [v (get-in action [:action :from])]
                     (when (vector? v) v))
        source-indicator
        (when from-space
          (let [[sx sy] (get locations from-space)]
            (case action-type
              :circulate
              ;; Lit-up food coin at the source's food position
              ^{:key "src-food"}
              [:g {:style {:pointer-events "none"}}
               [:circle
                {:cx sx :cy (- sy (* radius 0.3))
                 :r (* radius 0.34)
                 :fill "#FFD030"
                 :stroke (board/brighten color 0.8)
                 :stroke-width 3
                 :fill-opacity 1.0}]]

              (:move :grow)
              ;; Lit-up halo around the source element
              ^{:key "src-el"}
              [:g {:style {:pointer-events "none"}}
               [:circle
                {:cx sx :cy sy
                 :r (* radius 1.15)
                 :stroke (board/brighten color 0.8)
                 :stroke-width (* 0.32 radius)
                 :fill "white"
                 :fill-opacity 0.22}]]

              nil)))
        dest-halos
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 d-hovered? (= space d-hover)]
             ^{:key (str "target-" space)}
             [:circle
              {:cx x :cy y
               :r (* radius (if d-hovered? 1.15 1.0))
               :stroke (if d-hovered?
                         (board/brighten color 0.9)
                         (board/brighten color 0.7))
               :stroke-width (if d-hovered? (* 0.28 radius) (* 0.18 radius))
               :stroke-dasharray (when-not d-hovered? "4,3")
               :fill (if d-hovered?
                       (board/brighten color 0.6)
                       (board/brighten color 0.3))
               :fill-opacity (if d-hovered? 0.40 0.18)
               :style {:cursor "pointer"}
               :on-mouse-enter (fn [_e]
                                 (cancel-from-hover-clear!)
                                 (reset! dest-hover space))
               :on-mouse-leave (fn [_e] (reset! dest-hover nil))
               :on-click (fn [_e]
                           (reset! dest-hover nil)
                           (reset! from-hover nil)
                           (send-choice! choices space true))}]))
         spaces)]
    (if source-indicator
      (cons source-indicator dest-halos)
      dest-halos)))

(defn grow-element-highlights
  [game board turn choices]
  (chosen-organism-highlights
   game board
   (fn [element event]
     (send-choice! choices (:type element) true))
   turn choices))

(defn grow-from-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)

        source @food-source
        elements (game/current-organism-elements game)
        fed-growers (filter
                     (fn [{:keys [type space food]}]
                       (and
                        (= :grow type)
                        (< 0 (- food (get source space 0)))))
                     elements)
        spaces (map :space fed-growers)

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [{:keys [space] :as element} event]
           (when ((set spaces) space)
             (choose-food-source! space)
             (let [source @food-source]
               (if (get choices source)
                 (send-choice! choices source true)))))
         turn choices)
        
        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-circle
              x y radius (board/brighten color 0.2)
              (fn [event]
                (choose-food-source! space)
                (let [source @food-source]
                  (if (get choices source)
                    (send-choice! choices source true)))))))
         spaces)]
    (concat highlights element-highlights)))

(defn grow-to-highlights
  [game board turn choices]
  (let [spaces (keys choices)
        elements (game/current-organism-elements game)

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [element event]
           (if (get choices (:space element))
             (send-choice! choices (:space element) true)))
         turn choices)
        
        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (send-choice! choices space true)))]
    (concat highlights element-highlights)))

(defn find-highlights
  [game board colors turn choices]
  (let [highlights
        (condp = turn
          :open []
          :create (create-highlights game board colors turn choices)
          :introduce (introduce-highlights game board turn choices)
          :choose-organism (choose-organism-highlights game board turn choices)
          :choose-action-type (choose-action-type-highlights game board turn choices)
          :choose-action (choose-action-highlights game board turn choices)
          :eat-to (choose-target-highlights game board turn choices)
          :eat-from (choose-target-highlights game board turn choices)
          :circulate-from (choose-target-highlights game board turn choices)
          :circulate-to (choose-target-highlights game board turn choices)
          :grow-element (choose-action-type-highlights game board turn choices)
          :grow-from (grow-from-highlights game board turn choices)
          :grow-to (choose-target-highlights game board turn choices)
          :move-from (choose-target-highlights game board turn choices)
          :move-to (choose-target-highlights game board turn choices)
          [])]
    ^{:key "highlights"}
    (if (empty? highlights)
      []
      (into [] (concat [:g] highlights)))))

(defn organism-board
  [game board colors turn choices]
  (let [progress @transition-progress
        from-state @displayed-state
        to-state (:state game)
        animating? (and from-state
                        (< progress 1.0)
                        (not (identical? from-state to-state)))
        transitions (when animating? (diff-transitions from-state to-state))
        render-state (if animating?
                       (during-transition-state from-state transitions)
                       to-state)
        render-game-wrapper (assoc game :state render-state)
        svg (board/render-game board render-game-wrapper)
        ;; Always compute highlights from the current (post-transition) game
        ;; so the user can start interacting with the next phase immediately.
        ;; Hiding them during animation caused hover state to drop on the
        ;; stationary cursor once they reappeared.
        highlights (find-highlights game board colors turn choices)
        anim-overlay (when animating?
                       (animation-overlay board from-state (:adjacencies game)
                                          transitions progress))]
    (cond-> svg
      anim-overlay (conj anim-overlay)
      (not-empty highlights) (conj highlights))))

(defn generate-game-state
  [{:keys [ring-count player-count players colors player-captures mutations] :as invocation}]
  (let [ring-count   (if (number? ring-count) ring-count 4)
        player-count (if (number? player-count) player-count 2)
        symmetry (board/player-symmetry player-count)
        rings (take ring-count board/total-rings)
        starting
        (if (:RAIN mutations)
          (board/find-rain-spaces symmetry rings players)
          (board/find-starting-spaces symmetry rings players))
        _ (println "STARTING PLAYERS" starting)
        game-players (game/initial-players starting player-captures)
        game {:players game-players}
        board
        (board/generate-board
         colors
         (map first game-players)
         rings
         mutations)]
    (println "game players" game-players)
    {:game game
     :player js/playerKey
     :history []
     :board board
     :turn :create
     :choices []}))

(defn apply-invocation!
  [invocation]
  (println "INVOCATION" invocation)
  (let [generated (generate-game-state invocation)]
    (swap!
     player-captures-order
     (fn [captures-order]
       (reduce
        (fn [order [index captures]]
          (assoc order index captures))
        captures-order
        (map vector (range) (:player-captures invocation)))))
    (swap!
     player-order
     (fn [order]
       (reduce
        (fn [order [index player]]
          (assoc order index player))
        order
        (map vector (range) (:players invocation)))))
    (reset!
     game-state
     generated)))

(defn current-player-banner
  ([player color turn] (current-player-banner player color turn nil "/"))
  ([player color turn tooltip] (current-player-banner player color turn tooltip "/"))
  ([player color turn tooltip href]
   (let [show-tooltip (r/atom false)
         dismiss (fn dismiss []
                   (reset! show-tooltip false)
                   (.removeEventListener js/document "click" dismiss))]
     (fn [player color turn tooltip href]
       [:div
        {:style
         {:color "#fff"
          :border-radius "50px"
          :cursor "pointer"
          :background color
          :letter-spacing "8px"
          :font-family font-choice
          :margin "20px 0px"
          :padding "25px 60px"}}
        [:h1
         [:a
          {:style
           {:color "#fff"}
           :href href}
          player]]
        [:div
         {:style
          {:font-size "1.3em"
           :letter-spacing "5px"
           :margin "10px 0px"
           :display "flex"
           :align-items "center"
           :gap "12px"}}
         (string/join " " (string/split (name turn) #"-"))
         (when tooltip
           [:span
            {:style {:position "relative"}}
            [:span
             {:on-click (fn [e]
                          (.stopPropagation e)
                          (if @show-tooltip
                            (dismiss)
                            (do
                              (reset! show-tooltip true)
                              (.addEventListener js/document "click" dismiss))))
              :style
              {:font-size "0.7em"
               :cursor "pointer"
               :border "2px solid rgba(255,255,255,0.4)"
               :border-radius "50%"
               :width "1.4em"
               :height "1.4em"
               :display "inline-flex"
               :align-items "center"
               :justify-content "center"
               :line-height "1"
               :letter-spacing "0"
               :flex-shrink 0
               :opacity "0.45"
               :color "rgba(255,255,255,0.7)"}}
             "?"]
            (when @show-tooltip
              [:div
               {:on-click (fn [e] (.stopPropagation e))
                :style
                {:position "absolute"
                 :top "2em"
                 :left "0"
                 :z-index 100
                 :background "rgba(30,30,30,0.95)"
                 :color "#ddd"
                 :border-radius "12px"
                 :padding "16px 20px"
                 :width "320px"
                 :font-size "0.85em"
                 :letter-spacing "1px"
                 :line-height "1.6"
                 :white-space "pre-line"
                 :box-shadow "0 4px 20px rgba(0,0,0,0.5)"
                 :cursor "default"}}
               tooltip])])]]))))

(def turn-descriptions
  {:pass "pass"
   :actions-complete "resolve conflicts"
   :resolve-conflicts "check integrity"
   :player-victory "declare victory!"
   :check-integrity "confirm turn"})

(defn progress-control
  [turn choices advance]
  (if-let [description (get turn-descriptions turn)]
    [:span
     {:style
      {:color "#fff"
       :cursor "pointer"
       :border-radius "20px"
       :background "hsl(100,50%,50%)"
       :font-size "1.2em"
       :letter-spacing "4px"
       :margin "10px 10px"
       :padding "5px 20px"}
      :on-click
      (fn [event]
        (send-state! (get-in choices [advance :state]) true))}
     description]))

(defn current-action-index
  [num-actions actions]
  (cond
    (empty? actions) 0

    (game/complete-action? (last actions))
    (when (not= (count actions) num-actions)
      (count actions))

    :else (dec (count actions))))

(def background-color "#222")

(defn eat-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background background-color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :color "#fff"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      "eat"]
     [:span
      {:style
       {:margin "0px 5px"}}
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(def element-choice-map
  {:eat "eater"
   :grow "grower"
   :move "mover"})

(defn grow-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background background-color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :cursor "pointer"
          :color "#fff"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      "grow"]
     (if-let [element (-> action :action :element)]
       [:span
        {:style
         {:margin "0px 20px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "5px"
          :background background-color
          :font-size "1.0em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "2px 10px"}}
        element]
       [:span
        {:style
         {:margin "0px 10px"}}
        (map-indexed
         (fn [index element-choice]
           ^{:key element-choice}
           [:span
            [:span
             {:style
              {:margin "0px 10px"
               :color "#fff"
               :border-width "2px"
               :border-radius "5px"
               :background color
               :font-size "1.0em"
               :letter-spacing "5px"
               :font-family font-choice
               :cursor "pointer"
               :padding "2px 10px"}
              :on-click
              (fn [event]
                (if-let [choice (get choices element-choice)]
                  (send-state! (:state choice) true)))}
             (element-choice-map element-choice)]
            (if (not= index (dec (count choices)))
              " / ")])
         (keys choices))])
     [:span
      (if-let [from (-> action :action :from)]
        (concat
         (list " from ")
         (map
          (fn [[[ring space] food]]
            ^{:key [ring space]}
            [:span
             {:style
              {:background (get board-colors ring)
               :color "#fff"
               :margin "0px 5px"
               :font-family font-choice
               :padding "5px 5px"
               :border-radius "5px"}}
             (str " " ring " " space " : " food " ")])
          from)))
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn move-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background background-color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :color "#fff"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      "move"]
     [:span
      {:style
       {:margin "0px 5px"}}
      (if-let [from (-> action :action :from)]
        (list
         " from "
         [:span
          {:style
           {:background (get board-colors (first from))
            :color "#fff"
            :font-family font-choice
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " from)]))
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn circulate-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background background-color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :color "#fff"
          :cursor "pointer"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      (if (-> action :action :pass)
        "pass"
        "circulate")]
     [:span
      {:style
       {:margin "0px 5px"}}
      (if-let [from (-> action :action :from)]
        (list
         " from "
         [:span
          {:style
           {:background (get board-colors (first from))
            :color "#fff"
            :font-family font-choice
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " from)]))
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn pass-action-control
  [board-colors turn choices color action action-index])

(def action-control-map
  {:eat eat-action-control
   :grow grow-action-control
   :move move-action-control
   :circulate circulate-action-control
   :pass pass-action-control})

(defn past-action-control
  [board-colors turn choices color choice action action-index]
  [:div
   [(get action-control-map (:type action)) board-colors turn choices color action action-index]])

(defn choose-action-control
  [turn choices color choice]
  [:span
   {:style
    {:color "#fff"
     :border-radius "20px"
     :margin "20px 5px"
     :cursor "pointer"
     :background color
     :font-size "1.2em"
     :letter-spacing "7px"
     :font-family font-choice
     :padding "5px 20px"}
    :on-click
    (condp = turn
      :choose-action
      (fn [event]
        (if (get choices choice)
          (send-choice! choices choice true)))
      (fn [event]))}
   choice])

(defn circulate-control
  [turn choices color]
  [:span
   {:style
    {:color "#fff"
     :border-radius "20px"
     :margin "20px 5px"
     :cursor "pointer"
     :background color
     :font-size "1.2em"
     :letter-spacing "7px"
     :font-family font-choice
     :padding "5px 20px"}
    :on-click
    (condp = turn
      :choose-action
      (fn [event]
        (if (:circulate choices)
          (send-choice! choices :circulate true)))
      (fn [event]))}
   "circulate"])

(defn current-action-control
  [board-colors turn choices color choice action action-index]
  (if-let [type (:type action)]
    [:div
     [(get action-control-map type) board-colors turn choices color action action-index]]
    [:div
     {:style
      {:margin "20px 0px"}}
     (if (get choices choice)
       [choose-action-control turn choices color choice])
     (if (:circulate choices)
       [:span
        " / "
        [circulate-control turn choices color]])]))

(defn future-control
  [color choice]
  [:span
   {:style
    {:color color
     :border-style "solid"
     :border-width "2px"
     :border-radius "10px"
     :margin "20px 5px"
     :background background-color
     :font-size "1.0em"
     :letter-spacing "7px"
     :font-family font-choice
     :padding "5px 20px"}}
   choice])

(defn future-action-control
  [board-colors turn choices color choice action action-index]
  [:div
   {:style
    {:margin "20px 0px"}}
   [future-control color choice]
   " / "
   [future-control color "circulate"]])

(defn action-controls
  [board-colors turn choices color {:keys [choice num-actions actions] :as organism-turn}]
  (if choice
    (let [current-action (current-action-index num-actions actions)]
      [:div
       (map
        (fn [action-index]
          (let [action
                (if (< action-index (count actions))
                  (nth actions action-index)
                  {})]
            ^{:key action-index}
            (cond

              (nil? current-action)
              [past-action-control board-colors turn choices color choice action action-index]

              (> action-index current-action)
              [future-action-control board-colors turn choices color choice action action-index]

              (= action-index current-action)
              [current-action-control board-colors turn choices color choice action action-index]

              :else
              [past-action-control board-colors turn choices color choice action action-index])))

        (range num-actions))])
    [:div]))

(defn undo-control
  [turn choices state]
  [:div
   {:style
    {:font-family font-choice
     :margin "40px 0px"}}

   [:div
    {:style
     {:margin "15px 0px"}}
    [:span
     {:title "reset to the beginning of your turn"
      :style
      {:color "#fff"
       :cursor "pointer"
       :border-radius "10px"
       :background "hsl(200,50%,80%)"
       :font-size "1.2em"
       :letter-spacing "4px"
       :margin "0px 10px"
       :padding "5px 20px"}
      :on-click
      (fn [event]
        (if (and
             (= turn :introduce)
             (not= @introduction empty-introduction))
          (reset! introduction empty-introduction)
          (send-clear!)))}
     "clear"]]

   [:div
    {:style
     {:margin "15px 0px"}}
    [:span
     {:title "take one step back, potentially to previous player's turn"
      :style
      {:color "#fff"
       :cursor "pointer"
       :border-radius "10px"
       :background "hsl(0,50%,50%)"
       :font-size "1.2em"
       :letter-spacing "4px"
       :margin "0px 10px"
       :padding "5px 20px"}
      :on-click
      (fn [event]
        (if (and
             (= turn :introduce)
             (not= @introduction empty-introduction))
          (reset! introduction empty-introduction)
          (do
            (reset! food-source {})
            (send-reset! state))))}
     "undo"]]

   (when (= turn :choose-action)
     [:div
      {:style
       {:margin "15px 0px"}}
      [:span
       {:title "pass this action"
        :style
        {:color "#fff"
         :cursor "pointer"
         :border-radius "10px"
         :background "hsl(100,50%,50%)"
         :font-size "1.2em"
         :letter-spacing "4px"
         :margin "0px 10px"
         :padding "5px 20px"}
        :on-click
        (fn [event]
          (send-state!
           (-> {:state state}
               (game/choose-action :circulate)
               (game/pass-action)
               :state)
           true))}
       "pass"]])

   [:div
    {:style
     {:margin "15px 0px"}}
    [progress-control turn choices (if (= turn :pass) :pass :advance)]]])

(defn organism-controls
  [game board turn choices history]
  (let [player-turn (game/get-player-turn game)
        organism-turn (game/get-organism-turn game)
        action-type (:choice organism-turn)
        current-action (last (:actions organism-turn))

        player-colors (:player-colors board)
        current-player (game/current-player game)
        current-color (get player-colors current-player)
        dormant-color (board/brighten current-color -0.7)
        focus-color (board/brighten current-color 0.4)
        board-colors (into {} (:colors board))

        element-radius 45
        element-controls
        (map
         vector
         [[50 50] [150 50] [100 130]]
         [:eat :grow :move])
        {:keys [chosen-space chosen-element progress] :as introduce} @introduction]

    (if current-player
      [:div
       {:style
        {:margin "20px 20px"}}
       [current-player-banner current-player current-color turn nil (str js/playerPath "/" js/playerKey)]
       [:div
        {:style
         {:margin "0px 40px"}}
        [:svg
         {:width 200 :height 180}

         ;; ELEMENT CONTROLS
         (vec
          (concat
           [:g]
           (for [[location type] element-controls]
             (let [type->location
                   (into
                    {}
                    (map
                     (fn [[location type]]
                       [type location])
                     element-controls))

                   element-state
                   (cond 
                     (and
                      (not (= turn :choose-organism))
                      (or
                       (and
                        (= turn :introduce)
                        (= chosen-element type))
                       (= type action-type)))
                     :focus

                     (or
                      (and
                       (= turn :introduce)
                       (some #{type} (vals progress)))
                      (not (nil? action-type)))
                     :dormant
                     :else :neutral)
                   
                   color
                   (condp = element-state
                     :focus focus-color
                     :dormant dormant-color
                     :neutral current-color)]

               ^{:key type}
               (-> (board/render-element
                    color color
                    {:ratio 0.02 :color "#ccc"}
                    location
                    element-radius
                    {:type type :food 0})
                   (assoc-prop :style {:cursor "pointer"})
                   (assoc-prop :title type)
                   
                   (assoc-prop
                    :on-click
                    (fn [event]
                      (condp = turn
                        :introduce
                        (if (= type chosen-element)
                          (swap!
                           introduction
                           dissoc
                           :chosen-element)
                          (if chosen-space
                            (do
                              (swap!
                               introduction
                               (fn [intro]
                                 (-> intro
                                     (dissoc :chosen-element)
                                     (dissoc :chosen-space)
                                     (update :progress (fn [pro] (assoc pro chosen-space type))))))
                              (send-introduction! choices @introduction))
                            (swap! introduction assoc :chosen-element type)))
                        :choose-action-type
                        (send-choice! choices type true)))))))))]

        [:br]

        (when-not (= turn :choose-organism)
          [action-controls board-colors turn choices current-color organism-turn])

        (if-not (-> game :state :winner)
          [undo-control turn choices (:state game)])]])))

(defn flex-direction
  [direction]
  {:style
   {:display "flex"
    :flex-direction flex-direction}})

(defn flex-grow
  [direction grow]
  (assoc-in
   (flex-direction direction)
   [:style :flex-grow]
   grow))

(defn game-layout
  [inner]
  [:div
   (assoc-in
    (flex-direction "column")
    [:style :color]
    "#eee")
   inner])

(defn reset-colors-input
  [color]
  (let [invocation @board-invocation]
    [:input
     {:type :button
      :value "reset colors"
      :style
      {:border-radius "20px"
       :color "#fff"
       :cursor "pointer"
       :background color
       :border "0px solid"
       :font-size "1.0em"
       :letter-spacing "3px"
       :margin "10px 0px"
       :padding "7px 20px"}
      :on-click
      (fn [event]
        (let [invocation @board-invocation
              colors (board/generate-colors-buffer
                      board/total-rings
                      (:ring-count invocation)
                      max-players)]
          (-> invocation
              (assoc :colors colors)
              send-create!)))}]))

        ;; (if valid?
        ;;   (ws/send-transit-message!
        ;;    {:type "trigger-creation"})
        ;;   (dom/redirect!
        ;;    (str js/playerPath "/" js/playerKey)))

(defn ring-count-input
  [color]
  (let [invocation @board-invocation]
    [:div
     [:select
      {:id "ring-count"
       :name "ring-count"
       :value (:ring-count invocation)
       :style
       {:background-color color}
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)
               invocation @board-invocation
               colors (board/generate-colors-buffer
                       board/total-rings
                       value
                       max-players)]
           (-> invocation
               (assoc :ring-count value)
               (assoc :colors colors)
               send-create!)))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 3 8))]
     [:label
      {:for "ring-count"
       :style
       {:font-size "1.5em"}}
      "rings"]]))

(defn player-count-input
  [color]
  (let [invocation @board-invocation]
    [:div
     [:select
      {:id "player-count"
       :name "player-count"
       :value (:player-count invocation)
       :style
       {:background-color color}
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)
               order @player-order
               captures-order @player-captures-order
               colors (board/generate-colors-buffer
                       board/total-rings
                       (:ring-count invocation)
                       max-players)
               players (vec
                        (take
                         (if (get-in invocation [:mutations :RAIN])
                           (inc value)
                           value)
                         order))
               captures (vec
                         (take
                          (if (get-in invocation [:mutations :RAIN])
                           (inc value)
                           value)
                          captures-order))]
           (-> invocation
               (assoc :colors colors)
               (assoc :player-count value)
               (assoc :players players)
               (assoc :player-captures captures)
               send-create!)))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 1 11))]
     [:label
      {:for "player-count"
       :style
       {:font-size "1.5em"}}
      "players"]]))

(defn organism-victory-input
  [color]
  (let [invocation @board-invocation]
    [:div
     [:select
      {:id "organism-victory"
       :name "organism-victory"
       :value (:organism-victory invocation)
       :style
       {:background-color color}
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)]
           (-> invocation
               (assoc :organism-victory value)
               (send-create!))))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 3 14))]
     [:label
      {:for "organism-victory"
       :style
       {:font-size "1.5em"}}
      "organisms for victory"]]))

(defn send-player-name!
  [index player-name]
  (swap! player-order assoc index player-name)
  (swap! board-invocation update :players
         (fn [players] (assoc (vec players) index player-name)))
  (components/send-player-name! index player-name))

(defn player-slot-input
  "Wraps the shared player-search-input for organism's create page."
  [index color player page-player invocation in-game?]
  [components/player-search-input
   {:slot-id   index
    :value     player
    :color     color
    :game-type "organism"
    :search?   in-game?
    :placeholder (if in-game? "search players..." "click to join")
    :on-change (fn [v] (send-player-name! index v))
    :on-select (fn [{:keys [name bot?]}]
                 ;; If picking a bot, auto-suffix alphabetically (OBO-A, OBO-B, ...)
                 (let [existing (->> (:players invocation)
                                     (map-indexed vector)
                                     (remove (fn [[i _]] (= i index)))
                                     (map second)
                                     set)
                       chosen (if bot?
                                (or (some
                                     (fn [c]
                                       (let [candidate (str name "-" c)]
                                         (when-not (existing candidate) candidate)))
                                     (map char (range 65 91))) ;; A-Z
                                    name)
                                name)]
                   (send-player-name! index chosen)
                   (send-open-game! (update invocation :players assoc index chosen))))
    :on-focus  (fn []
                 (when (and (not in-game?) (empty? player))
                   (send-player-name! index page-player)
                   (send-open-game! (update invocation :players assoc index page-player))))
    :on-blur   (fn [] (send-open-game! invocation))}])

(defn players-input
  [page-player invocation]
  (let [{:keys [player-count colors player-captures mutations]} invocation
        player-count (if (:RAIN mutations)
                       (inc player-count)
                       player-count)
        order @player-order
        captures-order @player-captures-order
        in-game? (some #{page-player} (take player-count order))]
    [:div
     [:h3
      {:style
       {:margin "20px 0px 0px 0px"}}
      [:span
       {:title "click an empty field to join the game\nor modify to add other players"}
       "players joined "]
      [:span
       {:title "how many captures each player is required to win"
        :style {:font-size "0.8em"}}
       " (capture limit)"]]
     (map
      (fn [index color player captures]
        ^{:key index}
        [:div
         [player-slot-input index color player page-player invocation in-game?]

         [:select
          {:value captures
           :style
           {:background-color color}
           :on-change
           (fn [event]
             (let [value (-> event .-target .-value js/parseInt)]
               (swap!
                player-captures-order
                assoc index value)
               (-> invocation
                   (assoc
                    :player-captures
                    (vec
                     (take
                      player-count
                      @player-captures-order)))
                   (send-create!))))}

          (map
           (fn [n]
             ^{:key n}
             [:option
              {:value n}
              n])
           (range 1 14))]])
      (range)
      (reverse
       (take
        player-count
        (map last colors)))
      order
      (take player-count captures-order))]))

(defn create-button
  [active-color inactive-color invocation]
  (let [valid? (board/valid-invocation? invocation)]
    [:input
     {:type :button
      :value (if valid? "CREATE" "incomplete")
      :style
      {:border-radius (if valid? "50px" "10px")
       :color "#fff"
       :cursor "pointer"
       :background (if valid? active-color inactive-color)
       :border "3px solid"
       :font-size "2em"
       :letter-spacing "8px"
       :margin "10px 0px"
       :padding "25px 60px"}
      :on-click
      (fn [event]
        (if valid?
          (let [game-key (if (empty? @create-game-key)
                           (let [k (generate-game-key)]
                             (reset! create-game-key k)
                             k)
                           @create-game-key)
                trigger! (fn []
                           (ws/send-transit-message!
                            {:type "create"
                             :invocation @board-invocation})
                           (ws/send-transit-message!
                            {:type "trigger-creation"}))]
            (if @ws/ws-channel
              (trigger!)
              (connect-create-ws! game-key trigger!)))
          (dom/redirect!
           (str js/playerPath "/" js/playerKey))))}]))

(defn description-input
  [{:keys [description] :as invocation} foreground-color background-color]
  [:div
   [:h3
    {:style
     {:margin "20px 0px 0px 0px"}}
    [:span
     {:title "explain a bit about the game you are creating for potential players"}
     "description"]]
   [:textarea
    {:value (or description "")
     :rows (inc (quot (count description) 49))
     :style
     {:border-radius "25px"
      :color foreground-color
      :background background-color
      ;; :border "3px solid"
      :font-size "0.9em"
      :letter-spacing "1px"
      :margin "2px 0px"
      :width "460px"
      :padding "10px 30px"}
     ;; :on-blur
     ;; (fn [event]
     ;;   (send-open-game!
     ;;    (assoc invocation :description @description)))
     :on-change
     (fn [event]
       (let [value (-> event .-target .-value)]
         (send-create!
          (assoc invocation :description value))))}]])

(defn invocation-player-colors
  [number invocation]
  (reverse
   (take
    number
    (map
     last
     (:colors invocation)))))

(defn adjust-players
  [invocation player-count]
  (-> invocation
      (assoc :players (take player-count @player-order))
      (assoc :player-captures (take player-count @player-captures-order))))

(defn increase-players
  [invocation]
  (let [player-count (inc (:player-count invocation))]
    (adjust-players invocation player-count)))

(defn decrease-players
  [invocation]
  (let [player-count (:player-count invocation)]
    (adjust-players invocation player-count)))

(def invocation-mutations
  {:RAIN
   {:mutate increase-players
    :unmutate decrease-players}})

(defn mutate-invocation
  [mutation-key mutation-state invocation]
  (let [invocation (assoc-in invocation [:mutations mutation-key] mutation-state)]
    (if-let [mutate (get-in invocation-mutations [mutation-key :mutate])]
      (mutate invocation)
      invocation)))

(defn unmutate-invocation
  [mutation-key mutation-state invocation]
  (let [invocation (update invocation :mutations dissoc mutation-key)]
    (if-let [mutate (get-in invocation-mutations [mutation-key :unmutate])]
      (mutate invocation)
      invocation)))

(defn mutation-choice
  [color invocation [mutation-key mutation-description]]
  ^{:key mutation-key}
  [:div
   [:input
    {:type "checkbox"
     :id mutation-key
     :name mutation-key
     :value mutation-key
     :checked (get-in invocation [:mutations mutation-key])
     :style
     {:margin "5px 10px"
      :background-color color}
     :on-change
     (fn [event]
       (let [target (.-target event)
             checked (.-checked target)
             mutation-state (get game/default-mutation-state mutation-key {})
             invocation
             (if checked
               (mutate-invocation mutation-key mutation-state invocation)
               (unmutate-invocation mutation-key mutation-state invocation))]
         (send-create! invocation)))}]
   [:label
    {:for mutation-key
     :style
     {:color color}}
    (display-mutation mutation-key mutation-description)]])

(defn mutations-select
  [color invocation]
  [:div
   [:h3
    {:style
     {:margin "20px 0px 0px 0px"}}
    [:span
     {:title "choose which mutations you want to be active in the game"}
     "mutations"]]
   [:div
    (map (partial mutation-choice color invocation) possible-mutations)]])

(defn connect-create-ws!
  ([game-key] (connect-create-ws! game-key nil))
  ([game-key on-open]
   (components/connect-create-ws! "/ws/organism/play/" game-key update-messages! on-open)))

(defn game-name-input
  [color]
  (let [connected? (some? @ws/ws-channel)]
    [:div
     {:style {:margin-bottom "30px"}}
     [:h3
      {:style {:margin "20px 0px 0px 0px"}}
      "name"]
     [:input
      {:value @create-game-key
       :style
       {:border-radius "25px"
        :color "#fff"
        :background (if connected? color "#555")
        :border (str "3px solid " (if connected? color "#777"))
        :font-size "1.5em"
        :letter-spacing "6px"
        :margin "2px 0px"
        :width "366px"
        :padding "10px 30px"}
       :on-change
       (fn [event]
         (reset! create-game-key (-> event .-target .-value)))
       :on-blur
       (fn [_] (connect-create-ws! @create-game-key))
       :on-key-up
       (fn [event]
         (when (= (.-key event) "Enter")
           (connect-create-ws! @create-game-key)))}]]))

(def create-explanation
  (string/join "\n\n"
    ["Every game has a unique key. A game will always be in one of three states: OPEN / ACTIVE / COMPLETE."
     "From this page you can choose the number of rings and number of players, as well as the number of organisms required for victory."
     "You can also choose which other players will be in the game, as well as their personal capture limit required for victory (this defaults to 5)."
     "If you want to leave some player spots open for others to join, just leave them blank. It will show up in everyone's player page under OPEN."
     "To join an open game, simply click on the empty player slot and it will fill in your player name."
     "Once all players have joined and you feel good about the game, hit the CREATE button to begin!"]))

(defn create-page
  []
  (let [invocation @board-invocation
        {:keys [game board turn choices]} @game-state
        {:keys [state turn-order]} game
        turn-order (:players invocation)
        player-captures (:player-captures invocation)
        organism-victory (:organism-victory invocation)
        description (:description invocation)
        mutations (:mutations invocation)
        invocation-colors (invocation-player-colors (count turn-order) invocation)
        player-colors (into {} (map vector turn-order invocation-colors))
        create-color (-> invocation :colors rest first last)
        select-color (-> invocation :colors first last)
        inactive-color (-> invocation :colors last last)]
    (game-layout
     [:main
      (flex-grow "row" 1)
      [:nav
       {:style
        {:width "30%"}}
       [:div
        {:style
         {:margin "20px 20px"}}
        [current-player-banner js/playerKey (get player-colors js/playerKey inactive-color) "create game" create-explanation js/homePath]]
       [:form
        {:style
         {:margin "40px 60px"}}
        [game-name-input create-color]
        [ring-count-input select-color]
        [player-count-input select-color]
        [description-input invocation select-color inactive-color]
        [players-input js/playerKey invocation]
        [:div
         {:style {:display "flex" :flex-direction "column" :align-items "center" :width "fit-content" :margin "20px 40px"}}
         [reset-colors-input inactive-color]
         [create-button create-color inactive-color invocation]]
        [mutations-select create-color invocation]]]
      [:article
       {:style {:flex-grow 1}}
       [organism-board game board invocation-colors turn choices]]
      (println "INVOCATION" invocation)
      [:aside
       {:style
        {:width "30%"}}
       [chat-panel description turn-order organism-victory invocation-colors player-colors player-captures mutations state [] nil @chat]]])))

(defn play-player-banner
  "Current-player box for the play page. The player name shrinks/wraps to fit
   the column (never grows the layout), and the phase shows a smaller subphase /
   current choice beside it."
  [player color turn subphase href]
  [:div
   {:style
    {:color "#fff"
     :border-radius "40px"
     :background color
     :font-family font-choice
     :margin "20px 0px"
     :padding "18px 30px"
     :overflow "hidden"}}
   [:a
    {:href href
     :style
     {:color "#fff"
      :display "block"
      :font-size "1.6em"
      :font-weight "bold"
      :letter-spacing "4px"
      :overflow-wrap "anywhere"
      :line-height "1.1"}}
    player]
   (when turn
     [:div
      {:style
       {:margin "10px 0px 0px 0px"
        :display "flex"
        :align-items "baseline"
        :gap "10px"
        :flex-wrap "wrap"}}
      [:span {:style {:font-size "1.15em" :letter-spacing "3px"}}
       (string/join " " (string/split (name turn) #"-"))]
      (when (and subphase (not= subphase turn))
        [:span {:style {:font-size "0.8em" :letter-spacing "2px" :opacity "0.7"}}
         (string/join " " (string/split (name subphase) #"-"))])])])

(defn game-info-panel
  "Merged single info column for the play page: game name + round, current
   player + phase/subphase, description, action/turn controls + clear/undo,
   then score / history / help / discussion. The element controls are gone —
   every action now happens on the board itself."
  [game board turn choices history cursor
   description turn-order organism-victory colors player-colors
   player-captures mutations state chat]
  (let [organism-turn (game/get-organism-turn game)
        action-type (:choice organism-turn)
        current-player (game/current-player game)
        current-color (or (get player-colors current-player) (first colors) "#445")
        board-colors (into {} (:colors board))]
    [:div
     {:style {:margin "20px"}}
     ;; game name + round
     [round-banner current-color (:round state)]
     ;; current player + phase + subphase
     (when current-player
       [play-player-banner current-player current-color turn action-type
        (str js/playerPath "/" js/playerKey)])
     [:div
      {:style {:margin "10px 10px"}}
      ;; description (moved over from the old right column)
      [description-panel current-color description]
      ;; action / turn controls + clear / undo  (element controls removed)
      (when current-player
        [:div
         (when-not (= turn :choose-organism)
           [action-controls board-colors turn choices current-color organism-turn])
         (when-not (-> game :state :winner)
           [undo-control turn choices (:state game)])])
      ;; score / history / help / discussion
      [scoreboard turn-order organism-victory colors player-captures mutations state]
      [history-controls history cursor]
      [help-panel current-color]
      [:h3 "discussion"]
      [chat-list player-colors chat]
      [:br]
      [chat-input]]]))

(defn game-page
  []
  (let [invocation @board-invocation
        {:keys [game board turn choices history cursor]} @game-state
        {:keys [state turn-order]} game
        {:keys [player-captures organism-victory description mutations]} invocation
        state (if cursor (nth history cursor) state)
        game (assoc game :state state)
        invocation-colors (invocation-player-colors (count turn-order) invocation)
        [turn choices] (if cursor (choice/find-state game) [turn choices])
        {:keys [player-colors]} board]
    (game-layout
     [:main
      (flex-grow "row" 1)
      [:aside
       {:style {:width "34%" :min-width "340px"}}
       [game-info-panel game board turn choices history cursor
        description turn-order organism-victory invocation-colors player-colors
        player-captures mutations state @chat]]
      [:article
       {:style {:flex-grow 1}}
       [organism-board game board invocation-colors turn choices]]])))


(defn open-games-section
  "Organism wrapper around the shared open-games-section."
  [player games]
  [components/open-games-section
   {:games games
    :link-prefix "/organism/create/"
    :current-player player
    :font-family font-choice
    :colors-fn (fn [invocation]
                 (invocation-player-colors (:player-count invocation) invocation))}])

(defn player-active?
  [player games]
  (let [active-games (get games "active")]
    (some?
     (some
      (fn [game]
        (= player (:current-player game)))
      active-games))))

(defn active-games-section
  [player games]
  (when-not (empty? games)
    [:div
     {:style
      {:margin "20px 40px"}}
     [:h2
      [:span
       {:title "A solid color row indicates it is your turn in that game.\nThe icon on the tab for this page will turn green when it is your turn."}
       "ACTIVE"]]
     (for [{:keys [game round players player-colors current-player invocation]} games]
       (let [player-color (get player-colors player)
             ring-count (:ring-count invocation)
             organism-victory (:organism-victory invocation)]
         ^{:key game}
         [:div
          {:style
           (if (= player current-player)
             {:background player-color
              :margin "10px 20px"
              :padding "10px 0px"
              :border-radius "10px"}
             {:margin "10px 20px"
              :padding "10px 0px"})}
          [:span
           {:title
            (str
             (when ring-count
               (str ring-count " rings | "))
             (when organism-victory
               (str organism-victory " organisms for victory\n\n"))
             (:description invocation))}
           [:a
            {:href (str "/organism/play/" game)
             :style
             {:color "#fff"
              :border-radius "15px"
              :background player-color
              :padding "10px 20px"
              :letter-spacing "5px"
              :font-family font-choice
              :font-size "1.3em"}}
            game]]
          [:span
           {:style
            {:margin "0px 20px"}}
           " round " (inc round)]
          (for [game-player players]
            (let [current-color (get player-colors game-player)]
              ^{:key game-player}
              [:span
               [:a
                {:href (str js/playerPath "/" game-player)
                 :style
                 (if (= game-player current-player)
                   {:color "#fff"
                    :border-radius "20px"
                    :background current-color
                    :margin "0px 10px"
                    :padding "7px 20px"}
                   {:padding "5px 10px"
                    :margin "0px 10px"
                    :border-style "solid"
                    :border-width "2px"
                    :border-color current-color
                    :border-radius "5px"
                    :color current-color})}
                game-player]]))]))]))

(defn complete-games-section
  [player games]
  (when-not (empty? games)
    [:div
     {:style
      {:margin "20px 40px"}}
     [:h2 "COMPLETE"]
     (for [{:keys [game round players player-colors winner]} (reverse games)]
       (let [player-color (get player-colors player)]
         ^{:key game}
         [:div
          {:style
           (if (= player winner)
             {:background player-color
              :margin "10px 20px"
              :padding "10px 0px"
              :border-radius "10px"}
             {:margin "10px 20px"
              :padding "10px 0px"})}
          [:span
           [:a
            {:href (str "/organism/play/" game)
             :style
             {:color "#fff"
              :border-radius "15px"
              :background player-color
              :padding "10px 20px"
              :letter-spacing "5px"
              :font-family font-choice
              :font-size "1.3em"}}
            game]]
          [:span
           {:style
            {:margin "0px 20px"}}
           " round " (inc round)]
          (for [game-player players]
            (let [current-color (get player-colors game-player)]
              ^{:key game-player}
              [:span
               [:a
                {:href (str js/playerPath "/" game-player)
                 :style
                 (if (= game-player winner)
                   {:color "#fff"
                    :border-radius "20px"
                    :background current-color
                    :margin "0px 10px"
                    :padding "7px 20px"}
                   {:padding "5px 10px"
                    :margin "0px 10px"
                    :border-style "solid"
                    :border-width "2px"
                    :border-color current-color
                    :border-radius "5px"
                    :color current-color})}
                game-player]]))]))]))

(defn player-page-banner
  [player color turn]
  [:div
   {:style
    {:color "#fff"
     :border-radius "50px"
     :cursor "pointer"
     :background color
     :letter-spacing "8px"
     :font-family font-choice
     :margin "20px 0px"
     :padding "25px 60px"}
    :on-click
    (fn [event]
      (let [color (board/random-color 0.2 0.9)]
        (swap! player-preferences assoc :color color)
        (ajax/post-preferences! player {:color color})))}
   [:h1
    [:a
     {:style
      {:color "#fff"}
      :href js/homePath}
     player]]
   [:div
    {:style
     {:font-size "1.3em"
      :letter-spacing "5px"
      :margin "10px 0px"}}
    (string/join " " (string/split (name turn) #"-"))]])

(defn player-page
  [player]
  (let [games @player-games
        color (:color @player-preferences)]
    [:div
     {:style
      {:padding "20px"
       :color "#eee"}}
     [player-page-banner player color "games"]
     [open-games-section player (get games "open")]
     [active-games-section player (get games "active")]
     [complete-games-section player (get games "complete")]]))

(defn valid-player-name?
  [players player]
  (and
   (not
    (empty? player))
   (not
    (players player))))

(defonce player-key
  (r/atom ""))

(defonce home-color
  (r/atom (board/random-color 0.5 0.8)))

(defn home-page
  [player-records]
  (let [color @home-color
        players (set (map :key player-records))
        active-color "#3b5"
        inactive-color "#444"]
    [:div
     {:style
      {:padding "20px 0px"
       :color "#eee"}}
     [:div
      {:style
       {:color "#fff"
        :border-radius "50px"
        :cursor "pointer"
        :background color
        :letter-spacing "8px"
        :font-size "1.2em"
        :margin "0px 20px"
        :padding "25px 60px"}}
      [:h1 "ORGANISM"]
      [:h2 "welcome"]]
     [:div
      {:style
       {:margin "20px 20px"
        :padding "25px 60px"
        :font-size "1.2em"
        :font-family font-choice}}
      [:p "Welcome to ORGANISM!"]
      [:p "To begin, choose a player name ->"]
      [:input
       {:type :text
        :style
        {:border-radius "25px"
         :color "#fff"
         :background color
         :border "3px solid"
         :font-size "2em"
         :letter-spacing "8px"
         :margin "20px 20px"
         :padding "10px 40px"}
        :on-key-up
        (fn [event]
          (let [value (-> event .-target .-value)
                key (-> event .-key)]
            (reset! player-key value)
            (let [valid? (valid-player-name? players @player-key)]
              (if (and valid? (= key "Enter"))
                (dom/redirect!
                 (str js/playerPath "/" value))))))}]
      [:div
       (let [valid? (valid-player-name? players @player-key)]
         [:input
          {:type :button
           :value (if valid? "PLAY" "name taken")
           :style
           {:border-radius (if valid? "50px" "10px")
            :color "#fff"
            :cursor "pointer"
            :background (if valid? active-color inactive-color)
            :border "3px solid"
            :font-size (if valid? "1.3em" "1.1em")
            :letter-spacing "8px"
            :margin "15px 50px"
            :padding "10px 40px"}
           :on-click
           (fn [event]
             (when (valid-player-name? players @player-key)
               (dom/redirect!
                (str js/playerPath "/" @player-key))))}])]]]))

;; observe-games-section moved to organism.components (shared library)

(defn observe-page []
  [components/observe-page
   {:title "observe"
    :games @observe-games
    :link-prefix "/organism/play/"
    :player-link-prefix (or js/playerPath "/organism/player/")
    :home-path js/homePath
    :font-family font-choice
    :colors-fn (fn [invocation]
                 (invocation-player-colors (count (:players invocation)) invocation))}])

(defn stats-page []
  [components/players-page
   {:title "players"
    :stats @player-stats
    :player-link-prefix (or js/playerPath "/organism/player")
    :home-path js/homePath
    :font-family font-choice}])

(defn page-container
  []
  (cond
    js/isStats    [stats-page]
    js/isObserve  [observe-page]
    js/isCreate   [create-page]
    js/playerKey  (cond
                    js/playKey (let [invocation @board-invocation]
                                 (if (:created invocation)
                                   [game-page]
                                   [create-page]))
                    :else      [player-page js/playerKey])
    :else         [home-page (reader/read-string js/players)]))

(defn update-messages!
  [{:keys [type] :as received}]
  (println "MESSAGE RECEIVED" received)
  (condp = type
    "initialize"
    (if js/isCreate
      (dom/redirect! (str "/organism/play/" @create-game-key))
      (do
        (swap! game-state initialize-game received)
        (reset! board-invocation (:invocation received))
        (reset! clear-state (-> received :game :state))
        ;; Seed the animation baseline so the first in-game transition has
        ;; a starting state to interpolate from.
        (let [current-state (-> @game-state :game :state)]
          (reset! displayed-state current-state)
          (reset! target-state current-state)
          (reset! transition-progress 1.0))
        (swap! chat initialize-chat received)
        (if-let [cursor (:cursor @game-state)]
          (let [total (count (:history received))]
            (if (< cursor total)
              (set-history-advance! total cursor))))))
    "create"
    (if js/isCreate
      ;; On the create page, local state is authoritative — don't let
      ;; the server's default overwrite what the user configured
      nil
      (do
        (reset! board-invocation (:invocation received))
        (reset! chat (:chat received))
        (apply-invocation! @board-invocation)))
    "player-name"
    (let [{:keys [index player]} received]
      (swap! player-order assoc index player)
      (swap! board-invocation update :players (fn [players] (assoc (vec players) index player))))
    "game-state"
    (do
      (swap! game-state update-game received)
      (start-transition! (-> @game-state :game :state))
      (reset! food-source {})
      ;; Clear stale hover/popup state from the previous phase so the
      ;; new phase's highlights render fresh without leaked hover state.
      (cancel-from-hover-clear!)
      (reset! action-hover nil)
      (reset! from-hover nil)
      (reset! dest-hover nil)
      (reset! action-popup nil)
      (reset! intro-hover nil)
      (swap!
       introduction
       (fn [introduction]
         (-> introduction
             (assoc :progress (-> received :game :state :player-turn :introduction))
             (assoc :chosen-element nil)
             (assoc :chosen-space nil)))))
    ;; bot-choices: server sends a list of choice keys the bot picked.
    ;; Client replays them via the SAME find-state choice flow the bot used.
    "bot-choices"
    (let [choice-keys (:choices received)]
      (swap! game-state
             (fn [gs]
               (let [replayed
                     (reduce
                      (fn [game ck]
                        (let [[_phase choices] (choice/find-state game)
                              ;; Look up the choice by key, fall back to advance/pass
                              next (or (get choices ck)
                                       (get choices :advance)
                                       (get choices :pass))]
                          (or next game)))
                      (:game gs)
                      choice-keys)
                     [final-game turn choices] (choice/find-next-choices replayed)]
                 (-> gs
                     (assoc :game final-game)
                     (update :history conj (:state final-game))
                     (assoc :turn turn)
                     (assoc :choices choices)))))
      (start-transition! (-> @game-state :game :state))
      (cancel-from-hover-clear!)
      (reset! action-hover nil)
      (reset! from-hover nil)
      (reset! dest-hover nil)
      (reset! action-popup nil)
      (reset! intro-hover nil))
    "chat" (swap! chat update-chat received)))

;; -------------------------
;; Routes

(def router
  (reitit/router
    [["/" :home]
     ["/about" :about]
     ["/player/:player"]
     ["/game/:game"]]))

(defn match-route [uri]
  (->> (or (not-empty (string/replace uri #"^.*#" "")) "/")
       (reitit/match-by-path router)
       :data
       :name))
;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
      HistoryEventType/NAVIGATE
      (fn [^js/Event.token event]
        (swap! session assoc :page (match-route (.-token event)))))
    (.setEnabled true)))

(defn mount-components
  []
  (println "MOUNTING")
  (rdom/render [#'page-container] (.getElementById js/document "organism")))

(def dormant-favicon "/favicon/dormant.ico")
(def active-favicon "/favicon/active.ico")
(def neutral-favicon "/favicon/neutral.ico")

(defn init!
  []
  (let [player? (not (empty? js/playerKey))
        game? (not (empty? js/playKey))
        player-games? (and player? (not game?))
        observer? (and game? (not player?))
        player (if player? js/playerKey game/observer-key)
        window-height (.-innerHeight js/window)
        body-height (.-scrollHeight (.-body js/document))]
    (ajax/load-interceptors!)
    (hook-browser-navigation!)
    (let [protocol
          (if (= (.-protocol js/location) "https:")
            "wss:"
            "ws:")]
      (if js/playerPreferences
        (reset!
         player-preferences
         (merge
          {:color (board/random-color 0.2 0.9)}
          (reader/read-string js/playerPreferences))))
      (if js/playerGames
        (let [games (reader/read-string js/playerGames)
              favicon-path
              (if (player-active? player games)
                active-favicon
                dormant-favicon)]
          (dom/change-favicon favicon-path)
          (reset! player-games games)
          (.setInterval
           js/window
           (fn []
             (.reload js/location))
           300000))
        (dom/change-favicon neutral-favicon))
      (when js/isObserve
        (when js/observeGames
          (reset! observe-games (reader/read-string js/observeGames))))
      (when js/isStats
        (when js/playerStats
          (reset! player-stats (reader/read-string js/playerStats))))
      (when js/isCreate
        ;; Always start from a fresh empty-invocation, then overlay preloaded state.
        ;; This guards against stale defonce values from prior browser sessions.
        (let [base (board/empty-invocation
                    (if (and (exists? js/playerKey) (not (empty? js/playerKey)))
                      js/playerKey
                      "orb"))]
          (reset! board-invocation base))
        (when-let [inv (components/preloaded-invocation)]
          (reset! board-invocation inv))
        (when-let [pk (components/preloaded-play-key)]
          (reset! create-game-key pk))
        (apply-invocation! @board-invocation)
        (when-let [pk (components/preloaded-play-key)]
          (connect-create-ws! pk)))
      (when game?
        (ws/make-websocket!
         (str protocol "//" (.-host js/location) "/ws/organism/play/" js/playKey)
         update-messages!))
      (mount-components))))
