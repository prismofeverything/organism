(ns organism.sphere
  "The SPHERE mutation: the game on a globe.

   A sphere board is data (organism.sphere-boards), one per player count, and
   everything else is the ordinary game: the rules only ever ask what a space
   is next to, where each player starts, and where the core is. Spaces keep the
   flat boards' [ring index] names -- ring A is the core, each ring after it a
   step further away -- so ring colours, labels and the game format carry over.

   From five players a sphere has two cores, at opposite poles, so the players
   can stand evenly round the globe and still each see the same board. Both
   pay their holder, as the one core does (see game/centers).

   The 2D page shows the globe flattened: one core, one disc centred on it with
   the rings running out to the far side at the rim; two cores, a disc round
   each."
  (:require
   [organism.board :as board]
   [organism.game :as game]
   [organism.sphere-boards :as boards]))

(defn sphere?
  [mutations]
  (boolean (get mutations :SPHERE)))

(def player-counts
  "The player counts there is a sphere for."
  (vec (sort (keys boards/boards))))

(defn sphere-board
  [player-count]
  (get boards/boards player-count))

(defn ring-count
  "How many rings this player count's sphere has: its colours follow them."
  [player-count]
  (:ring-count (sphere-board player-count)))

(defn rings
  [player-count]
  (vec (take (ring-count player-count) board/total-rings)))

(defn starting-spaces
  "Each player and their three home spaces, in turn order round the globe --
   the shape board/starting-spaces gives."
  [player-count players]
  (mapv vector players (:homes (sphere-board player-count))))

(defn create-game
  "The opening position on this player count's sphere."
  [player-info organism-victory mutations]
  (let [{:keys [adjacencies centers]} (sphere-board (count player-info))]
    (-> (game/initial-game
         (rings (count player-info))
         adjacencies
         (first centers)
         player-info
         organism-victory
         mutations)
        (assoc :centers centers))))

;; ── The flattened globe ───────────────────────────────────────────────────

(defn- acos [x] (#?(:clj Math/acos :cljs js/Math.acos) (max -1.0 (min 1.0 x))))
(defn- atan2 [y x] (#?(:clj Math/atan2 :cljs js/Math.atan2) y x))
(defn- cos [x] (#?(:clj Math/cos :cljs js/Math.cos) x))
(defn- sin [x] (#?(:clj Math/sin :cljs js/Math.sin) x))
(defn- sqrt [x] (#?(:clj Math/sqrt :cljs js/Math.sqrt) x))
(defn- dot [a b] (reduce + (map * a b)))

(def ^:private disc-radius 400)
(def ^:private disc-gap 30)
;; room round the discs for the spaces on their rims
(def ^:private pad 30)

(defn- nearest-center
  [positions centers p]
  (apply max-key #(dot p (get positions %)) (reverse centers)))

(defn- flatten-space
  "Where a point of the globe goes on its core's disc: the angle from the core
   is the distance from the disc's centre, `reach` of it at the rim, and the
   bearing is kept -- mirrored on the second disc, which looks at the globe
   from the other side."
  [p [cx cy] radius reach mirror?]
  (let [[x y z] p
        theta (acos (if mirror? (- y) y))
        r (* radius (/ theta reach))
        phi (atan2 z x)
        phi (if mirror? (- phi) phi)]
    [(+ cx (* r (cos phi))) (+ cy (* r (sin phi)))]))

(defn locations
  "Every space of this sphere on the page, and the discs they sit on."
  [player-count]
  (let [{:keys [positions centers]} (sphere-board player-count)
        two? (= 2 (count centers))
        ;; two discs share the room one would have, so the page lays the
        ;; board out as it does a flat one
        radius (if two? (/ (- disc-radius (/ disc-gap 2)) 2) disc-radius)
        y (+ pad radius)
        discs (if two?
                [[y (+ pad disc-radius)] [(+ y (* 2 radius) disc-gap) (+ pad disc-radius)]]
                [[y y]])
        ;; each disc runs from its core to the furthest space it holds
        owner (into {} (for [[space p] positions]
                         [space (if two? (let [c (nearest-center positions centers p)]
                                           (first (keep-indexed #(when (= c %2) %1) centers)))
                                     0)]))
        reach (fn [i]
                (let [angles (for [[space p] positions
                                   :when (= i (owner space))]
                               (acos (if (zero? i) (second p) (- (second p)))))]
                  (* 1.04 (apply max angles))))
        reaches (mapv reach (range (count discs)))]
    {:discs (mapv (fn [c] {:center c :radius radius}) discs)
     :width (+ (* 2 pad) (* 2 disc-radius))
     :height (+ (* 2 pad) (* 2 disc-radius))
     :locations (into {} (for [[space p] positions
                               :let [i (owner space)]]
                           [space (flatten-space p (nth discs i) radius (nth reaches i) (= i 1))]))}))

(defn- space-radius
  "A space's circle on the flattened globe: a little under half the closest
   any two spaces come on the page, so none overlap. The grid is not quite
   even, and the map only ever spreads it, so that is near the cores."
  [player-count locs]
  (let [{:keys [adjacencies]} (sphere-board player-count)
        gaps (for [[a ns] adjacencies n ns
                   :let [[x1 y1] (get locs a) [x2 y2] (get locs n)]]
               (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))]
    (* 0.44 (apply min gaps))))

(defn- layout
  [player-count colors {:keys [discs width height locations]} radius]
  (let [{:keys [adjacencies]} (sphere-board player-count)
        ring-color (into {} colors)
        edges (distinct (for [[a ns] adjacencies n ns] (sort [a n])))]
    [:svg
     {:width width :height height}
     (vec
      (concat
       [:g]
       (for [{[x y] :center r :radius} discs]
         [:circle {:cx x :cy y :r (+ r (* 0.6 pad)) :fill "#111"}])
       ;; what is next to what: the map stretches toward the rim, so the
       ;; links say it where position no longer does
       (for [[a b] edges
             :let [[x1 y1] (get locations a) [x2 y2] (get locations b)
                   d (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))]
             ;; a link across the rim or between discs would cut the board
             :when (< d (* 6 radius))]
         [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke "#3a4152" :stroke-width (* 0.12 radius)}])
       (for [[[ring _] [x y]] (sort-by key locations)]
         (board/circle [x y radius (get ring-color ring "#556")]))))]))

(defn generate-board
  "The 2D board for a sphere game -- what board/generate-board is for a flat one."
  [colors players]
  (let [player-count (count players)
        flat (locations player-count)
        radius (space-radius player-count (:locations flat))
        ;; players take their colours from the end of the whole list, as on a
        ;; flat board; the rings, from its start
        player-colors (board/find-player-colors players (map last colors))
        ring-colors (vec (take (ring-count player-count) colors))]
    (board/->Board
     6
     radius
     2.1
     ring-colors
     (layout player-count ring-colors flat radius)
     (:locations flat)
     player-colors)))

(defn ring-colors
  "Colours for an invocation on this player count's sphere: one per ring, and
   enough after them for every player."
  [player-count max-players]
  (board/generate-colors-buffer board/total-rings (ring-count player-count) max-players))
