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
;;
;; The 2D page shows the globe from one direction: everything within `reach`
;; of the point it looks at, true to distance from that point and fanned out
;; round it -- past the equator, but nowhere near the far side, where any flat
;; map of a sphere has to tear neighbours apart. The view turns: `view` is a
;; rotation, and dragging the board turns it (see turn).

(defn- acos [x] (#?(:clj Math/acos :cljs js/Math.acos) (max -1.0 (min 1.0 x))))
(defn- atan2 [y x] (#?(:clj Math/atan2 :cljs js/Math.atan2) y x))
(defn- cos [x] (#?(:clj Math/cos :cljs js/Math.cos) x))
(defn- sin [x] (#?(:clj Math/sin :cljs js/Math.sin) x))
(defn- dot [a b] (reduce + (map * a b)))

(def ^:private disc-radius 400)
;; room round the disc for the spaces on its rim
(def ^:private pad 30)
;; how far from the point looked at the page shows: past the equator
(def ^:private reach (* 105 (/ #?(:clj Math/PI :cljs js/Math.PI) 180)))

(def ^:private away
  "Where a space round the back of the globe goes: off the page, so nothing
   drawn for it shows, while everything that looks a space up still finds one."
  [-100000 -100000])

(def identity-view [[1 0 0] [0 1 0] [0 0 1]])

(defn- mat* [a b]
  (vec (for [row a] (vec (for [j (range 3)] (dot row (map #(nth % j) b)))))))

(defn- mat-vec [m v] (mapv #(dot % v) m))

(defn turn
  "The view turned by a drag of [dx dy] on the page: the globe follows the
   pointer, the point under it moving with it."
  [view [dx dy]]
  (let [per-unit (/ reach disc-radius)
        a (* -1 per-unit dx)
        b (* per-unit dy)
        about-z [[(cos a) (- (sin a)) 0] [(sin a) (cos a) 0] [0 0 1]]
        about-x [[1 0 0] [0 (cos b) (- (sin b))] [0 (sin b) (cos b)]]]
    (mat* about-x (mat* about-z view))))

(defn- flatten-space
  "Where a point of the globe goes on the page, seen through `view`: its angle
   from the point looked at is its distance from the disc's centre, and its
   bearing round that point is kept."
  [view p]
  (let [[x y z] (mat-vec view p)
        theta (acos y)
        c (+ pad disc-radius)]
    (if (> theta reach)
      away
      (let [r (* disc-radius (/ theta reach))
            phi (atan2 z x)]
        [(+ c (* r (cos phi))) (+ c (* r (sin phi)))]))))

(defn locations
  "Every space of this sphere on the page, seen through `view`."
  [player-count view]
  (into {} (for [[space p] (:positions (sphere-board player-count))]
             [space (flatten-space view p)])))

(defn- space-radius
  "A space's circle: a little under half the closest two neighbours come on
   the globe. The map is true to distance from its centre and only spreads
   things round it, so it never brings them closer -- and the size holds as
   the view turns."
  [player-count]
  (let [{:keys [adjacencies positions]} (sphere-board player-count)
        angle (apply min (for [[a ns] adjacencies n ns]
                           (acos (dot (get positions a) (get positions n)))))]
    (* 0.44 angle (/ disc-radius reach))))

(defn- shown? [[x _]] (not= x (first away)))

(defn- layout
  [player-count colors locations radius]
  (let [{:keys [adjacencies]} (sphere-board player-count)
        ring-color (into {} colors)
        c (+ pad disc-radius)
        edges (distinct (for [[a ns] adjacencies n ns] (sort [a n])))]
    [:svg
     {:width (* 2 c) :height (* 2 c)}
     (vec
      (concat
       [:g]
       ;; the globe itself: dragging it turns it
       [[:circle {:cx c :cy c :r (+ disc-radius (* 0.6 pad)) :fill "#111"
                  :data-sphere-globe true :style {:cursor "grab"}}]]
       ;; what is next to what, so the fanned-out rim still reads
       (for [[a b] edges
             :let [pa (get locations a) pb (get locations b)]
             :when (and (shown? pa) (shown? pb))
             :let [[x1 y1] pa [x2 y2] pb]]
         [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke "#3a4152" :stroke-width (* 0.12 radius)
                 :pointer-events "none"}])
       (for [[[ring _] [x y :as at]] (sort-by key locations)
             :when (shown? at)]
         (board/circle [x y radius (get ring-color ring "#556")]))))]))

(defn generate-board
  "The 2D board for a sphere game -- what board/generate-board is for a flat
   one -- seen through `view`, by default looking down on the (first) core."
  ([colors players] (generate-board colors players identity-view))
  ([colors players view]
   (let [player-count (count players)
         locs (locations player-count view)
         radius (space-radius player-count)
         ;; players take their colours from the end of the whole list, as on a
         ;; flat board; the rings, from its start
         player-colors (board/find-player-colors players (map last colors))
         ring-colors (vec (take (ring-count player-count) colors))]
     (board/->Board
      6
      radius
      2.1
      ring-colors
      (layout player-count ring-colors locs radius)
      locs
      player-colors))))

(defn turned-board
  "This sphere board seen through another `view`: the same board, its spaces
   moved."
  [board player-count view]
  (let [locs (locations player-count view)]
    (assoc board
           :locations locs
           :layout (layout player-count (:colors board) locs (:radius board)))))

(defn ring-colors
  "Colours for an invocation on this player count's sphere: one per ring, and
   enough after them for every player."
  [player-count max-players]
  (board/generate-colors-buffer board/total-rings (ring-count player-count) max-players))
