(ns journey.game)

;; --- constants ---

(def tile-colors [:sun :silver :green :blue :purple :void])

(def num-worlds-per-color 13)

(def card-suits 5)
(def cards-per-suit 13)

(def movement-min 3)
(def movement-max 8)

;; --- hex lattice ---
;; Axial coordinates [q r]. All six neighbor directions.

(def hex-directions
  [[1 0] [1 -1] [0 -1] [-1 0] [-1 1] [0 1]])

(defn hex-neighbors
  [[q r]]
  (mapv (fn [[dq dr]] [(+ q dq) (+ r dr)]) hex-directions))

(defn hex-distance
  [[q1 r1] [q2 r2]]
  (let [dq (- q2 q1)
        dr (- r2 r1)]
    (/ (+ (Math/abs dq) (Math/abs dr) (Math/abs (+ dq dr))) 2)))

;; --- board tiles ---
;; The board is a map from [q r] to tile.
;; Tiles start unexplored; :world is nil until a world tile is drawn from the bag.
;; :contents is a vector of piece descriptors {:type kw :player player-key}.

(defn make-tile
  [color]
  {:color    color
   :world    true
   :station  nil
   :beacon   nil
   ;; map from player to how many sundivers are in the space
   :sundivers {}})

(defn add-station
  [tile station player level]
  (assoc tile :station {:type station :player player :level level}))

;; --- world bag ---
;; 13 circular world tiles of each of the 6 colors, drawn to reveal a tile.

(defn full-bag
  []
  (zipmap tile-colors (repeat num-worlds-per-color)))

(defn bag-empty?
  [bag]
  (every? zero? (vals bag)))

(defn bag-choices
  [bag]
  (apply
   concat
   (map
    (fn [[color n]]
      (repeat n color))
    bag)))


(defn draw-from-bag
  "Remove one world tile of the given color from the bag.
   Returns [updated-bag color] or nil if that color is exhausted."
  [bag]
  (let [choices (bag-choices bag)
        color (rand-nth choices)]
    [(update bag color dec) color]))

;; --- cards ---
;; Shared deck of 5 suits × 13 cards = 65 cards.
;; Suits are numbered 0–4; values 1–13.

(defn make-card [suit value]
  {:suit suit :value value})

(def base-deck
  (vec
   (for [suit  (range card-suits)
         value (range 1 (inc cards-per-suit))]
     (make-card suit value))))

;; --- cipher ---
;; A mini hex grid: center [0 0] plus the 6 neighbor offsets from hex-directions.
;; Each outer position is associated with one tile color (in tile-colors order).
;; :beacon is nil or a player-key.

(defn initial-cipher
  []
  (into
   {[0 0] {:color nil :beacon nil}}
   (map (fn [offset color] [offset {:color color :beacon nil}])
        hex-directions
        tile-colors)))

;; --- player state ---
;; Physical pieces only. Placement on the board is tracked via tile :contents;
;; these counts reflect pieces still in hand (available to be placed).

(defn initial-player
  []
  {:habitat         {:sundivers 8}
   :movement        movement-min   ;; integer 3–8
   :stations        {} ;; map of location to station type + level of station
   :gates           {} ;; map of location to all locations connected to (both directions are in the map)
   :reserve
   {:sundivers       5
    :foundries       3
    :matrixes        3
    :towers          3
    :gates           8
    :beacons         21
    :level-platforms 13}})

;; --- game state ---

(defn initial-state
  "Build the starting game state for the given turn order.
   The board starts empty; call a setup function to draw the first world
   and establish the starting tile."
  [turn-order]
  (let [bag (full-bag)
        [bag first-world] (draw-from-bag bag)
        first-tile (make-tile first-world)
        first-tile (add-station first-tile :tower "NEUTRAL" 0)]
    {:board         {[0 0] first-tile}
     :bag           bag
     :deck          (shuffle base-deck)
     :discard       []
     :ark           [0 0]
     :heading-token [0 1]
     :captain-flame (last turn-order)
     :cipher        (initial-cipher)
     :players       (into {} (map (fn [p] [p (initial-player)]) turn-order))
     :turn-order    turn-order
     :round         0
     :player-turn   {:player (first turn-order)
                     :phase  :choose-action-type}}))

;; --- accessors ---

(defn get-tile   [state pos]       (get-in state [:board pos]))
(defn get-player [state player]    (get-in state [:players player]))
(defn current-player [state]       (get-in state [:player-turn :player]))
(defn current-phase  [state]       (get-in state [:player-turn :phase]))
