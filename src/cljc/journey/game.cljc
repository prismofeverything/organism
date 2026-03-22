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

(declare add-hex hex-neighbors matrix-beacon-positions
         total-spendable-sundivers stations-of-type-with-sundiver station-action-counts)

;; --- cipher ---
;; A mini hex grid: center [0 0] plus the 6 neighbor offsets from hex-directions.
;; Each outer position is associated with one or more world token colors.
;; :beacons is a vector of {:player player-key :color color}.

(defn initial-cipher
  []
  (into
   {[0 0] {:colors {}}}
   (map (fn [offset color] [offset {:colors {color {}}}])
        hex-directions
        tile-colors)))

(defn cipher-placement-cost
  "Sundivers required to place color at cipher position pos.
   0 if color is already present; otherwise 1 per existing color."
  [state pos color]
  (let [existing (get-in state [:cipher pos :colors] {})]
    (if (contains? existing color)
      0
      (count existing))))

(defn cipher-place-beacon
  "Place player's beacon of color at cipher pos.
   Adds color to the position if new (draws from bag if from-bag? is true).
   Increments the player's beacon count for that color at that position."
  [state player pos color from-bag?]
  (let [new-color? (not (contains? (get-in state [:cipher pos :colors] {}) color))]
    (cond-> state
      new-color?              (assoc-in [:cipher pos :colors color] {})
      (and new-color? from-bag?) (update-in [:bag color] dec)
      true                    (update-in [:cipher pos :colors color player] (fnil inc 0)))))

;; --- landing check ---

(defn count-cipher-matches
  "Count how many of the 6 tile neighbors of pos match the corresponding outer cipher position."
  [state pos]
  (count
   (filter
    (fn [dir]
      (let [neighbor       (add-hex pos dir)
            neighbor-color (get-in state [:board neighbor :color])
            cipher-colors  (get-in state [:cipher dir :colors] {})]
        (and neighbor-color (contains? cipher-colors neighbor-color))))
    hex-directions)))

(defn can-land-at?
  "True if the Ark can choose to land on tile at pos.
   Requires the tile color in the center cipher, and ≥5 directional matches.
   5 matches: Ark must be exactly on pos. 6 matches: Ark may be on pos or adjacent."
  [state pos]
  (let [center-colors (get-in state [:cipher [0 0] :colors] {})
        tile-color    (get-in state [:board pos :color])
        ark           (:ark state)
        matches       (count-cipher-matches state pos)]
    (and (contains? center-colors tile-color)
         (or (and (= matches 5) (= ark pos))
             (and (= matches 6)
                  (or (= ark pos)
                      (some #{ark} (hex-neighbors pos))))))))

(defn available-landings
  "All tile positions where the Ark can currently choose to land."
  [state]
  (filterv #(can-land-at? state %) (keys (:board state))))

;; --- scoring and game over ---

(defn- add-beacons-to-scores
  "Accumulate beacon counts from a {player count} map into scores."
  [scores beacons]
  (reduce-kv (fn [acc player n] (update acc player (fnil + 0) n)) scores beacons))

(defn compute-scores
  "Score beacons on the cipher for a landing at pos.
   Center position [0 0]: score beacons for the landed tile's color.
   Each outer position (= a direction): score beacons for the board tile color
   in that direction from pos, only if that color is registered at that cipher position."
  [state pos]
  (let [landed-color (get-in state [:board pos :color])
        scores (add-beacons-to-scores
                {} (get-in state [:cipher [0 0] :colors landed-color] {}))]
    (reduce
     (fn [scores dir]
       (let [neighbor-color (get-in state [:board (add-hex pos dir) :color])]
         (if (and neighbor-color
                  (contains? (get-in state [:cipher dir :colors] {}) neighbor-color))
           (add-beacons-to-scores
            scores (get-in state [:cipher dir :colors neighbor-color] {}))
           scores)))
     scores
     hex-directions)))

(defn land-ark
  "End the game: compute scores from landing on tile at pos."
  [state pos]
  (let [scores    (compute-scores state pos)
        max-score (if (seq scores) (apply max (vals scores)) 0)
        winners   (filterv #(= (get scores % 0) max-score) (:turn-order state))]
    (-> state
        (assoc :game-over {:type :landing :tile pos :scores scores :winners winners})
        (assoc-in [:player-turn :phase] :game-over))))

(defn trigger-loss
  "End the game: everyone loses. The captain especially loses."
  [state]
  (-> state
      (assoc :game-over {:type :loss :captain (:captain-flame state)})
      (assoc-in [:player-turn :phase] :game-over)))

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
    :level-platforms 13}
   :held-card nil})

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
     :neutral-tower [0 0]
     :heading-token [0 1]
     :captain-flame (last turn-order)
     :cipher        (initial-cipher)
     :pending-cipher []
     :flares-drawn  0
     :players       (into {} (map (fn [p] [p (initial-player)]) turn-order))
     :turn-order    turn-order
     :round         0
     :player-turn   {:player (first turn-order)
                     :phase  :choose-action-type}}))

;; --- accessors ---

(defn get-tile      [state pos]    (get-in state [:board pos]))
(defn get-player    [state player] (get-in state [:players player]))
(defn current-player [state]       (get-in state [:player-turn :player]))
(defn current-phase  [state]       (get-in state [:player-turn :phase]))

;; --- hex direction utilities ---

(defn add-hex
  [[q1 r1] [q2 r2]]
  [(+ q1 q2) (+ r1 r2)])

(defn direction-index
  "Index of dir in hex-directions, or nil if not found."
  [dir]
  (first (keep-indexed (fn [i d] (when (= d dir) i)) hex-directions)))

(defn rotate-cw
  "Rotate a hex direction 60° clockwise."
  [dir]
  (nth hex-directions (mod (inc (direction-index dir)) 6)))

(defn rotate-ccw
  "Rotate a hex direction 60° counter-clockwise."
  [dir]
  (nth hex-directions (mod (dec (direction-index dir)) 6)))

(defn heading-direction
  "Unit direction vector from the Ark toward the Heading token."
  [state]
  (let [[aq ar] (:ark state)
        [hq hr] (:heading-token state)]
    [(- hq aq) (- hr ar)]))

(defn launch-positions
  "The four positions a sundiver can be launched to: the Ark's position,
   the space directly in front (heading direction), and the two flanking spaces."
  [state]
  (let [ark (:ark state)
        dir (heading-direction state)]
    [ark
     (add-hex ark dir)
     (add-hex ark (rotate-ccw dir))
     (add-hex ark (rotate-cw dir))]))

;; --- move action ---

(defn immobile?
  "True if a sundiver at pos is marked immobile for the current turn
   (e.g. it just explored a new tile)."
  [state pos]
  (contains? (get-in state [:player-turn :immobile] #{}) pos))

(defn mark-immobile
  [state pos]
  (update-in state [:player-turn :immobile] (fnil conj #{}) pos))

(defn explore
  "Draw a world from the bag, place a new tile of that color at pos,
   add one sundiver there for player, and mark that sundiver immobile."
  [state player pos]
  (let [[bag color] (draw-from-bag (:bag state))
        tile (assoc-in (make-tile color) [:sundivers player] 1)]
    (-> state
        (assoc :bag bag)
        (assoc-in [:board pos] tile)
        (mark-immobile pos))))

(defn launch-sundiver
  "Transfer one sundiver from player's habitat to pos.
   If pos has a tile, add to its sundivers map. If empty, explore."
  [state player pos]
  (let [state (update-in state [:players player :habitat :sundivers] dec)]
    (if (get-tile state pos)
      (update-in state [:board pos :sundivers player] (fnil inc 0))
      (explore state player pos))))

(defn gate-owner
  "Returns the player who owns a gate connecting from-pos and to-pos, or nil."
  [state from-pos to-pos]
  (first
   (filter
    (fn [player]
      (contains? (get-in state [:players player :gates from-pos] #{}) to-pos))
    (:turn-order state))))

(defn add-gate
  "Sundiver at from-pos returns to reserve; a gate is placed connecting
   from-pos and to-pos (bidirectional) using one of player's gate pieces."
  [state player from-pos to-pos]
  (-> state
      (update-in [:board from-pos :sundivers player] dec)
      (update-in [:players player :reserve :sundivers] inc)
      (update-in [:players player :reserve :gates] dec)
      (update-in [:players player :gates from-pos] (fnil conj #{}) to-pos)
      (update-in [:players player :gates to-pos] (fnil conj #{}) from-pos)
      (update-in [:player-turn :action :gates-created] (fnil inc 0))))

(defn fly-sundiver
  "Move one sundiver for player from from-pos to to-pos.
   Empty space → explore. Same color → move. Different color → gate."
  [state player from-pos to-pos]
  (let [from-tile (get-tile state from-pos)
        to-tile   (get-tile state to-pos)]
    (cond
      (nil? to-tile)
      (-> state
          (update-in [:board from-pos :sundivers player] dec)
          (explore player to-pos))

      (= (:color from-tile) (:color to-tile))
      (-> state
          (update-in [:board from-pos :sundivers player] dec)
          (update-in [:board to-pos :sundivers player] (fnil inc 0)))

      :else
      (add-gate state player from-pos to-pos))))

(defn fly-through-gate
  "Move sundiver for player from from-pos to to-pos via an existing gate.
   If player is not the gate owner, the owner gains one sundiver (reserve → habitat)."
  [state player from-pos to-pos]
  (let [owner (gate-owner state from-pos to-pos)
        state (-> state
                  (update-in [:board from-pos :sundivers player] dec)
                  (update-in [:board to-pos :sundivers player] (fnil inc 0)))]
    (if (and owner (not= player owner))
      (-> state
          (update-in [:players owner :reserve :sundivers] dec)
          (update-in [:players owner :habitat :sundivers] inc))
      state)))

;; --- convert action ---

(defn subtract-hex
  [[q1 r1] [q2 r2]]
  [(- q2 q1) (- r2 r1)])

(defn adjacent-direction
  "Direction index from 'from' toward 'to' (must be adjacent). Nil if not a valid direction."
  [from to]
  (direction-index (subtract-hex from to)))

(defn direction-diff
  "Minimum angular distance between two direction indices, in 60° steps (0–3)."
  [d1 d2]
  (let [diff (mod (- d2 d1) 6)]
    (min diff (- 6 diff))))

;; --- region logic ---

(defn region-tiles
  "BFS: all tiles in the same contiguous same-color region as pos."
  [state pos]
  (let [color (get-in state [:board pos :color])
        board (:board state)]
    (loop [visited #{pos} queue [pos]]
      (if (empty? queue)
        visited
        (let [curr   (first queue)
              rest-q (rest queue)
              new-ns (filter
                       (fn [n]
                         (and (not (visited n))
                              (= color (get-in board [n :color]))))
                       (hex-neighbors curr))]
          (recur (into visited new-ns)
                 (into rest-q new-ns)))))))

(defn region-station-level
  "Level of the first station found in region, or nil if none."
  [state region]
  (first (keep #(get-in state [:board % :station :level]) region)))

(defn gates-leaving-region
  "All tile positions reachable by gate from any tile in region that are outside region."
  [state region]
  (set
   (mapcat
    (fn [player]
      (mapcat
       (fn [pos]
         (remove region (get-in state [:players player :gates pos] #{})))
       region))
    (:turn-order state))))

(defn region-level
  "Level for a new station placed in the region containing target.
   Uses existing station level if region already has one; otherwise
   depth from gate-connected regions (level = max neighbor level + 1, or 1 if none)."
  [state target]
  (let [region   (region-tiles state target)
        existing (region-station-level state region)]
    (if (some? existing)
      existing
      (let [exits           (gates-leaving-region state region)
            neighbor-levels (keep
                              (fn [exit]
                                (region-station-level state (region-tiles state exit)))
                              exits)]
        (if (seq neighbor-levels)
          (inc (apply max neighbor-levels))
          1)))))

;; --- conversion pattern detection ---

(defn player-adjacent-sundivers
  "Sundiver positions owned by player that are adjacent to target."
  [state player target]
  (filter
   (fn [pos]
     (and (pos? (get-in state [:board pos :sundivers player] 0))
          (some? (adjacent-direction target pos))))
   (hex-neighbors target)))

(defn find-conversions
  "Find all valid conversion patterns for player. Returns a seq of
   {:type :foundry/:matrix/:tower, :target pos, :sundivers [pos...]}."
  [state player]
  (let [board (:board state)]
    (mapcat
     (fn [target]
       (when (and (contains? board target)
                  (nil? (get-in board [target :station])))
         (let [adj     (vec (player-adjacent-sundivers state player target))
               indexed (mapv (fn [s] [s (adjacent-direction target s)]) adj)]
           (concat
            ;; Foundry: two sundivers at 120° from each other (direction-diff == 2)
            (for [[s1 d1] indexed [s2 d2] indexed
                  :when (and (not= s1 s2)
                             (= 2 (direction-diff d1 d2))
                             (< d1 d2))]
              {:type :foundry :target target :sundivers [s1 s2]})
            ;; Matrix: two sundivers directly across (direction-diff == 3)
            (for [[s1 d1] indexed [s2 d2] indexed
                  :when (and (not= s1 s2)
                             (= 3 (direction-diff d1 d2))
                             (< d1 d2))]
              {:type :matrix :target target :sundivers [s1 s2]})
            ;; Tower: three sundivers equally spaced (directions i, i+2, i+4)
            (for [[s1 d1] indexed [s2 d2] indexed [s3 d3] indexed
                  :when (and (not= s1 s2) (not= s1 s3) (not= s2 s3)
                             (let [ds (sort [d1 d2 d3])]
                               (or (= ds [0 2 4]) (= ds [1 3 5])))
                             (< d1 d2 d3))]
              {:type :tower :target target :sundivers [s1 s2 s3]})))))
     (keys board))))

;; Reserve uses plural keys; station type keywords are singular.
(def station-reserve-key
  {:foundry :foundries
   :matrix  :matrixes
   :tower   :towers})

(defn convert
  "Return sundivers to reserve, place station at target, record level, advance phase."
  [state player station-type target sundiver-positions]
  (let [level (region-level state target)
        state (reduce
               (fn [s pos]
                 (-> s
                     (update-in [:board pos :sundivers player] dec)
                     (update-in [:players player :reserve :sundivers] inc)))
               state
               sundiver-positions)]
    (-> state
        (assoc-in [:board target :station] {:type station-type :player player :level level})
        (assoc-in [:players player :stations target] {:type station-type :level level})
        (update-in [:players player :reserve (station-reserve-key station-type)] dec)
        (assoc-in [:player-turn :action :cards-to-draw] level)
        (assoc-in [:player-turn :phase] :draw-cards))))

;; --- activate action ---

(def activation-actions-table
  {0 {:base 1 :bonus 0}
   1 {:base 1 :bonus 1}
   2 {:base 2 :bonus 1}
   3 {:base 3 :bonus 2}})

(defn station-action-counts
  "Returns {:base n :bonus n} for the given station level (clamped 0–3)."
  [level]
  (get activation-actions-table (min (max level 0) 3)))

(defn- total-activation-actions
  "Minimum forced actions for the activating player: base only for every station.
   Bonus is always optional (own or other), so it does not affect feasibility."
  [state player station-type]
  (apply +
         (map (fn [pos]
                (let [level (get-in state [:board pos :station :level] 0)]
                  (:base (station-action-counts level))))
              (stations-of-type-with-sundiver state player station-type))))

(defn can-fully-activate-matrix?
  "True if player has enough beacon positions, spendable sundivers, and beacon reserves
   to complete all base (+ bonus for own) matrix actions."
  [state player]
  (let [n         (total-activation-actions state player :matrix)
        positions (count (matrix-beacon-positions state player))
        spendable (total-spendable-sundivers state player)
        beacons   (get-in state [:players player :reserve :beacons] 0)]
    (or (zero? n)
        (and (>= positions n) (>= spendable n) (>= beacons n)))))

(defn can-fully-activate-tower?
  "True if player has enough spendable sundivers to pay 1 per tower action."
  [state player]
  (let [n         (total-activation-actions state player :tower)
        spendable (total-spendable-sundivers state player)]
    (or (zero? n) (>= spendable n))))

(defn can-fully-activate-foundry?
  "True if player has at least 1 reserve sundiver per foundry action."
  [state player]
  (let [n       (total-activation-actions state player :foundry)
        reserve (get-in state [:players player :reserve :sundivers] 0)]
    (or (zero? n) (>= reserve n))))

(defn activatable-station-types
  "Set of station types where player has a sundiver on a station tile and
   the full activation can be completed (no partial activations)."
  [state player]
  (set
   (keep
    (fn [pos]
      (let [tile  (get-tile state pos)
            stype (get-in tile [:station :type])]
        (when (and (some? (:station tile))
                   (pos? (get-in tile [:sundivers player] 0))
                   (case stype
                     :matrix  (can-fully-activate-matrix? state player)
                     :tower   (can-fully-activate-tower? state player)
                     :foundry (can-fully-activate-foundry? state player)
                     true))
          stype)))
    (keys (:board state)))))

(defn stations-of-type-with-sundiver
  "All station positions of the given type where player has at least one sundiver."
  [state player station-type]
  (filterv
   (fn [pos]
     (let [tile (get-tile state pos)]
       (and (= station-type (get-in tile [:station :type]))
            (pos? (get-in tile [:sundivers player] 0)))))
   (keys (:board state))))

(defn foundry-action
  "Move up to 2 sundivers from player's reserve to habitat."
  [state player]
  (let [available (get-in state [:players player :reserve :sundivers] 0)
        amount    (min 2 available)]
    (-> state
        (update-in [:players player :reserve :sundivers] - amount)
        (update-in [:players player :habitat :sundivers] + amount))))

(defn spend-sundiver
  "Move one sundiver for player from pos (nil = habitat) to reserve."
  [state player pos]
  (if pos
    (-> state
        (update-in [:board pos :sundivers player] dec)
        (update-in [:players player :reserve :sundivers] inc))
    (-> state
        (update-in [:players player :habitat :sundivers] dec)
        (update-in [:players player :reserve :sundivers] inc))))

(defn place-beacon
  [state player pos]
  (-> state
      (assoc-in [:board pos :beacon] player)
      (update-in [:players player :reserve :beacons] dec)))

(defn matrix-beacon-positions
  "Valid positions to place a matrix beacon: the Ark, the space behind the Ark,
   the two flanking spaces, and any board space where player has a sundiver.
   Position must have a world tile with no existing beacon."
  [state player]
  (let [ark     (:ark state)
        dir     (heading-direction state)
        dir-idx (direction-index dir)
        behind  (add-hex ark (nth hex-directions (mod (+ dir-idx 3) 6)))
        specials (into #{ark behind (add-hex ark (rotate-ccw dir)) (add-hex ark (rotate-cw dir))}
                       (filter #(pos? (get-in state [:board % :sundivers player] 0))
                               (keys (:board state))))]
    (filterv
     (fn [pos]
       (let [tile (get-tile state pos)]
         (and tile (:world tile) (nil? (:beacon tile)))))
     specials)))

(defn sundiver-spend-positions
  "Positions where player has board sundivers, plus nil for habitat if non-empty."
  [state player]
  (let [board-positions (filterv
                         #(pos? (get-in state [:board % :sundivers player] 0))
                         (keys (:board state)))
        habitat-count   (get-in state [:players player :habitat :sundivers] 0)]
    (cond-> (set board-positions)
      (pos? habitat-count) (conj nil))))

(defn total-spendable-sundivers
  "Total sundivers available to spend: board + habitat."
  [state player]
  (let [board (apply + (map #(get-in state [:board % :sundivers player] 0) (keys (:board state))))
        hab   (get-in state [:players player :habitat :sundivers] 0)]
    (+ board hab)))

(defn become-captain [state player]
  (assoc state :captain-flame player))

(defn turn-heading
  "Rotate heading direction :left (CCW) or :right (CW); :none leaves it unchanged."
  [state turn-dir]
  (if (= turn-dir :none)
    state
    (let [ark     (:ark state)
          dir     (heading-direction state)
          new-dir (case turn-dir
                    :left  (rotate-ccw dir)
                    :right (rotate-cw dir))]
      (assoc state :heading-token (add-hex ark new-dir)))))

(defn advance-ark
  "Moves the Ark to the heading-token position; heading-token advances one more step."
  [state]
  (let [dir        (heading-direction state)
        new-ark    (:heading-token state)
        new-heading (add-hex new-ark dir)]
    (-> state
        (assoc :ark new-ark)
        (assoc :heading-token new-heading))))

(defn discover-beacon
  "Remove tile with beacon from board; enqueue in :pending-cipher for end-of-turn resolution."
  [state pos actor-player]
  (let [tile (get-tile state pos)]
    (-> state
        (update :board dissoc pos)
        (update :pending-cipher (fnil conj [])
                {:world        (:color tile)
                 :beacon-owner (:beacon tile)
                 :discoverer   actor-player
                 :joiners      []}))))

(defn join-beacon-to-cipher
  "Add player's beacon to a pending-cipher entry.
   If cipher-idx is omitted, updates the most recent entry."
  ([state player]
   (join-beacon-to-cipher state player (dec (count (:pending-cipher state)))))
  ([state player cipher-idx]
   (-> state
       (update-in [:pending-cipher cipher-idx :joiners] conj player)
       (update-in [:players player :reserve :beacons] dec))))

;; --- activate state machine ---

(declare advance-after-actions begin-next-station)

(defn actor-player
  "The player executing actions: activator = turn player, owner = station owner."
  [state actor]
  (if (= actor :activator)
    (current-player state)
    (get-in state [:player-turn :action :current-owner])))

(defn begin-actor-actions
  "Start the given actor's action sequence. For foundry, executes all actions
   immediately. For matrix/tower, sets the appropriate choice phase."
  [state actor]
  (let [actions-key  (if (= actor :activator) :activator-actions :owner-actions)
        n-actions    (get-in state [:player-turn :action actions-key] 0)
        station-type (get-in state [:player-turn :action :station-type])
        aplayer      (actor-player state actor)]
    (cond
      (zero? n-actions)
      (advance-after-actions state actor)

      (= station-type :foundry)
      (let [s (reduce (fn [acc _] (foundry-action acc aplayer)) state (range n-actions))]
        (-> s
            (assoc-in [:player-turn :action actions-key] 0)
            (advance-after-actions actor)))

      :else
      (-> state
          (assoc-in [:player-turn :action :current-actor] actor)
          (assoc-in [:player-turn :phase]
                    (case station-type
                      :matrix :choose-activate-matrix-beacon
                      :tower  :choose-activate-tower-heading))))))

(defn advance-after-actions
  "After current actor finishes:
   - Activator done → offer owner their bonus (if other's station with bonus), else next station.
   - Owner done → next station."
  [state actor]
  (if (= actor :activator)
    (let [bonus-total (get-in state [:player-turn :action :bonus-total] 0)
          owner       (get-in state [:player-turn :action :current-owner])
          player      (current-player state)]
      (if (and (pos? bonus-total) (not= player owner))
        (-> state
            (assoc-in [:player-turn :choice-player] owner)
            (assoc-in [:player-turn :phase] :choose-activate-owner-bonus))
        (begin-next-station state)))
    (begin-next-station state)))

(defn begin-next-station
  "Pop the next station from the queue and set it up, or finish the activate action."
  [state]
  (let [player       (current-player state)
        queue        (get-in state [:player-turn :action :stations-queue] [])
        station-type (get-in state [:player-turn :action :station-type])]
    (if (empty? queue)
      (assoc-in state [:player-turn :phase] :draw-cards)
      (let [pos      (first queue)
            rest-q   (vec (rest queue))
            tile     (get-tile state pos)
            owner    (get-in tile [:station :player])
            level    (get-in tile [:station :level])
            {:keys [base bonus]} (station-action-counts level)
            same?    (= player owner)
            state    (-> state
                         (cond-> (= station-type :tower) (become-captain player))
                         (assoc-in [:player-turn :action :stations-queue] rest-q)
                         (assoc-in [:player-turn :action :current-station] pos)
                         (assoc-in [:player-turn :action :current-owner] owner)
                         (assoc-in [:player-turn :action :bonus-total] bonus)
                         (assoc-in [:player-turn :action :beacons-joined] 0)
                         (update-in [:player-turn :action :cards-to-draw] (fnil + 0) level))]
        (if (and same? (pos? bonus))
          ;; Own station with bonus: ask activator how many bonus actions to take
          (-> state
              (assoc-in [:player-turn :action :activator-actions] base)
              (assoc-in [:player-turn :action :owner-actions] 0)
              (assoc-in [:player-turn :phase] :choose-activate-self-bonus))
          ;; All other cases: activator does base actions first;
          ;; owner bonus (if any) is offered after activator finishes.
          (-> state
              (assoc-in [:player-turn :action :activator-actions] base)
              (assoc-in [:player-turn :action :owner-actions] 0)
              (begin-actor-actions :activator)))))))

(defn start-activate
  "Initialize the activate action for the chosen station type."
  [state station-type]
  (let [player   (current-player state)
        stations (stations-of-type-with-sundiver state player station-type)]
    (-> state
        (assoc-in [:player-turn :action :station-type] station-type)
        (assoc-in [:player-turn :action :stations-queue] (vec stations))
        begin-next-station)))

;; --- card drawing and post-action phases ---

(def flare-suit 4)

(defn flare? [card] (= (:suit card) flare-suit))

(defn draw-n-cards
  "Draw n cards from the deck, reshuffling discard if needed.
   Returns [updated-state [cards...]]."
  [state n]
  (loop [s state drawn []]
    (if (= (count drawn) n)
      [s drawn]
      (let [s    (if (empty? (:deck s))
                   (-> s (assoc :deck (shuffle (:discard s))) (assoc :discard []))
                   s)
            card (first (:deck s))]
        (recur (-> s (update :deck rest) (update :discard conj card))
               (conj drawn card))))))

(defn compute-draw-count
  "Number of cards to draw: gates-created (move) or cards-to-draw (convert/activate)."
  [state]
  (let [action-type (get-in state [:player-turn :action-type])
        action      (get-in state [:player-turn :action])]
    (case action-type
      :move    (get action :gates-created 0)
      :convert (get action :cards-to-draw 0)
      :activate (get action :cards-to-draw 0)
      0)))

(defn process-draw-and-flares
  "Draw cards for end of action. Advances Ark once per flare drawn, queueing
   beacon-join opportunities for the captain. Triggers loss on the 13th flare.
   Transitions to :flare-beacon-join or :keep-card."
  [state]
  (let [n          (compute-draw-count state)
        player     (current-player state)
        captain    (:captain-flame state)
        [s cards]  (draw-n-cards state n)
        grouped    (group-by flare? cards)
        flares     (get grouped true [])
        non-flares (get grouped false [])
        s          (update s :flares-drawn (fnil + 0) (count flares))]
    (if (>= (:flares-drawn s) 13)
      (trigger-loss s)
      ;; Advance Ark once per flare; collect cipher indices for non-captain beacons
      (let [[s join-indices]
            (reduce
             (fn [[acc-s indices] _]
               (let [new-s   (advance-ark acc-s)
                     new-ark (:ark new-s)
                     tile    (get-tile new-s new-ark)]
                 (if (and tile (:beacon tile))
                   (let [beacon-owner (:beacon tile)
                         cipher-idx   (count (:pending-cipher new-s))
                         new-s        (discover-beacon new-s new-ark player)]
                     (if (= beacon-owner captain)
                       [new-s indices]
                       [new-s (conj indices cipher-idx)]))
                   [new-s indices])))
             [s []]
             flares)]
        (-> s
            (assoc-in [:player-turn :action :drawn-cards] (vec non-flares))
            (assoc-in [:player-turn :action :flare-join-indices] join-indices)
            (assoc-in [:player-turn :captain-beacons-joined] 0)
            (cond-> (seq join-indices)
              (assoc-in [:player-turn :choice-player] captain))
            (assoc-in [:player-turn :phase]
                      (if (seq join-indices) :flare-beacon-join :keep-card)))))))

;; --- turn transitions ---

(defn next-player-index
  [state player]
  (first (keep-indexed #(when (= %2 player) %1) (:turn-order state))))

(defn begin-next-player-turn
  "Advance to the next player; increment round when wrapping around."
  [state]
  (let [order    (vec (:turn-order state))
        current  (current-player state)
        idx      (next-player-index state current)
        next-idx (mod (inc idx) (count order))
        next-p   (order next-idx)
        round    (if (zero? next-idx) (inc (:round state)) (:round state))]
    (-> state
        (assoc :round round)
        (update :pending-cipher (constantly []))
        (assoc :player-turn {:player                  next-p
                             :phase                   :choose-action-type
                             :captain-beacons-joined  0}))))

;; --- movement points ---

(defn station-colors
  "Set of distinct tile colors where player has stations, plus the neutral tower's tile color.
   The neutral tower counts as a station for all players."
  [state player]
  (let [player-station-positions (keys (get-in state [:players player :stations]))
        neutral-pos              (:neutral-tower state)
        all-positions            (cond-> player-station-positions
                                   neutral-pos (conj neutral-pos))]
    (set (keep #(get-in state [:board % :color]) all-positions))))

(defn move-points
  "Base 2 move points, plus 1 for each distinct tile color the player has a station on."
  [state player]
  (+ 2 (count (station-colors state player))))

;; --- turn transitions ---

(def action-types [:move :convert :activate])

(defn choose-action-type
  "Record the chosen action type and advance to the first sub-phase for that action.
   For :move, initialises :moves-remaining based on the player's current move points."
  [state action-type]
  (let [player      (current-player state)
        action-data (if (= action-type :move)
                      {:moves-remaining (move-points state player)}
                      {})]
    (-> state
        (assoc-in [:player-turn :action-type] action-type)
        (assoc-in [:player-turn :action] action-data)
        (assoc-in [:player-turn :phase] (keyword (str "choose-" (name action-type)))))))
