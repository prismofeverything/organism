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

(defn activatable-station-types
  "Set of station types where player has at least one sundiver on a station tile."
  [state player]
  (set
   (keep
    (fn [pos]
      (let [tile (get-tile state pos)]
        (when (and (some? (:station tile))
                   (pos? (get-in tile [:sundivers player] 0)))
          (get-in tile [:station :type]))))
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
  "After current actor finishes, hand off to owner actions or move to next station."
  [state actor]
  (if (= actor :activator)
    (begin-actor-actions state :owner)
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
        (if (and (not same?) (pos? bonus))
          (-> state
              (assoc-in [:player-turn :action :activator-actions] base)
              (assoc-in [:player-turn :action :owner-actions] 0)
              (assoc-in [:player-turn :choice-player] owner)
              (assoc-in [:player-turn :phase] :choose-activate-owner-bonus))
          (-> state
              (assoc-in [:player-turn :action :activator-actions] (+ base bonus))
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
   beacon-join opportunities for the captain. Transitions to :flare-beacon-join
   or :keep-card."
  [state]
  (let [n          (compute-draw-count state)
        player     (current-player state)
        captain    (:captain-flame state)
        [s cards]  (draw-n-cards state n)
        grouped    (group-by flare? cards)
        flares     (get grouped true [])
        non-flares (get grouped false [])
        ;; Advance Ark once per flare; collect cipher indices for non-captain beacons
        [s join-indices]
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
                  (if (seq join-indices) :flare-beacon-join :keep-card)))))

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
