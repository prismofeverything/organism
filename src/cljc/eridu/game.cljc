(ns eridu.game)

;; =============================================================================
;; Constants
;; =============================================================================

(def resource-types [:tools :pottery :gold :gems])

(def action-types
  #{:take :sell :deploy :travel :build :influence :excel :temple})

(def action-icons
  {:take     "shopping_bag"
   :sell     "storefront"
   :deploy   "flag"
   :travel   "explore"
   :build    "construction"
   :influence "groups"
   :excel    "star"
   :temple   "temple_buddhist"})

;; --- Astrology board ---
;; 7 spaces in a clockwise circle, each with 4 actions.

(def astrology-spaces
  {1 {:actions [{:type :take :resources [:gems :tools]}
                {:type :sell}
                {:type :sell}
                {:type :travel}]}
   2 {:actions [{:type :take :resources [:gold :pottery]}
                {:type :deploy}
                {:type :deploy}
                {:type :travel}]}
   3 {:actions [{:type :take :resources [:pottery :gems]}
                {:type :travel}
                {:type :travel}
                {:type :build}]}
   4 {:actions [{:type :take :resources [:pottery :tools]}
                {:type :influence}
                {:type :influence}
                {:type :deploy}]}
   5 {:actions [{:type :take :resources [:tools :gold]}
                {:type :excel}
                {:type :travel}
                {:type :travel}]}
   6 {:actions [{:type :take :resources [:gems :gold]}
                {:type :temple}
                {:type :temple}
                {:type :influence}]}
   7 {:actions [{:type :deploy}
                {:type :sell}
                {:type :influence}
                {:type :build}]}})

;; Clockwise adjacency: 1->2->3->4->5->6->7->1
(def astrology-adjacency
  {1 [7 2], 2 [1 3], 3 [2 4], 4 [3 5], 5 [4 6], 6 [5 7], 7 [6 1]})

;; --- City board ---
;; Full graph (9 cities). 1/4-player uses all 9, 2/3-player removes Samarra + Nineveh.

(def all-cities
  #{:samarra :nineveh :kish :babylon :napur :buruq :lagash :uruk :eridu})

(def city-connections
  "Bidirectional adjacency for the full city board."
  {:samarra  #{:nineveh :kish}
   :nineveh  #{:samarra :babylon}
   :kish     #{:samarra :babylon :napur}
   :babylon  #{:kish :nineveh :uruk}
   :napur    #{:kish :buruq :lagash}
   :buruq    #{:napur}
   :lagash   #{:napur :uruk :eridu}
   :uruk     #{:babylon :lagash :eridu}
   :eridu    #{:uruk :lagash}})

(def city-demand-count
  "Number of demand spaces per city."
  {:samarra 2, :nineveh 1, :kish 1, :babylon 1,
   :napur 1, :buruq 1, :lagash 1, :uruk 1, :eridu 2})

(def neutral-magistrate-cities
  "Starting cities for the two neutral magistrates."
  [:uruk :kish])

(defn city-set
  "Return the set of cities for the given player count."
  [player-count]
  (if (#{2 3} player-count)
    (disj all-cities :samarra :nineveh)
    all-cities))

(defn city-graph
  "Return the adjacency map for the given player count."
  [player-count]
  (let [cities (city-set player-count)]
    (into {}
          (for [c cities]
            [c (set (filter cities (get city-connections c)))]))))

;; --- Demand token bag ---

(def tokens-per-type 7)

(defn full-demand-bag
  "Initial bag: 7 of each demand type."
  []
  (zipmap resource-types (repeat tokens-per-type)))

(defn bag-total [bag]
  (apply + (vals bag)))

(defn draw-demand-token
  "Draw a random demand token from the bag. Returns [updated-bag token-type] or nil if empty."
  [bag]
  (let [choices (mapcat (fn [[t n]] (repeat n t)) bag)]
    (when (seq choices)
      (let [token (rand-nth choices)]
        [(update bag token dec) token]))))

(defn fill-demand-spaces
  "Draw tokens to fill all demand spaces on the city board.
   Returns [updated-bag city-demands] where city-demands is {city -> [token ...]}."
  [bag cities]
  (reduce
   (fn [[bag demands] city]
     (let [n (get city-demand-count city 1)]
       (loop [b bag
              tokens []
              remaining n]
         (if (or (zero? remaining) (zero? (bag-total b)))
           [b (assoc demands city tokens)]
           (let [[b' token] (draw-demand-token b)]
             (recur b' (conj tokens token) (dec remaining)))))))
   [bag {}]
   cities))

;; --- Roles / tracks ---

(def roles [:merchant :warrior :priest :scholar])

(def max-role-level 5)

;; --- Player components ---

(def player-components
  "Starting inventory counts per player."
  {:caravan    1
   :astronomers 3
   :dice       4
   :bonus-tokens 4
   :raiders    6
   :temples    8})

;; --- Bonus board ---

(def bonus-spaces 5)
;; Space 0 is the continuous bonus, spaces 1-4 are one-time.

;; --- Starting cards ---
;; Each card: {:number N :city :city-key :role :role-key :resource :resource-type}
;; The role on the card starts at level 2 (all others at 1).

(def starting-cards
  [{:number 1 :city :eridu   :role :merchant :resource :gems}
   {:number 2 :city :babylon :role :warrior  :resource :gold}
   {:number 3 :city :uruk    :role :priest   :resource :pottery}
   {:number 4 :city :lagash  :role :scholar  :resource :tools}
   {:number 5 :city :kish    :role :merchant :resource :gold}
   {:number 6 :city :napur   :role :warrior  :resource :gems}
   {:number 7 :city :buruq   :role :priest   :resource :tools}
   {:number 8 :city :babylon :role :scholar  :resource :pottery}])

;; --- Feat cards ---
;; Placeholder feat descriptions; gameplay will check conditions.

(def feat-cards
  [{:id 1 :name "Builder of Empires"   :description "Build temples in 4 different cities"}
   {:id 2 :name "Master Trader"        :description "Sell goods worth 12 or more total"}
   {:id 3 :name "Star Reader"          :description "Have astronomers on 3 different astrology spaces"}
   {:id 4 :name "Conqueror"            :description "Deploy raiders to 4 different cities"}
   {:id 5 :name "Sage of the Ziggurat" :description "Reach level 4 in two different roles"}])

;; =============================================================================
;; Game state
;; =============================================================================

(defn make-player
  "Create initial player state given a starting card."
  [player-key card]
  (let [role-levels (into {}
                         (for [r roles]
                           [r (if (= r (:role card)) 2 1)]))]
    {:key          player-key
     :roles        role-levels
     :resources    {:tools 0 :pottery 0 :gold 0 :gems 0
                    (keyword (name (:resource card))) 1}
     :caravan      (:city card)
     :astronomers  []          ;; positions on astrology board, filled during setup
     :dice         [nil nil nil nil] ;; 4 dice, values set during setup
     :bonus-tokens 4
     :raiders-remaining  6
     :temples-remaining  8
     :raiders      {}          ;; {city -> count}
     :temples      {}          ;; {city -> count}
     :bonus-board  (vec (repeat bonus-spaces false))
     :amity        0
     :glory        0}))

(defn roll-dice
  "Roll n dice (values 1-7 for the 7 astrology spaces)."
  [n]
  (vec (repeatedly n #(inc (rand-int 7)))))

(defn setup-player-astronomers
  "Roll 3 dice and place astronomers on the matching astrology spaces."
  [player]
  (let [rolls (roll-dice 3)]
    (-> player
        (assoc :dice (conj rolls nil))
        (assoc :astronomers rolls))))

(defn current-player
  "Return the key of the player whose turn it is."
  [state]
  (get-in state [:turn-order (:current-player-idx state)]))

(defn current-phase
  [state]
  (get-in state [:player-turn :phase]))

(defn advance-turn
  "Move to the next player's turn."
  [state]
  (let [n (count (:turn-order state))
        next-idx (mod (inc (:current-player-idx state)) n)
        new-round? (zero? next-idx)]
    (cond-> state
      true (assoc :current-player-idx next-idx)
      new-round? (update :round inc)
      true (assoc :player-turn {:phase :choose-action}))))

(defn initial-state
  "Create the initial game state for the given player keys."
  [player-keys]
  (let [player-count (count player-keys)
        ;; Shuffle and deal starting cards
        deck (shuffle starting-cards)
        dealt (take player-count deck)
        ;; Sort by card number to determine turn order
        sorted-deals (sort-by (comp :number second) (map vector player-keys dealt))
        turn-order (mapv first sorted-deals)
        ;; Build player states
        players (into {}
                      (for [[pk card] sorted-deals]
                        [pk (setup-player-astronomers (make-player pk card))]))
        ;; City board
        cities (city-set player-count)
        graph (city-graph player-count)
        ;; Demand tokens
        [bag city-demands] (fill-demand-spaces (full-demand-bag) (vec cities))
        ;; Feat cards
        feats (vec (take 5 (shuffle feat-cards)))
        ;; Magistrates — only place in cities that exist
        magistrates (filterv cities neutral-magistrate-cities)]
    {:turn-order        turn-order
     :current-player-idx 0
     :round             1
     :player-turn       {:phase :choose-action}
     :players           players
     :astrology         astrology-spaces
     :city-graph        graph
     :city-demands      city-demands
     :demand-bag        bag
     :magistrates       (zipmap magistrates (repeat :neutral))
     :feats             feats
     :game-over         nil}))
