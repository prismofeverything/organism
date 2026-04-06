(ns eridu.game)

;; =============================================================================
;; Constants
;; =============================================================================

(def resource-types [:tools :pottery :gold :gems])

(def action-types
  #{:take :sell :deploy :travel :influence :temple})

(def action-icons
  {:take      "shopping_bag"
   :sell      "storefront"
   :deploy    "flag"
   :travel    "explore"
   :influence "groups"
   :temple    "temple_buddhist"})

;; --- Action board (astrology wheel) ---
;; 7 spaces in a clockwise circle.
;; Spaces 1-6 each have: take-goods + two of one action + one of another.
;; Space 7 has: sell, deploy, temple, influence (the four strategic actions).

(def action-spaces
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
                {:type :temple}]}
   4 {:actions [{:type :take :resources [:pottery :tools]}
                {:type :influence}
                {:type :influence}
                {:type :deploy}]}
   5 {:actions [{:type :take :resources [:tools :gold]}
                {:type :sell}
                {:type :travel}
                {:type :travel}]}
   6 {:actions [{:type :take :resources [:gems :gold]}
                {:type :temple}
                {:type :temple}
                {:type :influence}]}
   7 {:actions [{:type :deploy}
                {:type :sell}
                {:type :influence}
                {:type :temple}]}})

;; Clockwise order: 1->2->3->4->5->6->7->1
(def action-space-order [1 2 3 4 5 6 7])

(defn move-astronomer-clockwise
  "Move an astronomer from space `from` by `steps` spaces clockwise.
   Returns the destination space number."
  [from steps]
  (let [idx (.indexOf action-space-order from)
        new-idx (mod (+ idx steps) 7)]
    (nth action-space-order new-idx)))

;; =============================================================================
;; City board
;; =============================================================================

;; Full board: 8 cities. 2/3-player removes Samarra (7 cities).
(def all-cities
  #{:samarra :nineveh :kish :babylon :nippur :lagash :uruk :eridu})

;; Routes are edges between cities. There are two types: :road and :river.
;; Magistrates only move clockwise on road routes.
;; Caravans can travel on any route.
;; Raiders are placed on routes (edges).

(def city-routes
  "All routes as #{[city-a city-b]} with route type."
  [{:from :samarra :to :nineveh :type :road}
   {:from :samarra :to :kish    :type :road}
   {:from :nineveh :to :babylon :type :road}
   {:from :babylon :to :uruk    :type :road}
   {:from :uruk    :to :eridu   :type :road}
   {:from :eridu   :to :lagash  :type :road}
   {:from :lagash  :to :nippur  :type :road}
   {:from :nippur  :to :kish    :type :road}
   ;; River routes (shortcuts across the board)
   {:from :babylon :to :kish    :type :river}
   {:from :uruk    :to :nippur  :type :river}
   {:from :uruk    :to :lagash  :type :river}])

(defn route-key
  "Canonical key for a route between two cities (alphabetical order)."
  [city-a city-b]
  (let [a (name city-a) b (name city-b)]
    (if (neg? (compare a b))
      [city-a city-b]
      [city-b city-a])))

(defn city-neighbors
  "All cities reachable from a city via any route type."
  [city routes]
  (set
   (for [{:keys [from to]} routes
         :when (or (= from city) (= to city))]
     (if (= from city) to from))))

(defn city-graph
  "Return adjacency map for the given player count."
  [player-count]
  (let [cities (if (#{2 3} player-count)
                 (disj all-cities :samarra)
                 all-cities)
        active-routes (filter (fn [{:keys [from to]}]
                                (and (contains? cities from)
                                     (contains? cities to)))
                              city-routes)]
    (into {}
          (for [c cities]
            [c (city-neighbors c active-routes)]))))

(defn active-routes
  "Return routes active for the given player count."
  [player-count]
  (let [cities (if (#{2 3} player-count)
                 (disj all-cities :samarra)
                 all-cities)]
    (filterv (fn [{:keys [from to]}]
               (and (contains? cities from)
                    (contains? cities to)))
             city-routes)))

(defn routes-from-city
  "All routes adjacent to a city."
  [city routes]
  (filterv (fn [{:keys [from to]}]
             (or (= from city) (= to city)))
           routes))

(defn route-other-city
  "Given a route and one city on it, return the other city."
  [route city]
  (if (= (:from route) city) (:to route) (:from route)))

;; Clockwise road order for magistrate movement
(def road-clockwise-order
  [:samarra :kish :nippur :lagash :eridu :uruk :babylon :nineveh])

(defn road-clockwise-next
  "Next city clockwise along roads from the given city."
  [city active-cities]
  (let [order (filterv active-cities road-clockwise-order)
        idx (.indexOf order city)]
    (when (>= idx 0)
      (nth order (mod (inc idx) (count order))))))

(def city-demand-count
  {:samarra 2 :nineveh 1 :kish 1 :babylon 1
   :nippur 1 :lagash 1 :uruk 1 :eridu 2})

;; --- Demand token bag ---

(def demand-tokens-per-type 7)

(defn full-demand-bag []
  (zipmap resource-types (repeat demand-tokens-per-type)))

(defn bag-total [bag]
  (apply + (vals bag)))

(defn draw-demand-token [bag]
  (let [choices (mapcat (fn [[t n]] (repeat n t)) bag)]
    (when (seq choices)
      (let [token (rand-nth choices)]
        [(update bag token dec) token]))))

(defn fill-demand-spaces
  "Draw tokens to fill empty demand spaces on cities.
   Returns [updated-bag updated-city-demands]."
  [bag city-demands cities]
  (reduce
   (fn [[bag demands] city]
     (let [max-slots (get city-demand-count city 1)
           current (get demands city [])
           needed (- max-slots (count current))]
       (if (pos? needed)
         (loop [b bag tokens current remaining needed]
           (if (or (zero? remaining) (zero? (bag-total b)))
             [b (assoc demands city tokens)]
             (let [[b' token] (draw-demand-token b)]
               (recur b' (conj tokens token) (dec remaining)))))
         [bag demands])))
   [bag city-demands]
   cities))

;; =============================================================================
;; Roles and levels
;; =============================================================================

(def roles [:merchant :priest :raider :leader])

(def max-role-level 5)

;; What each role level grants:
;; Merchant: amity scored per sell action
(def merchant-score {1 2, 2 3, 3 4, 4 5, 5 5})
;; Priest: maximum temples on the board
(def priest-max-temples {1 3, 2 4, 3 5, 4 8, 5 8})
;; Raider: maximum raiders deployed
(def raider-max-deployed {1 2, 2 3, 3 4, 4 6, 5 6})
;; Leader: max magistrate movement AND bonus glory for magistrate city
(def leader-movement {1 1, 2 2, 3 4, 4 5, 5 5})
(def leader-bonus    {1 1, 2 1, 3 2, 4 2, 5 3})

;; Threshold costs to advance roles (level you're entering -> cost).
;; nil means free. These are placeholder - adjust per actual player boards.
(def role-threshold-costs
  {:merchant {3 :pottery, 4 :gold}
   :priest   {3 :tools,   4 :gems}
   :raider   {3 :gold,    4 :tools}
   :leader   {3 :gems,    4 :pottery}})

;; =============================================================================
;; Player state
;; =============================================================================

(def starting-cards
  [{:number 1 :city :eridu   :role :merchant :resource :gems}
   {:number 2 :city :babylon :role :raider   :resource :gold}
   {:number 3 :city :uruk    :role :priest   :resource :pottery}
   {:number 4 :city :lagash  :role :leader   :resource :tools}
   {:number 5 :city :kish    :role :merchant :resource :gold}
   {:number 6 :city :nippur  :role :raider   :resource :gems}
   {:number 7 :city :uruk    :role :priest   :resource :tools}
   {:number 8 :city :babylon :role :leader   :resource :pottery}])

(def feat-cards
  [{:id :A :name "Builder of Empires"   :description "Build temples in 4 different cities"}
   {:id :B :name "Master Trader"        :description "Sell goods in 3 different cities in one round"}
   {:id :C :name "Star Reader"          :description "Have astronomers on 3 different action spaces"}
   {:id :D :name "Conqueror"            :description "Have 4 raiders deployed"}
   {:id :E :name "Sage of the Ziggurat" :description "Reach level 4 in two different roles"}])

(defn make-player
  [player-key card player-count]
  (let [num-astronomers (if (= 2 player-count) 3 2)
        role-levels (into {}
                         (for [r roles]
                           [r (if (= r (:role card)) 2 1)]))]
    {:key              player-key
     :roles            role-levels
     :resources        (merge {:tools 0 :pottery 0 :gold 0 :gems 0}
                              {(:resource card) 1})
     :caravan          (:city card)
     :astronomers      []     ;; positions on action wheel, filled during setup
     :dice-available   []     ;; rolled at start of each round
     :dice-used        []     ;; dice used this round
     :bonus-tokens     5
     :raiders-supply   6      ;; in player's supply (not yet deployed)
     :temples-supply   7      ;; 8 total, 1 placed at starting city
     :raiders          {}     ;; {route-key -> :raiding | :point}
     :temples          {}     ;; {city -> :face-up | :face-down}
     :demand-tokens    []     ;; collected demand tokens
     :bonus-board      (vec (repeat 5 :covered))
     :amity            0
     :glory            0
     :num-astronomers  num-astronomers
     :starting-card    card}))

(defn roll-dice
  "Roll 4 six-sided dice."
  []
  (vec (repeatedly 4 #(inc (rand-int 6)))))

(defn setup-player
  "Set up a player: roll dice for astronomer placement, place caravan and temple."
  [player _player-count]
  (let [n (:num-astronomers player)
        ;; Roll n dice for initial astronomer placement
        rolls (vec (repeatedly n #(inc (rand-int 6))))
        ;; Map die values to action spaces (1-6 map directly, but 7 isn't possible on d6)
        astronomer-positions (mapv #(if (> % 7) (mod % 7) %) rolls)]
    (-> player
        (assoc :astronomers astronomer-positions)
        ;; Place one face-up temple at starting city
        (assoc-in [:temples (:caravan player)] :face-up)
        ;; Roll 4 dice for the first round
        (assoc :dice-available (roll-dice)))))

;; =============================================================================
;; State queries
;; =============================================================================

(defn current-player [state]
  (get-in state [:turn-order (:current-player-idx state)]))

(defn current-phase [state]
  (get-in state [:player-turn :phase]))

(defn player-data [state player]
  (get-in state [:players player]))

(defn count-face-down-temples
  "Count how many face-down temples a player has."
  [player-data]
  (count (filter #(= :face-down (val %)) (:temples player-data))))

(defn count-temples-placed
  "Total temples on the board for a player."
  [player-data]
  (count (:temples player-data)))

(defn count-raiders-deployed
  "Total raiders deployed on routes for a player."
  [player-data]
  (count (:raiders player-data)))

(defn astronomers-on-space
  "Return list of [player-key astronomer-index] for all astronomers on a given space."
  [state space]
  (vec
   (for [[pk pdata] (:players state)
         [idx pos] (map-indexed vector (:astronomers pdata))
         :when (= pos space)]
     [pk idx])))

(defn magistrate-in-city?
  "True if any magistrate is in the given city."
  [state city]
  (some #(= city %) (vals (:magistrates state))))

;; =============================================================================
;; Turn & round management
;; =============================================================================

(def rounds-per-game 3)
(def turns-per-round 4)

(defn advance-turn
  "Move to the next player's turn."
  [state]
  (let [n (count (:turn-order state))
        next-idx (mod (inc (:current-player-idx state)) n)
        turn-in-round (get state :turn-in-round 1)
        last-turn? (and (= next-idx 0) (>= turn-in-round turns-per-round))]
    (if last-turn?
      ;; End of round
      (if (>= (:round state) rounds-per-game)
        ;; Game over
        (assoc state :game-over {:reason :end-of-game})
        ;; Start new round
        (let [new-round (inc (:round state))
              ;; Roll new dice for all players
              players (reduce-kv
                       (fn [ps pk pdata]
                         (assoc ps pk
                                (-> pdata
                                    (assoc :dice-available (roll-dice))
                                    (assoc :dice-used []))))
                       {}
                       (:players state))
              ;; Refill demand spaces
              cities (keys (:city-graph state))
              [bag demands] (fill-demand-spaces
                             (:demand-bag state)
                             (:city-demands state)
                             cities)]
          (-> state
              (assoc :round new-round
                     :turn-in-round 1
                     :current-player-idx 0
                     :players players
                     :demand-bag bag
                     :city-demands demands
                     :player-turn {:phase :choose-die}))))
      ;; Same round, next player (or next turn in round if wrapped)
      (let [new-turn (if (zero? next-idx) (inc turn-in-round) turn-in-round)]
        (-> state
            (assoc :current-player-idx next-idx
                   :turn-in-round new-turn
                   :player-turn {:phase :choose-die}))))))

;; =============================================================================
;; Initial state
;; =============================================================================

(defn initial-state [player-keys]
  (let [player-count (count player-keys)
        deck (shuffle starting-cards)
        dealt (take player-count deck)
        sorted-deals (sort-by (comp :number second)
                              (map vector player-keys dealt))
        turn-order (mapv first sorted-deals)
        players (into {}
                      (for [[pk card] sorted-deals]
                        [pk (setup-player
                             (make-player pk card player-count)
                             player-count)]))
        cities (if (#{2 3} player-count)
                 (disj all-cities :samarra)
                 all-cities)
        graph (city-graph player-count)
        routes (active-routes player-count)
        [bag city-demands] (fill-demand-spaces
                            (full-demand-bag)
                            {}
                            (vec cities))
        feats (vec (take 5 (shuffle feat-cards)))
        magistrate-cities (filterv cities [:uruk :kish])]
    {:turn-order         turn-order
     :current-player-idx 0
     :round              1
     :turn-in-round      1
     :player-turn        {:phase :choose-die}
     :players            players
     :action-spaces      action-spaces
     :city-graph         graph
     :routes             routes
     :city-demands       city-demands
     :demand-bag         bag
     :magistrates        (zipmap magistrate-cities (repeat :neutral))
     :first-player       (first turn-order)
     :feats              feats
     :feat-claims        {}  ;; {feat-id -> [player ...]} order of claims
     :game-over          nil}))
