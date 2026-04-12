(ns eridu.game)

;; =============================================================================
;; Constants
;; =============================================================================

(def resource-types [:tools :pottery :gold :gems])

(def action-types
  #{:take :sell :deploy :travel :influence :temple})

;; Mesopotamian-themed symbols for the astronomy board
(def action-icons
  {:take      "\uD83C\uDF3E"   ;; 🌾 sheaf of grain (goods/harvest)
   :sell      "\u2696"          ;; ⚖ scales (trade/commerce)
   :deploy    "\u2694"          ;; ⚔ crossed swords (raiders)
   :travel    "\uD83D\uDC2A"   ;; 🐪 camel (caravan travel)
   :influence "\uD83D\uDC51"   ;; 👑 crown (magistrate influence)
   :temple    "\uD83C\uDFDB"}) ;; 🏛 classical building (temple)

(def resource-icons
  {:tools   "\uD83D\uDD28"   ;; 🔨
   :pottery "\uD83C\uDFFA"   ;; 🏺
   :gold    "\uD83E\uDE99"   ;; 🪙
   :gems    "\uD83D\uDC8E"}) ;; 💎

(def resource-colors
  {:tools "#D4913A" :pottery "#B074C8" :gold "#E8D44D" :gems "#4DE8D4"})

;; Player colors for distinguishing pieces
(def player-colors
  ["#4A90D9" "#D94A4A" "#4AD95A" "#D9B44A" "#9A4AD9" "#D94A90"])

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
   ;; Nineveh↔Kish road (active only in 2-3 player when Samarra is removed)
   {:from :nineveh :to :kish    :type :road :only-without #{:samarra}}
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

(defn- route-active?
  "True if a route is active for the given set of cities."
  [cities {:keys [from to only-without]}]
  (and (contains? cities from)
       (contains? cities to)
       (or (nil? only-without)
           (every? #(not (contains? cities %)) only-without))))

(defn city-graph
  "Return adjacency map for the given player count."
  [player-count]
  (let [cities (if (#{2 3} player-count)
                 (disj all-cities :samarra)
                 all-cities)
        routes (filter (partial route-active? cities) city-routes)]
    (into {}
          (for [c cities]
            [c (city-neighbors c routes)]))))

(defn active-routes
  "Return routes active for the given player count."
  [player-count]
  (let [cities (if (#{2 3} player-count)
                 (disj all-cities :samarra)
                 all-cities)]
    (filterv (partial route-active? cities) city-routes)))

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

(defn road-clockwise-path
  "Return the list of [from to] route pairs for a magistrate moving
   `steps` spaces clockwise from `start-city`."
  [start-city steps active-cities]
  (loop [city start-city
         remaining steps
         path []]
    (if (zero? remaining)
      path
      (let [next-city (road-clockwise-next city active-cities)]
        (if next-city
          (recur next-city (dec remaining)
                 (conj path [city next-city]))
          path)))))

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

;; =============================================================================
;; Bonus Contests (feat/race cards) — from MSE "Bonus Contests.mse-set"
;; =============================================================================
;; Symbol key from MSE: G=gems O=gold P=pottery L=tools A=amity Y=glory
;; Bonus values per card: first to claim gets 3, then 2, 1, 1
;; Contest IDs are the "Cost" field from MSE

(def bonus-contest-values [3 2 1 1])

(def bonus-contests
  [;; --- A: Fulfill goods ---
   {:id :A1 :name "Fulfill Gems/Gold"
    :description "Fulfill 3 Gems and/or Gold"
    :category :fulfill}
   {:id :A2 :name "Fulfill Tools/Pottery"
    :description "Fulfill 3 Tools and/or Pottery"
    :category :fulfill}
   ;; --- B: Fulfill patterns ---
   {:id :B1 :name "Fulfill Same Type"
    :description "Fulfill 3 goods of the same type"
    :category :fulfill}
   {:id :B2 :name "Fulfill All Types"
    :description "Fulfill one or more good of all four types"
    :category :fulfill}
   ;; --- C: Temple count ---
   {:id :C1 :name "Four Face-Up Temples"
    :description "Four face-up temples"
    :category :temple}
   {:id :C2 :name "Four Face-Down Temples"
    :description "Four face-down temples"
    :category :temple}
   ;; --- D: Temple placement ---
   {:id :D1 :name "Temples in Eridu & Nineveh"
    :description "A temple in each Eridu and Nineveh"
    :category :temple}
   {:id :D2 :name "Temples in River Cities"
    :description "A temple in four river cities"
    :category :temple}
   ;; --- E: Raider placement ---
   {:id :E1 :name "Surround Kish"
    :description "Raiders surrounding Kish"
    :category :raider}
   {:id :E2 :name "Raiders at Eridu & Nineveh"
    :description "Raiders next to Eridu and Nineveh"
    :category :raider}
   ;; --- F: Raider state ---
   {:id :F1 :name "Three Point Raiders"
    :description "Three Raiders on their point side"
    :category :raider}
   {:id :F2 :name "Raiders on Rivers"
    :description "A raider on each river"
    :category :raider}
   ;; --- G: Magistrate movement ---
   {:id :G1 :name "Move Magistrate Four"
    :description "Move one Magistrate four cities in one turn"
    :category :magistrate}
   {:id :G2 :name "Magistrate Through Raiders"
    :description "Move a Magistrate through three raiders (owned by any player)"
    :category :magistrate}
   ;; --- H: Role levels ---
   {:id :H1 :name "Two Roles at Level 3+"
    :description "Two roles at level 3 or higher"
    :category :role}
   {:id :H2 :name "Any Role at Level 5"
    :description "Any Role at Level 5"
    :category :role}
   ;; --- I: Scoring thresholds ---
   {:id :I1 :name "10 Points with Temple Flip"
    :description "Earn 10 points on a turn where you flip at least 1 Temple (Amity and/or Glory)"
    :category :scoring}
   {:id :I2 :name "5 Glory in One Turn"
    :description "Score 5 Glory in one turn"
    :category :scoring}
   ;; --- J: Scoring thresholds ---
   {:id :J1 :name "5 Amity in One Turn"
    :description "Score 5 Amity in one turn"
    :category :scoring}
   {:id :J2 :name "Only Tools"
    :description "Have two Tools but no other goods"
    :category :resource}
   ;; --- K: Sell achievements ---
   {:id :K1 :name "Big Gold Sale"
    :description "Earn 5 total points by selling one Gold (Amity and/or Glory)"
    :category :sell}
   {:id :K2 :name "Sell in Surrounded City"
    :description "Sell in a city surrounded by Raiders"
    :category :sell}
   ;; --- L: Resource hoarding ---
   {:id :L1 :name "5 Gems"
    :description "Have 5 Gems"
    :category :resource}
   {:id :L2 :name "5 Pottery"
    :description "Have 5 Pottery"
    :category :resource}
   ;; --- M: Magistrate + temple combos ---
   {:id :M1 :name "Magistrates at Temples"
    :description "Both Magistrates in cities with your facedown temples"
    :category :magistrate}
   {:id :M2 :name "Temples Without Demand"
    :description "Four temples in cities with no demand"
    :category :temple}])

;; =============================================================================
;; Bonus Boards — from MSE "BonusBoards.mse-set"
;; =============================================================================
;; Each board has 5 effects: Effect1 is a persistent/passive ability,
;; Effects 2-5 are one-time bonuses uncovered in order.
;; Symbol key: G=gems O=gold P=pottery L=tools A=amity Y=glory

(def bonus-boards
  [{:id 1 :name "Shield of Gilgamesh"
    :effects
    ["When you surround a city with Raiders, put a temple in it (you don't have to be there)"
     "Travel to Kish via the shortest route (you may choose between equal routes)"
     "Increase your Raider and Leader Roles (paying any costs)"
     "Place two Raiders adjacent to Lagash (you don't have to be there)"
     "Score Glory for each demand you have fulfilled"]}
   {:id 2 :name "Seal of Enmerkar"
    :effects
    ["When you score a Raider you may increase your Priest role (paying any costs)"
     "Increase your Merchant and Raider Roles (paying any costs)"
     "Score 5 Amity if you are in a city with a Magistrate"
     "Place a Temple in a city with a Magistrate (even if you already have a temple there)"
     "Score Glory for each of your facedown Temples"]}
   {:id 3 :name "Voyage of Ziusudra"
    :effects
    ["When you Travel across a river take a Gem. Your Gems are worth Amity each at end of game"
     "Increase your Leader Role for Free"
     "Place a Temple in Lagash (even if you already have a temple there)"
     "Place a Raider adjacent to Eridu and gain a good of your choice"
     "Take a travel action then a Sell action"]}
   {:id 4 :name "Blessing of Inanna"
    :effects
    ["When you flip a temple you may sell in that city"
     "Place a Temple in Eridu (even if you already have a temple there)"
     "Gain Tools, Gems, Gold"
     "Score Amity based on your Leader level x 2"
     "Score 2 Amity for each of your Raiders"]}
   {:id 5 :name "Wisdom of Adapa"
    :effects
    ["When you Influence a Magistrate in your city you may travel with it"
     "Increase your Priest Role for Free"
     "Place two random Demand Tokens in Uruk. Gain the matching resources"
     "Take a Deploy action then a Temple action"
     "Score 2 Amity for each of your Raiders"]}
   {:id 6 :name "Trade of Dumuzid"
    :effects
    ["When you use action space 7 you get a free Travel action"
     "Increase your Merchant and Priest Roles (paying any costs)"
     "Place a temple in each city with a Magistrate (if you don't have one there)"
     "Sell to Babylon for double points (you don't need to be there)"
     "Place a Raider adjacent to Lagash. Gain Tools, Tools"]}
   {:id 7 :name "March of Lugalbanda"
    :effects
    ["When you place Raiders you may place an additional one next to a Magistrate"
     "Increase your Merchant and Leader Roles (paying any costs)"
     "Place a Temple in a city with a Magistrate (even if you already have a temple there)"
     "Take a travel action. Score 3 Glory if you are in Eridu"
     "Take a travel action. Score 3 Amity if you are in Kish"]}
   {:id 8 :name "Fury of Enkidu"
    :effects
    ["When you score a Raider, instead flip it to its active side"
     "Increase your Raider and Priest Roles (paying any costs)"
     "Place one random Demand Token in Nippur and Babylon each. Then you may sell once in your city"
     "Gain Gold, Gems, Pottery. Then you may sell once in your city"
     "Flip all of your Raiders to their point side"]}
   {:id 9 :name "Rites of Ninhursag"
    :effects
    ["When you flip a Temple, you may increase a role (paying any costs)"
     "Gain Tools, Gold, Pottery. Score Amity based on your Leader level"
     "Increase your Priest and Leader Roles (paying any costs)"
     "Place a Raider on each River"
     "Sell to any city with a Magistrate. If you are in that city, you may take a Temple action"]}
   {:id 10 :name "Wealth of Meskalamdug"
    :effects
    ["You may sell Gold to cities with no demands. If you do, place a random Demand Token on that city"
     "Increase your Merchant Role for Free"
     "Increase your Merchant Role for Free"
     "Place a Raider adjacent to a Magistrate. Score Amity based on your Leader level"
     "Place a Temple in Nippur (even if you already have a temple there)"]}
   {:id 11 :name "Ambition of Sargon"
    :effects
    ["When you meet this and other contests, score additional Glory based on your Leader level"
     "Place two random Demand Tokens in Lagash. Gain matching resources"
     "Sell to Lagash for Double Glory points (you don't have to be there)"
     "Increase your Raider Role for Free"
     "Score Glory for each of your facedown Temples"]}
   {:id 12 :name "Currents of Enki"
    :effects
    ["When you cross a river, place a raider on that river"
     "Increase all of your Level One Roles"
     "Gain Gold, Gold, Gold, Gems"
     "Increase your Merchant level (paying any costs). Then Sell to the city you are in for Glory instead"
     "Score Glory for each of your facedown Temples"]}
   {:id 13 :name "Pillars of Etana"
    :effects
    ["When you place a Temple you may place a Raider adjacent to it"
     "Gain Tools, Tools, Tools. Score Glory based on your Leader Level"
     "Gain Pottery, Pottery, Pottery. Score Glory based on your Leader Level"
     "Increase all of your Level Three Roles (paying any costs)"
     "Place a Temple adjacent to one of your Raiders (even if you already have a temple there)"]}
   {:id 14 :name "Roads of Shulgi"
    :effects
    ["On your turn you may move between Uruk and an adjacent city by discarding one good as a bonus action"
     "Place a Raider adjacent to Lagash. Then score Glory for each of your Raiders"
     "Move a Magistrate to Uruk. Then gain resources matching Uruk's demands"
     "Place two random Demand Tokens in Eridu. Travel to Eridu via the shortest route (you may choose between equal routes)"
     "Place a Temple in Babylon (even if you already have a temple there)"]}
   {:id 15 :name "Ascent of Ur-Nammu"
    :effects
    ["When you increase a role, you may increase it for free"
     "For each demand you have fulfilled, take a matching good"
     "Increase your Priest role. Then score 4 Glory if you have a facedown temple in Babylon"
     "Increase your lowest role then take a Travel action (you pick if there is a tie)"
     "Score 3 Amity for each Raider you have adjacent to a Magistrate"]}
   {:id 16 :name "Dominion of Hammurabi"
    :effects
    ["When you take an action space with exactly two astronomers on it, take a third action"
     "Take a Pottery for each Temple you have"
     "Deploy then score Amity for each Raider you have"
     "Increase your Leader role twice (paying any costs)"
     "Put two random demand tokens on the city you are in. You may take Sell action"]}
   {:id 17 :name "Cunning of Kubaba"
    :effects
    ["When you use action space 7 take a good of your choice"
     "Place a Raider next to Eridu on its point side"
     "Place one facedown Temple on each city with a Magistrate (even if you have temples there)"
     "Score 8 Amity if you have Uruk surrounded by Raiders. Then you may flip one of those raiders"
     "Sell to the city your caravan is in for Glory instead"]}
   {:id 18 :name "Forge of Tubal-Cain"
    :effects
    ["When you spend Tools in any way, instead keep them. Your Tools are worth Glory each at end of game"
     "Move a Magistrate across a river. You may sell in your caravan's city"
     "Take a travel action then score 5 Glory if you have a facedown temple in Samarra"
     "Score 6 Amity if you have Kish surrounded by Raiders. Then you may flip one of those raiders"
     "Score 4 Amity for each of your Raiders on their point side. Then remove those raiders"]}
   {:id 19 :name "Kilns of Ninkasi"
    :effects
    ["When you take Pottery, take an extra Pottery, Pottery"
     "Increase your Priest role twice (paying any costs)"
     "Sell to two cities that demand Pottery (you don't have to be there)"
     "Discard a good to move a Magistrate to your City. Then take a sell action"
     "Flip all of your placed Raiders to their point side"]}
   {:id 20 :name "Vision of Rimush"
    :effects
    ["When you flip a Temple you may discard a Pottery. If you do, score 3 Glory"
     "Place a Raider on each route with an opposing raider"
     "Increase your Merchant role twice (paying any costs)"
     "Influence a Magistrate. Then score Amity based on your leader level"
     "Take up to four goods based on the action spaces your Astronomers occupy"]}
   {:id 21 :name "Legacy of Eannatum"
    :effects
    ["When you place a temple in a city, you may place an additional temple facedown in that city"
     "If you are in Eridu, travel anywhere via the shortest path (you choose between ties)"
     "Increase your Raider and Leader roles (paying any costs)"
     "Travel to an adjacent city then you may Sell to it"
     "Score Glory for each demand you have fulfilled"]}
   {:id 22 :name "Strategy of Naram-Sin"
    :effects
    ["When taking actions on action space 7 you may take the same action twice"
     "Increase your Raider and Merchant Roles (paying any costs)"
     "Put a random demand token on each of your facedown temples. Only you may fulfill those demands"
     "Take a good of your choice. Then take a travel action"
     "Score 2 Amity for each of your Raiders. Then take a travel action"]}
   {:id 23 :name "Market of Puabi"
    :effects
    ["When you sell, score Glory instead of Amity"
     "Increase your Priest and Merchant Roles (paying any costs)"
     "Sell twice to Eridu (you don't need to be there)"
     "Take a good of your choice. Then take a travel action. Increase your Merchant Role (paying any costs)"
     "Place a Temple in a city with a Magistrate (even if you already have a temple there)"]}
   {:id 24 :name "Siege of Shulme"
    :effects
    ["When you surround a City with Raiders you may Sell to that city (even if you aren't there)"
     "Increase your Raider and Leader Roles (paying any costs)"
     "Put a random demand token on each Magistrate. Only you may fulfill those demands"
     "Score Glory for each demand you have fulfilled"
     "Take a good for each demand in cities with Magistrates"]}
   {:id 25 :name "Command of Mesannepada"
    :effects
    ["You may have two raiders on each path"
     "Influence a Magistrate. Immediately score all of your raiders it moved through"
     "Increase your Merchant and Leader Roles (paying any costs)"
     "Place two facedown temples in your city (even if you already have a temple there)"
     "Take a good of your choice. Then take a Travel action"]}
   {:id 26 :name "Court of Enshakushanna"
    :effects
    ["When you score Magistrate bonus points, score an additional 2 Amity"
     "Increase your Priest and Leader Roles (paying any costs)"
     "Increase your Priest and Raider Roles (paying any costs)"
     "Sell in your city. If you sold Tools or Pottery you may place a Temple in your city (even if you already have a temple there)"
     "Place a Raider adjacent to your city. If you surround it, you may place a temple in it (even if you already have a temple there)"]}
   {:id 27 :name "Path of Alulim"
    :effects
    ["When you increase a role, you may increase another role, paying double the normal cost"
     "Travel to an adjacent city then you may Sell to it"
     "Travel to an adjacent city then you may take a Deploy action"
     "Travel to an adjacent city then you may place a Temple in it"
     "Take three goods of your choice"]}
   {:id 28 :name "Stars of Sin-Kashid"
    :effects
    ["You may increase a role at the end of your turn if you landed on a space with four or more Astronomers"
     "Travel to an adjacent city then you may place a Temple in it (even if you already have a temple there)"
     "Travel to an adjacent city then you may place a Temple in it (even if you already have a temple there)"
     "Sell Gold or Gold to your city if it has no Demands. Then place a random demand on it"
     "Put a raider point-side up adjacent to Kish"]}
   {:id 29 :name "Treasury of Ibbi-Sin"
    :effects
    ["When you pay a Gold for any reason gain 2 Amity"
     "Decrease your Leader role to increase all of your other roles (paying any costs)"
     "Take a travel action then you may take a sell action"
     "Place a raider on each river"
     "Place a Temple in each city surrounded by your Raiders (even if you have a Temple there)"]}
   {:id 30 :name "Council of Amar-Sin"
    :effects
    ["When taking goods you may instead take goods based on one of your other Astronomer's location on the action wheel"
     "Influence a Magistrate then take a Travel action"
     "Influence a Magistrate then take a Sell action"
     "Take a Deploy action then Influence a Magistrate"
     "Influence a Magistrate then take a Temple action"]}
   {:id 31 :name "Horizon of Sharkalisharri"
    :effects
    ["When taking actions if one of your other Astronomers is on space 7, you may take a bonus Travel action"
     "Increase all of your level one roles"
     "Increase all of your level three roles (paying any costs)"
     "Gain a resource of your choice and place a Facedown temple in your city (even if you already have a Temple there)"
     "Gain a resource of your choice and take a Deploy action"]}
   {:id 32 :name "Jewel of Ku-Bau"
    :effects
    ["When you sell you may discard a Gem to score Amity based on your Priest level instead of Merchant level"
     "Sell in your city then Score Glory for each demand you have fulfilled"
     "Take a Gem. Take two travel actions"
     "Place a raider in each route that has one of your Temples in both cities"
     "Influence a Magistrate then you may take sell action"]}
   {:id 33 :name "Vanguard of Enmebaragesi"
    :effects
    ["When you deploy, you may Influence an adjacent Magistrate"
     "Decrease your Merchant role to increase all of your other roles (paying any costs)"
     "Place a facedown Temple in your city then take a travel action (even if you already have a Temple there)"
     "Place a face up Temple in Uruk (even if you already have a Temple there)"
     "Deploy a raider adjacent to your city then take a travel action"]}
   {:id 34 :name "Honor of Agga"
    :effects
    ["When you score raiders, score Amity instead of Glory"
     "Pay Tools, Tools to place a Raider on each space surrounding Uruk"
     "Place a raider on each route you have a raider"
     "Take a Sell action in each city that has both a Magistrate and one of your Temples (you don't have to be there)"
     "Take a Sell action in each city that has both a Magistrate and one of your Temples (you don't have to be there)"]}
   {:id 35 :name "Wanderer of Dumuzi"
    :effects
    ["At the start of your turn if you have no goods, gain a good of your choice"
     "Travel then take a Sell action"
     "You may pay any number of Pottery. For each Pottery you paid, place a Temple in a city which you have a Temple"
     "Increase the role of your choice (paying any costs)"
     "Influence a Magistrate. Score each of your Raiders it moved through"]}])

(def bonus-boards-by-id
  "Lookup bonus board by numeric ID."
  (into {} (map (juxt :id identity) bonus-boards)))

(def bonus-contests-by-id
  "Lookup bonus contest by keyword ID."
  (into {} (map (juxt :id identity) bonus-contests)))

(defn player-color
  "Get the color for a player based on turn order index."
  [state player-key]
  (let [idx (.indexOf (:turn-order state) player-key)]
    (get player-colors (max 0 idx) "#888")))

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
        ;; Select 5 double-sided feat cards: group by letter, pick 5 cards,
        ;; randomly show one side of each
        contest-pairs (vals (group-by #(first (name (:id %))) bonus-contests))
        selected-pairs (take 5 (shuffle contest-pairs))
        contests (vec (map #(rand-nth %) selected-pairs))
        boards (vec (take player-count (shuffle bonus-boards)))
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
     :contests           contests          ;; bonus contest cards in play
     :contest-claims     {}                ;; {contest-id -> [player ...]} claim order
     :bonus-boards       (zipmap turn-order
                                 (map :id boards))  ;; {player-key -> board-id}
     :log                []
     :game-over          nil}))
