(ns eridu.cards
  "Card data extracted from eridu.game in QA lesson 10. Pure declarative
   data — no functions, no dependencies on game state. Sourced from the
   MSE files in Dropbox (Starting Cards.mse-set, Bonus Contests.mse-set,
   BonusBoards.mse-set).

   Consumers should prefer requiring this namespace directly. The
   eridu.game namespace re-exports these symbols for backward compat with
   existing callers.")

;; =============================================================================
;; Starting Cards — from MSE "Starting Cards.mse-set"
;; =============================================================================
;; Symbol key: G=gems O=gold P=pottery L=tools

(def starting-cards
  [{:number 1 :city :babylon :role :leader   :resource :gems}     ;; A1
   {:number 2 :city :nippur  :role :merchant :resource :tools}    ;; B1
   {:number 3 :city :lagash  :role :merchant :resource :pottery}  ;; B2
   {:number 4 :city :babylon :role :priest   :resource :tools}    ;; A2
   {:number 5 :city :kish    :role :raider   :resource :gems}     ;; C1
   {:number 6 :city :kish    :role :leader   :resource :pottery}  ;; C2
   {:number 7 :city :uruk    :role :raider   :resource :pottery}  ;; D1
   {:number 8 :city :uruk    :role :priest   :resource :pottery}  ;; D2
   ])

;; =============================================================================
;; Bonus Contests (feat/race cards) — from MSE "Bonus Contests.mse-set"
;; =============================================================================
;; Symbol key from MSE: G=gems O=gold P=pottery L=tools A=amity Y=glory
;; Contest IDs are the "Cost" field from MSE

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
