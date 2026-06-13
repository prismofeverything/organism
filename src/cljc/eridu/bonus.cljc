(ns eridu.bonus
  "Bonus board classification and choice routing — pure data + lookup helpers
   with NO dependency on game state. Extracted from eridu.game in QA lesson 11.

   What lives here:
   - effect-implementation-status: per-slot honesty map (:implemented / :partial / :persistent / ...)
   - board-effect-diagnostic:      tally of how many slots are in each status
   - bonus-needs-choice?:          pure choice-descriptor lookup for the UI/WS layer

   What does NOT live here (yet):
   - apply-bonus-effect / -dispatch / -with-choice: still in eridu.game because
     they need state predicates (magistrate-cities, routes-from-city, etc.).
     A future lesson could pull those out alongside an eridu.state ns.

   The eridu.game namespace re-exports these symbols for backward compat with
   existing callers (game/effect-implementation-status, etc.).")

(def effect-implementation-status
  "Classification of every board effect: what it needs to work.
   :implemented = faithful to the printed card,
   :partial     = the code does *something* but doesn't fully match the
                  printed text — see the per-entry comment for what's missing,
   :persistent  = slot-0 passive (tracked separately from instant slots),
   :needs-compound  = requires multi-action sequence engine (aspirational),
   :needs-placement = requires conditional placement logic (aspirational),
   :needs-demand    = requires demand token manipulation (aspirational),
   :conditional     = requires specific board state check (aspirational)."
  {[1 0] :persistent    ;; When you surround a city with Raiders, temple in it
   [1 1] :implemented   ;; Travel to Kish
   [1 2] :implemented   ;; Increase Raider and Leader
   [1 3] :implemented   ;; Place two raiders near Lagash
   [1 4] :implemented   ;; Glory per demand fulfilled
   [2 0] :persistent    ;; When you score Raider, increase Priest
   [2 1] :implemented   ;; Increase Merchant and Raider
   [2 2] :implemented   ;; 5 Amity if at magistrate
   [2 3] :implemented   ;; Temple in magistrate city
   [2 4] :implemented   ;; Glory per facedown temple
   [3 0] :persistent    ;; River travel → gem + gems worth amity
   [3 1] :implemented   ;; Increase Leader free
   [3 2] :implemented   ;; Temple in Lagash
   [3 3] :implemented   ;; Raider near Eridu + good
   [3 4] :implemented   ;; Travel then Sell
   [4 0] :persistent    ;; When flip temple, may sell
   [4 1] :implemented   ;; Temple in Eridu
   [4 2] :implemented   ;; Gain Tools, Gems, Gold
   [4 3] :implemented   ;; Amity = Leader x 2
   [4 4] :implemented   ;; 2 Amity per raider
   [5 0] :persistent    ;; Influence magistrate in your city, travel with it
   [5 1] :implemented   ;; Increase Priest free
   [5 2] :implemented   ;; Place demand tokens in Uruk + gain resources
   [5 3] :implemented   ;; Deploy then Temple
   [5 4] :implemented   ;; 2 Amity per raider
   [6 0] :persistent    ;; Space 7 → free Travel
   [6 1] :implemented   ;; Increase Merchant and Priest
   [6 2] :implemented   ;; Temple in each magistrate city
   [6 3] :implemented   ;; Sell to Babylon double
   [6 4] :partial         ;; Raider near Lagash + Tools x2 (partial)
   [7 0] :persistent    ;; Place raiders, extra one next to magistrate
   [7 1] :implemented   ;; Increase Merchant and Leader
   [7 2] :implemented   ;; Temple in magistrate city
   [7 3] :implemented   ;; Travel + 3 Glory if at Eridu
   [7 4] :implemented   ;; Travel + 3 Amity if at Kish
   [8 0] :persistent    ;; Score raider → flip to active instead
   [8 1] :implemented   ;; Increase Raider and Priest
   [8 2] :implemented   ;; Place demand + sell
   [8 3] :partial         ;; Gain Gold, Gems, Pottery (partial, no sell)
   [8 4] :implemented   ;; Flip all raiders to point
   [9 0] :persistent    ;; Flip temple → may increase role
   [9 1] :implemented   ;; Gain Tools, Gold, Pottery + Amity = leader
   [9 2] :implemented   ;; Increase Priest and Leader
   [9 3] :implemented   ;; Raider on each river
   [9 4] :implemented   ;; Sell to magistrate city + temple
   [10 0] :persistent   ;; Sell gold to empty demand cities
   [10 1] :implemented  ;; Increase Merchant free
   [10 2] :implemented  ;; Increase Merchant free
   [10 3] :implemented  ;; Raider near magistrate + amity
   [10 4] :implemented  ;; Temple in Nippur
   [11 0] :persistent   ;; Extra glory on contest claims
   [11 1] :implemented  ;; Place demand tokens in Lagash
   [11 2] :implemented  ;; Sell to Lagash double glory
   [11 3] :implemented  ;; Increase Raider free
   [11 4] :implemented  ;; Glory per facedown temple
   [12 0] :persistent   ;; River crossing → place raider
   [12 1] :implemented  ;; Increase all level-1 roles
   [12 2] :implemented  ;; Gain Gold x3 + Gems
   [12 3] :implemented  ;; Increase merchant + sell for glory
   [12 4] :implemented  ;; Glory per facedown temple
   [13 0] :persistent   ;; Temple placement → raider adjacent
   [13 1] :implemented  ;; Gain Tools x3 + Glory = leader
   [13 2] :implemented  ;; Gain Pottery x3 + Glory = leader
   [13 3] :implemented  ;; Increase all level-3 roles
   [13 4] :implemented  ;; Temple adjacent to raider (player picks the city)
   [14 0] :persistent   ;; Uruk travel bonus action
   [14 1] :partial       ;; Glory per raider (partial: no placement)
   [14 2] :partial       ;; Resources (partial: no magistrate move)
   [14 3] :partial       ;; Travel to Eridu (partial: no demands)
   [14 4] :implemented  ;; Temple in Babylon
   [15 0] :persistent   ;; Free role increases
   [15 1] :implemented  ;; Good per demand fulfilled
   [15 2] :implemented  ;; Increase Priest + 4 Glory if Babylon temple
   [15 3] :partial       ;; Increase lowest role (partial: no travel)
   [15 4] :partial       ;; 2 Amity per raider (partial: no adjacency check)
   [16 0] :persistent   ;; 2-astronomer space → third action
   [16 1] :implemented  ;; Pottery per temple
   [16 2] :implemented  ;; Deploy + amity per raider
   [16 3] :implemented  ;; Increase Leader twice
   [16 4] :implemented  ;; Place demands + sell
   [17 0] :persistent   ;; Space 7 → good of choice
   [17 1] :partial       ;; Flip one raider to point (partial: no placement)
   [17 2] :partial       ;; Temple in magistrate city (partial: no facedown)
   [17 3] :partial       ;; 4 Amity (partial: no Uruk surround check)
   [17 4] :partial       ;; Glory = merchant level (partial: no sell)
   [18 0] :persistent   ;; Keep tools when spent + tools worth glory
   [18 1] :partial       ;; Resources (partial: no magistrate move/sell)
   [18 2] :partial        ;; 5 Glory if facedown Samarra (partial otherwise)
   [18 3] :partial       ;; 3 Amity (partial: no surround check)
   [18 4] :implemented  ;; 4 Amity per point raider, then remove (lesson 8 fix)
   [19 0] :persistent   ;; Take pottery → extra pottery x2
   [19 1] :implemented  ;; Increase Priest twice
   [19 2] :implemented  ;; Sell to pottery cities
   [19 3] :implemented  ;; Discard good + move magistrate + sell
   [19 4] :implemented  ;; Flip all raiders to point
   [20 0] :persistent   ;; Flip temple → discard pottery for 3 glory
   [20 1] :implemented  ;; Raider on each opposing route
   [20 2] :implemented  ;; Increase Merchant twice
   [20 3] :partial        ;; Amity = leader level (partial, no influence)
   [20 4] :implemented  ;; Take goods from astronomer spaces
   [21 0] :persistent   ;; Temple placement → extra facedown
   [21 1] :implemented  ;; Travel to Eridu
   [21 2] :implemented  ;; Increase Raider and Leader
   [21 3] :partial       ;; Travel to Eridu (partial: no sell)
   [21 4] :implemented  ;; Glory per demand fulfilled
   [22 0] :persistent   ;; Space 7 same action twice
   [22 1] :implemented  ;; Increase Raider and Merchant
   [22 2] :implemented  ;; Demands on facedown temples
   [22 3] :implemented  ;; Good + travel
   [22 4] :partial        ;; 2 Amity per raider (partial, no travel)
   [23 0] :persistent   ;; Sell → glory instead of amity
   [23 1] :implemented  ;; Increase Priest and Merchant
   [23 2] :implemented  ;; Sell twice to Eridu
   [23 3] :implemented  ;; Good + travel + increase merchant
   [23 4] :implemented  ;; Temple in magistrate city
   [24 0] :persistent   ;; Surround city → sell there
   [24 1] :implemented  ;; Increase Raider and Leader
   [24 2] :implemented  ;; Demands on magistrates
   [24 3] :implemented  ;; Glory per demand fulfilled
   [24 4] :implemented  ;; Goods per demand at magistrates
   [25 0] :persistent   ;; Two raiders per path
   [25 1] :implemented  ;; Influence + score raiders
   [25 2] :implemented  ;; Increase Merchant and Leader
   [25 3] :implemented  ;; Two facedown temples
   [25 4] :implemented  ;; Good + travel
   [26 0] :persistent   ;; Extra 2 amity on magistrate bonus
   [26 1] :implemented  ;; Increase Priest and Leader
   [26 2] :implemented  ;; Increase Priest and Raider
   [26 3] :implemented  ;; Sell + temple
   [26 4] :implemented  ;; Raider + surround check
   [27 0] :persistent   ;; Role increase → another role for double cost
   [27 1] :implemented  ;; Travel + sell
   [27 2] :implemented  ;; Travel + deploy
   [27 3] :implemented  ;; Travel + temple
   [27 4] :implemented  ;; Three goods
   [28 0] :persistent   ;; 4+ astronomers → role increase
   [28 1] :implemented  ;; Travel + temple
   [28 2] :implemented  ;; Travel + temple
   [28 3] :implemented  ;; Sell gold to empty city
   [28 4] :implemented  ;; Raider point-side near Kish
   [29 0] :persistent   ;; Pay gold → 2 amity
   [29 1] :implemented  ;; Decrease leader + increase others
   [29 2] :implemented  ;; Travel + sell
   [29 3] :implemented  ;; Raider on each river
   [29 4] :implemented  ;; Temple in surrounded cities
   [30 0] :persistent   ;; Take goods from other astronomer location
   [30 1] :partial       ;; Glory = leader level (partial: no influence+travel)
   [30 2] :partial       ;; Amity = leader level (partial: no influence+sell)
   [30 3] :partial       ;; Glory = raider level (partial: no deploy+influence)
   [30 4] :partial       ;; Amity = priest level (partial: no influence+temple)
   [31 0] :persistent   ;; Other astronomer on space 7 → bonus travel
   [31 1] :implemented  ;; Increase all level-1 roles
   [31 2] :implemented  ;; Increase all level-3 roles
   [31 3] :implemented  ;; Resource + facedown temple
   [31 4] :implemented  ;; Resource + deploy
   [32 0] :persistent   ;; Sell: discard gem for priest-level scoring
   [32 1] :partial        ;; Glory per demand (partial, no sell)
   [32 2] :partial        ;; Gem (partial, no travel)
   [32 3] :implemented  ;; Raider between temple cities
   [32 4] :implemented  ;; Influence + sell
   [33 0] :persistent   ;; Deploy → influence adjacent magistrate
   [33 1] :implemented  ;; Decrease merchant + increase others
   [33 2] :implemented  ;; Facedown temple + travel
   [33 3] :implemented  ;; Temple in Uruk
   [33 4] :implemented  ;; Deploy + travel
   [34 0] :persistent   ;; Score raiders → amity instead of glory
   [34 1] :implemented  ;; Pay tools for raiders around Uruk
   [34 2] :implemented  ;; Raider on each existing route
   [34 3] :implemented  ;; Sell at magistrate+temple cities
   [34 4] :implemented  ;; Same as 34-3
   [35 0] :persistent   ;; No goods → gain good of choice
   [35 1] :implemented  ;; Travel + sell
   [35 2] :implemented  ;; Pay pottery for temples
   [35 3] :implemented  ;; Increase role of choice
   [35 4] :implemented  ;; Influence + score raiders
   })

(defn board-effect-diagnostic
  "Generate a diagnostic report of which board effects work and which don't.
   Returns {:total N :implemented N :partial N :persistent N
            :needs-compound N :needs-placement N :needs-demand N :conditional N}.
   :partial entries are 'implemented enough to do something' but not faithful
   to the printed card text — see per-entry comments in effect-implementation-status."
  []
  (let [by-status (group-by val effect-implementation-status)]
    {:total (count effect-implementation-status)
     :implemented (count (get by-status :implemented []))
     :partial (count (get by-status :partial []))
     :persistent (count (get by-status :persistent []))
     :needs-compound (count (get by-status :needs-compound []))
     :needs-placement (count (get by-status :needs-placement []))
     :needs-demand (count (get by-status :needs-demand []))
     :conditional (count (get by-status :conditional []))}))

(defn bonus-needs-choice?
  "Returns a choice descriptor if [board-id slot-idx] needs player input,
   nil if it auto-resolves. Choice types:
   :pick-resource — player picks one of 4 resources
   :pick-city — player picks a city (for temple/travel)
   :pick-role — player picks a role to increase"
  [board-id slot-idx]
  (case [board-id slot-idx]
    ;; ── Pick resource ──────────────────────────────────────────────
    ([3 3] [17 1] [22 3] [23 3] [25 4] [31 3] [31 4])
    {:type :pick-resource :prompt "Choose a resource to gain"}
    [27 4] {:type :pick-resource :prompt "Choose a resource to gain (1 of 3)" :count 3}
    [19 3] {:type :pick-resource :prompt "Choose a resource to discard (to move magistrate + sell)"}

    ;; ── Pick role ───────────────────────────────────────────────
    ([15 3] [35 3])
    {:type :pick-role :prompt "Choose a role to increase"}

    ;; ── Travel to adjacent city + action ────────────────────────
    ;; sell after travel
    ([3 4] [27 1] [21 3] [29 2] [35 1])
    {:type :pick-city :prompt "Travel to adjacent city and sell"
     :filter :adjacent :action :sell}

    ;; ── Board 34 #4/#5: sell in each city with Magistrate + your Temple
    ;; (you don't have to be there) — multi-pick, repeats until all done
    ([34 3] [34 4])
    {:type :pick-city
     :prompt "Take a Sell action in a city with a Magistrate + your Temple (no travel)"
     :filter :magistrate-and-my-temple
     :action :sell
     :no-travel true
     :multi true}
    ;; deploy after travel
    ;; [5 3] removed: "Deploy then Temple" needs no city pick (arm auto-resolves
    ;; deploy on an open route + temple in caravan) — was a dead prompt.
    ([27 2] [33 4])
    {:type :pick-city :prompt "Travel to adjacent city and deploy"
     :filter :adjacent :action :deploy}
    ;; temple after travel
    ([27 3] [28 1] [28 2])
    {:type :pick-city :prompt "Travel to adjacent city and place a temple"
     :filter :adjacent :action :temple}
    ;; simple travel (score/action happens at destination automatically)
    ([7 3] [7 4] [18 2] [22 4] [30 1] [32 2] [33 2])
    {:type :pick-city :prompt "Choose a city to travel to"
     :filter :adjacent}
    ;; travel anywhere (from Eridu)
    [21 1] {:type :pick-city :prompt "Travel anywhere (from Eridu)"
            :filter :any}

    ;; ── Temple adjacent to one of your raiders (Board 13 #4) ─────
    ;; Player picks which raider-adjacent city gets the temple; eligible
    ;; cities are computed server-side (cities-adjacent-to-my-raiders).
    [13 4] {:type :pick-city
            :prompt "Place a temple adjacent to one of your raiders"
            :filter :adjacent-to-raider}

    ;; ── Pick magistrate city (temple/sell/influence) ────────────
    ;; temple in magistrate city
    ([2 3] [7 2] [23 4])
    {:type :pick-city :prompt "Choose a magistrate city for your temple"
     :filter :magistrate}
    ;; sell in magistrate city
    ;; [12 3] removed: card sells in the CURRENT city for glory (no city pick);
    ;; arm auto-resolves (increase merchant + glory) — was a dead prompt.
    [9 4]
    {:type :pick-city :prompt "Choose a magistrate city to sell in"
     :filter :magistrate}
    ;; influence magistrate + action
    ;; [35 4] moved here from :pick-role (S4 fix): card is "Influence a Magistrate,
    ;; score raiders it moved through" — the choice is the magistrate destination.
    ([20 3] [25 1] [30 2] [30 4] [32 4] [35 4])
    {:type :pick-city :prompt "Choose magistrate destination"
     :filter :magistrate}
    ;; influence + deploy
    [30 3] {:type :pick-city :prompt "Choose magistrate destination then deploy"
            :filter :magistrate}
    ;; move magistrate across river
    [18 1] {:type :pick-city :prompt "Move magistrate across a river"
            :filter :magistrate}

    ;; Default: auto-resolve
    nil))
