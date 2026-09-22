(ns universe.play
  "The UNIVERSE hold'em table.

   Globals set by the HTML shell, the same way every other game here does it:

     js/playKey      — /universe/play/:play   a table (WebSocket)
     js/playerKey    — who you are, or --observer--
     js/isCreate     — /universe/create
     js/isObserve    — /universe/observe
     js/isRules      — /universe/rules

   The state that arrives has already been cut down for you by the server, so
   there is nothing here that has to remember not to draw somebody else's
   cards: the cards are simply absent."
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.ajax :as ajax]
   [organism.components :as components]
   [organism.websockets :as ws]
   [universe.card :as card]
   [universe.deck :as deck]))

(defonce game-state (r/atom nil))
(defonce player-key (r/atom nil))
(defonce chat-lines (r/atom []))
(defonce deadline   (r/atom nil))
(defonce now-atom   (r/atom 0))
(defonce raise-to   (r/atom nil))

(def ground "#12101c")
(def panel  "#1b1828")
(def gold   "#E6AD24")
(def faint  "#6b6480")

(defn- safe-read [s]
  (when (and s (not (str/blank? s)))
    (try (reader/read-string s) (catch :default _ nil))))

;; ── Connection ─────────────────────────────────────────────────────────────

(defn- receive-message! [message]
  (let [kind (get message "type")]
    (case kind
      ("initialize" "game-state")
      (do
        (when-let [st (safe-read (get message "state"))]
          (let [was (:to-act @game-state)]
            (reset! game-state st)
            ;; a fresh decision wants a fresh default bet size
            (when (not= was (:to-act st)) (reset! raise-to nil))))
        (when-let [c (safe-read (get message "chat"))] (reset! chat-lines c))
        (reset! deadline (some-> (get message "deadline") js/parseInt)))
      (js/console.log "universe: unknown message" kind))))

(defn connect-ws! [pk]
  (let [proto (if (= "https:" (.-protocol js/location)) "wss:" "ws:")
        url   (str proto "//" (.-host js/location) "/ws/universe/play/" pk)]
    (ws/make-websocket! url receive-message!)))

(defn- send-action! [action]
  (ws/send-transit-message! {"type" "action" "choice" (pr-str action)}))

(defn- send-start! [] (ws/send-transit-message! {"type" "start"}))

(defn- send-chat! [line]
  (ws/send-transit-message! {"type" "chat" "line" line}))

;; ── Reading the table ──────────────────────────────────────────────────────

(defn- your-hand
  "Your five cards, once all three board cards are out."
  [state]
  (let [you (:you state)
        hole (get-in state [:hands you])
        board (:board state)]
    (when (and hole (= 3 (count board)))
      (concat hole board))))

;; ── Pieces of the table ────────────────────────────────────────────────────

(defn- chips [n]
  [:span {:style {:color gold :font-variant-numeric "tabular-nums"}} (str n)])

(defn- pot-total
  "The pot empties into the stacks the moment a hand is paid out, so at the
   showdown read what the layers held instead -- otherwise the table reads
   \"pot 0\" exactly when everyone is looking at it."
  [state]
  (if (pos? (:pot state 0))
    (:pot state)
    (reduce + 0 (map :amount (get-in state [:result :pots])))))

(defn- result-view
  "Who took it, and with what."
  [state]
  (when-let [awards (seq (get-in state [:result :awards]))]
    [:div {:style {:padding "10px 0" :font-size "15px"}}
     (for [[seat amount] awards]
       ^{:key seat}
       [:div {:style {:color gold}}
        (get-in state [:players seat :name])
        " takes " amount
        (when-let [h (get-in state [:result :hands seat :hand])]
          (str " with a " (deck/hand-name h)))])]))

(defn- ring-position
  "Seats evenly spaced around the oval, starting at the top and running
   clockwise -- the same convention the deck's own rosettes are packed on.
   The centre sits a little below the middle so the tall seat at the top,
   which is yours, has somewhere to be."
  [i n]
  (let [t (* 2 js/Math.PI (/ i n))]
    {:left (str (+ 50 (* 40 (js/Math.sin t))) "%")
     :top  (str (- 52 (* 36 (js/Math.cos t))) "%")}))

(defn- ring-order
  "Players in seat order, rotated so that you are first and therefore on top.
   An observer, who is nobody, gets the table as it was dealt."
  [state]
  (let [ps    (vec (:players state))
        n     (count ps)
        start (or (:you state) 0)]
    (vec (for [k (range n)] (nth ps (mod (+ start k) n))))))

(defn- seat-view
  "One player around the rim. Yours draws its cards large, since it is the one
   hand you actually have to read."
  [state {:keys [seat name stack]} pos you?]
  (let [folded?   (contains? (:folded state) seat)
        all-in?   (contains? (:all-in state) seat)
        acting?   (= seat (:to-act state))
        cards     (get-in state [:hands seat])
        dealt?    (contains? (:seated state) seat)
        bet       (get-in state [:bets seat] 0)
        shown     (get-in state [:result :hands seat])
        width     (if you? 104 46)
        remaining (when (and acting? @deadline (pos? @deadline))
                    (max 0 (int (/ (- @deadline @now-atom) 1000))))]
    [:div {:style (merge pos
                         {:position "absolute" :transform "translate(-50%,-50%)"
                          :text-align "center" :opacity (if folded? 0.38 1)
                          :transition "opacity 250ms"})}
     [:div {:style {:display "flex" :justify-content "center" :gap "5px"
                    :margin-bottom "7px"
                    ;; reserved whether or not there are cards, so the
                    ;; nameplates stay put as hands come and go
                    :min-height (str (js/Math.round (* width 1.4)) "px")}}
      (when (and dealt? (not folded?))
        (for [[i c] (map-indexed vector (or cards [nil nil]))]
          ^{:key i} [card/card {:value c :width width}]))]
     [:div {:style {:display "inline-block" :padding "5px 12px" :border-radius "14px"
                    :background (if acting? "#3a2f10" "#221d33")
                    :border (str "1px solid " (if acting? gold "#322b48"))
                    :white-space "nowrap" :font-size (if you? "15px" "13px")}}
      [:span {:style {:color (if you? gold "#d8d2e8") :font-weight (if you? 700 400)}} name]
      [:span {:style {:color faint :margin "0 7px"}} "\u00b7"]
      [chips stack]
      (when (= seat (:button state))
        [:span {:style {:margin-left "8px" :background gold :color ground
                        :border-radius "50%" :padding "1px 6px" :font-size "11px"
                        :font-weight 700}} "D"])]
     [:div {:style {:font-size "12px" :color (if all-in? gold faint)
                    :margin-top "5px" :height "16px"}}
      (cond all-in?          "all in"
            remaining        (str remaining "s")
            (and dealt? folded?) "folded"
            :else            "")]
     (when (pos? bet)
       [:div {:style {:margin-top "1px"}} [chips bet]])
     (when-let [h (:hand shown)]
       [:div {:style {:color gold :font-size "12px" :margin-top "3px"}}
        (deck/hand-name h)])]))

(defn- oval-table
  "The table itself, dressed like the back of a card: a violet-black ground
   with a thin gold ring, which is the one piece of the deck that is already
   about being looked at from the outside."
  [state]
  (let [order (ring-order state)
        n     (count order)]
    [:div {:style {:position "relative" :width "100%" :max-width "1060px"
                   :height "700px" :margin "0 auto"}}
     [:div {:style {:position "absolute" :left "5%" :top "9%"
                    :width "90%" :height "86%" :border-radius "50%"
                    :background "radial-gradient(ellipse at 50% 42%, #221c34 0%, #191324 72%)"
                    :border "1px solid #2e2743"
                    :box-shadow "inset 0 0 90px rgba(0,0,0,0.6)"}}]
     [:div {:style {:position "absolute" :left "11%" :top "16%"
                    :width "78%" :height "72%" :border-radius "50%"
                    :border (str "1px solid " gold) :opacity 0.26
                    :pointer-events "none"}}]
     [:div {:style {:position "absolute" :left "50%" :top "52%"
                    :transform "translate(-50%,-50%)" :text-align "center"}}
      [:div {:style {:display "flex" :gap "9px" :justify-content "center"}}
       (for [i (range 3)]
         ^{:key i} [card/card {:value (nth (:board state) i nil) :width 92}])]
      [:div {:style {:color faint :font-size "15px" :margin-top "14px"}}
       "pot " [chips (pot-total state)]]
      [result-view state]]
     (for [[i p] (map-indexed vector order)]
       ^{:key (:seat p)}
       [seat-view state p (ring-position i n) (= (:seat p) (:you state))])]))

(defn- action-bar [state]
  (let [{:keys [check call min-raise-to max-raise-to]} (:actions state)
        btn (fn [label on-click & [accent?]]
              [:button {:on-click on-click
                        :style {:background (if accent? gold "#2b2740")
                                :color (if accent? "#12101c" "#d8d2e8")
                                :border "none" :border-radius "6px"
                                :padding "10px 20px" :cursor "pointer"
                                :font-family "monospace" :font-size "14px"}}
               label])]
    (when (:actions state)
      [:div {:style {:display "flex" :gap "10px" :align-items "center"
                     :padding "14px" :background panel :border-radius "8px"}}
       [btn "fold" #(send-action! {:action :fold})]
       (if check
         [btn "check" #(send-action! {:action :check})]
         [btn (str "call " call) #(send-action! {:action :call})])
       (when min-raise-to
         (let [to (or @raise-to min-raise-to)]
           [:<>
            [:input {:type "range" :min min-raise-to :max max-raise-to :value to
                     :on-change #(reset! raise-to (js/parseInt (.. % -target -value)))
                     :style {:width "160px"}}]
            [:input {:type "number" :min min-raise-to :max max-raise-to :value to
                     :on-change #(reset! raise-to (js/parseInt (.. % -target -value)))
                     :style {:width "80px" :background "#12101c" :color gold
                             :border "1px solid #2b2740" :border-radius "4px"
                             :padding "8px" :font-family "monospace"}}]
            [btn (if (= to max-raise-to) (str "all in " to) (str "raise to " to))
                 #(send-action! {:action :raise :to to})
                 true]]))])))

(defn- your-hand-view [state]
  (when-let [five (your-hand state)]
    (let [row (deck/classify five)]
      [:div {:style {:color gold :font-size "15px" :padding "6px 0"}}
       "you have a " (deck/hand-name row)
       [:span {:style {:color faint :margin-left "10px" :font-size "13px"}}
        (str "1 in " (js/Math.round (/ deck/total-hands (:count row))))]])))

(defn- chat-view []
  (let [draft (r/atom "")]
    (fn []
      [:div {:style {:display "flex" :flex-direction "column" :gap "6px"}}
       [:div {:style {:height "150px" :overflow-y "auto" :background panel
                      :border-radius "6px" :padding "8px" :font-size "13px"}}
        (for [[i {:keys [player line]}] (map-indexed vector @chat-lines)]
          ^{:key i}
          [:div [:span {:style {:color gold}} player ": "]
           [:span {:style {:color "#d8d2e8"}} line]])]
       [:input {:type "text" :value @draft
                :placeholder "say something"
                :on-change #(reset! draft (.. % -target -value))
                :on-key-down #(when (= "Enter" (.-key %))
                                (send-chat! @draft)
                                (reset! draft ""))
                :style {:background "#12101c" :color "#d8d2e8" :padding "8px"
                        :border "1px solid #2b2740" :border-radius "4px"
                        :font-family "monospace"}}]])))

;; ── Views ──────────────────────────────────────────────────────────────────

(defn table-view []
  (let [state @game-state]
    [:div {:style {:background ground :min-height "100vh" :color "#d8d2e8"
                   :font-family "monospace" :padding "24px"}}
     [:div {:style {:display "flex" :justify-content "space-between"
                    :align-items "baseline" :margin-bottom "16px"}}
      [:h2 {:style {:color gold :margin 0 :letter-spacing "3px"}} "UNIVERSE"]
      (when state
        [:span {:style {:color faint :font-size "13px"}}
         (str "hand " (:hand-number state)
              " · blinds " (str/join "/" (nth (:levels state)
                                              (min (:level state)
                                                   (dec (count (:levels state))))
                                              ["" ""])))])]
     (cond
       (nil? state)
       [:div {:style {:color faint}} "connecting…"]

       (:winner state)
       [:div {:style {:padding "40px" :text-align "center"}}
        [:h3 {:style {:color gold}} (str (:winner state) " takes the table")]]

       :else
       [:div
        [oval-table state]
        [:div {:style {:max-width "1060px" :margin "0 auto" :display "flex"
                       :flex-direction "column" :align-items "center" :gap "12px"}}
         [your-hand-view state]
         (if (= :waiting (:street state))
           [:button {:on-click send-start!
                     :style {:background gold :color ground :border "none"
                             :border-radius "6px" :padding "12px 28px"
                             :cursor "pointer" :font-family "monospace"
                             :font-size "15px" :letter-spacing "2px"}}
            "deal"]
           [action-bar state])]
        [:div {:style {:margin "28px auto 0" :max-width "440px"}} [chat-view]]])]))

(defn create-view []
  [components/create-lobby
   {:game-type      "universe"
    :title          "UNIVERSE — New Table"
    :current-player (when (and (exists? js/playerKey)
                               (not (str/blank? js/playerKey))
                               (not= "--observer--" js/playerKey))
                      js/playerKey)
    :min-players    2
    :max-players    9
    :accent         gold
    :slot-bg        panel
    :background     ground}])

(defn observe-view []
  (let [games (safe-read (when (exists? js/observeGames) js/observeGames))]
    [:div {:style {:padding "32px" :background ground :color "#d8d2e8"
                   :font-family "monospace" :min-height "100vh"}}
     [:h2 {:style {:color gold}} "UNIVERSE — tables"]
     (if (seq games)
       (for [g games]
         ^{:key (:key g)}
         [:div {:style {:padding "10px 0" :border-bottom "1px solid #2b2740"}}
          [:a {:href (str "/universe/play/" (:key g)) :style {:color gold}} (:key g)]
          [:span {:style {:color faint :margin-left "12px"}}
           (str (str/join ", " (:players g)) " · hand " (:hand g))]])
       [:div {:style {:color faint}} "no tables running"])]))

(defn rules-view []
  (let [chart (safe-read (when (exists? js/chart) js/chart))]
    [:div {:style {:padding "32px" :background ground :color "#d8d2e8"
                   :font-family "monospace" :min-height "100vh" :max-width "760px"}}
     [:h2 {:style {:color gold :letter-spacing "3px"}} "UNIVERSE HOLD'EM"]
     [:p "Two cards to you, three to the table, turned one at a time. Your hand
          is your two and all three — five cards, never a choice of which five,
          which is why every hand you are dealt is one of the nineteen below at
          exactly the rarity printed against it."]
     [:p {:style {:color faint}}
      "Betting after the hole cards and after each board card. Equal hands are
       settled by the numbers — groups first, five high — and then by color,
       purple over green over yellow. Shape never breaks a tie: the deck orders
       its three colors by lightness and gives its four marks no order at all."]
     [:table {:style {:width "100%" :border-collapse "collapse" :margin-top "20px"}}
      [:tbody
       (for [row chart]
         ^{:key (:name row)}
         [:tr {:style {:border-bottom "1px solid #221f33"}}
          [:td {:style {:padding "6px 0" :color gold}} (:label row)]
          [:td {:style {:padding "6px 0" :color faint :text-align "right"}}
           (str "1 in " (:odds row))]])]]]))

(defn games-view []
  (let [games (safe-read (when (exists? js/playerGames) js/playerGames))]
    [:div {:style {:padding "32px" :background ground :color "#d8d2e8"
                   :font-family "monospace" :min-height "100vh"}}
     [:h2 {:style {:color gold}} "your tables"]
     (for [g (concat (:active games) (:complete games) (when (sequential? games) games))]
       ^{:key (:game g)}
       [:div {:style {:padding "8px 0"}}
        [:a {:href (str "/universe/play/" (:game g)) :style {:color gold}} (:game g)]])]))

(defn page []
  (cond
    (and (exists? js/isCreate) js/isCreate)   [create-view]
    (and (exists? js/isObserve) js/isObserve) [observe-view]
    (and (exists? js/isRules) js/isRules)     [rules-view]
    (and (exists? js/isGames) js/isGames)     [games-view]
    (and (exists? js/playKey) (not (str/blank? js/playKey))) [table-view]
    :else [:div {:style {:padding "48px" :color faint :background ground
                         :font-family "monospace" :min-height "100vh"}}
           "Loading universe…"]))

(defn mount-components []
  (card/load!)
  (when-let [el (.getElementById js/document "universe")]
    (rdom/render [page] el))
  (let [pk (when (exists? js/playKey) js/playKey)]
    (when (and pk (not (str/blank? pk))
               (not (and (exists? js/isCreate) js/isCreate)))
      (reset! player-key (when (exists? js/playerKey) js/playerKey))
      (connect-ws! pk))))

(defonce clock-ticker
  ;; one timer for the whole page, so the countdown on the seat that is acting
  ;; moves without every state message having to carry it
  (js/setInterval #(reset! now-atom (.now js/Date)) 500))

(defn init! []
  (ajax/load-interceptors!)
  (mount-components))
