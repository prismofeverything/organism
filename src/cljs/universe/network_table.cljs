(ns universe.network-table
  "The UNIVERSE network table.

   Your hand is laid out as a map of the whole deck, so a card's place says
   what it is: the four shapes run across, the three colors down, and inside
   each box the five numbers left to right. Every card has one home; the
   empty homes are drawn faintly. Lines join the cards that link, and the
   strip above the map lights each color, shape and number your biggest
   linked group already shows.

   A turn: click cards to set aside if you like (the order you click is the
   order they go onto your pile), then click where your first card comes
   from -- the deck or the top of anyone's pile. That draws it and sets the
   chosen cards aside together; you then draw one more for each card set
   aside, and end by choosing one card to discard."
  (:require
   [reagent.core :as r]
   [universe.card :as card]
   [universe.deck :as deck]
   [universe.network :as net]))

(def ground "#12101c")
(def panel  "#1b1828")
(def gold   "#E6AD24")
(def faint  "#6b6480")
(def ink    "#d8d2e8")

(defonce chosen (r/atom []))

(defn- toggle! [c]
  (swap! chosen (fn [v] (if (some #{c} v) (vec (remove #{c} v)) (conj v c)))))

(defonce viewport (r/atom {:w 1440 :h 900}))
(defonce viewport-watch
  (let [m! #(reset! viewport {:w (.-innerWidth js/window) :h (.-innerHeight js/window)})]
    (m!) (.addEventListener js/window "resize" m!) true))

(defn- button [label on-click enabled? & [primary?]]
  [:button {:on-click (when enabled? on-click) :disabled (not enabled?)
            :style {:background (cond (not enabled?) "#221d33" primary? gold :else "#2b2740")
                    :color (cond (not enabled?) faint primary? ground :else ink)
                    :border "none" :border-radius "6px" :padding "9px 18px"
                    :cursor (if enabled? "pointer" "default")
                    :font-family "monospace" :font-size "13px"}}
   label])

;; ── The map ────────────────────────────────────────────────────────────────

(def ^:private slot-gap 8)    ; between the four shapes in a cell
(def ^:private box-gap 26)    ; between cells, across and down

(defn- geometry
  "Where each card's home is, for cards `w` wide. Numbers run across, one
   column each; colors run down; and in each cell the four shapes sit in a
   two-by-two square -- so every link is between neighbours: across to the
   next column, down the column, or within the cell."
  [w]
  (let [h      (* 1.4 w)
        cell-w (+ (* 2 w) slot-gap)
        cell-h (+ (* 2 h) slot-gap)]
    {:w w :h h
     :width  (+ (* 5 cell-w) (* 4 box-gap))
     :height (+ (* 3 cell-h) (* 2 box-gap))
     :pos    (fn [c] (let [s (deck/shape-index c)]
                       [(+ (* (deck/number-index c) (+ cell-w box-gap)) (* (mod s 2) (+ w slot-gap)))
                        (+ (* (deck/color-index c) (+ cell-h box-gap)) (* (quot s 2) (+ h slot-gap)))]))}))

(defn- link-path
  "A straight line from centre to centre. The lines are drawn under the cards,
   so what shows is the stretch across the gap between them."
  [{:keys [w h pos]} a b]
  (let [[ax ay] (pos a) [bx by] (pos b)]
    (str "M" (+ ax (/ w 2)) "," (+ ay (/ h 2)) " L" (+ bx (/ w 2)) "," (+ by (/ h 2)))))

(defn- coverage-strip [group]
  (let [cs (set (map deck/color-index group))
        ss (set (map deck/shape-index group))
        nums (set (map deck/number group))
        pill (fn [k on? label & [colour]]
               ^{:key k}
               [:span {:style {:padding "3px 9px" :border-radius "10px" :font-size "12px"
                               :border (str "1px solid " (if on? (or colour gold) "#322b48"))
                               :color (if on? ink faint)
                               :background (if on? "#2a2340" "transparent")}}
                label])]
    [:div {:style {:display "flex" :gap "6px" :justify-content "center" :flex-wrap "wrap"
                   :margin-bottom "16px"}}
     (for [[i c] (map-indexed vector deck/colors)]
       (pill (str "c" i) (cs i) (name c) (deck/hex c)))
     [:span {:style {:width "10px"}}]
     (for [[i s] (map-indexed vector deck/shapes)]
       (pill (str "s" i) (ss i) (name s)))
     [:span {:style {:width "10px"}}]
     (for [n (range 1 6)]
       (pill (str "n" n) (nums n) (str n)))
     [:span {:style {:color faint :font-size "12px" :margin-left "10px" :align-self "center"}}
      (str (net/coverage group) " of 12 in your biggest linked group")]]))

(defn- hand-map [hand w]
  (let [g      (geometry w)
        {:keys [pos width height]} g
        best   (set (net/best-group hand))
        order  @chosen
        links  (for [[i a] (map-indexed vector hand) b (drop (inc i) hand)
                     :when (net/linked? a b)] [a b])]
    [:div
     [coverage-strip best]
     [:div {:style {:position "relative" :width (str width "px") :height (str height "px")
                    :margin "0 auto"}}
      ;; every home, faint, so the gaps read as gaps
      (for [c deck/all-cards :let [[x y] (pos c)]]
        ^{:key (str "home" c)}
        [:div {:style {:position "absolute" :left (str x "px") :top (str y "px")
                       :width (str w "px") :height (str (* 1.4 w) "px")
                       :border-radius (str (* 0.05 w) "px")
                       :border "1px dashed #2b2740"}}])
      [:svg {:width width :height height
             :style {:position "absolute" :left 0 :top 0 :overflow "visible"
                     :pointer-events "none" :z-index 0}}
       (for [[a b] links]
         ^{:key (str a "-" b)}
         [:path {:d (link-path g a b) :fill "none"
                 :stroke (if (and (best a) (best b)) gold "#6b6480")
                 :stroke-width 3 :stroke-linecap "round" :opacity 0.9}])]
      (for [c hand :let [[x y] (pos c) k (.indexOf (clj->js order) c)]]
        ^{:key c}
        [:div {:on-click #(toggle! c)
               :style {:position "absolute" :left (str x "px")
                       :top (str (- y (if (>= k 0) 8 0)) "px")
                       :z-index 1 :cursor "pointer" :transition "top 120ms"
                       :border-radius (str (* 0.05 w) "px")
                       :box-shadow (when (>= k 0) (str "0 0 0 3px " gold))}}
         [card/card {:value c :width w}]
         (when (>= k 0)
           [:div {:style {:position "absolute" :right "-6px" :top "-8px"
                          :background gold :color ground :border-radius "50%"
                          :width "18px" :height "18px" :font-size "11px" :font-weight 700
                          :display "flex" :align-items "center" :justify-content "center"}}
            (inc k)])])]]))

;; ── The rest of the table ──────────────────────────────────────────────────

(defn- name-of [state seat] (net/player-name state seat))

(defn- source-card
  "A face-up pile's top card, or the deck's back, clickable as a draw when
   one is allowed. `label` says whose it is."
  [state source label send-action! w]
  (let [{:keys [step sources]} (:actions state)
        live?  (and (#{:start :drawing} step) (some #{source} sources))
        c      (net/top state source)
        empty? (and (not= :deck source) (nil? c))]
    [:div {:style {:text-align "center"}}
     [:div {:on-click (when live?
                        (fn []
                          (send-action! (cond-> {:action :draw :from source}
                                          (and (= step :start) (seq @chosen))
                                          (assoc :set-aside @chosen)))
                          (when (= step :start) (reset! chosen []))))
            :style {:cursor (if live? "pointer" "default") :border-radius "6px"
                    :box-shadow (when live? (str "0 0 0 2px " gold))
                    :opacity (if empty? 0.3 1)}}
      [card/card {:value c :width w :dim? empty?}]]
     [:div {:style {:color faint :font-size "11px" :margin-top "5px"}} label]]))

(defn- opponent [state {:keys [seat name]} send-action!]
  (let [acting? (= seat (:to-act state))
        pile    (get-in state [:piles seat])
        shown   (get-in state [:hands seat])]
    [:div {:style {:background (if acting? "#3a2f10" panel)
                   :border (str "1px solid " (if acting? gold "#322b48"))
                   :border-radius "8px" :padding "10px 12px" :min-width "150px"
                   :display "flex" :gap "12px" :align-items "center"}}
     [source-card state [:pile seat] (str (count pile) " discarded") send-action! 44]
     [:div
      [:div {:style {:color (if acting? gold ink) :font-size "14px"}} name]
      [:div {:style {:color faint :font-size "12px" :margin-top "4px"}}
       "score " (get-in state [:scores seat])
       (when acting?
         [:div {:style {:color gold}}
          (case (:step state)
            :start "starting…" :drawing (str "drawing, " (:draws-left state) " left…")
            :discard "discarding…" "")])]
      (when shown
        [:div {:style {:display "flex" :gap "2px" :margin-top "6px" :flex-wrap "wrap"}}
         (for [c (sort shown)] ^{:key c} [card/card {:value c :width 16}])])]]))

(defn- turn-bar [state send-action!]
  (let [you    (:you state)
        {:keys [step draws-left] :as acts} (:actions state)
        picked @chosen]
    [:div {:style {:display "flex" :gap "26px" :justify-content "center" :align-items "flex-end"
                   :margin "8px 0 22px"}}
     [source-card state :deck (str "deck · " (:deck-count state)) send-action! 64]
     [source-card state [:pile you] (str "your pile · " (count (get-in state [:piles you])))
      send-action! 64]
     (when (net/solo? state)
       [source-card state :flip "turned up" send-action! 64])
     (when (seq (:aside state))
       [:div {:style {:text-align "center"}}
        [:div {:style {:display "flex" :gap "3px"}}
         (for [[i c] (map-indexed vector (:aside state))]
           ^{:key i} [card/card {:value c :width 40}])]
        [:div {:style {:color faint :font-size "11px" :margin-top "5px"}} "set aside"]])
     [:div {:style {:text-align "center" :max-width "240px" :min-height "60px"}}
      [:div {:style {:color faint :font-size "12px" :margin-bottom "8px" :line-height "17px"}}
       (case (when acts step)
         :start   (if (seq picked)
                    (str (count picked) " to set aside — now click where your first card comes from ("
                         (inc (count picked)) " draws)")
                    "your turn — click the deck or any pile to draw; click your cards first to set some aside for extra draws")
         :drawing (str draws-left " more to draw — click the deck or any pile")
         :discard (if (= 1 (count picked)) "discard it to end your turn"
                      "select one card to discard")
         "")]
      (when (= step :discard)
        [button "discard"
         (fn [] (send-action! {:action :discard :card (first picked)}) (reset! chosen []))
         (and acts (= 1 (count picked))) true])]]))

(defn- log-line [state {:keys [event seat card from count why each]}]
  (let [who (name-of state seat)]
    (case event
      :start     "ten cards each"
      :set-aside (str who " sets " count " aside")
      :draw      (str who " draws " (cond (= from :deck) "from the deck"
                                          (= from :flip) (str "the turned-up " (deck/describe card))
                                          :else (str (deck/describe card) " from "
                                                     (if (= (second from) seat) "their own pile"
                                                         (str (name-of state (second from)) "'s pile")))))
      :discard   (str who " discards " (deck/describe card))
      :network   (str who " makes a network!")
      :run-out   (str "the deck runs out: everyone gives back " each)
      :over      (str "game over — " (case why
                                       :network "a network is made"
                                       :run-out-limit "the deck ran out for the third time"
                                       :no-discards "the deck ran out with someone at no discards"
                                       (name why)))
      nil)))

(defn- rail [state chat]
  [:div {:style {:width "300px" :flex-shrink 0 :padding "16px" :box-sizing "border-box"
                 :background "#15121f" :border-left "1px solid #2b2740"
                 :height "100vh" :display "flex" :flex-direction "column" :gap "14px"}}
   [:div
    [:h2 {:style {:color gold :margin "0 0 6px 0" :letter-spacing "3px" :font-size "20px"}}
     "UNIVERSE"]
    [:div {:style {:color faint :font-size "12px" :line-height "18px"}}
     "network · ten cards linking all 3 colors, 4 shapes, 5 numbers" [:br]
     "linked: one thing differs — a number by one" [:br]
     "first network ends it · score: your pile + 3 per value your best group lacks · lowest wins"]]
   [:div {:style {:background panel :border-radius "6px" :padding "8px" :font-size "13px"}}
    (for [{:keys [seat name]} (:players state)]
      ^{:key seat}
      [:div {:style {:display "flex" :justify-content "space-between"
                     :color (if (= seat (:you state)) gold ink)}}
       [:span name]
       [:span {:style {:color faint}}
        (str (count (get-in state [:piles seat])) " + 3×" (net/missing state seat) " = ")
        [:span {:style {:color ink}} (get-in state [:scores seat])]]])]
   [:div {:style {:flex "1 1 auto" :overflow-y "auto" :background panel :border-radius "6px"
                  :padding "8px" :font-size "12px" :line-height "18px"
                  :display "flex" :flex-direction "column-reverse"}}
    [:div
     (for [[i line] (map-indexed vector (keep #(log-line state %) (:log state)))]
       ^{:key i} [:div {:style {:color "#b6afc9"}} line])]]
   [:div [:div {:style {:color faint :font-size "12px" :margin-bottom "6px"}} "chat"] chat]])

(defn table
  "The whole network table. `send-action!`, `send-start!` and the `chat`
   component come from play.cljs."
  [state {:keys [send-action! send-start! chat]}]
  (let [you  (:you state)
        hand (vec (sort (get-in state [:hands you])))
        ;; ten card-widths across and 8.4 card-heights' worth down, in what the
        ;; rail, the opponents and the turn bar leave
        {vw :w vh :h} @viewport
        by-w (/ (- vw 300 48 (* 4 box-gap) (* 5 slot-gap)) 10)
        by-h (/ (- vh 400 (* 2 box-gap) (* 3 slot-gap)) 8.4)
        w    (max 26 (min 90 by-w by-h))]
    [:div {:style {:background ground :min-height "100vh" :color ink
                   :font-family "monospace" :display "flex"}}
     [:div {:style {:flex "1 1 auto" :min-width 0 :padding "20px 24px"}}
      [:div {:style {:display "flex" :gap "12px" :justify-content "center" :flex-wrap "wrap"}}
       (for [p (:players state) :when (not= (:seat p) you)]
         ^{:key (:seat p)} [opponent state p send-action!])]
      (cond
        (= :waiting (:phase state))
        [:div {:style {:text-align "center" :margin "70px 0"}}
         [:div {:style {:color faint :margin-bottom "16px"}}
          "Ten cards each. Link every color, shape and number."]
         [button "deal" send-start! true true]]

        :else
        [:div
         (when (:winner state)
           [:div {:style {:text-align "center" :margin "16px 0" :font-size "22px" :color gold}}
            (:winner state) " wins with the lowest score"
            [:div {:style {:margin-top "10px" :font-size "14px"}}
             [:a {:href "/universe/create" :style {:color faint}} "new table"]]])
         [:div {:style {:margin-top "18px"}} [turn-bar state send-action!]]
         (when you [hand-map hand w])])]
     [rail state chat]]))
