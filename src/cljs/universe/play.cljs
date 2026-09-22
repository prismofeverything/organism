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
   [universe.deck :as deck]
   [universe.holdem :as holdem]
   [universe.layout :as layout]))

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

(defn- said-hand
  "A revealed hand, said with the numbers that make it -- \"dyad of 2s\".
   Nil until there is a showdown to reveal anything."
  [state seat]
  (let [shown (get-in state [:result :hands seat])]
    (when (and (:hand shown) (= 3 (count (:board state))))
      (deck/describe-hand (concat (:cards shown) (:board state))))))

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
        (when-let [said (said-hand state seat)]
          (str " with a " said))])]))

(defonce viewport (r/atom nil))

(defn watch-viewport!
  "The layout is solved from the space it has, so the space has to be known."
  []
  (let [measure! #(reset! viewport {:w (.-innerWidth js/window)
                                    :h (.-innerHeight js/window)})]
    (measure!)
    (.addEventListener js/window "resize" measure!)))

(defonce ^:private viewport-watch (watch-viewport!))

(defonce client-errors (r/atom []))

(defn watch-errors!
  "Surface client-side failures on the page. A websocket game that throws in a
   render leaves a table that looks right and does nothing, which is
   indistinguishable from a rules bug unless the error is put somewhere
   visible."
  []
  (.addEventListener js/window "error"
                     (fn [e] (swap! client-errors conj
                                    (str (.-message e) " @ " (.-filename e) ":" (.-lineno e)))))
  (.addEventListener js/window "unhandledrejection"
                     (fn [e] (swap! client-errors conj (str "promise: " (.-reason e))))))

(defonce ^:private error-watch (watch-errors!))

(defonce ^:private solve-cache (atom {}))

(defn- solved
  "The solved layout for this window and this many players. Cached: it is the
   same answer until one of those changes, and it is asked for on every state
   message."
  [n]
  (let [{:keys [w h]} (or @viewport {:w 1440 :h 900})
        area (layout/table-area w h)
        ;; quantised, so dragging a window edge does not re-solve on every
        ;; pixel and fill the cache with near-identical answers
        qw   (* 20 (quot (:w area) 20))
        qh   (* 20 (quot (:h area) 20))
        k    [qw qh n]]
    (or (get @solve-cache k)
        (let [v (layout/solve qw qh n)]
          (swap! solve-cache assoc k v)
          v))))

(defn- ring-order
  "Players in seat order, rotated so that you are first and therefore on top.
   An observer, who is nobody, gets the table as it was dealt."
  [state]
  (let [ps    (vec (:players state))
        n     (count ps)
        start (or (:you state) 0)]
    (vec (for [k (range n)] (nth ps (mod (+ start k) n))))))

(defn- seat-view
  "One player, placed in the rectangle the solver worked out for them."
  [state {:keys [seat name stack]} rect you?]
  (let [folded?   (contains? (:folded state) seat)
        all-in?   (contains? (:all-in state) seat)
        acting?   (= seat (:to-act state))
        cards     (get-in state [:hands seat])
        dealt?    (contains? (:seated state) seat)
        bet       (get-in state [:bets seat] 0)
        said      (said-hand state seat)
        width     (:card rect)
        remaining (when (and acting? @deadline (pos? @deadline))
                    (max 0 (int (/ (- @deadline @now-atom) 1000))))
        cards-el
        [:div {:style {:display "flex" :justify-content "center"
                       :gap (if you? "8px" "5px")
                       :height (str (js/Math.round (* 1.4 width)) "px")}}
         (when (and dealt? (not folded?))
           (for [[i c] (map-indexed vector (or cards [nil nil]))]
             ^{:key i} [card/card {:value c :width width}]))]
        plate-el
        [:div {:style {:height (str layout/plate-h "px") :display "flex"
                       :align-items "center" :justify-content "center"}}
         [:span {:style {:padding "4px 12px" :border-radius "14px"
                         :background (if acting? "#3a2f10" "#221d33")
                         :border (str "1px solid " (if acting? gold "#322b48"))
                         :white-space "nowrap" :font-size (if you? "15px" "13px")}}
          [:span {:style {:color (if you? gold "#d8d2e8")
                          :font-weight (if you? 700 400)}} name]
          [:span {:style {:color faint :margin "0 7px"}} "\u00b7"]
          [chips stack]
          (when (= seat (:button state))
            [:span {:style {:margin-left "8px" :background gold :color ground
                            :border-radius "50%" :padding "1px 6px"
                            :font-size "11px" :font-weight 700}} "D"])]]
        tail-el
        [:div {:style {:height (str layout/tail "px") :font-size "12px"
                       :line-height "18px"}}
         [:div {:style {:color (if all-in? gold faint)}}
          (cond all-in?              "all in"
                remaining            (str remaining "s")
                (and dealt? folded?) "folded"
                :else                "")]
         [:div (when (pos? bet) [chips bet])]
         [:div {:style {:color gold :overflow "hidden" :text-overflow "ellipsis"}}
          (or said "")]]]
    (into [:div {:style {:position "absolute"
                         :left   (str (:left rect) "px")
                         :top    (str (:top rect) "px")
                         :width  (str (:width rect) "px")
                         :text-align "center"
                         :opacity (if folded? 0.4 1)
                         :transition "opacity 250ms"}}]
          (if (:below? rect)
            [plate-el [:div {:style {:height (str layout/card-gap "px")}}] cards-el tail-el]
            [cards-el [:div {:style {:height (str layout/card-gap "px")}}] plate-el tail-el]))))

(defn- oval-table
  "The table, dressed like the back of a card: a violet-black ground with a
   thin gold ring, which is the one part of the deck already about being seen
   from the outside. The seats sit on its rim, where the solver put them."
  [state]
  (let [order (ring-order state)
        n     (count order)
        {:keys [cx cy rx ry board height seats] :as l} (solved n)]
    (when (seq seats)
      (into
       [:div {:style {:position "relative" :width "100%"
                      :height (str (js/Math.round height) "px")}}
        ;; the felt, with the nameplates riding its rim
        [:div {:style {:position "absolute"
                       :left (str (- cx rx) "px") :top (str (- cy ry) "px")
                       :width (str (* 2 rx) "px") :height (str (* 2 ry) "px")
                       :border-radius "50%"
                       :background "radial-gradient(ellipse at 50% 42%, #221c34 0%, #191324 74%)"
                       :border "1px solid #2e2743"
                       :box-shadow "inset 0 0 90px rgba(0,0,0,0.6)"}}]
        [:div {:style {:position "absolute"
                       :left (str (- cx (* rx 0.72)) "px")
                       :top  (str (- cy (* ry 0.72)) "px")
                       :width (str (* 2 rx 0.72) "px")
                       :height (str (* 2 ry 0.72) "px")
                       :border-radius "50%"
                       :border (str "1px solid " gold) :opacity 0.22
                       :pointer-events "none"}}]
        ;; the shared cards, in the middle
        [:div {:style {:position "absolute"
                       :left (str (:left board) "px") :top (str (:top board) "px")
                       :width (str (- (:right board) (:left board)) "px")
                       :text-align "center"}}
         [:div {:style {:display "flex" :gap "9px" :justify-content "center"}}
          (for [i (range 3)]
            ^{:key i} [card/card {:value (nth (:board state) i nil)
                                  :width (:board (:sizes l))}])]
         [:div {:style {:color faint :font-size "15px" :margin-top "12px"}}
          "pot " [chips (pot-total state)]]
         [result-view state]]]
       (map (fn [p rect] ^{:key (:seat p)}
              [seat-view state p rect (= (:seat p) (:you state))])
            order seats)))))

;; ── The rail ───────────────────────────────────────────────────────────────

(declare chat-view)

(defn- log-line [state {:keys [event seat to amount card]}]
  (let [who (get-in state [:players seat :name])]
    (case event
      :hand-start "\u2014 new hand \u2014"
      :fold       (str who " folds")
      :check      (str who " checks")
      :call       (str who " calls " amount)
      :raise      (str who " raises to " to)
      :board      (str "board: " (deck/describe card))
      :returned   (str who " takes back " amount)
      :refund     (str who " takes back " amount)
      nil)))

(defn- status-rail [state]
  (let [level (nth (:levels state)
                   (min (:level state 0) (dec (count (:levels state))))
                   nil)]
    [:div {:style {:width (str layout/rail-width "px") :flex-shrink 0
                   :display "flex" :flex-direction "column" :gap "14px"
                   :padding "16px" :box-sizing "border-box"
                   :background "#15121f" :border-left "1px solid #2b2740"
                   :height "100vh" :overflow-y "auto"}}
     [:div
      [:h2 {:style {:color gold :margin "0 0 10px 0" :letter-spacing "3px"
                    :font-size "20px"}} "UNIVERSE"]
      [:div {:style {:color faint :font-size "13px" :line-height "20px"}}
       [:div "hand " (:hand-number state)]
       (when level [:div "blinds " (str/join "/" level)])
       [:div "pot " [chips (pot-total state)]]
       [:a {:href "/universe/rules" :style {:color gold}} "the chart"]]]
     [:div
      [:div {:style {:color faint :font-size "12px" :margin-bottom "6px"}} "this hand"]
      [:div {:style {:background "#1b1828" :border-radius "6px" :padding "8px"
                     :height "180px" :overflow-y "auto" :font-size "12px"
                     :line-height "18px"}}
       (for [[i line] (map-indexed vector (keep #(log-line state %) (:log state)))]
         ^{:key i} [:div {:style {:color (if (str/starts-with? line "\u2014")
                                           faint "#b6afc9")}} line])]]
     (when (seq @client-errors)
       [:div {:style {:background "#3a1414" :border "1px solid #7a2a2a"
                      :border-radius "6px" :padding "8px" :font-size "11px"
                      :color "#ffb4b4" :line-height "16px"}}
        [:div {:style {:font-weight 700 :margin-bottom "4px"}} "client error"]
        (for [[i m] (map-indexed vector (take-last 3 @client-errors))]
          ^{:key i} [:div m])])
     [:div {:style {:flex 1 :display "flex" :flex-direction "column" :min-height "200px"}}
      [:div {:style {:color faint :font-size "12px" :margin-bottom "6px"}} "chat"]
      [chat-view]]]))

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
       "you have a " (deck/describe-hand five)
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

;; ── The summary, once somebody has all the chips ───────────────────────────

(def series-colors
  "Categorical slots, in fixed order, never cycled. These are the dataviz
   reference palette's dark steps, validated against this page's own surface
   (#12101c) rather than assumed: worst adjacent CVD deltaE 8.4, worst
   normal-vision 19.3, all eight at or above 3:1 contrast.

   Eight slots is the whole palette. A ninth player does not get an invented
   colour -- the chart becomes small multiples instead."
  ["#3987e5" "#d95926" "#199e70" "#c98500"
   "#d55181" "#008300" "#9085e9" "#e66767"])

(def ^:private ink-primary "#e6e2f0")
(def ^:private ink-second  "#b6afc9")
(def ^:private grid-ink    "#2b2740")

(defonce ^:private hover-hand (r/atom nil))

(defn- nice-ticks
  "Four to eight round numbers covering 0..top -- 1/2/5 x a power of ten, which
   is what reads as round. Doubling from 100 overshoots and leaves three ticks
   on a four-thousand-chip table."
  [top]
  (let [raw  (/ (double top) 5)
        mag  (js/Math.pow 10 (js/Math.floor (js/Math.log10 (max raw 1))))
        step (first (filter #(>= % raw) (map #(* % mag) [1 2 5 10])))]
    (vec (take-while #(<= % top) (iterate #(+ % step) 0)))))

(defn- chips-chart
  "Every player's stack after every hand. The story is who crossed whom, and
   when each of them went out."
  [{:keys [series hands]}]
  (let [w 900 h 340 ml 62 mr 158 mt 18 mb 40
        pw (- w ml mr) ph (- h mt mb)
        top (reduce + 0 (map :final series))
        xat (fn [i] (+ ml (* pw (/ (double i) (max 1 hands)))))
        yat (fn [v] (+ mt (* ph (- 1 (/ (double v) (max 1 top))))))
        ticks (nice-ticks top)]
    [:div {:style {:position "relative"}}
     [:svg {:viewBox (str "0 0 " w " " h) :width "100%"
            :style {:display "block"}
            :on-mouse-leave #(reset! hover-hand nil)
            :on-mouse-move
            (fn [e]
              (let [r    (.getBoundingClientRect (.-currentTarget e))
                    frac (/ (- (.-clientX e) (.-left r)) (.-width r))
                    sx   (* frac w)
                    i    (js/Math.round (* hands (/ (- sx ml) pw)))]
                (reset! hover-hand (max 0 (min hands i)))))}
      ;; recessive grid: hairline, solid, one step off the surface
      (for [t ticks]
        ^{:key t}
        [:g [:line {:x1 ml :y1 (yat t) :x2 (+ ml pw) :y2 (yat t)
                    :stroke grid-ink :stroke-width 1}]
         [:text {:x (- ml 10) :y (+ (yat t) 4) :text-anchor "end"
                 :fill ink-second :font-size 12 :font-family "monospace"} t]])
      ;; which hand, so "who went out when" reads off the axis
      (for [k (range 5)]
        (let [hx (js/Math.round (* hands (/ k 4)))]
          ^{:key k}
          [:text {:x (xat hx) :y (+ mt ph 18) :text-anchor "middle"
                  :fill faint :font-size 11 :font-family "monospace"} hx]))
      [:text {:x (- ml 10) :y (+ mt ph 18) :text-anchor "end"
              :fill faint :font-size 11 :font-family "monospace"} "hand"]
      (when-let [i @hover-hand]
        [:line {:x1 (xat i) :y1 mt :x2 (xat i) :y2 (+ mt ph)
                :stroke gold :stroke-width 1 :opacity 0.5}])
      (for [[idx {:keys [player points final out-at]}] (map-indexed vector series)]
        (let [colour (nth series-colors (mod idx (count series-colors)))
              ;; a line stops where its player busted: a flat run along zero for
              ;; the fifty hands they were not in says nothing, and puts every
              ;; loser's endpoint on the same pixel
              drawn  (if out-at (filterv #(<= (first %) out-at) points) points)
              d (str/join " " (map-indexed
                               (fn [k [hand v]]
                                 (str (if (zero? k) "M" "L") (xat hand) "," (yat v)))
                               drawn))
              [lh lv] (last drawn)]
          ^{:key idx}
          [:g
           [:path {:d d :fill "none" :stroke colour :stroke-width 2
                   :stroke-linejoin "round" :stroke-linecap "round"}]
           ;; end marker, ringed in the surface colour so crossings stay legible
           [:circle {:cx (xat lh) :cy (yat lv) :r 5 :fill colour
                     :stroke ground :stroke-width 2}]
           (when-let [i @hover-hand]
             (when (or (nil? out-at) (<= i out-at))
               (let [v (second (nth points (min i (dec (count points))) [0 0]))]
                 [:circle {:cx (xat i) :cy (yat v) :r 4 :fill colour
                           :stroke ground :stroke-width 2}])))
           ;; Only the line still holding chips is labelled. Everybody else ends
           ;; a tournament on zero, so labelling each endpoint either stacks them
           ;; all on one pixel or pushes them up over the data; the legend
           ;; carries them, with the hand they went out on.
           (when (pos? final)
             [:g
              [:circle {:cx (+ (xat lh) 16) :cy (yat lv) :r 4 :fill colour}]
              [:text {:x (+ (xat lh) 26) :y (+ (yat lv) 4) :fill ink-primary
                      :font-size 12 :font-family "monospace"}
               (str player " " final)]])]))]
     (when-let [i @hover-hand]
       [:div {:style {:position "absolute" :left (str (* 100 (/ (xat i) w)) "%")
                      :top "0" :transform (if (> i (/ hands 2))
                                            "translate(-104%, 0)" "translate(4%, 0)")
                      :background "#1b1828" :border (str "1px solid " grid-ink)
                      :border-radius "6px" :padding "8px 10px" :font-size "12px"
                      :pointer-events "none" :white-space "nowrap" :z-index 3}}
        [:div {:style {:color faint :margin-bottom "4px"}}
         (if (zero? i) "before the first hand" (str "after hand " i))]
        (for [[idx {:keys [player points out-at]}] (map-indexed vector series)]
          ^{:key idx}
          [:div {:style {:display "flex" :align-items "center" :gap "6px"}}
           [:span {:style {:width "8px" :height "8px" :border-radius "50%"
                           :background (nth series-colors (mod idx (count series-colors)))
                           :display "inline-block"}}]
           [:span {:style {:color ink-second}} player]
           [:span {:style {:color ink-primary :margin-left "auto"}}
            (if (and out-at (> i out-at))
              "out"
              (second (nth points (min i (dec (count points))) [0 0])))]])])]))

(defn- small-multiples
  "Nine players is more than the palette has slots for, and a ninth invented
   hue is how a chart starts lying. One panel each instead."
  [{:keys [series hands]}]
  (let [top (reduce + 0 (map :final series))]
    [:div {:style {:display "grid" :grid-template-columns "repeat(3, 1fr)" :gap "10px"}}
     (for [[idx {:keys [player points final]}] (map-indexed vector series)]
       (let [w 260 h 90 ml 4 mt 6
             pw (- w 8) ph (- h 12)
             xat (fn [i] (+ ml (* pw (/ (double i) (max 1 hands)))))
             yat (fn [v] (+ mt (* ph (- 1 (/ (double v) (max 1 top))))))]
         ^{:key idx}
         [:div {:style {:background "#1b1828" :border-radius "6px" :padding "8px"}}
          [:div {:style {:display "flex" :justify-content "space-between"
                         :font-size "12px" :margin-bottom "4px"}}
           [:span {:style {:color ink-second}} player]
           [:span {:style {:color ink-primary}} final]]
          [:svg {:viewBox (str "0 0 " w " " h) :width "100%"
                 :style {:display "block"}}
           [:line {:x1 ml :y1 (yat 0) :x2 (+ ml pw) :y2 (yat 0)
                   :stroke grid-ink :stroke-width 1}]
           [:path {:d (str/join " " (map-indexed
                                     (fn [k [hand v]]
                                       (str (if (zero? k) "M" "L") (xat hand) "," (yat v)))
                                     points))
                   :fill "none" :stroke (first series-colors) :stroke-width 2
                   :stroke-linejoin "round" :stroke-linecap "round"}]]]))]))

(defn- legend
  "Always present for two or more lines -- identity is never colour alone. It
   also carries what became of each player, since only the survivor is labelled
   on the chart itself."
  [series]
  [:div {:style {:display "flex" :flex-wrap "wrap" :gap "18px" :margin-top "12px"}}
   (for [[idx {:keys [player final out-at won]}] (map-indexed vector series)]
     ^{:key idx}
     [:div {:style {:display "flex" :align-items "center" :gap "7px"}}
      [:span {:style {:width "14px" :height "2px" :border-radius "1px"
                      :background (nth series-colors (mod idx (count series-colors)))
                      :display "inline-block"}}]
      [:span {:style {:color ink-second :font-size "12px"}} player]
      [:span {:style {:color (if (pos? final) gold faint) :font-size "12px"}}
       (if (pos? final) (str final) (str "out, hand " out-at))]
      [:span {:style {:color faint :font-size "11px"}}
       (str "\u00b7 won " won)]])])

(defn- stat-tile [label value sub]
  [:div {:style {:background "#1b1828" :border-radius "8px" :padding "14px 16px"
                 :min-width "150px" :flex "1 1 150px"}}
   [:div {:style {:color faint :font-size "11px" :letter-spacing "1px"
                  :text-transform "uppercase"}} label]
   [:div {:style {:color gold :font-size "24px" :margin "4px 0 2px"}} value]
   (when sub [:div {:style {:color ink-second :font-size "12px"}} sub])])

(defn- hands-table [state]
  (let [name-of #(get-in state [:players % :name])]
    [:div {:style {:max-height "320px" :overflow-y "auto"}}
     [:table {:style {:width "100%" :border-collapse "collapse" :font-size "13px"}}
      [:thead
       [:tr {:style {:color faint :text-align "left"}}
        (for [c ["hand" "board" "pot" "won by" "with"]]
          ^{:key c} [:th {:style {:padding "6px 8px" :font-weight 400
                                  :position "sticky" :top 0 :background ground}} c])]]
      [:tbody
       (for [h (reverse (:history state))]
         ^{:key (:hand h)}
         [:tr {:style {:border-top (str "1px solid " grid-ink)}}
          [:td {:style {:padding "6px 8px" :color faint}} (:hand h)]
          [:td {:style {:padding "6px 8px" :color ink-second}}
           (if (seq (:board h))
             [:span {:style {:display "flex" :gap "3px"}}
              (for [[i c] (map-indexed vector (:board h))]
                ^{:key i} [card/card {:value c :width 22}])]
             "—")]
          [:td {:style {:padding "6px 8px"}} [chips (:pot h)]]
          [:td {:style {:padding "6px 8px" :color ink-primary}}
           (str/join ", " (map name-of (keys (:awards h))))]
          [:td {:style {:padding "6px 8px" :color ink-second}}
           (if-let [shown (seq (:shown h))]
             (let [[_ {:keys [cards]}] (first (sort-by (fn [[s _]] (- (get (:awards h) s 0))) shown))]
               (deck/describe-hand (concat cards (:board h))))
             (if (:showdown? h) "—" "everyone folded"))]])]]]))

(defn summary-view [state]
  (let [s (holdem/summary state)
        best (:best-hand s)]
    [:div {:style {:padding "32px 40px" :max-width "1100px" :margin "0 auto"
                   :overflow-y "auto" :height "100vh" :box-sizing "border-box"}}
     [:div {:style {:text-align "center" :margin-bottom "28px"}}
      [:div {:style {:color faint :font-size "12px" :letter-spacing "3px"}} "UNIVERSE"]
      [:h2 {:style {:color gold :font-size "30px" :letter-spacing "2px"
                    :margin "6px 0 0"}}
       (str (:winner s) " takes the table")]]

     [:div {:style {:display "flex" :gap "12px" :flex-wrap "wrap"
                    :margin-bottom "28px"}}
      [stat-tile "hands" (:hands s)
       (str (:showdowns s) " went to a showdown")]
      [stat-tile "biggest pot" (:amount (:biggest-pot s))
       (str "hand " (:hand (:biggest-pot s)) " · "
            (str/join ", " (:players (:biggest-pot s))))]
      (if best
        [stat-tile "best hand shown"
         (deck/hand-name (:hand best))
         (str (:player best) " · hand " (:at-hand best) " · 1 in "
              (js/Math.round (/ deck/total-hands (:count (:hand best)))))]
        [stat-tile "best hand shown" "—" "no hand was ever turned over"])]

     [:div {:style {:background "#15121f" :border-radius "10px" :padding "18px"
                    :margin-bottom "24px"}}
      [:div {:style {:color ink-second :font-size "14px" :margin-bottom "10px"}}
       "chips over the course of the table"]
      (if (> (count (:series s)) (count series-colors))
        [small-multiples s]
        [:div [chips-chart s] [legend (:series s)]])]

     [:div {:style {:background "#15121f" :border-radius "10px" :padding "18px"}}
      [:div {:style {:color ink-second :font-size "14px" :margin-bottom "10px"}}
       "every hand"]
      [hands-table state]]

     [:div {:style {:text-align "center" :margin "28px 0"}}
      [:a {:href "/universe/create"
           :style {:background gold :color ground :padding "12px 28px"
                   :border-radius "6px" :text-decoration "none"
                   :letter-spacing "2px"}}
       "new table"]]]))

(defn table-view []
  (let [state @game-state]
    [:div {:style {:background ground :min-height "100vh" :color "#d8d2e8"
                   :font-family "monospace" :display "flex"}}
     [:div {:style {:flex "1 1 auto" :min-width 0 :display "flex"
                    :flex-direction "column" :padding-top (str layout/top-gap "px")}}
      (cond
        (nil? state)
        [:div {:style {:padding "40px" :color faint}} "connecting\u2026"]

        (:winner state)
        [summary-view state]

        :else
        [:div
         [oval-table state]
         [:div {:style {:height (str layout/action-height "px")
                        :display "flex" :flex-direction "column"
                        :align-items "center" :justify-content "center" :gap "8px"}}
          [your-hand-view state]
          (if (= :waiting (:street state))
            [:button {:on-click send-start!
                      :style {:background gold :color ground :border "none"
                              :border-radius "6px" :padding "12px 28px"
                              :cursor "pointer" :font-family "monospace"
                              :font-size "15px" :letter-spacing "2px"}}
             "deal"]
            [action-bar state])]])]
     (when state [status-rail state])]))

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
