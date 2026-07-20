(ns future.play
  "Reagent frontend for FUTURE. Four pages, dispatched by JS globals set
   in the HTML shell:

     js/playKey     — /future/play/:play   live game (WebSocket)
     js/isCreate    — /future/create       new-game lobby
     js/isObserve   — /future/observe      observer list
     js/isGenerate  — /future/generate     local bot simulator"
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.websockets :as ws]
   [organism.ajax :as ajax]
   [organism.components :as components]
   [future.board :as board]
   [future.game :as game]))

;; ── Atoms ─────────────────────────────────────────────────────────────────

(defonce game-state         (r/atom nil))
(defonce player-key         (r/atom nil))
(defonce selected-space     (r/atom nil))
(defonce legal-actions-atom (r/atom {}))
(defonce connection-status  (r/atom :disconnected))
(defonce error-log          (r/atom []))
(defonce action-log         (r/atom []))

;; ── Player identity colors ────────────────────────────────────────────────

(def player-fill
  {:silver "#cccccc"
   :green  "#3fbf4f"
   :blue   "#3f7fdf"
   :purple "#9f3fdf"
   :void   "#666666"})

(defn player-color [state pk]
  (player-fill (get-in state [:players pk :wedge-color])))

;; ── Helpers ───────────────────────────────────────────────────────────────

(defn- safe-read [s]
  (try (reader/read-string s)
       (catch :default e
         (swap! error-log conj (str "parse error: " e))
         nil)))

(defn- safe-name [k]
  (cond (keyword? k) (name k)
        (string? k)  k
        :else        (str k)))

(defn- sid->str [sid]
  (cond
    (and (vector? sid) (= :orbit (first sid)))
    (str (safe-name (second sid)) "·" (nth sid 2))

    (and (vector? sid) (= :sun (first sid)))
    (str "sun" (second sid))

    :else (pr-str sid)))

(defn- action-label
  "Human-readable label for a choice-key."
  [k]
  (cond
    (not (vector? k)) (pr-str k)
    :else
    (let [verb (first k)
          args (rest k)]
      (case verb
        :place-mothership     (str "place mothership @ " (sid->str (first args)))
        :stay                 "stay (no shift)"
        :shift-in             "shift inward"
        :shift-out            "shift outward"
        :choose-move          "MOVE (5 points)"
        :choose-activate      "ACTIVATE"
        :launch               (str "launch → " (sid->str (first args)))
        :fly                  (str "fly " (sid->str (first args)) " → " (sid->str (second args)))
        :path                 (str "path " (sid->str (first args)) " → " (sid->str (second args)))
        :planet-on            (str "board planet @ " (sid->str (first (first args))))
        :planet-off           (str "disembark planet @ " (sid->str (first (first args))))
        :done-moving          "done moving"
        :activate-sun         "activate: SUN"
        :activate-planets     "activate: PLANETS"
        :activate-cities      "activate: CITIES"
        :activate-space       (str "activate space " (sid->str (first args)))
        :done-activating      "done activating"
        :done-activating-space "skip this activation"
        :no-activation-possible "no activation possible"
        :sun-outer            (str "sun-outer sundiver #" (first args))
        :sun-inner            (str "sun-inner sundiver #" (first args))
        :planet-buy           (str "buy resource (sundiver #" (first args) ")")
        :planet-build         (str "build " (safe-name (second args))
                                   " city (sundiver #" (first args) ")")
        :link                 (str "link "
                                   (sid->str (first args))
                                   " → "
                                   (sid->str (second args))
                                   (when (nth args 2 nil)
                                     (str " (exhaust " (safe-name (nth args 2)) ")")))
        :done-linking         "done linking"
        :take-bonus           "take bonus links"
        :decline-bonus        "decline bonus"
        :draw-next            "draw card"
        :orbit-resolved       "resolve orbit"
        :advance-resolved     "advance mothership"
        :begin-next-turn      "begin next turn"
        :end                  "END"
        (pr-str k)))))

;; ── WebSocket ─────────────────────────────────────────────────────────────

(defn- push-log! [action-key]
  (when-let [s @game-state]
    (swap! action-log conj
           {:turn (:turn s)
            :player (:flame s)
            :phase (:phase s)
            :action (action-label action-key)})))

(defn receive-message! [msg]
  (let [type (get msg "type")]
    (case type
      "initialize"
      (do (reset! connection-status :connected)
          (reset! player-key (get msg "player"))
          (when-let [s (get msg "state")]
            (let [st (safe-read s)]
              (reset! game-state st)
              (reset! legal-actions-atom (game/legal-actions st)))))

      "game-state"
      (when-let [s (get msg "state")]
        (let [st  (safe-read s)
              ak  (when-let [a (get msg "action")] (safe-read a))]
          (when ak (push-log! ak))
          (reset! game-state st)
          (reset! legal-actions-atom (game/legal-actions st))))

      (println "future: unknown message type" type))))

(defn connect-ws! [pk]
  (let [proto (if (= "https:" (.-protocol js/location)) "wss:" "ws:")
        host  (.-host js/location)
        url   (str proto "//" host "/ws/future/play/" pk)]
    (ws/make-websocket! url receive-message!)))

(defn send-action! [action-key]
  (ws/send-transit-message!
    {"type" "action"
     "choice" (pr-str action-key)}))

;; ── SVG board ─────────────────────────────────────────────────────────────

(def view-size board/view-size)
(def vcenter board/center)

(defn- background []
  [:rect {:width view-size :height view-size :fill "#06070d"}])

(defn- beam-line []
  [:line {:x1 vcenter :y1 (- vcenter board/sun-inner-r)
          :x2 vcenter :y2 (- vcenter (second (board/orbit-radii :void)))
          :stroke board/beam-color :stroke-width 2 :opacity 0.55}])

(defn- ring-divider [r]
  [:circle {:cx vcenter :cy vcenter :r r
            :fill "none" :stroke "#000" :stroke-width 1 :opacity 0.4}])

(defn- action-touches-sid?
  "Does any action key contain sid as an argument?"
  [actions sid]
  (some (fn [[k _]]
          (and (vector? k)
               (some #(= sid %) k)))
        actions))

(defn- orbital-space-component [sid state selected actions]
  (let [d       (board/orbit-space-path sid)
        city    (game/city-at state sid)
        ms-owner (some (fn [[pk pd]] (when (= sid (:mothership pd)) pk))
                       (:players state))
        planet? (some (fn [[_ p]] (= p sid)) (:planets state))
        sel?    (= sid selected)
        base    (get board/orbit-colors (board/orbit-of sid))
        fill    (cond
                  ms-owner (player-color state ms-owner)
                  city     (get board/orbit-colors (:color city))
                  sel?     "#ffe066"
                  :else    base)
        touched? (action-touches-sid? actions sid)]
    [:path {:d d
            :fill fill
            :fill-opacity (if city 0.85 0.55)
            :stroke (cond
                      sel?     "#ffe066"
                      planet?  "#ffffff"
                      touched? "#8fddff"
                      :else    "#000")
            :stroke-width (cond
                            sel?     2
                            planet?  1.8
                            touched? 1.5
                            :else    0.5)
            :style {:cursor (if touched? "pointer" "default")}
            :on-click #(reset! selected-space sid)}]))

(defn- wedge-component [sid state actions selected]
  (let [k       (board/wedge-of sid)
        col     (get board/orbit-colors (board/wedge-color k))
        inner-d (board/wedge-triangle-path k)
        outer-d (board/wedge-outer-path k)
        sn      (get-in state [:solar-network k] {:active {} :exhausted {}})
        active-n  (apply + (vals (:active sn)))
        exhaust-n (apply + (vals (:exhausted sn)))
        [cx cy] (board/space-center sid)
        touched? (action-touches-sid? actions sid)
        sel?    (= sid selected)]
    [:g {:key (str "wedge-" k)}
     [:path {:d outer-d
             :fill board/sun-outer-color :fill-opacity 0.95
             :stroke (cond sel? "#ffe066"
                           touched? "#8fddff"
                           :else "#000")
             :stroke-width (if (or sel? touched?) 1.6 0.8)
             :on-click #(reset! selected-space sid)
             :style {:cursor "pointer"}}]
     [:path {:d inner-d :fill col :fill-opacity 0.95
             :stroke "#000" :stroke-width 0.8
             :on-click #(reset! selected-space sid)
             :style {:cursor "pointer"}}]
     [:text {:x cx :y (+ cy 3) :text-anchor "middle"
             :font-size 9 :font-family "monospace" :fill "#000"
             :style {:pointer-events "none"}}
      (str active-n "/" (+ active-n exhaust-n))]]))

(defn- planet-marker [orbit sid]
  (let [[x y] (board/space-center sid)]
    [:circle {:cx x :cy y :r 7
              :fill (get board/planet-fill orbit)
              :stroke "#ffffff" :stroke-width 1.5
              :style {:pointer-events "none"}}]))

(defn- city-marker [sid c]
  (let [[x y] (board/space-center sid)]
    [:g {:style {:pointer-events "none"}}
     [:rect {:x (- x 7) :y (- y 7) :width 14 :height 14
             :fill (get board/orbit-colors (:color c))
             :stroke "#000" :stroke-width 1.2}]
     [:text {:x x :y (+ y 4) :text-anchor "middle"
             :font-size 9 :font-family "monospace" :fill "#000"} "C"]]))

(defn- mothership-marker [state pk sid]
  (let [[x y] (board/space-center sid)
        pc    (player-color state pk)]
    [:g {:style {:pointer-events "none"}}
     [:circle {:cx x :cy y :r 10 :fill pc :stroke "#fff" :stroke-width 1.5}]
     [:text {:x x :y (+ y 4) :text-anchor "middle"
             :font-size 9 :font-family "monospace" :fill "#000"} "M"]]))

(defn- sundiver-pips [state sid divs]
  (let [[cx cy] (board/space-center sid)
        n (count divs)]
    (into [:g {:style {:pointer-events "none"}}]
      (for [[i d] (map-indexed vector divs)]
        (let [ox (* (- i (/ (dec n) 2.0)) 5)
              oy (- cy 11)]
          [:g {:key (str "sd-" (pr-str sid) "-" i)}
           [:circle {:cx (+ cx ox) :cy oy :r 2.4
                     :fill (player-color state (:owner d))
                     :stroke "#000" :stroke-width 0.4}]
           (when (:resource d)
             [:circle {:cx (+ cx ox) :cy (- oy 4) :r 1.6
                       :fill (get board/orbit-colors (:resource d))
                       :stroke "#000" :stroke-width 0.3}])
           (when (:on-planet? d)
             [:circle {:cx (+ cx ox) :cy oy :r 3.6
                       :fill "none" :stroke "#fff" :stroke-width 0.6}])])))))

(defn- flame-marker [sid]
  (let [[x y] (board/space-center sid)]
    [:g {:style {:pointer-events "none"}}
     [:circle {:cx x :cy y :r 14 :fill "none"
               :stroke board/flame-color :stroke-width 2 :opacity 0.9}]
     [:text {:x x :y (+ y 5) :text-anchor "middle"
             :font-size 13 :font-weight "bold"
             :font-family "monospace" :fill board/flame-color} "F"]]))

(defn- link-line [state {:keys [a b owner]}]
  (let [[ax ay] (board/space-center a)
        [bx by] (board/space-center b)]
    [:line {:x1 ax :y1 ay :x2 bx :y2 by
            :stroke (player-color state owner)
            :stroke-width 2
            :opacity 0.85
            :style {:pointer-events "none"}}]))

(defn- resource-token-glyph [sid resources]
  (let [[cx cy] (board/space-center sid)
        n (count resources)]
    (into [:g {:style {:pointer-events "none"}}]
      (for [[i c] (map-indexed vector resources)]
        [:circle {:key (str "res-" (pr-str sid) "-" i)
                  :cx (+ cx (* (- i (/ (dec n) 2.0)) 4))
                  :cy (+ cy 10)
                  :r 2.0
                  :fill (get board/orbit-colors c)
                  :stroke "#000" :stroke-width 0.3}]))))

(defn board-svg []
  (let [state   @game-state
        sel     @selected-space
        actions @legal-actions-atom]
    (when state
      [:svg {:viewBox (str "0 0 " view-size " " view-size)
             :preserveAspectRatio "xMidYMid meet"
             :style {:width "100%" :height "100%"
                     :max-width "820px" :max-height "820px"}}
       [background]
       [beam-line]
       (for [ring board/orbits
             :let [[ri ro] (board/orbit-radii ring)]]
         ^{:key (str "rd-" (name ring))}
         [:g
          [ring-divider ri]
          [ring-divider ro]])
       (for [ring (reverse board/orbits)
             i (range (board/ring-sizes ring))
             :let [sid (board/orbit-space ring i)]]
         ^{:key (str "sp-" (name ring) "-" i)}
         [orbital-space-component sid state sel actions])
       (for [k (range board/num-wedges)
             :let [sid (board/sun-space k)]]
         ^{:key (str "w-" k)}
         [wedge-component sid state actions sel])
       (for [[orbit sid] (:planets state)]
         ^{:key (str "pl-" (name orbit))}
         [planet-marker orbit sid])
       (for [[i link] (map-indexed vector (:links state))]
         ^{:key (str "lk-" i)}
         [link-line state link])
       (for [[sid c] (:cities state)]
         ^{:key (str "ci-" (pr-str sid))}
         [city-marker sid c])
       (for [[pk pd] (:players state)
             :when (and (:mothership pd) (not= :supply (:mothership pd)))]
         ^{:key (str "ms-" pk)}
         [mothership-marker state pk (:mothership pd)])
       (for [[sid divs] (:sundivers state)
             :when (seq divs)]
         ^{:key (str "sd-" (pr-str sid))}
         [sundiver-pips state sid divs])
       (for [[sid res] (:resources state)
             :when (seq res)]
         ^{:key (str "rt-" (pr-str sid))}
         [resource-token-glyph sid res])
       (when-let [fs (game/flame-space state)]
         ^{:key "flame"}
         [flame-marker fs])])))

;; ── Side panels ───────────────────────────────────────────────────────────

(defn- panel-label [s]
  [:div {:style {:color "#5a5a78" :font-size "0.72rem"
                 :letter-spacing "0.1em" :margin "10px 0 4px 0"
                 :text-transform "uppercase"}}
   s])

(defn- chip [bg fg text]
  [:span {:style {:display "inline-block"
                  :padding "2px 6px" :margin "1px 3px 1px 0"
                  :background bg :color fg
                  :border-radius "3px"
                  :font-size "0.72rem"}}
   text])

(defn game-status []
  (let [state @game-state]
    (when state
      [:div
       (when-let [w (:winner state)]
         [:div {:style {:padding "10px" :margin "0 0 10px 0"
                        :border-radius "4px"
                        :background (cond
                                      (= w :salvation)         "#1f3a3a"
                                      (= (:result w) :win)     "#1f3a1f"
                                      :else                    "#3a1f1f")
                        :color (cond
                                 (= w :salvation)     "#9feeee"
                                 (= (:result w) :win) "#9fee9f"
                                 :else                "#ee9f9f")
                        :font-weight "bold"}}
          (cond
            (= w :salvation)          "SALVATION — communal victory"
            (= (:result w) :win)      (str "VICTORY — " (:winner w))
            (= (:result w) :none)     "NO WINNER — tie cascade")])
       [:div {:style {:color (player-color state (:flame state))
                      :font-size "1.05rem" :font-weight "bold"}}
        (str "> " (:flame state))]
       [:div {:style {:color "#5a5a78" :font-size "0.78rem"}}
        (str "phase: " (name (:phase state))
             " · turn " (:turn state)
             " · flares " (:flares-drawn state) "/" game/flares-to-end)]
       (when (= :moving (:phase state))
         [:div {:style {:color "#88ccff" :font-size "0.78rem"}}
          (str "moves left: " (get-in state [:phase-data :moves-left]))])
       (when (= :activating (:phase state))
         [:div {:style {:color "#cc88ff" :font-size "0.78rem"}}
          (str "target: " (name (or (get-in state [:phase-data :target]) :unset))
               " · activated: " (get-in state [:phase-data :activated-count] 0)
               " · cards-owed: " (get-in state [:phase-data :cards-owed] 0))])
       (when (= :link-placement (:phase state))
         [:div {:style {:color "#ffcc44" :font-size "0.78rem"}}
          (str "actions left: " (get-in state [:phase-data :actions-left])
               " · actor: " (get-in state [:phase-data :actor])
               (when (get-in state [:phase-data :is-bonus?]) " (BONUS)"))])
       (when (= :drawing-cards (:phase state))
         [:div {:style {:color "#88ffaa" :font-size "0.78rem"}}
          (str "draw " (get-in state [:phase-data :cards-drawn] 0)
               " / " (get-in state [:phase-data :cards-owed] 0))])])))

(defn market-panel []
  (let [state @game-state]
    (when state
      [:div
       [panel-label "MARKET"]
       (for [c board/orbits]
         ^{:key (str "mk-" (name c))}
         [:div {:style {:font-size "0.78rem" :line-height "1.4"
                        :color (get board/orbit-colors c)}}
          (name c) ": "
          (chip "#2a1a1a" (get board/orbit-colors c)
                (str "R " (get-in state [:market-resources c] 0)))
          (chip "#1a2a1a" (get board/orbit-colors c)
                (str "C " (get-in state [:market-cities c] 0)))])
       [:div {:style {:color "#ffcc44" :font-size "0.78rem" :margin-top "6px"}}
        "energy pool: " (:energy-pool state)]])))

(defn players-panel []
  (let [state @game-state
        cur   (game/current-player state)]
    (when state
      [:div
       [panel-label "PLAYERS"]
       (for [pk (:turn-order state)
             :let [pd (get-in state [:players pk])
                   col (player-fill (:wedge-color pd))]]
         ^{:key (str "pl-" pk)}
         [:div {:style {:padding "4px 6px"
                        :margin-bottom "3px"
                        :background (if (= pk cur) "#1f1f33" "#0c0c14")
                        :border-left (str "3px solid " col)}}
          [:div {:style {:color col :font-size "0.85rem"
                         :font-weight (if (= pk cur) "bold" "normal")}}
           pk (when (= pk cur) " <")]
          [:div {:style {:color "#5a5a78" :font-size "0.7rem"}}
           (str "E:" (:energy pd) " H:" (:habitat pd)
                " R:" (:reserve pd) " C:" (:components pd)
                " P:" (:city-platforms pd) " L:" (:links-supply pd)
                " V:" (:vaporized pd))]])])))

(defn hand-panel []
  (let [state @game-state pk @player-key]
    (when (and state pk)
      (let [hand (get-in state [:hands pk] [])]
        [:div
         [panel-label (str "HAND (" (count hand) ")")]
         (if (empty? hand)
           [:span {:style {:color "#444" :font-size "0.75rem"}} "-"]
           (for [[i c] (map-indexed vector hand)
                 :let [col (if (game/flare-card? c) "#ff8844"
                             (get board/orbit-colors (:suit c)))]]
             ^{:key (str "h-" i)}
             [:span {:style {:display "inline-block"
                             :padding "2px 5px" :margin "1px 2px"
                             :background "#1a1a2a"
                             :border (str "1px solid " col)
                             :border-radius "3px"
                             :color col
                             :font-size "0.7rem"}}
              (str (name (:suit c)) " " (:value c))]))]))))

(defn solar-panel []
  (let [state @game-state]
    (when state
      [:div
       [panel-label "SOLAR NETWORK"]
       (for [k (range board/num-wedges)
             :let [c (board/wedge-color k)
                   sn (get-in state [:solar-network k])
                   act (apply + (vals (:active sn)))
                   exh (apply + (vals (:exhausted sn)))]]
         ^{:key (str "sn-" k)}
         [:div {:style {:font-size "0.78rem" :line-height "1.4"
                        :color (get board/orbit-colors c)}}
          (str "wedge " k " (" (name c) "): "
               act " active, " exh " spent")])])))

(defn actions-panel []
  (let [state   @game-state
        actions @legal-actions-atom
        pk      @player-key
        cur     (game/choice-player state)]
    (when state
      [:div
       [panel-label (cond
                      (= pk cur) "YOUR TURN — CHOOSE"
                      (nil? cur) "GAME OVER"
                      :else      (str "WAITING ON " cur))]
       (cond
         (empty? actions)
         [:div {:style {:color "#444" :font-size "0.75rem"}} "(no legal actions)"]

         :else
         (for [[ak _] actions]
           ^{:key (pr-str ak)}
           [:button
            {:style {:display "block" :width "100%"
                     :padding "5px 8px" :margin "2px 0"
                     :background "#1a1a2c" :color "#ccbbee"
                     :border "1px solid #4a3a6a"
                     :border-radius "3px"
                     :font-family "monospace"
                     :font-size "0.78rem"
                     :text-align "left"
                     :cursor "pointer"}
             :disabled (not= pk cur)
             :on-click #(send-action! ak)}
            (action-label ak)]))])))

(defn log-panel []
  (let [entries @action-log
        recent  (vec (take-last 25 entries))]
    [:div
     [panel-label "LOG"]
     [:div {:style {:max-height "180px" :overflow-y "auto"
                    :font-size "0.72rem"}}
      (for [[i e] (map-indexed vector (reverse recent))]
        ^{:key (str "log-" (- (count entries) i))}
        [:div {:style {:padding "1px 0"}}
         [:span {:style {:color "#444"}} (str "T" (:turn e) " ")]
         [:span {:style {:color (player-fill
                                  (get-in @game-state [:players (:player e) :wedge-color]))}}
          (:player e)]
         [:span {:style {:color "#444"}} (str " [" (name (or (:phase e) "")) "] ")]
         [:span {:style {:color "#aabbcc"}} (:action e)]])]]))

;; ── Live game view ────────────────────────────────────────────────────────

(defn game-view []
  [:div {:style {:display "flex" :flex-direction "row" :height "100vh"
                 :background "#06070d" :color "#ccbbee"
                 :font-family "monospace"}}
   [:div {:style {:flex 1 :display "flex" :justify-content "center"
                  :align-items "center" :padding "12px"}}
    [board-svg]]
   [:div {:style {:width "360px" :padding "14px" :overflow-y "auto"
                  :border-left "1px solid #161620"}}
    [game-status]
    [players-panel]
    [market-panel]
    [solar-panel]
    [hand-panel]
    [actions-panel]
    [log-panel]]])

;; ── Create view ───────────────────────────────────────────────────────────

(defn create-view []
  [components/create-lobby
   {:game-type      "future"
    :title          "FUTURE — New Game"
    :current-player (when (and (exists? js/playerKey)
                               (not (str/blank? js/playerKey))
                               (not= "--observer--" js/playerKey))
                      js/playerKey)
    :min-players    2
    :max-players    5
    :accent         "#ff8844"
    :slot-bg        "#1a1a2c"
    :background     "#06070d"}])

;; ── Observe list view ────────────────────────────────────────────────────

(defn observe-view []
  (let [raw  (when (exists? js/observeGames) js/observeGames)
        list (when raw (safe-read raw))]
    [:div {:style {:padding "32px" :background "#06070d"
                   :color "#ccbbee" :font-family "monospace" :min-height "100vh"}}
     [:h2 {:style {:color "#ff8844" :margin-bottom "18px"}} "OBSERVE FUTURE GAMES"]
     (if (seq list)
       (for [g list]
         ^{:key (str (:key g))}
         [:a {:href (str "/future/play/" (:key g))
              :style {:display "block" :padding "10px 14px" :margin "4px 0"
                      :color "#ccbbee" :background "#1a1a2c"
                      :border "1px solid #4a3a6a" :border-radius "3px"
                      :text-decoration "none"}}
          (str (:key g) " — players: " (str/join ", " (:players g)))])
       [:div {:style {:color "#5a5a78"}} "(no live future games)"])]))

;; ── Local bot simulator (/future/generate) ────────────────────────────────

(def default-bot-players ["Sola" "Vega" "Lyra" "Nova" "Pyre"])

(def terminal-choice-keys
  #{[:done-moving] [:done-activating] [:done-linking]
    [:done-activating-space]
    [:decline-bonus] [:no-activation-possible]})

(defn bot-pick [actions]
  (let [entries (vec actions)
        non-term (filterv (fn [[ck _]] (not (contains? terminal-choice-keys ck))) entries)
        pool    (if (seq non-term) non-term entries)]
    (rand-nth pool)))

(defonce gen-running    (r/atom false))
(defonce gen-interval-ms (r/atom 250))
(defonce gen-timer      (atom nil))
(defonce gen-num-players (r/atom 3))

(defn- gen-step! []
  (let [state @game-state]
    (when (and state (not= :game-over (:phase state)))
      (let [actions (game/legal-actions state)]
        (when (seq actions)
          (let [[ak nxt] (bot-pick actions)]
            (push-log! ak)
            (reset! game-state nxt)
            (reset! legal-actions-atom (game/legal-actions nxt))))))))

(defn- gen-stop! []
  (reset! gen-running false)
  (when-let [t @gen-timer]
    (js/clearInterval t)
    (reset! gen-timer nil)))

(defn- gen-start! []
  (gen-stop!)
  (reset! gen-running true)
  (reset! gen-timer
          (js/setInterval
            (fn []
              (if (or (not @gen-running) (= :game-over (:phase @game-state)))
                (gen-stop!)
                (gen-step!)))
            @gen-interval-ms)))

(defn- gen-new! []
  (gen-stop!)
  (reset! action-log [])
  (let [n  (max 2 (min 5 @gen-num-players))
        ps (vec (take n default-bot-players))
        st (game/create-game ps)]
    (reset! game-state st)
    (reset! legal-actions-atom (game/legal-actions st))
    (reset! player-key (first ps))))

(defn gen-controls []
  (let [st @game-state run? @gen-running done? (and st (= :game-over (:phase st)))]
    [:div
     [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap"
                    :align-items "center" :margin-bottom "8px"}}
      [:label {:style {:color "#5a5a78" :font-size "0.72rem"}} "players:"]
      [:input {:type "number" :min 2 :max 5
               :value @gen-num-players
               :on-change (fn [e]
                            (reset! gen-num-players
                                    (js/parseInt (-> e .-target .-value))))
               :style {:width "44px" :padding "4px 6px"
                       :background "#111" :color "#ccbbee"
                       :border "1px solid #334" :border-radius "3px"
                       :font-family "monospace"}}]
      [:button {:on-click gen-new!
                :style {:padding "6px 10px" :background "#2a2a44"
                        :color "#ccbbee" :border "1px solid #4a3a6a"
                        :border-radius "3px" :cursor "pointer"
                        :font-family "monospace"}}
       "New game"]
      [:button {:on-click gen-step! :disabled (or run? done?)
                :style {:padding "6px 10px" :background "#2a443a"
                        :color "#88ddaa" :border "1px solid #4a6a5a"
                        :border-radius "3px" :cursor "pointer"
                        :font-family "monospace"
                        :opacity (if (or run? done?) 0.4 1)}}
       "Step"]
      (if run?
        [:button {:on-click gen-stop!
                  :style {:padding "6px 10px" :background "#442a2a"
                          :color "#ff9999" :border "1px solid #6a4a4a"
                          :border-radius "3px" :cursor "pointer"
                          :font-family "monospace"}}
         "Stop"]
        [:button {:on-click gen-start! :disabled (or done? (nil? st))
                  :style {:padding "6px 10px" :background "#2a3a44"
                          :color "#88bbdd" :border "1px solid #4a5a6a"
                          :border-radius "3px" :cursor "pointer"
                          :font-family "monospace"
                          :opacity (if (or done? (nil? st)) 0.4 1)}}
         "Auto"])]
     [:div {:style {:display "flex" :align-items "center" :gap "6px"}}
      [:span {:style {:color "#5a5a78" :font-size "0.75rem"}} "speed:"]
      [:input {:type "range" :min 40 :max 1200 :step 20
               :value @gen-interval-ms
               :on-change (fn [e]
                            (let [v (js/parseInt (-> e .-target .-value))]
                              (reset! gen-interval-ms v)
                              (when run? (gen-start!))))
               :style {:width "120px"}}]
      [:span {:style {:color "#5a5a78" :font-size "0.75rem"}}
       (str @gen-interval-ms "ms")]]]))

(defn generate-view []
  [:div {:style {:display "flex" :flex-direction "row" :height "100vh"
                 :background "#06070d" :color "#ccbbee"
                 :font-family "monospace"}}
   [:div {:style {:flex 1 :display "flex" :justify-content "center"
                  :align-items "center" :padding "12px"}}
    [board-svg]]
   [:div {:style {:width "360px" :padding "14px" :overflow-y "auto"
                  :border-left "1px solid #161620"}}
    [:h3 {:style {:color "#ff8844" :margin "0 0 8px 0"}} "GENERATE"]
    [gen-controls]
    (when @game-state
      [:div
       [game-status]
       [players-panel]
       [market-panel]
       [solar-panel]
       [log-panel]])]])

;; ── Top-level dispatcher ─────────────────────────────────────────────────

(defn page []
  (let [pk          (when (exists? js/playKey) js/playKey)
        is-create?  (and (exists? js/isCreate) js/isCreate)
        is-observe? (and (exists? js/isObserve) js/isObserve)
        is-gen?     (and (exists? js/isGenerate) js/isGenerate)]
    (cond
      is-gen?     [generate-view]
      is-create?  [create-view]
      is-observe? [observe-view]
      (and pk (not (str/blank? pk))) [game-view]
      :else
      [:div {:style {:padding "48px" :color "#5a5a78"
                     :font-family "monospace" :background "#06070d"
                     :min-height "100vh"}}
       "Loading future…"])))

;; ── Mount ────────────────────────────────────────────────────────────────

(defn mount-components
  "Render the top-level page and wire up the WebSocket / generator per
   the JS globals set in the HTML shell. Called on init and after every
   hot reload."
  []
  (when-let [el (.getElementById js/document "future")]
    (rdom/render [page] el))
  (let [pk         (when (exists? js/playKey) js/playKey)
        is-create? (and (exists? js/isCreate)   js/isCreate)
        is-observe?(and (exists? js/isObserve)  js/isObserve)
        is-gen?    (and (exists? js/isGenerate) js/isGenerate)]
    (when (and pk (not (str/blank? pk))
               (not is-create?) (not is-observe?) (not is-gen?))
      (reset! player-key (when (exists? js/playerKey) js/playerKey))
      (connect-ws! pk))
    (when is-gen? (gen-new!))))

(defn init!
  "shadow-cljs entrypoint."
  []
  (ajax/load-interceptors!)
  (mount-components))
