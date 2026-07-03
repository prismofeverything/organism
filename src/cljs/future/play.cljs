(ns future.play
  "Frontend for FUTURE. One bundle, four pages selected by JS globals set in
   the HTML template:

     js/playKey      → /play/:play   live game (WebSocket)
     js/isCreate     → /create        new-game lobby
     js/isObserve    → /observe       observer of an arbitrary play key
     js/isGenerate   → /generate      local single-page bot simulator"
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

;; ── Atoms ──────────────────────────────────────────────────────────────────

(defonce game-state         (r/atom nil))
(defonce player-key         (r/atom nil))
(defonce selected-space     (r/atom nil))
(defonce legal-actions-atom (r/atom {}))
(defonce connection-status  (r/atom :disconnected))
(defonce error-log          (r/atom []))

;; ── Player colors (the wedge color is the player's identity) ──────────────

(def player-fill
  {:silver "#cccccc"
   :green  "#3fbf4f"
   :blue   "#3f7fdf"
   :purple "#9f3fdf"
   :void   "#666666"})

(defn player-color [state pk]
  (player-fill (get-in state [:players pk :wedge-color])))

;; ── WebSocket ──────────────────────────────────────────────────────────────

(defn- safe-read [s]
  (try (reader/read-string s)
       (catch :default e
         (swap! error-log conj (str "parse error: " e))
         nil)))

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
        (let [st (safe-read s)]
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

;; ── Helpers ────────────────────────────────────────────────────────────────

(defn- safe-name [k] (cond (keyword? k) (name k) (string? k) k :else (str k)))

(defn- sid->str [sid]
  (cond
    (and (vector? sid) (= :orbit (first sid)))
    (str (safe-name (second sid)) "·" (nth sid 2))
    (and (vector? sid) (= :sun (first sid)))
    (str "sun" (second sid))
    :else (pr-str sid)))

(defn- action-label [k]
  (cond
    (not (vector? k)) (pr-str k)
    :else
    (let [verb (first k)
          rest-args (rest k)]
      (case verb
        :place-mothership (str "place mothership @ " (sid->str (first rest-args)))
        :stay             "stay (no shift)"
        :shift-in         "shift inward"
        :shift-out        "shift outward"
        :move             "MOVE (5 points)"
        :activate         (str "ACTIVATE " (safe-name (first rest-args)))
        :launch           (str "launch → " (sid->str (first rest-args)))
        :fly              (str "fly " (sid->str (first rest-args)) " → " (sid->str (second rest-args)))
        :link-travel      (str "travel via "
                               (safe-name (nth rest-args 2))
                               "'s links: "
                               (sid->str (first rest-args))
                               " → "
                               (sid->str (second rest-args)))
        :done-moving      "done moving"
        :done-activating  "done activating"
        :activate-sun-outer (str "sun-outer wedge " (first rest-args))
        :activate-sun-inner (str "sun-inner wedge " (first rest-args))
        :planet-buy       (str "buy " (safe-name (first rest-args)) " resource")
        :planet-build-city (str "build " (safe-name (nth rest-args 2))
                                " city in " (safe-name (first rest-args)))
        :activate-city    (str "activate city @ " (sid->str (first rest-args)))
        :link             (str "link " (sid->str (first rest-args))
                               " → " (sid->str (second rest-args))
                               " (" (safe-name (nth rest-args 2)) ")")
        :skip-links       "skip remaining links"
        (pr-str k)))))

;; ── SVG board ──────────────────────────────────────────────────────────────

(def view-size board/view-size)
(def vcenter   board/center)

(defn- background []
  [:rect {:width view-size :height view-size :fill "#06070d"}])

(defn- beam-line
  "Yellow beam from the sun outward through index-0 of every ring (north)."
  []
  [:line {:x1 vcenter :y1 (- vcenter board/sun-inner-r)
          :x2 vcenter :y2 (- vcenter (last (board/orbit-radii :void)))
          :stroke board/beam-color :stroke-width 2 :opacity 0.55}])

(defn- ring-divider [r]
  [:circle {:cx vcenter :cy vcenter :r r
            :fill "none" :stroke "#000" :stroke-width 1 :opacity 0.4}])

(defn- orbital-space-component
  [sid state selected actions]
  (let [d        (board/orbit-space-path sid)
        struct   (when (game/city-at state sid) {:piece :city :color (:color (game/city-at state sid))})
        ms-owner (some (fn [[pk pd]] (when (= sid (:mothership pd)) pk)) (:players state))
        planet?  (some (fn [[_ p]] (= p sid)) (:planets state))
        divs     (game/sundivers-at state sid)
        sel?     (= sid selected)
        base     (board/orbit-colors (board/orbit-of sid))
        fill     (cond
                   ms-owner (player-color state ms-owner)
                   (:color struct) (board/orbit-colors (:color struct))
                   sel? "#ffe066"
                   :else base)
        clickable? (some (fn [[k _]]
                           (and (vector? k) (some #(= sid %) k)))
                         actions)]
    [:path {:d d
            :fill fill
            :fill-opacity (if struct 0.85 0.65)
            :stroke (cond
                      sel? "#ffe066"
                      planet? "#ffffff"
                      :else "#000")
            :stroke-width (cond sel? 2 planet? 1.5 :else 0.5)
            :style {:cursor (if clickable? "pointer" "default")}
            :on-click #(reset! selected-space sid)}]))

(defn- wedge-component
  [sid state]
  (let [k    (board/wedge-of sid)
        col  (board/orbit-colors (board/wedge-color k))
        inner-d (board/wedge-triangle-path k)
        outer-d (board/wedge-outer-path k)
        sn      (get-in state [:solar-network k] {:active {} :exhausted {}})
        active-n   (apply + (vals (:active sn)))
        exhaust-n  (apply + (vals (:exhausted sn)))
        _divs   (game/sundivers-at state sid)
        [cx cy] (board/space-center sid)]
    [:g {:key (str "wedge-" k)}
     [:path {:d outer-d :fill board/sun-outer-color :fill-opacity 0.95
             :stroke "#000" :stroke-width 0.8
             :on-click #(reset! selected-space sid)
             :style {:cursor "pointer"}}]
     [:path {:d inner-d :fill col :fill-opacity 0.95
             :stroke "#000" :stroke-width 0.8
             :on-click #(reset! selected-space sid)
             :style {:cursor "pointer"}}]
     ;; component counts on the wedge
     [:text {:x cx :y (+ cy 2) :text-anchor "middle"
             :font-size 9 :font-family "monospace" :fill "#000"
             :style {:pointer-events "none"}}
      (str active-n "/" (+ active-n exhaust-n))]]))

(defn- planet-marker
  [orbit sid]
  (let [[x y] (board/space-center sid)]
    [:circle {:cx x :cy y :r 7
              :fill (board/orbit-colors orbit)
              :stroke "#ffffff" :stroke-width 1.5
              :style {:pointer-events "none"}}]))

(defn- city-marker
  [sid c]
  (let [[x y] (board/space-center sid)]
    [:g {:style {:pointer-events "none"}}
     [:rect {:x (- x 7) :y (- y 7) :width 14 :height 14
             :fill (board/orbit-colors (:color c))
             :stroke "#000" :stroke-width 1.2}]
     [:text {:x x :y (+ y 3) :text-anchor "middle"
             :font-size 9 :font-family "monospace" :fill "#000"} "C"]]))

(defn- mothership-marker
  [state pk sid]
  (let [[x y] (board/space-center sid)
        pc    (player-color state pk)]
    [:g {:style {:pointer-events "none"}}
     [:circle {:cx x :cy y :r 10 :fill pc :stroke "#fff" :stroke-width 1.5}]
     [:text {:x x :y (+ y 3) :text-anchor "middle"
             :font-size 9 :font-family "monospace" :fill "#000"} "M"]]))

(defn- sundiver-pips
  [state sid divs]
  (let [[cx cy] (board/space-center sid)
        n (count divs)]
    (into [:g {:style {:pointer-events "none"}}]
          (for [[i d] (map-indexed vector divs)]
            (let [ox (* (- i (/ (dec n) 2.0)) 5)
                  oy (- cy 11)]
              [:g {:key (str "sd-" (pr-str sid) "-" i)}
               [:circle {:cx (+ cx ox) :cy oy :r 2.2
                         :fill (player-color state (:owner d))
                         :stroke "#000" :stroke-width 0.3}]
               (when (:resource d)
                 [:circle {:cx (+ cx ox) :cy (- oy 4) :r 1.5
                           :fill (board/orbit-colors (:resource d))
                           :stroke "#000" :stroke-width 0.3}])])))))

(defn- flame-marker [sid]
  (let [[x y] (board/space-center sid)]
    [:g {:style {:pointer-events "none"}}
     [:circle {:cx x :cy y :r 14 :fill "none"
               :stroke board/flame-color :stroke-width 2 :opacity 0.9}]
     [:text {:x x :y (+ y 4) :text-anchor "middle"
             :font-size 11 :font-weight "bold"
             :font-family "monospace" :fill board/flame-color} "♨"]]))

(defn- link-line [_state {:keys [a b color]}]
  (let [[ax ay] (board/space-center a)
        [bx by] (board/space-center b)]
    [:line {:x1 ax :y1 ay :x2 bx :y2 by
            :stroke (board/orbit-colors color)
            :stroke-width 2
            :opacity 0.85
            :style {:pointer-events "none"}}]))

(defn board-svg []
  (let [state    @game-state
        sel      @selected-space
        actions  @legal-actions-atom]
    (when state
      (do
        [:svg {:viewBox (str "0 0 " view-size " " view-size)
               :preserveAspectRatio "xMidYMid meet"
               :style {:width "100%" :height "100%"
                       :max-width "820px" :max-height "820px"}}
         [background]
         [beam-line]
         ;; Ring outlines (visual aid)
         (for [orbit board/orbits
               :let [[ri ro] (board/orbit-radii orbit)]]
           ^{:key (str "rd-" (name orbit))}
           [:g
            [ring-divider ri]
            [ring-divider ro]])
         ;; Orbital spaces
         (for [orbit (reverse board/orbits)
               i (range (board/orbit-sizes orbit))
               :let [sid (board/orbit-space orbit i)]]
           ^{:key (str "sp-" (name orbit) "-" i)}
           [orbital-space-component sid state sel actions])
         ;; Sun wedges
         (for [k (range board/num-wedges)
               :let [sid (board/sun-space k)]]
           ^{:key (str "w-" k)}
           [wedge-component sid state])
         ;; Planets
         (for [[orbit sid] (:planets state)]
           ^{:key (str "pl-" (name orbit))}
           [planet-marker orbit sid])
         ;; Links
         (for [[i link] (map-indexed vector (:links state))]
           ^{:key (str "lk-" i)}
           [link-line state link])
         ;; Cities
         (for [[sid c] (:cities state)]
           ^{:key (str "ci-" (pr-str sid))}
           [city-marker sid c])
         ;; Motherships
         (for [[pk pd] (:players state)
               :when (and (:mothership pd) (not= :supply (:mothership pd)))]
           ^{:key (str "ms-" pk)}
           [mothership-marker state pk (:mothership pd)])
         ;; Sundivers (pips above each space)
         (for [[sid divs] (:sundivers state)
               :when (seq divs)]
           ^{:key (str "sd-" (pr-str sid))}
           [sundiver-pips state sid divs])
         ;; Flame
         (when-let [fs (:flame-space state)]
           ^{:key "flame"}
           [flame-marker fs])]))))

;; ── Panels ────────────────────────────────────────────────────────────────

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
         [:div {:style {:padding "10px"
                        :margin "0 0 10px 0"
                        :border-radius "4px"
                        :background (if (= :win (:result w)) "#1f3a1f" "#3a1f1f")
                        :color      (if (= :win (:result w)) "#9fee9f" "#ee9f9f")
                        :font-weight "bold"}}
          (if (= :win (:result w))
            (str "VICTORY — winners: " (str/join ", " (:winners w)))
            (str "DEFEAT — " (name (:reason w))))])
       [:div {:style {:color (player-fill (get-in state [:players (:flame state) :wedge-color]))
                      :font-size "1.05rem" :font-weight "bold"}}
        (str "▶ " (:flame state))]
       [:div {:style {:color "#5a5a78" :font-size "0.78rem"}}
        (str "phase: " (name (:phase state))
             " · turn " (:turn state)
             " · flares " (:flares-drawn state) "/" game/flares-to-end)]
       (when (= :moving (:phase state))
         [:div {:style {:color "#88ccff" :font-size "0.78rem"}}
          (str "moves left: " (get-in state [:phase-data :moves-remaining]))])
       (when (= :activating (:phase state))
         [:div {:style {:color "#cc88ff" :font-size "0.78rem"}}
          (str "activating " (name (get-in state [:phase-data :activate-type]))
               " — draws queued: " (get-in state [:phase-data :cards-pending]))])
       (when (= :placing-links (:phase state))
         [:div {:style {:color "#ffcc44" :font-size "0.78rem"}}
          (str "links remaining: " (get-in state [:phase-data :links-remaining])
               " on " (sid->str (get-in state [:phase-data :city-being-activated])))])])))

(defn market-panel []
  (let [state @game-state]
    (when state
      [:div
       [panel-label "MARKET"]
       (for [c board/orbits]
         ^{:key (str "mk-" (name c))}
         [:div {:style {:font-size "0.78rem" :line-height "1.4"
                        :color (board/orbit-colors c)}}
          (name c) ": "
          (chip "#2a1a1a" (board/orbit-colors c)
                (str "R " (get-in state [:market-resources c] 0)))
          (chip "#1a2a1a" (board/orbit-colors c)
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
           pk (when (= pk cur) " ◀")]
          [:div {:style {:color "#5a5a78" :font-size "0.7rem"}}
           (str "E:" (:energy pd) " H:" (:habitat pd)
                " R:" (:reserve pd) " C:" (:components pd)
                " CP:" (:city-platforms pd) " L:" (:links-supply pd))]])])))

(defn supply-panel []
  (let [state @game-state pk @player-key]
    (when (and state pk)
      (let [pd (get-in state [:players pk])]
        (when pd
          [:div
           [panel-label (str "YOU — " (name (:wedge-color pd)))]
           [:div {:style {:color "#8a8aae" :font-size "0.85rem" :line-height "1.6"}}
            [:div "habitat: " (:habitat pd)]
            [:div "reserve: " (:reserve pd)]
            [:div "energy: " (:energy pd)]
            [:div "components: " (:components pd)]
            [:div "city platforms: " (:city-platforms pd)]
            [:div "links: " (:links-supply pd)]
            [:div "vaporized: " (:vaporized pd)]]])))))

(defn hand-panel []
  (let [state @game-state pk @player-key]
    (when (and state pk)
      (let [hand (get-in state [:hands pk] [])]
        [:div
         [panel-label (str "HAND (" (count hand) ")")]
         (if (empty? hand)
           [:span {:style {:color "#444" :font-size "0.75rem"}} "—"]
           (for [[i c] (map-indexed vector hand)
                 :let [col (if (game/flare-card? c) "#ff8844"
                              (board/orbit-colors (:suit c)))]]
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
             :let [c     (board/wedge-color k)
                   sn    (get-in state [:solar-network k])
                   act   (apply + (vals (:active sn)))
                   exh   (apply + (vals (:exhausted sn)))]]
         ^{:key (str "sn-" k)}
         [:div {:style {:font-size "0.78rem" :line-height "1.4"
                        :color (board/orbit-colors c)}}
          (str "wedge " k " (" (name c) "): "
               act " active, " exh " spent")])])))

(defn actions-panel []
  (let [state   @game-state
        actions @legal-actions-atom
        pk      @player-key
        cur     (game/current-player state)]
    (when (and state (or (= pk cur)
                         (= pk "--observer--")))
      [:div
       [panel-label (if (= pk cur) "YOUR TURN — CHOOSE"
                                   (str "WAITING ON " (or cur "—")))]
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

;; ── Game view ─────────────────────────────────────────────────────────────

(defn game-view []
  [:div {:style {:display "flex" :flex-direction "row" :height "100vh"
                 :background "#06070d" :color "#ccbbee"
                 :font-family "monospace"}}
   [:div {:style {:flex 1 :display "flex" :justify-content "center"
                  :align-items "center" :padding "12px"}}
    [board-svg]]
   [:div {:style {:width "340px" :padding "14px"
                  :overflow-y "auto"
                  :border-left "1px solid #161620"}}
    [game-status]
    [players-panel]
    [market-panel]
    [solar-panel]
    [supply-panel]
    [hand-panel]
    [actions-panel]]])

;; ── Create form ──────────────────────────────────────────────────────────

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

;; ── Observe view ─────────────────────────────────────────────────────────

(defn observe-view []
  (let [raw (when (exists? js/observeGames) js/observeGames)
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

;; ── Generate (local bot simulator) ───────────────────────────────────────

(def bot-players ["Sola" "Vega" "Lyra" "Nova" "Pyre"])

(defonce gen-running    (r/atom false))
(defonce gen-interval-ms (r/atom 300))
(defonce gen-timer      (atom nil))
(defonce gen-history    (r/atom []))

(defn- bot-pick [actions]
  (let [es (vec actions)
        ;; Mild preference: skip end-turn ends if real options exist
        ends? (fn [[ak _]] (#{[:done-moving] [:done-activating] [:skip-links]} ak))
        non-end (filterv #(not (ends? %)) es)
        candidates (if (seq non-end) non-end es)]
    (rand-nth candidates)))

(defn- gen-step! []
  (let [state @game-state]
    (when (and state (not (:winner state)))
      (let [actions (game/legal-actions state)]
        (when (seq actions)
          (let [[ak next-state] (bot-pick actions)
                p (game/current-player state)]
            (swap! gen-history conj
                   {:turn (:turn state) :player p
                    :phase (:phase state) :action (action-label ak)})
            (reset! game-state next-state)
            (reset! legal-actions-atom (game/legal-actions next-state))))))))

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
              (if (or (not @gen-running) (:winner @game-state))
                (gen-stop!)
                (gen-step!)))
            @gen-interval-ms)))

(defn- gen-new! []
  (gen-stop!)
  (reset! gen-history [])
  (let [st (game/create-game bot-players)]
    (reset! game-state st)
    (reset! legal-actions-atom (game/legal-actions st))
    (reset! player-key (first bot-players))))

(defn gen-controls []
  (let [st @game-state run? @gen-running done? (and st (:winner st))]
    [:div
     [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap"
                    :margin-bottom "8px"}}
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
      [:input {:type "range" :min 50 :max 1200 :step 50
               :value @gen-interval-ms
               :on-change (fn [e]
                            (let [v (js/parseInt (-> e .-target .-value))]
                              (reset! gen-interval-ms v)
                              (when run? (gen-start!))))
               :style {:width "120px"}}]
      [:span {:style {:color "#5a5a78" :font-size "0.75rem"}}
       (str @gen-interval-ms "ms")]]]))

(defn gen-log []
  (let [history @gen-history
        recent  (vec (take-last 30 history))]
    [:div {:style {:margin-top "10px" :max-height "240px"
                   :overflow-y "auto" :font-size "0.72rem"
                   :color "#67678a"}}
     [panel-label "LOG"]
     (for [[i e] (map-indexed vector (reverse recent))]
       ^{:key (str "log-" (- (count history) i))}
       [:div {:style {:padding "2px 0" :border-bottom "1px solid #1a1a2c"}}
        [:span {:style {:color "#444"}} (str "T" (:turn e) " ")]
        [:span {:style {:color (player-fill
                                 (get-in @game-state [:players (:player e) :wedge-color]))}}
         (:player e)]
        [:span {:style {:color "#444"}} (str " [" (name (or (:phase e) "")) "] ")]
        [:span {:style {:color "#aabbcc"}} (:action e)]])]))

(defn generate-view []
  [:div {:style {:display "flex" :flex-direction "row" :height "100vh"
                 :background "#06070d" :color "#ccbbee"
                 :font-family "monospace"}}
   [:div {:style {:flex 1 :display "flex" :justify-content "center"
                  :align-items "center" :padding "12px"}}
    [board-svg]]
   [:div {:style {:width "340px" :padding "14px" :overflow-y "auto"
                  :border-left "1px solid #161620"}}
    [:h3 {:style {:color "#ff8844" :margin "0 0 8px 0"}} "GENERATE"]
    [gen-controls]
    (when @game-state
      [:div
       [game-status]
       [players-panel]
       [market-panel]
       [solar-panel]
       [gen-log]])]])

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

(defn mount-components []
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

(defn init! []
  (ajax/load-interceptors!)
  (mount-components))
