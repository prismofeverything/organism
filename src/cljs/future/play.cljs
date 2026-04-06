(ns future.play
  "Frontend for the Future game: renders the Fibonacci ring board and handles
   player interaction via WebSocket."
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.websockets :as ws]
   [future.board :as board]
   [future.game :as game]))

;; ── State atoms ─────────────────────────────────────────────────────────────

(defonce game-state (r/atom nil))
(defonce player-key (r/atom nil))
(defonce selected-space (r/atom nil))
(defonce legal-actions-atom (r/atom {}))
(defonce connection-status (r/atom :disconnected))

;; ── Player colors ───────────────────────────────────────────────────────────

(def player-display-colors
  {:silver "#cccccc"
   :green  "#44dd44"
   :blue   "#5599ff"
   :purple "#bb66ee"
   :void   "#666666"})

(def player-glow-colors
  {:silver "#eeeeee"
   :green  "#66ff66"
   :blue   "#77bbff"
   :purple "#dd88ff"
   :void   "#888888"})

;; ── WebSocket communication ─────────────────────────────────────────────────

(defn receive-message! [message]
  (let [msg-type (get message "type")]
    (case msg-type
      "initialize"
      (do
        (reset! connection-status :connected)
        (when-let [state-str (get message "state")]
          (let [state (reader/read-string state-str)]
            (reset! game-state state)
            (reset! legal-actions-atom (game/legal-actions state)))))

      "game-state"
      (when-let [state-str (get message "state")]
        (let [state (reader/read-string state-str)]
          (reset! game-state state)
          (reset! legal-actions-atom (game/legal-actions state))))

      (println "Unknown message type:" msg-type))))

(defn connect-ws! [play-key]
  (let [protocol (if (= "https:" (.-protocol js/location)) "wss:" "ws:")
        host (.-host js/location)
        url (str protocol "//" host "/ws/future/play/" play-key)]
    (ws/make-websocket! url receive-message!)))

(defn send-create! [players]
  (ws/send-transit-message!
   {"type" "create"
    "players" (pr-str players)}))

(defn send-action! [action-key]
  (ws/send-transit-message!
   {"type" "action"
    "choice" (pr-str action-key)}))

;; ── SVG Board Rendering ─────────────────────────────────────────────────────

(def view-size 800)
(def center (/ view-size 2))

(defn arc-path
  "SVG arc path for a ring segment. start/end are fractions [0,1), r-inner/r-outer are radii."
  [start end r-inner r-outer]
  (let [;; Convert fractions to radians, 0 = north, clockwise
        start-rad (* 2 Math/PI start)
        end-rad   (* 2 Math/PI end)
        ;; We need to subtract PI/2 to start from north
        sa (- start-rad (/ Math/PI 2))
        ea (- end-rad   (/ Math/PI 2))
        ;; Outer arc points
        ox1 (+ center (* r-outer (Math/cos sa)))
        oy1 (+ center (* r-outer (Math/sin sa)))
        ox2 (+ center (* r-outer (Math/cos ea)))
        oy2 (+ center (* r-outer (Math/sin ea)))
        ;; Inner arc points
        ix1 (+ center (* r-inner (Math/cos ea)))
        iy1 (+ center (* r-inner (Math/sin ea)))
        ix2 (+ center (* r-inner (Math/cos sa)))
        iy2 (+ center (* r-inner (Math/sin sa)))
        ;; Large arc flag
        large (if (> (- end start) 0.5) 1 0)]
    (str "M " ox1 " " oy1
         " A " r-outer " " r-outer " 0 " large " 1 " ox2 " " oy2
         " L " ix1 " " iy1
         " A " r-inner " " r-inner " 0 " large " 0 " ix2 " " iy2
         " Z")))

(defn space-component
  "Render a single board space as an SVG path."
  [space-id state selected _actions]
  (let [[orbit idx] space-id
        n (board/orbit-sizes orbit)
        start (/ idx n)
        end   (/ (inc idx) n)
        [r-inner r-outer] (board/orbit-radii orbit)
        path (arc-path start end r-inner r-outer)
        base-color (board/orbit-colors orbit)
        struct (get-in state [:structures space-id])
        divs (get-in state [:sundivers space-id] [])
        is-selected (= space-id selected)
        planet-here (some (fn [[_orb sp]] (= sp space-id))
                          (:planets state))
        struct-owner-color (when struct
                             (get player-display-colors (:owner struct)))
        has-divers (seq divs)
        fill (cond
               struct-owner-color  struct-owner-color
               is-selected         "#ffff44"
               planet-here         "#ffffff"
               has-divers          (get player-display-colors (:owner (first divs)) "#aaa")
               :else               base-color)
        stroke (if is-selected "#ffff00" "#000000")
        stroke-width (if is-selected 2 0.5)]
    [:path {:d path
            :fill fill
            :stroke stroke
            :stroke-width stroke-width
            :opacity 0.85
            :style {:cursor "pointer"}
            :on-click #(reset! selected-space space-id)}]))

(defn sun-component
  "Render the central Sun."
  []
  [:circle {:cx center :cy center :r 50
            :fill board/sun-color
            :stroke "#ff4444"
            :stroke-width 2}])

(defn beam-component
  "Render the beam (yellow line from sun center outward)."
  [_state]
  (let [;; Beam points north by default, could be rotated based on game state
        angle-rad (- (/ Math/PI 2))]
    [:line {:x1 center :y1 center
            :x2 (+ center (* 380 (Math/cos angle-rad)))
            :y2 (+ center (* 380 (Math/sin angle-rad)))
            :stroke "#ffff00"
            :stroke-width 2
            :opacity 0.6}]))

(defn planet-component
  "Render a planet marker on its current space."
  [orbit space-id]
  (let [[x y] (board/space-center-xy space-id)
        color (board/orbit-colors orbit)]
    [:circle {:cx x :cy y :r 8
              :fill color
              :stroke "#ffffff"
              :stroke-width 1.5}]))

(defn structure-label
  "Render a label for a structure on a space."
  [space-id struct]
  (let [[x y] (board/space-center-xy space-id)
        ch (case (:piece struct)
             :mothership  "M"
             :energy-node "E"
             :tower       "T"
             :city        "C"
             "?")]
    [:text {:x x :y (+ y 4)
            :text-anchor "middle"
            :font-size 11 :font-weight "bold"
            :font-family "monospace"
            :fill "#ffffff"
            :style {:pointer-events "none"}}
     ch]))

(defn sundiver-pips
  "Render small dots for sundivers on a space."
  [space-id divs]
  (let [[cx cy] (board/space-center-xy space-id)
        n (count divs)]
    [:g
     (for [i (range n)]
       (let [offset-x (* (- i (/ (dec n) 2.0)) 6)]
         ^{:key (str "pip-" (pr-str space-id) "-" i)}
         [:circle {:cx (+ cx offset-x) :cy (- cy 8) :r 2.5
                   :fill (get player-display-colors
                              (:owner (nth divs i)) "#aaa")}]))]))

(defn board-svg
  "Full board SVG component."
  []
  (let [state @game-state
        selected @selected-space
        actions @legal-actions-atom]
    (when state
      [:svg {:viewBox (str "0 0 " view-size " " view-size)
             :width "100%"
             :height "100%"
             :style {:max-width "800px" :max-height "800px"}}
       ;; Background
       [:rect {:width view-size :height view-size :fill "#000000"}]
       ;; Beam (behind everything)
       [beam-component state]
       ;; Sun
       [sun-component]
       ;; Board spaces
       (for [orbit (reverse board/orbits)
             idx (range (board/orbit-sizes orbit))]
         (let [sid (board/space-id orbit idx)]
           ^{:key (pr-str sid)}
           [space-component sid state selected actions]))
       ;; Planets
       (for [[orbit space-id] (:planets state)]
         ^{:key (str "planet-" (name orbit))}
         [planet-component orbit space-id])
       ;; Links (lines between linked spaces)
       (for [[i link] (map-indexed vector (:links state))]
         (let [[ax ay] (board/space-center-xy (:a link))
               [bx by] (board/space-center-xy (:b link))]
           ^{:key (str "link-" i)}
           [:line {:x1 ax :y1 ay :x2 bx :y2 by
                   :stroke "#ffaa00" :stroke-width 1.5 :opacity 0.7}]))
       ;; Flame marker
       (when-let [fs (:flame-space state)]
         (let [[fx fy] (board/space-center-xy fs)]
           [:g
            [:circle {:cx fx :cy fy :r 12
                      :fill "none" :stroke "#ff4400" :stroke-width 2}]
            [:text {:x fx :y (+ fy 4)
                    :text-anchor "middle" :font-size 12
                    :font-family "monospace" :fill "#ff4400"
                    :style {:pointer-events "none"}}
             "F"]]))
       ;; Structures
       (for [[sid struct] (:structures state)
             :when struct]
         ^{:key (str "struct-" (pr-str sid))}
         [structure-label sid struct])
       ;; Sundivers
       (for [[sid divs] (:sundivers state)
             :when (seq divs)]
         ^{:key (str "divs-" (pr-str sid))}
         [sundiver-pips sid divs])])))

;; ── Info panels ─────────────────────────────────────────────────────────────

(defn game-status []
  (let [state @game-state]
    (when state
      [:div {:style {:margin-bottom "8px"}}
       ;; Win/loss banner
       (when-let [w (:winner state)]
         [:div {:style {:padding "8px" :margin-bottom "8px" :border-radius "4px"
                        :background (if (= w :win) "#224422" "#442222")
                        :color (if (= w :win) "#66ff66" "#ff6666")
                        :font-size "1.2rem" :font-weight "bold"}}
          (if (= w :win) "VICTORY!" "DEFEAT — 13th flare drawn")])
       ;; Flame / turn
       [:div {:style {:color "#ff8844" :font-size "1.2rem"}}
        "Flame: " [:strong (:flame state)]
        " (turn " (:turn state) ")"]
       ;; Phase
       [:div {:style {:color "#aa6633" :font-size "0.9rem"}}
        "Phase: " (name (:phase state))
        (when (:moves-left state)
          (str " — moves left: " (:moves-left state)))
        (when (:actions-left state)
          (str " — actions left: " (:actions-left state)))]
       ;; Flares
       [:div {:style {:color "#cc4444" :font-size "0.85rem"}}
        "Flares: " (:flares-drawn state 0) " / 13"]])))

(defn resource-panel []
  (let [state @game-state]
    (when state
      [:div {:style {:margin-bottom "12px"}}
       [:div {:style {:color "#888" :font-size "0.8rem" :margin-bottom "4px"}}
        "RESOURCES"]
       (for [orbit board/orbits]
         ^{:key (str "res-" (name orbit))}
         [:span {:style {:color (board/orbit-colors orbit)
                         :margin-right "12px"}}
          (name orbit) ": " (get-in state [:resources orbit] 0)])
       [:div {:style {:color "#ffcc00" :margin-top "4px"}}
        "Energy pool: " (:energy-pool state)]])))

(defn city-panel []
  (let [state @game-state]
    (when state
      [:div {:style {:margin-bottom "12px"}}
       [:div {:style {:color "#888" :font-size "0.8rem" :margin-bottom "4px"}}
        "CITIES"]
       (for [orbit board/orbits]
         ^{:key (str "city-" (name orbit))}
         [:span {:style {:color (board/orbit-colors orbit)
                         :margin-right "12px"}}
          (name orbit) ": " (get-in state [:cities orbit] 0)])])))

(defn solar-network-panel []
  (let [state @game-state]
    (when state
      [:div {:style {:margin-bottom "12px"}}
       [:div {:style {:color "#888" :font-size "0.8rem" :margin-bottom "4px"}}
        "SOLAR NETWORK"]
       (for [orbit board/orbits
             :let [section (get-in state [:solar-network orbit])]]
         ^{:key (str "solar-" (name orbit))}
         [:div {:style {:color (board/orbit-colors orbit) :font-size "0.85rem"}}
          (name orbit) ": "
          (str (count (:available section)) " avail, "
               (count (:exhausted section)) " spent")])])))

(defn hand-panel []
  (let [state @game-state
        player @player-key]
    (when (and state player)
      (let [hand (get-in state [:hands player])]
        [:div {:style {:margin-bottom "12px"}}
         [:div {:style {:color "#888" :font-size "0.8rem" :margin-bottom "4px"}}
          "YOUR HAND"]
         (if (seq hand)
           (for [[i card] (map-indexed vector hand)]
             ^{:key (str "card-" i)}
             [:span {:style {:display "inline-block"
                             :padding "4px 8px"
                             :margin "2px"
                             :background "#1a1a2a"
                             :border (str "1px solid "
                                          (get board/orbit-colors (:suit card) "#666"))
                             :border-radius "3px"
                             :color (get board/orbit-colors (:suit card) "#ccc")
                             :cursor "pointer"}}
              (str (name (:suit card)) " " (:value card))])
           [:span {:style {:color "#555"}} "No cards"])]))))

(defn player-supply-panel []
  (let [state @game-state
        player @player-key]
    (when (and state player)
      (let [ps (get-in state [:players player])]
        [:div {:style {:margin-bottom "12px"}}
         [:div {:style {:color "#888" :font-size "0.8rem" :margin-bottom "4px"}}
          (str "SUPPLY (" (name (:color ps)) ")")]
         [:div {:style {:color "#aaa" :font-size "0.85rem" :line-height "1.8"}}
          [:div (str "Mothership: " (if (= :supply (:mothership ps)) "ready" "placed"))]
          [:div (str "Habitat: " (:habitat ps) " sundivers")]
          [:div (str "Reserve: " (:reserve ps) " sundivers")]
          [:div (str "Energy nodes: " (:energy-nodes ps)
                     " | Towers: " (:towers ps))]
          [:div (str "Components: " (:components ps)
                     " | Cities: " (:city-tokens ps))]
          [:div (str "Links: " (:links ps)
                     " | Energy: " (:energy ps))]]]))))

(defn actions-panel []
  (let [state @game-state
        actions @legal-actions-atom
        player @player-key]
    (when (and state (= player (game/current-player state)))
      [:div {:style {:margin-top "12px"}}
       [:div {:style {:color "#888" :font-size "0.8rem" :margin-bottom "4px"}}
        "ACTIONS"]
       (for [[ak _] actions]
         ^{:key (pr-str ak)}
         [:button
          {:style {:display "block"
                   :padding "6px 12px"
                   :margin "4px 0"
                   :background "#2a2a3a"
                   :border "1px solid #4a3a6a"
                   :border-radius "3px"
                   :color "#ccbbee"
                   :cursor "pointer"
                   :font-family "monospace"}
           :on-click #(send-action! ak)}
          (pr-str ak)])])))

;; ── Create game form ────────────────────────────────────────────────────────

(defonce create-player-count (r/atom 2))
(defonce create-game-key (r/atom ""))

(defn create-form []
  [:div {:style {:padding "48px" :color "#aabbcc" :font-family "monospace"}}
   [:h2 {:style {:color "#ff8844" :margin-bottom "24px"}} "CREATE FUTURE GAME"]
   [:div {:style {:margin-bottom "12px"}}
    [:label "Game key: "]
    [:input {:type "text"
             :value @create-game-key
             :on-change #(reset! create-game-key (-> % .-target .-value))
             :style {:background "#1a1a2a" :color "#ccc" :border "1px solid #444"
                     :padding "4px 8px" :font-family "monospace"}}]]
   [:div {:style {:margin-bottom "12px"}}
    [:label "Players: "]
    [:select {:value @create-player-count
              :on-change #(reset! create-player-count
                                  (js/parseInt (-> % .-target .-value)))
              :style {:background "#1a1a2a" :color "#ccc" :border "1px solid #444"
                      :padding "4px 8px"}}
     [:option {:value 2} "2"]
     [:option {:value 3} "3"]
     [:option {:value 4} "4"]
     [:option {:value 5} "5"]]]
   [:button
    {:on-click (fn []
                 (let [_n @create-player-count
                       key (if (str/blank? @create-game-key)
                             (str "future-" (random-uuid))
                             @create-game-key)]
                   (set! (.-href js/location)
                         (str "/future/play/" key))))
     :style {:padding "8px 16px" :background "#4a3a6a" :color "#ccbbee"
             :border "1px solid #6a5a8a" :border-radius "3px"
             :cursor "pointer" :font-family "monospace"}}
    "CREATE"]])

;; ── Observe page ────────────────────────────────────────────────────────────

(defn observe-view []
  [:div {:style {:padding "48px" :color "#aabbcc" :font-family "monospace"}}
   [:h2 {:style {:color "#ff8844" :margin-bottom "24px"}} "OBSERVE FUTURE GAMES"]
   [:p {:style {:color "#555"}} "Games in progress will appear here."]])

;; ── Generate page (bot simulation) ─────────────────────────────────────────

(def bot-players ["Void" "Purple" "Blue" "Green" "Silver"])

(defonce gen-running (r/atom false))
(defonce gen-speed (r/atom 300))   ; ms between steps
(defonce gen-timer (atom nil))
(defonce gen-history (r/atom []))  ; [{:turn :player :action :phase}]

(defn bot-pick-action
  "Simple heuristic bot: prefer non-pass actions, pick randomly."
  [actions]
  (let [entries (vec actions)
        non-pass (filterv (fn [[ak _]] (not= ak [:pass])) entries)]
    (if (seq non-pass)
      (rand-nth non-pass)
      (rand-nth entries))))

(defn gen-step!
  "Advance the simulation by one action."
  []
  (when-let [state @game-state]
    (when-not (:winner state)
      (let [actions (game/legal-actions state)]
        (when (seq actions)
          (let [[ak next-state] (bot-pick-action actions)
                player (game/current-player state)]
            (swap! gen-history conj
                   {:turn (:turn state)
                    :player player
                    :phase (:phase state)
                    :action (pr-str ak)})
            (reset! game-state next-state)
            (reset! legal-actions-atom (game/legal-actions next-state))))))))

(defn gen-stop! []
  (reset! gen-running false)
  (when-let [t @gen-timer]
    (js/clearInterval t)
    (reset! gen-timer nil)))

(defn gen-start! []
  (gen-stop!)
  (reset! gen-running true)
  (reset! gen-timer
          (js/setInterval
           (fn []
             (if (or (:winner @game-state) (not @gen-running))
               (gen-stop!)
               (gen-step!)))
           @gen-speed)))

(defn gen-new-game! []
  (gen-stop!)
  (let [state (game/create-game bot-players)]
    (reset! game-state state)
    (reset! legal-actions-atom (game/legal-actions state))
    (reset! gen-history [])))

(defn gen-controls []
  (let [state @game-state
        running @gen-running
        done (and state (:winner state))]
    [:div {:style {:margin-bottom "12px"}}
     [:div {:style {:display "flex" :gap "6px" :margin-bottom "8px" :flex-wrap "wrap"}}
      [:button
       {:on-click gen-new-game!
        :style {:padding "6px 12px" :background "#3a3a5a" :color "#ccbbee"
                :border "1px solid #5a4a7a" :border-radius "3px"
                :cursor "pointer" :font-family "monospace"}}
       "New Game"]
      (when (and state (not done))
        [:button
         {:on-click gen-step!
          :disabled running
          :style {:padding "6px 12px" :background "#2a4a3a" :color "#88ddaa"
                  :border "1px solid #4a6a5a" :border-radius "3px"
                  :cursor "pointer" :font-family "monospace"
                  :opacity (if running 0.5 1)}}
         "Step"])
      (when (and state (not done))
        (if running
          [:button
           {:on-click gen-stop!
            :style {:padding "6px 12px" :background "#4a2a2a" :color "#ff8888"
                    :border "1px solid #6a4a4a" :border-radius "3px"
                    :cursor "pointer" :font-family "monospace"}}
           "Stop"]
          [:button
           {:on-click gen-start!
            :style {:padding "6px 12px" :background "#2a3a4a" :color "#88bbdd"
                    :border "1px solid #4a5a6a" :border-radius "3px"
                    :cursor "pointer" :font-family "monospace"}}
           "Auto"]))]
     ;; Speed slider
     (when (and state (not done))
       [:div {:style {:display "flex" :align-items "center" :gap "8px"}}
        [:span {:style {:color "#666" :font-size "0.8rem"}} "Speed:"]
        [:input {:type "range" :min 50 :max 1000 :step 50
                 :value @gen-speed
                 :on-change (fn [e]
                              (let [v (js/parseInt (-> e .-target .-value))]
                                (reset! gen-speed v)
                                (when running
                                  (gen-start!))))
                 :style {:width "120px"}}]
        [:span {:style {:color "#666" :font-size "0.8rem"}}
         (str @gen-speed "ms")]])]))

(defn gen-log []
  (let [history @gen-history
        recent (vec (take-last 20 history))]
    [:div {:style {:margin-top "8px" :max-height "200px" :overflow-y "auto"
                   :font-size "0.75rem" :color "#667"}}
     [:div {:style {:color "#888" :font-size "0.8rem" :margin-bottom "4px"}} "LOG"]
     (for [[i entry] (map-indexed vector (reverse recent))]
       ^{:key (str "log-" (- (count history) i))}
       [:div {:style {:border-bottom "1px solid #1a1a1a" :padding "2px 0"}}
        [:span {:style {:color "#555"}} (str "T" (:turn entry) " ")]
        [:span {:style {:color (get player-display-colors
                                    (keyword (str/lower-case (or (:player entry) "")))
                                    "#888")}}
         (:player entry)]
        [:span {:style {:color "#444"}} (str " [" (name (:phase entry)) "] ")]
        [:span {:style {:color "#556"}} (:action entry)]])]))

(defn all-players-panel []
  (let [state @game-state]
    (when state
      [:div {:style {:margin-bottom "8px"}}
       [:div {:style {:color "#888" :font-size "0.8rem" :margin-bottom "4px"}} "PLAYERS"]
       (for [pk (:flame-order state)]
         (let [ps (get-in state [:players pk])
               color-kw (:color ps)
               is-current (= pk (game/current-player state))]
           ^{:key pk}
           [:div {:style {:padding "3px 6px" :margin-bottom "3px" :font-size "0.8rem"
                          :background (if is-current "#1a2a1a" "#111")
                          :border-left (str "3px solid "
                                            (get player-display-colors color-kw "#666"))}}
            [:div {:style {:color (get player-display-colors color-kw "#aaa")
                           :font-weight (if is-current "bold" "normal")}}
             pk
             (when is-current " ◀")]
            [:div {:style {:color "#667" :font-size "0.7rem"}}
             (str "E:" (:energy ps)
                  " H:" (:habitat ps)
                  " R:" (:reserve ps)
                  " C:" (:components ps)
                  " CT:" (:city-tokens ps)
                  " L:" (:links ps))]]))])))

(defn generate-view []
  [:div {:style {:display "flex" :flex-direction "row" :height "100vh"
                 :background "#111" :color "#ccc" :font-family "monospace"}}
   ;; Board
   [:div {:style {:flex 1 :display "flex" :justify-content "center"
                  :align-items "center" :padding "16px"}}
    [board-svg]]
   ;; Sidebar
   [:div {:style {:width "360px" :padding "12px" :overflow-y "auto"
                  :border-left "1px solid #222"}}
    [:h3 {:style {:color "#ff8844" :margin-bottom "8px"}} "GENERATE"]
    [gen-controls]
    (when @game-state
      [:div
       [game-status]
       [all-players-panel]
       [resource-panel]
       [city-panel]
       [solar-network-panel]
       [gen-log]])]])

;; ── Main layout ─────────────────────────────────────────────────────────────

(defn game-view []
  [:div {:style {:display "flex" :flex-direction "row" :height "100vh"
                 :background "#111" :color "#ccc" :font-family "monospace"}}
   ;; Board panel
   [:div {:style {:flex 1 :display "flex" :justify-content "center"
                  :align-items "center" :padding "16px"}}
    [board-svg]]
   ;; Info sidebar
   [:div {:style {:width "320px" :padding "16px" :overflow-y "auto"
                  :border-left "1px solid #222"}}
    [game-status]
    [resource-panel]
    [city-panel]
    [solar-network-panel]
    [hand-panel]
    [player-supply-panel]
    [actions-panel]]])

(defn page []
  (let [play-key (when (exists? js/playKey) js/playKey)
        is-create (and (exists? js/isCreate) js/isCreate)
        is-observe (and (exists? js/isObserve) js/isObserve)
        is-generate (and (exists? js/isGenerate) js/isGenerate)]
    (cond
      is-generate [generate-view]
      is-create   [create-form]
      is-observe  [observe-view]
      play-key    [game-view]
      :else       [:div {:style {:padding "48px" :color "#555"}}
                    "Loading..."])))

;; ── Mount / init ────────────────────────────────────────────────────────────

(defn mount-components []
  (when-let [el (.getElementById js/document "future")]
    (rdom/render [page] el))
  ;; Connect websocket if we have a play key
  (when (and (exists? js/playKey) js/playKey
             (not (str/blank? js/playKey)))
    (reset! player-key (when (exists? js/playerKey) js/playerKey))
    (connect-ws! js/playKey)))

(defn init! []
  (mount-components))
