(ns eridu.play
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [ajax.core :refer [POST]]
   [eridu.game :as game]
   [eridu.choice :as choice]
   [organism.ajax :as ajax]
   [organism.websockets :as ws]))

;; ── State ─────────────────────────────────────────────────────────────────────

(defonce game-state    (r/atom nil))
(defonce player-key    (r/atom (when (exists? js/playerKey) js/playerKey)))
(defonce player-preferences (r/atom {}))
(defonce observe-games (r/atom []))
(defonce bots-set      (r/atom #{}))
(defonce can-undo?     (r/atom false))
(defonce server-choices (r/atom nil))
(defonce play-history  (r/atom []))

;; ── Helpers ───────────────────────────────────────────────────────────────────

(defn choice-player [state]
  (game/current-player state))

(defn my-turn? [state my-player]
  (and (some? state)
       (= (choice-player state) my-player)
       (not (:game-over state))))

(defn choice-label [k]
  (cond
    (keyword? k) (name k)
    (integer? k) (str "action " k)
    (= k :skip)  "skip"
    (= k :done)  "done"
    :else         (pr-str k)))

;; ── WebSocket communication ───────────────────────────────────────────────────

(defn send-action! [choice-key]
  (ws/send-transit-message! {:type "action" :choice (pr-str choice-key)}))

(defn send-undo! []
  (ws/send-transit-message! {:type "undo"}))

;; ── Rendering: Astrology board ────────────────────────────────────────────────

(def astrology-positions
  "Layout the 7 spaces in a circle."
  (let [cx 200 cy 200 r 130]
    (into {}
          (for [i (range 1 8)
                :let [angle (- (* (/ (* 2 js/Math.PI) 7) (dec i)) (/ js/Math.PI 2))
                      x (+ cx (* r (js/Math.cos angle)))
                      y (+ cy (* r (js/Math.sin angle)))]]
            [i {:x x :y y}]))))

(defn astrology-space-component
  "Render one astrology space."
  [space-id actions players-on-space]
  (let [{:keys [x y]} (get astrology-positions space-id)]
    [:g {:key space-id}
     [:circle {:cx x :cy y :r 35
               :fill "#1a1a2e" :stroke "#334" :stroke-width 1.5}]
     [:text {:x x :y (- y 15) :text-anchor "middle"
             :fill "#aaa" :font-size 10}
      (str "Space " space-id)]
     ;; Action icons
     (for [[idx action] (map-indexed vector actions)]
       (let [ax (+ (- x 24) (* idx 16))
             ay (+ y 5)]
         ^{:key idx}
         [:text {:x ax :y ay :text-anchor "middle"
                 :fill (case (:type action)
                         :take "#4a4" :sell "#aa4" :deploy "#a44"
                         :travel "#48a" :build "#a84" :influence "#84a"
                         :excel "#4aa" :temple "#a4a" "#888")
                 :font-size 9}
          (name (:type action))]))
     ;; Astronomers on this space
     (for [[idx pk] (map-indexed vector players-on-space)]
       ^{:key (str "ast-" pk "-" idx)}
       [:circle {:cx (+ x -10 (* idx 8)) :cy (+ y 20) :r 3
                 :fill "#7af" :stroke "#fff" :stroke-width 0.5}])]))

(defn astrology-board-component [state]
  (let [players (:players state)]
    [:svg {:viewBox "0 0 400 400" :width 400 :height 400
           :style {:background "#0a0a18" :border-radius 8}}
     [:text {:x 200 :y 30 :text-anchor "middle" :fill "#666" :font-size 14}
      "Astrology Board"]
     ;; Connection lines
     (for [[from neighbors] game/astrology-adjacency
           to neighbors
           :when (< from to)]
       (let [{x1 :x y1 :y} (get astrology-positions from)
             {x2 :x y2 :y} (get astrology-positions to)]
         ^{:key (str "line-" from "-" to)}
         [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                 :stroke "#223" :stroke-width 1}]))
     ;; Spaces
     (for [[space-id space-data] game/astrology-spaces]
       (let [players-on-space
             (mapcat
              (fn [[pk pdata]]
                (for [pos (:astronomers pdata)
                      :when (= pos space-id)]
                  pk))
              players)]
         ^{:key space-id}
         [astrology-space-component space-id (:actions space-data) players-on-space]))]))

;; ── Rendering: City board ─────────────────────────────────────────────────────

(def city-positions
  "Hand-placed positions for the city graph."
  {:samarra  {:x 150 :y 40}
   :nineveh  {:x 60  :y 110}
   :kish     {:x 240 :y 110}
   :babylon  {:x 60  :y 200}
   :napur    {:x 340 :y 110}
   :buruq    {:x 420 :y 60}
   :lagash   {:x 340 :y 200}
   :uruk     {:x 200 :y 280}
   :eridu    {:x 280 :y 330}})

(defn city-component [state city my-player choices]
  (let [{:keys [x y]} (get city-positions city)
        demands (get-in state [:city-demands city] [])
        graph (:city-graph state)
        is-choice? (and choices (contains? choices city))]
    [:g {:key (name city)
         :on-click (when is-choice?
                     #(send-action! city))
         :style (when is-choice? {:cursor "pointer"})}
     [:rect {:x (- x 40) :y (- y 18) :width 80 :height 36
             :rx 6 :fill (if is-choice? "#2a3a2a" "#1a1a2e")
             :stroke (if is-choice? "#4a4" "#334") :stroke-width 1.5}]
     [:text {:x x :y (+ y 2) :text-anchor "middle"
             :fill (if is-choice? "#8f8" "#ccc") :font-size 11}
      (str/capitalize (name city))]
     ;; Demand tokens
     (for [[idx token] (map-indexed vector demands)]
       ^{:key (str "demand-" (name city) "-" idx)}
       [:circle {:cx (+ (- x 20) (* idx 14)) :cy (+ y 14) :r 4
                 :fill (case token
                         :tools "#a84" :pottery "#84a"
                         :gold "#aa4" :gems "#4aa" "#666")}])]))

(defn city-board-component [state my-player choices]
  (let [graph (:city-graph state)
        cities (keys graph)]
    [:svg {:viewBox "0 0 500 380" :width 500 :height 380
           :style {:background "#0a0a18" :border-radius 8}}
     [:text {:x 250 :y 22 :text-anchor "middle" :fill "#666" :font-size 14}
      "City Board"]
     ;; Connection lines
     (for [[from neighbors] graph
           to neighbors
           :when (pos? (compare (name from) (name to)))]
       (let [{x1 :x y1 :y} (get city-positions from)
             {x2 :x y2 :y} (get city-positions to)]
         ^{:key (str "cline-" (name from) "-" (name to))}
         [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                 :stroke "#223" :stroke-width 1}]))
     ;; Cities
     (for [city cities]
       ^{:key (name city)}
       [city-component state city my-player choices])
     ;; Caravans
     (for [[pk pdata] (:players state)
           :let [city (:caravan pdata)
                 {:keys [x y]} (get city-positions city)]]
       ^{:key (str "caravan-" pk)}
       [:rect {:x (- x 5) :y (- y 28) :width 10 :height 6
               :fill "#f84" :rx 2}])]))

;; ── Rendering: Player info ────────────────────────────────────────────────────

(defn player-info-component [state my-player]
  [:div {:style {:display "flex" :gap 16 :flex-wrap "wrap" :margin "8px 0"}}
   (for [[pk pdata] (:players state)]
     ^{:key pk}
     [:div {:style {:background (if (= pk (game/current-player state)) "#1a2a1a" "#111")
                    :border (str "1px solid " (if (= pk my-player) "#48a" "#333"))
                    :border-radius 6 :padding 10 :min-width 180}}
      [:div {:style {:color "#adf" :font-weight "bold" :margin-bottom 6}} pk]
      [:div {:style {:color "#888" :font-size 12}}
       (str "Caravan: " (when (:caravan pdata) (str/capitalize (name (:caravan pdata)))))]
      [:div {:style {:color "#888" :font-size 12}}
       (str "Resources: "
            (str/join ", "
                      (for [[r n] (:resources pdata) :when (pos? n)]
                        (str (name r) " " n))))]
      [:div {:style {:color "#888" :font-size 12}}
       (str "Roles: "
            (str/join ", "
                      (for [[r lv] (:roles pdata)]
                        (str (name r) " " lv))))]
      [:div {:style {:color "#888" :font-size 12}}
       (str "Amity: " (:amity pdata 0) " Glory: " (:glory pdata 0))]
      [:div {:style {:color "#888" :font-size 12}}
       (str "Raiders: " (:raiders-remaining pdata 0)
            " Temples: " (:temples-remaining pdata 0)
            " Astronomers: " (count (:astronomers pdata)))]])])

;; ── Rendering: Choices ────────────────────────────────────────────────────────

(defn choices-panel [state my-player]
  (let [[phase choices] (if @server-choices
                          [(keyword (subs (first @server-choices) 1))
                           (into #{} (second @server-choices))]
                          (when state (choice/find-state-raw state)))
        is-my-turn (my-turn? state my-player)]
    [:div {:style {:margin "8px 0" :padding 10
                   :background "#111" :border-radius 6
                   :border "1px solid #333"}}
     [:div {:style {:color "#888" :font-size 12 :margin-bottom 6}}
      (str "Phase: " (when phase (name phase))
           " | Current player: " (game/current-player state)
           (when-not is-my-turn " (waiting...)"))]
     (when (and is-my-turn (map? choices) (seq choices))
       [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
        (for [[k _v] choices
              :when (not (#{} k))]
          ^{:key (pr-str k)}
          [:button
           {:on-click #(send-action! k)
            :style {:background "#2a3a2a" :color "#8f8"
                    :border "1px solid #4a4" :border-radius 4
                    :padding "4px 12px" :cursor "pointer"
                    :font-size 13}}
           (choice-label k)])])
     (when (and is-my-turn @can-undo?)
       [:button
        {:on-click send-undo!
         :style {:background "#2a2a2a" :color "#aa8"
                 :border "1px solid #553" :border-radius 4
                 :padding "4px 12px" :cursor "pointer"
                 :font-size 12 :margin-top 6}}
        "undo"])]))

;; ── Create game form ──────────────────────────────────────────────────────────

(defonce create-state
  (r/atom {:play-name "" :players [""] :bots #{}}))

(defn create-form []
  (let [{:keys [play-name players bots]} @create-state]
    [:div {:style {:max-width 500 :margin "40px auto" :padding 20
                   :background "#111" :border-radius 8
                   :font-family "monospace" :color "#ccc"}}
     [:h2 {:style {:color "#7a9" :margin-bottom 16}} "Create Eridu Game"]
     [:div {:style {:margin-bottom 12}}
      [:label {:style {:color "#888" :display "block" :margin-bottom 4}} "Game name"]
      [:input {:type "text" :value play-name
               :on-change #(swap! create-state assoc :play-name (-> % .-target .-value))
               :style {:background "#1a1a2e" :color "#ccc" :border "1px solid #334"
                       :border-radius 4 :padding "6px 10px" :width "100%"}}]]
     [:div {:style {:margin-bottom 12}}
      [:label {:style {:color "#888" :display "block" :margin-bottom 4}} "Players"]
      (for [[idx p] (map-indexed vector players)]
        ^{:key idx}
        [:div {:style {:display "flex" :gap 8 :margin-bottom 4}}
         [:input {:type "text" :value p
                  :on-change #(swap! create-state assoc-in [:players idx] (-> % .-target .-value))
                  :placeholder (str "Player " (inc idx))
                  :style {:background "#1a1a2e" :color "#ccc" :border "1px solid #334"
                          :border-radius 4 :padding "6px 10px" :flex 1}}]
         [:label {:style {:color "#666" :font-size 12 :display "flex" :align-items "center" :gap 4}}
          [:input {:type "checkbox"
                   :checked (contains? bots p)
                   :on-change #(swap! create-state update :bots
                                      (if (contains? bots p) disj conj) p)}]
          "bot"]])
      [:button {:on-click #(swap! create-state update :players conj "")
                :style {:background "#1a2a1a" :color "#8a8" :border "1px solid #343"
                        :border-radius 4 :padding "4px 12px" :cursor "pointer"
                        :margin-top 4}}
       "+ add player"]]
     [:button
      {:on-click
       (fn []
         (let [{:keys [play-name players bots]} @create-state
               valid-players (filterv seq players)]
           (when (and (seq play-name) (seq valid-players))
             (POST "/eridu/create"
                   {:params {:play-name play-name
                             :players valid-players
                             :bots (vec (filter (set valid-players) bots))}
                    :handler (fn [resp]
                               (let [play-key (or (:play-key resp) (get resp "play-key"))]
                                 (set! (.-location js/window) (str "/eridu/play/" play-key))))
                    :error-handler (fn [err] (js/alert (str "Error: " (pr-str err))))}))))
       :style {:background "#2a3a2a" :color "#8f8" :border "1px solid #4a4"
               :border-radius 4 :padding "8px 20px" :cursor "pointer"
               :font-size 14 :margin-top 8}}
      "Create Game"]]))

;; ── Observe page ──────────────────────────────────────────────────────────────

(defn observe-list []
  (let [games @observe-games]
    [:div {:style {:max-width 600 :margin "40px auto" :padding 20
                   :font-family "monospace" :color "#ccc"}}
     [:h2 {:style {:color "#7a9" :margin-bottom 16}} "Observe Eridu Games"]
     (if (empty? games)
       [:div {:style {:color "#666"}} "No active games."]
       (for [g games]
         ^{:key (:key g)}
         [:a {:href (str "/eridu/play/" (:key g))
              :style {:display "block" :padding "10px 14px" :margin-bottom 8
                      :background "#0a0e1c" :border "1px solid #2a4a80"
                      :border-radius 4 :color "#acc" :text-decoration "none"}}
          [:div (:key g)]
          [:div {:style {:color "#667" :font-size 12}}
           (str "Players: " (str/join ", " (:players g))
                " | Round: " (:round g))]]))]))

;; ── Main game view ────────────────────────────────────────────────────────────

(defn game-view []
  (let [state @game-state
        my-player @player-key]
    (if state
      [:div {:style {:padding 16 :font-family "monospace"}}
       [:div {:style {:display "flex" :gap 16 :flex-wrap "wrap" :align-items "flex-start"}}
        [astrology-board-component state]
        [city-board-component state my-player
         (when (and (my-turn? state my-player)
                    (contains? #{:choose-travel-destination :choose-deploy-city
                                 :choose-build-city :choose-temple-city}
                               (game/current-phase state)))
           (let [[_ choices] (choice/find-state-raw state)]
             choices))]]
       [player-info-component state my-player]
       [choices-panel state my-player]]
      [:div {:style {:color "#666" :padding 40 :font-family "monospace"}}
       "Waiting for game state..."])))

;; ── Root component ────────────────────────────────────────────────────────────

(defn root-component []
  (cond
    (and (exists? js/isCreate) js/isCreate)
    [create-form]

    (and (exists? js/isObserve) js/isObserve)
    [observe-list]

    :else
    [game-view]))

;; ── WebSocket message handler ─────────────────────────────────────────────────

(defn handle-ws-message [message]
  (let [msg-type (get message "type")]
    (case msg-type
      "initialize"
      (do
        (when-let [s (get message "state")]
          (reset! game-state (reader/read-string s)))
        (when-let [b (get message "bots")]
          (reset! bots-set (set b)))
        (when-let [cu (get message "can-undo")]
          (reset! can-undo? cu))
        (when-let [ch (get message "choices")]
          (reset! server-choices [(get message "phase") (reader/read-string ch)])))

      "game-state"
      (do
        (when-let [s (get message "state")]
          (reset! game-state (reader/read-string s)))
        (when-let [cu (get message "can-undo")]
          (reset! can-undo? cu))
        (when-let [ch (get message "choices")]
          (reset! server-choices [(get message "phase") (reader/read-string ch)])))

      "chat"
      (println "chat:" (get message "message"))

      (println "unknown message type:" msg-type))))

;; ── Init ──────────────────────────────────────────────────────────────────────

(defn mount-components []
  (when-let [el (.getElementById js/document "eridu")]
    (rdom/render [root-component] el)))

(defn connect-ws! []
  (when (exists? js/playKey)
    (when-let [play-key js/playKey]
      (when (seq play-key)
        (let [protocol (if (= "https:" (.-protocol js/location)) "wss:" "ws:")
              host     (.-host js/location)
              url      (str protocol "//" host "/ws/eridu/play/" play-key)]
          (ws/make-websocket! url handle-ws-message))))))

(defn init! []
  (ajax/load-interceptors!)
  (when (and (exists? js/playerPreferences) js/playerPreferences)
    (try
      (reset! player-preferences (reader/read-string js/playerPreferences))
      (catch :default _ nil)))
  (when (and (exists? js/observeGames) js/observeGames)
    (try
      (reset! observe-games (reader/read-string js/observeGames))
      (catch :default _ nil)))
  (mount-components)
  (connect-ws!))
