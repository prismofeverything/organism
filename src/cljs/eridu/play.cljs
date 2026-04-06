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

;; ── Helpers ───────────────────────────────────────────────────────────────────

(defn my-turn? [state my-player]
  (and (some? state)
       (= (game/current-player state) my-player)
       (not (:game-over state))))

(defn choice-label [k]
  (cond
    (= k :skip)         "skip"
    (= k :done)         "done"
    (= k :begin)        "begin actions"
    (= k :increase-role) "increase a role"
    (keyword? k)        (name k)
    (integer? k)        (let [state @game-state
                              phase (game/current-phase state)]
                          (case phase
                            :choose-die
                            (let [dice (get-in state [:players (game/current-player state) :dice-available])]
                              (str "die: " (nth dice k "?")))
                            :choose-astronomer
                            (str "astronomer " (inc k))
                            :choose-action
                            (let [space (get-in state [:player-turn :space])
                                  action (nth (:actions (get game/action-spaces space)) k nil)]
                              (if action
                                (str (name (:type action))
                                     (when (:resources action)
                                       (str " (" (str/join "+" (map name (:resources action))) ")")))
                                (str "action " k)))
                            (str k)))
    (vector? k)         (str (name (first k)) " -> " (name (second k))
                              (when (= 3 (count k)) (str " (" (nth k 2) ")")))
    :else               (pr-str k)))

;; ── WebSocket communication ───────────────────────────────────────────────────

(defn send-action! [choice-key]
  (ws/send-transit-message! {:type "action" :choice (pr-str choice-key)}))

(defn send-undo! []
  (ws/send-transit-message! {:type "undo"}))

;; ── Rendering: Action board (wheel) ──────────────────────────────────────────

(def action-board-cx 200)
(def action-board-cy 200)
(def action-board-r 130)

(defn action-space-pos [space-id]
  (let [angle (- (* (/ (* 2 js/Math.PI) 7) (dec space-id)) (/ js/Math.PI 2))
        x (+ action-board-cx (* action-board-r (js/Math.cos angle)))
        y (+ action-board-cy (* action-board-r (js/Math.sin angle)))]
    {:x x :y y}))

(def action-type-colors
  {:take "#6a6" :sell "#ca6" :deploy "#c66"
   :travel "#68a" :influence "#86a" :temple "#a6a"})

(defn action-board-component [state]
  (let [_players (:players state)]
    [:svg {:viewBox "0 0 400 400" :width 380 :height 380
           :style {:background "#0a0a18" :border-radius 8}}
     [:text {:x 200 :y 25 :text-anchor "middle" :fill "#555" :font-size 12}
      "Action Board"]
     ;; Connection lines (clockwise circle)
     (for [i (range 1 8)
           :let [j (if (= i 7) 1 (inc i))
                 {x1 :x y1 :y} (action-space-pos i)
                 {x2 :x y2 :y} (action-space-pos j)]]
       ^{:key (str "aline-" i "-" j)}
       [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke "#333" :stroke-width 1.5}])
     ;; Spaces
     (for [space-id (range 1 8)
           :let [{:keys [x y]} (action-space-pos space-id)
                 space-data (get game/action-spaces space-id)
                 actions (:actions space-data)
                 astros (game/astronomers-on-space state space-id)]]
       ^{:key (str "space-" space-id)}
       [:g
        [:circle {:cx x :cy y :r 32 :fill "#1a1a2e" :stroke "#445" :stroke-width 1.5}]
        [:text {:x x :y (- y 12) :text-anchor "middle" :fill "#aaa" :font-size 14 :font-weight "bold"}
         (str space-id)]
        ;; Action labels
        (for [[idx action] (map-indexed vector actions)
              :let [ax (+ (- x 22) (* idx 15))
                    ay (+ y 6)]]
          ^{:key (str "act-" space-id "-" idx)}
          [:text {:x ax :y ay :text-anchor "middle"
                  :fill (get action-type-colors (:type action) "#888")
                  :font-size 7}
           (subs (name (:type action)) 0 (min 3 (count (name (:type action)))))])
        ;; Astronomer dots
        (for [[idx [_pk _]] (map-indexed vector astros)]
          ^{:key (str "astro-" space-id "-" idx)}
          [:circle {:cx (+ x -8 (* idx 6)) :cy (+ y 18) :r 3
                    :fill "#7af" :stroke "#fff" :stroke-width 0.5}])])]))

;; ── Rendering: City board ─────────────────────────────────────────────────────

(def city-positions
  {:samarra  {:x 340 :y 50}
   :nineveh  {:x 80  :y 80}
   :kish     {:x 400 :y 150}
   :babylon  {:x 100 :y 200}
   :nippur   {:x 420 :y 260}
   :lagash   {:x 380 :y 350}
   :uruk     {:x 180 :y 320}
   :eridu    {:x 160 :y 420}})

(defn city-board-component [state _my-player choices]
  (let [graph (:city-graph state)
        routes (:routes state)
        cities (keys graph)]
    [:svg {:viewBox "0 0 500 470" :width 480 :height 450
           :style {:background "#0a0a18" :border-radius 8}}
     [:text {:x 250 :y 22 :text-anchor "middle" :fill "#555" :font-size 12}
      "City Board"]
     ;; Route lines
     (for [{:keys [from to type]} routes
           :let [{x1 :x y1 :y} (get city-positions from)
                 {x2 :x y2 :y} (get city-positions to)]]
       ^{:key (str "route-" (name from) "-" (name to))}
       [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
               :stroke (if (= type :river) "#346" "#443")
               :stroke-width (if (= type :river) 2 1.5)
               :stroke-dasharray (when (= type :river) "6,3")}])
     ;; Raiders on routes
     (for [[pk pdata] (:players state)
           [rk raider-state] (:raiders pdata)
           :let [[c1 c2] rk
                 {x1 :x y1 :y} (get city-positions c1)
                 {x2 :x y2 :y} (get city-positions c2)
                 mx (/ (+ x1 x2) 2)
                 my (/ (+ y1 y2) 2)]]
       ^{:key (str "raider-" pk "-" (name c1) "-" (name c2))}
       [:rect {:x (- mx 5) :y (- my 5) :width 10 :height 10
               :fill (if (= raider-state :point) "#f84" "#844")
               :rx 2 :stroke "#fff" :stroke-width 0.5}])
     ;; Cities
     (for [city cities
           :let [{:keys [x y]} (get city-positions city)
                 demands (get-in state [:city-demands city] [])
                 has-magistrate (game/magistrate-in-city? state city)
                 is-choice? (and choices (contains? choices city))]]
       ^{:key (str "city-" (name city))}
       [:g {:on-click (when is-choice? #(send-action! city))
            :style (when is-choice? {:cursor "pointer"})}
        [:rect {:x (- x 38) :y (- y 16) :width 76 :height 32
                :rx 5 :fill (cond is-choice? "#2a3a2a"
                                  has-magistrate "#2a2a1a"
                                  :else "#1a1a2e")
                :stroke (cond is-choice? "#4a4"
                              has-magistrate "#aa4"
                              :else "#334")
                :stroke-width 1.5}]
        [:text {:x x :y (+ y 3) :text-anchor "middle"
                :fill (cond is-choice? "#8f8"
                            has-magistrate "#ee8"
                            :else "#ccc")
                :font-size 10}
         (str/capitalize (name city))]
        ;; Demand tokens
        (for [[idx token] (map-indexed vector demands)]
          ^{:key (str "demand-" (name city) "-" idx)}
          [:circle {:cx (+ (- x 18) (* idx 14)) :cy (+ y 14) :r 4
                    :fill (case token
                            :tools "#a84" :pottery "#84a"
                            :gold "#aa4" :gems "#4aa" "#666")}])
        ;; Temples
        (for [[pk pdata] (:players state)
              :let [temple-state (get-in pdata [:temples city])]
              :when temple-state]
          ^{:key (str "temple-" pk "-" (name city))}
          [:polygon {:points (let [tx (+ x 25) ty (- y 10)]
                               (str tx "," (- ty 6) " "
                                    (- tx 4) "," (+ ty 2) " "
                                    (+ tx 4) "," (+ ty 2)))
                     :fill (if (= temple-state :face-up) "#a6a" "#636")
                     :stroke "#fff" :stroke-width 0.5}])])
     ;; Caravans
     (for [[pk pdata] (:players state)
           :let [city (:caravan pdata)
                 {:keys [x y]} (get city-positions city)]]
       ^{:key (str "caravan-" pk)}
       [:rect {:x (- x 5) :y (- y 26) :width 10 :height 6
               :fill "#f84" :rx 2}])]))

;; ── Rendering: Player info ────────────────────────────────────────────────────

(defn player-info-component [state my-player]
  [:div {:style {:display "flex" :gap 12 :flex-wrap "wrap" :margin "8px 0"}}
   (for [[pk pdata] (:players state)
         :let [is-current (= pk (game/current-player state))]]
     ^{:key pk}
     [:div {:style {:background (if is-current "#1a2a1a" "#111")
                    :border (str "1px solid " (if (= pk my-player) "#48a" "#333"))
                    :border-radius 6 :padding 8 :min-width 170 :font-size 11}}
      [:div {:style {:color "#adf" :font-weight "bold" :margin-bottom 4}}
       (str pk (when is-current " *"))]
      [:div {:style {:color "#888"}}
       (str "Caravan: " (when (:caravan pdata) (str/capitalize (name (:caravan pdata)))))]
      [:div {:style {:color "#888"}}
       (str "Resources: "
            (str/join " "
                      (for [[r n] (:resources pdata) :when (pos? n)]
                        (str (name r) ":" n))))]
      [:div {:style {:color "#888"}}
       (str "Roles: "
            (str/join " "
                      (for [[r lv] (:roles pdata)]
                        (str (subs (name r) 0 3) ":" lv))))]
      [:div {:style {:color "#888"}}
       (str "Amity:" (:amity pdata 0) " Glory:" (:glory pdata 0))]
      [:div {:style {:color "#888"}}
       (str "Dice: " (str/join "," (or (:dice-available pdata) []))
            " | Temples:" (:temples-supply pdata 0)
            " Raiders:" (:raiders-supply pdata 0))]])])

;; ── Rendering: Choices ────────────────────────────────────────────────────────

(defn choices-panel [state my-player]
  (let [[phase choices] (when state (choice/find-state-raw state))
        is-my-turn (my-turn? state my-player)]
    [:div {:style {:margin "8px 0" :padding 10
                   :background "#111" :border-radius 6
                   :border "1px solid #333"}}
     [:div {:style {:color "#888" :font-size 11 :margin-bottom 6}}
      (str "Round " (:round state 1)
           " Turn " (:turn-in-round state 1)
           " | Phase: " (when phase (name phase))
           " | Player: " (game/current-player state)
           (when-not is-my-turn " (waiting...)"))]
     (when (and is-my-turn (map? choices) (seq choices))
       [:div {:style {:display "flex" :gap 6 :flex-wrap "wrap"}}
        (for [[k _v] choices]
          ^{:key (pr-str k)}
          [:button
           {:on-click #(send-action! k)
            :style {:background "#2a3a2a" :color "#8f8"
                    :border "1px solid #4a4" :border-radius 4
                    :padding "4px 10px" :cursor "pointer"
                    :font-size 12}}
           (choice-label k)])])
     (when (and is-my-turn @can-undo?)
       [:button
        {:on-click send-undo!
         :style {:background "#2a2a2a" :color "#aa8"
                 :border "1px solid #553" :border-radius 4
                 :padding "4px 10px" :cursor "pointer"
                 :font-size 11 :margin-top 4}}
        "undo"])]))

;; ── Create game form ──────────────────────────────────────────────────────────

(defonce create-state
  (r/atom {:play-name "" :players [""] :bots #{}}))

(defn create-form []
  (let [{:keys [play-name players bots]} @create-state]
    [:div {:style {:max-width 500 :margin "40px auto" :padding 20
                   :background "#111" :border-radius 8
                   :font-family "monospace" :color "#ccc"}}
     [:h2 {:style {:color "#BB9944" :margin-bottom 16}} "Create Eridu Game"]
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
     [:h2 {:style {:color "#BB9944" :margin-bottom 16}} "Observe Eridu Games"]
     (if (empty? games)
       [:div {:style {:color "#666"}} "No active games."]
       (for [g games]
         ^{:key (:key g)}
         [:a {:href (str "/eridu/play/" (:key g))
              :style {:display "block" :padding "10px 14px" :margin-bottom 8
                      :background "#0a0e1c" :border "1px solid #5a4a20"
                      :border-radius 4 :color "#cc8" :text-decoration "none"}}
          [:div (:key g)]
          [:div {:style {:color "#667" :font-size 12}}
           (str "Players: " (str/join ", " (:players g))
                " | Round: " (:round g))]]))]))

;; ── Main game view ────────────────────────────────────────────────────────────

(defn game-view []
  (let [state @game-state
        my-player @player-key]
    (if state
      [:div {:style {:padding 12 :font-family "monospace"}}
       (when (:game-over state)
         [:div {:style {:background "#2a1a1a" :border "1px solid #644"
                        :border-radius 6 :padding 12 :margin-bottom 12
                        :color "#faa" :text-align "center"}}
          [:div {:style {:font-size 16 :font-weight "bold"}} "Game Over"]
          [:div {:style {:margin-top 6 :color "#ccc"}}
           (let [scores (for [[pk pdata] (:players state)]
                          {:player pk
                           :amity (:amity pdata 0)
                           :glory (:glory pdata 0)
                           :reputation (min (:amity pdata 0) (:glory pdata 0))})
                 winner (first (sort-by #(- (:reputation %)) scores))]
             (str "Winner: " (:player winner)
                  " (Reputation: " (:reputation winner) ")"))]])
       [:div {:style {:display "flex" :gap 12 :flex-wrap "wrap" :align-items "flex-start"}}
        [action-board-component state]
        [city-board-component state my-player
         (when (and (my-turn? state my-player)
                    (contains? #{:resolve-travel :resolve-temple
                                 :resolve-deploy :resolve-influence}
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

(defn handle-ws-message [{:keys [type state] :as message}]
  (case type
    "initialize"
    (do
      (when state
        (reset! game-state (reader/read-string state)))
      (when (contains? message :bots)
        (reset! bots-set (set (:bots message))))
      (when (contains? message :can-undo)
        (reset! can-undo? (:can-undo message)))
      (when (:choices message)
        (reset! server-choices [(:phase message) (reader/read-string (:choices message))])))

    "game-state"
    (do
      (when state
        (reset! game-state (reader/read-string state)))
      (when (contains? message :can-undo)
        (reset! can-undo? (:can-undo message)))
      (when (:choices message)
        (reset! server-choices [(:phase message) (reader/read-string (:choices message))])))

    "chat"
    (println "chat:" (:message message))

    (println "unknown message type:" type)))

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
