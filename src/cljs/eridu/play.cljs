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
                              (str "🎲 " (nth dice k "?")))
                            :choose-astronomer
                            (str "⭐ astronomer " (inc k))
                            :choose-action
                            (let [space (get-in state [:player-turn :space])
                                  action (nth (:actions (get game/action-spaces space)) k nil)]
                              (if action
                                (str (get game/action-icons (:type action) "")
                                     " " (name (:type action))
                                     (when (:resources action)
                                       (str " ("
                                            (str/join " " (map #(get game/resource-icons % (name %))
                                                               (:resources action)))
                                            ")")))
                                (str "action " k)))
                            (str k)))
    (vector? k)         (str (name (first k)) " → " (name (second k))
                              (when (= 3 (count k)) (str " (" (nth k 2) ")")))
    :else               (pr-str k)))

;; ── WebSocket communication ───────────────────────────────────────────────────

(defn send-action! [choice-key]
  (ws/send-transit-message! {:type "action" :choice (pr-str choice-key)}))

(defn send-undo! []
  (ws/send-transit-message! {:type "undo"}))

;; ── Unicode die faces ─────────────────────────────────────────────────────────

(def die-faces {1 "⚀" 2 "⚁" 3 "⚂" 4 "⚃" 5 "⚄" 6 "⚅"})

;; ── Rendering: Dice display ──────────────────────────────────────────────────

(defn dice-display [state my-player]
  (let [player (game/current-player state)
        pdata (game/player-data state player)
        dice-available (:dice-available pdata [])
        dice-used (:dice-used pdata [])
        phase (game/current-phase state)
        is-choosing (and (= phase :choose-die)
                         (my-turn? state my-player))
        chosen-die (when (= phase :choose-astronomer)
                     (get-in state [:player-turn :die-value]))
        p-color (game/player-color state player)]
    [:div {:style {:background "#0d0d1a" :border-radius 8 :padding 12
                   :border (str "1px solid " p-color) :margin-bottom 8}}
     [:div {:style {:color "#aaa" :font-size 11 :margin-bottom 6}}
      (str "🎲 " player "'s Dice")]
     ;; Show available dice
     [:div {:style {:display "flex" :gap 8 :align-items "center" :flex-wrap "wrap"}}
      (for [[idx die-val] (map-indexed vector dice-available)]
        ^{:key (str "avail-" idx)}
        [:div {:on-click (when is-choosing #(send-action! idx))
               :style {:font-size 36 :cursor (when is-choosing "pointer")
                       :padding "4px 8px" :border-radius 6
                       :background (if is-choosing "#1a2a1a" "#111")
                       :border (str "2px solid " (if is-choosing "#4a4" "#333"))
                       :color (if is-choosing "#8f8" "#ccc")
                       :transition "all 0.2s"
                       :text-align "center" :min-width 44}}
         [:div (get die-faces die-val (str die-val))]
         [:div {:style {:font-size 10 :color "#666"}} (str die-val)]])
      ;; Show chosen die for current turn
      (when chosen-die
        ^{:key "chosen"}
        [:div {:style {:font-size 36 :padding "4px 8px" :border-radius 6
                       :background "#2a2a1a" :border "2px solid #aa8"
                       :color "#ff8" :text-align "center" :min-width 44}}
         [:div (get die-faces chosen-die (str chosen-die))]
         [:div {:style {:font-size 10 :color "#aa8"}} "chosen"]])
      ;; Show used dice (grayed out)
      (for [[idx die-val] (map-indexed vector dice-used)]
        ^{:key (str "used-" idx)}
        [:div {:style {:font-size 28 :padding "4px 8px" :border-radius 6
                       :background "#0a0a0a" :border "1px solid #222"
                       :color "#444" :text-align "center" :min-width 40
                       :opacity 0.5}}
         [:div (get die-faces die-val (str die-val))]
         [:div {:style {:font-size 9 :color "#333"}} "used"]])]]))

;; ── Rendering: Action board (wheel) ──────────────────────────────────────────

(def action-board-cx 220)
(def action-board-cy 220)
(def action-board-r 150)

(defn action-space-pos [space-id]
  (let [angle (- (* (/ (* 2 js/Math.PI) 7) (dec space-id)) (/ js/Math.PI 2))
        x (+ action-board-cx (* action-board-r (js/Math.cos angle)))
        y (+ action-board-cy (* action-board-r (js/Math.sin angle)))]
    {:x x :y y}))

(def action-type-colors
  {:take "#5B8C3E" :sell "#C4A535"  :deploy "#C44B35"
   :travel "#3581A8" :influence "#8B5BC4" :temple "#C45BA8"})

(defn action-board-component [state]
  (let [current-space (get-in state [:player-turn :landed-space])
        phase (game/current-phase state)
        player (game/current-player state)
        p-color (game/player-color state player)]
    [:svg {:viewBox "0 0 440 440" :width 420 :height 420
           :style {:background "radial-gradient(circle, #0d0d1e, #050510)"
                   :background-color "#070712"
                   :border-radius 8 :border (str "1px solid #333")}}
     ;; Title with star
     [:text {:x 220 :y 22 :text-anchor "middle" :fill "#886622" :font-size 13
             :font-weight "bold"}
      "✦ Astronomy Board ✦"]
     ;; Center star decoration
     [:text {:x 220 :y 225 :text-anchor "middle" :fill "#221a00" :font-size 50}
      "✸"]
     ;; Connection lines (clockwise circle)
     (for [i (range 1 8)
           :let [j (if (= i 7) 1 (inc i))
                 {x1 :x y1 :y} (action-space-pos i)
                 {x2 :x y2 :y} (action-space-pos j)]]
       ^{:key (str "aline-" i "-" j)}
       [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke "#2a2a3e" :stroke-width 2
               :stroke-dasharray "4,4"}])
     ;; Spaces
     (for [space-id (range 1 8)
           :let [{:keys [x y]} (action-space-pos space-id)
                 space-data (get game/action-spaces space-id)
                 actions (:actions space-data)
                 astros (game/astronomers-on-space state space-id)
                 is-landed (= space-id current-space)]]
       ^{:key (str "space-" space-id)}
       [:g
        ;; Outer glow for landed space
        (when is-landed
          [:circle {:cx x :cy y :r 48 :fill "none"
                    :stroke "#aa8833" :stroke-width 2 :opacity 0.6
                    :stroke-dasharray "3,3"}])
        ;; Main circle
        [:circle {:cx x :cy y :r 40
                  :fill (if is-landed "#1a1a10" "#0e0e1e")
                  :stroke (if is-landed "#aa8833" "#334")
                  :stroke-width (if is-landed 2.5 1.5)}]
        ;; Space number
        [:text {:x x :y (- y 18) :text-anchor "middle"
                :fill (if is-landed "#ee8" "#777") :font-size 13 :font-weight "bold"}
         (if (= space-id 7) "★" (str space-id))]
        ;; Action icons in a grid
        (let [n (count actions)
              cols (if (= n 4) 2 2)
              rows (js/Math.ceil (/ n cols))]
          (for [[idx action] (map-indexed vector actions)
                :let [col (mod idx cols)
                      row (quot idx cols)
                      ax (+ x (* (- col (/ (dec cols) 2)) 20))
                      ay (+ y -2 (* row 16))
                      atype (:type action)
                      color (get action-type-colors atype "#666")
                      icon (get game/action-icons atype "?")]]
            ^{:key (str "act-" space-id "-" idx)}
            [:text {:x ax :y ay :text-anchor "middle" :fill color :font-size 12
                    :style {:filter (when is-landed "brightness(1.3)")}}
             icon]))
        ;; Astronomer dots with player/solo-color
        (let [solo? (game/solo-mode? state)
              solo-pairs (:solo-pairs state [[0 1] [2 3] [4 5]])
              solo-colors ["#4A90D9" "#D94A90" "#4AD95A"]  ;; Alpha, Beta, Gamma
              active-pair (when solo? (set (game/solo-active-indices state)))
              astro-color (fn [pk astro-idx]
                            (if solo?
                              (let [pair-idx (some (fn [[pi pair]]
                                                     (when (some #{astro-idx} pair) pi))
                                                   (map-indexed vector solo-pairs))]
                                (get solo-colors (or pair-idx 0) "#888"))
                              (game/player-color state pk)))]
          (for [[didx [pk astro-idx]] (map-indexed vector astros)
                :let [acolor (astro-color pk astro-idx)
                      is-active (or (not solo?) (contains? active-pair astro-idx))]]
            ^{:key (str "astro-" space-id "-" didx)}
            [:g
             [:circle {:cx (+ x -10 (* didx 10)) :cy (+ y 28) :r 5
                       :fill acolor :stroke (if is-active "#fff" "#444")
                       :stroke-width (if is-active 0.8 0.5)
                       :opacity (if is-active 1.0 0.4)}]
             [:text {:x (+ x -10 (* didx 10)) :y (+ y 31)
                     :text-anchor "middle" :fill (if is-active "#fff" "#444")
                     :font-size 5 :font-weight "bold"}
              (if solo?
                (let [pair-idx (some (fn [[pi pair]]
                                       (when (some #{astro-idx} pair) pi))
                                     (map-indexed vector solo-pairs))]
                  (get ["α" "β" "γ"] (or pair-idx 0) "?"))
                "★")]]))])
     ;; Legend
     (let [legend-items [[:take "🌾 Take"] [:sell "⚖ Sell"] [:deploy "⚔ Deploy"]
                         [:travel "🐪 Travel"] [:influence "👑 Influence"] [:temple "🏛 Temple"]]]
       (for [[idx [atype label]] (map-indexed vector legend-items)]
         ^{:key (str "legend-" idx)}
         [:text {:x (+ 30 (* (mod idx 3) 140))
                 :y (+ 418 (* (quot idx 3) 14))
                 :fill (get action-type-colors atype "#666")
                 :font-size 9}
          label]))]))

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
        cities (keys graph)
        current-player-key (game/current-player state)]
    [:svg {:viewBox "0 0 500 470" :width 480 :height 450
           :style {:background "linear-gradient(180deg, #080812, #0a0a1a)"
                   :background-color "#090914"
                   :border-radius 8 :border "1px solid #333"}}
     [:text {:x 250 :y 22 :text-anchor "middle" :fill "#886622" :font-size 13
             :font-weight "bold"}
      "✦ City Board ✦"]
     ;; Route lines
     (for [{:keys [from to type]} routes
           :let [{x1 :x y1 :y} (get city-positions from)
                 {x2 :x y2 :y} (get city-positions to)]]
       ^{:key (str "route-" (name from) "-" (name to))}
       [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
               :stroke (if (= type :river) "#1a4a6a" "#4a3a1a")
               :stroke-width (if (= type :river) 3 2.5)
               :stroke-dasharray (when (= type :river) "8,4")
               :opacity 0.7}])
     ;; Raiders on routes
     (for [[pk pdata] (:players state)
           [rk raider-state] (:raiders pdata)
           :let [[c1 c2] rk
                 {x1 :x y1 :y} (get city-positions c1)
                 {x2 :x y2 :y} (get city-positions c2)
                 mx (/ (+ x1 x2) 2)
                 my (/ (+ y1 y2) 2)
                 p-color (game/player-color state pk)
                 is-raiding (= raider-state :raiding)]]
       ^{:key (str "raider-" pk "-" (name c1) "-" (name c2))}
       [:g
        ;; Raider background shape
        (if is-raiding
          ;; Raiding state: shield shape with sword
          [:g
           [:rect {:x (- mx 9) :y (- my 9) :width 18 :height 18
                   :fill p-color :rx 3 :stroke "#fff" :stroke-width 1 :opacity 0.9}]
           [:text {:x mx :y (+ my 5) :text-anchor "middle" :fill "#fff" :font-size 13}
            "⚔"]]
          ;; Point state: flag/banner (flipped, ready to score)
          [:g
           [:rect {:x (- mx 9) :y (- my 9) :width 18 :height 18
                   :fill "#111" :rx 3 :stroke p-color :stroke-width 2}]
           [:text {:x mx :y (+ my 5) :text-anchor "middle" :fill p-color :font-size 12}
            "🏴"]])])
     ;; Magistrates on cities (rendered before cities so they appear as decoration)
     (for [[mag-city _owner] (:magistrates state)
           :let [{:keys [x y]} (get city-positions mag-city)]
           :when (get city-positions mag-city)]
       ^{:key (str "magistrate-" (name mag-city))}
       [:g
        ;; Gold crown glow
        [:circle {:cx x :cy (- y 28) :r 12
                  :fill "#2a2210" :stroke "#C4A535" :stroke-width 1.5}]
        [:text {:x x :y (- y 23) :text-anchor "middle" :fill "#FFD700" :font-size 16}
         "👑"]])
     ;; Cities
     (for [city cities
           :let [{:keys [x y]} (get city-positions city)
                 demands (get-in state [:city-demands city] [])
                 has-magistrate (game/magistrate-in-city? state city)
                 is-choice? (and choices (contains? choices city))]]
       ^{:key (str "city-" (name city))}
       [:g {:on-click (when is-choice? #(send-action! city))
            :style (when is-choice? {:cursor "pointer"})}
        ;; City rectangle
        [:rect {:x (- x 42) :y (- y 18) :width 84 :height 36
                :rx 6 :fill (cond is-choice? "#1a2a1a"
                                  has-magistrate "#1c1c10"
                                  :else "#0e0e1e")
                :stroke (cond is-choice? "#5a5"
                              has-magistrate "#C4A535"
                              :else "#334")
                :stroke-width (if (or is-choice? has-magistrate) 2 1.5)}]
        ;; City name
        [:text {:x x :y (+ y 4) :text-anchor "middle"
                :fill (cond is-choice? "#8f8"
                            has-magistrate "#FFD700"
                            :else "#ccc")
                :font-size 11 :font-weight "bold"}
         (str/capitalize (name city))]
        ;; Demand tokens with resource icons
        (for [[idx token] (map-indexed vector demands)]
          ^{:key (str "demand-" (name city) "-" idx)}
          [:g
           [:circle {:cx (+ (- x 20) (* idx 16)) :cy (+ y 16) :r 6
                     :fill (get game/resource-colors token "#444")
                     :stroke "#fff" :stroke-width 0.5 :opacity 0.9}]
           [:text {:x (+ (- x 20) (* idx 16)) :cy (+ y 19)
                   :y (+ y 19) :text-anchor "middle" :fill "#fff" :font-size 7}
            (get game/resource-icons token "?")]])
        ;; Temples (per player, with player color)
        (for [[pk pdata] (:players state)
              :let [temple-state (get-in pdata [:temples city])]
              :when temple-state
              :let [p-color (game/player-color state pk)
                    is-face-up (= temple-state :face-up)]]
          ^{:key (str "temple-" pk "-" (name city))}
          [:g
           [:text {:x (+ x 30) :y (- y 6) :text-anchor "middle"
                   :fill (if is-face-up p-color "#444")
                   :font-size 14
                   :opacity (if is-face-up 1.0 0.5)}
            "🏛"]
           (when is-face-up
             [:circle {:cx (+ x 38) :cy (- y 10) :r 3
                       :fill p-color :stroke "#fff" :stroke-width 0.5}])])])
     ;; Caravans with player colors
     (for [[pk pdata] (:players state)
           :let [city (:caravan pdata)
                 {:keys [x y]} (get city-positions city)
                 p-color (game/player-color state pk)
                 p-idx (.indexOf (:turn-order state) pk)]]
       ^{:key (str "caravan-" pk)}
       [:g
        [:text {:x (+ (- x 12) (* p-idx 12)) :y (- y 22)
                :text-anchor "middle" :fill p-color :font-size 16}
         "🐪"]
        [:rect {:x (+ (- x 15) (* p-idx 12)) :y (- y 18)
                :width 8 :height 3 :fill p-color :rx 1 :opacity 0.8}]])]))

;; ── Rendering: Player info ────────────────────────────────────────────────────

(def role-icons
  {:merchant "⚖" :priest "🏛" :raider "⚔" :leader "👑"})

(defn player-info-component [state my-player]
  [:div {:style {:display "flex" :gap 12 :flex-wrap "wrap" :margin "8px 0"}}
   (for [[pk pdata] (:players state)
         :let [is-current (= pk (game/current-player state))
               p-color (game/player-color state pk)]]
     ^{:key pk}
     [:div {:style {:background (if is-current "#0d1a0d" "#0a0a12")
                    :border (str "2px solid " (if is-current p-color "#222"))
                    :border-radius 8 :padding 10 :min-width 200 :font-size 11}}
      ;; Player name with color indicator
      [:div {:style {:display "flex" :align-items "center" :gap 6 :margin-bottom 6}}
       [:div {:style {:width 10 :height 10 :border-radius "50%"
                      :background p-color}}]
       [:span {:style {:color p-color :font-weight "bold" :font-size 13}}
        (str pk (when is-current " ✦"))]]
      ;; Caravan location
      [:div {:style {:color "#8aa" :margin-bottom 3}}
       (str "🐪 " (when (:caravan pdata) (str/capitalize (name (:caravan pdata)))))]
      ;; Resources with icons
      [:div {:style {:display "flex" :gap 8 :margin-bottom 3 :flex-wrap "wrap"}}
       (for [[r n] (:resources pdata) :when (pos? n)]
         ^{:key (str pk "-res-" (name r))}
         [:span {:style {:color (get game/resource-colors r "#888")
                         :background "#111" :padding "1px 5px" :border-radius 4
                         :border (str "1px solid " (get game/resource-colors r "#333"))}}
          (str (get game/resource-icons r "") " " n)])]
      ;; Roles with levels
      [:div {:style {:display "flex" :gap 6 :margin-bottom 3 :flex-wrap "wrap"}}
       (for [[r lv] (:roles pdata)]
         ^{:key (str pk "-role-" (name r))}
         [:span {:style {:color "#aaa" :background "#111" :padding "1px 5px"
                         :border-radius 4 :border "1px solid #333"}}
          (str (get role-icons r "") " " (subs (name r) 0 3) ":" lv)])]
      ;; Amity & Glory with visual bars
      [:div {:style {:display "flex" :gap 12 :margin-bottom 3}}
       [:span {:style {:color "#6ac"}}
        (str "♥ Amity:" (:amity pdata 0))]
       [:span {:style {:color "#ca6"}}
        (str "⚡ Glory:" (:glory pdata 0))]
       [:span {:style {:color "#e8e8e8" :font-weight "bold"}}
        (str "★ " (min (:amity pdata 0) (:glory pdata 0)))]]
      ;; Supply counts
      [:div {:style {:color "#555" :font-size 10}}
       (str "🏛×" (:temples-supply pdata 0)
            " ⚔×" (:raiders-supply pdata 0)
            " 🎲" (str/join "," (or (:dice-available pdata) [])))]])])

;; ── Rendering: Feat cards & Bonus boards ─────────────────────────────────────

;; ── Rendering: Bonus Contests ──────────────────────────────────────────────────

(def contest-category-icons
  {:fulfill "🌾" :temple "🏛" :raider "⚔" :magistrate "👑"
   :role "📜" :scoring "⭐" :sell "⚖" :resource "💎"})

(defn contests-component [state]
  (let [contests (:contests state [])
        claims (:contest-claims state {})]
    [:div {:style {:background "#0a0a12" :border "1px solid #333"
                   :border-radius 8 :padding 10 :margin "8px 0"}}
     [:div {:style {:color "#886622" :font-weight "bold" :font-size 13
                    :margin-bottom 8}}
      "✦ Bonus Contests"]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      (for [contest contests
            :let [contest-id (:id contest)
                  claimers (get claims contest-id [])
                  claimed? (seq claimers)
                  cat-icon (get contest-category-icons (:category contest) "📜")]]
        ^{:key (str "contest-" (name contest-id))}
        [:div {:style {:background (if claimed? "#1a1a0a" "#0e0e1e")
                       :border (str "1px solid " (if claimed? "#aa8" "#333"))
                       :border-radius 6 :padding 8 :min-width 140 :max-width 180}}
         ;; Category icon and contest ID
         [:div {:style {:display "flex" :justify-content "space-between"
                        :align-items "center" :margin-bottom 4}}
          [:span {:style {:font-size 16}} cat-icon]
          [:span {:style {:color "#555" :font-size 9}} (name contest-id)]]
         ;; Contest name
         [:div {:style {:color (if claimed? "#ee8" "#aaa") :font-weight "bold"
                        :font-size 10 :margin-bottom 2}}
          (:name contest)]
         ;; Description
         [:div {:style {:color "#666" :font-size 9 :margin-bottom 4}}
          (:description contest)]
         ;; Bonus point slots: 3, 2, 1, 1
         [:div {:style {:display "flex" :gap 3 :justify-content "center"}}
          (for [[idx bonus-val] (map-indexed vector game/bonus-contest-values)]
            ^{:key (str "cbonus-" (name contest-id) "-" idx)}
            [:div {:style {:width 20 :height 20 :border-radius 4
                           :display "flex" :align-items "center" :justify-content "center"
                           :font-size 9 :font-weight "bold"
                           :background (if (< idx (count claimers))
                                         (game/player-color state (nth claimers idx))
                                         "#1a1a2e")
                           :border (str "1px solid "
                                        (if (< idx (count claimers)) "#fff" "#333"))
                           :color "#fff"}}
             (str "+" bonus-val)])]
         ;; List claimers
         (when (seq claimers)
           [:div {:style {:color "#888" :font-size 9 :text-align "center" :margin-top 3}}
            (str/join ", " (map-indexed
                           (fn [i pk] (str (inc i) ". " pk))
                           claimers))])])]]))

;; ── Rendering: Bonus boards per player ────────────────────────────────────────

(defn bonus-boards-component [state]
  (let [board-assignments (:bonus-boards state {})]
    [:div {:style {:background "#0a0a12" :border "1px solid #333"
                   :border-radius 8 :padding 10 :margin "8px 0"}}
     [:div {:style {:color "#886622" :font-weight "bold" :font-size 13
                    :margin-bottom 8}}
      "✦ Player Bonus Boards"]
     [:div {:style {:display "flex" :gap 10 :flex-wrap "wrap"}}
      (for [[pk pdata] (:players state)
            :let [p-color (game/player-color state pk)
                  board-id (get board-assignments pk)
                  board (get game/bonus-boards-by-id board-id)
                  board-slots (:bonus-board pdata (vec (repeat 5 :covered)))
                  effects (or (:effects board) [])]]
        ^{:key (str "bboard-" pk)}
        [:div {:style {:background "#0e0e1e" :border (str "2px solid " p-color)
                       :border-radius 8 :padding 8 :min-width 220 :max-width 320}}
         ;; Header
         [:div {:style {:display "flex" :justify-content "space-between"
                        :align-items "center" :margin-bottom 6}}
          [:span {:style {:color p-color :font-weight "bold" :font-size 12}} pk]
          [:span {:style {:color "#555" :font-size 9}}
           (when board (str "Board " (:id board)))]]
         ;; Effect 1 (persistent ability) - always visible
         (when (seq effects)
           [:div {:style {:background "#111" :border "1px solid #2a2a1a"
                          :border-radius 4 :padding 6 :margin-bottom 4
                          :font-size 9 :color "#c8a832"}}
            [:div {:style {:font-size 8 :color "#666" :margin-bottom 2}} "⚡ PASSIVE"]
            (first effects)])
         ;; Effects 2-5 (one-time bonuses, uncovered by contests)
         [:div {:style {:display "flex" :flex-direction "column" :gap 3}}
          (for [[idx slot] (map-indexed vector board-slots)
                :let [effect-text (get effects (inc idx) "???")
                      uncovered? (not= slot :covered)]]
            ^{:key (str "bslot-" pk "-" idx)}
            [:div {:style {:background (if uncovered? "#1a1a0a" "#0a0a10")
                           :border (str "1px solid " (if uncovered? "#aa8" "#222"))
                           :border-radius 4 :padding "4px 6px"
                           :font-size 9
                           :color (if uncovered? "#dda" "#333")}}
             [:span {:style {:color (if uncovered? "#aa8" "#333")
                             :font-size 8 :margin-right 4}}
              (str (inc idx) ".")]
             (if uncovered?
               effect-text
               "▮▮▮▮▮▮▮▮▮")])]])]]))

;; ── Rendering: Choices ────────────────────────────────────────────────────────

(defn choices-panel [state my-player]
  (let [[phase choices] (when state (choice/find-state-raw state))
        is-my-turn (my-turn? state my-player)]
    [:div {:style {:margin "8px 0" :padding 12
                   :background "#0a0a12" :border-radius 8
                   :border (str "1px solid " (if is-my-turn "#4a4" "#333"))}}
     [:div {:style {:color "#888" :font-size 11 :margin-bottom 8
                    :display "flex" :gap 12 :align-items "center" :flex-wrap "wrap"}}
      (when (= :solo (:mode state))
        [:span {:style {:color "#88f" :font-weight "bold"
                        :background "#1a1a2a" :padding "2px 8px" :border-radius 4}}
         (str "SOLO — " (get game/solo-color-names (dec (:round state 1)) "?") " astronomers")])
      [:span {:style {:color "#886622" :font-weight "bold"}}
       (str "Round " (:round state 1) "/" game/rounds-per-game)]
      [:span (str "Turn " (:turn-in-round state 1) "/" game/turns-per-round)]
      [:span {:style {:color "#aaa"}}
       (str "Phase: " (when phase (name phase)))]
      [:span {:style {:color (if is-my-turn "#8f8" "#666")}}
       (str "Player: " (game/current-player state)
            (when-not is-my-turn " (waiting...)"))]]
     ;; Special dice display for choose-die phase
     (when (and is-my-turn (= phase :choose-die))
       [dice-display state my-player])
     ;; Action buttons
     (when (and is-my-turn (map? choices) (seq choices)
                (not= phase :choose-die)) ;; dice handled by dice-display
       [:div {:style {:display "flex" :gap 6 :flex-wrap "wrap"}}
        (for [[k _v] choices]
          ^{:key (pr-str k)}
          [:button
           {:on-click #(send-action! k)
            :style {:background "#1a2a1a" :color "#8f8"
                    :border "1px solid #4a4" :border-radius 6
                    :padding "6px 14px" :cursor "pointer"
                    :font-size 12 :font-family "monospace"
                    :transition "all 0.15s"}}
           (choice-label k)])])
     (when (and is-my-turn @can-undo?)
       [:button
        {:on-click send-undo!
         :style {:background "#1a1a1a" :color "#aa8"
                 :border "1px solid #553" :border-radius 6
                 :padding "4px 12px" :cursor "pointer"
                 :font-size 11 :margin-top 6}}
        "↩ undo"])]))

;; ── Create game form ──────────────────────────────────────────────────────────

(defonce create-state
  (r/atom {:play-name "" :players [""] :bots #{} :mode :normal}))

(defn create-form []
  (let [{:keys [play-name players bots]} @create-state]
    [:div {:style {:max-width 500 :margin "40px auto" :padding 20
                   :background "#0a0a12" :border-radius 8
                   :font-family "monospace" :color "#ccc"
                   :border "1px solid #333"}}
     [:h2 {:style {:color "#BB9944" :margin-bottom 16}} "✦ Create Eridu Game"]
     ;; Mode selector
     [:div {:style {:display "flex" :gap 8 :margin-bottom 12}}
      [:button {:on-click #(swap! create-state assoc :mode :normal :players [""])
                :style {:background (if (= (:mode @create-state) :normal) "#2a3a2a" "#111")
                        :color (if (= (:mode @create-state) :normal) "#8f8" "#666")
                        :border "1px solid #333" :border-radius 4
                        :padding "6px 14px" :cursor "pointer"}}
       "Multiplayer"]
      [:button {:on-click #(swap! create-state assoc :mode :solo :players [(or @player-key "")])
                :style {:background (if (= (:mode @create-state) :solo) "#2a2a3a" "#111")
                        :color (if (= (:mode @create-state) :solo) "#88f" "#666")
                        :border "1px solid #333" :border-radius 4
                        :padding "6px 14px" :cursor "pointer"}}
       "Solo"]
      [:a {:href "/eridu/stats"
           :style {:background "#111" :color "#a86" :border "1px solid #333"
                   :border-radius 4 :padding "6px 14px" :text-decoration "none"
                   :display "flex" :align-items "center"}}
       "Simulation & Stats →"]]
     [:div {:style {:margin-bottom 12}}
      [:label {:style {:color "#888" :display "block" :margin-bottom 4}} "Game name"]
      [:input {:type "text" :value play-name
               :on-change #(swap! create-state assoc :play-name (-> % .-target .-value))
               :style {:background "#111" :color "#ccc" :border "1px solid #334"
                       :border-radius 4 :padding "6px 10px" :width "100%"}}]]
     [:div {:style {:margin-bottom 12}}
      [:label {:style {:color "#888" :display "block" :margin-bottom 4}} "Players"]
      (for [[idx p] (map-indexed vector players)]
        ^{:key idx}
        [:div {:style {:display "flex" :gap 8 :margin-bottom 4}}
         [:input {:type "text" :value p
                  :on-change #(swap! create-state assoc-in [:players idx] (-> % .-target .-value))
                  :placeholder (str "Player " (inc idx))
                  :style {:background "#111" :color "#ccc" :border "1px solid #334"
                          :border-radius 4 :padding "6px 10px" :flex 1}}]
         [:label {:style {:color "#666" :font-size 12 :display "flex" :align-items "center" :gap 4}}
          [:input {:type "checkbox"
                   :checked (contains? bots p)
                   :on-change #(swap! create-state update :bots
                                      (if (contains? bots p) disj conj) p)}]
          "🤖 bot"]])
      [:button {:on-click #(swap! create-state update :players conj "")
                :style {:background "#1a2a1a" :color "#8a8" :border "1px solid #343"
                        :border-radius 4 :padding "4px 12px" :cursor "pointer"
                        :margin-top 4}}
       "+ add player"]]
     [:button
      {:on-click
       (fn []
         (let [{:keys [play-name players bots mode]} @create-state
               valid-players (filterv seq players)]
           (when (and (seq play-name) (seq valid-players))
             (POST "/eridu/create"
                   {:params {:play-name play-name
                             :players valid-players
                             :bots (vec (filter (set valid-players) bots))
                             :mode (name (or mode :normal))}
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
     [:h2 {:style {:color "#BB9944" :margin-bottom 16}} "✦ Observe Eridu Games"]
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

;; ── Rendering: Game log ────────────────────────────────────────────────────────

(def log-type-icons
  {:die "🎲" :astronomer "⭐" :landing "📍" :first-player "👑"
   :role-increase "📈" :action-select "▶" :take "🌾" :sell "⚖"
   :temple "🏛" :temple-visit "🏛" :deploy "⚔" :travel "🐪"
   :travel-extend "🐪" :influence "👑"
   :raider-flip "🔄" :raider-score "💀" :magistrate-raider-flip "👑"})

(def log-type-colors
  {:die "#aaf" :astronomer "#aaf" :landing "#aaa" :first-player "#FFD700"
   :role-increase "#af8" :action-select "#ccc" :take "#5B8C3E" :sell "#C4A535"
   :temple "#C45BA8" :temple-visit "#C45BA8" :deploy "#C44B35" :travel "#3581A8"
   :travel-extend "#3581A8" :influence "#8B5BC4"
   :raider-flip "#f84" :raider-score "#ff4" :magistrate-raider-flip "#f84"})

(defn game-log-component [state]
  (let [log (reverse (:log state []))
        current-round (:round state 1)
        current-turn (:turn-in-round state 1)]
    [:div {:style {:background "#0a0a12" :border "1px solid #333"
                   :border-radius 8 :padding 10
                   :width 320 :min-width 280
                   :max-height "calc(100vh - 40px)" :overflow-y "auto"
                   :flex-shrink 0}}
     [:div {:style {:color "#886622" :font-weight "bold" :font-size 13
                    :margin-bottom 8 :position "sticky" :top 0
                    :background "#0a0a12" :padding-bottom 4}}
      "✦ Game Log"]
     (if (empty? log)
       [:div {:style {:color "#444" :font-size 10}} "No actions yet..."]
       (let [prev-round-turn (atom nil)]
         (for [[idx entry] (map-indexed vector log)
               :let [round-turn [(:round entry) (:turn entry)]
                     show-header (not= round-turn @prev-round-turn)
                     _ (reset! prev-round-turn round-turn)
                     icon (get log-type-icons (:type entry) "•")
                     color (get log-type-colors (:type entry) "#888")
                     p-color (when (:player entry)
                               (game/player-color state (:player entry)))]]
           ^{:key (str "log-" idx)}
           [:div
            (when show-header
              [:div {:style {:color "#555" :font-size 9 :margin-top 6
                             :margin-bottom 2 :border-top "1px solid #222"
                             :padding-top 4}}
               (str "Round " (:round entry) " · Turn " (:turn entry))])
            [:div {:style {:display "flex" :gap 4 :margin-bottom 2
                           :font-size 10 :line-height "1.3"}}
             [:span {:style {:flex-shrink 0}} icon]
             [:div
              [:span {:style {:color p-color :font-weight "bold" :margin-right 4}}
               (:player entry)]
              [:span {:style {:color color}} (:message entry)]]]])))]))

;; ── Rendering: Rules reference ───────────────────────────────────────────────

(defn rules-reference []
  [:div {:style {:background "#0a0a12" :border "1px solid #333"
                 :border-radius 8 :padding 10 :margin-top 8
                 :width 320 :min-width 280 :flex-shrink 0}}
   [:div {:style {:color "#886622" :font-weight "bold" :font-size 13
                  :margin-bottom 8}}
    "✦ Quick Reference"]
   ;; Actions summary
   [:div {:style {:font-size 9 :color "#999" :line-height "1.5"}}
    [:div {:style {:color "#5B8C3E" :font-weight "bold" :margin-top 4}} "🌾 Take Goods"]
    [:div "Take both listed resources from the action space"]
    [:div {:style {:color "#C4A535" :font-weight "bold" :margin-top 4}} "⚖ Sell (Merchant)"]
    [:div "Discard a good matching city demand. Score Amity = Merchant level."]
    [:div {:style {:color "#666" :font-size 8}} "Merchant: lv1→2, lv2→3, lv3→4, lv4-5→5 Amity"]
    [:div {:style {:color "#C45BA8" :font-weight "bold" :margin-top 4}} "🏛 Temple (Priest)"]
    [:div "Place face-up in caravan's city or magistrate's city. When caravan visits: flip face-down, score Amity = # face-down temples."]
    [:div {:style {:color "#666" :font-size 8}} "Priest: lv1→3, lv2→4, lv3→5, lv4-5→8 max temples"]
    [:div {:style {:color "#C44B35" :font-weight "bold" :margin-top 4}} "⚔ Deploy (Raider)"]
    [:div "Place up to 2 raiders on routes next to caravan. Raiding side up. Opposing caravans & magistrates flip to point side. Score 4 Glory when your caravan crosses own point raider."]
    [:div {:style {:color "#666" :font-size 8}} "Raider: lv1→2, lv2→3, lv3→4, lv4-5→6 max deployed"]
    [:div {:style {:color "#3581A8" :font-weight "bold" :margin-top 4}} "🐪 Travel"]
    [:div "Move caravan 1 space (road or river). May discard 1 good to move again. Visits own face-up temples. Flips enemy raiders, scores own point raiders."]
    [:div {:style {:color "#8B5BC4" :font-weight "bold" :margin-top 4}} "👑 Influence (Leader)"]
    [:div "Move magistrate clockwise on roads (up to Leader level spaces). Flips raiders passed through. Sell/temple in magistrate city = bonus Glory."]
    [:div {:style {:color "#666" :font-size 8}} "Leader: lv1→1, lv2→2, lv3→4, lv4→5, lv5→5 spaces. Bonus: lv1-2→1, lv3-4→2, lv5→3 Glory"]
    [:div {:style {:color "#aa8" :font-weight "bold" :margin-top 6}} "Scoring"]
    [:div "Reputation = min(Amity, Glory). Highest reputation wins."]
    [:div {:style {:color "#666" :font-size 8}} "Amity: selling, visiting temples | Glory: scoring raiders (4 each), magistrate bonuses"]
    [:div {:style {:color "#aa8" :font-weight "bold" :margin-top 6}} "Role Thresholds"]
    [:div {:style {:color "#666" :font-size 8}}
     "Merchant: lv3=pottery, lv4=gold | Priest: lv3=tools, lv4=gems"]
    [:div {:style {:color "#666" :font-size 8}}
     "Raider: lv3=gold, lv4=tools | Leader: lv3=gems, lv4=pottery"]]])

;; ── Main game view ────────────────────────────────────────────────────────────

(defn game-view []
  (let [state @game-state
        my-player @player-key]
    (if state
      [:div {:style {:padding 12 :font-family "monospace"
                     :background "#050510" :min-height "100vh"}}
       (when (:game-over state)
         [:div {:style {:background (if (= :victory (get-in state [:game-over :solo-result]))
                                      "#1a2a1a" "#2a1a1a")
                        :border (str "2px solid "
                                     (case (get-in state [:game-over :solo-result])
                                       :victory "#4a4" :defeat "#844" "#844"))
                        :border-radius 8 :padding 16 :margin-bottom 12
                        :text-align "center"}}
          [:div {:style {:font-size 20 :font-weight "bold"
                         :color (case (get-in state [:game-over :solo-result])
                                  :victory "#8f8" :defeat "#faa" "#faa")}}
           (case (get-in state [:game-over :solo-result])
             :victory "✦ Victory! ✦"
             :defeat  "✦ Defeat ✦"
             "✦ Game Over ✦")]
          (when (get-in state [:game-over :solo-result])
            [:div {:style {:color "#aaa" :font-size 12 :margin-top 4}}
             (str "Feats completed: " (get-in state [:game-over :feats-met])
                  "/" (get-in state [:game-over :feats-needed]))])
          [:div {:style {:margin-top 8 :color "#ccc"}}
           (let [scores (for [[pk pdata] (:players state)]
                          {:player pk
                           :amity (:amity pdata 0)
                           :glory (:glory pdata 0)
                           :reputation (min (:amity pdata 0) (:glory pdata 0))})
                 sorted (sort-by #(- (:reputation %)) scores)
                 winner (first sorted)]
             [:div
              [:div {:style {:font-size 16 :color "#FFD700" :margin-bottom 8}}
               (str "👑 Winner: " (:player winner)
                    " (Reputation: " (:reputation winner) ")")]
              [:div {:style {:display "flex" :gap 12 :justify-content "center"
                             :flex-wrap "wrap" :margin-top 8}}
               (for [{:keys [player amity glory reputation]} sorted]
                 ^{:key player}
                 [:div {:style {:background "#111" :padding "6px 12px"
                                :border-radius 6
                                :border (str "1px solid "
                                             (game/player-color state player))
                                :color "#ccc" :font-size 11}}
                  [:div {:style {:color (game/player-color state player)
                                 :font-weight "bold"}} player]
                  (str "♥" amity " ⚡" glory " ★" reputation)])]])]])
       ;; Main two-column layout: game area left, log right
       [:div {:style {:display "flex" :gap 12 :align-items "flex-start"}}
        ;; Left column: boards, player info, choices
        [:div {:style {:flex 1 :min-width 0}}
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
         [choices-panel state my-player]
         [contests-component state]
         [bonus-boards-component state]]
        ;; Right column: game log and rules reference
        [:div {:style {:flex-shrink 0}}
         [game-log-component state]
         [rules-reference]]]]
      [:div {:style {:color "#666" :padding 40 :font-family "monospace"
                     :text-align "center"}}
       [:div {:style {:font-size 24 :margin-bottom 8}} "✸"]
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
