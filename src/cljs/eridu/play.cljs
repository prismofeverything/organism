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
(defonce pending-claim  (r/atom nil))

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

(defn send-claim-feat!
  ([feat-id] (send-claim-feat! feat-id nil))
  ([feat-id slot-idx]
   (ws/send-transit-message! (cond-> {:type "claim-feat" :feat-id (name feat-id)}
                               slot-idx (assoc :slot-idx slot-idx)))))

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

(def action-board-cx 280)
(def action-board-cy 280)
(def action-board-r 190)

(defn action-space-pos [space-id]
  (let [angle (- (* (/ (* 2 js/Math.PI) 7) (dec space-id)) (/ js/Math.PI 2))
        x (+ action-board-cx (* action-board-r (js/Math.cos angle)))
        y (+ action-board-cy (* action-board-r (js/Math.sin angle)))]
    {:x x :y y}))

(def action-type-colors
  {:take "#5B8C3E" :sell "#C4A535"  :deploy "#C44B35"
   :travel "#3581A8" :influence "#8B5BC4" :temple "#C45BA8"})

(defn action-board-component [state my-player]
  (let [current-space (get-in state [:player-turn :landed-space])
        landed-space (or current-space
                         (get-in state [:player-turn :space]))
        phase (game/current-phase state)
        player (game/current-player state)
        p-color (game/player-color state player)
        my-turn (my-turn? state my-player)
        [_ choices] (when state (choice/find-state-raw state))
        ;; In choose-astronomer, highlight my eligible astronomers
        astronomer-choosable (when (and my-turn (= phase :choose-astronomer))
                               (set (keys choices)))
        ;; In choose-action, highlight eligible action indices on landed space
        action-choosable (when (and my-turn (= phase :choose-action))
                           (set (filter integer? (keys choices))))]
    [:svg {:viewBox "0 0 560 560" :width 540 :height 540
           :style {:background "radial-gradient(circle, #0d0d1e, #050510)"
                   :background-color "#070712"
                   :border-radius 8 :border (str "1px solid #333")}}
     ;; Title with star
     [:text {:x 280 :y 26 :text-anchor "middle" :fill "#886622" :font-size 16
             :font-weight "bold"}
      "✦ Astronomy Board ✦"]
     ;; Center star decoration
     [:text {:x 280 :y 285 :text-anchor "middle" :fill "#221a00" :font-size 60}
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
          [:circle {:cx x :cy y :r 58 :fill "none"
                    :stroke "#aa8833" :stroke-width 2 :opacity 0.6
                    :stroke-dasharray "3,3"}])
        ;; Main circle
        [:circle {:cx x :cy y :r 50
                  :fill (if is-landed "#1a1a10" "#0e0e1e")
                  :stroke (if is-landed "#aa8833" "#334")
                  :stroke-width (if is-landed 2.5 1.5)}]
        ;; Space number
        [:text {:x x :y (- y 24) :text-anchor "middle"
                :fill (if is-landed "#ee8" "#777") :font-size 15 :font-weight "bold"}
         (if (= space-id 7) "★" (str space-id))]
        ;; Action icons in a grid
        (let [n (count actions)
              cols (if (= n 4) 2 2)
              rows (js/Math.ceil (/ n cols))]
          (for [[idx action] (map-indexed vector actions)
                :let [col (mod idx cols)
                      row (quot idx cols)
                      ax (+ x (* (- col (/ (dec cols) 2)) 24))
                      ay (+ y -4 (* row 22))
                      atype (:type action)
                      color (get action-type-colors atype "#666")
                      icon (get game/action-icons atype "?")
                      resources (:resources action)
                      ;; Is this action clickable right now?
                      clickable? (and (= space-id landed-space)
                                      (contains? (or action-choosable #{}) idx))]]
            ^{:key (str "act-" space-id "-" idx)}
            [:g (cond-> {:style {:cursor (when clickable? "pointer")}}
                  clickable? (assoc :on-click #(send-action! idx)))
             ;; Clickable background circle (bigger hit target + glow)
             (when clickable?
               [:circle {:cx ax :cy (- ay 4) :r 16
                         :fill "#1a3a1a" :stroke "#5f5" :stroke-width 2
                         :opacity 0.8}])
             (if (and (= atype :take) resources)
               ;; Take action: show the two resource icons
               (for [[ri res] (map-indexed vector resources)
                     :let [rx (+ ax (* (- ri 0.5) 14))
                           res-icon (get game/resource-icons res "?")
                           res-color (get game/resource-colors res "#888")]]
                 ^{:key (str "take-res-" space-id "-" idx "-" ri)}
                 [:text {:x rx :y ay :text-anchor "middle"
                         :fill (if clickable? "#fff" res-color) :font-size 16
                         :style {:pointer-events "none"}}
                  res-icon])
               ;; Other actions: use action icon
               [:text {:x ax :y ay :text-anchor "middle"
                       :fill (if clickable? "#fff" color)
                       :font-size 22
                       :style {:pointer-events "none"}}
                icon])]))
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
                      is-active (or (not solo?) (contains? active-pair astro-idx))
                      ;; Is this astronomer clickable right now?
                      clickable? (and (= pk my-player)
                                      (contains? (or astronomer-choosable #{}) astro-idx))
                      cx (+ x -16 (* didx 14))
                      cy (+ y 34)
                      glyph (if solo?
                              (let [pair-idx (some (fn [[pi pair]]
                                                     (when (some #{astro-idx} pair) pi))
                                                   (map-indexed vector solo-pairs))]
                                (get ["α" "β" "γ"] (or pair-idx 0) "?"))
                              "★")]]
            ^{:key (str "astro-" space-id "-" didx)}
            [:g (cond-> {:style {:cursor (when clickable? "pointer")}}
                  clickable? (assoc :on-click #(send-action! astro-idx)))
             ;; Pulsing halo when clickable
             (when clickable?
               [:circle {:cx cx :cy cy :r 14
                         :fill "none" :stroke "#8f8" :stroke-width 2
                         :opacity 0.85
                         :stroke-dasharray "3,2"}])
             ;; Main astronomer disc
             [:circle {:cx cx :cy cy :r 9
                       :fill acolor
                       :stroke (cond clickable? "#8f8"
                                     is-active "#fff"
                                     :else "#444")
                       :stroke-width (if (or clickable? is-active) 1.5 0.5)
                       :opacity (if is-active 1.0 0.4)}]
             [:text {:x cx :y (+ cy 3)
                     :text-anchor "middle"
                     :fill (cond clickable? "#fff"
                                 is-active "#fff"
                                 :else "#666")
                     :font-size 10 :font-weight "bold"
                     :style {:pointer-events "none"}}
              glyph]]))])
     ;; Legend
     (let [legend-items [[:take "🌾 Take"] [:sell "⚖ Sell"] [:deploy "⚔ Deploy"]
                         [:travel "🐪 Travel"] [:influence "👑 Influence"] [:temple "🏛 Temple"]]]
       (for [[idx [atype label]] (map-indexed vector legend-items)]
         ^{:key (str "legend-" idx)}
         [:text {:x (+ 40 (* (mod idx 3) 170))
                 :y (+ 536 (* (quot idx 3) 18))
                 :fill (get action-type-colors atype "#666")
                 :font-size 13}
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
        current-player-key (game/current-player state)
        ;; Identify route choices: keys are vectors [city1 city2]
        route-choices (when choices
                        (set (filter #(and (vector? %) (= 2 (count %))
                                           (keyword? (first %))
                                           (keyword? (second %))
                                           (not= (first %) (second %)))
                                     (keys choices))))
        ;; Identify influence choices: keys are [mag-city dest steps]
        ;; Map destination city → best (longest) choice key
        influence-by-dest
        (when choices
          (reduce (fn [m k]
                    (if (and (vector? k) (= 3 (count k)) (number? (nth k 2)))
                      (let [dest (second k)]
                        (if (or (not (contains? m dest))
                                (> (nth k 2) (nth (get m dest) 2)))
                          (assoc m dest k)
                          m))
                      m))
                  {}
                  (keys choices)))
        ;; City-level choices (travel, temple, sell-city)
        city-choices (when choices
                       (set (filter keyword? (keys choices))))]
    [:svg {:viewBox "0 0 500 470" :width 540 :height 510
           :style {:background "linear-gradient(180deg, #080812, #0a0a1a)"
                   :background-color "#090914"
                   :border-radius 8 :border "1px solid #333"}}
     [:text {:x 250 :y 22 :text-anchor "middle" :fill "#886622" :font-size 15
             :font-weight "bold"}
      "✦ City Board ✦"]
     ;; Route lines (with click for deploy)
     (for [{:keys [from to type]} routes
           :let [{x1 :x y1 :y} (get city-positions from)
                 {x2 :x y2 :y} (get city-positions to)
                 rk1 [from to]
                 rk2 [to from]
                 selectable-rk (cond
                                 (contains? (or route-choices #{}) rk1) rk1
                                 (contains? (or route-choices #{}) rk2) rk2)]]
       ^{:key (str "route-" (name from) "-" (name to))}
       [:g
        ;; Wider invisible click target
        (when selectable-rk
          [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                  :stroke "#5a5" :stroke-width 12 :opacity 0.25
                  :on-click #(send-action! selectable-rk)
                  :style {:cursor "pointer"}}])
        [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                :stroke (cond selectable-rk "#5a5"
                              (= type :river) "#1a4a6a"
                              :else "#4a3a1a")
                :stroke-width (cond selectable-rk 4
                                    (= type :river) 3
                                    :else 2.5)
                :stroke-dasharray (when (= type :river) "8,4")
                :opacity (if selectable-rk 0.95 0.7)
                :style (when selectable-rk {:cursor "pointer"
                                             :filter "drop-shadow(0 0 3px #8f8)"})
                :on-click (when selectable-rk #(send-action! selectable-rk))}]])
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
           [:rect {:x (- mx 12) :y (- my 12) :width 24 :height 24
                   :fill p-color :rx 4 :stroke "#fff" :stroke-width 1.5 :opacity 0.9}]
           [:text {:x mx :y (+ my 6) :text-anchor "middle" :fill "#fff" :font-size 17}
            "⚔"]]
          ;; Point state: flag/banner (flipped, ready to score)
          [:g
           [:rect {:x (- mx 12) :y (- my 12) :width 24 :height 24
                   :fill "#111" :rx 4 :stroke p-color :stroke-width 2}]
           [:text {:x mx :y (+ my 6) :text-anchor "middle" :fill p-color :font-size 16}
            "🏴"]])])
     ;; Magistrates on cities (offset when stacked)
     (let [mag-by-city (reduce (fn [m [c _]] (update m c (fnil inc 0)))
                               {} (:magistrates state))
           mag-offset (atom {})] ;; track how many drawn per city
       (for [[mag-city _owner] (:magistrates state)
             :let [{:keys [x y]} (get city-positions mag-city)
                   idx (get (swap! mag-offset update mag-city (fnil inc 0)) mag-city)
                   offset-x (* (dec idx) 22)]
             :when (get city-positions mag-city)]
         ^{:key (str "magistrate-" (name mag-city) "-" idx)}
         [:g
          ;; Gold crown glow
          [:circle {:cx (+ x offset-x) :cy (- y 30) :r 14
                    :fill "#2a2210" :stroke "#C4A535" :stroke-width 1.5}]
          [:text {:x (+ x offset-x) :y (- y 24) :text-anchor "middle"
                  :fill "#FFD700" :font-size 20}
           "👑"]]))
     ;; Cities
     (for [city cities
           :let [{:keys [x y]} (get city-positions city)
                 demands (get-in state [:city-demands city] [])
                 has-magistrate (game/magistrate-in-city? state city)
                 ;; City clickable via direct choice OR influence destination
                 is-direct-choice? (and choices (contains? choices city))
                 influence-key (get influence-by-dest city)
                 is-influence-dest? (some? influence-key)
                 is-clickable? (or is-direct-choice? is-influence-dest?)
                 click-action (cond is-direct-choice? city
                                    is-influence-dest? influence-key)]]
       ^{:key (str "city-" (name city))}
       [:g {:on-click (when is-clickable? #(send-action! click-action))
            :style (when is-clickable? {:cursor "pointer"})}
        ;; City rectangle
        [:rect {:x (- x 42) :y (- y 18) :width 84 :height 36
                :rx 6 :fill (cond is-clickable? "#1a2a1a"
                                  has-magistrate "#1c1c10"
                                  :else "#0e0e1e")
                :stroke (cond is-clickable? "#5a5"
                              has-magistrate "#C4A535"
                              :else "#334")
                :stroke-width (if (or is-clickable? has-magistrate) 2 1.5)}]
        ;; City name
        [:text {:x x :y (+ y 4) :text-anchor "middle"
                :fill (cond is-clickable? "#8f8"
                            has-magistrate "#FFD700"
                            :else "#ccc")
                :font-size 13 :font-weight "bold"}
         (str/capitalize (name city))]
        ;; Demand tokens with resource icons
        (for [[idx token] (map-indexed vector demands)]
          ^{:key (str "demand-" (name city) "-" idx)}
          [:g
           [:circle {:cx (+ (- x 22) (* idx 18)) :cy (+ y 18) :r 8
                     :fill (get game/resource-colors token "#444")
                     :stroke "#fff" :stroke-width 0.8 :opacity 0.9}]
           [:text {:x (+ (- x 22) (* idx 18)) :cy (+ y 21)
                   :y (+ y 21) :text-anchor "middle" :fill "#fff" :font-size 10}
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
                   :font-size 18
                   :opacity (if is-face-up 1.0 0.5)}
            "🏛"]
           (when is-face-up
             [:circle {:cx (+ x 40) :cy (- y 10) :r 4
                       :fill p-color :stroke "#fff" :stroke-width 0.5}])])])
     ;; Caravans with player colors
     (for [[pk pdata] (:players state)
           :let [city (:caravan pdata)
                 {:keys [x y]} (get city-positions city)
                 p-color (game/player-color state pk)
                 p-idx (.indexOf (:turn-order state) pk)]]
       ^{:key (str "caravan-" pk)}
       [:g
        ;; Colored background pill for caravan
        [:rect {:x (+ (- x 20) (* p-idx 18)) :y (- y 32)
                :width 22 :height 22 :rx 11
                :fill p-color :opacity 0.25
                :stroke p-color :stroke-width 1.5}]
        [:text {:x (+ (- x 10) (* p-idx 18)) :y (- y 18)
                :text-anchor "middle" :fill p-color :font-size 18}
         "🐪"]])]))

;; ── Rendering: Player info ────────────────────────────────────────────────────

(def role-icons
  {:merchant "⚖" :priest "🏛" :raider "⚔" :leader "👑"})

(def role-track-colors
  {:merchant "#6ac" :priest "#6ac" :raider "#ca6" :leader "#ca6"})

(def role-bonus-descriptions
  "What each level grants, per role."
  {:merchant {1 "2" 2 "3" 3 "4" 4 "5" 5 "5"}
   :priest   {1 "3" 2 "4" 3 "5" 4 "8" 5 "8"}
   :raider   {1 "2" 2 "3" 3 "4" 4 "6" 5 "6"}
   :leader   {1 "1/1" 2 "2/1" 3 "4/2" 4 "5/2" 5 "5/3"}})

(def role-bonus-labels
  {:merchant "Amity/sell" :priest "max temples" :raider "max raiders" :leader "move/glory"})

;; End-game lv5 bonus track (opposite of in-game scoring per game.cljc/role-end-game-bonus)
(def role-track-label
  {:merchant "Glory" :priest "Glory" :raider "Amity" :leader "Amity"})

(defn role-table-component
  "Consolidated role table showing level track, thresholds, bonuses, and current level.
   When in choose-role-increase phase, available roles are clickable."
  [pk pdata p-color state my-player]
  (let [roles (:roles pdata)
        phase (game/current-phase state)
        is-my-turn (and (= pk (game/current-player state)) (= pk my-player))
        role-choosable? (and is-my-turn (= phase :choose-role-increase))
        [_ role-choices] (when role-choosable? (choice/find-state-raw state))]
    [:table {:style {:border-collapse "collapse" :width "100%" :font-size 12
                     :margin "6px 0"}}
     [:thead
      [:tr {:style {:border-bottom "1px solid #333"}}
       [:th {:style {:text-align "left" :padding "4px 6px" :color "#888" :font-size 11}} "Role"]
       (for [lv (range 1 6)]
         ^{:key (str pk "-th-" lv)}
         [:th {:style {:text-align "center" :padding "4px 4px" :color "#888" :font-size 11
                        :min-width 44}}
          (str "Lv" lv)])
       [:th {:style {:text-align "center" :padding "4px 6px" :color "#888" :font-size 11}} "Lv5 End"]]]
     [:tbody
      (for [role [:merchant :priest :raider :leader]
            :let [current-lv (get roles role 1)
                  icon (get role-icons role "?")
                  track-color (get role-track-colors role "#888")
                  thresholds (get game/role-threshold-costs role {})
                  bonus-map (get role-bonus-descriptions role {})
                  end-bonus (get game/role-end-game-bonus role)
                  can-increase? (and role-choices (contains? role-choices role))]]
        ^{:key (str pk "-role-" (name role))}
        [:tr (cond-> {:style (cond-> {:border-bottom "1px solid #1a1a2e"}
                               can-increase? (assoc :background "#0a2a0a"
                                                     :cursor "pointer"))}
               can-increase? (assoc :on-click #(send-action! role)))
         [:td {:style {:padding "5px 6px" :white-space "nowrap"}}
          [:span {:style {:font-size 18 :margin-right 4}} icon]
          [:span {:style {:color "#ccc" :font-weight "bold" :font-size 12}}
           (str/capitalize (name role))]
          [:div {:style {:color "#555" :font-size 9 :margin-top 1}}
           (get role-bonus-labels role "")]]
         (for [lv (range 1 6)
               :let [is-current (= lv current-lv)
                     is-past (< lv current-lv)
                     threshold-cost (get thresholds lv)
                     cost-resources (cond
                                      (nil? threshold-cost) []
                                      (vector? threshold-cost) threshold-cost
                                      :else [threshold-cost])
                     bonus-val (get bonus-map lv "")]]
           ^{:key (str pk "-" (name role) "-lv" lv)}
           [:td {:style {:text-align "center" :padding "4px 3px" :position "relative"
                         :background (cond is-current "#1a2a1a"
                                           is-past "#0d1a0d"
                                           :else "transparent")
                         :border (if is-current
                                   (str "2px solid " p-color)
                                   "1px solid transparent")
                         :border-radius 4}}
            [:div {:style {:font-size 14 :font-weight "bold"
                           :color (cond is-current "#fff"
                                        is-past track-color
                                        :else "#444")}}
             bonus-val]
            (when (seq cost-resources)
              [:div {:style {:font-size 12 :margin-top 1
                              :display "flex" :gap 2 :justify-content "center"}}
               (for [cr cost-resources]
                 ^{:key (str pk "-" (name role) "-lv" lv "-cost-" (name cr))}
                 [:span {:style {:color (get game/resource-colors cr "#888")
                                  :font-size 12}}
                  (get game/resource-icons cr "?")])])])
         [:td {:style {:text-align "center" :padding "4px 6px"
                        :color (if (= 5 current-lv) "#ff8" "#555")
                        :font-size 11 :font-weight "bold"}}
          (str "+10 " (get role-track-label role ""))]])]]))

(defn player-info-component [state my-player]
  [:div {:style {:display "flex" :gap 12 :flex-wrap "wrap" :margin "8px 0"}}
   (for [[pk pdata] (:players state)
         :let [is-current (= pk (game/current-player state))
               p-color (game/player-color state pk)]]
     ^{:key pk}
     [:div {:style {:background (if is-current "#0d1a0d" "#0a0a12")
                    :border (str "2px solid " (if is-current p-color "#222"))
                    :border-radius 8 :padding 12 :min-width 360 :font-size 13}}
      ;; Player name with color indicator
      [:div {:style {:display "flex" :align-items "center" :gap 8 :margin-bottom 8}}
       [:div {:style {:width 14 :height 14 :border-radius "50%"
                      :background p-color}}]
       [:span {:style {:color p-color :font-weight "bold" :font-size 16}}
        (str pk (when is-current " ✦"))]
       [:span {:style {:color "#8aa" :font-size 13 :margin-left 8}}
        (str "🐪 " (when (:caravan pdata) (str/capitalize (name (:caravan pdata)))))]]
      ;; Scoring bar
      [:div {:style {:display "flex" :gap 16 :margin-bottom 8 :font-size 15
                     :align-items "center"}}
       [:span {:style {:color "#6ac" :font-size 15}}
        (str "♥ " (:amity pdata 0))]
       [:span {:style {:color "#ca6" :font-size 15}}
        (str "⚡ " (:glory pdata 0))]
       [:span {:style {:color "#e8e8e8" :font-weight "bold" :font-size 16
                        :background "#1a1a2e" :padding "2px 10px" :border-radius 4
                        :border "1px solid #444"}}
        (str "★ " (min (:amity pdata 0) (:glory pdata 0)))]]
      ;; Resources with icons
      [:div {:style {:display "flex" :gap 10 :margin-bottom 8 :flex-wrap "wrap"}}
       (for [[r n] (:resources pdata) :when (pos? n)]
         ^{:key (str pk "-res-" (name r))}
         [:span {:style {:color (get game/resource-colors r "#888")
                         :background "#111" :padding "3px 8px" :border-radius 4
                         :border (str "1px solid " (get game/resource-colors r "#333"))
                         :font-size 14}}
          (str (get game/resource-icons r "") " " n)])]
      ;; Role table (clickable during choose-role-increase)
      [role-table-component pk pdata p-color state my-player]
      ;; Demand tokens earned (goods sold)
      (let [demands (:demand-tokens pdata [])]
        (when (seq demands)
          [:div {:style {:display "flex" :gap 4 :margin-top 4 :flex-wrap "wrap"
                         :align-items "center"}}
           [:span {:style {:color "#888" :font-size 11 :margin-right 4}} "Sold:"]
           (for [[idx d] (map-indexed vector demands)]
             ^{:key (str pk "-demand-" idx)}
             [:span {:style {:background (get game/resource-colors d "#444")
                             :color "#fff" :padding "2px 6px" :border-radius 10
                             :font-size 11 :opacity 0.9}}
              (get game/resource-icons d "?")])]))
      ;; Supply counts + wild points
      [:div {:style {:color "#777" :font-size 12 :margin-top 6
                     :display "flex" :gap 14}}
       [:span (str "🏛 ×" (:temples-supply pdata 0))]
       [:span (str "⚔ ×" (:raiders-supply pdata 0))]
       (when (pos? (:wild-points pdata 0))
         [:span {:style {:color "#ff8" :font-weight "bold"}}
          (str "★ " (:wild-points pdata 0) " wild")])]])])

;; ── Rendering: Feat cards & Bonus boards ─────────────────────────────────────

;; ── Rendering: Bonus Contests ──────────────────────────────────────────────────

(def contest-category-icons
  {:fulfill "🌾" :temple "🏛" :raider "⚔" :magistrate "👑"
   :role "📜" :scoring "⭐" :sell "⚖" :resource "💎"})

(defn contests-component [state]
  (let [contests (:contests state [])
        claims (:contest-claims state {})
        my-player @player-key
        ;; Has the player already claimed each feat?
        my-claimed? (fn [cid] (some #{my-player} (get claims cid [])))
        ;; Does the current state meet the feat condition?
        condition-met? (fn [c] (try (game/evaluate-contest state my-player c)
                                    (catch :default _ false)))
        ;; Does the player have a covered slot to uncover?
        my-bonus-board (get-in state [:players my-player :bonus-board]
                               (vec (repeat 5 :covered)))
        has-covered-slot? (some #(= % :covered) (rest my-bonus-board))]
    [:div {:style {:background "#0a0a12" :border "1px solid #333"
                   :border-radius 8 :padding 12 :margin "8px 0"}}
     [:div {:style {:color "#886622" :font-weight "bold" :font-size 15
                    :margin-bottom 10}}
      "✦ Bonus Contests"
      [:span {:style {:color "#888" :font-weight "normal" :font-size 12 :margin-left 12}}
       "(click a glowing feat to claim it)"]]
     [:div {:style {:display "flex" :gap 10 :flex-wrap "wrap"}}
      (for [contest contests
            :let [contest-id (:id contest)
                  claimers (get claims contest-id [])
                  claimed? (seq claimers)
                  cat-icon (get contest-category-icons (:category contest) "📜")
                  ;; Highlight if I can claim it now
                  claimable? (and (not (my-claimed? contest-id))
                                  has-covered-slot?
                                  (condition-met? contest))]]
        ^{:key (str "contest-" (name contest-id))}
        [:div {:on-click (when claimable?
                           #(send-claim-feat! contest-id))
               :style (cond-> {:background (cond claimable? "#1a3a1a"
                                                 claimed? "#1a1a0a"
                                                 :else "#0e0e1e")
                               :border (str "2px solid "
                                            (cond claimable? "#5f5"
                                                  claimed? "#aa8"
                                                  :else "#333"))
                               :border-radius 6 :padding 10
                               :min-width 170 :max-width 220
                               :transition "all 0.2s"}
                        claimable? (assoc :cursor "pointer"
                                          :box-shadow "0 0 12px rgba(85,255,85,0.7)"))}
         ;; Category icon and contest ID
         [:div {:style {:display "flex" :justify-content "space-between"
                        :align-items "center" :margin-bottom 6}}
          [:span {:style {:font-size 22}} cat-icon]
          [:span {:style {:color (if claimable? "#8f8" "#666") :font-size 11
                          :font-weight (if claimable? "bold" "normal")}}
           (if claimable? "✦ CLAIM ✦" (name contest-id))]]
         ;; Contest name
         [:div {:style {:color (cond claimable? "#dfd"
                                     claimed? "#ee8"
                                     :else "#aaa")
                        :font-weight "bold"
                        :font-size 13 :margin-bottom 3}}
          (:name contest)]
         ;; Description
         [:div {:style {:color (if claimable? "#aca" "#888") :font-size 11 :margin-bottom 6}}
          (:description contest)]
         ;; Bonus point slots: 3, 2, 1, 1
         [:div {:style {:display "flex" :gap 4 :justify-content "center"}}
          (for [[idx bonus-val] (map-indexed vector game/bonus-contest-values)]
            ^{:key (str "cbonus-" (name contest-id) "-" idx)}
            [:div {:style {:width 26 :height 26 :border-radius 4
                           :display "flex" :align-items "center" :justify-content "center"
                           :font-size 12 :font-weight "bold"
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
  (let [board-assignments (:bonus-boards state {})
        claim @pending-claim
        my-player @player-key
        ;; Is there a pending claim for this player?
        claiming? (and claim (= (:player claim) my-player))]
    [:div {:style {:background "#0a0a12" :border "1px solid #333"
                   :border-radius 8 :padding 12 :flex 1 :min-width 480}}
     [:div {:style {:color "#886622" :font-weight "bold" :font-size 15
                    :margin-bottom 10}}
      "✦ Bonus Board"
      (when claiming?
        [:span {:style {:color "#8f8" :font-size 12 :margin-left 12
                        :font-weight "normal"}}
         (str "Select a slot to uncover for feat " (name (:contest-id claim)))])]
     (for [[pk pdata] (:players state)
           :let [p-color (game/player-color state pk)
                 board-id (get board-assignments pk)
                 board (get game/bonus-boards-by-id board-id)
                 full-slots (:bonus-board pdata (vec (repeat 5 :covered)))
                 effects (or (:effects board) [])
                 is-my-board (= pk my-player)]]
       ^{:key (str "bboard-" pk)}
       [:div {:style {:background "#0e0e1e" :border (str "2px solid " p-color)
                      :border-radius 8 :padding 10 :margin-bottom 8}}
        ;; Header
        [:div {:style {:display "flex" :justify-content "space-between"
                       :align-items "center" :margin-bottom 8}}
         [:span {:style {:color p-color :font-weight "bold" :font-size 14}} pk]
         [:span {:style {:color "#777" :font-size 11}}
          (when board (str (:name board) " (#" (:id board) ")"))]]
        ;; HORIZONTAL strip layout (matches physical player board image)
        [:div {:style {:display "flex" :gap 4}}
         (for [[idx slot] (map-indexed vector full-slots)
               :let [effect-text (get effects idx "—")
                     uncovered? (not= slot :covered)
                     is-passive (= idx 0)
                     ;; Slot is selectable if: my board, claiming, covered (including passive)
                     selectable? (and claiming? is-my-board
                                      (= slot :covered))]]
           ^{:key (str "bslot-" pk "-" idx)}
           [:div (cond-> {:style (cond-> {:flex 1
                                          :border-radius 5 :padding "6px 8px"
                                          :font-size 11 :min-height 90
                                          :transition "all 0.2s"}
                                   is-passive
                                   (assoc :background "#1a1a0a"
                                          :border "1px solid #4a4020"
                                          :color "#c8a832")
                                   uncovered?
                                   (assoc :background "#1a2a0a"
                                          :border "1px solid #5a7"
                                          :color "#dda")
                                   selectable?
                                   (assoc :background "#0a2a0a"
                                          :border "2px solid #5f5"
                                          :color "#cfc"
                                          :cursor "pointer"
                                          :box-shadow "0 0 8px rgba(85,255,85,0.5)")
                                   (and (not is-passive) (not uncovered?) (not selectable?))
                                   (assoc :background "#0a0a10"
                                          :border "1px solid #1a1a2e"
                                          :color "#666"
                                          :opacity 0.85))}
                   selectable?
                   (assoc :on-click #(send-claim-feat! (:contest-id claim) idx)))
            [:div {:style {:font-size 9
                           :color (cond is-passive "#aa8"
                                        selectable? "#8f8"
                                        uncovered? "#7a5"
                                        :else "#555")
                           :font-weight "bold" :margin-bottom 3}}
             (cond is-passive "⚡ PASSIVE"
                   selectable? "⬆ SELECT"
                   uncovered? (str "✓ #" idx)
                   :else (str "#" idx))]
            [:div {:style {:font-size 10 :line-height "1.3"}}
             effect-text]])]])]))

;; ── Rendering: Choices ────────────────────────────────────────────────────────

(def ^:private board-handled-phases
  "Phases where choices are handled by clicking board elements, not buttons."
  #{:choose-die :choose-astronomer :choose-action :choose-role-increase
    :resolve-deploy :resolve-influence})

;; Auto-advance removed — was causing race conditions that skipped actions.
;; The :done button is always visible when available; player clicks it explicitly.

(defn choices-panel [state my-player]
  (let [[phase choices] (when state (choice/find-state-raw state))
        is-my-turn (my-turn? state my-player)
        ;; Only show buttons for phases NOT handled by board clicks
        show-buttons? (and is-my-turn
                           (map? choices) (seq choices)
                           (not (contains? board-handled-phases phase)))
]
    [:div {:style {:margin "4px 0" :padding "8px 12px"
                   :background "#0a0a12" :border-radius 8
                   :border (str "1px solid " (if is-my-turn "#4a4" "#222"))
                   :display "flex" :gap 12 :align-items "center" :flex-wrap "wrap"}}
     ;; Status bar with clear turn indicator
     (let [current-p (game/current-player state)
           is-bot (contains? @bots-set current-p)
           p-color (game/player-color state current-p)]
       [:span {:style {:color p-color :font-weight "bold" :font-size 13
                       :background "#111" :padding "2px 10px" :border-radius 4
                       :border (str "2px solid " p-color)}}
        (str current-p (when is-bot " 🤖") "'s turn")])
     (when (= :solo (:mode state))
       [:span {:style {:color "#88f" :font-weight "bold"
                       :background "#1a1a2a" :padding "2px 8px" :border-radius 4
                       :font-size 12}}
        (str "SOLO — " (get game/solo-color-names (dec (:round state 1)) "?"))])
     [:span {:style {:color "#886622" :font-weight "bold" :font-size 12}}
      (str "R" (:round state 1) "/" game/rounds-per-game
           " T" (:turn-in-round state 1) "/" (game/turns-per-round state))]
     [:span {:style {:color (if is-my-turn "#8f8" "#888") :font-size 12}}
      (when phase
        (if is-my-turn
          (case phase
            :choose-die "Select a die"
            :choose-astronomer "Click an astronomer to move"
            :choose-action "Click an action on the board"
            :choose-role-increase "Click a role to increase"
            :resolve-sell "Choose a good to sell"
            :resolve-temple "Choose a city for your temple"
            :resolve-travel "Choose where to travel"
            :travel-continue "Discard a good for extra travel, or done"
            :resolve-deploy "Click a route to deploy"
            :resolve-influence "Click a magistrate destination"
            :game-over "Game Over"
            (name phase))
          (str "Waiting for " (game/current-player state) "...")))]
     ;; Action buttons for non-board-handled phases (sell resource, travel dest, etc.)
     (when show-buttons?
       (for [[k _v] choices
             :when (not= k :skip)] ;; skip handled separately
         ^{:key (pr-str k)}
         [:button
          {:on-click #(send-action! k)
           :style {:background "#1a2a1a" :color "#8f8"
                   :border "1px solid #4a4" :border-radius 6
                   :padding "6px 14px" :cursor "pointer"
                   :font-size 12 :font-family "monospace"}}
          (choice-label k)]))
     ;; Skip button when available
     (when (and is-my-turn (map? choices) (contains? choices :skip))
       [:button
        {:on-click #(send-action! :skip)
         :style {:background "#1a1a1a" :color "#888"
                 :border "1px solid #333" :border-radius 6
                 :padding "4px 12px" :cursor "pointer" :font-size 11}}
        "skip"])
     ;; Done button when available
     (when (and is-my-turn (map? choices) (contains? choices :done))
       [:button
        {:on-click #(send-action! :done)
         :style {:background "#1a1a2a" :color "#aaf"
                 :border "1px solid #449" :border-radius 6
                 :padding "4px 12px" :cursor "pointer" :font-size 11}}
        "done"])
     ;; Undo
     (when (and is-my-turn @can-undo?)
       [:button
        {:on-click send-undo!
         :style {:background "#1a1a1a" :color "#aa8"
                 :border "1px solid #553" :border-radius 6
                 :padding "4px 12px" :cursor "pointer" :font-size 11}}
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
                   :on-change (fn [_]
                                (let [current-name (get-in @create-state [:players idx])]
                                  (when (seq current-name)
                                    (swap! create-state update :bots
                                           (if (contains? (:bots @create-state) current-name)
                                             disj conj) current-name))))}]
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
                   :border-radius 8 :padding 12
                   :flex 1 :min-width 320
                   :max-height 400 :overflow-y "auto"}}
     [:div {:style {:color "#886622" :font-weight "bold" :font-size 15
                    :margin-bottom 10 :position "sticky" :top 0
                    :background "#0a0a12" :padding-bottom 4}}
      "✦ Game Log"]
     (if (empty? log)
       [:div {:style {:color "#444" :font-size 12}} "No actions yet..."]
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
              [:div {:style {:color "#666" :font-size 11 :margin-top 8
                             :margin-bottom 3 :border-top "1px solid #222"
                             :padding-top 5}}
               (str "Round " (:round entry) " · Turn " (:turn entry))])
            [:div {:style {:display "flex" :gap 5 :margin-bottom 3
                           :font-size 12 :line-height "1.4"}}
             [:span {:style {:flex-shrink 0}} icon]
             [:div
              [:span {:style {:color p-color :font-weight "bold" :margin-right 4}}
               (:player entry)]
              [:span {:style {:color color}} (:message entry)]]]])))]))

;; ── Rendering: Rules reference ───────────────────────────────────────────────

(defn rules-reference []
  [:div {:style {:background "#0a0a12" :border "1px solid #333"
                 :border-radius 8 :padding 12
                 :flex 1 :min-width 320
                 :max-height 400 :overflow-y "auto"}}
   [:div {:style {:color "#886622" :font-weight "bold" :font-size 15
                  :margin-bottom 10}}
    "✦ Quick Reference"]
   [:div {:style {:font-size 12 :color "#999" :line-height "1.6"}}
    [:div {:style {:color "#5B8C3E" :font-weight "bold" :margin-top 6 :font-size 13}} "🌾 Take Goods"]
    [:div "Take both listed resources from the action space"]
    [:div {:style {:color "#C4A535" :font-weight "bold" :margin-top 6 :font-size 13}} "⚖ Sell (Merchant)"]
    [:div "Discard a good matching city demand. Score Amity based on Merchant level."]
    [:div {:style {:color "#C45BA8" :font-weight "bold" :margin-top 6 :font-size 13}} "🏛 Temple (Priest)"]
    [:div "Place face-up in caravan's or magistrate's city. When caravan visits: flip face-down, score Amity = # face-down temples."]
    [:div {:style {:color "#C44B35" :font-weight "bold" :margin-top 6 :font-size 13}} "⚔ Deploy (Raider)"]
    [:div "Place up to 2 raiders on routes near caravan. Opposing caravans & magistrates flip to point side. Score 4 Glory crossing own point raider."]
    [:div {:style {:color "#3581A8" :font-weight "bold" :margin-top 6 :font-size 13}} "🐪 Travel"]
    [:div "Move caravan 1 space. Discard a good to move again. Visits temples, flips enemy raiders, scores own point raiders."]
    [:div {:style {:color "#8B5BC4" :font-weight "bold" :margin-top 6 :font-size 13}} "👑 Influence (Leader)"]
    [:div "Move magistrate clockwise on roads. Flips raiders along the way. Sell/temple in magistrate city = bonus Glory."]
    [:div {:style {:color "#aa8" :font-weight "bold" :margin-top 8 :font-size 13}} "Scoring"]
    [:div "Reputation = min(Amity, Glory). Highest reputation wins."]
    [:div {:style {:color "#777" :font-size 11 :margin-top 2}}
     "Role levels shown in player info table above."]]])

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
       ;; Layout: feats → boards + choices → my player board → opponents → log
       [:div {:style {:display "flex" :flex-direction "column" :gap 8}}
        ;; 1. Feats/contests at top (claim by clicking)
        [contests-component state]
        ;; 2. Boards row: action board + dice, city board
        [:div {:style {:display "flex" :gap 12 :flex-wrap "wrap" :align-items "flex-start"}}
         [:div {:style {:display "flex" :flex-direction "column" :gap 8}}
          [action-board-component state my-player]
          [dice-display state my-player]]
         [city-board-component state my-player
          (when (and (my-turn? state my-player)
                     (contains? #{:resolve-travel :resolve-temple
                                  :resolve-deploy :resolve-influence}
                                (game/current-phase state)))
            (let [[_ choices] (choice/find-state-raw state)]
              choices))]]
        ;; 3. Choices panel (skip/done/undo) right below boards
        [choices-panel state my-player]
        ;; 4. MY player board: my info + my bonus board side by side
        [:div {:style {:display "flex" :gap 12 :flex-wrap "wrap" :align-items "stretch"}}
         ;; Only show MY player info here
         [:div {:style {:flex 1}}
          (let [pdata (get-in state [:players my-player])
                p-color (game/player-color state my-player)
                is-current (= my-player (game/current-player state))]
            (when pdata
              ^{:key (str "my-" my-player)}
              [:div {:style {:background (if is-current "#0d1a0d" "#0a0a12")
                             :border (str "2px solid " (if is-current p-color "#222"))
                             :border-radius 8 :padding 12 :font-size 13}}
               [:div {:style {:display "flex" :align-items "center" :gap 8 :margin-bottom 8}}
                [:div {:style {:width 14 :height 14 :border-radius "50%"
                               :background p-color}}]
                [:span {:style {:color p-color :font-weight "bold" :font-size 16}}
                 (str my-player (when is-current " ✦"))]
                [:span {:style {:color "#8aa" :font-size 13 :margin-left 8}}
                 (str "🐪 " (when (:caravan pdata) (str/capitalize (name (:caravan pdata)))))]]
               [:div {:style {:display "flex" :gap 16 :margin-bottom 8 :font-size 15
                              :align-items "center"}}
                [:span {:style {:color "#6ac"}} (str "♥ " (:amity pdata 0))]
                [:span {:style {:color "#ca6"}} (str "⚡ " (:glory pdata 0))]
                [:span {:style {:color "#e8e8e8" :font-weight "bold" :font-size 16
                                 :background "#1a1a2e" :padding "2px 10px" :border-radius 4
                                 :border "1px solid #444"}}
                 (str "★ " (min (:amity pdata 0) (:glory pdata 0)))]]
               [:div {:style {:display "flex" :gap 10 :margin-bottom 8 :flex-wrap "wrap"}}
                (for [[r n] (:resources pdata) :when (pos? n)]
                  ^{:key (str "my-res-" (name r))}
                  [:span {:style {:color (get game/resource-colors r "#888")
                                  :background "#111" :padding "3px 8px" :border-radius 4
                                  :border (str "1px solid " (get game/resource-colors r "#333"))
                                  :font-size 14}}
                   (str (get game/resource-icons r "") " " n)])]
               [role-table-component my-player pdata p-color state my-player]
               (let [demands (:demand-tokens pdata [])]
                 (when (seq demands)
                   [:div {:style {:display "flex" :gap 4 :margin-top 4 :flex-wrap "wrap"
                                  :align-items "center"}}
                    [:span {:style {:color "#888" :font-size 11 :margin-right 4}} "Sold:"]
                    (for [[idx d] (map-indexed vector demands)]
                      ^{:key (str "my-demand-" idx)}
                      [:span {:style {:background (get game/resource-colors d "#444")
                                      :color "#fff" :padding "2px 6px" :border-radius 10
                                      :font-size 11 :opacity 0.9}}
                       (get game/resource-icons d "?")])]))
               [:div {:style {:color "#777" :font-size 12 :margin-top 6
                              :display "flex" :gap 14}}
                [:span (str "🏛 ×" (:temples-supply pdata 0))]
                [:span (str "⚔ ×" (:raiders-supply pdata 0))]
                (when (pos? (:wild-points pdata 0))
                  [:span {:style {:color "#ff8" :font-weight "bold"}}
                   (str "★ " (:wild-points pdata 0) " wild")])]]))]
         [bonus-boards-component state]]
        ;; 5. Opponent player boards (compact)
        (let [opponents (remove #(= (first %) my-player) (:players state))]
          (when (seq opponents)
            [:div {:style {:background "#0a0a12" :border "1px solid #222"
                           :border-radius 8 :padding 10}}
             [:div {:style {:color "#666" :font-size 13 :margin-bottom 6}} "Opponents"]
             [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
              (for [[pk pdata] opponents
                    :let [p-color (game/player-color state pk)
                          is-current (= pk (game/current-player state))]]
                ^{:key (str "opp-" pk)}
                [:div {:style {:background "#0a0a12"
                               :border (str "1px solid " (if is-current p-color "#222"))
                               :border-radius 6 :padding 8 :min-width 180 :font-size 11}}
                 [:div {:style {:color p-color :font-weight "bold" :font-size 12 :margin-bottom 3}}
                  (str pk (when is-current " ✦") (when (contains? @bots-set pk) " 🤖"))]
                 [:div {:style {:display "flex" :gap 8 :margin-bottom 2}}
                  [:span {:style {:color "#6ac"}} (str "♥" (:amity pdata 0))]
                  [:span {:style {:color "#ca6"}} (str "⚡" (:glory pdata 0))]
                  [:span {:style {:color "#ccc" :font-weight "bold"}}
                   (str "★" (min (:amity pdata 0) (:glory pdata 0)))]]
                 [:div {:style {:color "#777" :font-size 10}}
                  (str/join " " (for [[r lv] (:roles pdata)]
                                  (str (get role-icons r "") (subs (name r) 0 3) lv)))]])]]))
        ;; 6. Bottom: game log and rules reference
        [:div {:style {:display "flex" :gap 12 :flex-wrap "wrap" :align-items "flex-start"}}
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
        (reset! server-choices [(:phase message) (reader/read-string (:choices message))]))
      ;; Pending feat claim → show slot picker on bonus board
      (reset! pending-claim
              (when (:pending-claim message)
                (reader/read-string (:pending-claim message)))))

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
