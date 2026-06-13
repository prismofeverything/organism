(ns eridu.play
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [ajax.core :refer [POST]]
   [eridu.game :as game]
   [eridu.choice :as choice]
   [eridu.personality :as personality]
   [organism.ajax :as ajax]
   [organism.websockets :as ws]))

;; ── State ─────────────────────────────────────────────────────────────────────

(defonce app-state
  (r/atom {:game nil
           :player (when (exists? js/playerKey) js/playerKey)
           :preferences {} :observe-games []
           :bots #{} :can-undo? false :server-choices nil
           :pending-claim nil :pending-bonus nil :show-tooltips? true}))

;; Cursors for backward-compatible deref/swap! semantics
(def game-state       (r/cursor app-state [:game]))
(def player-key       (r/cursor app-state [:player]))
(def player-preferences (r/cursor app-state [:preferences]))
(def observe-games    (r/cursor app-state [:observe-games]))
(def bots-set         (r/cursor app-state [:bots]))
(def can-undo?        (r/cursor app-state [:can-undo?]))
(def server-choices   (r/cursor app-state [:server-choices]))
(def pending-claim    (r/cursor app-state [:pending-claim]))
(def pending-bonus    (r/cursor app-state [:pending-bonus]))
(def show-tooltips?   (r/cursor app-state [:show-tooltips?]))

(defn tip
  "Returns {:title text} when tooltips are enabled, else {}."
  [text]
  (if @show-tooltips? {:title text} {}))

(def action-tooltips
  {:take "Take Goods — take both listed resources"
   :sell "Sell — discard a good matching city demand, score Amity"
   :deploy "Deploy — place up to 2 raiders on adjacent routes"
   :travel "Travel — move caravan 1 space, may extend once"
   :influence "Influence — move magistrate clockwise along roads"
   :temple "Temple — place face-up in caravan or magistrate city"})

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

;; ── Offline / solo-vs-AI mode ─────────────────────────────────────────────────

(def offline?-cursor       (r/cursor app-state [:offline?]))
(def bot-personalities     (r/cursor app-state [:bot-personalities]))

(def ^:private offline-storage-key "eridu-offline-game-v3")
(def ^:private offline-pending-sync-key "eridu-offline-pending-sync-v1")
(def ^:private offline-log-endpoint "/eridu/offline/log")
(def ^:private offline-protected-phases game/bot-protected-phases)

(defn- url-param [k]
  (try
    (let [p (.get (js/URLSearchParams. (.-search js/location)) k)]
      (when (seq p) p))
    (catch :default _ nil)))

(defn- update-server-choices-from-state! [state]
  (when state
    (let [[phase choices] (choice/find-state-raw state)]
      (reset! server-choices [phase choices]))))

(defn save-offline-game! []
  (when @offline?-cursor
    (try
      (.setItem js/localStorage offline-storage-key
                (pr-str (select-keys @app-state
                                     [:game :bot-personalities
                                      :offline-human :offline-bot :offline-archetype
                                      :offline-game-id :offline-initial-state
                                      :offline-actions :offline-started-at])))
      (catch :default _ nil))))

(defn- load-offline-game []
  (try
    (when-let [s (.getItem js/localStorage offline-storage-key)]
      (reader/read-string s))
    (catch :default _ nil)))

(defn- record-action!
  "Append a choice key to the offline action log so the game can be
   reproduced exactly during sync / replay."
  [choice-key]
  (swap! app-state update :offline-actions (fnil conj []) choice-key))

(defn run-ai-loop!
  "Apply AI moves while it's a bot's turn. Stops when a human has the turn,
   the game is over, or after a hard iteration cap (safety valve)."
  []
  (loop [iters 0]
    (let [state @game-state
          current (and state (game/current-player state))
          bots (set (keys @bot-personalities))]
      (cond
        (>= iters 500) nil
        (or (nil? state) (:game-over state)) (update-server-choices-from-state! state)
        (not (contains? bots current)) (update-server-choices-from-state! state)
        :else
        (let [weights (get @bot-personalities current personality/default-weights)
              [ck next-state] (personality/personality-step state weights)]
          (if next-state
            (let [advanced (choice/advance-through-trivial next-state offline-protected-phases)]
              (record-action! ck)
              (reset! game-state advanced)
              (recur (inc iters)))
            (update-server-choices-from-state! state)))))))

(defn- read-pending-queue []
  (try
    (or (some-> (.getItem js/localStorage offline-pending-sync-key) reader/read-string) [])
    (catch :default _ [])))

(defn- write-pending-queue! [q]
  (try
    (.setItem js/localStorage offline-pending-sync-key (pr-str (vec q)))
    (catch :default _ nil)))

(defn queue-completed-game!
  "Build a sync record from the current offline game state and append it
   to the pending-sync queue. Idempotent: if the same :offline-game-id is
   already queued, leaves the queue unchanged."
  []
  (let [s @app-state
        gid (:offline-game-id s)]
    (when (and gid (:game-over (:game s)))
      (let [record {:game-key      gid
                    :human         (:offline-human s)
                    :bot           (:offline-bot s)
                    :archetype     (:offline-archetype s)
                    :weights       (pr-str (get-in s [:bot-personalities (:offline-bot s)]))
                    :initial-state (pr-str (:offline-initial-state s))
                    :actions       (pr-str (vec (:offline-actions s)))
                    :final-state   (pr-str (:game s))
                    :started-at    (:offline-started-at s)
                    :completed-at  (.now js/Date)}
            queue   (read-pending-queue)
            already (some #(= gid (:game-key %)) queue)]
        (when-not already
          (write-pending-queue! (conj queue record)))))))

(defn flush-pending-games!
  "POST each queued game to /eridu/offline/log. On success, drop it from
   the queue. On network failure, leave it for next attempt. Fire-and-
   forget — does not block UI."
  []
  (let [queue (read-pending-queue)]
    (doseq [record queue]
      (POST offline-log-endpoint
            {:params record
             :format :transit
             :response-format :transit
             :handler (fn [_]
                        (let [remaining (vec (remove #(= (:game-key %)
                                                         (:game-key record))
                                                     (read-pending-queue)))]
                          (write-pending-queue! remaining)))
             :error-handler (fn [_]
                              ;; Silent — leave in queue, retry next time
                              nil)}))))

(defn apply-choice-locally!
  "Apply a player choice to local game state, then run AI turns until a
   human player has the turn (or the game ends)."
  [choice-key]
  (let [state @game-state
        [_phase choices] (choice/find-state-raw state)
        next-state (get choices choice-key)]
    (when next-state
      (let [advanced (choice/advance-through-trivial next-state offline-protected-phases)]
        (record-action! choice-key)
        (reset! game-state advanced)
        (run-ai-loop!)
        (save-offline-game!)
        (when (:game-over @game-state)
          (queue-completed-game!)
          (flush-pending-games!))))))

(defn- cache-personalities-on-state
  "Mirror simulate.clj: stash decision-relevant weights on each bot player so
   downstream feat/bonus heuristics can read them from state."
  [state pmap]
  (reduce (fn [s [pk weights]]
            (assoc-in s [:players pk :personality-cache]
                      (select-keys weights [:tempo :feat-awareness
                                            :prefer-onetime-bonus
                                            :feat-sequence :feat-closure-urgency])))
          state pmap))

(defn- offline-human-name
  "Username from the server template (when authenticated), falling back
   to a generic local id."
  []
  (if (and (exists? js/playerName) (seq js/playerName))
    js/playerName
    "player"))

(defn- offline-bot-name
  "A stable, descriptive id for the AI opponent so saved games can be
   identified by the personality that played them."
  [archetype-name]
  (str (or archetype-name "default") "-bot"))

(defn start-offline-game!
  "Initialize a fresh 2-player offline game.

   Human player slot uses the logged-in username (from js/playerName) so
   the game record can be tagged with the player's account when synced
   back to the server. The AI slot is named for its archetype.

   Player keys are strings to match the existing UI components and the
   server's online-game convention (slot keys are usernames, not :p1)."
  ([] (start-offline-game! "default"))
  ([archetype-name]
   (let [human    (offline-human-name)
         bot      (offline-bot-name archetype-name)
         archetype (or (get personality/archetypes archetype-name)
                       personality/default-weights)
         pmap     {bot archetype}
         initial  (-> (game/initial-state [human bot])
                      (cache-personalities-on-state pmap))
         advanced (choice/advance-through-trivial initial offline-protected-phases)
         game-id  (str (random-uuid))
         now      (.now js/Date)]
     (swap! app-state assoc
            :game advanced
            :player human
            :offline? true
            :offline-human human
            :offline-bot bot
            :offline-archetype archetype-name
            :offline-game-id game-id
            :offline-initial-state initial
            :offline-actions []
            :offline-started-at now
            :bot-personalities pmap
            :bots #{bot}
            :can-undo? false
            :pending-claim nil
            :pending-bonus nil)
     (run-ai-loop!)
     (save-offline-game!)
     (when (:game-over @game-state)
       (queue-completed-game!)
       (flush-pending-games!)))))

(defn resume-offline-game!
  "Restore an offline game from localStorage, if one is saved. Returns true
   on successful restore."
  []
  (when-let [{:keys [game bot-personalities offline-human offline-bot
                     offline-archetype offline-game-id offline-initial-state
                     offline-actions offline-started-at]} (load-offline-game)]
    (when (and game (seq bot-personalities))
      (let [human (or offline-human (offline-human-name))]
        (swap! app-state assoc
               :game game
               :player human
               :offline? true
               :offline-human human
               :offline-bot offline-bot
               :offline-archetype offline-archetype
               :offline-game-id offline-game-id
               :offline-initial-state offline-initial-state
               :offline-actions (vec offline-actions)
               :offline-started-at offline-started-at
               :bot-personalities bot-personalities
               :bots (set (keys bot-personalities)))
        (update-server-choices-from-state! game)
        (run-ai-loop!)
        true))))

;; ── WebSocket communication ───────────────────────────────────────────────────

(defn send-action! [choice-key]
  (if @offline?-cursor
    (apply-choice-locally! choice-key)
    (ws/send-transit-message! {:type "action" :choice (pr-str choice-key)})))

(defn send-undo! []
  (if @offline?-cursor
    (js/console.warn "undo is not yet supported in offline mode")
    (ws/send-transit-message! {:type "undo"})))

(defn send-resign! []
  (if @offline?-cursor
    (js/console.warn "resign is not yet supported in offline mode")
    (ws/send-transit-message! {:type "resign"})))

(defn send-claim-feat!
  ([feat-id] (send-claim-feat! feat-id nil))
  ([feat-id slot-idx]
   (if @offline?-cursor
     (js/console.warn "claim-feat is not yet supported in offline mode")
     (ws/send-transit-message! (cond-> {:type "claim-feat" :feat-id (name feat-id)}
                                 slot-idx (assoc :slot-idx slot-idx))))))

(defn send-resolve-bonus! [choice-val]
  (if @offline?-cursor
    (js/console.warn "resolve-bonus is not yet supported in offline mode")
    (ws/send-transit-message! {:type "resolve-bonus" :choice (pr-str choice-val)})))

(defn send-use-passive! [passive-id choice]
  (if @offline?-cursor
    (js/console.warn "use-passive is not yet supported in offline mode")
    (ws/send-transit-message! {:type "use-passive" :passive-id passive-id :choice (pr-str choice)})))

;; ── Bug report: ship the player's typed report + game snapshot to the server ──

(defonce bug-report-text (r/atom ""))
(defonce bug-report-status (r/atom nil))

(defn send-bug-report! [text]
  (let [play-key (when (exists? js/playKey) js/playKey)
        player   @player-key
        state    @game-state
        record   {:text     text
                  :play-key play-key
                  :player   player
                  :offline? (boolean @offline?-cursor)
                  :ts       (.toISOString (js/Date.))
                  :url      (.-href js/location)
                  :state    (pr-str state)}]
    (reset! bug-report-status :sending)
    (POST "/eridu/bug-report"
          {:params  record
           :format  :transit
           :response-format :transit
           :handler (fn [_]
                      (reset! bug-report-status :sent)
                      (reset! bug-report-text "")
                      (js/setTimeout #(reset! bug-report-status nil) 3000))
           :error-handler (fn [err]
                            (js/console.warn "bug-report failed" (pr-str err))
                            (reset! bug-report-status :error)
                            (js/setTimeout #(reset! bug-report-status nil) 5000))})))

;; ── Unicode die faces ─────────────────────────────────────────────────────────

(def die-faces {1 "⚀" 2 "⚁" 3 "⚂" 4 "⚃" 5 "⚄" 6 "⚅"})

;; ── Rendering: Dice display ──────────────────────────────────────────────────

(defn player-dice-row [state pk is-choosing chosen-die]
  (let [pdata (game/player-data state pk)
        dice-available (:dice-available pdata [])
        dice-used (:dice-used pdata [])
        p-color (game/player-color state pk)
        is-current (= pk (game/current-player state))]
    [:div {:style {:background "#0d0d1a" :border-radius 8 :padding 8
                   :border (str "1px solid " (if is-current p-color "#222"))
                   :opacity (if is-current 1.0 0.7)}}
     [:div {:style {:color p-color :font-size 11 :margin-bottom 4 :font-weight "bold"}}
      (str "🎲 " pk (when is-current " ✦"))]
     [:div {:style {:display "flex" :gap 6 :align-items "center" :flex-wrap "wrap"}}
      (for [[idx die-val] (map-indexed vector dice-available)]
        ^{:key (str "d-" pk "-a-" idx)}
        [:div {:on-click (when is-choosing #(send-action! idx))
               :style {:font-size 32 :cursor (when is-choosing "pointer")
                       :padding "6px 8px" :border-radius 6
                       :background (if is-choosing "#1a2a1a" "#111")
                       :border (str "2px solid " (if is-choosing "#4a4" "#333"))
                       :color (if is-choosing "#8f8" "#ccc")
                       :text-align "center" :min-width 44 :min-height 44
                       :touch-action "manipulation"}}
         [:div (get die-faces die-val (str die-val))]
         [:div {:style {:font-size 9 :color "#666"}} (str die-val)]])
      (when (and is-current chosen-die)
        ^{:key (str "d-" pk "-ch")}
        [:div {:style {:font-size 32 :padding "3px 6px" :border-radius 6
                       :background "#2a2a1a" :border "2px solid #aa8"
                       :color "#ff8" :text-align "center" :min-width 40}}
         [:div (get die-faces chosen-die (str chosen-die))]
         [:div {:style {:font-size 9 :color "#aa8"}} "chosen"]])
      (for [[idx die-val] (map-indexed vector dice-used)]
        ^{:key (str "d-" pk "-u-" idx)}
        [:div {:style {:font-size 24 :padding "2px 5px" :border-radius 5
                       :background "#0a0a0a" :border "1px solid #222"
                       :color "#444" :text-align "center" :min-width 36
                       :opacity 0.4}}
         [:div (get die-faces die-val (str die-val))]])]]))

(defn dice-display [state my-player]
  (let [phase (game/current-phase state)
        current (game/current-player state)
        is-my-choosing (and (= phase :choose-die) (= current my-player))
        chosen-die (when (= phase :choose-astronomer)
                     (get-in state [:player-turn :die-value]))]
    [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
     ;; Always show MY dice
     [player-dice-row state my-player is-my-choosing chosen-die]
     ;; Show active opponent's dice when it's their turn
     (when (not= current my-player)
       [player-dice-row state current false chosen-die])]))

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
    [:svg {:viewBox "0 0 560 560"
           :style {:width "100%" :max-width 540
                   :background "radial-gradient(circle, #0d0d1e, #050510)"
                   :background-color "#070712"
                   :border-radius 8 :border (str "1px solid #333")
                   :touch-action "manipulation"}}
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
             ;; SVG tooltip
             (when @show-tooltips?
               [:title (str (get action-tooltips atype (name atype))
                            (when resources
                              (str " (" (str/join ", " (map name resources)) ")")))])
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
    [:svg {:viewBox "0 0 500 470"
           :style {:width "100%" :max-width 540
                   :background "linear-gradient(180deg, #080812, #0a0a1a)"
                   :background-color "#090914"
                   :border-radius 8 :border "1px solid #333"
                   :touch-action "manipulation"}}
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
     ;; Raiders on routes (offset when multiple on same route)
     (let [route-counts (atom {})]
       (for [[pk pdata] (:players state)
             [rk raider-state] (:raiders pdata)
             :let [[c1 c2] rk
                   {x1 :x y1 :y} (get city-positions c1)
                   {x2 :x y2 :y} (get city-positions c2)
                   mx (/ (+ x1 x2) 2)
                   my (/ (+ y1 y2) 2)
                   ;; Offset perpendicular to route for stacking
                   idx (get (swap! route-counts update rk (fnil inc 0)) rk)
                   ;; Perpendicular offset direction
                   dx (- y2 y1) dy (- x1 x2)
                   len (js/Math.sqrt (+ (* dx dx) (* dy dy)))
                   offset (* (- idx 1) 20)
                   ox (if (pos? len) (* offset (/ dx len)) 0)
                   oy (if (pos? len) (* offset (/ dy len)) 0)
                   rx (+ mx ox) ry (+ my oy)
                   p-color (game/player-color state pk)
                   is-raiding (= raider-state :raiding)]]
         ^{:key (str "raider-" pk "-" (name c1) "-" (name c2))}
         [:g
          (if is-raiding
            [:g
             [:rect {:x (- rx 12) :y (- ry 12) :width 24 :height 24
                     :fill p-color :rx 4 :stroke "#fff" :stroke-width 1.5 :opacity 0.9}]
             [:text {:x rx :y (+ ry 6) :text-anchor "middle" :fill "#fff" :font-size 17}
              "⚔"]]
            [:g
             [:rect {:x (- rx 12) :y (- ry 12) :width 24 :height 24
                     :fill "#111" :rx 4 :stroke p-color :stroke-width 2}]
             [:text {:x rx :y (+ ry 6) :text-anchor "middle" :fill p-color :font-size 16}
              "🏴"]])]))
     ;; Magistrates on cities (offset when stacked)
     (let [mag-offset (atom {})]
       (for [[mag-id mag-city] (:magistrates state)
             :let [{:keys [x y]} (get city-positions mag-city)
                   idx (get (swap! mag-offset update mag-city (fnil inc 0)) mag-city)
                   offset-x (* (dec idx) 22)]
             :when (get city-positions mag-city)]
         ^{:key (str "magistrate-" (name mag-id))}
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
        ;; Demand tokens with resource icons (larger)
        (for [[idx token] (map-indexed vector demands)]
          ^{:key (str "demand-" (name city) "-" idx)}
          [:g (tip (str "Demand: " (name token)))
           [:circle {:cx (+ (- x 24) (* idx 22)) :cy (+ y 20) :r 11
                     :fill (get game/resource-colors token "#444")
                     :stroke "#fff" :stroke-width 1.0 :opacity 0.9}]
           [:text {:x (+ (- x 24) (* idx 22))
                   :y (+ y 24) :text-anchor "middle" :fill "#fff" :font-size 14}
            (get game/resource-icons token "?")]])
        ;; Temples (per player, with player color, offset to avoid overlap)
        (let [temple-players (vec (for [[pk pd] (:players state)
                                        :when (get-in pd [:temples city])]
                                    pk))]
          (for [[ti pk] (map-indexed vector temple-players)
                :let [pdata (game/player-data state pk)
                      temple-state (get-in pdata [:temples city])
                      p-color (game/player-color state pk)
                      is-face-up (= temple-state :face-up)
                      tx (+ x 24 (* ti 18))
                      ty (- y 6)]]
            ^{:key (str "temple-" pk "-" (name city))}
            [:g (tip (str pk "'s temple (" (if is-face-up "face-up" "face-down") ")"))
             ;; Player color indicator dot
             [:circle {:cx tx :cy (- ty 10) :r 4
                       :fill p-color :stroke "#fff" :stroke-width 0.5}]
             [:text {:x tx :y ty :text-anchor "middle"
                     :fill (if is-face-up p-color "#666")
                     :font-size 16
                     :opacity (if is-face-up 1.0 0.5)}
              "🏛"]]))])
     ;; Caravans with player colors
     (for [[pk pdata] (:players state)
           :let [city (:caravan pdata)
                 {:keys [x y]} (get city-positions city)
                 p-color (game/player-color state pk)
                 p-idx (.indexOf (:turn-order state) pk)]]
       ^{:key (str "caravan-" pk)}
       [:g (tip (str pk "'s caravan"))
        ;; Colored caravan disc with player initial
        [:circle {:cx (+ (- x 8) (* p-idx 24)) :cy (- y 24) :r 14
                  :fill p-color :opacity 0.4
                  :stroke p-color :stroke-width 2}]
        [:text {:x (+ (- x 8) (* p-idx 24)) :y (- y 18)
                :text-anchor "middle" :fill "#fff" :font-size 16
                :font-weight "bold" :style {:pointer-events "none"}}
         "🐪"]
        [:text {:x (+ (- x 8) (* p-idx 24)) :y (- y 31)
                :text-anchor "middle" :fill p-color :font-size 8
                :font-weight "bold" :style {:pointer-events "none"}}
         (subs pk 0 (min 3 (count pk)))]])]))

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
        [:tr (cond-> {:style (cond-> {:border-bottom "1px solid #1a1a2e"
                                       :min-height 44}
                               can-increase? (assoc :background "#0a2a0a"
                                                     :cursor "pointer"))}
               can-increase? (assoc :on-click #(send-action! role)))
         [:td {:style {:padding "8px 6px" :white-space "nowrap"}}
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
                    :border-radius 8 :padding 12 :min-width 0 :font-size 13}}
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
                  ;; Highlight if I can claim it now (only on MY turn)
                  is-my-turn (= my-player (game/current-player state))
                  claimable? (and is-my-turn
                                  (not (my-claimed? contest-id))
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
                               :min-width 140 :max-width 220
                               :transition "all 0.2s"}
                        claimable? (assoc :cursor "pointer"
                                          :box-shadow "0 0 12px rgba(85,255,85,0.7)"
                                          :touch-action "manipulation"))}
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
                   :border-radius 8 :padding 12 :flex 1 :min-width 0}}
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
        [:div {:style {:display "flex" :gap 4 :flex-wrap "wrap"}}
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
                                          :box-shadow "0 0 8px rgba(85,255,85,0.5)"
                                          :touch-action "manipulation")
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

;; ── Board 14: Uruk bonus travel UI state ─────────────────────────────────────

;; UI-local state consolidated into ui-state atom
(defonce ui-state
  (r/atom {:create {:play-name "" :players [""] :bots #{} :mode :normal}
           :uruk-travel nil}))

(def uruk-travel-state (r/cursor ui-state [:uruk-travel]))

(defn- uruk-travel-available?
  "True if player can use Board 14 Uruk bonus travel this turn."
  [state player]
  (when (and state player)
    (let [pdata (game/player-data state player)
          board-id (game/player-board-id state player)
          caravan (:caravan pdata)
          uruk-adj (set (get-in state [:city-graph :uruk]))]
      (and (= board-id 14)
           (game/has-passive? state player)
           (not (:used-uruk-travel pdata))
           (or (= caravan :uruk) (contains? uruk-adj caravan))
           (some #(pos? (get-in pdata [:resources %] 0)) game/resource-types)))))

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
            :turn-complete "Turn complete — claim feats or click done"
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
                   :padding "10px 16px" :cursor "pointer"
                   :font-size 14 :font-family "monospace"
                   :min-height 44 :min-width 44
                   :touch-action "manipulation"}}
          (choice-label k)]))
     ;; Free travel (Board 6 passive) — shown even in board-handled phases
     (when (and is-my-turn (map? choices) (contains? choices :free-travel))
       [:button
        {:on-click #(send-action! :free-travel)
         :style {:background "#1a3a1a" :color "#8f8"
                 :border "2px solid #5f5" :border-radius 6
                 :padding "8px 16px" :cursor "pointer" :font-size 13
                 :min-height 44 :min-width 44
                 :box-shadow "0 0 8px rgba(85,255,85,0.5)"
                 :touch-action "manipulation"}}
        "🐪 Free Travel (Board Passive)"])
     ;; Skip button when available
     (when (and is-my-turn (map? choices) (contains? choices :skip))
       [:button
        {:on-click #(send-action! :skip)
         :style {:background "#1a1a1a" :color "#888"
                 :border "1px solid #333" :border-radius 6
                 :padding "10px 16px" :cursor "pointer" :font-size 13
                 :min-height 44 :min-width 44
                 :touch-action "manipulation"}}
        "skip"])
     ;; Done button when available
     (when (and is-my-turn (map? choices) (contains? choices :done))
       [:button
        {:on-click #(send-action! :done)
         :style {:background "#1a1a2a" :color "#aaf"
                 :border "1px solid #449" :border-radius 6
                 :padding "10px 16px" :cursor "pointer" :font-size 13
                 :min-height 44 :min-width 44
                 :touch-action "manipulation"}}
        "done"])
     ;; Board 14: Uruk bonus travel button
     (when (and is-my-turn (uruk-travel-available? state my-player))
       (let [ut @uruk-travel-state
             pdata (game/player-data state my-player)
             caravan (:caravan pdata)
             in-uruk? (= caravan :uruk)
             uruk-adj (set (get-in state [:city-graph :uruk]))]
         (cond
           ;; Picking destination (only when in Uruk)
           (and ut (= (:picking ut) :dest))
           [:div {:style {:display "flex" :gap 6 :align-items "center"}}
            [:span {:style {:color "#c90" :font-size 12}} "Travel to:"]
            (for [dest (sort uruk-adj)]
              ^{:key dest}
              [:button
               {:on-click (fn []
                            (send-use-passive! "14" [(:resource ut) dest])
                            (reset! uruk-travel-state nil))
                :style {:background "#2a1a0a" :color "#fc0"
                        :border "1px solid #c90" :border-radius 6
                        :padding "8px 12px" :cursor "pointer" :font-size 12
                        :min-height 36 :touch-action "manipulation"}}
               (str (name dest))])
            [:button
             {:on-click #(reset! uruk-travel-state nil)
              :style {:background "#1a1a1a" :color "#888"
                      :border "1px solid #333" :border-radius 6
                      :padding "8px 12px" :cursor "pointer" :font-size 12
                      :min-height 36 :touch-action "manipulation"}}
             "cancel"]]

           ;; Picking resource to discard
           (and ut (= (:picking ut) :resource))
           [:div {:style {:display "flex" :gap 6 :align-items "center"}}
            [:span {:style {:color "#c90" :font-size 12}} "Discard:"]
            (for [res game/resource-types
                  :when (pos? (get-in pdata [:resources res] 0))]
              ^{:key res}
              [:button
               {:on-click (fn []
                            (if in-uruk?
                              ;; Need to pick destination next
                              (reset! uruk-travel-state {:picking :dest :resource res})
                              ;; Adjacent to Uruk — destination is Uruk
                              (do (send-use-passive! "14" [res :uruk])
                                  (reset! uruk-travel-state nil))))
                :style {:background "#2a1a0a" :color "#fc0"
                        :border "1px solid #c90" :border-radius 6
                        :padding "8px 12px" :cursor "pointer" :font-size 12
                        :min-height 36 :touch-action "manipulation"}}
               (str (get game/resource-icons res "") " " (name res))])
            [:button
             {:on-click #(reset! uruk-travel-state nil)
              :style {:background "#1a1a1a" :color "#888"
                      :border "1px solid #333" :border-radius 6
                      :padding "8px 12px" :cursor "pointer" :font-size 12
                      :min-height 36 :touch-action "manipulation"}}
             "cancel"]]

           ;; Initial button
           :else
           [:button
            {:on-click #(reset! uruk-travel-state {:picking :resource})
             :style {:background "#2a1a0a" :color "#fc0"
                     :border "1px solid #c90" :border-radius 6
                     :padding "10px 16px" :cursor "pointer" :font-size 13
                     :min-height 44 :min-width 44
                     :touch-action "manipulation"}}
            (str "Bonus Travel "
                 (if in-uruk? "(from Uruk)" "(to Uruk)"))])))
     ;; Undo
     (when (and is-my-turn @can-undo?)
       [:button
        {:on-click send-undo!
         :style {:background "#1a1a1a" :color "#aa8"
                 :border "1px solid #553" :border-radius 6
                 :padding "10px 16px" :cursor "pointer" :font-size 13
                 :min-height 44 :min-width 44
                 :touch-action "manipulation"}}
        "↩ undo"])
     ;; Resign — only when the game isn't already over
     (when (and (not (:game-over state)) (not @offline?-cursor))
       [:button
        {:on-click #(when (js/confirm "Resign this game? This will end the game and cannot be undone.")
                      (send-resign!))
         :style {:background "#1a0a0a" :color "#c66"
                 :border "1px solid #844" :border-radius 6
                 :padding "10px 16px" :cursor "pointer" :font-size 13
                 :min-height 44 :min-width 44
                 :margin-left "auto"
                 :touch-action "manipulation"}}
        "⚑ resign"])]))

;; ── Create game form ──────────────────────────────────────────────────────────

(def create-state (r/cursor ui-state [:create]))

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
   :raider-flip "🔄" :raider-score "💀" :magistrate-raider-flip "👑"
   :feat-claim "🏆" :bonus-effect "✨"})

(def log-type-colors
  {:die "#aaf" :astronomer "#aaf" :landing "#aaa" :first-player "#FFD700"
   :role-increase "#af8" :action-select "#ccc" :take "#5B8C3E" :sell "#C4A535"
   :temple "#C45BA8" :temple-visit "#C45BA8" :deploy "#C44B35" :travel "#3581A8"
   :travel-extend "#3581A8" :influence "#8B5BC4"
   :raider-flip "#f84" :raider-score "#ff4" :magistrate-raider-flip "#f84"
   :feat-claim "#ff8" :bonus-effect "#8ff"})

(defn bug-report-component []
  (let [status @bug-report-status
        text   @bug-report-text]
    [:div {:style {:background "#1a0a0a" :border "1px solid #633"
                   :border-radius 8 :padding 10 :margin-top 8}}
     [:div {:style {:color "#c66" :font-weight "bold" :font-size 13 :margin-bottom 6}}
      "🐞 Report a bug (sends current game state)"]
     [:textarea
      {:value text
       :on-change #(reset! bug-report-text (-> % .-target .-value))
       :placeholder "What went wrong? e.g. \"Board 34 #4 didn't ask me to sell at Kish.\""
       :style {:width "100%" :min-height 60 :background "#0a0508" :color "#fcc"
               :border "1px solid #533" :border-radius 4 :padding 6
               :font-family "monospace" :font-size 12 :resize "vertical"
               :box-sizing "border-box"}}]
     [:div {:style {:display "flex" :gap 8 :align-items "center" :margin-top 6}}
      [:button
       {:on-click #(when (seq (.trim text)) (send-bug-report! text))
        :disabled (or (empty? (.trim text)) (= :sending status))
        :style {:background (if (seq (.trim text)) "#3a1a1a" "#222")
                :color (if (seq (.trim text)) "#fcc" "#666")
                :border "1px solid #844" :border-radius 4
                :padding "6px 14px" :cursor "pointer" :font-size 13}}
       (case status :sending "Sending…" :sent "✓ Sent" :error "✗ Failed" "Send")]
      (when status
        [:span {:style {:color (case status :sent "#7c7" :error "#c77" "#888")
                        :font-size 11}}
         (case status
           :sending "uploading game state…"
           :sent "queued for triage"
           :error "send failed (check console)"
           "")])]]))

(defn game-log-component [state]
  (let [log (reverse (:log state []))
        current-round (:round state 1)
        current-turn (:turn-in-round state 1)]
    [:div {:style {:background "#0a0a12" :border "1px solid #333"
                   :border-radius 8 :padding 12
                   :flex 1 :min-width 0
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
                 :flex 1 :min-width 0
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
                     :background "#050510" :min-height "100vh"
                     :overflow-x "hidden" :max-width "100vw"}}
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
        ;; Settings row
        [:div {:style {:display "flex" :justify-content "flex-end" :margin-bottom 4}}
         [:label {:style {:color "#555" :font-size 11 :display "flex"
                          :align-items "center" :gap 4 :cursor "pointer"}}
          [:input {:type "checkbox" :checked @show-tooltips?
                   :on-change #(swap! show-tooltips? not)}]
          "Tooltips"]]
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
        ;; 3b. Bonus resolution picker (when a bonus effect needs player choice)
        (when-let [bonus @pending-bonus]
          (when (= (:player bonus) my-player)
            [:div {:style {:background "#0a2a2a" :border "2px solid #4cc"
                           :border-radius 8 :padding 12 :margin "4px 0"}}
             [:div {:style {:color "#8ff" :font-weight "bold" :font-size 14 :margin-bottom 8}}
              (str "✨ " (:prompt bonus "Choose"))]
             (case (:choice-type bonus)
               ;; Resource picker
               :pick-resource
               [:div {:style {:display "flex" :gap 8}}
                (for [r game/resource-types]
                  ^{:key (str "bonus-res-" (name r))}
                  [:button
                   {:on-click #(send-resolve-bonus! r)
                    :style {:background "#111" :padding "10px 16px" :border-radius 6
                            :border (str "2px solid " (get game/resource-colors r "#888"))
                            :color (get game/resource-colors r "#ccc")
                            :cursor "pointer" :font-size 18
                            :min-height 44 :min-width 44
                            :touch-action "manipulation"}}
                   (str (get game/resource-icons r "") " " (name r))])]
               ;; City picker
               :pick-city
               (let [my-pdata (game/player-data state my-player)
                     cities (or (:eligible-cities bonus)
                                (case (:filter bonus)
                                  :magistrate-and-my-temple
                                  (filter (set (vals (:magistrates state)))
                                          (keys (:temples my-pdata)))
                                  :magistrate (distinct (vals (:magistrates state)))
                                  :adjacent (get-in state [:city-graph (:caravan my-pdata)])
                                  :adjacent-to-raider
                                  (distinct (mapcat (fn [[a b]] [a b])
                                                    (keys (:raiders my-pdata))))
                                  (keys (:city-graph state))))]
                 [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
                  (for [city cities]
                    ^{:key (str "bonus-city-" (name city))}
                    [:button
                     {:on-click #(send-resolve-bonus! city)
                      :style {:background "#1a2a1a" :color "#8f8"
                              :border "1px solid #4a4" :border-radius 6
                              :padding "10px 16px" :cursor "pointer" :font-size 14
                              :min-height 44 :min-width 44
                              :touch-action "manipulation"}}
                     (str/capitalize (name city))])])
               ;; Role picker (optionally filtered by :options from passive)
               :pick-role
               (let [available (or (:options bonus) game/roles)]
                 [:div {:style {:display "flex" :gap 8}}
                  (for [r available]
                    ^{:key (str "bonus-role-" (name r))}
                    [:button
                     {:on-click #(send-resolve-bonus! r)
                      :style {:background "#111" :color "#ccc"
                              :border "1px solid #555" :border-radius 6
                              :padding "10px 16px" :cursor "pointer" :font-size 14
                              :min-height 44 :min-width 44
                              :touch-action "manipulation"}}
                     (str (get role-icons r "") " " (name r))])])
               ;; Yes/No picker (for passive effects like "discard X for Y?")
               :yes-no
               [:div {:style {:display "flex" :gap 12}}
                [:button
                 {:on-click #(send-resolve-bonus! :yes)
                  :style {:background "#1a2a1a" :color "#4d4"
                          :border "2px solid #4a4" :border-radius 6
                          :padding "10px 20px" :cursor "pointer" :font-size 16
                          :min-height 44 :min-width 60
                          :touch-action "manipulation" :font-weight "bold"}}
                 "Yes"]
                [:button
                 {:on-click #(send-resolve-bonus! :no)
                  :style {:background "#2a1a1a" :color "#d44"
                          :border "2px solid #a44" :border-radius 6
                          :padding "10px 20px" :cursor "pointer" :font-size 16
                          :min-height 44 :min-width 60
                          :touch-action "manipulation" :font-weight "bold"}}
                 "No"]]
               ;; Fallback
               [:div {:style {:color "#888"}} "Unknown choice type"])]))
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
                               :border-radius 6 :padding 8 :min-width 140 :font-size 11}}
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
         [:div {:style {:flex 1 :min-width 0 :display "flex" :flex-direction "column"}}
          [game-log-component state]
          [bug-report-component]]
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
                (reader/read-string (:pending-claim message))))
      ;; Pending bonus resolution → show choice picker
      (reset! pending-bonus
              (when (:pending-bonus message)
                (reader/read-string (:pending-bonus message)))))

    "chat"
    (println "chat:" (:message message))

    (println "unknown message type:" type)))

;; ── Init ──────────────────────────────────────────────────────────────────────

(defn inject-mobile-styles! []
  (let [style-el (.createElement js/document "style")]
    (set! (.-textContent style-el)
          (str
           ;; Ensure proper mobile viewport behavior
           "* { box-sizing: border-box; }"
           ;; Prevent horizontal overflow on mobile
           "#eridu { overflow-x: hidden; max-width: 100vw; }"
           ;; Make all buttons and clickable elements easier to tap
           "#eridu button { min-height: 44px; min-width: 44px; touch-action: manipulation; }"
           ;; Table rows with on-click need adequate touch height
           "#eridu tr[style*='cursor'] td { padding-top: 10px !important; padding-bottom: 10px !important; }"
           ;; SVG elements should not overflow
           "#eridu svg { max-width: 100%; height: auto; }"
           ;; Mobile-specific adjustments
           "@media (max-width: 600px) {"
           "  #eridu { font-size: 13px; }"
           "  #eridu table { font-size: 11px; }"
           "  #eridu table th, #eridu table td { padding: 6px 3px !important; }"
           "}"))
    (.appendChild (.-head js/document) style-el)))

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
  (inject-mobile-styles!)
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
  (cond
    (url-param "offline")  (do
                             (or (resume-offline-game!)
                                 (start-offline-game! (or (url-param "ai") "default")))
                             (flush-pending-games!))
    :else                  (connect-ws!)))
