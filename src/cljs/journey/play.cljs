(ns journey.play
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [journey.game :as game]
   [journey.choice :as choice]
   [journey.board :as board]
   [organism.ajax :as ajax]
   [organism.websockets :as ws]))

;; ── State ─────────────────────────────────────────────────────────────────────

(defonce game-state
  (r/atom nil))

(defonce player-key
  (r/atom (when (exists? js/playerKey) js/playerKey)))

(defonce player-preferences
  (r/atom {}))

(defonce observe-games
  (r/atom []))

;; ── Helpers ───────────────────────────────────────────────────────────────────

(defn choice-player
  "The player who must make the current choice (may differ from current-player
   for phases like :choose-activate-owner-bonus)."
  [state]
  (or (get-in state [:player-turn :choice-player])
      (game/current-player state)))

(defn my-turn?
  "True if the local player is the one who must choose right now."
  [state my-player]
  (and (some? state)
       (= (choice-player state) my-player)
       (not (:game-over state))))

;; ── Choice → UI mapping ───────────────────────────────────────────────────────

(defn hex-pos? [k]
  (and (vector? k) (= 2 (count k)) (number? (first k))))

(defn wrap-choice? [k]
  (and (vector? k) (= 2 (count k)) (= :wrap (first k))))

(defn choice-label
  "Human-readable label for a choice key."
  [k]
  (cond
    (keyword? k)  (name k)
    (integer? k)  (str k " bonus")
    (hex-pos? k)  (str "[" (first k) "," (second k) "]")
    (wrap-choice? k) (str "wrap → " (choice-label (second k)))
    (map? k)      (str (name (:type k)) " @ " (choice-label (:target k)))
    :else         (pr-str k)))

(defn partition-choices
  "Split choices into pos-highlights (hex clicks) and button-choices (everything else).
   Wrap choices become separate buttons."
  [choices]
  (reduce-kv
   (fn [[pos-set btns] k _v]
     (cond
       (hex-pos? k)   [(conj pos-set k) btns]
       :else          [pos-set (conj btns {:label (choice-label k) :choice-key k})]))
   [#{} []]
   choices))

;; ── WebSocket communication ───────────────────────────────────────────────────

(defn send-action!
  "Send a choice to the server."
  [choice-key]
  (ws/send-transit-message! {:type "action" :choice (pr-str choice-key)}))

;; ── Choice interaction ────────────────────────────────────────────────────────

(defn on-hex-click [state my-player choices pos]
  (when (my-turn? state my-player)
    (when-let [_ (get choices pos)]
      (send-action! pos))))

(defn on-button-click [choice-key]
  (send-action! choice-key))

;; ── Interactive board view (pan / zoom) ──────────────────────────────────────

(defn board-view
  "Wraps render-game with mouse-drag pan and keyboard/slider zoom.
   Arrow keys pan; PageUp/PageDown zoom; drag the starfield to pan.
   Optional on-navigate fn(dir) — when provided, Up/Down call it instead of panning."
  [state pos-highlights on-hex-click choice-buttons & [{:keys [on-navigate]}]]
  (r/with-let
    [pan-x (r/atom 0)
     pan-y (r/atom 0)
     zoom  (r/atom 1.0)
     drag  (r/atom nil)
     on-key
     (fn [e]
       (case (.-key e)
         "ArrowLeft"  (swap! pan-x + 60)
         "ArrowRight" (swap! pan-x - 60)
         "ArrowUp"    (if on-navigate (on-navigate :up)   (swap! pan-y + 60))
         "ArrowDown"  (if on-navigate (on-navigate :down) (swap! pan-y - 60))
         "PageUp"     (swap! zoom #(min 4.0 (* % 1.15)))
         "PageDown"   (swap! zoom #(max 0.2  (/ % 1.15)))
         " "          (do (reset! pan-x 0) (reset! pan-y 0) (reset! zoom 1.0))
         nil))
     on-move
     (fn [e]
       (when-let [{:keys [mx my px py]} @drag]
         (reset! pan-x (+ px (- (.-clientX e) mx)))
         (reset! pan-y (+ py (- (.-clientY e) my)))))
     on-up (fn [_] (reset! drag nil))
     _     (do (js/document.addEventListener "keydown" on-key)
               (js/document.addEventListener "mousemove" on-move)
               (js/document.addEventListener "mouseup" on-up))]
    [:div {:style {:position "relative" :width "100%" :height "100%"}}
     [board/render-game state pos-highlights on-hex-click choice-buttons
      {:pan-x @pan-x :pan-y @pan-y :zoom @zoom
       :on-bg-mouse-down
       (fn [e]
         (.preventDefault e)
         (reset! drag {:mx (.-clientX e) :my (.-clientY e)
                       :px @pan-x :py @pan-y}))}]
     [:div {:style {:position "absolute" :bottom "16px" :left "16px"
                    :display "flex" :align-items "center" :gap "8px"}}
      [:input {:type "range" :min 20 :max 400 :step 5
               :value (int (* @zoom 100))
               :on-change #(reset! zoom (/ (js/parseInt (.. % -target -value)) 100))
               :style {:width "100px" :cursor "pointer"}}]
      [:span {:style {:color "#334455" :font-family "monospace" :font-size "10px"
                      :min-width "32px"}}
       (str (int (* @zoom 100)) "%")]]]
    (finally
      (js/document.removeEventListener "keydown" on-key)
      (js/document.removeEventListener "mousemove" on-move)
      (js/document.removeEventListener "mouseup" on-up))))

;; ── Play page ─────────────────────────────────────────────────────────────────

(defn game-phase-label [state]
  (let [phase (game/current-phase state)
        cp    (choice-player state)]
    (str (name (or phase :?)) " — " cp)))

(defn play-page []
  (let [state @game-state
        my    @player-key]
    (if (nil? state)
      [:div {:style {:color "#556" :padding "40px" :font-family "monospace"}}
       [:p "Waiting for game state…"]]
      (let [[_ choices]       (choice/find-state state)
            active?           (my-turn? state my)
            [pos-hl btn-choices] (if active?
                                   (partition-choices choices)
                                   [#{} []])
            on-click          (when active?
                                (partial on-hex-click state my choices))
            buttons           (when active?
                                (map (fn [{:keys [label choice-key]}]
                                       {:label    label
                                        :on-click #(on-button-click choice-key)})
                                     btn-choices))]
        [:div {:style {:width "100vw" :height "100vh"
                       :overflow "hidden" :background "#04040E"}}
         ;; Phase/turn indicator strip
         [:div {:style {:position "absolute" :top "6px" :left "8px"
                        :color "#334455" :font-size "11px"
                        :font-family "monospace" :z-index 10}}
          (game-phase-label state)
          (when active? " ← YOUR TURN")]
         ;; Game-over banner
         (when-let [go (:game-over state)]
           [:div {:style {:position "absolute"
                          :top "40%" :left "50%"
                          :transform "translate(-50%,-50%)"
                          :background "#0A0E1C"
                          :border "2px solid #3A5090"
                          :border-radius "8px"
                          :padding "32px 48px"
                          :color "#AAC8EE"
                          :font-family "monospace"
                          :font-size "18px"
                          :z-index 20
                          :text-align "center"}}
            [:div {:style {:font-size "22px" :margin-bottom "12px"}} "GAME OVER"]
            (if (= :landing (:type go))
              [:div
               [:div (str "Landing at " (pr-str (:tile go)))]
               (for [[p sc] (:scores go)]
                 [:div {:key p :style {:margin-top "4px"}}
                  (str p ": " sc " pts")])]
              [:div (str "Loss — captain: " (:captain go))])])
         ;; SVG board
         [board-view state pos-hl on-click buttons]]))))

;; ── Create / Observe pages ────────────────────────────────────────────────────

(defn create-page []
  (let [players-input (r/atom "")]
    (fn []
      [:div {:style {:color "#AABBCC" :padding "48px"
                     :font-family "monospace" :background "#04040E"
                     :min-height "100vh"}}
       [:h2 {:style {:color "#7AAAE0" :margin-bottom "24px"}} "JOURNEY — New Game"]
       [:p {:style {:color "#445566" :margin-bottom "12px"}}
        "Enter player names, comma-separated:"]
       [:input {:type "text"
                :value @players-input
                :on-change #(reset! players-input (-> % .-target .-value))
                :style {:background "#0A0E1C" :color "#AACCEE"
                        :border "1px solid #2A4A80" :border-radius "4px"
                        :padding "8px 12px" :font-family "monospace"
                        :font-size "14px" :width "320px"}}]
       [:button
        {:on-click #(ws/send-transit-message!
                     {:type    "create"
                      :players (->> (str/split @players-input #",")
                                    (map str/trim)
                                    (remove empty?)
                                    vec)})
         :style {:margin-left "12px"
                 :background "#10182A" :color "#7AAAE0"
                 :border "1px solid #2A4A80" :border-radius "4px"
                 :padding "8px 20px" :cursor "pointer"
                 :font-family "monospace" :font-size "14px"}}
        "Create"]])))

(defn observe-page []
  [:div {:style {:color "#AABBCC" :padding "48px"
                 :font-family "monospace" :background "#04040E"
                 :min-height "100vh"}}
   [:h2 {:style {:color "#7AAAE0"}} "JOURNEY — Observe"]
   [play-page]])

;; ── WebSocket messages ────────────────────────────────────────────────────────

(defn update-messages! [{:keys [type state] :as received}]
  (condp = type
    "game-state"
    (when state
      (reset! game-state (reader/read-string state)))

    "initialize"
    (when state
      (reset! game-state (reader/read-string state)))

    (js/console.log "unknown message" (pr-str received))))

;; ── Generate (simulation replay with choice log) ─────────────────────────────

;; Memoized list-row component: only re-renders when selected? changes.
;; The parent's for-loop still runs in O(n) but creates only cheap
;; [component ...] vectors; the actual DOM work runs for ≤2 rows per tick.
(def ^:private history-row
  (r/create-class
   {:display-name "history-row"
    :should-component-update
    (fn [_ [_ _ _ _ old-sel?] [_ _ _ _ new-sel?]]
      (not= old-sel? new-sel?))
    :reagent-render
    (fn [i {:keys [step player phase choice]} player-colors selected? group-start? show-sep? on-click]
      (let [ck   (get player-colors player :sun)
            fc   (board/ptb ck)
            bg-c (if (= ck :void) "#8888AA" fc)]
        [:div
         (when show-sep?
           [:div {:style {:height "1px" :margin "3px 0"
                          :background (str bg-c "66")}}])
         (when group-start?
           [:div {:style {:padding "4px 10px 1px"
                          :color (if selected? fc (str fc "99"))
                          :font-size "9px" :font-family "monospace"
                          :letter-spacing "1px"
                          :background (str bg-c "44")}}
            (str "▸ " (or player "—"))])
         [:div
          {:id       (str "hist-item-" i)
           :on-click on-click
           :style {:padding "4px 10px 4px 14px" :cursor "pointer"
                   :border-left (str "2px solid "
                                     (if selected? bg-c (str bg-c "88")))
                   :background (if selected? (str bg-c "60") (str bg-c "30"))}}
          [:div {:style {:color (if selected? fc (str fc "CC"))
                         :font-size "12px" :font-family "monospace"}}
           (str "·" step "  " (name phase))]
          [:div {:style {:color (if selected? (str fc "DD") (str fc "88"))
                         :font-size "10px" :font-family "monospace"
                         :word-break "break-all"}}
           choice]]]))}))

(defn generate-page []
  (r/with-let
      [history       (when (exists? js/generateHistory)
                       (reader/read-string js/generateHistory))
       n             (count history)
       player-order  (:turn-order (:state (first history)))
       player-colors (board/build-player-colors player-order)
       selected       (r/atom 0)
       playing?       (r/atom false)
       interval       (r/atom nil)
       history-focus? (r/atom false)
       ;; Stop interval without changing playing? flag
       stop-interval!
       (fn []
         (when-let [id @interval]
           (js/clearInterval id)
           (reset! interval nil)))
       ;; Advance one step; auto-stop at end
       advance!
       (fn []
         (let [next (inc @selected)]
           (if (< next n)
             (reset! selected next)
             (do (reset! playing? false)
                 (stop-interval!)))))
       ;; Start playback from current position
       start!
       (fn []
         (stop-interval!)
         (reset! playing? true)
         (reset! interval (js/setInterval advance! 40)))
       ;; Manual step — always pauses playback
       navigate!
       (fn [dir]
         (stop-interval!)
         (reset! playing? false)
         (swap! selected #(case dir
                            :up   (min (dec n) (inc %))
                            :down (max 0 (dec %)))))
       ;; Single capture-phase listener: intercepts keys when history has focus,
       ;; preventing the board's own keydown handler from also firing.
       ;; When history is not focused this handler is a no-op, so the board pans normally.
       on-key
       (fn [e]
         (case (.-key e)
           "Escape"
           (do (.preventDefault e) (swap! history-focus? not))
           (when @history-focus?
             (case (.-key e)
               "ArrowUp"
               (do (.preventDefault e) (.stopImmediatePropagation e) (navigate! :up))
               "ArrowDown"
               (do (.preventDefault e) (.stopImmediatePropagation e) (navigate! :down))
               " "
               (do (.preventDefault e) (.stopImmediatePropagation e)
                   (if @playing?
                     (do (stop-interval!) (reset! playing? false))
                     (start!)))
               nil))))
       _ (js/document.addEventListener "keydown" on-key #js {:capture true})]

      (let [sel     @selected
            focused @history-focus?
            entry   (nth history sel)
            state   (:state entry)]
        ;; Outer mousedown clears history focus; panel mousedown stops propagation
        [:div {:style    {:display "flex" :width "100vw" :height "100vh"
                          :background "#04040E" :overflow "hidden"}
               :on-mouse-down #(reset! history-focus? false)}

         ;; Board — no on-navigate; Up/Down always pan when history is not focused
         [:div {:style {:flex "1" :overflow "hidden"}}
          [board-view state #{} nil nil]]

         ;; History panel — mousedown stops propagation so outer blur doesn't fire
         [:div {:style         {:width "250px" :display "flex" :flex-direction "column"
                                :background "#05060F" :border-left "1px solid #141830"
                                :outline (if focused "1px solid #1E3A5A" "none")}
                :on-mouse-down #(.stopPropagation %)}

          ;; Header: step counter + play/pause
          [:div {:style {:display "flex" :align-items "center"
                         :padding "6px 10px 5px"
                         :border-bottom "1px solid #141830"}}
           [:span {:style {:flex "1" :color "#334455" :font-size "9px"
                           :font-family "monospace" :letter-spacing "1.5px"
                           :text-transform "uppercase"}}
            (str "step " (:step entry) " / " (dec n))]
           [:button
            {:on-click #(if @playing? (do (stop-interval!) (reset! playing? false)) (start!))
             :style    {:background "none" :border "1px solid #1E2A3A"
                        :border-radius "3px" :color "#445566"
                        :font-size "12px" :cursor "pointer"
                        :padding "1px 7px" :font-family "monospace"
                        :line-height "1.4"}}
            (if @playing? "⏸" "▶")]]

          ;; Scrollable step list — newest entry on top, oldest at bottom
          ;; Keep selected item in view after every render
          (do
            (r/after-render
             #(when-let [el (.getElementById js/document (str "hist-item-" sel))]
                (.scrollIntoView el #js {:block "nearest" :behavior "instant"})))
            [:div {:style {:flex "1" :overflow-y "auto" :padding "4px 0"}}
             (for [i (range (dec n) -1 -1)]
               (let [{:keys [player] :as entry} (nth history i)
                     above-player (:player (when (< i (dec n)) (nth history (inc i))))
                     group-start? (not= player above-player)
                     show-sep?    (and group-start? (< i (dec n)))]
                 ^{:key i}
                 [history-row i entry player-colors (= i sel) group-start? show-sep?
                  #(do (stop-interval!)
                       (reset! playing? false)
                       (reset! history-focus? true)
                       (reset! selected i))]))])]])

      (finally
        (stop-interval!)
        (js/document.removeEventListener "keydown" on-key #js {:capture true}))))

;; ── Page container ────────────────────────────────────────────────────────────

(defn page-container []
  (cond
    js/isGenerate [generate-page]
    js/isObserve  [observe-page]
    js/isCreate   [create-page]
    js/playKey    [play-page]
    :else         [:div]))

;; ── Init ──────────────────────────────────────────────────────────────────────

(defn mount-components []
  (rdom/render [#'page-container] (.getElementById js/document "journey")))

(defn init! []
  (ajax/load-interceptors!)
  (when js/playerPreferences
    (reset! player-preferences (reader/read-string js/playerPreferences)))
  (when js/isObserve
    (when js/observeGames
      (reset! observe-games (reader/read-string js/observeGames))))
  (when js/playKey
    (let [protocol (if (= (.-protocol js/location) "https:") "wss:" "ws:")]
      (ws/make-websocket!
       (str protocol "//" (.-host js/location) "/ws/journey/play/" js/playKey)
       update-messages!)))
  (mount-components))
