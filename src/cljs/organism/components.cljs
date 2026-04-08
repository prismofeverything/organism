(ns organism.components
  "Shared UI + state for game create pages.
   This is the canonical implementation — each game's create page should
   build on top of these primitives rather than duplicating the pattern.

   Provides:
   - player-search-input: autocomplete player input with arrow nav
   - open-game-card / open-games-section: render open games with player slots
   - WebSocket helpers: connect-create-ws!, send-open-game!, send-player-name!, send-create!
   - State: create-game-key, player-suggestions
   - Init helper: preload-create-state! (reads js/openInvocation, js/playKey)"
  (:require
   [clojure.string :as string]
   [reagent.core :as r]
   [cljs.reader :as reader]
   [ajax.core :as ajax-core]
   [organism.websockets :as ws]))

;; ── Shared atoms ────────────────────────────────────────────────────────────

(defonce create-game-key (r/atom ""))
(defonce player-suggestions (r/atom {}))    ;; {slot-id -> [name1 name2 ...]}
(defonce active-suggestion (r/atom nil))    ;; which slot-id has the dropdown open
(defonce suggestion-highlight (r/atom -1))  ;; keyboard-highlighted row index

;; ── WebSocket helpers ───────────────────────────────────────────────────────

(defn connect-create-ws!
  "Connect to a game's create WebSocket. ws-prefix is e.g. \"/ws/organism/play/\"."
  ([ws-prefix game-key update-fn] (connect-create-ws! ws-prefix game-key update-fn nil))
  ([ws-prefix game-key update-fn on-open]
   (when-not (string/blank? game-key)
     (ws/close-websocket!)
     (let [protocol (if (= (.-protocol js/location) "https:") "wss:" "ws:")]
       (ws/make-websocket!
        (str protocol "//" (.-host js/location) ws-prefix game-key)
        update-fn
        on-open)))))

(defn send-create!
  "Send a 'create' message with the current invocation."
  [invocation]
  (when @ws/ws-channel
    (ws/send-transit-message!
     {:type "create"
      :invocation invocation})))

(defn send-open-game!
  "Send an 'open-game' message — persists the open invocation."
  [invocation]
  (when @ws/ws-channel
    (ws/send-transit-message!
     {:type "open-game"
      :invocation invocation})))

(defn send-player-name!
  "Send a 'player-name' message updating slot index."
  [index player-name]
  (when @ws/ws-channel
    (ws/send-transit-message!
     {:type "player-name"
      :index index
      :player player-name})))

(defn send-trigger-creation! []
  (when @ws/ws-channel
    (ws/send-transit-message! {:type "trigger-creation"})))

;; ── Init helpers ────────────────────────────────────────────────────────────

(defn preloaded-invocation
  "Read js/openInvocation if present, returning a parsed invocation map or nil."
  []
  (when (and (exists? js/openInvocation)
             (string? js/openInvocation)
             (not (string/blank? js/openInvocation)))
    (try (reader/read-string js/openInvocation)
         (catch :default _ nil))))

(defn preloaded-play-key
  "Read js/playKey if present, returning the string or nil."
  []
  (when (and (exists? js/playKey)
             (string? js/playKey)
             (not (string/blank? js/playKey)))
    js/playKey))

;; ── Player search autocomplete ──────────────────────────────────────────────

(defn fetch-suggestions! [slot-id query]
  (if (and (string? query) (>= (count query) 1))
    (ajax-core/GET "/api/search-players"
      {:params {:q query}
       :handler (fn [response]
                  (let [players (get response "players" (get response :players []))]
                    (swap! player-suggestions assoc slot-id (vec players))
                    (reset! suggestion-highlight -1)))
       :error-handler (fn [_] nil)})
    (do (swap! player-suggestions dissoc slot-id)
        (reset! suggestion-highlight -1))))

(defn select-suggestion! [slot-id name on-select]
  (reset! active-suggestion nil)
  (reset! suggestion-highlight -1)
  (swap! player-suggestions dissoc slot-id)
  (when on-select (on-select name)))

(defn player-search-input
  "Autocomplete player input. Props:
   :slot-id    — unique key for this slot (e.g. index or keyword)
   :value      — current input value
   :color      — background color
   :placeholder — placeholder text
   :on-change  — (fn [new-value]) called on every keystroke
   :on-select  — (fn [chosen-name]) called when a suggestion is picked
   :on-focus   — (fn []) called on focus (optional)
   :on-blur    — (fn []) called on blur (optional)
   :search?    — whether to enable search (false = plain input)"
  [{:keys [slot-id value color placeholder on-change on-select on-focus on-blur search?]
    :or {search? true placeholder "search players..."}}]
  (let [suggestions (get @player-suggestions slot-id [])
        hl @suggestion-highlight]
    [:div {:style {:position "relative"}}
     [:input
      {:value value
       :placeholder placeholder
       :style {:border-radius "25px" :color "#fff"
               :background (or color "#333")
               :border-color (or color "#333")
               :border "3px solid"
               :font-size "1.5em" :letter-spacing "6px"
               :margin "2px 0px" :width "366px" :padding "10px 30px"}
       :on-focus (fn [_]
                   (when on-focus (on-focus))
                   (when search?
                     (reset! active-suggestion slot-id)
                     (reset! suggestion-highlight -1)))
       :on-blur (fn [_]
                  (js/setTimeout
                   (fn []
                     (reset! active-suggestion nil)
                     (reset! suggestion-highlight -1)
                     (swap! player-suggestions dissoc slot-id))
                   200)
                  (when on-blur (on-blur)))
       :on-key-down
       (fn [e]
         (when (and (= @active-suggestion slot-id) (seq suggestions))
           (case (.-key e)
             "ArrowDown" (do (.preventDefault e)
                             (swap! suggestion-highlight
                                    #(min (dec (count suggestions)) (inc %))))
             "ArrowUp"   (do (.preventDefault e)
                             (swap! suggestion-highlight #(max -1 (dec %))))
             "Enter"     (when (and (>= hl 0) (< hl (count suggestions)))
                           (.preventDefault e)
                           (select-suggestion! slot-id (nth suggestions hl) on-select))
             "Escape"    (do (reset! active-suggestion nil)
                             (reset! suggestion-highlight -1)
                             (swap! player-suggestions dissoc slot-id))
             nil)))
       :on-change
       (fn [event]
         (let [v (-> event .-target .-value)]
           (when on-change (on-change v))
           (when search?
             (reset! active-suggestion slot-id)
             (fetch-suggestions! slot-id v))))}]
     ;; Autocomplete dropdown
     (when (and (= @active-suggestion slot-id) (seq suggestions))
       [:div {:style {:position "absolute" :top "100%" :left "30px" :z-index 100
                      :background "#222" :border "1px solid #555" :border-radius "8px"
                      :max-height "200px" :overflow-y "auto" :width "366px"}}
        (for [[i sname] (map-indexed vector suggestions)
              :let [highlighted? (= i hl)]]
          [:div {:key sname
                 :on-mouse-down (fn [e]
                                  (.preventDefault e)
                                  (select-suggestion! slot-id sname on-select))
                 :style {:padding "8px 20px" :cursor "pointer" :color "#fff"
                         :background (if highlighted? "#444" "transparent")
                         :font-size "1.2em" :letter-spacing "4px"
                         :font-family "monospace"}
                 :on-mouse-enter #(reset! suggestion-highlight i)
                 :on-mouse-leave #(reset! suggestion-highlight -1)}
           sname])])]))

;; ── Open games display ──────────────────────────────────────────────────────

(defn open-game-card
  "Render a single open game card.
   Props:
   :game-key    — string game name
   :invocation  — full invocation map
   :colors      — vector of player colors (one per slot, parallel to :players)
   :link-prefix — URL prefix for the game key (e.g. \"/organism/create/\")
   :current-player — logged-in player name (highlighted)
   :font-family — optional font for the title (default monospace)"
  [{:keys [game-key invocation colors link-prefix current-player font-family]
    :or {font-family "monospace"}}]
  (let [{:keys [players ring-count description]} invocation
        first-color (or (first colors) "#445")]
    [:div
     [:div {:style {:margin "10px 20px" :padding "10px 0px"}}
      ;; Game name button
      [:span
       [:a {:href (str link-prefix game-key)
            :style {:color "#fff"
                    :border-radius "15px"
                    :background first-color
                    :padding "10px 20px"
                    :letter-spacing "5px"
                    :font-family font-family
                    :font-size "1.3em"
                    :text-decoration "none"}}
        game-key]]
      (when ring-count
        [:span {:style {:margin "0px 20px" :color "#aaa"}}
         (str " " ring-count " rings ")])
      ;; Player slots
      (for [[i [game-player color]]
            (map-indexed vector (map vector players colors))]
        ^{:key i}
        [:span
         (if (string/blank? game-player)
           ;; Open slot
           [:a {:href (str link-prefix game-key)
                :style {:padding "5px 10px" :margin "0px 10px"
                        :border-style "dashed" :border-width "2px"
                        :border-color (or color "#445") :border-radius "5px"
                        :color (or color "#445")
                        :text-decoration "none"
                        :font-family font-family}}
            "open"]
           ;; Filled slot
           [:a {:href (str link-prefix game-key)
                :style (if (= game-player current-player)
                         {:color "#fff"
                          :border-radius "20px"
                          :background color
                          :margin "0px 10px"
                          :padding "7px 20px"
                          :text-decoration "none"
                          :font-family font-family}
                         {:padding "5px 10px"
                          :margin "0px 10px"
                          :border-style "solid"
                          :border-width "2px"
                          :border-color color
                          :border-radius "5px"
                          :color color
                          :text-decoration "none"
                          :font-family font-family})}
            game-player])])]
     (when (and description (not (string/blank? description)))
       [:div {:style {:margin "0px 40px" :color "#aaa"}}
        description])]))

(defn open-games-section
  "Renders an 'OPEN' header and a list of open games. Props:
   :games          — seq of open game records (each {:key ... :invocation {...}})
   :link-prefix    — URL prefix for game links (e.g. \"/organism/create/\")
   :current-player — logged-in player name
   :colors-fn      — (fn [invocation]) returning a vector of colors per slot
   :font-family    — optional font family"
  [{:keys [games link-prefix current-player colors-fn font-family]}]
  (when (seq games)
    [:div {:style {:margin "20px 40px"}}
     [:h2
      [:span {:title "Click an open slot to join the game"} "OPEN"]]
     (for [{:keys [key invocation]} games
           :let [colors (when colors-fn (colors-fn invocation))]]
       ^{:key key}
       [open-game-card {:game-key key
                        :invocation invocation
                        :colors colors
                        :link-prefix link-prefix
                        :current-player current-player
                        :font-family font-family}])]))
