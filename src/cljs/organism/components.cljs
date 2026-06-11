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

(defn- ->keyword-map
  "Convert a string-keyed map (from JSON) to keyword-keyed map."
  [m]
  (if (map? m)
    (reduce-kv
     (fn [acc k v]
       (assoc acc (if (string? k) (keyword k) k) v))
     {} m)
    m))

(defn fetch-suggestions! [slot-id query game-type]
  (if (and (string? query) (>= (count query) 1))
    (ajax-core/GET "/api/search-players"
      {:params (cond-> {:q query}
                 game-type (assoc :game-type game-type))
       :handler (fn [response]
                  (let [players (or (get response "players")
                                    (get response :players []))
                        normalised (mapv ->keyword-map players)]
                    (swap! player-suggestions assoc slot-id normalised)
                    (reset! suggestion-highlight -1)))
       :error-handler (fn [_] nil)})
    (do (swap! player-suggestions dissoc slot-id)
        (reset! suggestion-highlight -1))))

(defn select-suggestion! [slot-id suggestion on-select]
  (reset! active-suggestion nil)
  (reset! suggestion-highlight -1)
  (swap! player-suggestions dissoc slot-id)
  (when on-select (on-select suggestion)))

(defn player-search-input
  "Autocomplete player input. Props:
   :slot-id     — unique key for this slot (e.g. index or keyword)
   :value       — current input value
   :color       — background color
   :placeholder — placeholder text
   :game-type   — e.g. \"organism\", used to look up bots
   :on-change   — (fn [new-value]) called on every keystroke
   :on-select   — (fn [{:name :bot? :description}]) called when a suggestion is picked
   :on-focus    — (fn []) called on focus (optional)
   :on-blur     — (fn []) called on blur (optional)
   :search?     — whether to enable search (false = plain input)"
  [{:keys [slot-id value color placeholder game-type on-change on-select on-focus on-blur search?]
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
             (fetch-suggestions! slot-id v game-type))))}]
     ;; Autocomplete dropdown
     (when (and (= @active-suggestion slot-id) (seq suggestions))
       [:div {:style {:position "absolute" :top "100%" :left "30px" :z-index 100
                      :background "#222" :border "1px solid #555" :border-radius "8px"
                      :max-height "240px" :overflow-y "auto" :width "366px"}}
        (for [[i suggestion] (map-indexed vector suggestions)
              :let [highlighted? (= i hl)
                    sname (:name suggestion)
                    bot?  (:bot? suggestion)]]
          [:div {:key (str sname "-" i)
                 :on-mouse-down (fn [e]
                                  (.preventDefault e)
                                  (select-suggestion! slot-id suggestion on-select))
                 :style {:padding "8px 20px" :cursor "pointer" :color "#fff"
                         :background (cond
                                       (and bot? highlighted?) "#3A5A2A"
                                       bot? "#2A4A1A"
                                       highlighted? "#444"
                                       :else "transparent")
                         :font-size "1.2em" :letter-spacing "4px"
                         :font-family "monospace"
                         :display "flex" :align-items "center"
                         :justify-content "space-between"}
                 :on-mouse-enter #(reset! suggestion-highlight i)
                 :on-mouse-leave #(reset! suggestion-highlight -1)}
           [:span sname]
           (when bot?
             [:span {:style {:color "#88CC66" :font-size "0.7em" :letter-spacing "2px"
                             :margin-left "10px"}}
              "(bot)"])])])]))

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

;; ── Active games (observe) display ──────────────────────────────────────────

(defn active-game-card
  "Render a single active/observed game card. Props:
   :game-key       — the game key
   :invocation     — full invocation (has :players :description etc.)
   :round          — current round number
   :current-player — the player whose turn it is
   :colors         — vector of player colors (parallel to :players)
   :link-prefix    — URL prefix for game links (e.g. \"/organism/play/\")
   :player-link-prefix — URL prefix for player links (e.g. \"/organism/player/\")
   :font-family    — optional font"
  [{:keys [game-key invocation round current-player colors link-prefix
           player-link-prefix font-family]
    :or {font-family "monospace"}}]
  (let [{:keys [players description]} invocation
        player-colors (into {} (map vector players (or colors (repeat "#444"))))
        current-color (or (get player-colors current-player) (first colors) "#445")]
    [:div {:style {:margin "10px 20px" :padding "10px 0px"}}
     [:span
      [:a {:href (str link-prefix game-key)
           :style {:color "#fff"
                   :border-radius "15px"
                   :background current-color
                   :padding "10px 20px"
                   :letter-spacing "5px"
                   :font-family font-family
                   :font-size "1.3em"
                   :text-decoration "none"}}
       game-key]]
     (when (and description (not (string/blank? description)))
       [:span {:style {:margin "0px 20px" :color "#888"
                       :font-style "italic"}}
        description])
     (when round
       [:span {:style {:margin "0px 20px" :color "#aaa"}}
        (str " round " (inc (or round 0)))])
     (for [game-player players
           :let [color (get player-colors game-player)]]
       ^{:key game-player}
       [:span
        [:a {:href (str player-link-prefix game-player)
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
         game-player]])]))

(defn observe-page
  "Complete observe page renderer. Props:
   :title          — page title (default \"observe\")
   :games          — seq of active game records ({:key :invocation :round :current-player})
   :link-prefix    — game link prefix
   :player-link-prefix — player link prefix
   :colors-fn      — (fn [invocation]) → vector of player colors
   :home-path      — link for the title
   :font-family    — optional font
   :title-bg       — optional header background color"
  [{:keys [title games link-prefix player-link-prefix colors-fn home-path
           font-family title-bg]
    :or {title "observe" title-bg "#333"}}]
  [:div {:style {:padding "20px" :color "#eee"}}
   [:div {:style {:color "#fff"
                  :border-radius "50px"
                  :letter-spacing "8px"
                  :font-family (or font-family "monospace")
                  :margin "0px 20px"
                  :padding "25px 60px"
                  :background title-bg}}
    [:h1 [:a {:style {:color "#fff" :text-decoration "none"}
              :href (or home-path "/")} title]]]
   (if (empty? games)
     [:p {:style {:margin "30px 40px" :color "#888"}} "no active games"]
     [:div {:style {:margin "20px 40px"}}
      (for [{:keys [key invocation round current-player]} games
            :let [colors (when colors-fn (colors-fn invocation))]]
        ^{:key key}
        [active-game-card {:game-key key
                           :invocation invocation
                           :round round
                           :current-player current-player
                           :colors colors
                           :link-prefix link-prefix
                           :player-link-prefix player-link-prefix
                           :font-family font-family}])])])

;; ── Player stats page ──────────────────────────────────────────────────────

(def ^:private stat-column-hues
  {:playing (rand) :complete (rand) :won (rand) :created (rand)})

(defn- col-color
  [hue ratio]
  (let [lightness (js/Math.round (+ 20 (* 50 (or ratio 0))))]
    (str "hsl(" (js/Math.round (* hue 360)) ",55%," lightness "%)")))

(defn- stat-cell
  [label value color]
  [:span
   {:style {:display "inline-flex"
            :flex-direction "column"
            :align-items "center"
            :color "#fff"
            :border-radius "20px"
            :background color
            :padding "7px 20px"
            :margin "0px 10px"}}
   [:span {:style {:font-size "1.1em"}} value]
   [:span {:style {:font-size "0.6em" :letter-spacing "2px"
                   :opacity "0.7" :margin-top "2px"}} label]])

(defn players-page
  "Complete players/stats page renderer. Props:
   :title              — page title (default \"players\")
   :stats              — seq of {:key :color :active :complete :wins :created}
   :player-link-prefix — URL prefix for player profile links
   :home-path          — link for the title
   :font-family        — optional font
   :title-bg           — optional header background"
  [{:keys [title stats player-link-prefix home-path font-family title-bg]
    :or {title "players" title-bg "#333"}}]
  (let [col-max  (fn [k] (apply max 1 (map k stats)))
        max-active   (col-max :active)
        max-complete (col-max :complete)
        max-wins     (col-max :wins)
        max-created  (col-max :created)]
    [:div {:style {:padding "20px" :color "#eee"}}
     [:div {:style {:color "#fff"
                    :border-radius "50px"
                    :letter-spacing "8px"
                    :font-family (or font-family "monospace")
                    :margin "0px 20px"
                    :padding "25px 60px"
                    :background title-bg}}
      [:h1 [:a {:style {:color "#fff" :text-decoration "none"}
                :href (or home-path "/")} title]]]
     (if (empty? stats)
       [:p {:style {:margin "30px 40px" :color "#888"}} "no players yet"]
       [:div {:style {:margin "20px 40px"}}
        (for [{:keys [key color active complete wins created]} stats]
          ^{:key key}
          [:div {:style {:margin "10px 20px" :padding "10px 0px"
                         :display "flex" :align-items "center"
                         :flex-wrap "wrap" :gap "4px"}}
           [:a {:href (str player-link-prefix "/" key)
                :style {:color "#fff"
                        :border-radius "15px"
                        :background (or color "#444")
                        :padding "10px 20px"
                        :letter-spacing "5px"
                        :font-family (or font-family "monospace")
                        :font-size "1.3em"
                        :margin-right "10px"
                        :text-decoration "none"}}
            key]
           [stat-cell "playing"  active
            (col-color (:playing  stat-column-hues) (/ active   max-active))]
           [stat-cell "complete" complete
            (col-color (:complete stat-column-hues) (/ complete max-complete))]
           [stat-cell "won"      wins
            (col-color (:won      stat-column-hues) (/ wins     max-wins))]
           [stat-cell "created"  created
            (col-color (:created  stat-column-hues) (/ created  max-created))]])])]))

;; ── Create lobby (shared create form) ───────────────────────────────────────

(defn create-lobby
  "Canonical create form shared by all games. Renders a game-name field and a
   dynamic list of player slots (human → autocomplete search, bot → plain
   input), validates, then POSTs {:play-name :players :bots} and boots into the
   game on success.

   Props:
   :game-type       — string, e.g. \"future\" (drives bot autocomplete + slot ids)
   :title           — heading text (default \"New Game\")
   :current-player  — logged-in player name, seeds slot 1 (optional)
   :min-players     — minimum slots (default 1)
   :max-players     — maximum slots (default 5)
   :post-url        — where to POST (default \"/<game-type>/create\")
   :play-url-prefix — redirect prefix on success (default \"/<game-type>/play/\")
   :accent          — accent color for headings/labels (default \"#7AAAE0\")
   :slot-bg         — background for human search inputs (default \"#10182A\")
   :background      — page background (default \"#04040E\")"
  [{:keys [game-type title current-player min-players max-players
           post-url play-url-prefix accent slot-bg background]
    :or   {title "New Game" min-players 1 max-players 5
           accent "#7AAAE0" slot-bg "#10182A" background "#04040E"}}]
  (let [post-url        (or post-url (str "/" game-type "/create"))
        play-url-prefix (or play-url-prefix (str "/" game-type "/play/"))
        play-name (r/atom "")
        slots     (r/atom (let [n    (min max-players (max min-players 2))
                                base [{:name (or current-player "") :bot? false}
                                      {:name "" :bot? true}]]
                            (vec (take n (concat base (repeat {:name "" :bot? false}))))))
        error     (r/atom nil)]
    (fn []
      (let [ss          @slots
            input-style {:background "#111" :color "#ccc"
                         :border "1px solid #334" :border-radius "4px"
                         :padding "8px 12px" :font-family "monospace"}
            btn-style   {:background slot-bg :color accent
                         :border (str "1px solid " accent) :border-radius "4px"
                         :padding "6px 14px" :cursor "pointer"
                         :font-family "monospace"}]
        [:div {:style {:color "#AABBCC" :padding "48px"
                       :font-family "monospace" :background background
                       :min-height "100vh"}}
         [:h2 {:style {:color accent :margin-bottom "24px"}} title]
         ;; Game name
         [:div {:style {:margin-bottom "20px"}}
          [:label {:style {:color "#556677" :display "block" :margin-bottom "6px"}}
           "Game name"]
          [:input {:type "text" :value @play-name
                   :on-change #(reset! play-name (-> % .-target .-value))
                   :placeholder "my-game"
                   :style (merge input-style {:width "260px"})}]]
         ;; Player slots
         [:div {:style {:margin-bottom "20px"}}
          [:label {:style {:color "#556677" :display "block" :margin-bottom "10px"}}
           (str "Players (" min-players "–" max-players ")")]
          (for [i (range (count ss))]
            (let [{:keys [name bot?]} (nth ss i)]
              ^{:key i}
              [:div {:style {:display "flex" :align-items "center"
                             :gap "8px" :margin-bottom "8px"}}
               [:span {:style {:color "#445566" :width "20px"}} (str (inc i) ".")]
               (if bot?
                 [:input {:type "text" :value name
                          :on-change #(swap! slots assoc-in [i :name] (-> % .-target .-value))
                          :placeholder "Bot name"
                          :style (merge input-style {:width "180px"})}]
                 [player-search-input
                  {:slot-id     (str game-type "-" i)
                   :value       name
                   :color       slot-bg
                   :search?     true
                   :game-type   game-type
                   :placeholder "Player name"
                   :on-change   (fn [v] (swap! slots assoc-in [i :name] v))
                   :on-select   (fn [s] (swap! slots update i merge
                                               {:name (:name s)
                                                :bot? (boolean (:bot? s))}))}])
               [:button
                {:on-click #(swap! slots update-in [i :bot?] not)
                 :style (merge btn-style
                               {:padding "6px 14px" :font-size "12px"
                                :background (if bot? "#1A2810" slot-bg)
                                :color (if bot? "#88CC66" accent)})}
                (if bot? "BOT" "HUMAN")]
               (when (> (count ss) min-players)
                 [:button
                  {:on-click #(swap! slots (fn [v] (vec (concat (subvec v 0 i)
                                                                (subvec v (inc i))))))
                   :style (merge btn-style {:padding "6px 10px" :font-size "12px"
                                            :color "#886666" :border-color "#4A2A2A"})}
                  "✕"])]))]
         ;; Add player
         (when (< (count ss) max-players)
           [:button {:on-click #(swap! slots conj {:name "" :bot? true})
                     :style (merge btn-style {:margin-bottom "20px"})}
            "+ Add Player"])
         ;; Error
         (when @error
           [:div {:style {:color "#CC4444" :margin-bottom "12px"}} @error])
         ;; Create
         [:button
          {:on-click
           (fn []
             (let [pname   (string/trim @play-name)
                   players (mapv #(string/trim (:name %)) ss)
                   bots    (vec (keep-indexed #(when (:bot? %2) (string/trim (:name %2))) ss))]
               (cond
                 (string/blank? pname)
                 (reset! error "Game name is required")
                 (some string/blank? players)
                 (reset! error "All player names are required")
                 (not= (count players) (count (set players)))
                 (reset! error "Player names must be unique")
                 :else
                 (do (reset! error nil)
                     (ajax-core/POST post-url
                       {:params          {:play-name pname :players players :bots bots}
                        :format          :transit
                        :response-format :transit
                        :handler         (fn [resp]
                                           (let [pk (or (:play-key resp) (get resp "play-key"))]
                                             (set! (.-location js/window)
                                                   (str play-url-prefix pk))))
                        :error-handler   (fn [err]
                                           (reset! error (str "Create failed: " (pr-str err))))})))))
           :style (merge btn-style {:padding "12px 36px" :font-size "16px"
                                    :background "#1A2810" :color "#88CC66"
                                    :border-color "#4A4"})}
          "Create Game"]]))))
