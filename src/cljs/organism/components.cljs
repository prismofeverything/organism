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
   [organism.base :as base]
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
        ;; game-key is a game name, so it needs encoding like any other one
        (str protocol "//" (.-host js/location) ws-prefix
             (js/encodeURIComponent game-key))
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

;; ── Deletion controls ───────────────────────────────────────────────────────
;;
;; Deleting a shared game is a workflow, not a button — see organism.persist.
;; The server decides between removing a game outright and marking it, so the
;; page just asks and then reloads: what comes back is what actually happened.

(def ^:private delete-btn-style
  {:background "transparent"
   :border "1px solid #4A2A2A"
   :border-radius "4px"
   :color "#886666"
   :padding "5px 10px"
   :margin "0px 10px"
   :font-size "12px"
   :font-family "monospace"
   :cursor "pointer"})

(def ^:private keep-btn-style
  (merge delete-btn-style
         {:border "1px solid #3A5A3A" :color "#88AA77"}))

(defn- time-until
  "Epoch-seconds in the future → \"in 2 days\" / \"in 5 hours\"."
  [secs]
  (when secs
    (let [remaining (- secs (quot (.now js/Date) 1000))
          phrase (fn [n unit] (str "in " n " " unit (when (> n 1) "s")))]
      (cond
        (<= remaining 0)     "any moment now"
        (< remaining 3600)   (phrase (max 1 (quot remaining 60)) "minute")
        (< remaining 86400)  (phrase (quot remaining 3600) "hour")
        :else                (phrase (quot remaining 86400) "day")))))

(defn- delete-confirm-text
  "History count is the forecast: 1 or less means only the initial state exists,
   so nobody has anything invested and the game goes immediately."
  [game-key history-count]
  (if (and history-count (<= history-count 1))
    (str "Delete " game-key "?\n\n"
         "Nothing has happened in this game yet, so it goes right away.")
    (str "Delete " game-key "?\n\n"
         "It gets marked for deletion. Any move, or any player pressing KEEP, "
         "cancels it. If nobody does either, it is removed in two days.")))

(defn game-url
  "A URL for one game. The key is encoded because game names are free text —
   spaces, apostrophes and punctuation are all ordinary here (\"2p Testing!\",
   \"Woogachaka's Game\"), and an unencoded space is not a valid URL."
  [prefix game-key suffix]
  (str (base/join-path prefix (js/encodeURIComponent game-key)) suffix))

(defn player-url
  "A link to a player's page. Player names are free text the same way game
   names are, and the prefix reaches us with or without its trailing slash
   depending on which page is asking."
  [prefix player]
  (base/join-path prefix (js/encodeURIComponent player)))

(defn- post-game-action!
  [url game-key on-done]
  (ajax-core/POST url
    {:params          {}
     :format          :transit
     :response-format :transit
     :handler         (fn [_] (on-done))
     :error-handler   (fn [err]
                        (js/alert
                         (str "Could not do that to " game-key ": "
                              (or (get-in err [:response :error])
                                  (:status-text err)))))}))

(defn request-delete!
  "Ask the server to delete (or mark) a game, then reload so the list shows
   whichever of the two happened."
  [play-prefix game-key]
  (post-game-action! (game-url play-prefix game-key "/delete") game-key
                     #(.reload js/location)))

(defn request-keep!
  "The objection — cancel a pending deletion."
  [play-prefix game-key]
  (post-game-action! (game-url play-prefix game-key "/keep") game-key
                     #(.reload js/location)))

(defn request-join!
  "Take a seat in an open lobby straight from the list. If that fills the last
   seat the server starts the game, and we go to it rather than back to a lobby
   that no longer exists."
  [play-prefix game-key index]
  (ajax-core/POST (game-url play-prefix game-key "/join")
    {:params          {:index index}
     :format          :transit
     :response-format :transit
     :handler         (fn [response]
                        (if (:begun response)
                          (set! (.-location js/window)
                                (game-url play-prefix game-key ""))
                          (.reload js/location)))
     :error-handler   (fn [err]
                        (js/alert (str "Could not join " game-key ": "
                                       (or (get-in err [:response :error])
                                           (:status-text err)))))}))

(defn- delete-control
  [{:keys [game-key history-count on-delete]}]
  (when on-delete
    [:button
     {:title    "delete this game"
      :on-click (fn [_]
                  (when (js/confirm (delete-confirm-text game-key history-count))
                    (on-delete)))
      :style    delete-btn-style}
     "\u2715"]))

(defn- deletion-notice
  "The line under a marked game: who marked it, when it goes, and the one click
   that calls it off."
  [{:keys [deletion on-keep]}]
  (let [{:keys [marked-by deadline]} deletion]
    [:div {:style {:margin "8px 20px 0px 20px" :padding "8px 14px"
                   :border "1px dashed #6A3A3A" :border-radius "6px"
                   :color "#BB8877" :font-size "0.85em"
                   ;; table = block-level (own line) but shrink-to-fit
                   :display "table"}}
     [:span (str "marked for deletion"
                 (when marked-by (str " by " marked-by))
                 " \u2014 removed " (or (time-until deadline) "soon")
                 " unless someone moves")]
     (when on-keep
       [:button {:on-click (fn [_] (on-keep))
                 :style    (merge keep-btn-style {:margin-left "14px"})}
        "KEEP"])]))

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
  [{:keys [game-key invocation colors link-prefix current-player font-family
           on-delete on-join]
    :or {font-family "monospace"}}]
  (let [{:keys [players ring-count description]} invocation
        first-color (or (first colors) "#445")]
    [:div
     [:div {:style {:margin "10px 20px" :padding "10px 0px"}}
      ;; Game name button
      [:span
       [:a {:href (game-url link-prefix game-key "")
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
           ;; Open slot — clicking it seats you here and there is nothing else
           ;; to decide, so it acts rather than sending you to the create page.
           (let [slot-style {:padding "5px 10px" :margin "0px 10px"
                             :border-style "dashed" :border-width "2px"
                             :border-color (or color "#445") :border-radius "5px"
                             :color (or color "#445")
                             :text-decoration "none"
                             :font-family font-family}]
             (if on-join
               [:button {:title (str "take this seat"
                                     (when current-player (str " as " current-player)))
                         :on-click (fn [_] (on-join i))
                         :style (merge slot-style {:background "transparent"
                                                   :cursor "pointer"})}
                "join"]
               [:a {:href (game-url link-prefix game-key "") :style slot-style}
                "open"]))
           ;; Filled slot
           [:a {:href (game-url link-prefix game-key "")
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
            game-player])])
      ;; A lobby nobody has joined has nothing at stake, so this one goes now.
      [delete-control {:game-key game-key
                       :history-count 0
                       :on-delete on-delete}]]
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
  [{:keys [games link-prefix current-player colors-fn font-family on-delete on-join]}]
  (when (seq games)
    [:div {:style {:margin "20px 40px"}}
     [:h2
      [:span {:title "Click an open slot to join the game"} "OPEN"]]
     (for [{:keys [key invocation] :as game} games
           :let [colors (when colors-fn (colors-fn invocation))]]
       ^{:key key}
       [open-game-card {:game-key key
                        :invocation invocation
                        :colors colors
                        :link-prefix link-prefix
                        :current-player current-player
                        :font-family font-family
                        :on-delete (when on-delete #(on-delete game))
                        :on-join (when on-join (fn [index] (on-join game index)))}])]))

;; ── Active games (observe) display ──────────────────────────────────────────

(defn- format-last-move
  "Epoch-seconds → concise local date/time, e.g. \"Jul 14, 2026, 3:45 PM\"."
  [secs]
  (when secs
    (.toLocaleString (js/Date. (* secs 1000))
                     js/undefined
                     #js {:month "short" :day "numeric" :year "numeric"
                          :hour "numeric" :minute "2-digit"})))

(defn- relative-time
  "Epoch-millis → coarse \"N units ago\" phrase (e.g. \"3 days ago\")."
  [ms]
  (when (and ms (pos? ms))
    (let [secs (quot (- (.now js/Date) ms) 1000)
          ago  (fn [n unit] (str n " " unit (when (> n 1) "s") " ago"))]
      (cond
        (< secs 60)       "just now"
        (< secs 3600)     (ago (quot secs 60) "minute")
        (< secs 86400)    (ago (quot secs 3600) "hour")
        (< secs 604800)   (ago (quot secs 86400) "day")
        (< secs 2592000)  (ago (quot secs 604800) "week")
        (< secs 31536000) (ago (quot secs 2592000) "month")
        :else             (ago (quot secs 31536000) "year")))))

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
           player-link-prefix font-family last-move-time]
    :or {font-family "monospace"}}]
  (let [{:keys [players description created]} invocation
        player-colors (into {} (map vector players (or colors (repeat "#444"))))
        current-color (or (get player-colors current-player) (first colors) "#445")
        ;; the player up after the current one (turn order = roster order, cycling)
        next-player   (let [idx (->> players
                                     (keep-indexed (fn [i p] (when (= p current-player) i)))
                                     first)]
                        (when (and idx (seq players))
                          (nth players (mod (inc idx) (count players)))))
        next-color    (or (get player-colors next-player) current-color)
        created-ago (relative-time created)
        last-ago    (relative-time (when last-move-time (* last-move-time 1000)))
        time-parts  (cond-> []
                      created-ago (conj [[:b "created"] (str " " created-ago)])
                      last-ago    (conj [[:b "last move"] (str " " last-ago)]))
        time-title  (string/join
                     " - "
                     (cond-> []
                       created        (conj (str "created " (format-last-move (quot created 1000))))
                       last-move-time (conj (str "last move " (format-last-move last-move-time)))))]
    [:div {:style {:margin "10px 20px" :padding "10px 0px"}}
     [:span
      [:a {:href (game-url link-prefix game-key "")
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
       [:span {:style {:color "#fff"
                       :border-radius "20px"
                       :background next-color
                       :padding "7px 20px"
                       :margin "0px 10px"
                       :vertical-align "middle"
                       :font-family font-family}}
        (str "round " (inc (or round 0)))])
     (when (seq time-parts)
       (into [:span {:style {:display "inline-block" :vertical-align "middle"
                             :margin "0px 20px" :color "#888" :font-size "0.85em"
                             :line-height "1.35"}
                     :title time-title}]
             (map (fn [part] (into [:span {:style {:display "block"}}] part))
                  time-parts)))
     (for [game-player players
           :let [color (get player-colors game-player)]]
       ^{:key game-player}
       [:span
        [:a {:href (player-url player-link-prefix game-player)
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
  ;; Active games (no winner) split by recency: moved in the last week → ACTIVE,
  ;; otherwise → INACTIVE. Games with a winner → COMPLETE. Highlight pill = whose
  ;; turn it is for open games, the winner for completed ones (like the play page).
  (let [week-ago  (- (quot (.now js/Date) 1000) (* 7 24 60 60))
        recent?   (fn [g] (>= (or (:last-move-time g) 0) week-ago))
        completed (filter :winner games)
        open      (remove :winner games)
        active    (filter recent? open)
        inactive  (remove recent? open)
        game-card (fn [{:keys [key invocation round last-move-time]} highlight]
                    [active-game-card {:game-key key
                                       :invocation invocation
                                       :round round
                                       :current-player highlight
                                       :last-move-time last-move-time
                                       :colors (when colors-fn (colors-fn invocation))
                                       :link-prefix link-prefix
                                       :player-link-prefix player-link-prefix
                                       :font-family font-family}])
        section   (fn [label tip items highlight-fn]
                    (when (seq items)
                      [:div {:style {:margin "20px 40px"}}
                       [:h2 (if tip [:span {:title tip} label] label)]
                       (for [{:keys [key] :as g} items]
                         ^{:key key}
                         [game-card g (highlight-fn g)])]))]
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
       [:p {:style {:margin "30px 40px" :color "#888"}} "no games yet"]
       [:div
        (section "ACTIVE"   "A solid color row shows whose turn it is." active   :current-player)
        (section "INACTIVE" "No move in over a week."                   inactive :current-player)
        (section "COMPLETE" nil                                         completed :winner)])]))

;; ── Player stats page ──────────────────────────────────────────────────────

(def ^:private stat-column-hues
  {:playing (rand) :complete (rand) :won (rand) :created (rand)
   :glicko (rand) :elo (rand)})

(defn- col-color
  [hue ratio]
  (let [lightness (js/Math.round (+ 20 (* 50 (or ratio 0))))]
    (str "hsl(" (js/Math.round (* hue 360)) ",55%," lightness "%)")))

(defn- stat-cell
  [label value color]
  [:span
   {:style {:display "inline-flex"
            :flex-direction "row"
            :align-items "baseline"
            :color "#fff"
            :border-radius "15px"
            :background color
            :padding "6px 16px"
            :margin "0px 10px"}}
   [:span {:style {:font-size "1.1em"}} value]
   [:span {:style {:font-size "0.6em" :letter-spacing "2px"
                   :opacity "0.7" :margin-left "8px"}} label]])

(defn- rating-cell
  "The headline number. Glicko-2 is a rating *and* a deviation, and showing the
   deviation is the whole point — 1600 ±40 and 1600 ±300 are different claims."
  [label value deviation color]
  [:span
   {:style {:display "inline-flex"
            :flex-direction "row"
            :align-items "baseline"
            :color "#fff"
            :border-radius "15px"
            :background color
            :padding "6px 16px"
            :margin "0px 10px"}}
   [:span {:style {:font-size "1.3em"}} value]
   (when deviation
     [:span {:style {:font-size "0.7em" :opacity "0.7" :margin-left "6px"}}
      (str "±" deviation)])
   [:span {:style {:font-size "0.6em" :letter-spacing "2px"
                   :opacity "0.7" :margin-left "8px"}} label]])

(defn players-page
  "Complete players/stats page renderer. Props:
   :title              — page title (default \"players\")
   :stats              — seq of {:key :color :active :complete :wins :created
                                 :elo :glicko :rd :rated :provisional}
                         already ordered by the server (see leaderboard/player-stats)
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
        max-created  (col-max :created)
        ;; Ratings all cluster around 1500, so shade them across the range the
        ;; board actually spans rather than as a fraction of the maximum.
        spread   (fn [k]
                   (let [values (keep k stats)]
                     (if (seq values)
                       (let [lo (apply min values)
                             hi (apply max values)]
                         (fn [v] (/ (- v lo) (max 1 (- hi lo)))))
                       (constantly 0))))
        glicko-ratio (spread :glicko)
        elo-ratio    (spread :elo)]
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
        [:p {:style {:margin "10px 20px 25px" :color "#888" :font-size "0.8em"
                     :font-family "monospace" :line-height "1.6em"}}
         "glicko — rating with the uncertainty around it, ranked by what a record establishes rather than by the number itself"
         [:br]
         "elo — the classic: a fixed step per game, no sense of its own confidence"
         [:br]
         "fewer than five finished games and a rating is still a guess, marked new and sorted below the rest"]
        (for [{:keys [key color active complete wins created
                      elo glicko rd rated provisional]} stats]
          ^{:key key}
          [:div {:style {:margin "10px 20px" :padding "10px 0px"
                         :display "flex" :align-items "center"
                         :flex-wrap "wrap" :gap "4px"
                         :opacity (if provisional "0.6" "1")}}
           [:a {:href (player-url player-link-prefix key)
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
           (if glicko
             [rating-cell "glicko" glicko rd
              (col-color (:glicko stat-column-hues) (glicko-ratio glicko))]
             [:span {:style {:color "#666" :font-size "0.8em" :margin "0px 10px"
                             :letter-spacing "2px"}} "unrated"])
           (when elo
             [rating-cell "elo" elo nil
              (col-color (:elo stat-column-hues) (elo-ratio elo))])
           ;; The "won" cell below counts every win; only games that finished
           ;; with a winner against other people move a rating, so say how many
           ;; of them there were rather than leave the two numbers to argue.
           (when (pos? (or rated 0))
             [:span {:style {:color "#888" :font-size "0.6em" :letter-spacing "2px"
                             :margin "0px 4px"}}
              (str rated " rated")])
           (when provisional
             [:span {:style {:color "#aaa" :font-size "0.6em" :letter-spacing "2px"
                             :border "1px solid #555" :border-radius "15px"
                             :padding "4px 10px" :margin "0px 10px"}}
              "new"])
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
                                                   (game-url play-url-prefix pk ""))))
                        :error-handler   (fn [err]
                                           (reset! error (str "Create failed: " (pr-str err))))})))))
           :style (merge btn-style {:padding "12px 36px" :font-size "16px"
                                    :background "#1A2810" :color "#88CC66"
                                    :border-color "#4A4"})}
          "Create Game"]]))))

;; ── Player games page ("my games") ─────────────────────────────────────────

(defn player-active?
  "True when it is `player`'s turn in any of their active games — drives the
   green tab favicon on the games list."
  [player games]
  (boolean
   (some (fn [game] (= player (:current-player game)))
         (get games "active"))))

;; The canonical /<game>/play list. Every game's play page should render this
;; rather than growing its own copy — pass link prefixes and a colour source.

(defn- game-row
  "One game row: the game-key pill, a meta note, then a pill per player.
   `emphasis` is the player to call out (whose turn it is, or the winner);
   when that is the viewer the whole row takes their colour."
  [{:keys [game-key href player-prefix players player-colors emphasis viewer
           note tooltip font-family deletion history-count on-delete on-keep]}]
  (let [viewer-color (get player-colors viewer "#445")
        base (if (and emphasis (= viewer emphasis))
               {:background viewer-color
                :margin "10px 20px"
                :padding "10px 0px"
                :border-radius "10px"}
               {:margin "10px 20px"
                :padding "10px 0px"})]
    [:div
     ;; A marked row gets outlined so it reads as pending rather than gone.
     {:style (if deletion
               (merge base {:border "1px dashed #6A3A3A"
                            :border-radius "10px"
                            :padding "10px 12px"})
               base)}
     [:span
      (when tooltip {:title tooltip})
      [:a
       {:href href
        :style {:color "#fff"
                :border-radius "15px"
                :background viewer-color
                :padding "10px 20px"
                :letter-spacing "5px"
                :font-family (or font-family "monospace")
                :font-size "1.3em"
                :text-decoration "none"}}
       game-key]]
     (when note
       [:span {:style {:margin "0px 20px"}} note])
     (for [game-player players]
       (let [player-color (get player-colors game-player "#445")]
         ^{:key game-player}
         [:span
          [:a
           {:href (player-url player-prefix game-player)
            :style (if (= game-player emphasis)
                     {:color "#fff"
                      :border-radius "20px"
                      :background player-color
                      :margin "0px 10px"
                      :padding "7px 20px"
                      :text-decoration "none"}
                     {:padding "5px 10px"
                      :margin "0px 10px"
                      :border-style "solid"
                      :border-width "2px"
                      :border-color player-color
                      :border-radius "5px"
                      :color player-color
                      :text-decoration "none"})}
           game-player]]))
     [delete-control {:game-key      game-key
                      :history-count history-count
                      :on-delete     on-delete}]
     (when deletion
       [deletion-notice {:deletion deletion :on-keep on-keep}])]))

(defn games-section
  "A titled list of game rows. Props:
   :title       — section heading (e.g. \"ACTIVE\")
   :tooltip     — optional heading tooltip
   :games       — seq of player-game records
   :viewer      — the logged-in player
   :play-prefix — URL prefix for the game link
   :player-prefix — URL prefix for player links
   :colors-fn   — (fn [record] → {player → css-color}); falls back to :player-colors
   :emphasis-fn — (fn [record] → player to call out)
   :note-fn     — (fn [record] → string shown next to the game name)
   :tooltip-fn  — (fn [record] → hover text for the game name)"
  [{:keys [title tooltip games viewer play-prefix player-prefix
           colors-fn emphasis-fn note-fn tooltip-fn font-family
           on-delete on-keep]}]
  (when (seq games)
    [:div {:style {:margin "20px 40px"}}
     [:h2 (if tooltip [:span {:title tooltip} title] title)]
     (for [{:keys [game players] :as record} games]
       ^{:key game}
       [game-row
        {:game-key      game
         :href          (game-url play-prefix game "")
         :player-prefix player-prefix
         :players       players
         :player-colors (if colors-fn (colors-fn record) (:player-colors record))
         :emphasis      (when emphasis-fn (emphasis-fn record))
         :viewer        viewer
         :note          (when note-fn (note-fn record))
         :tooltip       (when tooltip-fn (tooltip-fn record))
         :font-family   font-family
         :deletion      (:deletion record)
         :history-count (:history-count record)
         :on-delete     (when on-delete #(on-delete record))
         :on-keep       (when on-keep #(on-keep record))}])]))

(def ^:private picker-button-style
  {:background "#222" :color "#bbb" :border "1px solid #555"
   :border-radius "6px" :padding "7px 14px" :font-size "12px"
   :font-family "monospace" :letter-spacing "1px" :cursor "pointer"})

(defn- colour-picker
  "The panel the banner opens: a real colour input, the hex it is on, a reroll
   for the old behaviour, and a way out.

   There is no cancel. Every change has already been applied, so dismissing
   keeps whatever was chosen — which is the point, the colour used to be
   snatched away again on the next stray click."
  [{:keys [color on-color random-color on-close]}]
  (let [hex (base/color->hex color)]
    [:div
     {:on-click (fn [event] (.stopPropagation event))
      :style {:position "absolute" :top "100%" :left "40px" :z-index 200
              :margin-top "10px" :padding "14px 18px"
              :background "#161616" :border "1px solid #444"
              :border-radius "10px"
              :letter-spacing "normal" :font-family "monospace"
              :display "flex" :align-items "center" :gap "12px"
              :box-shadow "0 6px 24px rgba(0,0,0,0.5)"}}
     [:input
      {:type "color"
       :value hex
       :title "pick a colour"
       :on-change (fn [event] (on-color (-> event .-target .-value)))
       :style {:width "48px" :height "36px" :padding "0" :cursor "pointer"
               :background "transparent" :border "1px solid #555"
               :border-radius "6px"}}]
     [:code {:style {:color "#999" :font-size "12px"}} hex]
     (when random-color
       [:button {:on-click (fn [_] (on-color (random-color)))
                 :style picker-button-style}
        "random"])
     [:button {:on-click (fn [_] (on-close))
               :style picker-button-style}
      "done"]]))

(defn player-games-banner
  "The name banner over a games list.

   The name is a link and behaves like one. Clicking anywhere else on the
   banner opens the colour picker. It used to reroll a random colour on any
   click at all — including clicks meant for the name, which navigated and
   changed the colour at the same time, so the colour never settled anywhere."
  [_props]
  (let [open? (r/atom false)
        dismiss (fn dismiss []
                  (reset! open? false)
                  (.removeEventListener js/document "click" dismiss))]
    (fn [{:keys [player color label home-path font-family on-color random-color]}]
      [:div
       {:style {:color "#fff"
                :border-radius "50px"
                :cursor (when on-color "pointer")
                :background (or color "#445")
                :letter-spacing "8px"
                :font-family (or font-family "monospace")
                :margin "20px 0px"
                :padding "25px 60px"
                :position "relative"}
        :title (when on-color "click for a colour")
        :on-click (fn [event]
                    (when on-color
                      ;; stop here, or the listener below closes it again on
                      ;; the very click that opened it
                      (.stopPropagation event)
                      (if @open?
                        (dismiss)
                        (do (reset! open? true)
                            (.addEventListener js/document "click" dismiss)))))}
       [:h1 [:a {:style {:color "#fff" :text-decoration "none"}
                 :href (or home-path "/")
                 ;; the name is a link first — navigate without the picker
                 ;; opening behind it
                 :on-click (fn [event] (.stopPropagation event))}
             player]]
       (when label
         [:div {:style {:font-size "1.3em" :letter-spacing "5px" :margin "10px 0px"}}
          label])
       (when @open?
         [colour-picker {:color color
                         :on-color on-color
                         :random-color random-color
                         :on-close dismiss}])])))

(defn player-games-page
  "The shared 'my games' page: banner, then OPEN / ACTIVE / COMPLETE sections.
   Props:
   :player          — logged-in player
   :games           — {\"open\" [...] \"active\" [...] \"complete\" [...]}
   :color           — banner colour
   :label           — banner sub-label (default \"games\")
   :home-path       — banner link target
   :play-prefix     — e.g. \"/journey/play/\"
   :create-prefix   — where open games link (e.g. \"/organism/create/\")
   :player-prefix   — e.g. \"/player/\"
   :colors-fn       — (fn [record] → {player → css-color}) for active/complete
   :open-colors-fn  — (fn [invocation] → [colors]) for open game slots
   :note-fn         — (fn [record] → string) shown beside the game name
   :tooltip-fn      — (fn [record] → hover text)
   :empty-content   — hiccup shown when there are no games at all
   :on-color        — (fn [css-colour]) applied as the picker changes; supply it
                      to give the banner a colour picker at all
   :random-color    — (fn [] css-colour) behind the picker's \"random\" button
   :font-family"
  [{:keys [player games color label home-path play-prefix create-prefix player-prefix
           colors-fn open-colors-fn note-fn tooltip-fn empty-content
           on-color random-color font-family deletable? joinable?]
    :or   {label "games" player-prefix "/player/"}}]
  (let [open      (get games "open")
        active    (get games "active")
        completed (get games "complete")
        ;; Deletion is offered on open lobbies and live games only. Completed
        ;; games stay: the ratings and the player stats replay them, so
        ;; dropping one quietly rewrites history. `deletable?` is opt-in
        ;; because it needs the <play-prefix>/:play/delete routes wired.
        game-key  (fn [record] (or (:game record) (:key record)))
        on-delete (when deletable?
                    (fn [record] (request-delete! play-prefix (game-key record))))
        on-keep   (when deletable?
                    (fn [record] (request-keep! play-prefix (game-key record))))
        ;; Taking a seat needs the <play-prefix>/:play/join route, so it is
        ;; opt-in the same way deletion is.
        on-join   (when joinable?
                    (fn [record index] (request-join! play-prefix (game-key record) index)))
        section   (fn [title tooltip rows emphasis-fn extra]
                    [games-section
                     (merge
                      {:title         title
                       :tooltip       tooltip
                       :games         rows
                       :viewer        player
                       :play-prefix   play-prefix
                       :player-prefix player-prefix
                       :colors-fn     colors-fn
                       :emphasis-fn   emphasis-fn
                       :note-fn       note-fn
                       :tooltip-fn    tooltip-fn
                       :font-family   font-family}
                      extra)])]
    [:div {:style {:padding "20px" :color "#eee"}}
     [player-games-banner {:player player :color color :label label
                           :home-path home-path :font-family font-family
                           :on-color on-color :random-color random-color}]
     [open-games-section {:games open
                          :link-prefix (or create-prefix play-prefix)
                          :current-player player
                          :colors-fn open-colors-fn
                          :font-family font-family
                          :on-delete on-delete
                          :on-join on-join}]
     (section "ACTIVE"
              (str "A solid color row indicates it is your turn in that game.\n"
                   "The icon on the tab for this page will turn green when it is your turn.")
              active :current-player
              {:on-delete on-delete :on-keep on-keep})
     (section "COMPLETE" nil (reverse completed) :winner nil)
     (when (and (empty? open) (empty? active) (empty? completed))
       (or empty-content
           [:p {:style {:margin "30px 40px" :color "#888"}} "no games yet"]))]))
