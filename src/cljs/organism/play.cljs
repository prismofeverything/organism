(ns organism.play
  (:require
   [clojure.string :as string]
   [cljs.pprint :refer (pprint)]
   [cljs.reader :as reader]
   [goog.events :as events]
   [goog.history.EventType :as HistoryEventType]
   [reitit.core :as reitit]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.base :as base]
   [organism.game :as game]
   [organism.choice :as choice]
   [organism.board :as board]
   [organism.dom :as dom]
   [organism.ajax :as ajax]
   [organism.components :as components]
   [organism.websockets :as ws])
  (:import goog.History))

(def orange "rgb(225, 195, 61)")

(def ^:private word-pool
  ["amber" "arrow" "blade" "bloom" "briar" "brook" "cedar" "chain" "cliff"
   "coral" "crane" "crown" "dawn" "delta" "drift" "dusk" "echo" "ember"
   "fable" "fern" "flame" "flint" "frost" "gale" "ghost" "gleam" "grove"
   "haven" "hawk" "haze" "helm" "heron" "husk" "ivory" "jade" "knot"
   "larch" "lark" "leaf" "light" "lunar" "marsh" "mist" "moss" "nexus"
   "oak" "onyx" "orbit" "pearl" "pike" "plume" "pulse" "quartz" "raven"
   "reef" "ridge" "river" "root" "rune" "sage" "shard" "shell" "shore"
   "silk" "slate" "solar" "spark" "spire" "star" "steam" "stone" "storm"
   "swift" "thorn" "tide" "torch" "trail" "vale" "vault" "vine" "void"
   "wander" "wave" "weald" "wisp" "wolf" "wren" "zenith"])

(defn- generate-game-key []
  (let [pick #(nth word-pool (rand-int (count word-pool)))]
    (str (pick) "-" (pick) "-" (pick))))

(def font-choice
  "BlinkMacSystemFont,-apple-system,\"Segoe UI\",Roboto,Oxygen,Ubuntu,Cantarell,\"Fira Sans\",\"Droid Sans\",\"Helvetica Neue\",Helvetica,Arial,sans-serif")

(def possible-mutations
  {:COMMUNE "elements are are considered fed and mobile for movement if they are adjacent to at least two other fed elements"
   :PERSIST "elements are not lost to integrity unless the player has no living organisms remaining"
   :RAIN "the top two sides rain an increasing amount of neutral elements down upon your organisms"})


   ;; :BOOST "elements are mobile if they are adjacent to at least one other fed element"
   ;; :EXTRACT "the capturing element takes the food from captured element"
   ;; :ABSORB "any element lost to integrity that captured another element is added to that organism in place of the captured element"
   ;; :SKIP "start with 5 elements instead of 3"

(defn display-mutation
  [mutation-key mutation-description]
  (str
   (name mutation-key)
   " ➞ "
   mutation-description))

(defonce session (r/atom {:page :home}))
(defonce chat (r/atom []))

(defonce history-advance
  (r/atom nil))

(defonce player-order
  (r/atom
   (vec
    (take
     (count board/default-player-order)
     (repeat "")))))

(defonce player-captures-order
  (r/atom
   (vec
    (repeat
     (count board/default-player-order)
     board/default-player-captures))))

(defonce board-invocation
  (r/atom (board/empty-invocation
           (if (and (exists? js/playerKey) (not (empty? js/playerKey)))
             js/playerKey
             "orb"))))

(def empty-game-state
  {:game {}
   :created false
   :player js/playerKey
   :history []
   :cursor nil
   :board {}
   :turn :open
   :choices []})

(defonce game-state
  (r/atom empty-game-state))

(defonce clear-state
  (r/atom (game/initial-state board/default-player-order)))

(def empty-introduction
  {:chosen-space nil
   :chosen-element nil
   :progress {}})

(defonce introduction
  (r/atom empty-introduction))

;; Hover state for the choose-action-type phase: which element-type is hovered,
;; plus the [x y] of the hovered element so we can position the popup.
(defonce action-hover (r/atom nil))

;; Hover state for the choose-action phase: which "from" space is hovered
;; so we can show its destination spaces.
(defonce from-hover (r/atom nil))

;; Separate hover for an individual destination (so we can brighten it)
(defonce dest-hover (r/atom nil))

;; Pending timeout id for delayed clearing of from-hover on mouseleave
(defonce from-hover-timeout (atom nil))

(defn- cancel-from-hover-clear! []
  (when @from-hover-timeout
    (js/clearTimeout @from-hover-timeout)
    (reset! from-hover-timeout nil)))

(defn- schedule-from-hover-clear! []
  (cancel-from-hover-clear!)
  (reset! from-hover-timeout
          (js/setTimeout
           (fn []
             (reset! from-hover nil)
             (reset! dest-hover nil)
             (reset! from-hover-timeout nil))
           150)))

;; When the user clicks an element at :choose-action and multiple sub-actions
;; are possible (e.g. both grow and circulate), this holds {:space :options}
;; to render the popup.
(defonce action-popup (r/atom nil))

(defonce food-source
  (r/atom {}))

(defonce player-games
  (r/atom {}))

(defonce player-preferences
  (r/atom {}))

;; Use shared atom from organism.components
(def create-game-key components/create-game-key)

(defonce observe-games
  (r/atom []))

(defonce player-stats
  (r/atom []))

(declare update-messages!)
(declare apply-invocation!)
(declare connect-create-ws!)

(def max-players 10)

(def highlight-element-stroke {:ratio 0.04 :color "#ccc"})

(defn choose-food-source!
  [space]
  (swap! food-source update space inc))

(defn introduction-complete?
  [{:keys [progress]}]
  (and
   (= 3 (count (set (keys progress))))
   (= 3 (count (set (vals progress))))))

(defn assoc-prop
  [el key value]
  (if (> 1 (count el))
    (if (map? (nth el 1))
      (assoc-in el [1 key] value)
      (vec
       (concat
        [(first el) {key value}]
        (rest el))))
    (vec
     (conj
      (conj
       (rest el)
       {key value})
      (first el)))))

(defn send-state!
  [state complete]
  (ws/send-transit-message!
   {:type "game-state"
    :game state
    :complete complete}))

(defn send-reset!
  [state]
  (ws/send-transit-message!
   {:type "history"
    :game state}))

(defn send-clear!
  []
  (ws/send-transit-message!
   {:type "clear"}))

(defn send-choice!
  [choices match complete]
  (let [choice (get-in choices [match :state])]
    (if choice
      (send-state! choice complete)
      (println "NO CHOICE MATCHING" match))))

(defn send-introduction!
  [choices {:keys [progress] :as intro}]
  ;; if (introduction-complete? intro)
  (send-choice!
   choices
   {:spaces progress
    :organism 0}
   true))

(defn send-create!
  [invocation]
  (reset! board-invocation invocation)
  (apply-invocation! invocation)
  (components/send-create! invocation))

(def send-open-game! components/send-open-game!)

(defn initialize-chat
  [chat message]
  (println "initializing chat" (:chat message))
  (:chat message))

(defn initialize-game
  [game-state {:keys [game invocation player history board witness] :as message}]
  (let [{:keys [ring-count player-count players colors mutations]} invocation
        board (board/generate-board
               colors
               players
               (take ring-count board/total-rings)
               mutations)
        [game turn choices] (choice/find-next-choices game)
        cursor (if (< witness (count history)) witness)]
    (println "initializing game" game)
    (println "initializing board" board)
    (println "turn" turn)
    (println "choices" (count choices))
    {:game game
     :invocation invocation
     :player player
     :history history
     :board board
     :turn turn
     :cursor cursor
     :choices choices}))

(defn update-chat
  [chat message]
  (conj chat message))

(defn update-game
  [game-state message]
  (let [state (:game message)
        current-game (assoc (:game game-state) :state state)
        [final-game turn choices] (choice/find-next-choices current-game)
        game-state (assoc game-state :game final-game)]
    (-> game-state
        (update :history conj (:state final-game))
        (assoc :turn turn)
        (assoc :choices choices))))

(def number->word
  {3 "THREE"
   4 "FOUR"
   5 "FIVE"
   6 "SIX"
   7 "SEVEN"
   8 "EIGHT"})

(defn round-banner
  [color round]
  [:div
   {:style
    {:color "#fff"
     :border-radius "50px"
     :cursor "pointer"
     :background color
     :letter-spacing "8px"
     :font-family font-choice
     :margin "20px 0px"
     :padding "25px 60px"}}
   [:h1
    [:a
     {:style
      {:color "#fff"}
      :href (str js/playerPath "/" js/playerKey)}
     js/playKey]]
   [:h2 "round " (inc round)]])

(defn boundary-inc
  [total n]
  (cond
    (nil? n) nil
    (= n (dec total)) nil
    :else (inc n)))

(defn boundary-dec
  [total n]
  (cond
    (nil? n) (dec total)
    (zero? n) 0
    :else (dec n)))

(defn history-beginning-control
  []
  [:polygon
   {:points "0,5 10,5 10,25 50,0 50,50 10,25 10,45 0,45"
    :style {:fill "hsl(100, 20%, 30%)"}
    :on-click
    (fn [event]
      (swap! game-state assoc :cursor 0))}])

(defn history-back-control
  [cursor total]
  [:polygon
   {:points "70,25 100,5 100,45"
    :style {:fill "hsl(100, 20%, 30%)"}
    :on-click
    (fn [event]
      (swap! game-state update :cursor (partial boundary-dec total)))}])

(def history-interval 300)

(defn clear-history-advance!
  [advance]
  (when advance
    (.clearInterval
     js/window
     advance)
    (reset! history-advance nil)))

(defn set-history-advance!
  [total cursor]
  (if-let [advance @history-advance]
    (clear-history-advance! advance))
  (reset!
   history-advance
   (.setInterval
    js/window
    (fn []
      (let [cursor (:cursor @game-state)]
        (if (>= cursor (dec total))
          (do
            (clear-history-advance! @history-advance)
            (swap! game-state assoc :cursor nil))
          (swap! game-state update :cursor (partial boundary-inc total)))))
    history-interval)))

(defn history-status-display
  [cursor total]
  [:text
   {:x (if cursor "110" "160")
    :y "35"
    :width "80"
    :font-size "1.8em"
    :style
    {:fill "#eee"}
    :on-click
    (fn [event]
      (if-let [advance @history-advance]
        (clear-history-advance! advance)
        (do
          (when (nil? cursor)
            (swap! game-state assoc :cursor 0))
          (set-history-advance! total cursor))))}
   (if cursor
     (str (inc cursor) " / " total)
     (str total))])

(defn history-forward-control
  [total]
  [:polygon
   {:points "250,5 280,25 250,45"
    :style {:fill "hsl(100, 20%, 30%)"}
    :on-click
    (fn [event]
      (swap! game-state update :cursor (partial boundary-inc total)))}])

(defn history-end-control
  [total]
  [:polygon
   {:points "350,5 340,5 340,25 300,0 300,50 340,25 340,45 350,45"
    :style {:fill "hsl(100, 20%, 30%)"}
    :on-click
    (fn [event]
      (clear-history-advance! @history-advance)
      (swap! game-state assoc :cursor nil))}])

(defn history-controls
  [history cursor]
  (let [total (count history)]
    [:div
      {:style
       {:margin "0px 0px 0px 0px"}}
     [:h3 "history"]
     [:svg
      {:width 300
       :height 50
       :style
       {:margin "10px 0px 0px 30px"}}
      [:g
       {:transform "scale(0.6)"}
       [history-beginning-control]
       [history-back-control cursor total]
       [history-status-display cursor total]
       [history-forward-control total]
       [history-end-control total]]]]))

(defn mutation-display
  [color mutation-key]
  ^{:key mutation-key}
  [:h4
   {:style
    {:color color
     :margin-left "20px"}}
   (display-mutation
    mutation-key
    (get possible-mutations mutation-key))])

(defn mutations-display
  [mutations color]
  (let [chosen (map first (filter (fn [[key choice]] choice) mutations))]
    (if-not (empty? chosen)
      [:div
       [:h3 "mutations"]
       [:div
        (map (partial mutation-display color) chosen)]])))

(defn scoreboard
  [turn-order organism-victory colors player-captures mutations state]
  (let [player (get-in state [:player-turn :player])
        player-colors (into {} (map vector turn-order colors))]
    [:div
     [:h3 "score"]
     [:ul
      (let [player-captures (if player-captures player-captures (repeat board/default-player-captures))]
        (for [[player captures color] (map vector turn-order player-captures colors)]
          ^{:key (str player color)}
          [:li
           {:style {:color color}}
           player " - "
           (count (get-in state [:captures player])) " / "
           (if (get mutations :RAIN)
             (let [rain-player (last turn-order)]
               (if (= player rain-player)
                 captures
                 [:span {:style {:font-size "1.5em"}} "∞"]))
             captures)]))]

     [:h4
      {:style
       {:font-size "1.0em"
        :margin "12px 0px 0px 0px"}}
      [:span
       {:style
        {:color (get player-colors player)}}
       (get number->word organism-victory organism-victory)]
      " organisms for victory"]
     [mutations-display mutations (get player-colors player)]]))

(def chat-window 15)

(defn chat-list
  [player-colors chat]
  [:ul
   (let [total (count chat)
         visible (drop (- total chat-window) chat)]
     (for [[i message] (map-indexed vector visible)]
       (let [player (:player message)
             color (get player-colors player "black")]
         ^{:key i}
         [:li
          {:style {:color color}}
          player ": " (:message message)])))])

(defn chat-input
  []
  (if js/playerKey
    (let [value (r/atom nil)]
      (fn []
        [:input.form-control
         {:type :text
          :placeholder "respond"
          :value @value
          :on-change #(reset! value (-> % .-target .-value))
          :on-key-down
          #(when (= (.-keyCode %) 13)
             (ws/send-transit-message!
              {:type "chat"
               :player js/playerKey
               :message @value})
             (reset! value nil))}]))
    []))

(defn description-panel
  [player-color description]
  [:div
   [:h4
    {:style
     {:color (board/brighten player-color 0.3)}}
    description]])

(defn help-panel
  [color]
  [:div
   [:h3 "help"]
   [:div
    {:style
     {:color color
      :font-size "1.2em"
      :letter-spacing "3px"
      :margin "0px 0px 0px 30px"}}
    [:a
     {:href "/img/organism-player-diagram.png"
      :target "_blank"
      :style
      {:color "hsl(250, 30%, 70%)"}}
     "player aid"]
    " | "
    [:a
     {:href "/img/organism-rulebook.pdf"
      :target "_blank"
      :style
      {:color "hsl(130, 30%, 70%)"}}
     "rules"]]])

(defn chat-panel
  [description
   turn-order
   organism-victory
   colors
   player-colors
   player-captures
   mutations
   state
   history
   cursor
   chat]
  (let [player-color (get player-colors (-> state :player-turn :player) (first colors))]
    [:div
     {:style
      {:margin "20px"}}
     [round-banner
      player-color
      (:round state)]
     [:div
      {:style
       {:margin "20px 50px"}}
      [description-panel player-color description]
      [scoreboard turn-order organism-victory colors player-captures mutations state]
      [history-controls history cursor]
      [help-panel player-color]
      [:h3 "discussion"]
      [chat-list player-colors chat]
      [:br]
      [chat-input]]]))

(defn highlight-circle
  [x y radius color on-click]
  (let [highlight-color (board/brighten color 0.3)]
    [:circle
     {:cx x :cy y
      :r (* radius 1.1)
      :stroke highlight-color
      :stroke-width (* 0.19 radius)
      :fill-opacity 0.1
      :fill "white"
      :on-click on-click}]))

(defn focus-circle
  [x y radius color on-click]
  [:circle
   {:cx x :cy y
    :r radius
    :stroke (board/brighten color 0.2)
    :stroke-width (* 0.21 radius)
    :fill (board/brighten color 0.1)
    :on-click on-click}])

(defn highlight-element
  [type food x y radius color stroke on-click]
  (let [g (board/render-element
           (board/brighten color 0.1)
           "white"
           stroke
           [x y]
           radius
           {:type type :food food})]
    (assoc-prop g :on-click on-click)))

(defn render-element
  [type x y radius color food-color on-click]
  (let [g (board/render-element
           color
           food-color
           [x y]
           radius
           {:type type :food 1})]
    (assoc-prop g :on-click on-click)))

(def highlight-factor 0.93)
(def element-highlight-factor 1.0)

(defn create-highlights
  [game board colors turn choices]
  (let [players (:players game)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        highlights
        (base/map-cat
         (fn [[[player {:keys [starting-spaces]}] color]]
           (map
            (fn [space]
              (let [[x y] (get locations space)]
                (highlight-circle
                 x y radius color
                 (fn [event]))))
            starting-spaces))
         (map vector players colors))]
    (into [] (concat [:g] highlights))))

(defn introduce-highlights
  "Click a starting space → popup with available element types appears next
   to it. Pick a type → element placed there. After two are placed, the
   third space gets the remaining type automatically. No left-panel needed."
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        food-color (-> board :colors first last)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        element-radius (* (:radius board) 1)
        popup-radius (* (:radius board) 0.45)
        starting-spaces (get-in game [:players player :starting-spaces])
        {:keys [chosen-space progress]} @introduction
        all-types [:eat :grow :move]
        used-types (set (vals progress))
        available-types (vec (remove used-types all-types))

        ;; Place element at space, auto-completing if only one type remains
        place-at!
        (fn [space type]
          (let [new-progress (assoc progress space type)
                ;; If 2 placed and 1 starting space + 1 type remain, auto-place
                remaining-spaces (remove (set (keys new-progress)) starting-spaces)
                remaining-types (remove (set (vals new-progress)) all-types)
                final-progress
                (if (and (= 1 (count remaining-spaces))
                         (= 1 (count remaining-types)))
                  (assoc new-progress (first remaining-spaces) (first remaining-types))
                  new-progress)
                new-intro (-> @introduction
                              (assoc :chosen-space nil)
                              (assoc :chosen-element nil)
                              (assoc :progress final-progress))]
            (reset! introduction new-intro)
            (when (= (count final-progress) (count starting-spaces))
              (send-introduction! choices new-intro))))

        ;; Render unchosen starting spaces as click targets
        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-circle
              x y radius color
              (fn [_event]
                (swap! introduction assoc :chosen-space space)))))
         (remove
          (set (conj (keys progress) chosen-space))
          starting-spaces))

        ;; Chosen space — show its highlight + a popup with available types
        chosen-popup
        (when chosen-space
          (let [[x y] (get locations chosen-space)
                n (count available-types)
                ;; Layout the popup in a horizontal row above the space
                spread (* popup-radius 2.4)
                start-x (- x (* spread (/ (dec n) 2.0)))
                offset-y (- y (* (:radius board) 1.6))]
            ^{:key (str "popup-" chosen-space)}
            [:g {:key (str "popup-" chosen-space)}
             ;; Click-elsewhere catcher: highlight on the chosen space cancels
             (focus-circle
              x y radius color
              (fn [_e] (swap! introduction dissoc :chosen-space)))
             ;; The popup options
             (for [[i type] (map-indexed vector available-types)
                   :let [px (+ start-x (* i spread))]]
               ^{:key (str "popup-" chosen-space "-" type)}
               [:g {:on-click (fn [e]
                                (.stopPropagation e)
                                (place-at! chosen-space type))}
                ;; Background circle behind the icon
                [:circle {:cx px :cy offset-y :r popup-radius
                          :fill (board/brighten color 0.05)
                          :stroke (board/brighten color 0.4)
                          :stroke-width (* 0.15 popup-radius)
                          :style {:cursor "pointer"}}]
                (render-element
                 type px offset-y (* popup-radius 0.8) color food-color
                 (fn [e]
                   (.stopPropagation e)
                   (place-at! chosen-space type)))])]))

        ;; Render elements placed so far — clickable to remove (revert)
        elements
        (map
         (fn [[space type]]
           (let [[x y] (get locations space)]
             ^{:key space}
             (render-element
              type x y element-radius color food-color
              (fn [_event]
                (swap! introduction
                       (fn [intro]
                         (-> intro
                             (assoc :chosen-space nil)
                             (update :progress dissoc space))))))))
         progress)]
    ^{:key "highlights"}
    [:g (cond-> (concat highlights elements)
          chosen-popup (concat [chosen-popup]))]))

(defn chosen-organism-highlights
  [game board on-click turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) element-highlight-factor)
        elements (game/current-organism-elements game)
        element-stroke highlight-element-stroke
        source @food-source

        highlights
        (mapv
         (fn [{:keys [space organism type food] :as element}]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-element
              type (- food (get source space 0))
              x y radius
              color element-stroke
              (partial on-click element))))
         elements)]
    highlights))

(defn space-highlights
  [game board turn choices spaces on-click]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)

        space-highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 next-state (get-in choices [space :state])]
             ^{:key space}
             (highlight-circle
              x y radius color
              (partial on-click space))))
         spaces)]
    space-highlights))

(defn choose-organism-highlights
  [game board turn choices]
  (let [game (game/find-organisms game) ;; find organisms here to avoid finding for each introduction
        player (game/current-player game)
        organisms (game/player-organisms game player)
        available (keys choices)
        elements (base/map-cat organisms available)
        spaces (map :space elements)
        space-organisms
        (into
         {}
         (map
          (juxt :space :organism)
          elements))

        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (let [organism (get space-organisms space)]
             (send-choice! choices organism true))))]
    
    highlights))

(defn choose-action-type-highlights
  "During :choose-action-type, render a highlight circle around each
   current-organism element. Hovering brightens all elements of that type
   and shows a popup with the type name and number of actions you'll get."
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        ;; The game at :choose-action-type doesn't yet have the current
        ;; organism set in organism-turns. Find the player's first organism
        ;; (find-state already chose it; here we just look it up).
        game-with-orgs (game/find-organisms game)
        organisms (game/player-organisms game-with-orgs player)
        first-org-id (first (keys organisms))
        elements (or (not-empty (game/current-organism-elements game))
                     (get organisms first-org-id)
                     [])
        by-type (group-by :type elements)
        hover @action-hover
        hover-type (:type hover)

        ;; A click-target halo per element. Hovered type → brighter halo.
        element-halos
        (mapv
         (fn [{:keys [space type] :as _element}]
           (let [[x y] (get locations space)
                 hovered? (= hover-type type)
                 stroke-c (if hovered?
                            (board/brighten color 0.6)
                            (board/brighten color 0.3))
                 fill-op (if hovered? 0.25 0.10)]
             ^{:key (str "halo-" space)}
             [:circle
              {:cx x :cy y
               :r (* radius 1.15)
               :stroke stroke-c
               :stroke-width (if hovered? (* 0.28 radius) (* 0.19 radius))
               :fill "white"
               :fill-opacity fill-op
               :style {:cursor "pointer"}
               :on-mouse-enter (fn [_e]
                                 (reset! action-hover {:type type :x x :y y}))
               :on-mouse-leave (fn [_e] (reset! action-hover nil))
               :on-click (fn [_e]
                           (reset! action-hover nil)
                           (send-choice! choices type true))}]))
         elements)

        ;; Popup showing "EAT: 3 actions" above the hovered element
        popup
        (when hover
          (let [{:keys [type x y]} hover
                n (count (get by-type type []))
                label (str (clojure.string/upper-case (name type))
                           ": " n " action" (when (not= n 1) "s"))
                box-w 180
                box-h 38
                px (- x (/ box-w 2))
                py (- y (* (:radius board) 2.4))]
            ^{:key "action-hover-popup"}
            [:g {:pointer-events "none"}
             [:rect {:x px :y py :width box-w :height box-h :rx 6
                     :fill "#0A0E1C"
                     :stroke (board/brighten color 0.4)
                     :stroke-width 2
                     :opacity 0.95}]
             [:text {:x (+ px (/ box-w 2)) :y (+ py 25)
                     :text-anchor "middle"
                     :fill "#fff"
                     :font-family "monospace"
                     :font-size 16
                     :letter-spacing "1px"}
              label]]))]
    (cond-> element-halos
      popup (conj popup))))

(defn- compute-from-spaces-and-options
  "Given the post-:choose-action game wrap, return a map
   {space → [{:label ... :destinations [...] :next-state ...
              :sub-options [...]} ...]}
   For grow, the top-level option for each grower has :sub-options listing
   the available element-types as a nested popup."
  [post-action-game-wrap label-prefix]
  (try
    (let [[phase from-choices] (choice/find-state post-action-game-wrap)]
      (cond
        ;; Move/eat/circulate: from-choices is keyed by space directly
        (#{:move-from :eat-to :circulate-from} phase)
        (into {}
              (map
               (fn [space]
                 (let [from-state (get-in from-choices [space :state])
                       from-wrap (assoc post-action-game-wrap :state from-state)
                       dests (try
                               (let [[_ to-choices] (choice/find-state from-wrap)]
                                 (filter vector? (keys to-choices)))
                               (catch :default _ nil))]
                   [space [{:label label-prefix
                            :destinations (or dests [])
                            :next-state from-state}]]))
               (filter vector? (keys from-choices))))

        ;; Grow: top-level option is "GROW", nested sub-options are element types
        (= phase :grow-element)
        (let [type-keys (keys from-choices)
              ;; For each grower space, collect its sub-options (one per type)
              ;; sub-options-by-space: {grower-space [{:label :destinations :next-state} ...]}
              sub-by-space
              (reduce
               (fn [acc type-key]
                 (try
                   (let [type-state (get-in from-choices [type-key :state])
                         type-wrap (assoc post-action-game-wrap :state type-state)
                         [grow-from-phase grow-from-choices] (choice/find-state type-wrap)
                         sub-label (clojure.string/upper-case (name type-key))]
                     (if (= grow-from-phase :grow-from)
                       (reduce
                        (fn [acc contribution]
                          (let [contrib-state (get-in grow-from-choices [contribution :state])
                                contrib-wrap (assoc type-wrap :state contrib-state)
                                [_ to-choices] (choice/find-state contrib-wrap)
                                dests (filter vector? (keys to-choices))
                                sub-opt {:label sub-label
                                         :destinations (or dests [])
                                         :next-state contrib-state}]
                            (reduce
                             (fn [acc space]
                               (update acc space (fnil conj []) sub-opt))
                             acc
                             (keys contribution))))
                        acc
                        (keys grow-from-choices))
                       acc))
                   (catch :default _ acc)))
               {} type-keys)]
          ;; Wrap each grower's sub-options in a single top-level GROW option
          (into {}
                (map
                 (fn [[space subs]]
                   [space [{:label label-prefix
                            :destinations (->> subs
                                                (mapcat :destinations)
                                                distinct
                                                vec)
                            :sub-options subs}]])
                 sub-by-space)))

        :else {}))
    (catch :default _ {})))

(defn choose-action-highlights
  "During :choose-action, the chosen action type drives one set of clickable
   ELEMENT halos (move/eat/grow targets), and a separate set of FOOD halos
   (small markers on every element with food → click to circulate).

   - Click element halo → execute the action (popup only for grow's
     element-type sub-choice)
   - Click food halo → execute circulate from that element"
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        organism-turns (get-in game [:state :player-turn :organism-turns])
        action-type (get-in organism-turns [(dec (count organism-turns)) :choice])
        ;; Action type's flow (move-from / eat-to / grow-element)
        action-game-state (get-in choices [action-type :state])
        action-game (when action-game-state (assoc game :state action-game-state))
        action-from-map (when action-game
                          (compute-from-spaces-and-options
                           action-game (clojure.string/upper-case (name action-type))))
        ;; Circulate flow (any element with food)
        circ-game-state (get-in choices [:circulate :state])
        circ-game (when circ-game-state (assoc game :state circ-game-state))
        circ-from-map (when circ-game
                        (compute-from-spaces-and-options circ-game "CIRCULATE"))

        hover @from-hover  ;; {:space [...] :kind :element|:food}
        popup @action-popup

        ;; ── Element halo click ──────────────────────────────────────────
        opt->button
        (fn opt->button [space opt]
          {:label (:label opt)
           :on-click
           (fn []
             (cond
               (and (seq (:sub-options opt))
                    (= 1 (count (:sub-options opt))))
               (do (reset! action-popup nil)
                   (send-state! (:next-state (first (:sub-options opt))) true))
               (seq (:sub-options opt))
               (reset! action-popup
                       {:space space
                        :options (mapv (partial opt->button space) (:sub-options opt))})
               :else
               (do (reset! action-popup nil)
                   (send-state! (:next-state opt) true))))})

        click-element
        (fn [space]
          (reset! from-hover nil)
          (let [opts (get action-from-map space)]
            (cond
              (empty? opts) nil
              ;; Single top-level option with no sub-options → execute directly
              (and (= 1 (count opts))
                   (empty? (:sub-options (first opts))))
              (send-state! (:next-state (first opts)) true)
              ;; Single top-level option with exactly one sub-option → execute it
              (and (= 1 (count opts))
                   (= 1 (count (:sub-options (first opts)))))
              (send-state! (:next-state (first (:sub-options (first opts)))) true)
              ;; Single top-level option with multiple sub-options
              ;; (e.g. GROW with element-type choices) → skip top level and show sub-options
              (= 1 (count opts))
              (reset! action-popup
                      {:space space
                       :options (mapv (partial opt->button space)
                                      (:sub-options (first opts)))})
              ;; Multiple top-level options → top-level popup
              :else
              (reset! action-popup
                      {:space space
                       :options (mapv (partial opt->button space) opts)}))))

        click-food
        (fn [space]
          (reset! from-hover nil)
          (when-let [opt (first (get circ-from-map space))]
            (send-state! (:next-state opt) true)))

        ;; ── Element halos (the action's clickable elements) ────────────
        element-halos
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 hovered? (and (= :element (:kind hover))
                               (= space (:space hover)))
                 stroke-c (if hovered?
                            (board/brighten color 0.6)
                            (board/brighten color 0.3))
                 fill-op (if hovered? 0.25 0.10)]
             ^{:key (str "el-" space)}
             [:circle
              {:cx x :cy y
               :r (* radius 1.15)
               :stroke stroke-c
               :stroke-width (if hovered? (* 0.28 radius) (* 0.19 radius))
               :fill "white"
               :fill-opacity fill-op
               :style {:cursor "pointer"}
               :on-mouse-enter (fn [_e]
                                 (cancel-from-hover-clear!)
                                 (reset! dest-hover nil)
                                 (reset! from-hover {:space space :kind :element}))
               :on-mouse-leave (fn [_e] (schedule-from-hover-clear!))
               :on-click (fn [_e] (click-element space))}]))
         (keys action-from-map))

        ;; ── Food halos (every element with food → circulate sources) ───
        ;; Food on the element is rendered at (x, y - radius*0.3) by board/render-food
        food-halos
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 fx x
                 fy (- y (* radius 0.3))
                 hovered? (and (= :food (:kind hover))
                               (= space (:space hover)))]
             ^{:key (str "food-" space)}
             [:circle
              {:cx fx :cy fy
               :r (* radius (if hovered? 0.34 0.28))
               :fill "#FFD030"
               :stroke (board/brighten color 0.4)
               :stroke-width 2.5
               :fill-opacity (if hovered? 0.95 0.85)
               :style {:cursor "pointer"}
               :on-mouse-enter (fn [_e]
                                 (cancel-from-hover-clear!)
                                 (reset! dest-hover nil)
                                 (reset! from-hover {:space space :kind :food}))
               :on-mouse-leave (fn [_e] (schedule-from-hover-clear!))
               :on-click (fn [_e] (click-food space))}]))
         (keys circ-from-map))

        ;; ── Destination halos when hovering ────────────────────────────
        ;; Element hover → action's destinations
        ;; Food hover    → circulate's destinations
        hover-dests
        (when hover
          (let [{:keys [space kind]} hover
                src-map (case kind :food circ-from-map :element action-from-map nil)]
            (when src-map
              (->> (get src-map space)
                   (mapcat :destinations)
                   distinct
                   seq))))
        d-hover @dest-hover
        dest-halos
        (when (seq hover-dests)
          (mapv
           (fn [space]
             (let [[x y] (get locations space)
                   d-hovered? (= space d-hover)]
               ^{:key (str "dest-" space)}
               [:circle
                {:cx x :cy y
                 :r (* radius (if d-hovered? 1.15 1.0))
                 :stroke (if d-hovered?
                           (board/brighten color 0.9)
                           (board/brighten color 0.7))
                 :stroke-width (if d-hovered? (* 0.28 radius) (* 0.18 radius))
                 :stroke-dasharray (when-not d-hovered? "4,3")
                 :fill (if d-hovered?
                         (board/brighten color 0.6)
                         (board/brighten color 0.3))
                 :fill-opacity (if d-hovered? 0.40 0.18)
                 :style {:cursor "pointer"}
                 :on-mouse-enter (fn [_e]
                                   (cancel-from-hover-clear!)
                                   (reset! dest-hover space))
                 :on-mouse-leave (fn [_e]
                                   (reset! dest-hover nil)
                                   (schedule-from-hover-clear!))}]))
           hover-dests))

        ;; ── Popup (only for grow element-type selection) ────────────────
        popup-render
        (when popup
          (let [{:keys [space options]} popup
                [x y] (get locations space)
                n (count options)
                btn-w 130
                btn-h 36
                spread (+ btn-w 10)
                start-x (- x (* spread (/ (dec n) 2.0)))
                py (- y (* (:radius board) 2.4))]
            ^{:key "action-choice-popup"}
            [:g
             [:rect {:x -10000 :y -10000 :width 20000 :height 20000
                     :fill "transparent"
                     :on-click (fn [_e] (reset! action-popup nil))}]
             (for [[i opt] (map-indexed vector options)
                   :let [bx (- (+ start-x (* i spread)) (/ btn-w 2))]]
               ^{:key (str "popup-btn-" i)}
               [:g {:on-click (fn [e]
                                (.stopPropagation e)
                                ((:on-click opt)))
                    :style {:cursor "pointer"}}
                [:rect {:x bx :y py :width btn-w :height btn-h :rx 6
                        :fill "#0A0E1C"
                        :stroke (board/brighten color 0.5)
                        :stroke-width 2}]
                [:text {:x (+ bx (/ btn-w 2)) :y (+ py 24)
                        :text-anchor "middle"
                        :fill "#fff"
                        :font-family "monospace"
                        :font-size 14
                        :letter-spacing "1px"}
                 (:label opt)]])]))]
    (vec (concat element-halos
                 food-halos
                 (or dest-halos [])
                 (when popup-render [popup-render])))))

(defn choose-space-highlights
  [game board turn choices]
  (let [spaces (keys choices)
        elements (game/current-organism-elements game)

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [element event]
           (if (get choices (:space element))
             (send-choice! choices (:space element) true)))
         turn choices)
        
        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (send-choice! choices space true)))]
    (concat highlights element-highlights)))

(defn choose-target-highlights
  "Render the destination spaces from `choices` as halos with hover-brightening.
   Used for :move-to, :grow-to, :eat-from, :circulate-to, etc."
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        spaces (filter vector? (keys choices))
        d-hover @dest-hover]
    (mapv
     (fn [space]
       (let [[x y] (get locations space)
             d-hovered? (= space d-hover)]
         ^{:key (str "target-" space)}
         [:circle
          {:cx x :cy y
           :r (* radius (if d-hovered? 1.15 1.0))
           :stroke (if d-hovered?
                     (board/brighten color 0.9)
                     (board/brighten color 0.7))
           :stroke-width (if d-hovered? (* 0.28 radius) (* 0.18 radius))
           :stroke-dasharray (when-not d-hovered? "4,3")
           :fill (if d-hovered?
                   (board/brighten color 0.6)
                   (board/brighten color 0.3))
           :fill-opacity (if d-hovered? 0.40 0.18)
           :style {:cursor "pointer"}
           :on-mouse-enter (fn [_e]
                             (cancel-from-hover-clear!)
                             (reset! dest-hover space))
           :on-mouse-leave (fn [_e] (reset! dest-hover nil))
           :on-click (fn [_e]
                       (reset! dest-hover nil)
                       (reset! from-hover nil)
                       (send-choice! choices space true))}]))
     spaces)))

(defn grow-element-highlights
  [game board turn choices]
  (chosen-organism-highlights
   game board
   (fn [element event]
     (send-choice! choices (:type element) true))
   turn choices))

(defn grow-from-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)

        source @food-source
        elements (game/current-organism-elements game)
        fed-growers (filter
                     (fn [{:keys [type space food]}]
                       (and
                        (= :grow type)
                        (< 0 (- food (get source space 0)))))
                     elements)
        spaces (map :space fed-growers)

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [{:keys [space] :as element} event]
           (when ((set spaces) space)
             (choose-food-source! space)
             (let [source @food-source]
               (if (get choices source)
                 (send-choice! choices source true)))))
         turn choices)
        
        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-circle
              x y radius (board/brighten color 0.2)
              (fn [event]
                (choose-food-source! space)
                (let [source @food-source]
                  (if (get choices source)
                    (send-choice! choices source true)))))))
         spaces)]
    (concat highlights element-highlights)))

(defn grow-to-highlights
  [game board turn choices]
  (let [spaces (keys choices)
        elements (game/current-organism-elements game)

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [element event]
           (if (get choices (:space element))
             (send-choice! choices (:space element) true)))
         turn choices)
        
        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (send-choice! choices space true)))]
    (concat highlights element-highlights)))

(defn find-highlights
  [game board colors turn choices]
  (let [highlights
        (condp = turn
          :open []
          :create (create-highlights game board colors turn choices)
          :introduce (introduce-highlights game board turn choices)
          :choose-organism (choose-organism-highlights game board turn choices)
          :choose-action-type (choose-action-type-highlights game board turn choices)
          :choose-action (choose-action-highlights game board turn choices)
          :eat-to (choose-target-highlights game board turn choices)
          :eat-from (choose-target-highlights game board turn choices)
          :circulate-from (choose-target-highlights game board turn choices)
          :circulate-to (choose-target-highlights game board turn choices)
          :grow-element (choose-action-type-highlights game board turn choices)
          :grow-from (grow-from-highlights game board turn choices)
          :grow-to (choose-target-highlights game board turn choices)
          :move-from (choose-target-highlights game board turn choices)
          :move-to (choose-target-highlights game board turn choices)
          [])]
    ^{:key "highlights"}
    (if (empty? highlights)
      []
      (into [] (concat [:g] highlights)))))

(defn organism-board
  [game board colors turn choices]
  (println "organism board" colors turn choices board game)
  (let [svg (board/render-game board game)
        highlights (find-highlights game board colors turn choices)]
    (if (empty? highlights)
      svg
      (conj svg highlights))))

(defn generate-game-state
  [{:keys [ring-count player-count players colors player-captures mutations] :as invocation}]
  (let [ring-count   (if (number? ring-count) ring-count 4)
        player-count (if (number? player-count) player-count 2)
        symmetry (board/player-symmetry player-count)
        rings (take ring-count board/total-rings)
        starting
        (if (:RAIN mutations)
          (board/find-rain-spaces symmetry rings players)
          (board/find-starting-spaces symmetry rings players))
        _ (println "STARTING PLAYERS" starting)
        game-players (game/initial-players starting player-captures)
        game {:players game-players}
        board
        (board/generate-board
         colors
         (map first game-players)
         rings
         mutations)]
    (println "game players" game-players)
    {:game game
     :player js/playerKey
     :history []
     :board board
     :turn :create
     :choices []}))

(defn apply-invocation!
  [invocation]
  (println "INVOCATION" invocation)
  (let [generated (generate-game-state invocation)]
    (swap!
     player-captures-order
     (fn [captures-order]
       (reduce
        (fn [order [index captures]]
          (assoc order index captures))
        captures-order
        (map vector (range) (:player-captures invocation)))))
    (swap!
     player-order
     (fn [order]
       (reduce
        (fn [order [index player]]
          (assoc order index player))
        order
        (map vector (range) (:players invocation)))))
    (reset!
     game-state
     generated)))

(defn current-player-banner
  ([player color turn] (current-player-banner player color turn nil "/"))
  ([player color turn tooltip] (current-player-banner player color turn tooltip "/"))
  ([player color turn tooltip href]
   (let [show-tooltip (r/atom false)
         dismiss (fn dismiss []
                   (reset! show-tooltip false)
                   (.removeEventListener js/document "click" dismiss))]
     (fn [player color turn tooltip href]
       [:div
        {:style
         {:color "#fff"
          :border-radius "50px"
          :cursor "pointer"
          :background color
          :letter-spacing "8px"
          :font-family font-choice
          :margin "20px 0px"
          :padding "25px 60px"}}
        [:h1
         [:a
          {:style
           {:color "#fff"}
           :href href}
          player]]
        [:div
         {:style
          {:font-size "1.3em"
           :letter-spacing "5px"
           :margin "10px 0px"
           :display "flex"
           :align-items "center"
           :gap "12px"}}
         (string/join " " (string/split (name turn) #"-"))
         (when tooltip
           [:span
            {:style {:position "relative"}}
            [:span
             {:on-click (fn [e]
                          (.stopPropagation e)
                          (if @show-tooltip
                            (dismiss)
                            (do
                              (reset! show-tooltip true)
                              (.addEventListener js/document "click" dismiss))))
              :style
              {:font-size "0.7em"
               :cursor "pointer"
               :border "2px solid rgba(255,255,255,0.4)"
               :border-radius "50%"
               :width "1.4em"
               :height "1.4em"
               :display "inline-flex"
               :align-items "center"
               :justify-content "center"
               :line-height "1"
               :letter-spacing "0"
               :flex-shrink 0
               :opacity "0.45"
               :color "rgba(255,255,255,0.7)"}}
             "?"]
            (when @show-tooltip
              [:div
               {:on-click (fn [e] (.stopPropagation e))
                :style
                {:position "absolute"
                 :top "2em"
                 :left "0"
                 :z-index 100
                 :background "rgba(30,30,30,0.95)"
                 :color "#ddd"
                 :border-radius "12px"
                 :padding "16px 20px"
                 :width "320px"
                 :font-size "0.85em"
                 :letter-spacing "1px"
                 :line-height "1.6"
                 :white-space "pre-line"
                 :box-shadow "0 4px 20px rgba(0,0,0,0.5)"
                 :cursor "default"}}
               tooltip])])]]))))

(def turn-descriptions
  {:pass "pass"
   :actions-complete "resolve conflicts"
   :resolve-conflicts "check integrity"
   :player-victory "declare victory!"
   :check-integrity "confirm turn"})

(defn progress-control
  [turn choices advance]
  (if-let [description (get turn-descriptions turn)]
    [:span
     {:style
      {:color "#fff"
       :cursor "pointer"
       :border-radius "20px"
       :background "hsl(100,50%,50%)"
       :font-size "1.2em"
       :letter-spacing "4px"
       :margin "10px 10px"
       :padding "5px 20px"}
      :on-click
      (fn [event]
        (send-state! (get-in choices [advance :state]) true))}
     description]))

(defn current-action-index
  [num-actions actions]
  (cond
    (empty? actions) 0

    (game/complete-action? (last actions))
    (when (not= (count actions) num-actions)
      (count actions))

    :else (dec (count actions))))

(def background-color "#222")

(defn eat-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background background-color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :color "#fff"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      "eat"]
     [:span
      {:style
       {:margin "0px 5px"}}
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(def element-choice-map
  {:eat "eater"
   :grow "grower"
   :move "mover"})

(defn grow-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background background-color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :cursor "pointer"
          :color "#fff"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      "grow"]
     (if-let [element (-> action :action :element)]
       [:span
        {:style
         {:margin "0px 20px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "5px"
          :background background-color
          :font-size "1.0em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "2px 10px"}}
        element]
       [:span
        {:style
         {:margin "0px 10px"}}
        (map-indexed
         (fn [index element-choice]
           ^{:key element-choice}
           [:span
            [:span
             {:style
              {:margin "0px 10px"
               :color "#fff"
               :border-width "2px"
               :border-radius "5px"
               :background color
               :font-size "1.0em"
               :letter-spacing "5px"
               :font-family font-choice
               :cursor "pointer"
               :padding "2px 10px"}
              :on-click
              (fn [event]
                (if-let [choice (get choices element-choice)]
                  (send-state! (:state choice) true)))}
             (element-choice-map element-choice)]
            (if (not= index (dec (count choices)))
              " / ")])
         (keys choices))])
     [:span
      (if-let [from (-> action :action :from)]
        (concat
         (list " from ")
         (map
          (fn [[[ring space] food]]
            ^{:key [ring space]}
            [:span
             {:style
              {:background (get board-colors ring)
               :color "#fff"
               :margin "0px 5px"
               :font-family font-choice
               :padding "5px 5px"
               :border-radius "5px"}}
             (str " " ring " " space " : " food " ")])
          from)))
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn move-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background background-color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :color "#fff"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      "move"]
     [:span
      {:style
       {:margin "0px 5px"}}
      (if-let [from (-> action :action :from)]
        (list
         " from "
         [:span
          {:style
           {:background (get board-colors (first from))
            :color "#fff"
            :font-family font-choice
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " from)]))
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn circulate-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background background-color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :color "#fff"
          :cursor "pointer"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      (if (-> action :action :pass)
        "pass"
        "circulate")]
     [:span
      {:style
       {:margin "0px 5px"}}
      (if-let [from (-> action :action :from)]
        (list
         " from "
         [:span
          {:style
           {:background (get board-colors (first from))
            :color "#fff"
            :font-family font-choice
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " from)]))
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn pass-action-control
  [board-colors turn choices color action action-index])

(def action-control-map
  {:eat eat-action-control
   :grow grow-action-control
   :move move-action-control
   :circulate circulate-action-control
   :pass pass-action-control})

(defn past-action-control
  [board-colors turn choices color choice action action-index]
  [:div
   [(get action-control-map (:type action)) board-colors turn choices color action action-index]])

(defn choose-action-control
  [turn choices color choice]
  [:span
   {:style
    {:color "#fff"
     :border-radius "20px"
     :margin "20px 5px"
     :cursor "pointer"
     :background color
     :font-size "1.2em"
     :letter-spacing "7px"
     :font-family font-choice
     :padding "5px 20px"}
    :on-click
    (condp = turn
      :choose-action
      (fn [event]
        (if (get choices choice)
          (send-choice! choices choice true)))
      (fn [event]))}
   choice])

(defn circulate-control
  [turn choices color]
  [:span
   {:style
    {:color "#fff"
     :border-radius "20px"
     :margin "20px 5px"
     :cursor "pointer"
     :background color
     :font-size "1.2em"
     :letter-spacing "7px"
     :font-family font-choice
     :padding "5px 20px"}
    :on-click
    (condp = turn
      :choose-action
      (fn [event]
        (if (:circulate choices)
          (send-choice! choices :circulate true)))
      (fn [event]))}
   "circulate"])

(defn current-action-control
  [board-colors turn choices color choice action action-index]
  (if-let [type (:type action)]
    [:div
     [(get action-control-map type) board-colors turn choices color action action-index]]
    [:div
     {:style
      {:margin "20px 0px"}}
     (if (get choices choice)
       [choose-action-control turn choices color choice])
     (if (:circulate choices)
       [:span
        " / "
        [circulate-control turn choices color]])]))

(defn future-control
  [color choice]
  [:span
   {:style
    {:color color
     :border-style "solid"
     :border-width "2px"
     :border-radius "10px"
     :margin "20px 5px"
     :background background-color
     :font-size "1.0em"
     :letter-spacing "7px"
     :font-family font-choice
     :padding "5px 20px"}}
   choice])

(defn future-action-control
  [board-colors turn choices color choice action action-index]
  [:div
   {:style
    {:margin "20px 0px"}}
   [future-control color choice]
   " / "
   [future-control color "circulate"]])

(defn action-controls
  [board-colors turn choices color {:keys [choice num-actions actions] :as organism-turn}]
  (if choice
    (let [current-action (current-action-index num-actions actions)]
      [:div
       (map
        (fn [action-index]
          (let [action
                (if (< action-index (count actions))
                  (nth actions action-index)
                  {})]
            ^{:key action-index}
            (cond

              (nil? current-action)
              [past-action-control board-colors turn choices color choice action action-index]

              (> action-index current-action)
              [future-action-control board-colors turn choices color choice action action-index]

              (= action-index current-action)
              [current-action-control board-colors turn choices color choice action action-index]

              :else
              [past-action-control board-colors turn choices color choice action action-index])))

        (range num-actions))])
    [:div]))

(defn undo-control
  [turn choices state]
  [:div
   {:style
    {:font-family font-choice
     :margin "40px 0px"}}

   [:div
    {:style
     {:margin "15px 0px"}}
    [:span
     {:title "reset to the beginning of your turn"
      :style
      {:color "#fff"
       :cursor "pointer"
       :border-radius "10px"
       :background "hsl(200,50%,80%)"
       :font-size "1.2em"
       :letter-spacing "4px"
       :margin "0px 10px"
       :padding "5px 20px"}
      :on-click
      (fn [event]
        (if (and
             (= turn :introduce)
             (not= @introduction empty-introduction))
          (reset! introduction empty-introduction)
          (send-clear!)))}
     "clear"]]

   [:div
    {:style
     {:margin "15px 0px"}}
    [:span
     {:title "take one step back, potentially to previous player's turn"
      :style
      {:color "#fff"
       :cursor "pointer"
       :border-radius "10px"
       :background "hsl(0,50%,50%)"
       :font-size "1.2em"
       :letter-spacing "4px"
       :margin "0px 10px"
       :padding "5px 20px"}
      :on-click
      (fn [event]
        (if (and
             (= turn :introduce)
             (not= @introduction empty-introduction))
          (reset! introduction empty-introduction)
          (do
            (reset! food-source {})
            (send-reset! state))))}
     "undo"]]

   (when (= turn :choose-action)
     [:div
      {:style
       {:margin "15px 0px"}}
      [:span
       {:title "pass this action"
        :style
        {:color "#fff"
         :cursor "pointer"
         :border-radius "10px"
         :background "hsl(100,50%,50%)"
         :font-size "1.2em"
         :letter-spacing "4px"
         :margin "0px 10px"
         :padding "5px 20px"}
        :on-click
        (fn [event]
          (send-state!
           (-> {:state state}
               (game/choose-action :circulate)
               (game/pass-action)
               :state)
           true))}
       "pass"]])

   [:div
    {:style
     {:margin "15px 0px"}}
    [progress-control turn choices (if (= turn :pass) :pass :advance)]]])

(defn organism-controls
  [game board turn choices history]
  (let [player-turn (game/get-player-turn game)
        organism-turn (game/get-organism-turn game)
        action-type (:choice organism-turn)
        current-action (last (:actions organism-turn))

        player-colors (:player-colors board)
        current-player (game/current-player game)
        current-color (get player-colors current-player)
        dormant-color (board/brighten current-color -0.7)
        focus-color (board/brighten current-color 0.4)
        board-colors (into {} (:colors board))

        element-radius 45
        element-controls
        (map
         vector
         [[50 50] [150 50] [100 130]]
         [:eat :grow :move])
        {:keys [chosen-space chosen-element progress] :as introduce} @introduction]

    (if current-player
      [:div
       {:style
        {:margin "20px 20px"}}
       [current-player-banner current-player current-color turn nil (str js/playerPath "/" js/playerKey)]
       [:div
        {:style
         {:margin "0px 40px"}}
        [:svg
         {:width 200 :height 180}

         ;; ELEMENT CONTROLS
         (vec
          (concat
           [:g]
           (for [[location type] element-controls]
             (let [type->location
                   (into
                    {}
                    (map
                     (fn [[location type]]
                       [type location])
                     element-controls))

                   element-state
                   (cond 
                     (and
                      (not (= turn :choose-organism))
                      (or
                       (and
                        (= turn :introduce)
                        (= chosen-element type))
                       (= type action-type)))
                     :focus

                     (or
                      (and
                       (= turn :introduce)
                       (some #{type} (vals progress)))
                      (not (nil? action-type)))
                     :dormant
                     :else :neutral)
                   
                   color
                   (condp = element-state
                     :focus focus-color
                     :dormant dormant-color
                     :neutral current-color)]

               ^{:key type}
               (-> (board/render-element
                    color color
                    {:ratio 0.02 :color "#ccc"}
                    location
                    element-radius
                    {:type type :food 0})
                   (assoc-prop :style {:cursor "pointer"})
                   (assoc-prop :title type)
                   
                   (assoc-prop
                    :on-click
                    (fn [event]
                      (condp = turn
                        :introduce
                        (if (= type chosen-element)
                          (swap!
                           introduction
                           dissoc
                           :chosen-element)
                          (if chosen-space
                            (do
                              (swap!
                               introduction
                               (fn [intro]
                                 (-> intro
                                     (dissoc :chosen-element)
                                     (dissoc :chosen-space)
                                     (update :progress (fn [pro] (assoc pro chosen-space type))))))
                              (send-introduction! choices @introduction))
                            (swap! introduction assoc :chosen-element type)))
                        :choose-action-type
                        (send-choice! choices type true)))))))))]

        [:br]

        (when-not (= turn :choose-organism)
          [action-controls board-colors turn choices current-color organism-turn])

        (if-not (-> game :state :winner)
          [undo-control turn choices (:state game)])]])))

(defn flex-direction
  [direction]
  {:style
   {:display "flex"
    :flex-direction flex-direction}})

(defn flex-grow
  [direction grow]
  (assoc-in
   (flex-direction direction)
   [:style :flex-grow]
   grow))

(defn game-layout
  [inner]
  [:div
   (assoc-in
    (flex-direction "column")
    [:style :color]
    "#eee")
   inner])

(defn reset-colors-input
  [color]
  (let [invocation @board-invocation]
    [:input
     {:type :button
      :value "reset colors"
      :style
      {:border-radius "20px"
       :color "#fff"
       :cursor "pointer"
       :background color
       :border "0px solid"
       :font-size "1.0em"
       :letter-spacing "3px"
       :margin "10px 0px"
       :padding "7px 20px"}
      :on-click
      (fn [event]
        (let [invocation @board-invocation
              colors (board/generate-colors-buffer
                      board/total-rings
                      (:ring-count invocation)
                      max-players)]
          (-> invocation
              (assoc :colors colors)
              send-create!)))}]))

        ;; (if valid?
        ;;   (ws/send-transit-message!
        ;;    {:type "trigger-creation"})
        ;;   (dom/redirect!
        ;;    (str js/playerPath "/" js/playerKey)))

(defn ring-count-input
  [color]
  (let [invocation @board-invocation]
    [:div
     [:select
      {:id "ring-count"
       :name "ring-count"
       :value (:ring-count invocation)
       :style
       {:background-color color}
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)
               invocation @board-invocation
               colors (board/generate-colors-buffer
                       board/total-rings
                       value
                       max-players)]
           (-> invocation
               (assoc :ring-count value)
               (assoc :colors colors)
               send-create!)))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 3 8))]
     [:label
      {:for "ring-count"
       :style
       {:font-size "1.5em"}}
      "rings"]]))

(defn player-count-input
  [color]
  (let [invocation @board-invocation]
    [:div
     [:select
      {:id "player-count"
       :name "player-count"
       :value (:player-count invocation)
       :style
       {:background-color color}
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)
               order @player-order
               captures-order @player-captures-order
               colors (board/generate-colors-buffer
                       board/total-rings
                       (:ring-count invocation)
                       max-players)
               players (vec
                        (take
                         (if (get-in invocation [:mutations :RAIN])
                           (inc value)
                           value)
                         order))
               captures (vec
                         (take
                          (if (get-in invocation [:mutations :RAIN])
                           (inc value)
                           value)
                          captures-order))]
           (-> invocation
               (assoc :colors colors)
               (assoc :player-count value)
               (assoc :players players)
               (assoc :player-captures captures)
               send-create!)))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 1 11))]
     [:label
      {:for "player-count"
       :style
       {:font-size "1.5em"}}
      "players"]]))

(defn organism-victory-input
  [color]
  (let [invocation @board-invocation]
    [:div
     [:select
      {:id "organism-victory"
       :name "organism-victory"
       :value (:organism-victory invocation)
       :style
       {:background-color color}
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)]
           (-> invocation
               (assoc :organism-victory value)
               (send-create!))))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 3 14))]
     [:label
      {:for "organism-victory"
       :style
       {:font-size "1.5em"}}
      "organisms for victory"]]))

(defn send-player-name!
  [index player-name]
  (swap! player-order assoc index player-name)
  (swap! board-invocation update :players
         (fn [players] (assoc (vec players) index player-name)))
  (components/send-player-name! index player-name))

(defn player-slot-input
  "Wraps the shared player-search-input for organism's create page."
  [index color player page-player invocation in-game?]
  [components/player-search-input
   {:slot-id   index
    :value     player
    :color     color
    :game-type "organism"
    :search?   in-game?
    :placeholder (if in-game? "search players..." "click to join")
    :on-change (fn [v] (send-player-name! index v))
    :on-select (fn [{:keys [name bot?]}]
                 ;; If picking a bot, auto-suffix alphabetically (OBO-A, OBO-B, ...)
                 (let [existing (->> (:players invocation)
                                     (map-indexed vector)
                                     (remove (fn [[i _]] (= i index)))
                                     (map second)
                                     set)
                       chosen (if bot?
                                (or (some
                                     (fn [c]
                                       (let [candidate (str name "-" c)]
                                         (when-not (existing candidate) candidate)))
                                     (map char (range 65 91))) ;; A-Z
                                    name)
                                name)]
                   (send-player-name! index chosen)
                   (send-open-game! (update invocation :players assoc index chosen))))
    :on-focus  (fn []
                 (when (and (not in-game?) (empty? player))
                   (send-player-name! index page-player)
                   (send-open-game! (update invocation :players assoc index page-player))))
    :on-blur   (fn [] (send-open-game! invocation))}])

(defn players-input
  [page-player invocation]
  (let [{:keys [player-count colors player-captures mutations]} invocation
        player-count (if (:RAIN mutations)
                       (inc player-count)
                       player-count)
        order @player-order
        captures-order @player-captures-order
        in-game? (some #{page-player} (take player-count order))]
    [:div
     [:h3
      {:style
       {:margin "20px 0px 0px 0px"}}
      [:span
       {:title "click an empty field to join the game\nor modify to add other players"}
       "players joined "]
      [:span
       {:title "how many captures each player is required to win"
        :style {:font-size "0.8em"}}
       " (capture limit)"]]
     (map
      (fn [index color player captures]
        ^{:key index}
        [:div
         [player-slot-input index color player page-player invocation in-game?]

         [:select
          {:value captures
           :style
           {:background-color color}
           :on-change
           (fn [event]
             (let [value (-> event .-target .-value js/parseInt)]
               (swap!
                player-captures-order
                assoc index value)
               (-> invocation
                   (assoc
                    :player-captures
                    (vec
                     (take
                      player-count
                      @player-captures-order)))
                   (send-create!))))}

          (map
           (fn [n]
             ^{:key n}
             [:option
              {:value n}
              n])
           (range 1 14))]])
      (range)
      (reverse
       (take
        player-count
        (map last colors)))
      order
      (take player-count captures-order))]))

(defn create-button
  [active-color inactive-color invocation]
  (let [valid? (board/valid-invocation? invocation)]
    [:input
     {:type :button
      :value (if valid? "CREATE" "incomplete")
      :style
      {:border-radius (if valid? "50px" "10px")
       :color "#fff"
       :cursor "pointer"
       :background (if valid? active-color inactive-color)
       :border "3px solid"
       :font-size "2em"
       :letter-spacing "8px"
       :margin "10px 0px"
       :padding "25px 60px"}
      :on-click
      (fn [event]
        (if valid?
          (let [game-key (if (empty? @create-game-key)
                           (let [k (generate-game-key)]
                             (reset! create-game-key k)
                             k)
                           @create-game-key)
                trigger! (fn []
                           (ws/send-transit-message!
                            {:type "create"
                             :invocation @board-invocation})
                           (ws/send-transit-message!
                            {:type "trigger-creation"}))]
            (if @ws/ws-channel
              (trigger!)
              (connect-create-ws! game-key trigger!)))
          (dom/redirect!
           (str js/playerPath "/" js/playerKey))))}]))

(defn description-input
  [{:keys [description] :as invocation} foreground-color background-color]
  [:div
   [:h3
    {:style
     {:margin "20px 0px 0px 0px"}}
    [:span
     {:title "explain a bit about the game you are creating for potential players"}
     "description"]]
   [:textarea
    {:value (or description "")
     :rows (inc (quot (count description) 49))
     :style
     {:border-radius "25px"
      :color foreground-color
      :background background-color
      ;; :border "3px solid"
      :font-size "0.9em"
      :letter-spacing "1px"
      :margin "2px 0px"
      :width "460px"
      :padding "10px 30px"}
     ;; :on-blur
     ;; (fn [event]
     ;;   (send-open-game!
     ;;    (assoc invocation :description @description)))
     :on-change
     (fn [event]
       (let [value (-> event .-target .-value)]
         (send-create!
          (assoc invocation :description value))))}]])

(defn invocation-player-colors
  [number invocation]
  (reverse
   (take
    number
    (map
     last
     (:colors invocation)))))

(defn adjust-players
  [invocation player-count]
  (-> invocation
      (assoc :players (take player-count @player-order))
      (assoc :player-captures (take player-count @player-captures-order))))

(defn increase-players
  [invocation]
  (let [player-count (inc (:player-count invocation))]
    (adjust-players invocation player-count)))

(defn decrease-players
  [invocation]
  (let [player-count (:player-count invocation)]
    (adjust-players invocation player-count)))

(def invocation-mutations
  {:RAIN
   {:mutate increase-players
    :unmutate decrease-players}})

(defn mutate-invocation
  [mutation-key mutation-state invocation]
  (let [invocation (assoc-in invocation [:mutations mutation-key] mutation-state)]
    (if-let [mutate (get-in invocation-mutations [mutation-key :mutate])]
      (mutate invocation)
      invocation)))

(defn unmutate-invocation
  [mutation-key mutation-state invocation]
  (let [invocation (update invocation :mutations dissoc mutation-key)]
    (if-let [mutate (get-in invocation-mutations [mutation-key :unmutate])]
      (mutate invocation)
      invocation)))

(defn mutation-choice
  [color invocation [mutation-key mutation-description]]
  ^{:key mutation-key}
  [:div
   [:input
    {:type "checkbox"
     :id mutation-key
     :name mutation-key
     :value mutation-key
     :checked (get-in invocation [:mutations mutation-key])
     :style
     {:margin "5px 10px"
      :background-color color}
     :on-change
     (fn [event]
       (let [target (.-target event)
             checked (.-checked target)
             mutation-state (get game/default-mutation-state mutation-key {})
             invocation
             (if checked
               (mutate-invocation mutation-key mutation-state invocation)
               (unmutate-invocation mutation-key mutation-state invocation))]
         (send-create! invocation)))}]
   [:label
    {:for mutation-key
     :style
     {:color color}}
    (display-mutation mutation-key mutation-description)]])

(defn mutations-select
  [color invocation]
  [:div
   [:h3
    {:style
     {:margin "20px 0px 0px 0px"}}
    [:span
     {:title "choose which mutations you want to be active in the game"}
     "mutations"]]
   [:div
    (map (partial mutation-choice color invocation) possible-mutations)]])

(defn connect-create-ws!
  ([game-key] (connect-create-ws! game-key nil))
  ([game-key on-open]
   (components/connect-create-ws! "/ws/organism/play/" game-key update-messages! on-open)))

(defn game-name-input
  [color]
  (let [connected? (some? @ws/ws-channel)]
    [:div
     {:style {:margin-bottom "30px"}}
     [:h3
      {:style {:margin "20px 0px 0px 0px"}}
      "name"]
     [:input
      {:value @create-game-key
       :style
       {:border-radius "25px"
        :color "#fff"
        :background (if connected? color "#555")
        :border (str "3px solid " (if connected? color "#777"))
        :font-size "1.5em"
        :letter-spacing "6px"
        :margin "2px 0px"
        :width "366px"
        :padding "10px 30px"}
       :on-change
       (fn [event]
         (reset! create-game-key (-> event .-target .-value)))
       :on-blur
       (fn [_] (connect-create-ws! @create-game-key))
       :on-key-up
       (fn [event]
         (when (= (.-key event) "Enter")
           (connect-create-ws! @create-game-key)))}]]))

(def create-explanation
  (string/join "\n\n"
    ["Every game has a unique key. A game will always be in one of three states: OPEN / ACTIVE / COMPLETE."
     "From this page you can choose the number of rings and number of players, as well as the number of organisms required for victory."
     "You can also choose which other players will be in the game, as well as their personal capture limit required for victory (this defaults to 5)."
     "If you want to leave some player spots open for others to join, just leave them blank. It will show up in everyone's player page under OPEN."
     "To join an open game, simply click on the empty player slot and it will fill in your player name."
     "Once all players have joined and you feel good about the game, hit the CREATE button to begin!"]))

(defn create-page
  []
  (let [invocation @board-invocation
        {:keys [game board turn choices]} @game-state
        {:keys [state turn-order]} game
        turn-order (:players invocation)
        player-captures (:player-captures invocation)
        organism-victory (:organism-victory invocation)
        description (:description invocation)
        mutations (:mutations invocation)
        invocation-colors (invocation-player-colors (count turn-order) invocation)
        player-colors (into {} (map vector turn-order invocation-colors))
        create-color (-> invocation :colors rest first last)
        select-color (-> invocation :colors first last)
        inactive-color (-> invocation :colors last last)]
    (game-layout
     [:main
      (flex-grow "row" 1)
      [:nav
       {:style
        {:width "30%"}}
       [:div
        {:style
         {:margin "20px 20px"}}
        [current-player-banner js/playerKey (get player-colors js/playerKey inactive-color) "create game" create-explanation js/homePath]]
       [:form
        {:style
         {:margin "40px 60px"}}
        [game-name-input create-color]
        [ring-count-input select-color]
        [player-count-input select-color]
        [description-input invocation select-color inactive-color]
        [players-input js/playerKey invocation]
        [:div
         {:style {:display "flex" :flex-direction "column" :align-items "center" :width "fit-content" :margin "20px 40px"}}
         [reset-colors-input inactive-color]
         [create-button create-color inactive-color invocation]]
        [mutations-select create-color invocation]]]
      [:article
       {:style {:flex-grow 1}}
       [organism-board game board invocation-colors turn choices]]
      (println "INVOCATION" invocation)
      [:aside
       {:style
        {:width "30%"}}
       [chat-panel description turn-order organism-victory invocation-colors player-colors player-captures mutations state [] nil @chat]]])))

(defn game-page
  []
  (let [invocation @board-invocation
        {:keys [game board turn choices history cursor]} @game-state
        {:keys [state turn-order]} game
        {:keys [player-captures organism-victory description mutations]} invocation
        state (if cursor (nth history cursor) state)
        game (assoc game :state state)
        invocation-colors (invocation-player-colors (count turn-order) invocation)
        [turn choices] (if cursor (choice/find-state game) [turn choices])
        {:keys [player-colors]} board]
    (game-layout
     [:main
      (flex-grow "row" 1)
      [:aside
       {:style
        {:width "30%"}}
       [organism-controls game board turn choices history]]
      [:article
       {:style {:flex-grow 1}}
       [organism-board game board invocation-colors turn choices]]
      [:nav
       {:style {:width "30%"}}
       [chat-panel description turn-order organism-victory invocation-colors player-colors player-captures mutations state history cursor @chat]]])))


(defn open-games-section
  "Organism wrapper around the shared open-games-section."
  [player games]
  [components/open-games-section
   {:games games
    :link-prefix "/organism/create/"
    :current-player player
    :font-family font-choice
    :colors-fn (fn [invocation]
                 (invocation-player-colors (:player-count invocation) invocation))}])

(defn player-active?
  [player games]
  (let [active-games (get games "active")]
    (some?
     (some
      (fn [game]
        (= player (:current-player game)))
      active-games))))

(defn active-games-section
  [player games]
  (when-not (empty? games)
    [:div
     {:style
      {:margin "20px 40px"}}
     [:h2
      [:span
       {:title "A solid color row indicates it is your turn in that game.\nThe icon on the tab for this page will turn green when it is your turn."}
       "ACTIVE"]]
     (for [{:keys [game round players player-colors current-player invocation]} games]
       (let [player-color (get player-colors player)
             ring-count (:ring-count invocation)
             organism-victory (:organism-victory invocation)]
         ^{:key game}
         [:div
          {:style
           (if (= player current-player)
             {:background player-color
              :margin "10px 20px"
              :padding "10px 0px"
              :border-radius "10px"}
             {:margin "10px 20px"
              :padding "10px 0px"})}
          [:span
           {:title
            (str
             (when ring-count
               (str ring-count " rings | "))
             (when organism-victory
               (str organism-victory " organisms for victory\n\n"))
             (:description invocation))}
           [:a
            {:href (str "/organism/play/" game)
             :style
             {:color "#fff"
              :border-radius "15px"
              :background player-color
              :padding "10px 20px"
              :letter-spacing "5px"
              :font-family font-choice
              :font-size "1.3em"}}
            game]]
          [:span
           {:style
            {:margin "0px 20px"}}
           " round " (inc round)]
          (for [game-player players]
            (let [current-color (get player-colors game-player)]
              ^{:key game-player}
              [:span
               [:a
                {:href (str js/playerPath "/" game-player)
                 :style
                 (if (= game-player current-player)
                   {:color "#fff"
                    :border-radius "20px"
                    :background current-color
                    :margin "0px 10px"
                    :padding "7px 20px"}
                   {:padding "5px 10px"
                    :margin "0px 10px"
                    :border-style "solid"
                    :border-width "2px"
                    :border-color current-color
                    :border-radius "5px"
                    :color current-color})}
                game-player]]))]))]))

(defn complete-games-section
  [player games]
  (when-not (empty? games)
    [:div
     {:style
      {:margin "20px 40px"}}
     [:h2 "COMPLETE"]
     (for [{:keys [game round players player-colors winner]} (reverse games)]
       (let [player-color (get player-colors player)]
         ^{:key game}
         [:div
          {:style
           (if (= player winner)
             {:background player-color
              :margin "10px 20px"
              :padding "10px 0px"
              :border-radius "10px"}
             {:margin "10px 20px"
              :padding "10px 0px"})}
          [:span
           [:a
            {:href (str "/organism/play/" game)
             :style
             {:color "#fff"
              :border-radius "15px"
              :background player-color
              :padding "10px 20px"
              :letter-spacing "5px"
              :font-family font-choice
              :font-size "1.3em"}}
            game]]
          [:span
           {:style
            {:margin "0px 20px"}}
           " round " (inc round)]
          (for [game-player players]
            (let [current-color (get player-colors game-player)]
              ^{:key game-player}
              [:span
               [:a
                {:href (str js/playerPath "/" game-player)
                 :style
                 (if (= game-player winner)
                   {:color "#fff"
                    :border-radius "20px"
                    :background current-color
                    :margin "0px 10px"
                    :padding "7px 20px"}
                   {:padding "5px 10px"
                    :margin "0px 10px"
                    :border-style "solid"
                    :border-width "2px"
                    :border-color current-color
                    :border-radius "5px"
                    :color current-color})}
                game-player]]))]))]))

(defn player-page-banner
  [player color turn]
  [:div
   {:style
    {:color "#fff"
     :border-radius "50px"
     :cursor "pointer"
     :background color
     :letter-spacing "8px"
     :font-family font-choice
     :margin "20px 0px"
     :padding "25px 60px"}
    :on-click
    (fn [event]
      (let [color (board/random-color 0.2 0.9)]
        (swap! player-preferences assoc :color color)
        (ajax/post-preferences! player {:color color})))}
   [:h1
    [:a
     {:style
      {:color "#fff"}
      :href js/homePath}
     player]]
   [:div
    {:style
     {:font-size "1.3em"
      :letter-spacing "5px"
      :margin "10px 0px"}}
    (string/join " " (string/split (name turn) #"-"))]])

(defn player-page
  [player]
  (let [games @player-games
        color (:color @player-preferences)]
    [:div
     {:style
      {:padding "20px"
       :color "#eee"}}
     [player-page-banner player color "games"]
     [open-games-section player (get games "open")]
     [active-games-section player (get games "active")]
     [complete-games-section player (get games "complete")]]))

(defn valid-player-name?
  [players player]
  (and
   (not
    (empty? player))
   (not
    (players player))))

(defonce player-key
  (r/atom ""))

(defonce home-color
  (r/atom (board/random-color 0.5 0.8)))

(defn home-page
  [player-records]
  (let [color @home-color
        players (set (map :key player-records))
        active-color "#3b5"
        inactive-color "#444"]
    [:div
     {:style
      {:padding "20px 0px"
       :color "#eee"}}
     [:div
      {:style
       {:color "#fff"
        :border-radius "50px"
        :cursor "pointer"
        :background color
        :letter-spacing "8px"
        :font-size "1.2em"
        :margin "0px 20px"
        :padding "25px 60px"}}
      [:h1 "ORGANISM"]
      [:h2 "welcome"]]
     [:div
      {:style
       {:margin "20px 20px"
        :padding "25px 60px"
        :font-size "1.2em"
        :font-family font-choice}}
      [:p "Welcome to ORGANISM!"]
      [:p "To begin, choose a player name ->"]
      [:input
       {:type :text
        :style
        {:border-radius "25px"
         :color "#fff"
         :background color
         :border "3px solid"
         :font-size "2em"
         :letter-spacing "8px"
         :margin "20px 20px"
         :padding "10px 40px"}
        :on-key-up
        (fn [event]
          (let [value (-> event .-target .-value)
                key (-> event .-key)]
            (reset! player-key value)
            (let [valid? (valid-player-name? players @player-key)]
              (if (and valid? (= key "Enter"))
                (dom/redirect!
                 (str js/playerPath "/" value))))))}]
      [:div
       (let [valid? (valid-player-name? players @player-key)]
         [:input
          {:type :button
           :value (if valid? "PLAY" "name taken")
           :style
           {:border-radius (if valid? "50px" "10px")
            :color "#fff"
            :cursor "pointer"
            :background (if valid? active-color inactive-color)
            :border "3px solid"
            :font-size (if valid? "1.3em" "1.1em")
            :letter-spacing "8px"
            :margin "15px 50px"
            :padding "10px 40px"}
           :on-click
           (fn [event]
             (when (valid-player-name? players @player-key)
               (dom/redirect!
                (str js/playerPath "/" @player-key))))}])]]]))

;; observe-games-section moved to organism.components (shared library)

(defn observe-page []
  [components/observe-page
   {:title "observe"
    :games @observe-games
    :link-prefix "/organism/play/"
    :player-link-prefix (or js/playerPath "/organism/player/")
    :home-path js/homePath
    :font-family font-choice
    :colors-fn (fn [invocation]
                 (invocation-player-colors (count (:players invocation)) invocation))}])

(defn stats-page []
  [components/players-page
   {:title "players"
    :stats @player-stats
    :player-link-prefix (or js/playerPath "/organism/player")
    :home-path js/homePath
    :font-family font-choice}])

(defn page-container
  []
  (cond
    js/isStats    [stats-page]
    js/isObserve  [observe-page]
    js/isCreate   [create-page]
    js/playerKey  (cond
                    js/playKey (let [invocation @board-invocation]
                                 (if (:created invocation)
                                   [game-page]
                                   [create-page]))
                    :else      [player-page js/playerKey])
    :else         [home-page (reader/read-string js/players)]))

(defn update-messages!
  [{:keys [type] :as received}]
  (println "MESSAGE RECEIVED" received)
  (condp = type
    "initialize"
    (if js/isCreate
      (dom/redirect! (str "/organism/play/" @create-game-key))
      (do
        (swap! game-state initialize-game received)
        (reset! board-invocation (:invocation received))
        (reset! clear-state (-> received :game :state))
        (swap! chat initialize-chat received)
        (if-let [cursor (:cursor @game-state)]
          (let [total (count (:history received))]
            (if (< cursor total)
              (set-history-advance! total cursor))))))
    "create"
    (if js/isCreate
      ;; On the create page, local state is authoritative — don't let
      ;; the server's default overwrite what the user configured
      nil
      (do
        (reset! board-invocation (:invocation received))
        (reset! chat (:chat received))
        (apply-invocation! @board-invocation)))
    "player-name"
    (let [{:keys [index player]} received]
      (swap! player-order assoc index player)
      (swap! board-invocation update :players (fn [players] (assoc (vec players) index player))))
    "game-state"
    (do
      (swap! game-state update-game received)
      (reset! food-source {})
      (swap!
       introduction
       (fn [introduction]
         (-> introduction
             (assoc :progress (-> received :game :state :player-turn :introduction))
             (assoc :chosen-element nil)
             (assoc :chosen-space nil)))))
    ;; bot-choices: server sends a list of choice keys the bot picked.
    ;; Client replays them via the SAME find-state choice flow the bot used.
    "bot-choices"
    (let [choice-keys (:choices received)]
      (swap! game-state
             (fn [gs]
               (let [replayed
                     (reduce
                      (fn [game ck]
                        (let [[_phase choices] (choice/find-state game)
                              ;; Look up the choice by key, fall back to advance/pass
                              next (or (get choices ck)
                                       (get choices :advance)
                                       (get choices :pass))]
                          (or next game)))
                      (:game gs)
                      choice-keys)
                     [final-game turn choices] (choice/find-next-choices replayed)]
                 (-> gs
                     (assoc :game final-game)
                     (update :history conj (:state final-game))
                     (assoc :turn turn)
                     (assoc :choices choices))))))
    "chat" (swap! chat update-chat received)))

;; -------------------------
;; Routes

(def router
  (reitit/router
    [["/" :home]
     ["/about" :about]
     ["/player/:player"]
     ["/game/:game"]]))

(defn match-route [uri]
  (->> (or (not-empty (string/replace uri #"^.*#" "")) "/")
       (reitit/match-by-path router)
       :data
       :name))
;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
      HistoryEventType/NAVIGATE
      (fn [^js/Event.token event]
        (swap! session assoc :page (match-route (.-token event)))))
    (.setEnabled true)))

(defn mount-components
  []
  (println "MOUNTING")
  (rdom/render [#'page-container] (.getElementById js/document "organism")))

(def dormant-favicon "/favicon/dormant.ico")
(def active-favicon "/favicon/active.ico")
(def neutral-favicon "/favicon/neutral.ico")

(defn init!
  []
  (let [player? (not (empty? js/playerKey))
        game? (not (empty? js/playKey))
        player-games? (and player? (not game?))
        observer? (and game? (not player?))
        player (if player? js/playerKey game/observer-key)
        window-height (.-innerHeight js/window)
        body-height (.-scrollHeight (.-body js/document))]
    (ajax/load-interceptors!)
    (hook-browser-navigation!)
    (let [protocol
          (if (= (.-protocol js/location) "https:")
            "wss:"
            "ws:")]
      (if js/playerPreferences
        (reset!
         player-preferences
         (merge
          {:color (board/random-color 0.2 0.9)}
          (reader/read-string js/playerPreferences))))
      (if js/playerGames
        (let [games (reader/read-string js/playerGames)
              favicon-path
              (if (player-active? player games)
                active-favicon
                dormant-favicon)]
          (dom/change-favicon favicon-path)
          (reset! player-games games)
          (.setInterval
           js/window
           (fn []
             (.reload js/location))
           300000))
        (dom/change-favicon neutral-favicon))
      (when js/isObserve
        (when js/observeGames
          (reset! observe-games (reader/read-string js/observeGames))))
      (when js/isStats
        (when js/playerStats
          (reset! player-stats (reader/read-string js/playerStats))))
      (when js/isCreate
        ;; Always start from a fresh empty-invocation, then overlay preloaded state.
        ;; This guards against stale defonce values from prior browser sessions.
        (let [base (board/empty-invocation
                    (if (and (exists? js/playerKey) (not (empty? js/playerKey)))
                      js/playerKey
                      "orb"))]
          (reset! board-invocation base))
        (when-let [inv (components/preloaded-invocation)]
          (reset! board-invocation inv))
        (when-let [pk (components/preloaded-play-key)]
          (reset! create-game-key pk))
        (apply-invocation! @board-invocation)
        (when-let [pk (components/preloaded-play-key)]
          (connect-create-ws! pk)))
      (when game?
        (ws/make-websocket!
         (str protocol "//" (.-host js/location) "/ws/organism/play/" js/playKey)
         update-messages!))
      (mount-components))))
