(ns organism.play
  (:require
   [clojure.string :as string]
   [cljs.pprint :refer (pprint)]
   [goog.events :as events]
   [goog.history.EventType :as HistoryEventType]
   [reitit.core :as reitit]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.base :as base]
   [organism.game :as game]
   [organism.choice :as choice]
   [organism.board :as board]
   [organism.ajax :as ajax]
   [organism.websockets :as ws])
  (:import goog.History))

(defonce session (r/atom {:page :home}))
(defonce chat (r/atom []))

(defonce player-order
  (r/atom board/default-player-order))

(defonce board-invocation
  (r/atom (board/empty-invocation)))

(defonce game-state
  (r/atom
   {:game {}
    :created false
    :player js/playerKey
    :history []
    :board {}
    :turn :open
    :choices []}))

(defonce introduction
  (r/atom
   {:chosen-space nil
    :chosen-element nil
    :progress {}}))

(defonce food-source
  (r/atom {}))

(def max-players 7)

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

(defn initialize-chat
  [chat message]
  (println "initializing chat" (:chat message))
  (:chat message))

(defn initialize-game
  [game-state {:keys [game invocation player history board] :as message}]
  (let [{:keys [ring-count player-count players colors]} invocation
        board (board/generate-board
               colors
               players
               (take ring-count board/total-rings))
        [turn choices] (choice/find-state game)]
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
     :choices choices}))

(defn update-chat
  [chat message]
  (conj chat message))

(defn update-game
  [game-state message]
  (let [game-state (assoc-in game-state [:game :state] (:game message))
        [turn choices] (choice/find-state (:game game-state))]
    (-> game-state
        (update :history conj (:game message))
        (assoc :turn turn)
        (assoc :choices choices))))

(defn player-list
  []
  (let [{:keys [game board] :as game-state} @game-state
        {:keys [state turn-order]} game
        {:keys [player-colors]} board]
    [:div
     [:h3 "score"]
     [:ul
      (for [player turn-order]
        ^{:key player}
        [:li
         {:style {:color (get player-colors player)}}
         player " - " (count (get-in state [:captures player]))])]]))

(defn chat-list
  []
  [:ul
   (let [player-colors (get-in @game-state [:board :player-colors])]
     (for [[i message] (map-indexed vector @chat)]
       (let [player (:player message)
             color (get player-colors player)]
         ^{:key i}
         [:li
          {:style {:color color}}
          player ": " (:message message)])))])

(defn chat-input
  []
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
           (reset! value nil))}])))

(defn highlight-circle
  [x y radius color on-click]
  (let [highlight-color (board/brighten color 0.3)]
    [:circle
     {:cx x :cy y
      :r radius
      :stroke highlight-color
      :stroke-width (* 0.19 radius)
      :fill-opacity 0.04
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
  [type x y radius color on-click]
  (let [g (board/render-element
           (board/brighten color 0.1) "white"
           [x y]
           radius
           {:type type :food 1})]
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

(defn send-choice!
  [choices match complete]
  (let [choice (get-in choices [match :state])]
    (if choice
      (send-state! choice complete)
      (println "NO CHOICE MATCHING" match (keys choices)))))

(defn send-introduction!
  [choices {:keys [progress] :as intro}]
  (if (introduction-complete? intro)
    (send-choice!
     choices
     (assoc progress :organism 0)
     true)))

(defn send-create!
  [invocation]
  (ws/send-transit-message!
   {:type "create"
    :invocation invocation}))

(def highlight-factor 0.93)

(defn create-highlights
  [game board turn choices]
  (let [players (:players game)
        colors (:player-colors board)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        highlights
        (base/map-cat
         (fn [[player {:keys [starting-spaces]}]]
           (map
            (fn [space]
              (let [[x y] (get locations space)
                    color (get colors player)]
                (highlight-circle
                 x y radius color
                 (fn [event]))))
            starting-spaces))
         players)]
    (into [] (concat [:g] highlights))))

(defn introduce-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        food-color (-> board :colors first last)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        element-radius (* (:radius board) 1)
        starting-spaces (get-in game [:players player :starting-spaces])
        {:keys [chosen-space chosen-element progress]} (deref introduction)

        ;; unchosen starting spaces
        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-circle
              x y radius color
              (fn [event]
                (if chosen-element
                  (do
                    (swap!
                     introduction
                     (fn [intro]
                       (-> intro
                           (assoc :chosen-space nil)
                           (assoc :chosen-element nil)
                           (update :progress assoc chosen-element space))))
                    (send-introduction! choices @introduction))
                  (swap! introduction assoc :chosen-space space))))))
         (remove
          (set (conj (vals progress) chosen-space))
          starting-spaces))

        ;; chosen space without element
        highlights
        (if chosen-space
          (let [[x y] (get locations chosen-space)]
            (conj
             highlights
             ^{:key chosen-space}
             (focus-circle
              x y radius color
              (fn [event]
                (swap! introduction dissoc :chosen-space)))))
          highlights)

        ;; elements placed so far
        elements
        (map
         (fn [[type space]]
           (let [[x y] (get locations space)]
             ^{:key space}
             (render-element
              type x y element-radius color food-color
              (fn [event]
                (if chosen-element
                  (do
                    (swap!
                     introduction
                     (fn [intro]
                       (-> intro
                           (assoc :chosen-space nil)
                           (assoc :chosen-element nil)
                           (update :progress assoc chosen-element space))))
                    (send-introduction! choices @introduction))
                  (swap! introduction assoc :chosen-space space))))))
         progress)]
    ^{:key "highlights"}
    [:g (concat highlights elements)]))

(defn choose-organism-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        organisms (game/player-organisms game player)
        available (keys choices)
        elements (base/map-cat organisms available)

        highlights
        (mapv
         (fn [{:keys [space organism] :as element}]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-circle
              x y radius color
              (fn [event]
                (send-choice! choices organism true)))))
         elements)]

    ^{:key "highlights"}
    (into [] (concat [:g] highlights))))

(defn choose-space-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        spaces (keys choices)

        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 next-state (get-in choices [space :state])]
             ^{:key space}
             (highlight-circle
              x y radius color
              (fn [event]
                (send-choice! choices space true)))))
         spaces)]

    ^{:key "highlights"}
    (into [] (concat [:g] highlights))))

(defn choose-target-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        highlight-color (board/brighten color 0.2)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        element-radius (* (:radius board) 1.0)
        choose-from (game/get-action-field game :from)
        spaces (keys choices)

        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 next-state (get-in choices [space :state])]
             ^{:key space}
             (highlight-circle
              x y radius color
              (fn [event]
                (send-choice! choices space true)))))
         spaces)

        highlights
        (let [[x y] (get locations choose-from)
              type (-> game (game/get-element choose-from) :type)]
          (conj
           highlights
           ^{:key choose-from}
           (highlight-element
            type x y element-radius highlight-color
            (fn [event]
              (send-reset! (:state game))))))]

    ^{:key "highlights"}
    (into [] (concat [:g] highlights))))

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

        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-element
              :grow x y radius color
              (fn [event]
                (choose-food-source! space)
                (let [source @food-source]
                  (if (get choices source)
                    (send-choice! choices source true)))))))
         spaces)]

    ^{:key "highlights"}
    (into [] (concat [:g] highlights))))

(defn find-highlights
  [game board turn choices]
  (condp = turn
    :open []
    :create (create-highlights game board turn choices)
    :introduce (introduce-highlights game board turn choices)
    :choose-organism (choose-organism-highlights game board turn choices)
    :eat-to (choose-space-highlights game board turn choices)
    :circulate-from (choose-space-highlights game board turn choices)
    :circulate-to (choose-target-highlights game board turn choices)
    :grow-from (grow-from-highlights game board turn choices)
    :grow-to (choose-space-highlights game board turn choices)
    :move-from (choose-space-highlights game board turn choices)
    :move-to (choose-target-highlights game board turn choices)
    []))

(defn resolve-action
  []
  [:span
   {:style {:color "hsl(0, 10%, 80%)"}}
   "continue"])

(defn organism-board
  []
  (let [{:keys [game board turn choices]} @game-state
        svg (board/render-game board game)
        highlights (find-highlights game board turn choices)]
    (if (empty? highlights)
      svg
      (conj svg highlights))))

(defn generate-game-state
  [{:keys [ring-count player-count players colors] :as invocation}]
  (let [symmetry (board/player-symmetry player-count)
        rings (take ring-count board/total-rings)
        starting (board/find-starting-spaces symmetry rings players)
        game-players (game/initial-players starting)
        game {:players game-players}
        board
        (board/generate-board
         colors
         players
         rings)]
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
    (reset!
     game-state
     generated)))

(defn organism-controls
  []
  (let [{:keys [game board turn choices history]} @game-state
        player-turn (game/get-player-turn game)
        organism-turn (game/get-organism-turn game)
        action-type (:choice organism-turn)
        current-action (last (:actions organism-turn))

        player-colors (:player-colors board)
        current-player (game/current-player game)
        current-color (get player-colors current-player)
        dormant-color (board/brighten current-color -0.7)
        focus-color (board/brighten current-color 0.4)

        element-radius 45
        element-controls
        (map
         vector
         [[50 50] [150 50] [100 130]]
         [:eat :grow :move])
        {:keys [chosen-space chosen-element progress] :as introduce} @introduction]

    (if current-player
      [:div
       [:h1
        {:style
         {:color current-color}}
        current-player]
       [:h3
        {:style {:color current-color}}
        "phase - " (name turn)]
       [:svg
        {:width 200 :height 180}

        ;; ELEMENT CONTROLS
        (for [[location type] element-controls]
          ^{:key type}
          (let [type->location
                (into
                 {}
                 (map
                  (fn [[location type]]
                    [type location])
                  element-controls))

                element-state
                (cond
                  (or
                   (and
                    (= turn :introduce)
                    (= chosen-element type))
                   (and
                    (= turn :grow-element)
                    (get choices type))
                   (and
                    (= turn :choose-action)
                    (let [organism-turn (game/get-organism-turn game)]
                      (= type (:choice organism-turn)))))
                  :focus

                  (or
                   (and
                    (= turn :choose-action)
                    (let [organism-turn (game/get-organism-turn game)]
                      (not= type (:choice organism-turn))))
                   (and
                    (= turn :grow-element)
                    (not (get choices type)))
                   (and
                    (= turn :introduce)
                    (get progress type)))
                  :dormant

                  :else :neutral)
                
                color
                (condp = element-state
                  :focus focus-color
                  :dormant dormant-color
                  :neutral current-color)]

            (-> (board/render-element
                 color color
                 location
                 element-radius
                 {:type type :food 0})
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
                                  (update :progress (fn [pro] (assoc pro type chosen-space))))))
                           (send-introduction! choices @introduction))
                         (swap! introduction assoc :chosen-element type)))
                     :choose-action-type
                     (send-choice! choices type true)
                     :choose-action
                     (let [organism-turn (game/get-organism-turn game)]
                       (if (= type (:choice organism-turn))
                         (send-choice! choices type true)))
                     :grow-element
                     (if-let [choice (get choices type)]
                       (send-state! (:state choice) true))))))))]

       ;; CIRCULATE
       [:h2
        {:style
         {:color
          (cond
            (and
             (= turn :choose-action)
             (:circulate choices))
            focus-color
            :else dormant-color)}
         :on-click
         (condp = turn
           :choose-action
           (fn [event]
             (if (:circulate choices)
               (send-choice! choices :circulate true)))
           (fn [event]))}
        "circulate"]

       ;; RESET
       [:h3
        [:span
         {:style
          {:color "hsl(0,50%,50%)"}
          :on-click
          (fn [event]
            (send-reset! (:state game)))}
         "reset"]
        " | "

        ;; CONFIRM
        (condp = turn
          :pass
          [:span
           {:style
            {:color "hsl(100,50%,50%)"}
            :on-click
            (fn [event]
              (send-state! (get-in choices [:pass :state]) true))}
           "pass"]
          :actions-complete
          [:span
           {:style
            {:color "hsl(100,50%,50%)"}
            :on-click
            (fn [event]
              (send-state! (get-in choices [:advance :state]) true))}
           "resolve conflicts"]
          :resolve-conflicts
          [:span
           {:style
            {:color "hsl(100,50%,50%)"}
            :on-click
            (fn [event]
              (send-state! (get-in choices [:advance :state]) true))}
           "check integrity"]
          :check-integrity
          [:span
           {:style
            {:color "hsl(100,50%,50%)"}
            :on-click
            (fn [event]
              (send-state! (get-in choices [:advance :state]) true))}
           "confirm turn"]
          [:div])]

       (if (and
            organism-turn
            (nil? (:advance player-turn))
            (not= turn :actions-complete))
         [:h3
          {:style {:color current-color}}
          "organism - " (:organism organism-turn)
          (if-let [choice (:choice organism-turn)]
            [:span
             [:br] "element action - " choice
             [:br] "total actions - " (:num-actions organism-turn)])
          (if (not (empty? (:actions organism-turn)))
            [:span
             [:br] "current action - " (count (:actions organism-turn))])
          (if (not= turn :choose-action)
            [:span
             [:br] "action choice - " (:type (last (:actions organism-turn)))])])])))

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
   (flex-direction "column")
   [:header
    [:h1 "ORGANISM"]]
   inner
   [:footer
    [:h3 "organism"]]])

(defn ring-count-input
  []
  (let [invocation @board-invocation]
    [:div
     [:h3 "ring count"]
     [:select
      {:name "ring-count"
       :value (:ring-count invocation)
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
       (range 3 14))]]))

(defn player-count-input
  []
  (let [invocation @board-invocation]
    [:div
     [:h3 "player count"]
     [:select
      {:name "player-count"
       :value (:player-count invocation)
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)
               order @player-order
               colors (board/generate-colors-buffer
                       board/total-rings
                       (:ring-count invocation)
                       max-players)]
           (-> invocation
               (assoc :colors colors)
               (assoc :player-count value)
               (assoc :players (vec (take value order)))
               send-create!)))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 1 8))]]))

(defn organism-victory-input
  []
  (let [invocation @board-invocation]
    [:div
     [:h3 "organisms for victory"]
     [:select
      {:name "organism-victory"
       :value (:organism-victory invocation)
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)
               order @player-order
               rings (take (:ring-count invocation) board/total-rings)]
           (-> invocation
               (assoc :organism-victory value)
               (send-create!))))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 3 9))]]))

(defn send-player-name!
  [index player-name]
  (ws/send-transit-message!
   {:type "player-name"
    :index index
    :player player-name}))

(defn players-input
  []
  (let [{:keys [player-count colors]} @board-invocation
        ;; order @player-order
        player-colors (get-in @game-state [:board :player-colors])]
    [:div
     (map-indexed
      (fn [index [player color]]
        ^{:key index}
        [:div
         [:h3 "player " (inc index)]
         [:input
          {:value player
           :style
           {:border-radius "50px"
            :color "#fff"
            :background color
            :border "3px solid"
            :font-size "2em"
            :letter-spacing "8px"
            :margin "0px 0px"
            :padding "10px 40px"}
           :on-change
           (fn [event]
             (let [value (-> event .-target .-value)]
               (send-player-name! index value)))}]])
      player-colors)]))

(defn create-button
  [color]
  [:input
   {:type :button
    :value "CREATE"
    :style
    {:border-radius "50px"
     :color "#fff"
     :cursor "pointer"
     :background color
     :border "3px solid"
     :font-size "2em"
     :letter-spacing "8px"
     :margin "20px 0px"
     :padding "25px 60px"}
    :on-click
     (fn [event]
       (ws/send-transit-message!
        {:type "trigger-creation"}))}])

(defn create-page
  []
  (let [invocation @board-invocation]
    (game-layout
     [:main
      (flex-grow "row" 1)
      [:aside
       {:style {:width "30%"}}
       [:div
        [player-list]]
       [:div
        [:h3 "discuss"]
        [chat-list]
        [chat-input]]]
      [:article
       {:style {:flex-grow 1}}
       [organism-board]]
      (println "INVOCATION" invocation)
      [:nav
       {:style {:width "30%"}}
       [:div
        [create-button (-> invocation :colors rest first last)]]
       [:form
        [ring-count-input]
        [player-count-input]
        [organism-victory-input]
        [players-input]]]])))

(defn game-page
  []
  (game-layout
   [:main
    (flex-grow "row" 1)
    [:aside
     {:style {:width "30%"}}
     [:div
      [player-list]]
     [:div
      [:h3 "discuss"]
      [chat-list]
      [chat-input]]]
    [:article
     {:style {:flex-grow 1}}
     [organism-board]]
    [:nav
     {:style {:width "30%"}}
     [organism-controls]]]))

(defn page-container
  []
  (let [invocation @board-invocation]
    (if (:created invocation)
      [game-page]
      [create-page])))

(defn update-messages!
  [{:keys [type] :as received}]
  (println "MESSAGE RECEIVED" received)
  (condp = type
    "initialize"
    (do
      (swap! game-state initialize-game received)
      (reset! board-invocation (:invocation received))
      (swap! chat initialize-chat received))
    "create"
    (do
      (reset! board-invocation (:invocation received))
      (apply-invocation! @board-invocation))
    "player-name"
    (let [{:keys [index player]} received]
      (swap! player-order assoc index player)
      (swap! board-invocation update :players (fn [players] (assoc (vec players) index player))))
    "game-state"
    (do
      (swap! game-state update-game received)
      (swap!
       introduction
       (fn [introduction]
         (-> introduction
             (assoc :progress (-> received :game :state :player-turn :introduction))
             (assoc :chosen-element nil)
             (assoc :chosen-space nil))))
      (reset! food-source {}))
    "chat" (swap! chat update-chat received)))

;; -------------------------
;; Routes

(def router
  (reitit/router
    [["/" :home]
     ["/about" :about]
     ["/player/:player"]
     ["/player/:player/game/:game"]]))

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

(defn init!
  []
  (println "intializing game" js/gameKey)
  (ajax/load-interceptors!)
  (hook-browser-navigation!)
  (apply-invocation! @board-invocation)
  (let [protocol
        (if (= (.-protocol js/location) "https:")
          "wss:"
          "ws:")]
    (ws/make-websocket!
     (str protocol "//" (.-host js/location) "/ws/player/" js/playerKey "/game/" js/gameKey)
     update-messages!)
    (mount-components)))
