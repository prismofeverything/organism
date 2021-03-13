(ns organism.play
  (:require
   [cljs.pprint :refer (pprint)]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.game :as game]
   [organism.choice :as choice]
   [organism.board :as board]
   [organism.websockets :as ws]))

(defonce chat
  (r/atom []))

(defonce game-state
  (r/atom
   {:game {}
    :player nil
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
  [game-state {:keys [game player history colors board] :as message}]
  (let [board (board/build-board 5 40 2.1 colors (:turn-order game) true)
        [turn choices] (choice/find-state game)]
    (println "initializing game" game)
    (println "initializing board" board)
    (println "turn" turn)
    (println "choices" (count choices))
    {:game game
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
     [:h2 "score"]
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
       ^{:key i}
       (let [player (:player message)
             color (get player-colors player)]
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
             :player (:player @game-state)
             :message @value})
           (reset! value nil))}])))

(defn highlight-circle
  [x y radius color on-click]
  (let [highlight-color (board/brighten color 0.2)]
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
    :stroke (board/brighten color 0.3)
    :stroke-width (* 0.21 radius)
    :fill (board/brighten color 0.2)
    :on-click on-click}])

(defn highlight-element
  [type x y radius color on-click]
  (let [g
        (board/render-element
         (board/brighten color 0.2) "white"
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

(defn introduce-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        highlight-color (board/brighten color 0.2)
        locations (:locations board)
        factor 0.93
        radius (* (:radius board) factor)
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
             (highlight-element
              type x y element-radius color
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

(defn choose-space-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        factor 0.93
        radius (* (:radius board) factor)
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
        highlight-color (board/brighten color 0.3)
        locations (:locations board)
        factor 0.93
        radius (* (:radius board) factor)
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
            type x y element-radius color
            (fn [event]
              (send-reset! (:state game))))))]

    ^{:key "highlights"}
    (into [] (concat [:g] highlights))))

(defn grow-from-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        factor 0.93
        radius (* (:radius board) factor)

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
    :introduce (introduce-highlights game board turn choices)
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

(defn organism-controls
  []
  (let [{:keys [game board turn choices history]} @game-state
        player-colors (:player-colors board)
        current-player (game/current-player game)
        current-color (get player-colors current-player)
        highlight-color (board/brighten current-color 0.2)
        focus-color (board/brighten current-color 0.4)

        element-radius (* (:radius board) 1)
        element-controls
        (map
         vector
         [[50 50] [150 50] [100 130]]
         [:eat :grow :move])
        {:keys [chosen-space chosen-element progress] :as introduce} @introduction]

    (if current-player
      [:div
       [:h1
        {:style {:color current-color}}
        "> " current-player " - " (name turn)]
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
                   (= turn :choose-action-type)
                   (and
                    (= turn :choose-action)
                    (let [organism-turn (game/get-organism-turn game)]
                      (= type (:choice organism-turn))))
                   (and
                    (= turn :grow-element)
                    (get choices type)))
                  :focus

                  (and
                   (= turn :introduce)
                   (not (get progress type)))
                  :highlight

                  :else :neutral)
                
                color
                (condp = element-state
                  :focus focus-color
                  :highlight highlight-color
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
       [:h1
        {:style
         {:color
          (cond
            (and
             (= turn :choose-action)
             (:circulate choices))
            focus-color
            :else current-color)}
         :on-click
         (condp = turn
           :choose-action
           (fn [event]
             (if (:circulate choices)
               (send-choice! choices :circulate true)))
           (fn [event]))}
        "circulate"]

       ;; RESET
       [:h2
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
          [resolve-action])]

       [:h2 (with-out-str (pprint (-> game :state :player-turn)))]])))

(defn game-page
  []
  [:div
   {:style
    {:display "flex"
     :flex-direction "column"}}
   [:header
    [:h1 "ORGANISM"]]
   [:main
    {:style
     {:flex-grow 1
      :display "flex"
      :flex-direction "row"}}
    [:aside
     {:style {:width "30%"}}
     [:div
      [player-list]]
     [:div
      [:h2 "discuss"]
      [chat-list]
      [chat-input]]]
    [:article
     {:style {:flex-grow 1}}
     [organism-board]]
    [:nav
     {:style {:width "30%"}}
     [organism-controls]]]
   [:footer
    [:h2 "organism"]]])

(defn update-messages!
  [{:keys [type] :as received}]
  (println "MESSAGE RECEIVED" received)
  (condp = type
    "initialize"
    (do
      (swap! game-state initialize-game received)
      (swap! chat initialize-chat received))
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

(defn mount-components
  []
  (println "MOUNTING")
  (rdom/render [#'game-page] (.getElementById js/document "organism")))

(defn init!
  []
  (println "intializing game" js/gameKey)
  (let [protocol
        (if (= (.-protocol js/location) "https")
          "wss:"
          "ws:")]
    (ws/make-websocket!
     (str protocol "//" (.-host js/location) "/ws/" js/gameKey)
     update-messages!)
    (mount-components)))


