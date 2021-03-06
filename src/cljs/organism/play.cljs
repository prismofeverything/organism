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

(defn introduction-complete?
  [{:keys [progress]}]
  (and
   (= 3 (count (set (keys progress))))
   (= 3 (count (set (vals progress))))))

(defonce food-source
  (r/atom {}))

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
  (let [board (board/build-board 6 40 2.1 colors (:turn-order game) true)
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
  (-> game-state
      (assoc-in [:game :state] (:game message))
      (update :history conj (:game message))))

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

(defn introduce-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        highlight-color (board/brighten color 0.3)
        locations (:locations board)
        factor 0.93
        radius (* (:radius board) factor)
        element-radius (* (:radius board) 1)
        starting-spaces (get-in game [:players player :starting-spaces])
        {:keys [chosen-space chosen-element progress]} (deref introduction)
        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)]
             ^{:key space}
             [:circle {:cx x :cy y
                       :r radius
                       :stroke highlight-color
                       :stroke-width (* 0.19 radius)
                       :fill-opacity 0.04
                       :fill "white"
                       :on-click
                       (fn [event]
                         (println "choosing" space event)
                         (if chosen-element
                           (swap!
                            introduction
                            (fn [intro]
                              (-> intro
                                  (assoc :chosen-space nil)
                                  (assoc :chosen-element nil)
                                  (update :progress assoc chosen-element space))))
                           (swap! introduction assoc :chosen-space space)))}]))
         (remove
          (set (conj (vals progress) chosen-space))
          starting-spaces))
        highlights
        (if chosen-space
          (let [[x y] (get locations chosen-space)]
            (conj
             highlights
             ^{:key chosen-space}
             [:circle
              {:cx x :cy y
               :r radius
               :stroke (board/brighten color 0.5)
               :stroke-width (* 0.21 radius)
               :fill highlight-color
               :on-click
               (fn [event]
                 (swap! introduction dissoc :chosen-space))}]))
          highlights)
        elements
        (map
         (fn [[type space]]
           ^{:key space}
           (let [g (board/render-element
                    highlight-color "white"
                    (get locations space)
                    element-radius
                    {:type type :food 1})
                 _ (println "G before" g)
                 g (assoc-prop
                    g :on-click
                    (fn [event]
                      (println "choosing" space)
                      (if chosen-element
                        (swap!
                         introduction
                         (fn [intro]
                           (-> intro
                               (assoc :chosen-space nil)
                               (assoc :chosen-element nil)
                               (update :progress assoc chosen-element space))))
                        (swap! introduction assoc :chosen-space space))))]
             (println "G after" g)
             g))
         progress)]
    (println "highlights" highlights elements)
    ^{:key "highlights"}
    [:g (concat highlights elements)]))

(defn find-highlights
  [game board turn choices]
  (condp = turn
    :open []
    :introduce (introduce-highlights game board turn choices)))

(defn organism-board
  []
  (let [{:keys [game board turn choices]} @game-state
        svg (board/render-game board game)
        highlights (find-highlights game board turn choices)]
    (conj svg highlights)))

(defn organism-controls
  []
  (let [{:keys [game board turn choices] :as game-state} @game-state
        player-colors (:player-colors board)
        current-player (game/current-player game)
        current-color (get player-colors current-player)
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
        (for [[location type] element-controls]
          ^{:key type}
          (let [highlight-color (board/brighten current-color 0.3)

                type->location
                (into
                 {}
                 (map
                  (fn [[location type]]
                    [type location])
                  element-controls))

                _ (println "CONTROLS" turn progress type chosen-element chosen-space)
                color
                (cond
                  (and
                   (= turn :introduce)
                   (= chosen-element type))
                  (board/brighten current-color 0.6)

                  (and
                   (= turn :introduce)
                   (not (get progress type)))
                  highlight-color

                  :else current-color)]

            _ (println "COLORS" current-color highlight-color color)
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
                         (swap!
                          introduction
                          (fn [intro]
                            (-> intro
                                (dissoc :chosen-element)
                                (dissoc :chosen-space)
                                (update :progress (fn [pro] (assoc pro type chosen-space))))))
                         (swap! introduction assoc :chosen-element type)))))))))]
       [:h2
        [:span
         {:style
          {:color "hsl(0,50%,50%)"}
          :on-click
          (fn [event]
            (reset! introduction {:progress {}}))}
         "reset"]
        "  |  "
        (if (introduction-complete? introduce)
          [:span
           {:style
            {:color "hsl(100,50%,50%)"}
            :on-click
            (fn [event]
              (reset! introduction {:progress {}})
              (swap! game game/introduce current-player (assoc progress :organism 0)))}
           "confirm"]
          [:span
           {:style {:color "hsl(0, 10%, 80%)"}}
           "resolve"])]
       [:h2 (with-out-str (pprint (-> game :state :player-turn)))]])))

(defn game-page
  []
  [:main
   {:style
    {:flex-grow 1
     :display "flex"
     :flex-direction "row"}}
   [:aside
    {:style {:width "25%"}}
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
    {:style {:width "20%"}}
    [organism-controls]]])

(defn update-messages!
  [{:keys [type] :as received}]
  (println "MESSAGE RECEIVED" received)
  (condp = type
    "initialize"
    (do
      (swap! game-state initialize-game received)
      (swap! chat initialize-chat received))
    "game-state" (swap! game-state update-game received)
    "chat" (swap! chat update-chat received)))

(defn mount-components
  []
  (rdom/render [#'game-page] (.getElementById js/document "organism")))

(defn init!
  []
  (println "intializing game" js/gameKey)
  (ws/make-websocket!
   (str "ws://" (.-host js/location) "/ws/" js/gameKey)
   update-messages!)
  (mount-components))
