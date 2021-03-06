(ns organism.play
  (:require
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
    :history []
    :board {}
    :turn :open
    :choices []}))

(defonce introduction
  (r/atom
   {:chosen-space [:G 2]
    :chosen-element nil
    :progress {:eat [:G 4]}}))

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
  [game-state {:keys [game history colors board] :as message}]
  (let [board (board/build-board 6 40 2.1 colors (:turn-order game) true)
        [turn choices] (choice/find-state game)]
    (println "initializing game" game)
    (println "initializing board" board)
    (println "turn" turn)
    (println "choices" (count choices))
    {:game game
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
             :player (game/current-player (:game @game-state))
             :message @value})
           (reset! value nil))}])))

(defn find-highlights
  [game board turn choices]
  (condp = turn
    :open []
    :introduce
    (let [player (game/current-player game)
          color (get-in board [:player-colors player])
          highlight-color (board/brighten color 0.3)
          locations (:locations board)
          factor 0.93
          radius (* (:radius board) factor)
          element-radius (* (:radius board) 1)
          starting-spaces (get-in game [:players player :starting-spaces])
          {:keys [chosen-space progress]} (deref introduction)
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
                           (swap! introduction assoc :chosen-space space))}]))
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
                 :fill highlight-color}]))
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
                        (swap! introduction assoc :chosen-space space)))]
               (println "G after" g)
               g))
           progress)]
      (println "highlights" highlights elements)
      ^{:key "highlights"}
      [:g (concat highlights elements)])))

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
        element-radius (* (:radius board) 1)]
    (println "current color" player-colors)
    (if current-player
      [:div
       [:h1
        {:style {:color current-color}}
        "> " current-player " - " (name turn)]
       [:svg
        {:width 200 :height 180}
        (for [[location type]
              (map
               vector
               [[50 50] [150 50] [100 130]]
               [:eat :grow :move])]
          ^{:key type}
          (assoc-prop
           (board/render-element
            current-color current-color
            location
            element-radius
            {:type type :food 0})
           :id (str (name type) "-control")))]])))

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
