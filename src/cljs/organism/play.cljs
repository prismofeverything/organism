(ns organism.play
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.board :as board]
   [organism.websockets :as ws]))

(defonce chat
  (r/atom []))

(defonce game-state
  (r/atom
   {:game {}
    :player "NULL"
    :history []
    :board {}}))

(defn initialize-chat
  [chat message]
  (println "initializing chat" (:chat message))
  (:chat message))

(defn initialize-game
  [game-state {:keys [game player history colors board] :as message}]
  (let [board (board/build-board 6 35 2.1 colors (:turn-order game) true)]
    (println "initializing game" game)
    (println "initializing board" board)
    (println "player" player)
    {:game game
     :player player
     :history history
     :board board}))

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
    [:ul
     (for [player turn-order]
       ^{:key player}
       [:li
        {:style {:color (get player-colors player)}}
        player " - " (count (get-in state [:captures player]))])]))

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
             :player (get @game-state :player)
             :message @value})
           (reset! value nil))}])))

(defn organism-board
  []
  (let [{:keys [game board]} @game-state]
    (board/render-game board game)))

(defn game-page
  []
  [:div
   [:section.section
    [:div.container
     [player-list]]]
   [:section.section
    [:div.container
     [organism-board]]
    [:div.container
     [:div.row
      [:div.col-md-12
       [:h2 "discussion"]]]
     [:div.row
      [:div.col-sm-6
       [chat-list]]]
     [:div.row
      [:div.col-sm-6
       [chat-input]]]]]])

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
  (rdom/render [#'game-page] (.getElementById js/document "app")))

(defn init!
  []
  (println "intializing game" js/gameKey)
  (ws/make-websocket!
   (str "ws://" (.-host js/location) "/ws/" js/gameKey)
   update-messages!)
  (mount-components))
