(ns organism.play
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.websockets :as ws]))

(defonce chat (r/atom []))
(defonce game-state (r/atom {:game {} :history []}))

(defn initialize-chat
  [chat message]
  (println "initializing chat" (:chat message))
  (:chat message))

(defn initialize-game
  [game-state message]
  (println "initializing game" (:game message))
  {:game (:game message)
   :history (:history message)})

(defn update-chat
  [chat message]
  (conj chat message))

(defn update-game
  [game-state message]
  (-> game-state
      (assoc-in [:game :state] (:game message))
      (update :history conj (:game message))))

(defn chat-list
  []
  [:ul
   (for [[i message] (map-indexed vector @chat)]
     ^{:key i}
     [:li (:player message) ":" (:message message)])])

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
             :player "orb"
             :message @value})
           (reset! value nil))}])))

(defn game-page
  []
  [:section.section
   [:div.container
    [:div.row
     [:div.col-md-12
      [:h2 "discussion"]]]
    [:div.row
     [:div.col-sm-6
      [chat-list]]]
    [:div.row
     [:div.col-sm-6
      [chat-input]]]]])

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
