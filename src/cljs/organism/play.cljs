(ns organism.play
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.websockets :as ws]))

(defonce chat (r/atom []))
(defonce game (r/atom {}))

(defn chat-list
  []
  [:ul
   (for [[i message] (map-indexed vector @chat)]
     ^{:key i}
     [:li message])])

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
  [received]
  (let [{:keys [type message]} (:message received)]
    (condp = type
      "chat" (swap! chat conj message)
      "game-state" (swap! game update :state message))))

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
