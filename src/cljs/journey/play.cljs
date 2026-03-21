(ns journey.play
  (:require
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.ajax :as ajax]
   [organism.websockets :as ws]))

;;; --- State ---

(defonce game-state
  (r/atom {:created false
           :player js/playerKey
           :history []}))

(defonce player-preferences
  (r/atom {}))

(defonce observe-games
  (r/atom []))

(defonce create-play-key
  (r/atom ""))

;;; --- Pages ---

(defn create-page
  []
  [:div {:style {:color "#fff" :padding "40px"}}
   [:h1 "journey - create"]])

(defn play-page
  []
  [:div {:style {:color "#fff" :padding "40px"}}
   [:h1 "journey"]])

(defn observe-page
  []
  [:div {:style {:color "#fff" :padding "40px"}}
   [:h1 "journey - observe"]])

(defn page-container
  []
  (cond
    js/isObserve [observe-page]
    js/isCreate  [create-page]
    js/playKey   [play-page]
    :else        [:div]))

;;; --- WebSocket messages ---

(defn update-messages!
  [{:keys [type] :as received}]
  (println "MESSAGE RECEIVED" received))

;;; --- Init ---

(defn mount-components
  []
  (rdom/render [#'page-container] (.getElementById js/document "journey")))

(defn init!
  []
  (ajax/load-interceptors!)
  (when js/playerPreferences
    (reset! player-preferences
            (reader/read-string js/playerPreferences)))
  (when js/isObserve
    (when js/observeGames
      (reset! observe-games (reader/read-string js/observeGames))))
  (when js/playKey
    (let [protocol (if (= (.-protocol js/location) "https:") "wss:" "ws:")]
      (ws/make-websocket!
       (str protocol "//" (.-host js/location) "/ws/journey/play/" js/playKey)
       update-messages!)))
  (mount-components))
