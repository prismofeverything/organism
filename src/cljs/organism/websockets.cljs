(ns organism.websockets
  (:require
   [cognitect.transit :as t]))

(defonce ws-channel (atom nil))
(def json-reader (t/reader :json))
(def json-writer (t/writer :json))

(defn receive-transit-message!
  [update-fn]
  (fn [message]
    (->> message .-data (t/read json-reader))))

(defn send-transit-message!
  [message]
  (if @ws-channel
    (.send @ws-channel (t/write json-writer message))
    (throw (js/Error. "websocket not available"))))

(defn make-websocket!
  [url receive-handler]
  (println "connecting to websocket")
  (if-let [channel (js/WebSocket. url)]
    (do
      (set! (.-onmessage channel) (receive-transit-message! receive-handler))
      (reset! ws-channel channel)
      (println "websocket connection established with " url))
    (throw (js/Error. "websocket connection FAILED with " url))))
