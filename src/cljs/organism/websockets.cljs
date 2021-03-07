(ns organism.websockets
  (:require
   [cognitect.transit :as t]))

(defonce ws-channel (atom nil))
(def json-reader (t/reader :json))
(def json-writer (t/writer :json))

(defn receive-transit-message!
  [update-fn]
  (fn [raw]
    (let [message (->> raw .-data (t/read json-reader))]
      (update-fn message))))

(defn send-transit-message!
  [message]
  (if @ws-channel
    (try
      (.send @ws-channel (t/write json-writer message))
      (catch js/Object e
        (println "could not write: " message)))
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
