(ns organism.routes.journey-ws
  (:require
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [immutant.web.async :as async]
   [organism.persist :as persist])
  (:import
   [java.io ByteArrayOutputStream]))

(defn- ->stream [input]
  (cond (string? input) (io/input-stream (.getBytes input))
        :default input))

(defn read-json [input]
  (with-open [ins (->stream input)]
    (-> ins
        (transit/reader :json)
        transit/read)))

(defn write-json [output]
  (let [out (ByteArrayOutputStream. 4096)
        r (transit/writer out :json)
        _ (transit/write r output)
        ret (.toString out)]
    (.reset out)
    ret))

(defn send!
  [channel message]
  (async/send!
   channel
   (write-json message)))

(defonce games
  (atom {:games {}}))

(defn empty-game
  [play-key player channel]
  {:key play-key
   :chat []
   :history []
   :channels #{channel}})

(defn append-channel!
  [play-key channel]
  (swap!
   games
   update-in [:games play-key :channels]
   conj channel))

(defn load-game!
  [play-key player channel]
  (let [game-state (empty-game play-key player channel)]
    (swap! games assoc-in [:games play-key] game-state)
    game-state))

(defn find-game!
  [play-key player channel]
  (let [existing (get-in (deref games) [:games play-key])]
    (if (empty? existing)
      (load-game! play-key player channel)
      (do
        (append-channel! play-key channel)
        (update existing :channels conj channel)))))

(defn connect!
  [{:keys [db play-key player]} channel]
  (let [game-state (find-game! play-key player channel)]
    (log/info "CONNECTING" player play-key)
    (send!
     channel
     {:type "initialize"
      :key play-key
      :player player
      :history (:history game-state)
      :chat (:chat game-state)})))

(defn disconnect-game
  [play-key channel games]
  (let [games (update-in
               games [:games play-key :channels]
               #(remove #{channel} %))]
    (if (empty? (get-in games [:games play-key :channels]))
      (dissoc games play-key)
      games)))

(defn disconnect!
  [{:keys [db play-key player]} channel {:keys [code reason]}]
  (log/info "channel closed" player code reason)
  (swap!
   games
   (partial disconnect-game play-key channel)))

(defn send-channels!
  [channels message]
  (doseq [ch channels]
    (send! ch message)))

(defn timestamp
  []
  (quot (System/currentTimeMillis) 1000))

(defn update-chat
  [db player-key play-key channel {:keys [player message] :as received}]
  (let [chat-message
        {:type "chat"
         :player player
         :time (timestamp)
         :message message}
        _
        (swap!
         games
         update-in [:games play-key :chat]
         conj
         chat-message)
        channels (get-in @games [:games play-key :channels])]
    (doseq [ch channels]
      (send! ch chat-message))
    (persist/update-chat! db play-key chat-message)))

(defn notify-clients!
  [{:keys [db player play-key]} channel raw]
  (let [message (read-json raw)]
    (log/info "MESSAGE RECEIVED -" message)
    (condp = (:type message)
      "chat" (update-chat db player play-key channel message)
      (log/error "unknown message type!" (:type message)))))

(defn websocket-callbacks
  [db player play-key]
  (let [config {:db db :player player :play-key play-key}]
    {:on-open (partial connect! config)
     :on-close (partial disconnect! config)
     :on-message (partial notify-clients! config)}))

(defn ws-handler
  [db {:keys [path-params session] :as request}]
  (let [play (:play path-params)
        player (or (:player session) "--observer--")]
    (async/as-channel request (websocket-callbacks db player play))))

(defn journey-ws-routes
  [db]
  [["/ws/journey/play/:play" (partial ws-handler db)]])
