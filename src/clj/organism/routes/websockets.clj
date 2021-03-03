(ns organism.routes.websockets
  (:require
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [immutant.web.async :as async]
   [organism.game :as game]
   [organism.examples :as examples])
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

(defrecord GameState [key game current working history chat channels])

(defn load-game
  [game-key channel]
  (let [game (examples/six-player-game)
        state (:state game)]
    (GameState.
     game-key
     game
     state
     state
     [state]
     []
     #{channel})))

(defn connect!
  [{:keys [game-key player]} channel]
  (log/info "channel open")
  (let [existing (get-in (deref games) [:games game-key])]
    (if (empty? existing)
      (swap!
       games
       (fn [games]
         (assoc-in
          games [:games game-key]
          (load-game game-key channel))))
      (swap!
       games
       update-in [:games game-key :channels]
       conj channel))
    (let [game-state (get-in (deref games) [:games game-key])]
      (send!
       channel
       {:type "initialize"
        :game (:game game-state)
        :history (:history game-state)
        :chat (:chat game-state)}))))

(defn disconnect-game
  [game-key channel games]
  (let [games (update-in
               games [:games game-key :channels]
               #(remove #{channel} %))]
    (if (empty? (get-in games [:games game-key :channels]))
      (dissoc games game-key)
      games)))

(defn disconnect!
  [{:keys [game-key player]} channel {:keys [code reason]}]
  (log/info "channel closed" code reason)
  (swap!
   games
   (partial disconnect-game game-key channel)))

(defn update-game-state
  [game-key channel {:keys [game complete] :as message}]
  (log/info "received game-state message" message)
  (swap!
   games
   update-in [:games game-key]
   (fn [game-state]
     (let [game-state (assoc game-state :working game)]
       (if complete
         (-> game-state
             (update :current game)
             (update :history conj game))
         game-state))))
  (doseq [ch (get-in @games [:games game-key :channels])]
    (send! ch message)))

(defn timestamp
  []
  (quot (System/currentTimeMillis) 1000))

(defn update-chat
  [game-key channel {:keys [player message] :as received}]
  (log/info "received chat message" received)
  (let [chat-message
        {:type "chat"
         :player player
         :time (timestamp)
         :message message}
        _
        (swap!
         games
         update-in [:games game-key :chat]
         conj
         chat-message)
        channels (get-in @games [:games game-key :channels])]
    (doseq [ch channels]
      (log/info "sending" chat-message)
      (send! ch chat-message))))

(defn notify-clients!
  [{:keys [game-key player]} channel raw]
  (let [message (read-json raw)]
    (log/info "MESSAGE RECEIVED -" message)
    (condp = (:type message)
      "game-state" (update-game-state game-key channel message)
      "chat" (update-chat game-key channel message)
      (log/error "unknown message!" message))))

(defn websocket-callbacks
  [game-key]
  (let [player "orb"
        config {:game-key game-key :player player}]
    {:on-open (partial connect! config)
     :on-close (partial disconnect! config)
     :on-message (partial notify-clients! config)}))

(defn ws-handler
  [{:keys [path-params] :as request}]
  (let [{:keys [game]} path-params]
    (log/info (with-out-str (clojure.pprint/pprint request)))
    (async/as-channel request (websocket-callbacks game))))

(defn websocket-routes
  []
  [["/ws/:game" ws-handler]])
