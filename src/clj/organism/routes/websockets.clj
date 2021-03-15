(ns organism.routes.websockets
  (:require
   [clojure.pprint :refer (pprint)]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [immutant.web.async :as async]
   [organism.game :as game]
   [organism.board :as board]
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

(defrecord GameState [key invocation created game history chat channels])

(defn load-game
  [game-key channel]
  (let [game (examples/five-player-game)
        state (:state game)
        colors (board/generate-colors (:rings game))]
    (println "COLORS" colors)
    ;; GameState
    {:key game-key
     :invocation (board/empty-invocation)
     :created (System/currentTimeMillis)
     :game game
     :history [state]
     :chat []
     :channels #{channel}}))

(def player-cycle
  (atom (cycle board/default-player-order)))

(defn create-game
  [game-key channel]
  {:key game-key
   :invocation (board/empty-invocation)
   :chat []
   :channels #{channel}})

(defn append-channel!
  [game-key channel]
  (swap!
   games
   update-in [:games game-key :channels]
   conj channel))

(defn connect!
  [{:keys [game-key]} channel]
  (log/info "channel open")
  (let [existing (get-in (deref games) [:games game-key])]
    (cond

      (empty? existing)
      (let [game (create-game game-key channel)]
        (swap!
         games
         (fn [games]
           (assoc-in games [:games game-key] game)))
        (send!
         channel
         (-> game
             (select-keys [:key :invocation])
             (assoc :type "create"))))

      (-> existing :created empty?)
      (do
        (append-channel! game-key channel)
        (send!
         channel
         (-> existing
             (select-keys [:key :invocation])
             (assoc :type "create"))))

      :else
      (let [game-state (get-in (deref games) [:games game-key])
            player (first @player-cycle)]
        (append-channel! game-key channel)
        (swap! player-cycle rest)
        (send!
         channel
         {:type "initialize"
          :game (:game game-state)
          :player player
          :colors (:colors game-state)
          :history (:history game-state)
          :chat (:chat game-state)})))))

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

(defn send-channels!
  [channels message]
  (doseq [ch channels]
    (send! ch message)))

(defn update-create-game
  [game-key channel message]
  (swap!
   games
   assoc-in [:games game-key :invocation]
   (:invocation message))
  (send-channels!
   (get-in @games [:games game-key :channels])
   message))

(defn update-player-name
  [game-key channel {:keys [index player] :as message}]
  (swap!
   games
   update-in
   [:games game-key :invocation]
   (fn [invocation]
     (update invocation :players (fn [invoke] (assoc (vec invoke) index player)))))
  (send-channels!
   (get-in @games [:games game-key :channels])
   message))

(defn update-game-state
  [game-key channel {:keys [game complete] :as message}]
  (log/info "received game-state message" message)
  (swap!
   games
   update-in [:games game-key]
   (fn [game-state]
     (let [game-state (assoc-in game-state [:game :state] game)]
       (if complete
         (update game-state :history conj game)
         game-state))))
  (send-channels!
   (get-in @games [:games game-key :channels])
   message))

(defn walk-history
  [game-key channel message]
  (let [{:keys [game history channels]} (get-in (deref games) [:games game-key])
        present (last history)
        previous (last (butlast history))
        previous (if (empty? previous) present previous)
        already-here? (= present (:game message))]
    (if already-here?
      (swap!
       games
       (fn [games]
         (-> games
             (update-in [:games game-key :history] (comp vec butlast))
             (assoc-in [:games game-key :game :state] previous)))))
    (send-channels!
     channels
     {:type "game-state"
      :game
      (if already-here?
        previous
        present)})))

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
      "create" (update-create-game game-key channel message)
      "player-name" (update-player-name game-key channel message)
      "game-state" (update-game-state game-key channel message)
      "history" (walk-history game-key channel message)
      "chat" (update-chat game-key channel message)
      (log/error "unknown message!" message))))

(defn websocket-callbacks
  [game-key]
  (let [config {:game-key game-key}]
    {:on-open (partial connect! config)
     :on-close (partial disconnect! config)
     :on-message (partial notify-clients! config)}))

(defn ws-handler
  [{:keys [path-params] :as request}]
  (let [{:keys [game]} path-params]
    (async/as-channel request (websocket-callbacks game))))

(defn websocket-routes
  []
  [["/ws/:game" ws-handler]])
