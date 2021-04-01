(ns organism.routes.websockets
  (:require
   [clojure.pprint :refer (pprint)]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [immutant.web.async :as async]
   [organism.game :as game]
   [organism.board :as board]
   [organism.persist :as persist]
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

(defrecord GameState [key invocation game chat history channels])

(def player-cycle
  (atom (cycle board/default-player-order)))

(defn empty-game
  [game-key channel]
  {:key game-key
   :invocation (board/empty-invocation)
   :game nil
   :chat []
   :history []
   :channels #{channel}})

(defn append-channel!
  [game-key channel]
  (swap!
   games
   update-in [:games game-key :channels]
   conj channel))

(defn load-game
  [db game-key channel]
  (if-let [game-state (persist/load-game db game-key)]
    (assoc game-state :channels #{channel})
    (empty-game game-key channel)))

(defn load-game!
  [db game-key channel]
  (let [game-state (load-game db game-key channel)]
    (swap!
     games
     assoc-in [:games game-key]
     game-state)
    game-state))

(defn find-game!
  [db game-key channel]
  (let [existing (get-in (deref games) [:games game-key])]
    (if (empty? existing)
      (load-game! db game-key channel)
      (do
        (append-channel! game-key channel)
        (update existing :channels conj channel)))))

(defn connect!
  [{:keys [db player game-key]} channel]
  (let [game-state (find-game! db game-key channel)]
    (if (get-in game-state [:invocation :created])
      (do
        (println "CONNECTING" player game-key (get-in game-state [:game :state]))
        (send!
         channel
         {:type "initialize"
          :invocation (:invocation game-state)
          :game (:game game-state)
          :player player
          :history (:history game-state)
          :chat (:chat game-state)}))
      (send!
       channel
       (-> game-state
           (select-keys [:key :invocation])
           (assoc :type "create"))))))

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
  (log/info "channel closed" player code reason)
  (swap!
   games
   (partial disconnect-game game-key channel)))

(defn send-channels!
  [channels message]
  (doseq [ch channels]
    (send! ch message)))

(defn update-create-game
  [db player game-key channel message]
  (swap!
   games
   assoc-in [:games game-key :invocation]
   (:invocation message))
  (send-channels!
   (get-in @games [:games game-key :channels])
   message))

(defn update-player-name
  [db page-player game-key channel {:keys [index player] :as message}]
  (swap!
   games
   update-in
   [:games game-key :invocation]
   (fn [invocation]
     (let [previous-name (nth (:players invocation) index)]
       (-> invocation
           (update
            :players
            (fn [invoke]
              (assoc (vec invoke) index player)))))))
           ;; (update
           ;;  :player-captures
           ;;  (fn [organisms]
           ;;    (-> organisms
           ;;        (assoc player (get organisms previous-name))
           ;;        (dissoc previous-name))))

  (println "player name updated" player "invocation" (-> @games :games (get game-key) :invocation))
  (send-channels!
   (get-in @games [:games game-key :channels])
   message))

(defn complete-game-state
  [{:keys [invocation game channels history chat] :as game-state}]
  (let [{:keys [ring-count player-count players colors organism-victory player-captures]} invocation
        symmetry (board/player-symmetry player-count)
        starting (board/starting-spaces ring-count player-count players board/total-rings)
        player-info (game/initial-players starting player-captures)
        notches? (board/cut-notches? ring-count player-count)
        rings (vec (take ring-count board/total-rings))
        create (game/create-game symmetry rings player-info organism-victory notches?)
        created (System/currentTimeMillis)]
    (-> game-state
        (assoc-in [:invocation :created] created)
        (assoc :game create))))

(defn trigger-creation
  [db player game-key channel message]
  (let [game-state (get-in @games [:games game-key])
        {:keys [invocation game channels history chat] :as game-state}
        (complete-game-state game-state)]
    (swap!
     games
     assoc-in
     [:games game-key]
     game-state)
    (send-channels!
     channels
     {:type "initialize"
      :invocation invocation
      :game game
      :history history
      :chat chat})
    (persist/create-game! db (dissoc game-state :channels))))

(defn update-game-state
  [db player game-key channel {:keys [game complete] :as message}]
  (let [game-state (get-in @games [:games game-key])
        current-player (game/current-player (:game game-state))
        invocation (:invocation game-state)]
    (when (= player current-player)
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
       message)
      (persist/update-state! db game-key game)
      (if (:winner game)
        (persist/complete-game! db game-key game)
        (let [next-player (-> game :player-turn :player)]
          (when-not (= current-player next-player)
            (persist/update-player-games!
             db game-key
             (:players invocation)
             game)))))))

(defn walk-history
  [db player game-key channel message]
  (let [{:keys [game history channels invocation]} (get-in (deref games) [:games game-key])
        present (last history)
        previous (last (butlast history))
        previous (if (empty? previous) present previous)
        already-here? (= present (:game message))]
    (when (= player (get-in present [:player-turn :player]))
      (send-channels!
       channels
       {:type "game-state"
        :game
        (if already-here?
          previous
          present)})
      (when already-here?
        (swap!
         games
         (fn [games]
           (-> games
               (update-in [:games game-key :history] (comp vec butlast))
               (assoc-in [:games game-key :game :state] previous))))
        (persist/reset-state! db game-key)
        (when-not (= player (-> previous :player-turn :player))
          (persist/update-player-games!
           db game-key
           (:players invocation)
           previous))))))

(defn timestamp
  []
  (quot (System/currentTimeMillis) 1000))

(defn update-chat
  [db player-key game-key channel {:keys [player message] :as received}]
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
      (send! ch chat-message))
    (persist/update-chat! db game-key chat-message)))

(defn notify-clients!
  [{:keys [db player game-key]} channel raw]
  (let [message (read-json raw)]
    (log/info "MESSAGE RECEIVED -" message)
    (condp = (:type message)
      "create" (update-create-game db player game-key channel message)
      "trigger-creation" (trigger-creation db player game-key channel message)
      "player-name" (update-player-name db player game-key channel message)
      "game-state" (update-game-state db player game-key channel message)
      "history" (walk-history db player game-key channel message)
      "chat" (update-chat db player game-key channel message)
      (log/error "unknown message type!" (:type message)))))

(defn websocket-callbacks
  [db player game-key]
  (let [config {:db db :player player :game-key game-key}]
    {:on-open (partial connect! config)
     :on-close (partial disconnect! config)
     :on-message (partial notify-clients! config)}))

(defn ws-handler
  [db {:keys [path-params] :as request}]
  (let [{:keys [player game]} path-params]
    (async/as-channel request (websocket-callbacks db player game))))

(defn websocket-routes
  [db]
  [["/ws/player/:player/game/:game" (partial ws-handler db)]])
