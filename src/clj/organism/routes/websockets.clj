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

(defrecord GameState [key invocation created game chat history channels])

(def player-cycle
  (atom (cycle board/default-player-order)))

(defn empty-game
  [game-key channel]
  {:key game-key
   :invocation (board/empty-invocation)
   :created nil
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
    (println "LOAD GAME" game-state)
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
        (println "NOT EMPTY EXISTING" existing)
        (append-channel! game-key channel)
        (update existing :channels conj channel)))))

(defn connect!
  [{:keys [db game-key]} channel]
  (log/info "channel open")
  (let [game-state (find-game! db game-key channel)]
    (println "GAME STATE" game-state)
    (if (:created game-state)
      (let [player (first @player-cycle)]
        (swap! player-cycle rest)
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


    ;; (cond

    ;;   (empty? existing)
    ;;   (let [game (load-game db game-key channel)]
    ;;     (swap!
    ;;      games
    ;;      (fn [games]
    ;;        (assoc-in games [:games game-key] game)))
    ;;     (if (:created game)
    ;;       (do
    ;;         (println "FOUND GAME" game)
    ;;         )
    ;;       (do
    ;;         (println "NEW GAME" game)
    ;;         (send!
    ;;          channel
    ;;          (-> game
    ;;              (select-keys [:key :invocation])
    ;;              (assoc :type "create"))))))

    ;;   (-> existing :invocation :created nil?)
    ;;   (do
    ;;     (append-channel! game-key channel)
    ;;     (send!
    ;;      channel
    ;;      (-> existing
    ;;          (select-keys [:key :invocation])
    ;;          (assoc :type "create"))))

    ;;   :else
    ;;   (let [game-state (get-in (deref games) [:games game-key])
    ;;         player (first @player-cycle)]
    ;;     (append-channel! game-key channel)
    ;;     (swap! player-cycle rest)
    ;;     (send!
    ;;      channel
    ;;      {:type "initialize"
    ;;       :invocation (:invocation game-state)
    ;;       :game (:game game-state)
    ;;       :player player
    ;;       :history (:history game-state)
    ;;       :chat (:chat game-state)})))

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
  [db game-key channel message]
  (swap!
   games
   assoc-in [:games game-key :invocation]
   (:invocation message))
  (send-channels!
   (get-in @games [:games game-key :channels])
   message))

(defn update-player-name
  [db game-key channel {:keys [index player] :as message}]
  (swap!
   games
   update-in
   [:games game-key :invocation]
   (fn [invocation]
     (update invocation :players (fn [invoke] (assoc (vec invoke) index player)))))
  (send-channels!
   (get-in @games [:games game-key :channels])
   message))

(defn complete-game-state
  [{:keys [invocation game channels history chat] :as game-state}]
  (let [{:keys [ring-count player-count players colors organism-victory]} invocation
        symmetry (board/player-symmetry player-count)
        starting (board/starting-spaces ring-count player-count players board/total-rings)
        notches? (board/cut-notches? ring-count player-count)
        rings (vec (take ring-count board/total-rings))
        create (game/create-game symmetry rings starting organism-victory notches?)
        created (System/currentTimeMillis)]
    (-> game-state
        (assoc-in [:invocation :created] created)
        (assoc :game create))))

(defn trigger-creation
  [db game-key channel message]
  (let [;; {:keys [invocation game channels history chat] :as game-state}
        ;; {:keys [ring-count player-count players colors organism-victory]} invocation
        ;; symmetry (board/player-symmetry player-count)
        ;; starting (board/starting-spaces ring-count player-count players board/total-rings)
        ;; notches? (board/cut-notches? ring-count player-count)
        ;; rings (vec (take ring-count board/total-rings))
        ;; create (game/create-game symmetry rings starting organism-victory notches?)
        ;; created (System/currentTimeMillis)

        ;; game-state
        ;; (-> game-state
        ;;     (assoc-in [:invocation :created] created)
        ;;     (assoc :game create))

        game-state (get-in @games [:games game-key])
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
      :player (-> invocation :players first)
      :history history
      :chat chat})
    (persist/create-game! db (dissoc game-state :channels))))

(defn update-game-state
  [db game-key channel {:keys [game complete] :as message}]
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
   message)
  (persist/update-state! db game-key game))

(defn walk-history
  [db game-key channel message]
  (let [{:keys [game history channels]} (get-in (deref games) [:games game-key])
        present (last history)
        previous (last (butlast history))
        previous (if (empty? previous) present previous)
        already-here? (= present (:game message))]
    (send-channels!
     channels
     {:type "game-state"
      :game
      (if already-here?
        previous
        present)})
    (if already-here?
      (do
        (swap!
         games
         (fn [games]
           (-> games
               (update-in [:games game-key :history] (comp vec butlast))
               (assoc-in [:games game-key :game :state] previous))))
        (persist/reset-state! db game-key)))))

(defn timestamp
  []
  (quot (System/currentTimeMillis) 1000))

(defn update-chat
  [db game-key channel {:keys [player message] :as received}]
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
  [{:keys [db game-key]} channel raw]
  (let [message (read-json raw)]
    (log/info "MESSAGE RECEIVED -" message)
    (condp = (:type message)
      "create" (update-create-game db game-key channel message)
      "trigger-creation" (trigger-creation db game-key channel message)
      "player-name" (update-player-name db game-key channel message)
      "game-state" (update-game-state db game-key channel message)
      "history" (walk-history db game-key channel message)
      "chat" (update-chat db game-key channel message)
      (log/error "unknown message!" message))))

(defn websocket-callbacks
  [db game-key]
  (let [config {:db db :game-key game-key}]
    {:on-open (partial connect! config)
     :on-close (partial disconnect! config)
     :on-message (partial notify-clients! config)}))

(defn ws-handler
  [db {:keys [path-params] :as request}]
  (let [{:keys [game]} path-params]
    (async/as-channel request (websocket-callbacks db game))))

(defn websocket-routes
  [db]
  [["/ws/:game" (partial ws-handler db)]])
