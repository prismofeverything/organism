(ns organism.routes.journey-ws
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [immutant.web.async :as async]
   [journey.game :as game]
   [journey.choice :as choice]
   [organism.persist :as persist])
  (:import
   [java.io ByteArrayOutputStream]))

;; ── Transit helpers ───────────────────────────────────────────────────────────

(defn- ->stream [input]
  (cond (string? input) (io/input-stream (.getBytes input))
        :else input))

(defn read-json [input]
  (with-open [ins (->stream input)]
    (-> ins (transit/reader :json) transit/read)))

(defn write-json [output]
  (let [out (ByteArrayOutputStream. 4096)
        w   (transit/writer out :json)
        _   (transit/write w output)
        ret (.toString out)]
    (.reset out)
    ret))

(defn send! [channel message]
  (async/send! channel (write-json message)))

(defn send-channels! [channels message]
  (doseq [ch channels]
    (send! ch message)))

;; ── Games atom ────────────────────────────────────────────────────────────────
;; {:games {play-key → {:key      play-key
;;                       :state    game-state (or nil before creation)
;;                       :chat     [...]
;;                       :channels #{channel ...}}}}

(defonce games (atom {:games {}}))

;; ── State broadcasting ────────────────────────────────────────────────────────

(defn broadcast-state! [play-key]
  (let [game  (get-in @games [:games play-key])
        state (:state game)]
    (when state
      (send-channels!
       (:channels game)
       {:type  "game-state"
        :state (pr-str state)}))))

;; ── Game management ───────────────────────────────────────────────────────────

(defn empty-game [play-key channel]
  {:key      play-key
   :state    nil
   :chat     []
   :channels #{channel}})

(defn append-channel! [play-key channel]
  (swap! games update-in [:games play-key :channels] conj channel))

(defn load-game! [play-key channel]
  (let [g (empty-game play-key channel)]
    (swap! games assoc-in [:games play-key] g)
    g))

(defn find-game! [play-key channel]
  (let [existing (get-in @games [:games play-key])]
    (if (empty? existing)
      (load-game! play-key channel)
      (do (append-channel! play-key channel)
          (update existing :channels conj channel)))))

;; ── Message handlers ──────────────────────────────────────────────────────────

(defn handle-create! [play-key {:keys [players]}]
  (when (seq players)
    (let [state (game/initial-state (vec players))]
      (swap! games assoc-in [:games play-key :state] state)
      (log/info "Created journey game" play-key "players:" players)
      (broadcast-state! play-key))))

(defn handle-action! [play-key player-key {:keys [choice]}]
  (let [game-data (get-in @games [:games play-key])
        state     (:state game-data)]
    (when state
      (try
        (let [choice-key            (edn/read-string choice)
              [_phase choices-map]  (choice/find-state state)
              ;; Find the matching choice (keys can be positions, keywords, maps, etc.)
              next-state            (get choices-map choice-key)]
          (if next-state
            (do
              (swap! games assoc-in [:games play-key :state] next-state)
              (log/info "Action" play-key player-key (pr-str choice-key))
              (broadcast-state! play-key))
            (log/warn "Unknown choice key" play-key player-key (pr-str choice-key))))
        (catch Exception e
          (log/error "Failed to apply action" play-key player-key choice (.getMessage e)))))))

(defn handle-chat! [db play-key player-key {:keys [message]}]
  (let [msg {:type    "chat"
             :player  player-key
             :time    (quot (System/currentTimeMillis) 1000)
             :message message}]
    (swap! games update-in [:games play-key :chat] conj msg)
    (send-channels! (get-in @games [:games play-key :channels]) msg)
    (persist/update-chat! db play-key msg)))

;; ── WebSocket lifecycle ───────────────────────────────────────────────────────

(defn connect! [{:keys [play-key player]} channel]
  (let [game-data (find-game! play-key channel)]
    (log/info "Journey CONNECT" player play-key)
    (let [base-msg {:type    "initialize"
                    :key     play-key
                    :player  player
                    :chat    (:chat game-data)}]
      (send! channel
             (if-let [state (:state game-data)]
               (assoc base-msg :state (pr-str state))
               base-msg)))))

(defn disconnect! [{:keys [play-key player]} channel {:keys [code reason]}]
  (log/info "Journey DISCONNECT" player code reason)
  (swap! games
         (fn [gs]
           (let [remaining (remove #{channel}
                                   (get-in gs [:games play-key :channels]))]
             (if (empty? remaining)
               (update-in gs [:games] dissoc play-key)
               (assoc-in gs [:games play-key :channels] (set remaining)))))))

(defn notify-clients! [{:keys [db play-key player]} _channel raw]
  (let [{:keys [type] :as message} (read-json raw)]
    (log/info "Journey MSG" type player)
    (case type
      "create"  (handle-create! play-key message)
      "action"  (handle-action! play-key player message)
      "chat"    (handle-chat! db play-key player message)
      (log/warn "Unknown journey message type" type))))

;; ── Route wiring ─────────────────────────────────────────────────────────────

(defn websocket-callbacks [db player play-key]
  (let [cfg {:db db :player player :play-key play-key}]
    {:on-open    (partial connect!         cfg)
     :on-close   (partial disconnect!      cfg)
     :on-message (partial notify-clients!  cfg)}))

(defn ws-handler [db {:keys [path-params session] :as request}]
  (let [play   (:play path-params)
        player (or (:player session) "--observer--")]
    (async/as-channel request (websocket-callbacks db player play))))

(defn journey-ws-routes [db]
  [["/ws/journey/play/:play" (partial ws-handler db)]])
