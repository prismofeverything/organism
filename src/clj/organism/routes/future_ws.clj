(ns organism.routes.future-ws
  "WebSocket handler for Future game play."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [org.httpkit.server :as hk]
   [future.game :as game])
  (:import
   [java.io ByteArrayOutputStream]))

;; ── Transit helpers ─────────────────────────────────────────────────────────

(defn- ->stream [input]
  (cond (string? input) (io/input-stream (.getBytes ^String input))
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
  (hk/send! channel (write-json message)))

(defn send-channels! [channels message]
  (doseq [ch channels]
    (send! ch message)))

;; ── Games atom ──────────────────────────────────────────────────────────────
;; {:games {play-key → {:key play-key
;;                       :state    game-state
;;                       :channels #{channel ...}}}}

(defonce games (atom {:games {}}))

;; ── State broadcasting ──────────────────────────────────────────────────────

(defn broadcast-state!
  ([play-key] (broadcast-state! play-key nil))
  ([play-key action-key]
   (let [game-data (get-in @games [:games play-key])
         state (:state game-data)]
     (when state
       (send-channels!
        (:channels game-data)
        (cond-> {"type" "game-state"
                 "state" (pr-str state)}
          action-key (assoc "action" (pr-str action-key))))))))

;; ── Game management ─────────────────────────────────────────────────────────

(defn empty-game [play-key channel]
  {:key play-key
   :state nil
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

;; ── Message handlers ────────────────────────────────────────────────────────

(defn handle-create! [play-key message]
  (let [players-raw (or (get message "players") (get message :players))
        players (if (string? players-raw) (edn/read-string players-raw) players-raw)]
    (when players
      (let [state (game/create-game players)]
        (swap! games assoc-in [:games play-key :state] state)
        (log/info "Created future game" play-key "with players" players)
        (broadcast-state! play-key)))))

(defn handle-action! [play-key player-key message]
  (let [choice (or (get message "choice") (get message :choice))
        game-data (get-in @games [:games play-key])
        state (:state game-data)]
    (when (and choice state)
      (try
        (let [action-key (if (string? choice) (edn/read-string choice) choice)
              actions (game/legal-actions state)
              next-state (get actions action-key)]
          (if next-state
            (do
              (swap! games assoc-in [:games play-key :state] next-state)
              (log/info "Future action" play-key player-key (pr-str action-key))
              (broadcast-state! play-key action-key))
            (log/warn "Unknown future action" play-key player-key (pr-str action-key))))
        (catch Exception e
          (log/error "Failed to apply future action" play-key player-key
                     choice (.getMessage e)))))))

;; ── WebSocket lifecycle ─────────────────────────────────────────────────────

(defn connect! [{:keys [play-key player]} channel]
  (let [game-data (find-game! play-key channel)]
    (log/info "Future CONNECT" player play-key)
    (let [base-msg {"type" "initialize"
                    "key" play-key
                    "player" player}]
      (send! channel
             (if-let [state (:state game-data)]
               (assoc base-msg "state" (pr-str state))
               base-msg)))))

(defn disconnect! [{:keys [play-key player]} channel status]
  (log/info "Future DISCONNECT" player status)
  (swap! games
         (fn [gs]
           (let [remaining (remove #{channel}
                                   (get-in gs [:games play-key :channels]))]
             (if (empty? remaining)
               (update-in gs [:games] dissoc play-key)
               (assoc-in gs [:games play-key :channels] (set remaining)))))))

(defn notify-clients! [{:keys [play-key player]} _channel raw]
  (let [message (read-json raw)
        msg-type (or (get message "type") (get message :type))]
    (log/info "Future MSG" msg-type player)
    (case msg-type
      "create" (handle-create! play-key message)
      "action" (handle-action! play-key player message)
      (log/warn "Unknown future message type" msg-type))))

;; ── Route wiring ────────────────────────────────────────────────────────────

(defn websocket-callbacks [player play-key]
  (let [cfg {:player player :play-key play-key}]
    {:on-open    (partial connect!        cfg)
     :on-close   (partial disconnect!     cfg)
     :on-receive (partial notify-clients! cfg)}))

(defn ws-handler [{:keys [path-params session] :as request}]
  (let [play   (:play path-params)
        player (or (:player session) "--observer--")]
    (hk/as-channel request (websocket-callbacks player play))))

(defn future-ws-routes []
  [["/ws/future/play/:play" ws-handler]])
