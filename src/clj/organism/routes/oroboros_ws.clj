(ns organism.routes.oroboros-ws
  "WebSocket handler for oroboros game play.
   Same pattern as journey-ws but uses organism.oroboros.game."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [org.httpkit.server :as hk]
   [organism.oroboros.game :as game])
  (:import
   [java.io ByteArrayOutputStream]))

;; ── Transit helpers (shared with journey-ws) ────────────────────────────────────

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

;; ── Games atom ──────────────────────────────────────────────────────────────────
;; {:games {play-key → {:key play-key
;;                       :ruleset  ruleset-map
;;                       :board    board-data
;;                       :state    game-state
;;                       :channels #{channel ...}}}}

(defonce games (atom {:games {}}))

;; ── State broadcasting ──────────────────────────────────────────────────────────

(defn broadcast-state!
  ([play-key] (broadcast-state! play-key nil))
  ([play-key action-key]
   (let [game-data (get-in @games [:games play-key])
         state (:state game-data)
         ruleset (:ruleset game-data)]
     (when state
       (send-channels!
        (:channels game-data)
        (cond-> {"type" "game-state"
                 "state" (pr-str state)
                 "ruleset" (pr-str ruleset)}
          action-key (assoc "action" (pr-str action-key))))))))

;; ── Game management ─────────────────────────────────────────────────────────────

(defn empty-game [play-key channel]
  {:key play-key
   :ruleset nil
   :board nil
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

(declare compute-legal-actions)

;; ── AI agent ────────────────────────────────────────────────────────────────────

(defn ai-pick-action
  "Simple heuristic agent: prefer non-pass actions, random pick."
  [actions]
  (let [non-pass (remove #(= "pass" (first (first %))) actions)]
    (if (seq non-pass)
      (rand-nth (vec non-pass))
      (first actions))))

(defn play-ai-turns!
  "If the current player is not the human, auto-play AI turns until
   it's the human's turn again (or game over). Broadcasts after each move."
  [play-key human-player]
  (future
    (try
      (loop [n 200]
        (let [gd (get-in @games [:games play-key])
              st (:state gd)
              rs (:ruleset gd)
              bd (:board gd)]
          (when (and st rs (pos? n) (not (:winner st))
                     (not= (game/current-player st) human-player))
            (let [actions (compute-legal-actions rs bd st)]
              (when (seq actions)
                (let [[ak ns] (ai-pick-action (vec actions))]
                  (swap! games assoc-in [:games play-key :state] ns)
                  (log/info "AI move" play-key (game/current-player st) (pr-str ak))
                  (Thread/sleep 300)
                  (broadcast-state! play-key ak)
                  (recur (dec n))))))))
      (catch Exception e
        (log/error "AI agent error" play-key (.getMessage e))))))

;; ── Message handlers ────────────────────────────────────────────────────────────

(defn- create-game-dispatch
  "Create a game from a ruleset."
  [ruleset-map]
  (let [{:keys [topology state]} (game/create-game ruleset-map)]
    {:board topology :state state}))

(defn- compute-legal-actions [ruleset board state]
  (game/legal-actions ruleset board state))

(defn handle-create! [play-key message]
  (let [ruleset-raw (or (get message "ruleset") (get message :ruleset))]
    (when ruleset-raw
      (let [ruleset-map (if (string? ruleset-raw) (edn/read-string ruleset-raw) ruleset-raw)
            {:keys [board state]} (create-game-dispatch ruleset-map)]
        (swap! games
               (fn [gs]
                 (-> gs
                     (assoc-in [:games play-key :ruleset] ruleset-map)
                     (assoc-in [:games play-key :board] board)
                     (assoc-in [:games play-key :state] state)
                     (assoc-in [:games play-key :human-player] "0"))))
        (log/info "Created oroboros game" play-key)
        (broadcast-state! play-key)
        (play-ai-turns! play-key "0")))))

(defn handle-action! [play-key player-key message]
  (let [choice (or (get message "choice") (get message :choice))
        game-data (get-in @games [:games play-key])
        state (:state game-data)
        ruleset (:ruleset game-data)
        board (:board game-data)]
    (when (and choice state ruleset)
      (try
        (let [action-key (edn/read-string choice)
              actions (compute-legal-actions ruleset board state)
              next-state (get actions action-key)]
          (if next-state
            (do
              (swap! games assoc-in [:games play-key :state] next-state)
              (log/info "Universal action" play-key player-key (pr-str action-key))
              (broadcast-state! play-key action-key)
              ;; Trigger AI for non-human players
              (let [human (get-in @games [:games play-key :human-player] "0")]
                (play-ai-turns! play-key human)))
            (log/warn "Unknown action" play-key player-key (pr-str action-key))))
        (catch Exception e
          (log/error "Failed to apply oroboros action" play-key player-key
                     choice (.getMessage e)))))))

;; ── WebSocket lifecycle ─────────────────────────────────────────────────────────

(defn connect! [{:keys [play-key player]} channel]
  (let [game-data (find-game! play-key channel)]
    (log/info "Universal CONNECT" player play-key)
    (let [base-msg {"type" "initialize"
                    "key" play-key
                    "player" player}]
      (send! channel
             (if-let [state (:state game-data)]
               (assoc base-msg
                      "state" (pr-str state)
                      "ruleset" (pr-str (:ruleset game-data)))
               base-msg)))))

(defn disconnect! [{:keys [play-key player]} channel status]
  (log/info "Universal DISCONNECT" player status)
  (swap! games
         (fn [gs]
           (let [remaining (remove #{channel}
                                   (get-in gs [:games play-key :channels]))]
             (if (empty? remaining)
               (update-in gs [:games] dissoc play-key)
               (assoc-in gs [:games play-key :channels] (set remaining)))))))

(defn notify-clients! [{:keys [play-key player]} _channel raw]
  (let [message (read-json raw)
        ;; Transit may deliver keys as strings or keywords depending on format
        msg-type (or (get message "type") (get message :type))]
    (log/info "Universal MSG" msg-type player)
    (case msg-type
      "create" (handle-create! play-key message)
      "action" (handle-action! play-key player message)
      (log/warn "Unknown oroboros message type" msg-type))))

;; ── Route wiring ────────────────────────────────────────────────────────────────

(defn websocket-callbacks [player play-key]
  (let [cfg {:player player :play-key play-key}]
    {:on-open    (partial connect!        cfg)
     :on-close   (partial disconnect!     cfg)
     :on-receive (partial notify-clients! cfg)}))

(defn ws-handler [{:keys [path-params session] :as request}]
  (let [play   (:play path-params)
        player (or (:player session) "--observer--")]
    (hk/as-channel request (websocket-callbacks player play))))

(defn oroboros-ws-routes []
  [["/ws/oroboros/play/:play" ws-handler]])
