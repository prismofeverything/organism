(ns organism.routes.future-ws
  "WebSocket handler for Future game play."
  (:require
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [org.httpkit.server :as hk]
   [organism.game-ws :as gws :refer [read-json send! send-channels!]]
   [future.game :as game]))

;; ── Games atom ──────────────────────────────────────────────────────────────
;; {:games {play-key → {:key play-key
;;                       :state    game-state
;;                       :bots     #{bot-name ...}
;;                       :players  [player-name ...]
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

;; ── Bot policy ──────────────────────────────────────────────────────────────

(def ^:private end-choices
  #{[:done-moving] [:done-activating] [:skip-links]})

(defn- bot-pick
  "Random legal action, with a mild preference for non-terminal choices —
   mirrors the client-side bot in future/play.cljs."
  [actions]
  (let [entries (vec actions)
        non-end (filterv (fn [[ak _]] (not (contains? end-choices ak))) entries)
        pool    (if (seq non-end) non-end entries)]
    (rand-nth pool)))

(def ^:private bot-delay-ms 500)

(defn run-bot-turns!
  "Spawn a future that auto-plays bot turns for play-key until a human's turn
   (or game over)."
  [play-key]
  (future
    (try
      (loop []
        (let [game-data (get-in @games [:games play-key])
              state     (:state game-data)
              bots      (:bots game-data)
              cur       (game/current-player state)]
          (when (and state cur (contains? bots cur))
            (Thread/sleep bot-delay-ms)
            (let [current-state (:state (get-in @games [:games play-key]))
                  cur2          (game/current-player current-state)]
              (when (and current-state cur2 (contains? bots cur2))
                (let [actions (game/legal-actions current-state)]
                  (when (seq actions)
                    (let [[ak next-state] (bot-pick actions)]
                      (swap! games assoc-in [:games play-key :state] next-state)
                      (log/info "Future bot action" play-key cur2 (pr-str ak))
                      (broadcast-state! play-key ak)
                      (recur)))))))))
      (catch Exception e
        (log/error "Future bot loop error" play-key (.getMessage e))))))

;; ── Game management ─────────────────────────────────────────────────────────

(defn empty-game [play-key channel]
  {:key play-key
   :state nil
   :bots #{}
   :players []
   :channels #{channel}})

(defn find-game! [play-key channel]
  (gws/find-game! games play-key channel empty-game))

;; ── Message handlers ────────────────────────────────────────────────────────

(defn handle-create! [play-key message]
  (let [players-raw (or (get message "players") (get message :players))
        players     (if (string? players-raw) (edn/read-string players-raw) players-raw)
        bots-raw    (or (get message "bots") (get message :bots))
        bots        (if (string? bots-raw) (edn/read-string bots-raw) bots-raw)
        bot-set     (set (or bots []))]
    (when players
      (let [state (game/create-game players)]
        (swap! games update-in [:games play-key]
               (fn [g] (-> (or g {:key play-key :channels #{}})
                           (assoc :state state
                                  :bots bot-set
                                  :players (vec players)))))
        (log/info "Created future game" play-key
                  "players:" players "bots:" bots)
        (broadcast-state! play-key)
        (when (contains? bot-set (game/current-player state))
          (run-bot-turns! play-key))))))

(defn handle-action! [play-key player-key message]
  (let [choice (or (get message "choice") (get message :choice))
        game-data (get-in @games [:games play-key])
        state (:state game-data)
        bots  (:bots game-data)]
    (when (and choice state)
      (try
        (let [action-key (if (string? choice) (edn/read-string choice) choice)
              actions (game/legal-actions state)
              next-state (get actions action-key)]
          (if next-state
            (do
              (swap! games assoc-in [:games play-key :state] next-state)
              (log/info "Future action" play-key player-key (pr-str action-key))
              (broadcast-state! play-key action-key)
              (when (contains? bots (game/current-player next-state))
                (run-bot-turns! play-key)))
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
  (gws/remove-channel! games play-key channel))

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
  (gws/make-callbacks {:player player :play-key play-key}
                      {:on-open connect! :on-close disconnect! :on-receive notify-clients!}))

(defn ws-handler [{:keys [path-params session] :as request}]
  (let [play   (:play path-params)
        player (or (:player session) "--observer--")]
    (hk/as-channel request (websocket-callbacks player play))))

(defn future-ws-routes []
  [["/ws/future/play/:play" ws-handler]])
