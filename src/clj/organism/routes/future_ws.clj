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
;;                       :history  [prior-state ...]    ; for undo
;;                       :bots     #{bot-name ...}
;;                       :players  [player-name ...]
;;                       :channels #{channel ...}}}}

(defonce games (atom {:games {}}))

(def ^:private history-cap 500)

(defn- push-history [game-data]
  (let [h (or (:history game-data) [])
        h' (conj h (:state game-data))]
    (assoc game-data :history
           (if (> (count h') history-cap)
             (vec (drop (- (count h') history-cap) h'))
             h'))))

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
                 ;; Strip :board — the topology is static, and it's ~72%
                 ;; of the wire payload. Client reconstructs from
                 ;; board.cljc on receive.
                 "state" (pr-str (dissoc state :board))
                 "undo-depth" (str (count (:history game-data)))}
          action-key (assoc "action" (pr-str action-key))))))))

;; ── Bot policy ──────────────────────────────────────────────────────────────

(def ^:private end-choices
  #{[:done-moving] [:done-activating] [:done-linking] [:done-activating-space]
    [:decline-bonus] [:no-activation-possible]})

(defn- planet-on? [[ak _]]
  (and (vector? ak) (= :planet-on (first ak))))

(defn- planet-off? [[ak _]]
  (and (vector? ak) (= :planet-off (first ak))))

(defn- bot-pick
  "Bot policy:
     1. If any :planet-on is available → take it (auto-board)
     2. Otherwise from non-end choices (:done-* etc), minus :planet-off
        (bots don't disembark once boarded)
     3. Fall back to end choices only when nothing else remains."
  [actions]
  (let [entries (vec actions)
        boards  (filterv planet-on? entries)]
    (cond
      (seq boards)
      (rand-nth boards)

      :else
      (let [non-end (filterv (fn [e]
                               (and (not (contains? end-choices (first e)))
                                    (not (planet-off? e))))
                             entries)
            pool (if (seq non-end)
                   non-end
                   (or (seq (filterv (fn [e] (not (contains? end-choices (first e)))) entries))
                       entries))]
        (rand-nth pool)))))

(def ^:private bot-delay-ms 250)
(def ^:private bot-broadcast-throttle-ms 250)

(defn run-bot-turns!
  "Spawn a future that auto-plays bot turns for play-key until a human's turn
   (or game over).

   Broadcasting is throttled: we ship a state broadcast at most every
   `bot-broadcast-throttle-ms` OR whenever the current-player changes
   (turn boundary). Between broadcasts we still apply actions to the
   authoritative state, so undo history is complete."
  [play-key]
  (future
    (try
      (loop [last-broadcast-ms (- (System/currentTimeMillis)
                                  bot-broadcast-throttle-ms)
             last-player       nil
             last-action       nil]
        (let [game-data (get-in @games [:games play-key])
              state     (:state game-data)
              bots      (:bots game-data)
              cur       (game/current-player state)]
          (cond
            (or (nil? state) (nil? cur) (not (contains? bots cur)))
            (when last-action
              (broadcast-state! play-key last-action))

            :else
            (do
              (Thread/sleep bot-delay-ms)
              (let [current-state (:state (get-in @games [:games play-key]))
                    cur2          (game/current-player current-state)]
                (if (and current-state cur2 (contains? bots cur2))
                  (let [actions (game/legal-actions current-state)]
                    (if (seq actions)
                      (let [[ak thunk] (bot-pick actions)
                            next-state (game/force-choice thunk)
                            _ (swap! games update-in [:games play-key]
                                     (fn [g] (-> g push-history (assoc :state next-state))))
                            _ (log/info "Future bot action" play-key cur2 (pr-str ak))
                            now (System/currentTimeMillis)
                            next-cur (game/current-player next-state)
                            turn-boundary? (and last-player (not= last-player next-cur))
                            throttle-elapsed? (>= (- now last-broadcast-ms)
                                                  bot-broadcast-throttle-ms)]
                        (if (or turn-boundary? throttle-elapsed?)
                          (do (broadcast-state! play-key ak)
                              (recur now next-cur nil))
                          (recur last-broadcast-ms next-cur ak)))
                      (when last-action
                        (broadcast-state! play-key last-action))))
                  (when last-action
                    (broadcast-state! play-key last-action))))))))
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

(defn- apply-choice-chain
  "Apply a sequence of choice-keys to a state, checking legality at each
   step. Returns [ok? final-state applied-keys]. If any step's action is
   not legal, returns [false state applied-so-far]."
  [state chain]
  (loop [s state
         remaining chain
         applied []]
    (if (empty? remaining)
      [true s applied]
      (let [ck (first remaining)
            actions (game/legal-actions s)
            nxt (game/next-state actions ck)]
        (if nxt
          (recur nxt (rest remaining) (conj applied ck))
          [false s applied])))))

(defn handle-action! [play-key player-key message]
  (let [choice (or (get message "choice") (get message :choice))
        chain-raw (or (get message "chain") (get message :chain))
        game-data (get-in @games [:games play-key])
        state (:state game-data)
        bots  (:bots game-data)]
    (when (and state (or choice chain-raw))
      (try
        (let [chain (cond
                      chain-raw
                      (if (string? chain-raw) (edn/read-string chain-raw) chain-raw)
                      :else
                      [(if (string? choice) (edn/read-string choice) choice)])
              [ok? next-state applied] (apply-choice-chain state chain)]
          (if (and ok? (seq applied))
            (do
              (swap! games update-in [:games play-key]
                     (fn [g] (-> g push-history (assoc :state next-state))))
              (log/info "Future action" play-key player-key (pr-str applied))
              (broadcast-state! play-key (last applied))
              (when (contains? bots (game/current-player next-state))
                (run-bot-turns! play-key)))
            (log/warn "Unknown/illegal future action" play-key player-key
                      (pr-str chain) "applied so far:" (pr-str applied))))
        (catch Exception e
          (log/error "Failed to apply future action" play-key player-key
                     choice (.getMessage e)))))))

(defn handle-undo! [play-key player-key]
  (let [game-data (get-in @games [:games play-key])
        history (:history game-data)]
    (if (seq history)
      (let [prev (peek history)]
        (swap! games update-in [:games play-key]
               (fn [g] (-> g
                           (assoc :state prev)
                           (update :history pop))))
        (log/info "Future undo" play-key player-key)
        (broadcast-state! play-key [:undo]))
      (log/info "Future undo — history empty" play-key player-key))))

;; ── WebSocket lifecycle ─────────────────────────────────────────────────────

(defn connect! [{:keys [play-key player]} channel]
  (let [game-data (find-game! play-key channel)]
    (log/info "Future CONNECT" player play-key)
    (let [base-msg {"type" "initialize"
                    "key" play-key
                    "player" player}]
      (send! channel
             (if-let [state (:state game-data)]
               (assoc base-msg
                      "state" (pr-str (dissoc state :board))
                      "undo-depth" (str (count (:history game-data))))
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
      "undo"   (handle-undo! play-key player)
      (log/warn "Unknown future message type" msg-type))))

;; ── Route wiring ────────────────────────────────────────────────────────────

(defn websocket-callbacks [player play-key]
  ;; Var refs (#') let wrap-reload's namespace refresh reach the WS
  ;; message handler. Without them, the callback would capture the
  ;; original function value and never see reloaded code.
  (gws/make-callbacks {:player player :play-key play-key}
                      {:on-open    #'connect!
                       :on-close   #'disconnect!
                       :on-receive #'notify-clients!}))

(defn ws-handler [{:keys [path-params session] :as request}]
  (let [play   (:play path-params)
        player (or (:player session) "--observer--")]
    (hk/as-channel request (websocket-callbacks player play))))

(defn future-ws-routes []
  [["/ws/future/play/:play" ws-handler]])
