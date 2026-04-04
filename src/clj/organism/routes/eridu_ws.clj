(ns organism.routes.eridu-ws
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [immutant.web.async :as async]
   [eridu.game :as game]
   [eridu.choice :as choice]
   [organism.persist :as persist]
   [organism.persist-eridu :as persist-e])
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

(defonce games (atom {:games {}}))

;; ── Helpers ─────────────────────────────────────────────────────────────────

(defn- choice-player
  "The player who must make the current choice."
  [state]
  (game/current-player state))

;; ── Persistence helper ──────────────────────────────────────────────────────

(defn- save-state!
  ([db play-key] (save-state! db play-key nil))
  ([db play-key choice-key]
   (let [game (get-in @games [:games play-key])]
     (when (:state game)
       (persist-e/save-game!
        db play-key (:state game) (:bots game)
        (or (:players game) (:turn-order (:state game)))
        (:initial-state game))
       (when choice-key
         (persist-e/append-action! db play-key choice-key))))))

;; ── State broadcasting ────────────────────────────────────────────────────────

(defn broadcast-state! [play-key]
  (let [game  (get-in @games [:games play-key])
        state (:state game)]
    (when state
      (let [[phase choices] (choice/find-state-raw state)]
        (send-channels!
         (:channels game)
         {:type     "game-state"
          :state    (pr-str state)
          :phase    (str phase)
          :choices  (pr-str (keys choices))
          :bots     (vec (:bots game))
          :can-undo (boolean (seq (:history game)))})))))

;; ── Game management ───────────────────────────────────────────────────────────

(defn empty-game [play-key channel]
  {:key      play-key
   :state    nil
   :history  []
   :bots     #{}
   :players  []
   :chat     []
   :channels #{channel}})

(defn append-channel! [play-key channel]
  (swap! games update-in [:games play-key :channels] conj channel))

(defn load-game! [db play-key channel]
  (if-let [saved (persist-e/load-game db play-key)]
    (let [g {:key           play-key
             :state         (:state saved)
             :initial-state (:initial-state saved)
             :history       []
             :bots          (set (:bots saved))
             :players       (:players saved)
             :saved-history (:history saved)
             :chat          []
             :channels      #{channel}}]
      (swap! games assoc-in [:games play-key] g)
      g)
    (let [g (empty-game play-key channel)]
      (swap! games assoc-in [:games play-key] g)
      g)))

(defn find-game! [db play-key channel]
  (let [existing (get-in @games [:games play-key])]
    (if (empty? existing)
      (load-game! db play-key channel)
      (do (append-channel! play-key channel)
          (update existing :channels conj channel)))))

;; ── Message handlers ──────────────────────────────────────────────────────────

(defn handle-create! [db play-key {:keys [players bots]}]
  (when (seq players)
    (let [state    (game/initial-state (vec players))
          bot-set  (set (or bots []))]
      (swap! games
             (fn [gs]
               (-> gs
                   (assoc-in [:games play-key :state] state)
                   (assoc-in [:games play-key :initial-state] state)
                   (assoc-in [:games play-key :history] [])
                   (assoc-in [:games play-key :bots] bot-set)
                   (assoc-in [:games play-key :players] (vec players)))))
      (log/info "Created eridu game" play-key "players:" players "bots:" bots)
      (save-state! db play-key)
      (broadcast-state! play-key))))

(defn handle-action! [db play-key player-key {:keys [choice]}]
  (let [game-data (get-in @games [:games play-key])
        state     (:state game-data)]
    (when state
      (try
        (let [choice-key         (edn/read-string choice)
              [_phase choices-map] (choice/find-state-raw state)
              next-state          (get choices-map choice-key)]
          (if next-state
            (let [protected-phases #{:choose-action :choose-space-action
                                     :choose-deploy-city :choose-travel-destination
                                     :choose-build-city :choose-influence-role
                                     :choose-temple-city :game-over}
                  effective (loop [s next-state]
                              (let [p  (game/current-phase s)
                                    cs (second (choice/find-state-raw s))]
                                (if (and (= 1 (count cs))
                                         (not (contains? protected-phases p)))
                                  (recur (first (vals cs)))
                                  s)))
                  old-player    (choice-player state)
                  new-player    (choice-player effective)
                  turn-changed? (not= old-player new-player)]
              (swap! games
                     (fn [gs]
                       (-> gs
                           (assoc-in [:games play-key :state] effective)
                           (assoc-in [:games play-key :history]
                                     (if turn-changed?
                                       []
                                       (conj (:history (get-in gs [:games play-key])) state))))))
              (log/info "Action" play-key player-key (pr-str choice-key))
              (broadcast-state! play-key)
              (save-state! db play-key choice-key))
            (log/warn "Unknown choice key" play-key player-key (pr-str choice-key))))
        (catch Exception e
          (log/error "Failed to apply action" play-key player-key choice (.getMessage e)))))))

(defn handle-undo! [db play-key player-key]
  (let [game-data (get-in @games [:games play-key])
        history   (:history game-data)]
    (when (seq history)
      (let [prev-state (peek history)]
        (swap! games
               (fn [gs]
                 (-> gs
                     (assoc-in [:games play-key :state] prev-state)
                     (update-in [:games play-key :history] pop))))
        (log/info "Undo" play-key player-key)
        (broadcast-state! play-key)
        (save-state! db play-key)))))

(defn handle-chat! [db play-key player-key {:keys [message]}]
  (let [msg {:type    "chat"
             :player  player-key
             :time    (quot (System/currentTimeMillis) 1000)
             :message message}]
    (swap! games update-in [:games play-key :chat] conj msg)
    (send-channels! (get-in @games [:games play-key :channels]) msg)
    (persist/update-chat! db play-key msg)))

;; ── WebSocket lifecycle ───────────────────────────────────────────────────────

(defn connect! [{:keys [db play-key player]} channel]
  (let [game-data (find-game! db play-key channel)]
    (log/info "Eridu CONNECT" player play-key)
    (let [base-msg {:type    "initialize"
                    :key     play-key
                    :player  player
                    :bots    (vec (:bots game-data))
                    :chat    (:chat game-data)
                    :history (vec (map #(dissoc % :state) (or (:saved-history game-data) [])))}]
      (send! channel
             (if-let [state (:state game-data)]
               (let [[phase choices] (choice/find-state-raw state)]
                 (assoc base-msg
                        :state (pr-str state)
                        :phase (str phase)
                        :choices (pr-str (keys choices))
                        :can-undo (boolean (seq (:history game-data)))))
               base-msg)))))

(defn disconnect! [{:keys [play-key player]} channel {:keys [code reason]}]
  (log/info "Eridu DISCONNECT" player code reason)
  (swap! games
         (fn [gs]
           (let [remaining (remove #{channel}
                                   (get-in gs [:games play-key :channels]))]
             (if (empty? remaining)
               (update-in gs [:games] dissoc play-key)
               (assoc-in gs [:games play-key :channels] (set remaining)))))))

(defn notify-clients! [{:keys [db play-key player]} channel raw]
  (let [{:keys [type] :as message} (read-json raw)]
    (log/info "Eridu MSG" type player)
    (case type
      "create" (handle-create! db play-key message)
      "action" (handle-action! db play-key player message)
      "undo"   (handle-undo! db play-key player)
      "chat"   (handle-chat! db play-key player message)
      (log/warn "Unknown eridu message type" type))))

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

(defn eridu-ws-routes [db]
  [["/ws/eridu/play/:play" (partial ws-handler db)]])
