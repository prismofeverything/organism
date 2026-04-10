(ns organism.routes.journey-ws
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [org.httpkit.server :as hk]
   [journey.game :as game]
   [journey.choice :as choice]
   [journey.bot-flow :as bot-flow]
   [organism.persist :as persist]
   [organism.persist-journey :as persist-j]
   [organism.persist-journey-bots :as bots-db])
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
  (hk/send! channel (write-json message)))

(defn send-channels! [channels message]
  (doseq [ch channels]
    (send! ch message)))

;; ── Games atom ────────────────────────────────────────────────────────────────
;; {:games {play-key → {:key      play-key
;;                       :state    game-state (or nil before creation)
;;                       :history  []  — undo stack of previous states
;;                       :bots     #{} — set of bot player names
;;                       :players  []  — player name list
;;                       :chat     [...]
;;                       :channels #{channel ...}}}}

(defonce games (atom {:games {}}))

;; ── Helpers ─────────────────────────────────────────────────────────────────

(defn- choice-player
  "The player who must make the current choice."
  [state]
  (or (get-in state [:player-turn :choice-player])
      (game/current-player state)))

;; ── Persistence helper ──────────────────────────────────────────────────────

(defn- save-state!
  "Persist current state. When choice-key is provided, also append to action log."
  ([db play-key] (save-state! db play-key nil))
  ([db play-key choice-key]
   (let [game (get-in @games [:games play-key])]
     (when (:state game)
       (persist-j/save-game!
        db play-key (:state game) (:bots game)
        (or (:players game) (:turn-order (:state game)))
        (:initial-state game))
       (when choice-key
         (persist-j/append-action! db play-key choice-key))))))

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
  ;; Try loading from MongoDB first
  (if-let [saved (persist-j/load-game db play-key)]
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

;; ── Bot turns ───────────────────────────────────────────────────────────────

(defn- builtin-agent-step []
  (requiring-resolve 'organism.routes.journey/agent-step))

(defn- bot-agent-step
  "Pick the right agent-step for `bot-name`. Tries the exact name first, then
   the base name (strips suffix like OBO-A → OBO). Falls back to the built-in
   heuristic bot."
  [db bot-name]
  (let [base-name (when bot-name
                    (str/replace bot-name #"-(?:[A-Z]|\d+)$" ""))
        try-db    (fn [n]
                    (when n
                      (when-let [saved (bots-db/find-bot db n)]
                        (when-let [d (:definition saved)]
                          (fn [state] (bot-flow/agent-step d state))))))]
    (or (try-db bot-name)
        (when (not= base-name bot-name) (try-db base-name))
        (builtin-agent-step))))

(def ^:private bot-protected-phases
  "Phases where bot should stop, broadcast, and make a visible decision."
  #{:choose-action-type :choose-move :choose-convert :choose-activate
    :choose-activate-station :choose-activate-self-bonus :choose-activate-owner-bonus
    :choose-activate-matrix-beacon :choose-activate-tower-heading
    :choose-land :game-over})

(defn- bot-advance
  "Advance state through trivial single-choice phases for bot display.
   Stops at phases where the bot should visibly make a decision."
  [state]
  (loop [s state]
    (let [[phase cs] (choice/find-state-raw s)]
      (if (and (= 1 (count cs))
               (not (contains? bot-protected-phases phase)))
        (let [ns (first (vals cs))]
          (if ns (recur ns) s))
        s))))

(defn run-bot-turns!
  "Spawn a future that auto-plays bot turns with delay until a human's turn."
  [db play-key]
  (future
    (try
      (loop []
        (let [game-data (get-in @games [:games play-key])
              state     (:state game-data)
              bots      (:bots game-data)]
          (when (and state
                     (not (:game-over state))
                     (contains? bots (choice-player state)))
            (Thread/sleep (get game-data :bot-delay 500))
            (let [current-state (:state (get-in @games [:games play-key]))]
              (when (and current-state
                         (not (:game-over current-state))
                         (contains? bots (choice-player current-state)))
                ;; Use agent-step to pick, with fallback to first choice if agent can't decide
                (let [bot-name    (choice-player current-state)
                      step-fn     (bot-agent-step db bot-name)
                      step-result (or (step-fn current-state)
                                     ;; Fallback: pick first choice from raw phase
                                     (let [[_ cs] (choice/find-state-raw current-state)]
                                       (when (seq cs)
                                         [(first (keys cs)) (first (vals cs))])))]
                (when-let [[ck next-state] step-result]
                  ;; Track visited positions for bot fly logic
                  (let [phase     (game/current-phase current-state)
                        next-state (cond
                                     ;; After fly-from: record the from-pos as visited
                                     (and (vector? ck) (= :fly (first ck)))
                                     (update-in next-state [:player-turn :action :bot-fly-visited]
                                                (fnil conj #{}) (second ck))
                                     ;; After fly-to: record the destination as visited
                                     (= phase :choose-fly-to)
                                     (update-in next-state [:player-turn :action :bot-fly-visited]
                                                (fnil conj #{}) ck)
                                     :else next-state)
                        effective (bot-advance next-state)]
                    (swap! games
                           (fn [gs]
                             (-> gs
                                 (assoc-in [:games play-key :state] effective)
                                 (assoc-in [:games play-key :history] []))))
                    (broadcast-state! play-key)
                    (save-state! db play-key ck)
                    (recur)))))))))
      (catch Exception e
        (log/error "Bot turn error" play-key (.getMessage e))))))

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
      (log/info "Created journey game" play-key "players:" players "bots:" bots)
      (save-state! db play-key)
      (broadcast-state! play-key)
      (when (contains? bot-set (choice-player state))
        (run-bot-turns! db play-key)))))

(defn handle-action! [db play-key player-key {:keys [choice]}]
  (let [game-data (get-in @games [:games play-key])
        state     (:state game-data)]
    (when state
      (try
        (let [choice-key           (edn/read-string choice)
              ;; Use find-state-raw to avoid stale .cljc auto-advance logic
              [phase choices-map] (choice/find-state-raw state)
              _                   (log/info "Phase:" phase "choices:" (keys choices-map))
              next-state           (get choices-map choice-key)]
          (if next-state
            ;; Auto-advance through single-choice non-protected phases.
            ;; Implemented entirely in .clj — no dependency on .cljc auto-advance.
            (let [protected-phases #{:choose-action-type :choose-move :choose-convert
                                     :choose-activate :choose-activate-station
                                     :choose-activate-self-bonus
                                     :choose-activate-owner-bonus :choose-land
                                     :game-over}
                  effective (loop [s next-state]
                              (let [p  (game/current-phase s)
                                    cs (second (choice/find-state-raw s))]
                                (log/info "EFFECTIVE:" p "n-choices:" (count cs)
                                          "protected?" (contains? protected-phases p))
                                (if (and (= 1 (count cs))
                                         (not (contains? protected-phases p)))
                                  (recur (first (vals cs)))
                                  s)))
                  old-player    (choice-player state)
                  new-player    (choice-player effective)
                  turn-changed? (not= old-player new-player)
                  bots          (:bots game-data)]
              (swap! games
                     (fn [gs]
                       (-> gs
                           (assoc-in [:games play-key :state] effective)
                           (assoc-in [:games play-key :history]
                                     (if turn-changed?
                                       []
                                       (conj (:history (get-in gs [:games play-key])) state))))))
              (log/info "Action" play-key player-key (pr-str choice-key)
                        "turn-changed?" turn-changed?
                        "can-undo?" (boolean (seq (:history (get-in @games [:games play-key])))))
              (broadcast-state! play-key)
              (save-state! db play-key choice-key)
              ;; Check if the current choice-player (after auto-advance) is a bot
              (when (contains? bots new-player)
                (run-bot-turns! db play-key)))
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
    (log/info "Journey CONNECT" player play-key)
    (let [base-msg {:type    "initialize"
                    :key     play-key
                    :player  player
                    :bots    (vec (:bots game-data))
                    :chat    (:chat game-data)
                    ;; Send lightweight history (no states) — states are reconstructed via replay on demand
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

(defn disconnect! [{:keys [play-key player]} channel status]
  (log/info "Journey DISCONNECT" player status)
  (swap! games
         (fn [gs]
           (let [remaining (remove #{channel}
                                   (get-in gs [:games play-key :channels]))]
             (if (empty? remaining)
               (update-in gs [:games] dissoc play-key)
               (assoc-in gs [:games play-key :channels] (set remaining)))))))

(defn handle-replay-state! [db play-key _channel {:keys [step]}]
  (let [game-data (get-in @games [:games play-key])
        initial   (:initial-state game-data)]
    (when initial
      (let [actions (persist-j/load-actions db play-key)
            ;; Replay up to the requested step
            target-actions (take step actions)
            result-state   (persist-j/replay initial target-actions)]
        (send! _channel {:type "replay-state"
                         :step step
                         :state (pr-str result-state)})))))

(defn notify-clients! [{:keys [db play-key player]} channel raw]
  (let [{:keys [type] :as message} (read-json raw)]
    (log/info "Journey MSG" type player)
    (case type
      "create"       (handle-create! db play-key message)
      "action"       (handle-action! db play-key player message)
      "undo"         (handle-undo! db play-key player)
      "chat"         (handle-chat! db play-key player message)
      "replay-state" (handle-replay-state! db play-key channel message)
      (log/warn "Unknown journey message type" type))))

;; ── Route wiring ─────────────────────────────────────────────────────────────

(defn websocket-callbacks [db player play-key]
  (let [cfg {:db db :player player :play-key play-key}]
    {:on-open    (partial connect!         cfg)
     :on-close   (partial disconnect!      cfg)
     :on-receive (partial notify-clients!  cfg)}))

(defn ws-handler [db {:keys [path-params session] :as request}]
  (let [play   (:play path-params)
        player (or (:player session) "--observer--")]
    (hk/as-channel request (websocket-callbacks db player play))))

(defn journey-ws-routes [db]
  [["/ws/journey/play/:play" (partial ws-handler db)]])
