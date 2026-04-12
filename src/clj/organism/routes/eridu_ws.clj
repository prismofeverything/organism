(ns organism.routes.eridu-ws
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [org.httpkit.server :as hk]
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
  (hk/send! channel (write-json message)))

(defn send-channels! [channels message]
  (doseq [ch channels]
    (send! ch message)))

;; ── Games atom ────────────────────────────────────────────────────────────────

(defonce games (atom {:games {}}))

;; ── Helpers ─────────────────────────────────────────────────────────────────

(defn- choice-player [state]
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

;; ── Bot AI ───────────────────────────────────────────────────────────────────

(def ^:private bot-protected-phases
  "Phases where we broadcast before continuing, so watchers can see the game."
  #{:choose-die :choose-action :resolve-landing :game-over})

(defn- bot-advance
  "Advance state through trivial single-choice phases."
  [state]
  (loop [s state]
    (let [[p cs] (choice/find-state-raw s)]
      (if (and (= 1 (count cs))
               (not (contains? bot-protected-phases p)))
        (let [ns (first (vals cs))]
          (if ns (recur ns) s))
        s))))

(defn agent-step
  "Pick a choice for the bot. Simple heuristic AI.
   Returns [choice-key next-state] or nil."
  [state]
  (let [[phase choices] (choice/find-state-raw state)]
    (when (and (not= phase :game-over) (seq choices))
      (let [player (game/current-player state)
            pdata  (game/player-data state player)

            pick
            (case phase
              ;; Pick the highest die value for maximum astronomer movement
              :choose-die
              (let [dice (get pdata :dice-available [])
                    best-idx (apply max-key #(nth dice %) (range (count dice)))]
                best-idx)

              ;; Move the first astronomer (simple choice)
              :choose-astronomer
              (first (keys choices))

              ;; When landing: prefer actions if multiple astronomers, else increase role
              :resolve-landing
              (if (contains? choices :begin)
                :begin
                :increase-role)

              ;; Choose role increase: prefer lowest role to balance
              :choose-role-increase
              (if (> (count choices) 1)
                (let [role-choices (dissoc choices :skip)
                      role-levels (:roles pdata)]
                  (if (seq role-choices)
                    (apply min-key #(get role-levels % 99) (keys role-choices))
                    :skip))
                (first (keys choices)))

              ;; Choose action: prefer take-goods > sell > travel > temple > deploy > influence
              :choose-action
              (if (contains? choices :done)
                :done
                (let [space (get-in state [:player-turn :space])
                      action-priority {:take 0 :sell 1 :travel 2 :temple 3 :deploy 4 :influence 5}
                      action-choices (dissoc choices :done)
                      scored (for [[idx _] action-choices
                                   :let [action (nth (:actions (get game/action-spaces space)) idx)
                                         pri (get action-priority (:type action) 99)]]
                               [pri idx])]
                  (if (seq scored)
                    (second (first (sort scored)))
                    (first (keys choices)))))

              ;; Sell: pick any sellable resource
              :resolve-sell
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (first (keys non-skip))
                  :skip))

              ;; Temple: pick first available city
              :resolve-temple
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (first (keys non-skip))
                  :skip))

              ;; Deploy: place a raider if possible
              :resolve-deploy
              (let [non-skip (dissoc choices :skip :done)]
                (if (seq non-skip)
                  (first (keys non-skip))
                  (or (:done choices) (first (keys choices)))))

              ;; Travel: pick a random neighbor
              :resolve-travel
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (rand-nth (vec (keys non-skip)))
                  :skip))

              ;; After travel, decline to spend a good for extra movement
              :travel-continue
              :done

              ;; Influence: move first magistrate option or skip
              :resolve-influence
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (first (keys non-skip))
                  :skip))

              ;; Take goods: auto-resolve
              :resolve-take
              :done

              ;; Default: first choice
              (first (keys choices)))]

        (when-let [next-s (get choices pick)]
          [pick next-s])))))

;; ── Bot turns ────────────────────────────────────────────────────────────────

(defn run-bot-turns!
  "Spawn a future that auto-plays bot turns with delay until game over."
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
            (Thread/sleep (get game-data :bot-delay 300))
            (let [current-state (:state (get-in @games [:games play-key]))]
              (when (and current-state
                         (not (:game-over current-state))
                         (contains? bots (choice-player current-state)))
                (let [step-result (or (agent-step current-state)
                                      (let [[_ cs] (choice/find-state-raw current-state)]
                                        (when (seq cs)
                                          [(first (keys cs)) (first (vals cs))])))]
                  (when-let [[ck next-state] step-result]
                    (let [effective (bot-advance next-state)]
                      (swap! games
                             (fn [gs]
                               (-> gs
                                   (assoc-in [:games play-key :state] effective)
                                   (assoc-in [:games play-key :history] []))))
                      (broadcast-state! play-key)
                      (save-state! db play-key ck)
                      (recur)))))))))
      (catch Exception e
        (log/error "Eridu bot turn error" play-key (.getMessage e))))))

;; ── Message handlers ──────────────────────────────────────────────────────────

(def ^:private protected-phases
  #{:choose-die :choose-astronomer :choose-action :choose-role-increase
    :resolve-landing :resolve-sell :resolve-temple :resolve-deploy
    :resolve-travel :travel-continue :resolve-influence :game-over})

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
      (broadcast-state! play-key)
      (when (contains? bot-set (choice-player state))
        (run-bot-turns! db play-key)))))

(defn handle-action! [db play-key player-key {:keys [choice]}]
  (let [game-data (get-in @games [:games play-key])
        state     (:state game-data)]
    (when state
      (try
        (let [choice-key          (edn/read-string choice)
              [_phase choices-map] (choice/find-state-raw state)
              next-state           (get choices-map choice-key)]
          (if next-state
            (let [effective (loop [s next-state]
                              (let [p  (game/current-phase s)
                                    cs (second (choice/find-state-raw s))]
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
              (log/info "Action" play-key player-key (pr-str choice-key))
              (broadcast-state! play-key)
              (save-state! db play-key choice-key)
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
               base-msg))
      ;; If all players are bots and game isn't over, start bot turns
      (when-let [state (:state game-data)]
        (when (and (not (:game-over state))
                   (contains? (:bots game-data) (choice-player state)))
          (run-bot-turns! db play-key))))))

(defn disconnect! [{:keys [play-key player]} channel status]
  (log/info "Eridu DISCONNECT" player status)
  (swap! games
         (fn [gs]
           (let [remaining (remove #{channel}
                                   (get-in gs [:games play-key :channels]))]
             (if (empty? remaining)
               (update-in gs [:games] dissoc play-key)
               (assoc-in gs [:games play-key :channels] (set remaining)))))))

(defn notify-clients! [{:keys [db play-key player]} _channel raw]
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
     :on-receive (partial notify-clients!  cfg)}))

(defn ws-handler [db {:keys [path-params session] :as request}]
  (let [play   (:play path-params)
        player (or (:player session) "--observer--")]
    (hk/as-channel request (websocket-callbacks db player play))))

(defn eridu-ws-routes [db]
  [["/ws/eridu/play/:play" (partial ws-handler db)]])
