(ns organism.routes.distinctions-ws
  "WebSocket handler for DISTINCTIONS.

   Built the way UNIVERSE's is, since this is also a game of hidden hands:
   a watcher map (channel -> player) and a separate message per person, cut
   down by `game/view` on the way out. Unlike UNIVERSE there is no clock --
   a turn waits for its player, as the rest of the site does -- so the only
   thing that runs on its own is the bots."
  (:require
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [org.httpkit.server :as hk]
   [organism.bots :as bots]
   [organism.game-ws :as gws :refer [read-json send!]]
   [organism.persist-distinctions :as persist]
   [distinctions.bot :as bot]
   [distinctions.game :as game]))

;; {:games {play-key {:key :state :players :bots :channels :watchers :chat :tick}}}
(defonce games (atom {:games {}}))

(def bot-pause-ms
  "Long enough to see what a bot took before it throws something back."
  650)

;; ── Broadcasting ───────────────────────────────────────────────────────────

(defn- state-message [game player]
  {"type"  "game-state"
   "state" (pr-str (game/view (:state game) player))
   "chat"  (pr-str (:chat game))})

(defn broadcast-state! [play-key]
  (when-let [game (gws/game-record games play-key)]
    (when (:state game)
      (gws/send-views! (:watchers game) #(state-message game %)))))

(defn- advance! [play-key f]
  (swap! games update-in [:games play-key]
         (fn [game] (-> game (update :state f) (update :tick (fnil inc 0)))))
  (broadcast-state! play-key)
  (gws/game-record games play-key))

(defn- save! [play-key db]
  (when db
    (let [state (:state (gws/game-record games play-key))]
      (if (game/game-over? state)
        (persist/complete-game! db play-key state)
        (persist/save-state! db play-key state)))))

;; ── Bots ───────────────────────────────────────────────────────────────────

(doseq [{:keys [name caution description]} bot/profiles]
  (bots/register-bot!
   "distinctions" name
   {:agent-step  (fn [state] (bot/step state caution))
    :description description}))

(defn run-bots!
  "Play out bot moves -- each a draw and a discard, shown separately -- until
   a person is to act or the game is over."
  [play-key db]
  (loop [guard 0]
    (let [game  (gws/game-record games play-key)
          who   (game/current-player (:state game))
          step  (when who (bots/get-agent-step "distinctions" who))]
      (when (and who step (contains? (:bots game) who) (< guard 400))
        (Thread/sleep bot-pause-ms)
        (advance! play-key step)
        (save! play-key db)
        (recur (inc guard))))))

(defn- run-bots-async! [play-key db]
  (future
    (try (run-bots! play-key db)
         (catch Exception e (log/error "Distinctions bot error" play-key (.getMessage e))))))

;; ── Messages ───────────────────────────────────────────────────────────────

(defn- handle-action! [play-key player message db]
  (let [state  (:state (gws/game-record games play-key))
        seat   (game/seat-of state player)
        raw    (or (get message "choice") (get message :choice))
        action (if (string? raw) (edn/read-string raw) raw)]
    (when (and state seat (= seat (:to-act state)))
      (let [after (game/act state seat action)]
        (if (identical? state after)
          (log/warn "Distinctions illegal action" play-key player (pr-str action))
          (do (advance! play-key (constantly after))
              (save! play-key db)
              (run-bots-async! play-key db)))))))

(defn- handle-chat! [play-key player message]
  (let [line (or (get message "line") (get message :line))]
    (when-not (empty? line)
      (swap! games update-in [:games play-key :chat] (fnil conj []) {:player player :line line})
      (broadcast-state! play-key))))

(defn- handle-start! [play-key db]
  (let [game (gws/game-record games play-key)]
    (when (and game (= :waiting (:phase (:state game))))
      (advance! play-key #(game/start % (shuffle game/all-cards)))
      (save! play-key db)
      (run-bots-async! play-key db))))

;; ── Lifecycle ──────────────────────────────────────────────────────────────

(defn- load-game! [db play-key]
  (or (gws/game-record games play-key)
      (when-let [stored (and db (persist/load-game db play-key))]
        (gws/put-game! games play-key
                       {:key play-key :state (:state stored) :players (:players stored)
                        :bots (set (:bots stored)) :chat [] :channels #{} :watchers {}
                        :tick 0}))))

(defn connect! [{:keys [play-key player db]} channel]
  (let [before (gws/game-record games play-key)]
    (load-game! db play-key)
    (gws/watch! games play-key channel player)
    ;; a bot whose move was cut off by a restart is waiting on nobody
    (when (empty? (:channels before))
      (run-bots-async! play-key db)))
  (log/info "Distinctions CONNECT" player play-key)
  (let [game (gws/game-record games play-key)]
    (send! channel (cond-> {"type" "initialize" "key" play-key "player" (str player)}
                     (:state game) (merge (state-message game player))))))

(defn disconnect! [{:keys [play-key player]} channel status]
  (log/info "Distinctions DISCONNECT" player status)
  (gws/unwatch! games play-key channel)
  (let [game (gws/game-record games play-key)]
    (when (and (empty? (:channels game))
               (or (nil? (:state game)) (game/game-over? (:state game))))
      (gws/forget-game! games play-key))))

(defn notify-clients! [{:keys [play-key player db]} _channel raw]
  (let [message (read-json raw)
        kind    (or (get message "type") (get message :type))]
    (case kind
      "action" (handle-action! play-key player message db)
      "chat"   (handle-chat! play-key player message)
      "start"  (handle-start! play-key db)
      (log/warn "Unknown distinctions message" kind))))

(defn websocket-callbacks [player play-key db]
  (gws/make-callbacks {:player player :play-key play-key :db db}
                      {:on-open    #'connect!
                       :on-close   #'disconnect!
                       :on-receive #'notify-clients!}))

(defn ws-handler [db {:keys [path-params session] :as request}]
  (hk/as-channel request (websocket-callbacks (:player session) (:play path-params) db)))

(defn distinctions-ws-routes [db]
  [["/ws/distinctions/play/:play" (partial ws-handler db)]])
