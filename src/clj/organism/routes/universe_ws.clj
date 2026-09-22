(ns organism.routes.universe-ws
  "WebSocket handler for UNIVERSE hold'em.

   Two things here are not like the other games on this site.

   The state is private.  Every other `*_ws.clj` sends one identical state to
   every channel, which is fine for a board everybody can see.  A hand of cards
   is not, so this keeps a watcher map (channel -> player) and builds a
   separate message per person with `holdem/view`.  The redaction happens on
   the way out of the server; the hole cards a browser never receives are ones
   it cannot be tricked into showing.

   And the table runs on a clock.  The rest of the site is asynchronous -- a
   game waits as long as it likes for its turn -- but poker is played with
   other people sitting there, so a player who stops responding is checked or
   folded after `turn-seconds` and the hand carries on."
  (:require
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [org.httpkit.server :as hk]
   [organism.bots :as bots]
   [organism.game-ws :as gws :refer [read-json send!]]
   [organism.persist-universe :as persist]
   [universe.deck :as deck]
   [universe.holdem :as holdem]))

;; {:games {play-key {:key      play-key
;;                    :state    game-state
;;                    :players  [name ...]
;;                    :bots     #{name ...}
;;                    :channels #{channel ...}
;;                    :watchers {channel player-or-nil}
;;                    :chat     [line ...]
;;                    :tick     n}}}
(defonce games (atom {:games {}}))

(def turn-seconds
  "How long a player has to act before the table acts for them."
  60)

(def between-hands-ms
  "A pause at the showdown so everyone can read the cards before the next deal."
  6000)

(defn- shuffled
  "A fresh deck.  Nothing here is seeded: the server deals, and neither the
   client nor the player ever sees the undealt cards."
  []
  (shuffle (range 60)))

;; ── Broadcasting ───────────────────────────────────────────────────────────

(defn- state-message
  [game player]
  {"type"      "game-state"
   "state"     (pr-str (holdem/view (:state game) player))
   "chat"      (pr-str (:chat game))
   "deadline"  (str (:deadline game))})

(defn broadcast-state!
  "Send each watcher their own view.  An observer is passed nil and gets a
   table with no hole cards in it at all."
  [play-key]
  (when-let [game (gws/game-record games play-key)]
    (when (:state game)
      (gws/send-views! (:watchers game) #(state-message game %)))))

(defn- advance!
  "Store a new state, bump the tick that the clock watches, and tell everyone."
  [play-key f]
  (swap! games update-in [:games play-key]
         (fn [game]
           (-> game
               (update :state f)
               (update :tick (fnil inc 0)))))
  (broadcast-state! play-key)
  (gws/game-record games play-key))

;; ── The clock ──────────────────────────────────────────────────────────────

(declare after-action! deal-next-hand! run-bots!)

(defn- auto-action
  "What the table does for somebody who has stopped answering: check when it
   is free, fold when it is not.  Never bet on their behalf."
  [state]
  (if (:check (holdem/legal-actions state)) {:action :check} {:action :fold}))

(defn- arm-clock!
  "Start the countdown for whoever is to act.  The timer carries the tick it
   was armed at, so a timer left over from an action that already happened
   finds a newer tick and does nothing."
  [play-key db]
  (let [game (gws/game-record games play-key)
        tick (:tick game)]
    (when (and game (holdem/current-player (:state game)))
      (swap! games assoc-in [:games play-key :deadline]
             (+ (System/currentTimeMillis) (* 1000 turn-seconds)))
      (future
        (try
          (Thread/sleep (* 1000 turn-seconds))
          (let [now (gws/game-record games play-key)]
            (when (and now (= tick (:tick now)) (holdem/current-player (:state now)))
              (let [state (:state now)
                    seat  (:to-act state)]
                (log/info "Universe clock expired" play-key (holdem/player-name state seat))
                (advance! play-key #(holdem/act % seat (auto-action %)))
                (after-action! play-key db))))
          (catch Exception e
            (log/error "Universe clock error" play-key (.getMessage e))))))))

;; ── Bots ───────────────────────────────────────────────────────────────────

(defn- bot-action
  "A plain, honest opponent: it works out what its five cards would be worth
   if the board finished as it stands, and calls anything cheap, raises with a
   real hand, and folds the rest.  It does not bluff and it does not read you."
  [state]
  (let [{:keys [check call min-raise-to max-raise-to]} (holdem/legal-actions state)
        seat   (:to-act state)
        cards  (concat (get-in state [:hands seat]) (:board state))
        ;; strength runs 0..17.  Before the board is complete there is no
        ;; five-card hand to classify, so fall back on the biggest set of
        ;; matching numbers among the cards it can actually see.
        heat   (if (= 5 (count cards))
                 (/ (:strength (deck/classify cards)) 17.0)
                 (let [biggest (apply max (vals (frequencies (map deck/number cards))))]
                   (min 0.85 (* 0.22 biggest))))
        pot    (holdem/pot state)
        price  (if (pos? pot) (/ (double call) pot) 0.0)]
    (cond
      (and min-raise-to (> heat 0.6) (< (rand) 0.5))
      {:action :raise :to (min max-raise-to (max min-raise-to (quot (* 2 pot) 3)))}

      (zero? call)       (if check {:action :check} {:action :call})
      (< price heat)     {:action :call}
      :else              {:action :fold})))

(bots/register-bot!
 "universe" "ORACLE"
 {:agent-step  (fn [state] (holdem/act state (:to-act state) (bot-action state)))
  :description "Plays its cards and nothing else: calls what is cheap, raises a real hand, folds the rest. It does not bluff and it cannot read you."})

(defn run-bots!
  "Play out any bot turns until it is a human's move again.

   The step comes from the shared registry rather than straight from
   `bot-action`, so the auto-suffixed names the lobby hands out -- ORACLE-A,
   ORACLE-B -- resolve the same way they do for every other game."
  [play-key]
  (loop [guard 0]
    (let [game  (gws/game-record games play-key)
          state (:state game)
          who   (holdem/current-player state)
          step  (when who (bots/get-agent-step "universe" who))]
      (when (and game who step (contains? (:bots game) who) (< guard 60))
        (Thread/sleep 700)
        (advance! play-key step)
        (recur (inc guard))))))

;; ── Hands ──────────────────────────────────────────────────────────────────

(defn- deal-next-hand!
  "After a showdown, pause so the cards can be read, then deal again -- unless
   somebody has won the whole table."
  [play-key db]
  (future
    (try
      (Thread/sleep between-hands-ms)
      (let [game (gws/game-record games play-key)]
        (when (and game (not (holdem/game-over? (:state game)))
                   (>= (count (holdem/with-chips (:state game))) 2))
          (advance! play-key #(holdem/start-hand % (shuffled)))
          (when db (persist/save-state! db play-key (:state (gws/game-record games play-key))))
          ;; a deal that put everyone all-in is already finished, so the same
          ;; follow-up handles it as any other completed hand
          (after-action! play-key db)))
      (catch Exception e
        (log/error "Universe deal error" play-key (.getMessage e))))))

(defn after-action!
  "Everything that follows somebody acting: let the bots move, restart the
   clock, and if the hand is over, record it and set up the next one."
  [play-key db]
  (run-bots! play-key)
  (let [game (gws/game-record games play-key)]
    (if (holdem/hand-over? (:state game))
      (do
        (when db (persist/save-state! db play-key (:state game)))
        (if (holdem/game-over? (:state game))
          (do (log/info "Universe table finished" play-key (:winner (:state game)))
              (when db (persist/complete-game! db play-key (:state game))))
          (deal-next-hand! play-key db)))
      (arm-clock! play-key db))))

;; ── Messages ───────────────────────────────────────────────────────────────

(defn- handle-action!
  [play-key player message db]
  (let [game  (gws/game-record games play-key)
        state (:state game)
        seat  (holdem/seat-of state player)
        raw   (or (get message "choice") (get message :choice))
        action (if (string? raw) (edn/read-string raw) raw)]
    (when (and state seat (= seat (:to-act state)))
      (let [before (:state game)
            after  (holdem/act before seat action)]
        (if (identical? before after)
          (log/warn "Universe illegal action" play-key player (pr-str action))
          (do
            (advance! play-key (constantly after))
            (after-action! play-key db)))))))

(defn- handle-chat!
  [play-key player message]
  (let [line (or (get message "line") (get message :line))]
    (when-not (empty? line)
      (swap! games update-in [:games play-key :chat]
             (fnil conj []) {:player player :line line})
      (broadcast-state! play-key))))

(defn- handle-start!
  "Deal the first hand.  Anyone at the table can start it once everybody has
   had a chance to arrive."
  [play-key db]
  (let [game (gws/game-record games play-key)]
    (when (and game (= :waiting (:street (:state game))))
      (advance! play-key #(holdem/start-hand % (shuffled)))
      (when db (persist/save-state! db play-key (:state (gws/game-record games play-key))))
      (run-bots! play-key)
      (arm-clock! play-key db))))

;; ── Lifecycle ──────────────────────────────────────────────────────────────

(defn- load-game!
  "Bring a table into memory, from the database if it was there before."
  [db play-key]
  (or (gws/game-record games play-key)
      (when-let [stored (and db (persist/load-game db play-key))]
        (gws/put-game! games play-key
                       {:key play-key
                        :state (:state stored)
                        :players (:players stored)
                        :bots (set (:bots stored))
                        :chat []
                        :channels #{}
                        :watchers {}
                        :tick 0}))))

(defn connect! [{:keys [play-key player db]} channel]
  (load-game! db play-key)
  (gws/watch! games play-key channel player)
  (log/info "Universe CONNECT" player play-key)
  (let [game (gws/game-record games play-key)]
    (send! channel
           (cond-> {"type" "initialize" "key" play-key "player" (str player)}
             (:state game) (merge (state-message game player))))))

(defn disconnect! [{:keys [play-key player]} channel status]
  (log/info "Universe DISCONNECT" player status)
  (gws/remove-channel! games play-key channel))

(defn notify-clients! [{:keys [play-key player db]} _channel raw]
  (let [message (read-json raw)
        kind    (or (get message "type") (get message :type))]
    (case kind
      "action" (handle-action! play-key player message db)
      "chat"   (handle-chat! play-key player message)
      "start"  (handle-start! play-key db)
      (log/warn "Unknown universe message" kind))))

(defn websocket-callbacks [player play-key db]
  (gws/make-callbacks {:player player :play-key play-key :db db}
                      {:on-open    #'connect!
                       :on-close   #'disconnect!
                       :on-receive #'notify-clients!}))

(defn ws-handler [db {:keys [path-params session] :as request}]
  (let [play   (:play path-params)
        player (:player session)]
    (hk/as-channel request (websocket-callbacks player play db))))

(defn universe-ws-routes [db]
  [["/ws/universe/play/:play" (partial ws-handler db)]])
