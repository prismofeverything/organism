(ns organism.routes.websockets
  (:require
   [clojure.pprint :refer (pprint)]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [org.httpkit.server :as hk]
   [organism.bots :as bots]
   [organism.game :as game]
   [organism.board :as board]
   [organism.leaderboard :as leaderboard]
   [organism.persist :as persist]
   [organism.examples :as examples])
  (:import
   [java.io ByteArrayOutputStream]))

(defn- ->stream [input]
  (cond (string? input) (io/input-stream (.getBytes input))
        :default input))

(defn read-json [input]
  (with-open [ins (->stream input)]
    (-> ins
        (transit/reader :json)
        transit/read)))

(defn write-json [output]
  (let [out (ByteArrayOutputStream. 4096)
        r (transit/writer out :json)
        _ (transit/write r output)
        ret (.toString out)]
    (.reset out)
    ret))

(defn send!
  [channel message]
  (hk/send!
   channel
   (write-json message)))

(defonce games
  (atom {:games {}}))

(defrecord GameState [key invocation game chat history channels])

(def player-cycle
  (atom (cycle board/default-player-order)))

(defn empty-game
  [game-key player channel]
  {:key game-key
   :invocation (board/empty-invocation player)
   :game nil
   :chat []
   :history []
   :channels #{channel}})

(defn append-channel!
  [game-key channel]
  (swap!
   games
   update-in [:games game-key :channels]
   conj channel))

(defn load-game
  [db game-key player channel]
  (if-let [game-state (persist/load-game db game-key)]
    (assoc game-state :channels #{channel})
    (let [game (empty-game game-key player channel)]
      (if-let [game-state (persist/find-open-game db game-key)]
        (merge game game-state)
        game))))

(defn load-game!
  [db game-key player channel]
  (let [game-state (load-game db game-key player channel)]
    (swap!
     games
     assoc-in [:games game-key]
     game-state)
    game-state))

(defn find-game!
  [db game-key player channel]
  (let [existing (get-in (deref games) [:games game-key])]
    (if (empty? existing)
      (load-game! db game-key player channel)
      (do
        (append-channel! game-key channel)
        (update existing :channels conj channel)))))

(declare maybe-run-bot-turns!)

(defn connect!
  [{:keys [db game-key player]} channel]
  (let [game-state (find-game! db game-key player channel)]
    (if (get-in game-state [:invocation :created])
      (let [player-game (persist/find-player-game db game-key player)
            witness (:witness player-game)]
        (log/info "CONNECTING" player game-key "witness" witness (get-in game-state [:game :state]))
        (send!
         channel
         {:type "initialize"
          :invocation (:invocation game-state)
          :game (:game game-state)
          :player player
          :witness witness
          :history (:history game-state)
          :chat (:chat game-state)})
        ;; If the current turn belongs to a bot, kick off bot turns
        (maybe-run-bot-turns! db game-key))
      (send!
       channel
       (-> game-state
           (select-keys [:key :invocation :chat])
           (assoc :type "create"))))))

(defn disconnect-game
  [game-key channel games]
  (let [games (update-in
               games [:games game-key :channels]
               #(remove #{channel} %))]
    (if (empty? (get-in games [:games game-key :channels]))
      ;; dissoc off the :games map, not off the wrapper — dropping the key from
      ;; the top level did nothing, so every game ever opened stayed resident
      ;; and a reconnect read the stale registry copy instead of the database.
      (update games :games dissoc game-key)
      games)))

(defn disconnect!
  [{:keys [db game-key player]} channel status]
  (log/info "channel closed" player status)
  (swap!
   games
   (partial disconnect-game game-key channel))
  (persist/store-witness! db game-key player))

(defn send-channels!
  [channels message]
  (doseq [ch channels]
    (send! ch message)))

(defn drop-game!
  "Forget a deleted game: tell any open tabs, then take it out of the registry.

   Without this a connected client keeps a live channel on a key whose data is
   gone, and the next connect! would quietly rebuild it as an empty lobby."
  [game-key]
  (let [channels (get-in @games [:games game-key :channels])]
    (when (seq channels)
      (send-channels! channels {:type "deleted" :key game-key}))
    (swap! games update :games dissoc game-key)))

(defn update-create-game
  [db player game-key channel {:keys [invocation] :as message}]
  (if-let [problem (board/game-key-problem game-key)]
    (do
      (log/warn "refusing to open game" (pr-str game-key) "-" problem)
      (send! channel {:type "error"
                      :message (str (pr-str game-key) " will not work as a game name: "
                                    problem)}))
    (let [invocation (assoc invocation :game-type "organism")]
      (swap!
       games
       assoc-in [:games game-key :invocation]
       invocation)
      (send-channels!
       (get-in @games [:games game-key :channels])
       message)
      (persist/create-open-game! db game-key invocation player))))

(declare set-slot!)

(defn update-player-name
  [db page-player game-key channel {:keys [index player] :as message}]
  (set-slot! db page-player game-key index player)
  (log/info "player name updated" player "invocation"
            (-> @games :games (get game-key) :invocation)))

(defn update-open-game
  [db player game-key channel {:keys [invocation] :as message}]
  (let [players (:players invocation)
        invocation (assoc invocation :game-type "organism")]
    (log/info "OPEN GAME" game-key players invocation)
    (persist/create-open-game! db game-key invocation player)))

(defn complete-game-state
  [{:keys [invocation game channels history chat] :as game-state}]
  (let [{:keys
         [ring-count
          player-count
          players
          colors
          organism-victory
          player-captures
          mutations]} invocation
        symmetry (board/player-symmetry player-count)
        starting (board/starting-spaces ring-count player-count players board/total-rings mutations)
        player-info (game/initial-players starting player-captures)
        notches? (board/cut-notches? ring-count player-count mutations)
        rings (vec (take ring-count board/total-rings))
        create (game/create-game symmetry rings player-info organism-victory notches? mutations)
        created (System/currentTimeMillis)]
    (-> game-state
        (assoc-in [:invocation :created] created)
        (assoc :game create))))

(defn lobby-creator
  "Who set this lobby up. Falls back to whoever is asking, for lobbies opened
   before the creator was recorded."
  [db game-key fallback]
  (or (:created-by (persist/find-open-game db game-key)) fallback))

(defn begin-game!
  "Turn an open lobby into a live game: build the starting position, tell every
   watching tab to switch over, and move the record out of :open-games.

   `creator` is whoever set the lobby up, not whoever filled the last seat — a
   game that starts itself on someone else's join still belongs to its author."
  [db game-key creator]
  (let [game-state (get-in @games [:games game-key])
        {:keys [invocation game channels history chat] :as game-state}
        (complete-game-state game-state)]
    (swap!
     games
     assoc-in
     [:games game-key]
     game-state)
    (send-channels!
     channels
     {:type "initialize"
      :invocation invocation
      :game game
      :history history
      :chat chat})
    (persist/remove-open-game! db game-key)
    (persist/create-game! db (assoc (dissoc game-state :channels)
                                    :created-by creator
                                    :game-type "organism"))
    ;; If the first player is a bot, kick off bot turns immediately
    (maybe-run-bot-turns! db game-key)
    game-state))

(defn trigger-creation
  [db player game-key channel message]
  (begin-game! db game-key (lobby-creator db game-key player)))

(defn ensure-open-game!
  "The registry entry for an open lobby, read out of the database if no tab has
   one open. Callers with no websocket of their own — the HTTP join — need this
   before they can touch the roster."
  [db game-key]
  (or (get-in @games [:games game-key])
      (when-let [open (persist/find-open-game db game-key)]
        (let [record (merge {:key game-key :game nil :history [] :channels #{}}
                            open)]
          (swap! games assoc-in [:games game-key] record)
          record))))

(defn- set-slot-in-loaded-lobby!
  [db actor game-key index player-name seats]
  (swap!
   games
   assoc-in [:games game-key :invocation :players]
   (assoc seats index player-name))
  (let [invocation (get-in @games [:games game-key :invocation])
        ;; 2-arity: leave :created-by alone, a joiner does not own the lobby
        _ (persist/create-open-game! db game-key invocation)
        _ (send-channels!
           (get-in @games [:games game-key :channels])
           {:type "player-name" :index index :player player-name})
        joined-self? (and (seq player-name) (= player-name actor))
        begin? (and joined-self? (board/full-invocation? invocation))]
    (when begin?
      (log/info "lobby full on join, beginning" game-key))
    {:invocation invocation
     :begun? (boolean
              (when begin?
                (begin-game! db game-key (lobby-creator db game-key actor))
                true))}))

(defn set-slot!
  "Put `player-name` in seat `index` of an open lobby.

   Persisting here is the whole point. A claimed seat used to live only in the
   registry plus whatever open-game snapshot the browser sent afterwards, so a
   join could be quietly undone by a stale snapshot arriving late.

   The lobby begins on its own when this fills the last seat AND the name put
   there is the name of the person doing it — that is somebody joining and
   completing the roster. A creator typing another player's name is still just
   editing, and does not start the game out from under them."
  [db actor game-key index player-name]
  (let [record (ensure-open-game! db game-key)
        seats (vec (get-in record [:invocation :players]))]
    (cond
      (nil? record) {:error "no such open game"}
      (not (and (integer? index) (<= 0 index) (< index (count seats))))
      {:error "no such seat"}
      :else (set-slot-in-loaded-lobby! db actor game-key index player-name seats))))

(defn join-open-game!
  "Take a seat in an open lobby on behalf of `player`, with the checks a click
   from the games list needs — the seat has to exist, be empty, and the player
   must not already be seated. Returns {:invocation :begun?} or {:error}."
  [db game-key index player]
  (if-let [record (ensure-open-game! db game-key)]
    (let [invocation (:invocation record)
          players (vec (:players invocation))
          seats (or (:player-count invocation) (count players))]
      (cond
        (not (and (integer? index) (<= 0 index) (< index (count players))))
        {:error "no such seat"}

        (some #{player} (take seats players))
        {:error "you are already in this game"}

        (not (str/blank? (nth players index)))
        {:error (str "that seat is taken by " (nth players index))}

        :else
        ;; the joiner is both the actor and the name, which is what lets the
        ;; lobby start itself when this was the last empty seat
        (set-slot! db player game-key index player)))
    {:error "no such open game"}))

(defn- maybe-run-bot-turns!
  "After a turn change, if the new current player is a bot, spawn a future
   that runs bot turns until a human's turn (or game-over)."
  [db game-key]
  (let [game-state (get-in @games [:games game-key])
        gs (:game game-state)
        invocation (:invocation game-state)
        all-players (set (:players invocation))
        humans (set (remove #(bots/bot? "organism" %) all-players))
        current (when gs (game/current-player gs))]
    (when (and gs (not (contains? humans current)))
      ((requiring-resolve 'organism.routes.organism-bot/run-bot-until-human!)
       games game-key humans 600
       (fn [choice-keys _next-game]
         (let [channels (get-in @games [:games game-key :channels])]
           (when (seq channels)
             (send-channels! channels {:type "bot-choices"
                                       :choices choice-keys}))))
       (fn [next-state]
         (persist/update-state! db game-key next-state))
       db))))

(defn update-game-state
  [db player game-key channel {:keys [game complete] :as message}]
  (let [game-state (get-in @games [:games game-key])
        current-player (game/current-player (:game game-state))
        invocation (:invocation game-state)]
    (when (= player current-player)
      (swap!
       games
       update-in [:games game-key]
       (fn [game-state]
         (let [game-state (assoc-in game-state [:game :state] game)]
           (if complete
             (update game-state :history conj game)
             game-state))))
      (send-channels!
       (get-in @games [:games game-key :channels])
       message)
      (persist/update-state! db game-key game)
      (if (:winner game)
        (do
          (persist/complete-game! db game-key game)
          ;; Ratings replay the whole history, so this is safe to fire on a
          ;; completion that arrives twice or on a game that later gets undone.
          (leaderboard/rate-later! db))
        (let [next-player (-> game :player-turn :player)]
          (when-not (= current-player next-player)
            (persist/update-player-games!
             db game-key
             (:players invocation)
             game)
            ;; If the next player is a bot, run bot turns automatically
            (maybe-run-bot-turns! db game-key)))))))

(defn walk-history
  [db player game-key channel message]
  (let [{:keys [game history channels invocation]} (get-in (deref games) [:games game-key])
        present (last history)
        previous (last (butlast history))
        previous (if (empty? previous) present previous)]
    (when (= player (get-in present [:player-turn :player]))
      (send-channels!
       channels
       {:type "game-state"
        :game previous})
      (swap!
       games
       (fn [games]
         (-> games
             (update-in [:games game-key :history] (comp vec butlast))
             (assoc-in [:games game-key :game :state] previous))))
      (persist/reset-state! db game-key)
      (when (not= player (-> previous :player-turn :player))
        (persist/update-player-games!
         db game-key
         (:players invocation)
         previous))
      ;; If undoing landed us on a bot's turn, kick them off again
      (maybe-run-bot-turns! db game-key))))

(defn find-beginning
  [history]
  (when-not (empty? history)
    (let [initial-state (last history)
          initial-player (game/current-player {:state initial-state})
          initial-round (game/current-round {:state initial-state})
          now-back (reverse history)
          beginning
          (last
           (take-while
            (fn [state]
              (let [player (game/current-player {:state state})
                    round (game/current-round {:state state})]
                (and
                 (= player initial-player)
                 (= round initial-round))))
            now-back))]
      (if (empty? beginning)
        initial-state
        beginning))))

(defn clear-player-turn
  [db player game-key channel message]
  (let [{:keys [game history channels invocation]} (get-in (deref games) [:games game-key])
        present (last history)
        current-player (game/current-player {:state present})]
    (when (and
           (= player current-player)
           (not (game/beginning-of-turn? game)))
      (let [beginning (find-beginning history)]
        (swap!
         games
         update-in [:games game-key]
         (fn [game-state]
           (-> game-state
               (assoc-in [:game :state] beginning)
               (update :history conj beginning))))
        (send-channels!
         channels
         {:type "game-state"
          :game beginning})
        (persist/update-state! db game-key beginning)
        ;; If clearing landed us on a bot's turn, kick them off again
        (maybe-run-bot-turns! db game-key)))))

(defn timestamp
  []
  (quot (System/currentTimeMillis) 1000))

(defn update-chat
  [db player-key game-key channel {:keys [player message] :as received}]
  (let [chat-message
        {:type "chat"
         :player player
         :time (timestamp)
         :message message}
        _
        (swap!
         games
         update-in [:games game-key :chat]
         conj
         chat-message)
        channels (get-in @games [:games game-key :channels])]
    (doseq [ch channels]
      (send! ch chat-message))
    (persist/update-chat! db game-key chat-message)))

(defn notify-clients!
  [{:keys [db player game-key]} channel raw]
  (let [message (read-json raw)]
    (log/info "MESSAGE RECEIVED -" message)
    (condp = (:type message)
      "create" (update-create-game db player game-key channel message)
      "player-name" (update-player-name db player game-key channel message)
      "open-game" (update-open-game db player game-key channel message)
      "trigger-creation" (trigger-creation db player game-key channel message)
      "game-state" (update-game-state db player game-key channel message)
      "history" (walk-history db player game-key channel message)
      "clear" (clear-player-turn db player game-key channel message)
      "chat" (update-chat db player game-key channel message)
      (log/error "unknown message type!" (:type message)))))

(defn websocket-callbacks
  [db player game-key]
  (let [config {:db db :player player :game-key game-key}]
    {:on-open (partial connect! config)
     :on-close (partial disconnect! config)
     :on-receive (partial notify-clients! config)}))

(defn ws-handler
  [db {:keys [path-params session] :as request}]
  (let [play (:play path-params)
        player (or (:player session) "--observer--")]
    (hk/as-channel request (websocket-callbacks db player play))))

(defn websocket-routes
  [db]
  [["/ws/organism/play/:play" (partial ws-handler db)]])
