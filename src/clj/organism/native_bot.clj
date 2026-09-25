(ns organism.native-bot
  "Plays organism with a trained network, by handing positions to the native
   move server (`organism-train serve`) over a pipe.

   The network lives in a Rust process because that is where it was trained;
   starting one costs a few seconds of weight loading, so a server is started
   once and kept for the life of the web process. Each answer costs a fraction
   of a second on CPU — no GPU is involved.

   Positions cross the pipe as whole boards rather than as a list of moves,
   because a web game keeps an undo stack of states and has no move history to
   replay. `position` translates a game into the board the Rust engine
   serializes, and `choice-for` translates the move that comes back into one of
   the choice keys `organism.choice/find-state` is offering. The two engines
   index spaces and actions identically; `tests/check_native_parity.py` and
   `tests/check_native_bot.py` are what hold that agreement in place.

   One structural difference needs handling: funding a growth is a single
   choice here (which growers pay, all at once) and a run of choices there (one
   donor space per food). `grow-from-choice` replays that run against the
   server and reads the finished allocation off the action it lands on."
  (:require
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [organism.board :as board]
   [organism.bots :as bots]
   [organism.choice :as choice]
   [organism.game :as game]))

;; ── Action index layout ─────────────────────────────────────────────────────
;;
;; Shared with alphazero/games/organism/choices.py and native/src/game.rs.
;; Indices below the space count select a space; the rest are fixed slots.

(def element-types [:eat :grow :move])
(def action-types [:eat :grow :move :circulate])

(def introduce-permutations
  "The six orders of eat/grow/move, in the order every engine numbers them."
  [[:eat :grow :move] [:eat :move :grow]
   [:grow :eat :move] [:grow :move :eat]
   [:move :eat :grow] [:move :grow :eat]])

(defn board-spaces
  "Every space in the order all three engines index them: the centre, then each
   ring outward, each ring counted from its own zero step."
  [game]
  (let [symmetry (board/player-symmetry (count (:turn-order game)))]
    (vec (mapcat second (game/build-rings symmetry (:rings game))))))

(defn space-index
  "space → index, and the vector that inverts it."
  [game]
  (let [spaces (board-spaces game)]
    [(zipmap spaces (range)) spaces]))

;; ── Translating a game into a board the server understands ──────────────────

(defn- kind-name
  "Element and action types cross the wire capitalised, as the Rust enum spells
   them. Captures name why they were taken, so the reasons belong here too."
  [type]
  (if (nil? type)
    nil
    (case type
      :eat "Eat" :grow "Grow" :move "Move" :circulate "Circulate"
      :integrity "Integrity" :sacrifice "Sacrifice" :center "Center"
      (throw (ex-info "no native name for this type" {:type type})))))

(defn- wire-action
  "One action of a turn. A growth's `:from` is the map of growers that funded
   it, which the Rust engine keeps in its own field."
  [index-of {:keys [type action]}]
  (let [{:keys [from to element pass]} action
        funding? (map? from)]
    {"kind" (kind-name type)
     "element" (kind-name element)
     "from" (when (and from (not funding?)) (index-of from))
     "to" (when to (index-of to))
     "donors" (when funding?
                (into {} (map (fn [[space n]] [(str (index-of space)) n]) from)))
     "pass" (boolean pass)}))

(defn position
  "The current board, in the shape `organism-train serve` deserializes.

   `order` is the Rust engine's tie-break for which organism id survives a
   merge; ids only ever label groups, so counting spaces is as good an order as
   the one a replay would have produced."
  [game]
  (let [[index-of spaces] (space-index game)
        {:keys [turn-order state]} game
        {:keys [round elements captures player-turn winner]} state
        {:keys [player introduction organism-turns]} player-turn
        seat (zipmap turn-order (range))]
    {"pieces"
     (mapv
      (fn [space]
        (when-let [{:keys [player organism type food]} (get elements space)]
          {"player" (seat player) "kind" (kind-name type) "food" food
           "organism" organism "marks" [] "order" (index-of space)}))
      spaces)
     "food" (mapv (fn [space] (get-in state [:food space] 0)) spaces)
     "captures" (mapv
                 (fn [name]
                   (mapv (fn [{:keys [player type]}]
                           {"player" (seat player) "kind" (kind-name type)})
                         (get captures name)))
                 turn-order)
     "player" (seat player)
     "round" round
     "introduced" (boolean introduction)
     "partial" {}
     "next_order" (count spaces)
     "winner" (when winner (seat winner))
     "turns" (mapv
              (fn [{:keys [organism choice num-actions actions]}]
                {"organism" organism
                 "choice" (kind-name choice)
                 "num_actions" num-actions
                 "actions" (mapv (partial wire-action index-of) actions)})
              organism-turns)}))

;; ── Translating a move back into a choice ───────────────────────────────────

(defn- introduce-order
  "The element order an introduction lays down, read back in home-space order."
  [game introduction]
  (let [player (get-in game [:state :player-turn :player])
        starting (get-in game [:players player :starting-spaces])]
    (mapv (:spaces introduction) starting)))

(defn choice-for
  "The choice key matching action index `action`, or nil if none does.

   `choices` is the map `find-state` returned, so the lookup is over exactly
   what the game is offering rather than over what the action space allows.

   Funding a growth is the one phase this cannot answer — a whole allocation
   here is a run of donor picks there — and `grow-from-choice` handles it."
  [game phase choices action]
  (let [[index-of spaces] (space-index game)
        n (count spaces)
        slot (- action n)
        space (when (< action n) (nth spaces action))]
    (cond
      space
      (if (= phase :choose-organism)
        ;; An organism is named by the lowest-indexed space it holds. Only the
        ;; ones still on offer count: by the second organism of a turn the
        ;; player has others that have already acted.
        (let [organisms (game/player-organisms
                         game (get-in game [:state :player-turn :player]))]
          (first (for [id (keys choices)
                       :let [elements (get organisms id)]
                       :when (and elements
                                  (= action (apply min (map (comp index-of :space) elements))))]
                   id)))
        (first (filter #(= % space) (keys choices))))

      (< -1 slot 6)
      (let [order (nth introduce-permutations slot)]
        (first (filter #(= order (introduce-order game %)) (keys choices))))

      (< 5 slot 10) (nth action-types (- slot 6))
      (< 9 slot 13) (nth element-types (- slot 10))
      (= slot 13) :pass
      (= slot 14) {}
      :else nil)))

(defn action-for
  "The action index a choice key stands for, inverting `choice-for`.

   Funding a growth has no single index — one choice here is a run of donor
   picks there — so it answers nil and the caller replays that run instead."
  [game phase key]
  (let [[index-of spaces] (space-index game)
        n (count spaces)]
    (case phase
      :introduce (+ n (.indexOf introduce-permutations (introduce-order game key)))
      :choose-organism (let [elements (get (game/player-organisms
                                            game (get-in game [:state :player-turn :player]))
                                           key)]
                         (apply min (map (comp index-of :space) elements)))
      :choose-action-type (+ n 6 (.indexOf action-types key))
      :choose-action (if (= :pass key) (+ n 13) (+ n 6 (.indexOf action-types key)))
      :grow-element (+ n 10 (.indexOf element-types key))
      :grow-from nil
      (cond
        (= :pass key) (+ n 13)
        (contains? index-of key) (index-of key)
        :else nil))))

;; ── The move server ─────────────────────────────────────────────────────────

(def base-settings
  "A three-player, four-ring board, played by the network trained for it.

   The rules are the ones the model trained under, which are now also the ones
   the website plays: no deliberate passing, no eating past five food, no food
   left behind by wiping yourself out. Both engines have to agree about what is
   legal or the server could not read the website's positions at all.

   `sims` is how hard it thinks per decision, and the only real cost knob: on
   CPU each decision is a fraction of a second, and a three-player turn is about
   six decisions. The paths are where a built checkout keeps things; a deployed
   host overrides them — see `settings`."
  {:players 3
   :rings 4
   :blocks 8
   :filters 128
   :sims 64
   :threads 4
   :cpu true
   :eat-threshold 5
   :require-useful-action 1
   :sacrifice-yields-nothing 1
   :serve "native/target/release/organism-train"
   :torch ".venv-training/lib/python3.12/site-packages/torch/lib"
   :weights "checkpoints/organism-native/3p/serve.ot"})

(def settings-file
  "Where a deploy leaves its answer, relative to the working directory.

   It is a file rather than the service's environment because the deploying user
   may restart the service and nothing else — editing the unit needs a root this
   deploy deliberately does not have."
  "bot/settings.edn")

(defn- deployed-settings
  []
  (let [file (io/file settings-file)]
    (when (.exists file)
      (try
        (edn/read-string (slurp file))
        (catch Exception e
          (println "NEURON: ignoring unreadable" settings-file "—" (.getMessage e))
          nil)))))

(def ^:private from-environment
  [[:serve   "ORGANISM_BOT_SERVE"   identity]
   [:torch   "ORGANISM_BOT_TORCH"   identity]
   [:weights "ORGANISM_BOT_WEIGHTS" identity]
   [:sims    "ORGANISM_BOT_SIMS"    #(Integer/parseInt %)]
   [:threads "ORGANISM_BOT_THREADS" #(Integer/parseInt %)]])

(defn- environment-settings
  []
  (into {}
        (keep (fn [[key name read]]
                (when-let [value (System/getenv name)]
                  [key (read value)])))
        from-environment))

(defn settings
  "What to serve and how hard to think, in increasing order of specificity:
   the built-in defaults, what a deploy left in bot/settings.edn, and the
   environment for a one-off."
  []
  (merge base-settings (deployed-settings) (environment-settings)))

(defn executable [] (:serve (settings)))
(defn torch-lib [] (:torch (settings)))

(defn weights-path
  "The weights being served — deliberately a copy that `native/publish-bot.sh`
   put in place, not the live checkpoint: training rotates its snapshots away
   underneath a running server, and which model the site plays should be a
   decision rather than whatever iteration last finished."
  []
  (:weights (settings)))

(defn- command
  [{:keys [players rings blocks filters sims threads cpu serve weights
           eat-threshold require-useful-action sacrifice-yields-nothing]}]
  (cond-> [serve "serve"
           "--players" (str players) "--rings" (str rings)
           "--blocks" (str blocks) "--filters" (str filters)
           "--sims" (str sims) "--threads" (str threads)
           "--weights" (str weights)
           "--eat-threshold" (str eat-threshold)
           "--require-useful-action" (str require-useful-action)
           "--sacrifice-yields-nothing" (str sacrifice-yields-nothing)]
    cpu (conj "--cpu")))

(defn start-server!
  "Launch a move server and return a handle. The libtorch runtime lives in the
   training virtualenv, so the child needs it on its library path."
  [settings]
  (let [builder (ProcessBuilder. ^java.util.List (command settings))
        torch (.getCanonicalPath (io/file (:torch settings)))
        env (.environment builder)]
    (.put env "LD_LIBRARY_PATH"
          (str torch (when-let [existing (System/getenv "LD_LIBRARY_PATH")]
                       (str ":" existing))))
    (.redirectError builder java.lang.ProcessBuilder$Redirect/INHERIT)
    (let [process (.start builder)]
      ;; Nothing else would ever stop it: a child outlives the JVM that started
      ;; it, and it is holding a copy of the weights open.
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. ^Runnable (fn [] (.destroy ^Process process))))
      {:process process
       :settings settings
       :in (io/writer (.getOutputStream process))
       :out (io/reader (.getInputStream process))})))

(defonce ^{:doc "The running move server, started on the first move asked of it."}
  server (atom nil))

(defn ensure-server!
  "The running server, started or restarted as needed. A server that died —
   killed, or out of memory — is replaced on the next move asked for."
  [settings]
  (or (when-let [running @server]
        (when (.isAlive ^Process (:process running)) running))
      (locking server
        (or (when-let [running @server]
              (when (.isAlive ^Process (:process running)) running))
            (reset! server (start-server! settings))))))

(defn ask
  "One request, one reply. Serialized: the pipe carries a single conversation."
  [settings request]
  ;; Locking the atom rather than the handle: a server that dies mid-game is
  ;; replaced, and two threads must not end up writing to two different pipes.
  (locking server
    (let [{:keys [in out]} (ensure-server! settings)]
      (.write ^java.io.Writer in ^String (str (json/write-str request) "\n"))
      (.flush ^java.io.Writer in)
      (let [line (.readLine ^java.io.BufferedReader out)]
        (when-not line
          (reset! server nil)
          (throw (ex-info "move server closed the pipe" {})))
        (json/read-str line)))))

;; ── Choosing a move ─────────────────────────────────────────────────────────

(defn grow-from-choice
  "Fund a growth: the contribution map the server's donor picks add up to.

   The server chooses one donor space per food of growth, so its picks are
   replayed against it until it records a finished allocation."
  [settings game board]
  (let [[_ spaces] (space-index game)]
    (loop [board board
           step 0]
      (let [answer (ask settings {"position" board "echo" true})
            next-board (get answer "next")
            action (last (get (last (get next-board "turns")) "actions"))
            donors (get action "donors")]
        (cond
          donors (into {} (map (fn [[index n]] [(nth spaces (Integer/parseInt index)) n])
                               donors))
          ;; Every pick spends one food of a growth, and a growth is bounded by
          ;; the organism's size, so an unfinished allocation means a desync.
          (or (nil? next-board) (> step 32))
          (throw (ex-info "growth funding did not finish"
                          {:step step :phase (get answer "phase")}))
          :else (recur next-board (inc step)))))))

(defn plays?
  "Whether this is the board the served model was trained for.

   A network is shaped for one board. Handed another it could only answer with
   moves that mean something else, so it declines and the game falls back to a
   bot that plays anything."
  ([game] (plays? game (settings)))
  ([game {:keys [players rings serve weights] :as _settings}]
   (and (= players (count (:turn-order game)))
        (= rings (count (:rings game)))
        (empty? (:mutations game))
        (= 3 (:organism-victory game))
        (= 5 (:capture-limit game))
        (every? (fn [[_ {:keys [starting-spaces]}]] (= 3 (count starting-spaces)))
                (:players game))
        (.exists (io/file weights))
        (.exists (io/file serve)))))

(defn agent-step+key
  "Advance the game by one decision from the trained network. Returns
   [choice-key next-game], the shape the bot loop records for replay."
  ([game] (agent-step+key game (settings)))
  ([game settings]
   (try
     (let [[phase choices] (choice/find-state game)]
       (cond
         (empty? choices) nil

         ;; Not a decision: the game is advancing itself.
         (contains? choices :advance) [:advance (:advance choices)]
         (= 1 (count choices)) (first choices)

         :else
         (let [board (position game)
               key (if (= :grow-from phase)
                     (grow-from-choice settings game board)
                     (choice-for game phase choices
                                 (get (ask settings {"position" board}) "action")))]
           (if (and key (contains? choices key))
             [key (get choices key)]
             (do
               (println "NEURON: no choice matched" (pr-str key) "at" phase
                        "— falling back to the first offered")
               (first choices))))))
     (catch Exception e
       (println "NEURON error:" (.getMessage e))
       nil))))

(defn agent-step
  "Advance the game by one decision. Returns the next game state."
  [game]
  (when-let [[_key next-game] (agent-step+key game)]
    next-game))

(bots/register-bot!
 "organism" "NEURON"
 {:agent-step     agent-step
  :agent-step+key agent-step+key
  :plays?         plays?
  :description "Trained network — self-play on a three-player, four-ring board. Served on CPU."})
