(ns organism.routes.chroma-ws
  "Server-authoritative Chroma over websockets, mirroring the Eridu ws infra but
   built around Chroma's SIMULTANEOUS turn structure (all seats place at once, then
   draws + swaps resolve together). A turn is a two-phase machine:

     :place — wait until every human seat has submitted a placement (or pass);
              bots decide at resolution time. Then apply all placements + resolve
              draws and move to :swap.
     :swap  — wait until every human seat has submitted a swap (or skip); bots
              decide too. Resolve swaps, do end-of-turn bookkeeping, advance the
              turn, and return to :place (or :over).

   The whole engine runs on the server; the cljs client is a thin renderer fed a
   precomputed `view`. State is snapshot-persisted to Mongo each transition so a
   refresh resumes exactly (engine has no JS/localStorage anymore)."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [org.httpkit.server :as hk]
   [organism.chroma.engine :as e]
   [organism.persist-chroma :as persist-c])
  (:import [java.io ByteArrayOutputStream]))

;; ── transit helpers (same as eridu_ws) ───────────────────────────────────────

(defn- ->stream [input]
  (if (string? input) (io/input-stream (.getBytes ^String input)) input))

(defn read-json [input]
  (with-open [ins (->stream input)]
    (-> ins (transit/reader :json) transit/read)))

(defn write-json [output]
  (let [out (ByteArrayOutputStream. 4096)
        w   (transit/writer out :json)]
    (transit/write w output)
    (let [s (.toString out)] (.reset out) s)))

(defn send! [channel message] (hk/send! channel (write-json message)))
(defn send-channels! [channels message] (doseq [ch channels] (send! ch message)))

;; ── games atom ────────────────────────────────────────────────────────────────

(defonce games (atom {:games {}}))

;; GA-evolved bot genomes (from chroma-mock.html). Keyword-keyed for the engine.
(def evolved-bots
  [{:cmyFocus 1.7 :rgbFocus 0.24 :colorLock 0 :mudRush 1.6 :blankPriority 1.8
    :capPriority -0.75 :edgeCenterPref 0 :bridgeWeight 0.44 :growRegion 1.13
    :targetDraw 1.5 :anyDraw 0.67 :spendTargetPen 2.02 :dryAversion 0.2
    :mudAversion 0.75 :earlySwap 0 :lateSwap 0 :cycleLock 0}
   {:cmyFocus 1.07 :rgbFocus 0 :colorLock 0 :mudRush 0 :blankPriority 1.02
    :capPriority 0.09 :edgeCenterPref -0.47 :bridgeWeight 0.33 :growRegion 1.98
    :targetDraw 0.47 :anyDraw 0.67 :spendTargetPen 2.71 :dryAversion 2.41
    :mudAversion 0.75 :earlySwap 0 :lateSwap 0 :cycleLock 0.05}
   {:cmyFocus 1.07 :rgbFocus 0 :colorLock 0 :mudRush 0.18 :blankPriority 1.72
    :capPriority 0 :edgeCenterPref 0 :bridgeWeight 1.09 :growRegion 1.02
    :targetDraw 1.5 :anyDraw 0.67 :spendTargetPen 1.39 :dryAversion 0.9
    :mudAversion 0.81 :earlySwap 0 :lateSwap 0 :cycleLock 0}])

;; ── seat helpers ──────────────────────────────────────────────────────────────

(defn- seats [server] (range (:N (:state server))))
(defn- human-seat? [server i] (not (contains? (:bots server) i)))
(defn- human-seats [server] (filter #(human-seat? server %) (seats server)))

(defn- seat-of-player
  "Seat index for a connected player name (a non-bot seat whose name matches), or nil."
  [server player]
  (first (for [i (human-seats server) :when (= (nth (:players server) i nil) player)] i)))

;; ── view computation (everything the thin client renders) ─────────────────────

(defn- cell-view [G stacks c]
  (let [st (get stacks c [])
        [x y] (e/ax->px (first c) (second c))]
    {:c c :x x :y y
     :sector (e/sector (first c) (second c))
     :removed (boolean (contains? (:removed G #{}) c))
     :stack st
     :color (when (seq st) (e/classify (e/mix-stack st)))
     :rgb (e/display-rgb st)}))

(defn- you-view [server G you]
  (when (and you (not (:over G)))
    (let [p (get-in G [:players you])
          phase (:phase server)
          swap? (= phase :swap)
          mud? (and swap? (boolean (contains? (:turn-mudded server) you)))]
      ;; swap info is ALWAYS present so the client can keep a persistent hand + swap
      ;; area on screen; :canSwap gates whether the options are actionable this step
      ;; (sequence of play: place first, then swap). During placement the swaps are a
      ;; preview of the normal (non-mud) options on the current hand.
      (cond-> {:seat you :hand (:hand p)
               :wedge (e/wedge-of p (:turn G))
               :phase (name phase)
               :canSwap swap?
               :madeMud mud?
               :availableSwaps (e/available-swaps (:hand p) (:bag G) (:stacks G) mud?)
               :submitted (boolean (or (contains? (:pending-placements server) you)
                                       (and swap? (contains? (:pending-swaps server) you))))}
        (= phase :place)
        (assoc :legal (mapv :k (e/enumerate-moves G you))
               :moves (e/enumerate-moves G you)
               :canPass (e/can-pass G you))))))

(defn- view [server you]
  (let [G (:state server)]
    (e/with-config G
      (let [stacks (:stacks G)
            over? (:over G)
            scores (when over? (e/score-game (:players G) stacks))
            pal (e/palettes (or (:palette G) :CMY))
            ord (:order pal)
            reference (vec (for [row (cons "blank" ord)]
                             {:row row
                              :cells (vec (for [col ord]
                                            {:col col
                                             :res (e/classify (e/mix-stack (if (= row "blank") [col] [row col])))}))}))]
        {:type "game-state"
         :key (:key server)
         :phase (name (:phase server))
         :turn (:turn G)
         :over over?
         :ending (:ending G)
         :palette {:key (name (or (:palette G) :CMY))
                   :order ord :chip (:chip pal) :name (:name pal)}
         :reference reference
         :regions (e/largest-regions stacks)
         :maxDry (reduce max 0 (map :dry (:players G)))
         :board (mapv #(cell-view G stacks %) e/cells)
         :seats (mapv (fn [i]
                        (let [p (nth (:players G) i)]
                          {:seat i :name (:name p) :isBot (boolean (contains? (:bots server) i))
                           :handCount (count (:hand p)) :wedge (e/wedge-of p (:turn G))
                           :dry (:dry p) :swaps (:swaps p) :target (:target p)
                           :waiting (and (not over?)
                                         (human-seat? server i)
                                         (case (:phase server)
                                           :place (not (contains? (:pending-placements server) i))
                                           :swap  (not (contains? (:pending-swaps server) i))
                                           false))}))
                      (seats server))
         :bag (:bag G) :bagTotal (e/bag-total (:bag G))
         :scores scores
         :log (vec (take-last 40 (:log server)))
         :you (you-view server G you)}))))

(defn broadcast! [play-key]
  (let [server (get-in @games [:games play-key])]
    (when (:state server)
      (doseq [ch (:channels server)]
        ;; each channel may belong to a different player; send a per-channel view
        (let [player (get-in @games [:games play-key :channel-players ch])
              you (seat-of-player server player)]
          (send! ch (view server you)))))))

(defn- save! [db play-key]
  (when-let [server (get-in @games [:games play-key])]
    (when (:state server)
      (try
        (persist-c/save-game! db play-key server)
        ;; finished games are also archived under their unique :game-id so completed
        ;; history accumulates across games AND devices (the live doc keyed by play-key
        ;; gets reused by the next game). Idempotent: re-saving an over game upserts
        ;; the same archive row.
        (when (get-in server [:state :over])
          (persist-c/archive-completed! db server))
        (catch Exception e
          ;; persistence is best-effort: a Mongo outage must not break live play.
          ;; the game stays in memory and keeps running; it just won't survive a
          ;; server restart until the DB is back.
          (log/warn e "Chroma save-game! failed; game continues in memory only" play-key))))))

;; ── logging ──────────────────────────────────────────────────────────────────

(defn- logln [server & parts]
  (update server :log (fnil conj []) (apply str parts)))

;; ── turn resolution ───────────────────────────────────────────────────────────

(defn- valid-human-move?
  "Is move (a {:c :chit :k}) a legal placement for seat i right now?"
  [G i move]
  (some #(and (= (:c %) (:c move)) (= (:chit %) (:chit move)))
        (e/enumerate-moves G i)))

(defn- resolve-placements
  "Apply all seats' placements (bots decide, humans from pending), resolve draws,
   and move to the :swap phase. Returns the updated server map."
  [server]
  (let [G0 (:state server)]
    (e/with-config G0
      (let [rng (e/make-rng (e/turn-seed (:seed G0) (:turn G0)))
            [G1 plc]
            (reduce
             (fn [[G plc] i]
               (let [move (if (human-seat? server i)
                            (let [pp (get (:pending-placements server) i)]
                              (when (and pp (not= pp :pass) (valid-human-move? G i pp)) pp))
                            (e/decide G i rng))]
                 (if move
                   (let [[G' rec] (e/apply-placement G i move)] [G' (conj plc rec)])
                   [G plc])))
             [G0 []] (seats server))
            [G2 plc'] (e/resolve-draws G1 plc)
            mudded (set (keep #(when (= (:res %) "mud") (:pi %)) plc'))
            placed (set (map :pi plc'))
            server' (-> server
                        (assoc :state G2 :phase :swap
                               :turn-mudded mudded :turn-placed placed
                               :turn-plays plc' :pending-swaps {})
                        (logln "— turn " (:turn G2) " placements: "
                               (if (seq plc')
                                 (str/join "  "
                                   (map #(str "P" (:pi %) "→" (or (:color %) "·")
                                              "=" (or (:res %) "·")
                                              (cond (= (:dry %) "mud") " (mud)"
                                                    (:drew %) (str " +" (:drew %))
                                                    (:dry %) (str " (" (:dry %) ")") :else "")) plc'))
                                 "(all passed)")))]
        ;; auto-skip humans with no available swap and no mud (nothing to decide)
        (reduce (fn [s i]
                  (if (and (human-seat? s i)
                           (not (contains? (:turn-mudded s) i))
                           (empty? (e/available-swaps (get-in (:state s) [:players i :hand])
                                                      (:bag (:state s)) (:stacks (:state s)) false)))
                    (assoc-in s [:pending-swaps i] :skip)
                    s))
                server' (human-seats server'))))))

(defn- resolve-swaps-phase
  "Gather all seats' swaps (bots decide, humans from pending), resolve, do
   end-of-turn bookkeeping, advance the turn and re-enter :place (or finish)."
  [server]
  (let [G0 (:state server)]
    (e/with-config G0
      (let [reg (e/largest-regions (:stacks G0))
            plans (keep
                   (fn [i]
                     (if (human-seat? server i)
                       (let [ps (get (:pending-swaps server) i)]
                         (when (and (map? ps) (not= ps :skip)) (assoc ps :pi i)))
                       (e/decide-swap G0 i reg (boolean (contains? (:turn-mudded server) i)))))
                   (seats server))
            [G1 granted _denied] (if (seq plans) (e/resolve-swaps G0 (vec plans)) [G0 [] []])
            ;; passedLast bookkeeping from the placement phase
            placed (:turn-placed server)
            G2 (reduce (fn [g j] (assoc-in g [:players j :passedLast] (not (contains? placed j))))
                       G1 (seats server))
            G3 (update G2 :turn inc)
            ;; end-condition check
            G4 (cond
                 (:over G3) G3
                 (and (:ending G3) (> (:turn G3) (:finalTurn G3))) (assoc G3 :over true)
                 :else (let [max-dry (reduce max (map :dry (:players G3)))
                             bt (e/bag-total (:bag G3))]
                         (if (and (not (:ending G3)) (or (>= max-dry e/MUD_LIMIT) (zero? bt)))
                           (assoc G3 :ending true :finalTurn (:turn G3))
                           G3)))
            server' (-> server
                        (assoc :state G4 :phase (if (:over G4) :over :place)
                               :pending-placements {} :pending-swaps {}
                               :turn-mudded #{} :turn-placed #{} :turn-plays [])
                        (cond-> (seq granted)
                          (logln "   swaps: "
                                 (str/join "  "
                                   (map #(str "P" (:pi %) " " (str/join "+" (:discards %))
                                              "→" (:get %)) granted))))
                        (cond-> (:over G4)
                          (logln "=== game over after " (:turn G4) " turns ===")))]
        server'))))

(declare maybe-advance!)

(defn- auto-pass-forced
  "At :place entry, auto-pass any human seat that has no legal placement (their
   only legal action is to pass), so the human isn't asked to click pass."
  [server]
  (let [G (:state server)]
    (e/with-config G
      (reduce (fn [s i]
                (if (and (human-seat? s i)
                         (not (contains? (:pending-placements s) i))
                         (empty? (e/enumerate-moves (:state s) i)))
                  (assoc-in s [:pending-placements i] :pass)
                  s))
              server (human-seats server)))))

(defn- maybe-resolve
  "Drive the phase machine forward as far as it can go without further human input.
   Returns the advanced server map."
  [server]
  (loop [server (auto-pass-forced server)]
    (cond
      (= :over (:phase server)) server
      (and (= :place (:phase server))
           (every? #(contains? (:pending-placements server) %) (human-seats server)))
      (recur (resolve-placements server))
      (and (= :swap (:phase server))
           (every? #(contains? (:pending-swaps server) %) (human-seats server)))
      (recur (auto-pass-forced (resolve-swaps-phase server)))
      :else server)))

;; ── all-bot auto-run (observe / no human seats) ───────────────────────────────

(defn- run-bot-game! [db play-key]
  (future
    (try
      (loop []
        (let [server (get-in @games [:games play-key])]
          (when (and server (:state server) (not (:over (:state server)))
                     (empty? (human-seats server)))
            (Thread/sleep 250)
            (swap! games update-in [:games play-key] maybe-resolve)
            (broadcast! play-key)
            (save! db play-key)
            (recur))))
      (catch Exception ex (log/error ex "chroma bot-game loop")))))

;; ── message handlers ──────────────────────────────────────────────────────────

(defn- new-server-game
  [play-key {:keys [players bots palette depth trim seed]
             :or {palette "CMY" depth 3 trim true}}]
  (let [n (max 1 (count players))
        bot-set (set bots)
        pal-kw (keyword palette)
        seed (or seed (mod (System/nanoTime) 1000000007))
        specs (mapv (fn [i]
                      (if (contains? bot-set i)
                        {:isBot true :g (nth evolved-bots (mod i (count evolved-bots)))
                         :name (nth players i (str "Bot " (inc i)))}
                        {:isBot false :name (nth players i (str "Player " (inc i)))}))
                    (range n))
        G (e/new-game specs {:palette pal-kw :depth depth :seed seed
                             :removed (if trim e/trim-cells #{})})]
    {:key play-key
     ;; unique per-game id: the live doc is keyed by play-key (reused for resume),
     ;; but each finished game is archived under this id so history accumulates.
     :game-id (str play-key "-" (System/nanoTime))
     :state G :phase :place :bots bot-set
     :players (mapv #(nth players % (str "seat-" %)) (range n))
     :pending-placements {} :pending-swaps {}
     :turn-mudded #{} :turn-placed #{} :turn-plays []
     :log [(str "New game · " n "p · palette " palette " · depth " depth)]
     :channels (get-in @games [:games play-key :channels] #{})
     :channel-players (get-in @games [:games play-key :channel-players] {})}))

(defn handle-create! [db play-key message]
  (let [base (new-server-game play-key message)
        server (maybe-resolve base)]
    (swap! games assoc-in [:games play-key]
           (merge server {:channels (get-in @games [:games play-key :channels] #{})
                          :channel-players (get-in @games [:games play-key :channel-players] {})}))
    (broadcast! play-key)
    (save! db play-key)
    (when (empty? (human-seats server)) (run-bot-game! db play-key))))

(defn handle-place! [db play-key player {:keys [c chit pass]}]
  (let [server (get-in @games [:games play-key])
        seat (seat-of-player server player)]
    (when (and server seat (= :place (:phase server)) (not (:over (:state server))))
      (let [entry (if pass :pass {:c (vec c) :chit chit :k (vec c)})]
        (swap! games update-in [:games play-key]
               (fn [s] (maybe-resolve (assoc-in s [:pending-placements seat] entry))))
        (broadcast! play-key)
        (save! db play-key)))))

(defn handle-swap! [db play-key player {:keys [skip swap-type discards get]}]
  (let [server (get-in @games [:games play-key])
        seat (seat-of-player server player)]
    (when (and server seat (= :swap (:phase server)) (not (:over (:state server))))
      (let [entry (if skip :skip {:type swap-type :discards (vec discards) :get get})]
        (swap! games update-in [:games play-key]
               (fn [s] (maybe-resolve (assoc-in s [:pending-swaps seat] entry))))
        (broadcast! play-key)
        (save! db play-key)))))

(defn handle-chat! [db play-key player {:keys [message]}]
  (let [msg {:type "chat" :player player :time (quot (System/currentTimeMillis) 1000) :message message}]
    (swap! games update-in [:games play-key :log] (fnil conj []) (str player ": " message))
    (send-channels! (get-in @games [:games play-key :channels]) msg)))

;; ── websocket lifecycle ───────────────────────────────────────────────────────

(defn- find-or-load! [db play-key channel player]
  (let [existing (get-in @games [:games play-key])]
    (if (and existing (:state existing))
      (swap! games (fn [gs] (-> gs
                                (update-in [:games play-key :channels] (fnil conj #{}) channel)
                                (assoc-in [:games play-key :channel-players channel] player))))
      (if-let [loaded (try
                        (persist-c/load-game db play-key)
                        (catch Exception e
                          ;; a DB read failure must NOT hang the client on "Loading…":
                          ;; fall through to the fresh-game branch, which makes connect!
                          ;; send {:type "no-game"} and surface the create UI.
                          (log/warn e "Chroma load-game failed; serving a fresh game so Play can't hang on Loading" play-key)
                          nil))]
        (swap! games assoc-in [:games play-key]
               (assoc loaded :channels #{channel} :channel-players {channel player}
                      :phase (or (:phase loaded) :place)))
        (swap! games assoc-in [:games play-key]
               {:key play-key :state nil :phase :place :channels #{channel}
                :channel-players {channel player} :log []})))
    (get-in @games [:games play-key])))

(defn connect! [{:keys [db play-key player]} channel]
  (log/info "Chroma CONNECT" player play-key)
  (let [server (find-or-load! db play-key channel player)]
    (if (:state server)
      (do (send! channel (view server (seat-of-player server player)))
          (when (and (not (:over (:state server))) (empty? (human-seats server)))
            (run-bot-game! db play-key)))
      (send! channel {:type "no-game" :key play-key}))))

(defn disconnect! [{:keys [play-key player]} channel _status]
  (log/info "Chroma DISCONNECT" player play-key)
  (swap! games
         (fn [gs]
           (let [remaining (disj (set (get-in gs [:games play-key :channels])) channel)]
             (if (empty? remaining)
               (update-in gs [:games] dissoc play-key) ; drop from memory; Mongo has the snapshot
               (-> gs
                   (assoc-in [:games play-key :channels] remaining)
                   (update-in [:games play-key :channel-players] dissoc channel)))))))

(defn notify-clients! [{:keys [db play-key player]} _channel raw]
  (let [{:keys [type] :as message} (read-json raw)]
    (log/info "Chroma MSG" type player)
    (case type
      "create" (handle-create! db play-key message)
      "place"  (handle-place! db play-key player message)
      "swap"   (handle-swap! db play-key player message)
      "chat"   (handle-chat! db play-key player message)
      (log/warn "Unknown chroma message type" type))))

(defn websocket-callbacks [db player play-key]
  (let [cfg {:db db :player player :play-key play-key}]
    {:on-open (partial connect! cfg)
     :on-close (partial disconnect! cfg)
     :on-receive (partial notify-clients! cfg)}))

(defn ws-handler [db {:keys [path-params session] :as request}]
  (let [play (:play path-params)
        ;; identity falls back to the play-key (stable per page) so a logged-out
        ;; player still matches their seat in seat-of-player.
        player (or (:player session) play)]
    (hk/as-channel request (websocket-callbacks db player play))))

(defn chroma-ws-routes [db]
  [["/ws/chroma/play/:play" (partial ws-handler db)]])
