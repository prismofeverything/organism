(ns organism.persist-eridu
  "Eridu-specific game persistence to MongoDB.
   Uses event-sourcing: stores the initial state + sequence of choice keys.
   Any game state can be reconstructed by replaying choices from the initial state."
  (:require
   [eridu.game :as game]
   [eridu.choice :as choice]
   [organism.mongo :as db]))

(defn- player-games-key [key]
  (str "player-games-" key))

;; ── Event log ───────────────────────────────────────────────────────────────

(defn append-action!
  "Append a choice-key to the game's action log."
  [db game-key choice-key]
  (db/insert! db (str "eridu-actions-" game-key)
              {:choice (pr-str choice-key)}))

(defn load-actions
  "Load all choice keys for a game in order."
  [db game-key]
  (mapv #(-> % :choice read-string)
        (db/query db (str "eridu-actions-" game-key) {})))

;; ── Replay ──────────────────────────────────────────────────────────────────

(def ^:private replay-protected-phases
  "Phases where the player made an active choice during live play."
  game/human-protected-phases)

(defn- effective-advance
  "Advance through single-choice non-protected phases during replay."
  [state]
  (choice/advance-through-trivial state replay-protected-phases))

(defn replay
  "Replay a sequence of choice keys from an initial state.
   Returns the final state, or stops early if a choice is invalid."
  [initial-state choice-keys]
  (reduce
   (fn [state ck]
     (let [[_ choices] (choice/find-state-raw state)
           next-state  (get choices ck)]
       (if next-state
         (effective-advance next-state)
         (reduced state))))
   initial-state
   choice-keys))

(defn replay-with-history
  "Replay choice keys from initial state, collecting history entries."
  [initial-state choice-keys]
  (loop [state   initial-state
         ks      choice-keys
         step    0
         history [{:step 0
                   :player (game/current-player initial-state)
                   :phase  (get-in initial-state [:player-turn :phase])
                   :state  initial-state}]]
    (if (empty? ks)
      {:state state :history history}
      (let [[_phase choices] (choice/find-state-raw state)
            next-state       (get choices (first ks))]
        (if next-state
          (let [effective (effective-advance next-state)]
            (recur effective
                   (rest ks)
                   (inc step)
                   (conj history {:step    (inc step)
                                  :player  (game/current-player effective)
                                  :phase   (game/current-phase effective)
                                  :state   effective})))
          {:state state :history history})))))

;; ── Game persistence ────────────────────────────────────────────────────────

(defn save-game!
  "Create or update an eridu game in the database."
  [db game-key state bots players initial-state]
  (db/index! db :eridu-games [:key] {:unique true})
  (db/merge!
   db :eridu-games
   {:key game-key}
   {:state         (pr-str state)
    :initial-state (pr-str initial-state)
    :bots          (pr-str bots)
    :players       (pr-str players)
    :game-type     "eridu"
    :updated       (quot (System/currentTimeMillis) 1000)})
  ;; Update player-games for each player
  (doseq [player players]
    (db/index! db (player-games-key player) [:game] {:unique true})
    (let [current (game/current-player state)
          round   (:round state 0)
          go      (:game-over state)]
      (db/merge!
       db (player-games-key player)
       {:game game-key}
       {:round round
        :status (if go "complete" "active")
        :game-type "eridu"
        :players players
        :current-player current
        :winner (when go (:winner go))
        :last-move-at (quot (System/currentTimeMillis) 1000)}))))

;; ── Offline (human-vs-AI) games ─────────────────────────────────────────────
;;
;; Offline games are stored in :eridu-offline-games separately from live
;; multi-player games. Bench/ML pipelines can read from both. The schema
;; preserves enough to replay any game and to compute feat-synergy /
;; pairwise-claim signals without re-parsing the full state on every query.

(def offline-schema-version 1)

(defn- score-summary [pdata]
  (let [a (:amity pdata 0)
        g (:glory pdata 0)]
    {:amity a :glory g :reputation (min a g)}))

(defn extract-offline-summary
  "Distill final state + initial state into flat analytics fields. Captures
   feat-synergy data: which feats were on offer, claim order, and the
   resulting score per player. Pairwise/groupwise feat synergy can be
   reconstructed downstream from feat-slate × feat-claims-by-player."
  [initial-state final-state]
  (let [players (keys (:players final-state))
        scored  (into {} (map (juxt identity #(score-summary (get-in final-state [:players %])))) players)
        winner  (when-let [go (:game-over final-state)] (:winner go))
        claims  (:contest-claims final-state {})
        ;; Group claims by player so synergy is one lookup per player
        claims-by-player
        (reduce-kv
         (fn [acc feat-id claimers]
           (reduce (fn [m c] (update m c (fnil conj []) feat-id))
                   acc claimers))
         {} claims)]
    {:scores            scored
     :winner            winner
     :players           (vec players)
     :rounds            (:round final-state)
     :feat-slate        (vec (keys (:contests initial-state)))
     :feat-claims       claims
     :feat-claims-by-player claims-by-player
     :bonus-boards      (:bonus-boards initial-state)}))

(defn save-offline-game!
  "Persist a completed offline (human-vs-AI) game.

   `record` must include:
     :game-key    unique id (timestamp-based, supplied by client)
     :human       logged-in username (server validates against session)
     :bot         bot slot name (e.g. \"warlord-bot\")
     :archetype   archetype name (\"warlord\")
     :weights     pr-str'd personality weights snapshot (full reproducibility)
     :initial-state  pr-str'd initial game state
     :actions     pr-str'd vector of choice keys
     :final-state pr-str'd final game state
     :started-at  epoch ms
     :completed-at epoch ms"
  [db record]
  (db/index! db :eridu-offline-games [:game-key] {:unique true})
  (db/index! db :eridu-offline-games [:human] {})
  (db/index! db :eridu-offline-games [:archetype] {})
  (db/index! db :eridu-offline-games [:completed-at] {})
  (let [initial (when-let [s (:initial-state record)] (read-string s))
        final   (when-let [s (:final-state record)] (read-string s))
        summary (when (and initial final) (extract-offline-summary initial final))
        merged  (cond-> (assoc record
                               :game-type "eridu"
                               :source    "offline-human-vs-ai"
                               :version   offline-schema-version)
                  summary (merge summary)
                  true    (update :feat-claims        #(when % (pr-str %)))
                  true    (update :feat-claims-by-player #(when % (pr-str %)))
                  true    (update :feat-slate         #(when % (pr-str %)))
                  true    (update :bonus-boards       #(when % (pr-str %)))
                  true    (update :scores             #(when % (pr-str %))))]
    (db/merge! db :eridu-offline-games {:game-key (:game-key record)} merged)))

(defn load-offline-games
  "Return offline games matching optional filters."
  ([db] (load-offline-games db {}))
  ([db {:keys [human archetype]}]
   (let [where (cond-> {}
                 human     (assoc :human human)
                 archetype (assoc :archetype archetype))]
     (db/query db :eridu-offline-games where))))

(defn load-observe-games
  "Load all active eridu games for the observe page."
  [db]
  (let [all (db/find-all db :eridu-games)]
    (->> all
         (map (fn [doc]
                (let [state (when (:state doc) (read-string (:state doc)))
                      players (when (:players doc) (read-string (:players doc)))
                      bots (when (:bots doc) (read-string (:bots doc)))]
                  {:key            (:key doc)
                   :players        (or players [])
                   :bots           (or bots #{})
                   :current-player (when state (game/current-player state))
                   :round          (when state (:round state 0))
                   :game-over      (when state (:game-over state))
                   :updated        (:updated doc)})))
         (remove :game-over)
         (sort-by #(- (or (:updated %) 0))))))

;; ── Game list / cleanup / leaderboard helpers ───────────────────────────────

(defn- doc->summary
  "Materialize a stored :eridu-games doc into the lightweight summary used by
   game-list and leaderboard pages. Reads state once to avoid repeating the
   pr-str/read-string cycle in callers."
  [doc]
  (let [state   (when (:state doc) (read-string (:state doc)))
        players (when (:players doc) (read-string (:players doc)))
        bots    (when (:bots doc) (read-string (:bots doc)))]
    {:key            (:key doc)
     :players        (or players [])
     :bots           (or bots #{})
     :current-player (when state (game/current-player state))
     :round          (when state (:round state 0))
     :game-over      (when state (:game-over state))
     :updated        (:updated doc)
     :state          state}))

(defn load-player-game-summaries
  "Return the player's games (active + completed) with last-move age and
   whose-turn fields populated, suitable for rendering the games list."
  [db player]
  (let [pg-coll  (player-games-key player)
        records  (db/find-all db pg-coll)
        keys-set (set (keep :game records))
        all-game-docs (db/query db :eridu-games {:key {"$in" (vec keys-set)}})
        by-key   (into {} (map (juxt :key identity) all-game-docs))]
    (->> records
         (map (fn [pg]
                (let [doc (get by-key (:game pg))
                      summary (when doc (doc->summary doc))]
                  (when summary
                    (assoc summary
                           :last-move-at (:last-move-at pg)
                           :status       (:status pg))))))
         (remove nil?)
         (sort-by #(- (or (:last-move-at %) (:updated %) 0))))))

(defn- stale-where
  [cutoff-seconds]
  ;; Doc has no game-over (= active) AND updated < cutoff (or missing).
  {"$and" [{"$or" [{:state {"$exists" true}}
                   {:state nil}]}
           {"$or" [{:updated {"$lt" cutoff-seconds}}
                   {:updated {"$exists" false}}]}]})

(defn stale-games
  "Return summaries of active games whose `updated` timestamp (seconds) is
   older than `cutoff-seconds`. Used for the cleanup preview."
  [db cutoff-seconds]
  (->> (db/query db :eridu-games (stale-where cutoff-seconds))
       (map doc->summary)
       (remove :game-over)
       (sort-by :updated)))

(defn delete-stale-games!
  "Hard-delete stale active games (and the per-player references). Returns
   the number deleted. Completed games are left alone — those are kept for
   the leaderboard."
  [db cutoff-seconds]
  (let [victims (stale-games db cutoff-seconds)]
    (doseq [{:keys [key players]} victims]
      (doseq [player players]
        (db/delete! db (player-games-key player) {:game key}))
      (db/delete! db :eridu-games {:key key}))
    (count victims)))

(defn delete-game!
  "Delete a specific game by key (active or complete), including per-player
   references. Returns true if anything was removed."
  [db game-key]
  (when-let [doc (db/one db :eridu-games {:key game-key})]
    (let [players (when (:players doc) (read-string (:players doc)))]
      (doseq [player (or players [])]
        (db/delete! db (player-games-key player) {:game game-key}))
      (db/delete! db :eridu-games {:key game-key})
      true)))

(defn- player-final-scores
  "Pull final reputation scores per player from a finished game's state."
  [state]
  (when (:game-over state)
    (into {}
          (for [[pk pdata] (:players state)
                :let [amity (:amity pdata 0)
                      glory (:glory pdata 0)]]
            [pk {:amity amity :glory glory
                 :reputation (min amity glory)}]))))

(defn leaderboard-data
  "Compute per-player aggregates and a hall-of-fame of top single-game
   reputation scores. Reads both multiplayer (:eridu-games) and offline
   (:eridu-offline-games) collections. Limit hall-of-fame to top 50 by score."
  [db]
  (let [mp-docs      (db/find-all db :eridu-games)
        mp-summaries (map doc->summary mp-docs)
        completed-mp (filter :game-over mp-summaries)
        offline-docs (db/find-all db :eridu-offline-games)
        ;; Per-player aggregator
        agg (atom {})
        bump! (fn [pk delta]
                (swap! agg update pk (fnil (partial merge-with +) {})
                       delta))
        hall (atom [])]
    ;; Multiplayer completed games
    (doseq [{:keys [key players state game-over updated] :as g} completed-mp
            :let [scores (player-final-scores state)
                  winner (:winner game-over)]]
      (doseq [pk players
              :let [s (get scores pk)]
              :when s]
        (bump! pk {:games 1
                   :wins (if (= pk winner) 1 0)
                   :reputation-sum (:reputation s)
                   :amity-sum (:amity s)
                   :glory-sum (:glory s)})
        (swap! hall conj {:player pk
                          :game-key key
                          :game-type "multiplayer"
                          :reputation (:reputation s)
                          :amity (:amity s)
                          :glory (:glory s)
                          :winner? (= pk winner)
                          :at updated})))
    ;; Offline-vs-AI completed games — `:scores` is a pr-str'd map
    (doseq [doc offline-docs
            :let [scores (when (:scores doc) (read-string (:scores doc)))
                  winner (:winner doc)
                  human  (:human doc)
                  at     (or (:completed-at doc)
                             (when-let [c (:completed-at doc)] c))]]
      (doseq [[pk s] (or scores {})
              :let [reputation (:reputation s)]
              :when reputation]
        (bump! pk {:games 1
                   :wins (if (= pk winner) 1 0)
                   :reputation-sum reputation
                   :amity-sum (:amity s 0)
                   :glory-sum (:glory s 0)})
        (swap! hall conj {:player pk
                          :game-key (:game-key doc)
                          :game-type (if (= pk human) "solo (human)" "solo (AI)")
                          :reputation reputation
                          :amity (:amity s 0)
                          :glory (:glory s 0)
                          :winner? (= pk winner)
                          :at at})))
    {:aggregate (->> @agg
                     (map (fn [[pk stats]]
                            (let [games (max 1 (:games stats 0))]
                              (merge {:player pk
                                      :games (:games stats 0)
                                      :wins (:wins stats 0)
                                      :win-rate (double (* 100 (/ (:wins stats 0) games)))
                                      :avg-reputation (double (/ (:reputation-sum stats 0) games))
                                      :avg-amity (double (/ (:amity-sum stats 0) games))
                                      :avg-glory (double (/ (:glory-sum stats 0) games))}))))
                     (sort-by (juxt #(- (:wins %))
                                    #(- (:avg-reputation %))))
                     vec)
     :hall-of-fame (->> @hall
                        (sort-by #(- (:reputation %)))
                        (take 50)
                        vec)}))

(defn load-game
  "Load an eridu game from the database."
  [db game-key]
  (when-let [doc (db/one db :eridu-games {:key game-key})]
    (let [state   (when (:state doc) (read-string (:state doc)))
          initial (when (:initial-state doc) (read-string (:initial-state doc)))
          bots    (when (:bots doc) (read-string (:bots doc)))
          players (when (:players doc) (read-string (:players doc)))
          actions (when initial (load-actions db game-key))
          {:keys [history]} (when (and initial (seq actions))
                              (replay-with-history initial actions))]
      {:state         state
       :initial-state (or initial state)
       :bots          (or bots #{})
       :players       (or players [])
       :history       (or history [])})))
