(ns organism.persist-journey
  "Journey-specific game persistence to MongoDB.
   Uses event-sourcing: stores the initial state + sequence of choice keys.
   Any game state can be reconstructed by replaying choices from the initial state."
  (:require
   [journey.game :as game]
   [journey.choice :as choice]
   [organism.mongo :as db]))

(defn- player-games-key [key]
  (str "player-games-" key))

;; ── Event log ───────────────────────────────────────────────────────────────

(defn append-action!
  "Append a choice-key to the game's action log."
  [db game-key choice-key]
  (db/insert! db (str "journey-actions-" game-key)
              {:choice (pr-str choice-key)}))

(defn load-actions
  "Load all choice keys for a game in order."
  [db game-key]
  (mapv #(-> % :choice read-string)
        (db/query db (str "journey-actions-" game-key) {})))

;; ── Replay ──────────────────────────────────────────────────────────────────

(defn replay
  "Replay a sequence of choice keys from an initial state.
   Uses find-state (with auto-advance) to match how actions were recorded.
   Returns the final state, or stops early if a choice is invalid."
  [initial-state choice-keys]
  (reduce
   (fn [state ck]
     (let [[_ choices] (choice/find-state state)
           next-state  (get choices ck)]
       (if next-state
         next-state
         (reduced state))))
   initial-state
   choice-keys))

(defn replay-with-history
  "Replay choice keys from initial state, collecting history entries.
   Uses find-state (with auto-advance) to match how actions were recorded.
   Returns {:state final-state :history [{:step :player :phase} ...]}"
  [initial-state choice-keys]
  (loop [state   initial-state
         ks      choice-keys
         step    0
         history [{:step 0
                   :player (game/current-player initial-state)
                   :phase  (get-in initial-state [:player-turn :phase])}]]
    (if (empty? ks)
      {:state state :history history}
      (let [[phase choices] (choice/find-state state)
            next-state      (get choices (first ks))]
        (if next-state
          (recur next-state
                 (rest ks)
                 (inc step)
                 (conj history {:step    (inc step)
                                :player  (game/current-player next-state)
                                :phase   phase}))
          {:state state :history history})))))

;; ── Game persistence ────────────────────────────────────────────────────────

(defn save-game!
  "Create or update a journey game in the database.
   Stores initial state (for replay), current state (for fast load),
   plus bots and player list."
  [db game-key state bots players initial-state]
  (db/index! db :journey-games [:key] {:unique true})
  (db/merge!
   db :journey-games
   {:key game-key}
   {:state         (pr-str state)
    :initial-state (pr-str initial-state)
    :bots          (pr-str bots)
    :players       (pr-str players)
    :game-type     "journey"
    :updated       (quot (System/currentTimeMillis) 1000)})
  ;; Update player-games for each player
  (doseq [player players]
    (db/index! db (player-games-key player) [:game] {:unique true})
    (let [current (-> state :player-turn :player)
          round   (:round state 0)
          go      (:game-over state)]
      (db/merge!
       db (player-games-key player)
       {:game game-key}
       {:round round
        :status (if go "complete" "active")
        :game-type "journey"
        :players players
        :current-player current
        :winner (when go (first (:winners go)))
        :last-move-at (quot (System/currentTimeMillis) 1000)}))))

(defn load-observe-games
  "Load all active (non-complete) journey games for the observe page."
  [db]
  (let [all (db/find-all db :journey-games)]
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
                   :flares         (when state (:flares-drawn state 0))
                   :game-over      (when state (:game-over state))
                   :updated        (:updated doc)})))
         (remove :game-over)
         (sort-by #(- (or (:updated %) 0))))))

(defn load-game
  "Load a journey game from the database.
   Returns {:state :initial-state :bots :players :history} or nil.
   Replays actions from initial-state to reconstruct full history."
  [db game-key]
  (when-let [doc (db/one db :journey-games {:key game-key})]
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
