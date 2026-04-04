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
  #{:choose-action :choose-space-action :choose-deploy-city
    :choose-travel-destination :choose-build-city :choose-influence-role
    :choose-temple-city :game-over})

(defn- effective-advance
  "Apply the same effective loop as the server: advance through single-choice
   non-protected phases."
  [state]
  (loop [s state]
    (let [p  (game/current-phase s)
          cs (second (choice/find-state-raw s))]
      (if (and (= 1 (count cs))
               (not (contains? replay-protected-phases p)))
        (let [ns (first (vals cs))]
          (if ns (recur ns) s))
        s))))

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
