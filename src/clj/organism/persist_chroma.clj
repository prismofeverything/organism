(ns organism.persist-chroma
  "Chroma game persistence to MongoDB.

   Unlike Eridu (event-sourced), Chroma snapshots the whole server-game map each
   turn. Chroma state is tiny (91 cells x <=3 chits + a handful of players) and its
   per-turn RNG + simultaneous bot resolution make choice-replay fragile, so a
   straight snapshot is both simpler and more robust. The snapshot is the full
   server record: {:state <engine-G> :phase ... :pending-* ... :bots ... :players
   ... :chat ...}, stored pr-str'd under :chroma-games keyed by play-key."
  (:require
   [organism.mongo :as db]))

(defn- player-games-key [player]
  (str "player-games-chroma-" player))

;; ── snapshot persistence ─────────────────────────────────────────────────────

(defn save-game!
  "Upsert the full Chroma server-game snapshot. `server` is the in-memory game
   record (sans :channels, which are not serializable). `players` is the seat->name
   vector; `human-players` are the names that should see this game in their list."
  [db game-key server]
  (db/index! db :chroma-games [:key] {:unique true})
  (let [state   (:state server)
        over?   (get-in state [:over])
        players (:players server)
        humans  (->> (map-indexed vector players)
                     (remove (fn [[i _]] (contains? (:bots server) i)))
                     (map second)
                     (remove nil?))]
    (db/merge!
     db :chroma-games
     {:key game-key}
     {:snapshot   (pr-str (dissoc server :channels :channel-players))
      :game-type  "chroma"
      :turn       (:turn state)
      :phase      (name (or (:phase server) :place))
      :over       (boolean over?)
      :players    (pr-str players)
      :updated    (quot (System/currentTimeMillis) 1000)})
    (doseq [player humans]
      (db/index! db (player-games-key player) [:game] {:unique true})
      (db/merge!
       db (player-games-key player)
       {:game game-key}
       {:status       (if over? "complete" "active")
        :game-type    "chroma"
        :players      (pr-str players)
        :turn         (:turn state)
        :last-move-at (quot (System/currentTimeMillis) 1000)}))))

(defn load-game
  "Load a Chroma server-game snapshot. Returns the record (with an empty
   :channels set re-attached) or nil."
  [db game-key]
  (when-let [doc (db/one db :chroma-games {:key game-key})]
    (when-let [snap (:snapshot doc)]
      (assoc (read-string snap) :channels #{} :channel-players {}))))

(defn delete-game!
  "Delete a Chroma game and its per-player references. Returns true if removed."
  [db game-key]
  (when-let [doc (db/one db :chroma-games {:key game-key})]
    (let [players (when (:players doc) (read-string (:players doc)))]
      (doseq [player (or players [])]
        (db/delete! db (player-games-key player) {:game game-key}))
      (db/delete! db :chroma-games {:key game-key})
      true)))

;; ── listings ─────────────────────────────────────────────────────────────────

(defn- doc->summary [doc]
  {:key     (:key doc)
   :players (when (:players doc) (read-string (:players doc)))
   :turn    (:turn doc)
   :phase   (:phase doc)
   :over    (:over doc)
   :updated (:updated doc)})

(defn load-player-game-summaries
  "Games (active + complete) a player participates in, newest activity first."
  [db player]
  (let [records  (db/find-all db (player-games-key player))
        keys-set (vec (set (keep :game records)))
        docs     (when (seq keys-set) (db/query db :chroma-games {:key {"$in" keys-set}}))
        by-key   (into {} (map (juxt :key identity) docs))]
    (->> records
         (keep (fn [pg]
                 (when-let [doc (get by-key (:game pg))]
                   (assoc (doc->summary doc)
                          :status (:status pg)
                          :last-move-at (:last-move-at pg)))))
         (sort-by #(- (or (:last-move-at %) (:updated %) 0))))))

(defn load-observe-games
  "All active Chroma games for an observe/list page."
  [db]
  (->> (db/find-all db :chroma-games)
       (map doc->summary)
       (remove :over)
       (sort-by #(- (or (:updated %) 0)))))

(defn completed-games
  "All finished Chroma games, each as the full server snapshot (with :game-key and
   :updated re-attached). Used to compute the leaderboard."
  [db]
  (->> (db/find-all db :chroma-games)
       (filter :over)
       (keep (fn [doc]
               (when-let [s (:snapshot doc)]
                 (assoc (read-string s) :game-key (:key doc) :updated (:updated doc)))))))
