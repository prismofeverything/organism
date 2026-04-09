(ns organism.persist-journey-bots
  "MongoDB CRUD for player-defined journey bots.

   A bot is a flowchart definition (see journey.bot-flow) saved per player by
   name. Bot names are stored in upper case and uniquely identify a bot within
   the journey collection. Any player can join a game with another player's bot
   by typing its name in the create form."
  (:require
   [clojure.string :as str]
   [organism.mongo :as db]))

(def collection :journey-bots)

(defn- normalize-name [name]
  (-> (or name "") str/trim str/upper-case))

(defn- read-definition [doc]
  (when doc
    (let [s (:definition doc)]
      (cond
        (string? s) (read-string s)
        (map? s)    s
        :else       nil))))

(defn- ->record [doc]
  (when doc
    {:name        (:name doc)
     :owner       (:owner doc)
     :game-type   (:game-type doc)
     :updated     (:updated doc)
     :description (:description doc)
     :definition  (read-definition doc)}))

(defn ensure-indexes! [db]
  (db/index! db collection [:name] {:unique true}))

(defn save-bot!
  "Insert or update a bot. `bot` is a map of {:name :owner :description :definition}.
   Returns the normalized name actually stored."
  [db {:keys [name owner description definition]}]
  (ensure-indexes! db)
  (let [n (normalize-name name)]
    (when (and (seq n) definition)
      (db/merge!
       db collection
       {:name n}
       {:name        n
        :owner       owner
        :game-type   "journey"
        :description (or description "")
        :definition  (pr-str definition)
        :updated     (quot (System/currentTimeMillis) 1000)})
      n)))

(defn find-bot
  "Look up a bot by name (case-insensitive)."
  [db name]
  (->record (db/one db collection {:name (normalize-name name)})))

(defn list-bots
  "Return all journey bots, optionally filtered by owner."
  ([db]       (mapv ->record (db/find-all db collection)))
  ([db owner] (mapv ->record (db/query db collection {:owner owner}))))

(defn delete-bot!
  [db name]
  (db/delete! db collection {:name (normalize-name name)}))
