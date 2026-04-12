(ns organism.persist-journey-bots
  "MongoDB CRUD for player-defined flowchart bots.

   Bots are stored in a single shared `game-bots` collection with a unique
   compound index on [:game-type :name].  The same bot name can exist for
   different games (e.g. OBO for journey and OBO for organism).

   Bot names are stored in upper case.  Any player can use any bot by name
   in the create-game form; the :owner field controls who sees it on their
   bots management page and who can edit/delete it."
  (:require
   [clojure.string :as str]
   [organism.mongo :as db]))

(def collection :game-bots)

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
  (db/index! db collection [:game-type :name] {:unique true}))

(defn save-bot!
  "Insert or update a bot.  The compound key is (game-type, name).
   `bot` is a map with :name :game-type :owner :description :definition."
  [db {:keys [name game-type owner description definition]}]
  (ensure-indexes! db)
  (let [n  (normalize-name name)
        gt (or game-type "journey")]
    (when (and (seq n) definition)
      (db/merge!
       db collection
       {:game-type gt :name n}
       {:name        n
        :game-type   gt
        :owner       owner
        :description (or description "")
        :definition  (pr-str definition)
        :updated     (quot (System/currentTimeMillis) 1000)})
      n)))

(defn find-bot
  "Look up a bot by game-type and name."
  [db game-type name]
  (->record (db/one db collection {:game-type (or game-type "journey")
                                   :name      (normalize-name name)})))

(defn list-bots-for-game
  "All bots for a game type."
  [db game-type]
  (mapv ->record (db/query db collection {:game-type game-type})))

(defn list-bots-for-owner
  "All bots owned by a player, optionally filtered by game type."
  ([db owner]
   (mapv ->record (db/query db collection {:owner owner})))
  ([db owner game-type]
   (mapv ->record (db/query db collection {:owner owner :game-type game-type}))))

(defn delete-bot!
  [db game-type name]
  (db/delete! db collection {:game-type (or game-type "journey")
                             :name      (normalize-name name)}))
