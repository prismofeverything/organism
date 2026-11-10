(ns organism.persist-table
  "Storage for playtest tables, their uploaded files, and each player's saved
   components.

   A table is a snapshot -- the objects as EDN -- written a moment after it
   last changed, not an event log: nobody replays a playtest. The
   `player-games-*` rows are the shape every game writes, so a table shows on
   a player's lists like any other game.

   Files people bring to a table (card art, a model) are kept here too, under
   the hash of their contents, and the table holds only their URL: the site's
   WebSocket frames are capped at 64 KB, and the same image dropped twice is
   stored once."
  (:require
   [clojure.edn :as edn]
   [organism.mongo :as db])
  (:import
   [java.security MessageDigest]
   [java.util Base64]))

(def game-type "table")

(defn- now [] (quot (System/currentTimeMillis) 1000))
(defn- player-games-key [player] (str "player-games-" player))

(defn- read-edn [s] (when s (edn/read-string s)))

(defn add-player!
  "Put the table on `player`'s list."
  [db table-key player players]
  (db/index! db (player-games-key player) [:game] {:unique true})
  (db/merge! db (player-games-key player)
             {:game table-key}
             {:status "active" :game-type game-type :players (vec players)
              :round 0 :current-player nil :winner nil :last-move-at (now)}))

(defn create-table!
  [db table-key title player]
  (db/index! db :table-games [:key] {:unique true})
  (db/merge! db :table-games {:key table-key}
             {:title title :created-by player :players (pr-str [player])
              :objects (pr-str {}) :game-type game-type :status "active"
              :created (now) :updated (now)})
  (add-player! db table-key player [player]))

(defn load-table
  [db table-key]
  (when-let [r (db/one db :table-games {:key table-key})]
    {:key table-key
     :title (:title r)
     :created-by (:created-by r)
     :players (or (read-edn (:players r)) [])
     :objects (or (read-edn (:objects r)) {})}))

(defn save-objects!
  [db table-key objects]
  (db/merge! db :table-games {:key table-key} {:objects (pr-str objects) :updated (now)}))

(defn save-players!
  [db table-key players]
  (db/merge! db :table-games {:key table-key} {:players (pr-str (vec players))})
  (doseq [p players] (add-player! db table-key p players)))

(defn player-tables
  "The tables on player's list, most recently used first."
  [db player]
  (let [rows (db/query db (player-games-key player) {:game-type game-type})
        tables (keep #(db/one db :table-games {:key (:game %)}) rows)]
    (->> tables
         (sort-by :updated >)
         (mapv (fn [t] {:key (:key t) :title (:title t) :created-by (:created-by t)
                        :players (or (read-edn (:players t)) []) :updated (:updated t)})))))

(defn leave-table!
  "Take the table off player's list (the table itself stays for the others)."
  [db table-key player]
  (db/delete! db (player-games-key player) {:game table-key}))

;; ── uploaded files ─────────────────────────────────────────────────────────

(defn- sha1 [^bytes bs]
  (let [d (.digest (MessageDigest/getInstance "SHA-1") bs)]
    (apply str (map #(format "%02x" %) d))))

(defn put-asset!
  "Keep a data URL's contents; return its hash."
  [db data-url]
  (let [[_ mime b64] (re-find #"^data:([^;,]*)(?:;[^,]*)?;base64,(.*)$" data-url)
        bs (.decode (Base64/getDecoder) ^String b64)
        h (sha1 bs)]
    (db/index! db :table-assets [:hash] {:unique true})
    (when-not (db/one db :table-assets {:hash h})
      (db/merge! db :table-assets {:hash h}
                 {:mime (if (seq mime) mime "application/octet-stream") :data b64 :size (alength bs)}))
    h))

(defn get-asset
  "{:mime :bytes} for a hash, or nil."
  [db h]
  (when-let [r (db/one db :table-assets {:hash h})]
    {:mime (:mime r) :bytes (.decode (Base64/getDecoder) ^String (:data r))}))

;; ── saved components ───────────────────────────────────────────────────────

(defn load-components [db player]
  (or (read-edn (:items (db/one db :table-components {:player player}))) []))

(defn save-components! [db player items]
  (db/index! db :table-components [:player] {:unique true})
  (db/merge! db :table-components {:player player} {:items (pr-str (vec items))}))
