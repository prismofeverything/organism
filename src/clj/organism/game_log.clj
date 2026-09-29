(ns organism.game-log
  "A game is its log: every position it has been in, oldest first, in the
   database's history collection for that game. Nothing else holds positions.
   The present is the newest entry.

   Only this namespace reads or writes the log, and it changes in two ways:

     append   a new position, which the server has computed itself by
              replaying the choices a player made through the rules
     undo     the newest position goes, unless it is the only one

   There used to be a second copy of the history in server memory, kept in
   step by hand beside the database, and a third in each browser. They drifted
   apart -- undo deleted from the database without stepping back in memory,
   and a browser's whole state was stored as the present on its say-so -- and a
   round-10 game was rewound to its opening move. One log, written only here,
   is the fix: there is nothing left to disagree with.

   Writes to one game are serialised, so a bot finishing its turn and a person
   pressing undo cannot interleave."
  (:require
   [monger.query :as query]
   [organism.choice :as choice]
   [organism.persist :as persist]))

;; ── Reading ─────────────────────────────────────────────────────────────────

(defn entries
  "Every position, oldest first."
  [db game-key]
  (mapv persist/deserialize-state
        (query/with-collection db (persist/history-key game-key)
          (query/find {})
          (query/sort (array-map :$natural 1)))))

(defn length [db game-key] (persist/game-history-count db game-key))

(defn recent
  "The newest `n` positions, oldest first. What undo and clear need is always
   at the end of the log, so they read only that much of it."
  [db game-key n]
  (vec (rseq (mapv persist/deserialize-state
                   (query/with-collection db (persist/history-key game-key)
                     (query/find {})
                     (query/sort (array-map :$natural -1))
                     (query/limit n))))))

(defn this-turn
  "The positions of the turn under way, oldest first, with the one before it
   when there is one -- read back from the end of the log, a few at a time,
   only as far as the turn goes."
  [db game-key]
  (let [turn (juxt :round (comp :player :player-turn))
        total (length db game-key)]
    (loop [n 8]
      (let [tail (recent db game-key n)
            now (turn (peek tail))]
        (if (or (>= n total) (not= now (turn (first tail))))
          tail
          (recur (* 2 n)))))))

(defn present
  "The newest position, or nil for a game with no log."
  [db game-key]
  (some-> (persist/load-game-state db game-key) persist/deserialize-state))

;; ── Writing ─────────────────────────────────────────────────────────────────

(defonce ^:private locks (atom {}))

(defn- lock-for [game-key]
  (or (get @locks game-key)
      (get (swap! locks update game-key #(or % (Object.))) game-key)))

(defmacro with-game
  "Run `body` with this game's log to itself."
  [game-key & body]
  `(locking (#'lock-for ~game-key) ~@body))

(defn position
  "A state as the log holds it: without the choice path it was reached by,
   which belongs to the one choosing, not to the position."
  [state]
  (with-meta state nil))

(defn append!
  "Add a position. Returns the log's new length."
  [db game-key state]
  (persist/update-state! db game-key (position state))
  (length db game-key))

(defn undo!
  "Remove the newest position, unless it is the only one. Returns the new
   present, or nil when there was nothing to step back to."
  [db game-key]
  (when (>= (length db game-key) 2)
    (persist/reset-state! db game-key)
    (present db game-key)))

;; ── Choosing ────────────────────────────────────────────────────────────────

(defn apply-path
  "Replay a player's choices from `game`, each one a key among the choices the
   rules offer at that point. Nil if any key is not on offer -- which is how a
   browser that is behind, or wrong, is refused rather than believed."
  [game path]
  (when (seq path)
    (binding [*out* (java.io.StringWriter.)]   ; the rules print as they think
      (reduce (fn [g k]
                (let [[_ choices] (choice/find-state g)]
                  (if (contains? choices k)
                    (get choices k)
                    (reduced nil))))
              ;; the path starts here, whatever this state was reached by
              (update game :state position) path))))
