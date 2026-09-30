(ns organism.persist-distinctions
  "Storage for DISTINCTIONS games.

   A snapshot of the state, written after every move, rather than an event
   log: the deck is shuffled server-side and nobody replays a game of this.
   The per-player `player-games-*` rows are written in the shape the rest of
   the site reads, so a game shows up on the games list and in the stats."
  (:require
   [organism.mongo :as db]
   [distinctions.game :as game]))

(def game-type "distinctions")

(defn- player-games-key [player]
  (str "player-games-" player))

(defn save-state!
  "Write the table as it now stands."
  [db game-key state]
  (db/index! db :distinctions-games [:key] {:unique true})
  (db/merge!
   db :distinctions-games
   {:key game-key}
   {:state       (pr-str state)
    :game-type   game-type
    :turn        (:turn state)
    :winner      (:winner state)
    :status      (if (:winner state) "complete" "active")
    :updated     (quot (System/currentTimeMillis) 1000)})
  (doseq [player (map :name (:players state))]
    (db/index! db (player-games-key player) [:game] {:unique true})
    (db/merge!
     db (player-games-key player)
     {:game game-key}
     {:round          (:turn state)
      :status         (if (:winner state) "complete" "active")
      :game-type      game-type
      :players        (mapv :name (:players state))
      :current-player (game/current-player state)
      :winner         (:winner state)
      :last-move-at   (quot (System/currentTimeMillis) 1000)})))

(defn create-game!
  "Open a new table.  `bots` is stored beside the state because it is a
   property of the table rather than of the game, which does not care whether
   a seat is answered by a person."
  [db game-key players bots state]
  (db/index! db :distinctions-games [:key] {:unique true})
  (db/merge!
   db :distinctions-games
   {:key game-key}
   {:players    (pr-str (vec players))
    :bots       (pr-str (vec bots))
    :created    (quot (System/currentTimeMillis) 1000)
    :game-type  game-type
    :status     "active"})
  (save-state! db game-key state))

(defn load-game
  "The stored table, or nil.  Returns the state already read back, since every
   caller wants it that way."
  [db game-key]
  (when-let [record (db/one db :distinctions-games {:key game-key})]
    {:key     game-key
     :state   (when (:state record) (read-string (:state record)))
     :players (when (:players record) (read-string (:players record)))
     :bots    (when (:bots record) (read-string (:bots record)))
     :winner  (:winner record)}))

(defn complete-game!
  "Mark a finished table.  `save-state!` has already written the winner into
   every player's row; this is the game-level record and the stamp that stops
   the table being resumed."
  [db game-key state]
  (save-state! db game-key state)
  (db/merge!
   db :distinctions-games
   {:key game-key}
   {:status    "complete"
    :winner    (:winner state)
    :completed (quot (System/currentTimeMillis) 1000)}))

(defn load-open-games
  "Tables still being played, newest first."
  [db]
  (->> (db/query db :distinctions-games {:status "active"})
       (sort-by :updated >)
       (mapv (fn [r] {:key     (:key r)
                      :players (when (:players r) (read-string (:players r)))
                      :turn    (:turn r)
                      :updated (:updated r)}))))
