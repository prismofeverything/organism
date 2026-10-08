(ns universe.mode
  "One table, three games. The UNIVERSE socket handler and storage drive a
   table through these, and each call goes to hold'em, shedding or network by
   the state's `:mode` -- so the handler's clock, bots, resume and
   persistence are written once for all of them."
  (:require
   [universe.holdem :as holdem]
   [universe.network :as network]
   [universe.shed :as shed]))

(defn shed? [state] (= :shed (:mode state)))
(defn network? [state] (= :network (:mode state)))

(defn- pick
  "The rules namespace's function for this state's game."
  [state holdem-f shed-f network-f]
  (case (:mode state) :shed shed-f :network network-f holdem-f))

(defn waiting?
  "Created, not yet dealt."
  [state]
  (if (#{:shed :network} (:mode state)) (= :waiting (:phase state)) (= :waiting (:street state))))

(defn view [state player]
  ((pick state holdem/view shed/view network/view) state player))

(defn current-player [state]
  ((pick state holdem/current-player shed/current-player network/current-player) state))

(defn act [state seat action]
  ((pick state holdem/act shed/act network/act) state seat action))

(defn seat-of [state player]
  ((pick state holdem/seat-of shed/seat-of network/seat-of) state player))

(defn player-name [state seat]
  ((pick state holdem/player-name shed/player-name network/player-name) state seat))

(defn game-over? [state]
  ((pick state holdem/game-over? shed/game-over? network/game-over?) state))

(defn hand-over?
  "Shedding and network are each one hand: over when the game is."
  [state]
  ((pick state holdem/hand-over? shed/game-over? network/game-over?) state))

(defn deal [state deck]
  ((pick state holdem/start-hand shed/start network/start) state deck))

(defn bot-action
  "For the games without betting, one plain bot plays every seat; nil for
   hold'em, whose bots each have their own."
  [state]
  (case (:mode state)
    :shed    (shed/bot-action state)
    :network (network/bot-action state)
    nil))

(defn auto-action
  "What the clock does for someone who has stopped answering, outside hold'em."
  [state]
  (case (:mode state)
    :shed    (shed/auto-action state)
    :network (network/auto-action state)
    nil))
