(ns organism.tree
  (:require
   [organism.base :as base]
   [organism.game :as game]))

(defn walk-choose-action
  [game turn])

(defn walk-introduction
  [game {:keys [player] :as turn}]
  (let [organisms (game/player-organisms game)]
    (if (empty? (get organisms player))
      (let [introductions
            ()])
      (base/map-cat)
      (walk-choose-action game turn))))

(defn walk-choices
  [game player-name]
  (let [original-game game
        game (game/award-center game player-name)
        player (get-player game player-name)
        turn (PlayerTurn. player-name {} [])]
    (if (game/player-wins? game player-name)
      [[(assoc turn :introduction {:center 1}) game]]
      (walk-introduction game turn))))
