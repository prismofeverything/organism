(ns organism.examples
  (:require
   [organism.game :as game]))

(def two-player-close
  (game/create-game
   6
   [:yellow :red :blue :orange]
   [["orb" [[:orange 0] [:orange 1] [:orange 2]]]
    ["mass" [[:orange 9] [:orange 10] [:orange 11]]]]
   false))

