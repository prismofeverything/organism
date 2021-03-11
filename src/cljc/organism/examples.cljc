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

(defn six-player-game
  []
  (game/create-game
   6
   [:A :B :C :D :E :F :G]
   [["orb" [[:G 2] [:G 3] [:G 4]]]
    ["mass" [[:G 8] [:G 9] [:G 10]]]
    ["brone" [[:G 14] [:G 15] [:G 16]]]
    ["laam" [[:G 20] [:G 21] [:G 22]]]
    ["stuk" [[:G 26] [:G 27] [:G 28]]]
    ["faast" [[:G 32] [:G 33] [:G 34]]]]
   true))

(defn five-player-game
  []
  (game/create-game
   5
   [:A :B :C :D :E :F :G]
   [["orb" [[:G 2] [:G 3] [:G 4]]]
    ["mass" [[:G 8] [:G 9] [:G 10]]]
    ["brone" [[:G 14] [:G 15] [:G 16]]]
    ["laam" [[:G 20] [:G 21] [:G 22]]]
    ["stuk" [[:G 26] [:G 27] [:G 28]]]]
   true))

(defn seven-player-game
  []
  (game/create-game
   5
   [:A :B :C :D :E :F :G]
   [["orb" [[:G 2] [:G 3] [:G 4]]]
    ["mass" [[:G 8] [:G 9] [:G 10]]]
    ["brone" [[:G 14] [:G 15] [:G 16]]]
    ["laam" [[:G 20] [:G 21] [:G 22]]]
    ["stuk" [[:G 26] [:G 27] [:G 28]]]
    ["faast" [[:G 32] [:G 33] [:G 34]]]
    ["lelon" [[:G 38] [:G 39] [:G 40]]]]
   true))
