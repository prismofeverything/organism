(ns organism.examples
  (:require
   [organism.game :as game]))

(def two-player-close
  (game/create-game
   6
   [:yellow :red :blue :orange]
   [["orb" {:starting-spaces [[:orange 0] [:orange 1] [:orange 2]] :capture-limit 5}]
    ["mass" {:starting-spaces [[:orange 9] [:orange 10] [:orange 11]] :capture-limit 5}]]
   3
   false))

(defn six-player-game
  []
  (game/create-game
   6
   [:A :B :C :D :E :F :G]
   [["orb" {:starting-spaces [[:G 2] [:G 3] [:G 4]] :capture-limit 5}]
    ["mass" {:starting-spaces [[:G 8] [:G 9] [:G 10]] :capture-limit 5}]
    ["brone" {:starting-spaces [[:G 14] [:G 15] [:G 16]] :capture-limit 5}]
    ["laam" {:starting-spaces [[:G 20] [:G 21] [:G 22]] :capture-limit 5}]
    ["stuk" {:starting-spaces [[:G 26] [:G 27] [:G 28]] :capture-limit 5}]
    ["faast" {:starting-spaces [[:G 32] [:G 33] [:G 34]] :capture-limit 5}]]
   3
   true))

(defn five-player-game
  []
  (game/create-game
   5
   [:A :B :C :D :E :F :G]
   [["orb" {:starting-spaces [[:G 2] [:G 3] [:G 4]] :capture-limit 5}]
    ["mass" {:starting-spaces [[:G 8] [:G 9] [:G 10]] :capture-limit 5}]
    ["brone" {:starting-spaces [[:G 14] [:G 15] [:G 16]] :capture-limit 5}]
    ["laam" {:starting-spaces [[:G 20] [:G 21] [:G 22]] :capture-limit 5}]
    ["stuk" {:starting-spaces [[:G 26] [:G 27] [:G 28]] :capture-limit 5}]]
   3
   true))

(defn seven-player-game
  []
  (game/create-game
   7
   [:A :B :C :D :E :F :G]
   [["orb" {:starting-spaces [[:G 2] [:G 3] [:G 4]] :capture-limit 5}]
    ["mass" {:starting-spaces [[:G 8] [:G 9] [:G 10]] :capture-limit 5}]
    ["brone" {:starting-spaces [[:G 14] [:G 15] [:G 16]] :capture-limit 5}]
    ["laam" {:starting-spaces [[:G 20] [:G 21] [:G 22]] :capture-limit 5}]
    ["stuk" {:starting-spaces [[:G 26] [:G 27] [:G 28]] :capture-limit 5}]
    ["faast" {:starting-spaces [[:G 32] [:G 33] [:G 34]] :capture-limit 5}]
    ["lelon" {:starting-spaces [[:G 38] [:G 39] [:G 40]] :capture-limit 5}]]
   3
   true))
