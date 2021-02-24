(ns organism.choice
  (:require
   [clojure.math.combinatorics :as combine]
   [organism.base :as base]
   [organism.game :as game]))

(def element-types
  [:eat
   :grow
   :move])

(defn introduce-choices
  [{:keys [state players] :as game}]
  (let [{:keys [player-turn]} state
        {:keys [player]} player-turn
        organism 0
        starting (-> players (get player) :starting-spaces)
        orders (combine/permutations element-types)
        introductions
        (map
         (fn [order]
           (assoc
            (into
             {}
             (map
              vector
              order
              starting))
            :organism organism))
         orders)]
    (map
     (partial game/introduce game player)
     introductions)))

(defn find-choices
  [{:keys [state] :as game}]
  (let [{:keys [elements captures player-turn]} state
        {:keys [player introduction organism-turns]} player-turn
        organisms (game/player-organisms game player)]
    (cond
      (empty? organisms) (introduce-choices game)
      (empty? organism-turns) [])))
