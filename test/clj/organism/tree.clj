(ns organism.tree
  (:require
   [clojure.math.combinatorics :as combine]
   [organism.base :as base]
   [organism.game :as game]))

(def element-types [:eat :grow :move])

(declare walk-organism-turn)

(defn walk-circulate-actions)

(defn walk-eat-actions
  [game turn organism elements organisms organism-turn num-actions]
  (let [open-eaters (filter (comp (partial = :eat) :type) elements)]
    (base/map-cat
     eaters)))

(def element-walks
  {:eat walk-eat-actions
   :grow walk-grow-actions
   :move walk-move-actions})

(defn walk-choose-action
  [game turn organism elements organisms organism-turn]
  (let [{:keys [organism choice]} organism-turn
        num-actions (count
                     (filter
                      (fn [{:keys [type]}]
                        (= type choice))
                      elements))
        element-walk (get element-walks choice)]
    (element-walk game turn organism elements organisms organism-turn num-actions)))

(defn walk-organism-turn
  [game turn organisms]
  (if (empty? organisms)
    [game turn]
    (let [[organism elements] (first organisms)
          organism-turns
          (map
           (fn [type]
             (OrganismTurn. organism type []))
           element-types)]
      (mapcat
       (partial walk-choose-action game turn organism elements (rest organisms))
       organism-turns))))

(defn walk-perform-introduction
  [game {:keys [player] :as turn} introduction]
  (let [game (game/introduce game player introduction)
        turn (assoc turn :introduction introduction)
        organisms (game/player-organisms game player)]
    (walk-organism-turn game turn organisms)))

(defn walk-introduction
  [game {:keys [player] :as turn}]
  (let [organisms (game/player-organisms game player)]
    (if (empty? organisms)
      (let [starting (get-in game [:players player :starting-spaces])
            organism 0
            introductions
            (assoc
             (into
              {}
              (map
               vector
               (combine/permutations element-types)
               starting))
             :organism organism)]
        (base/map-cat
         (partial walk-perform-introduction game turn)
         introductions))
      (walk-organism-turn game turn organisms))))

(defn walk-turn
  [game player-name]
  (let [original-game game
        game (game/award-center game player-name)
        player (get-player game player-name)
        turn (PlayerTurn. player-name {} [])]
    (if (game/player-wins? game player-name)
      [[(assoc turn :introduction {:center 1}) game]]
      (walk-introduction game turn))))
