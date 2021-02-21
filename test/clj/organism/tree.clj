(ns organism.tree
  (:require
   [clojure.math.combinatorics :as combine]
   [organism.base :as base]
   [organism.game :as game]))

(declare walk-organism-turn)
(declare walk-eat-actions)
(declare walk-grow-actions)
(declare walk-move-actions)

(def element-types [:eat :grow :move])
(def element-walks
  {:eat walk-eat-actions
   :grow walk-grow-actions
   :move walk-move-actions})

(defn walk-next-action
  [game turn organism elements organisms organism-turn num-actions action]
  (let [game (game/perform-action game (:player turn) action)
        organism-turn (update organism-turn :actions conj action)
        elements (get (game/player-organisms game (:player turn)) organism)
        element-walk (get element-walks (:choice organism-turn))]
    (base/map-cat
     (partial
      element-walk
      game turn organism elements organisms organism-turn num-actions))))

(defn walk-circulate-action
  [game turn organism elements organisms organism-turn num-actions fed-element]
  (let [open-elements (filter
                       (fn [element]
                         (and
                          (game/open? element)
                          (not= (:space element) (:space fed-element))))
                       elements)
        possible-actions
        (map
         (fn [open-element]
           (game/->Action
            :circulate
            {:from (:space fed-element)
             :to (:space open-element)}))
         open-elements)]
    (base/map-cat
     (partial
      walk-next-action
      game turn organism elements organisms organism-turn num-actions)
     possible-actions)))

(defn walk-circulate-actions
  [game turn organism elements organisms organism-turn num-actions]
  (let [fed-elements (filter game/fed-element? elements)]
    (base/map-cat
     (partial
      walk-circulate-action
      game turn organism elements organisms organism-turn (dec num-actions))
     fed-elements)))

(defn walk-eat-actions
  [game turn organism elements organisms organism-turn num-actions]
  (if (zero? num-actions)
    (walk-organism-turn
     game
     (update turn :organism-turns conj organism-turn)
     organisms)

    (let [open-eaters
          (filter
           (fn [element]
             (and
              (= :eat (:type element))
              (game/open? element)))
           elements)

          eat-actions
          (map
           (fn [open-eater]
             (game/->Action :eat {:to (:space open-eater)}))
           open-eaters)
          actions-left (dec num-actions)]

      (concat
       (base/map-cat
        (partial
         walk-next-action
         game turn organism elements organisms organism-turn actions-left)
        eat-actions)
       (walk-circulate-actions
        game turn organism elements organisms organism-turn actions-left)))))

(defn walk-move-targets
  [game turn organism elements organisms organism-turn num-actions moving]
  (let [open (game/available-spaces game (:space moving))
        move-actions
        (map
         (fn [open-space]
           (game/->Action
            :move
            {:from (:space moving)
             :to open-space}))
         open)]
    (base/map-cat
     (partial
      walk-next-action
      game turn organism elements organisms organism-turn num-actions)
     move-actions)))

(defn walk-move-actions
  [game turn organism elements organisms organism-turn num-actions]
  (if (zero? num-actions)
    (walk-organism-turn
     game
     (update turn :organism-turns conj organism-turn)
     organisms)

    (let [mobile-elements
          (filter
           (comp
            (partial game/can-move? game)
            :space)
           elements)
          actions-left (dec num-actions)]

      (concat
       (base/map-cat
        (partial
         walk-move-targets
         game turn organism elements organisms organism-turn actions-left)
        mobile-elements)
       (walk-circulate-actions
        game turn organism elements organisms organism-turn actions-left)))))

(defn extend-contribution
  [elements contribution]
  (let [element-food
        (map
         (fn [{:keys [space food]}]
           (let [spent (get contribution space 0)]
             [space (- food spent)]))
         elements)
        possible-contributors
        (filter
         (fn [[space food]]
           (> food 0))
         element-food)]
    (map
     (fn [[space food]]
       (update contribution space (fnil inc 0)))
     possible-contributors)))

(defn food-contributions
  [elements total]
  (loop [contributions [{}]
         total total]
    (if (zero? total)
      contributions
      (let [contributions
            (base/map-cat
             (partial extend-contribution elements)
             contributions)]
        (recur contributions (dec total))))))

(defn walk-grow-contributions
  [game turn
   organism elements organisms organism-turn num-actions
   growers element-type contribution]
  (let [open (game/growable-spaces game (map :space growers))
        grow-actions
        (map
         (fn [target]
           (game/->Action
            :grow
            {:from contribution
             :to target
             :element element-type}))
         open)]
    (base/map-cat
     (partial
      walk-next-action
      game turn organism elements organisms organism-turn num-actions)
     grow-actions)))

(defn walk-grow-element-types
  [game turn organism elements organisms organism-turn num-actions element-type]
  (let [existing (count (filter (comp (partial = element-type) :type) elements))
        growers (filter (comp (partial = :grow) :type) elements)
        grower-food (reduce (comp + :food) 0 growers)]
    (if (>= grower-food existing)
      (let [contributions (food-contributions growers existing)]
        (base/map-cat
         (partial
          walk-grow-contributions
          game turn organism elements organisms organism-turn num-actions growers element-type)
         contributions)))))

(defn walk-grow-actions
  [game turn organism elements organisms organism-turn num-actions]
  (if (zero? num-actions)
    (walk-organism-turn
     game
     (update turn :organism-turns conj organism-turn)
     organisms)

    (let [actions-left (dec num-actions)]

      (concat
       (base/map-cat
        (partial
         walk-grow-element-types
         game turn organism elements organisms organism-turn actions-left)
        element-types)
       (walk-circulate-actions
        game turn organism elements organisms organism-turn actions-left)))))

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
  (println "walk organisms" (count organisms))
  (if (empty? organisms)
    [game turn]
    (let [[organism elements] (first organisms)
          organism-turns
          (map
           (fn [type]
             (game/->OrganismTurn organism type []))
           element-types)]
      (mapcat
       (partial walk-choose-action game turn organism elements (rest organisms))
       organism-turns))))

(defn walk-perform-introduction
  [game {:keys [player] :as turn} introduction]
  (println "performing introduction" introduction)
  (let [game (game/introduce game player introduction)
        turn (assoc turn :introduction introduction)
        organisms (game/player-organisms game player)]
    (walk-organism-turn game turn organisms)))

(defn walk-introduction
  [game {:keys [player] :as turn}]
  (println "introduction for" player turn)
  (let [organisms (game/player-organisms game player)]
    (if (empty? organisms)
      (let [starting (get-in game [:players player :starting-spaces])
            organism 0
            combinations (combine/permutations element-types)
            introductions
            (base/map-cat
             (fn [combination]
               (map
                vector
                combination
                starting))
             combinations)]
        (base/map-cat
         (partial walk-perform-introduction game turn)
         introductions))
      (walk-organism-turn game turn organisms))))

(defn walk-turn
  [game player-name]
  (println "")
  (let [original-game game
        game (game/award-center game player-name)
        player (game/get-player game player-name)
        turn (game/->PlayerTurn player-name {} [])]
    (if (game/player-wins? game player-name)
      [[(assoc turn :introduction {:center 1}) game]]
      (walk-introduction game turn))))
