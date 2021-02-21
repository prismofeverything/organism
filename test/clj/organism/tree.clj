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
  (let [fed-elements (filter game/fed? elements)]
    (base/map-cat
     (partial
      walk-circulate-action
      game turn organism elements organisms organism-turn (dec num-actions))
     fed-elements)))

(defn walk-eat-action
  [game turn organism elements organisms organism-turn num-actions open-eater]
  (let [action (game/->Action :eat {:to (:space open-eater)})]))

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

(defn walk-move-actions
  [game turn organism elements organisms organism-turn num-actions]
  (if (zero? num-actions)
    (walk-organism-turn
     game
     (update turn :organism-turns conj organism-turn)
     organisms)

    (let [mobile-elements
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
           mobile-elements)
          actions-left (dec num-actions)]

      (concat
       (base/map-cat
        (partial
         walk-next-action
         game turn organism elements organisms organism-turn actions-left)
        eat-actions)
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
        player (game/get-player game player-name)
        turn (game/->PlayerTurn player-name {} [])]
    (if (game/player-wins? game player-name)
      [[(assoc turn :introduction {:center 1}) game]]
      (walk-introduction game turn))))
