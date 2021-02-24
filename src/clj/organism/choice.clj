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

(defn choose-organism-choices
  [game organisms]
  (map
   (partial game/empty-organism-turn game)
   organisms))

(defn choose-action-type-choices
  [game organism]
  (map
   (partial game/choose-action-type game)
   element-types))

(defn choose-action-choices
  [game action-type]
  (map
   (partial game/choose-action game)
   [action-type :circulate]))

(defn eat-to-choices
  [game elements]
  (let [open-eaters
        (filter
         (fn [element]
           (and
            (= :eat (:type element))
            (game/open? element)))
         elements)]
    (map
     (comp
      (partial game/eat game)
      :space)
     elements)))

(defn grow-element-choices
  [game elements]
  (let [types (group-by :type elements)
        grower-food
        (reduce
         + 0
         (map :food (:grow types)))
        available
        (filter
         (fn [type]
           (<=
            (count (get types type))
            grower-food))
         element-types)]
    (map
     (partial game/choose-grow-element game)
     available)))

(defn grow-from-choices [])
(defn grow-to-choices [])
(defn move-from-choices [])
(defn move-to-choices [])
(defn circulate-from-choices [])
(defn circulate-to-choices [])

(def action-choices
  {[:eat :to] eat-to-choices
   [:grow :element] grow-element-choices
   [:grow :from] grow-from-choices
   [:grow :to] grow-to-choices
   [:move :from] move-from-choices
   [:move :to] move-to-choices
   [:circulate :from] circulate-from-choices
   [:circulate :to] circulate-to-choices})

(defn find-choices
  [{:keys [state] :as game}]
  (let [{:keys [elements captures player-turn]} state
        {:keys [player introduction organism-turns]} player-turn
        organisms (game/player-organisms game player)]

    (cond
      (empty? organisms) (introduce-choices game)

      (empty? organism-turns)
      (if (> (count organisms) 1)
        (choose-organism-choices game (keys organisms))
        (choose-action-type-choices game (-> organisms keys first)))

      :else
      (let [{:keys [organism choice actions] :as organism-turn} (last organism-turns)
            elements (get organisms organism)
            types (group-by :type elements)]

        (cond
          (empty? choice) (choose-action-type-choices game)

          (every? game/complete-action? actions)
          (cond
            (< (count actions) (count (get types choice)))
            (choose-action-choices game choice)

            (< (count organism-turns) (count organisms))
            (let [acted (set (map :organism organism-turns))
                  missing (remove acted (keys organisms))]
              (if (> (count missing) 1)
                (choose-organism-choices game missing)
                (choose-action-type-choices game (first missing))))

            ;; move on to the next player here?
            :else [game])

          :else
          (let [{:keys [type action]} (last actions)
                fields (get game/action-fields type)
                fields-present (-> action keys set)
                next-field (some (fn [field] (not (fields-present field))) fields)
                next-choices (get action-choices [type next-field])]
            (next-choices game elements)))))))
