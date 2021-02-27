(ns organism.choice
  (:require
   [clojure.math.combinatorics :as combine]
   [clojure.pprint :refer (pprint)]
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
    (mapv
     (partial game/introduce game player)
     introductions)))

(defn choose-organism-choices
  [game organisms]
  (mapv
   (partial game/choose-organism game)
   organisms))

(defn choose-action-type-choices
  [game]
  (mapv
   (partial game/choose-action-type game)
   element-types))

(defn choose-action-choices
  [game action-type]
  (mapv
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
    (println "EAT TO" (:state game))
    (mapv
     (comp
      game/complete-action
      (partial game/choose-action-field game :to)
      :space)
     open-eaters)))

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
    (mapv
     (partial game/choose-action-field game :element)
     available)))

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
    (mapv
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

(defn grow-from-choices
  [game elements]
  (let [types (group-by :type elements)
        element-choice (game/get-action-field game :element)
        existing (count (get types element-choice))
        contributions (food-contributions elements existing)]
    (mapv
     (partial game/choose-action-field game :from)
     contributions)))

(defn grow-to-choices
  [game elements]
  (let [types (group-by :type elements)
        growers (get types :grow)
        growable (game/growable-spaces game (map :space growers))]
    (mapv
     (comp
      game/complete-action
      (partial game/choose-action-field game :to))
     growable)))

(defn move-from-choices
  [game elements]
  (let [mobile-elements
        (filter
         (partial game/can-move? game)
         (map :space elements))]
    (mapv
     (partial game/choose-action-field game :from)
     mobile-elements)))

(defn move-to-choices
  [game elements]
  (let [from (game/get-action-field game :from)
        open-spaces (game/available-spaces game from)]
    (mapv
     (comp
      game/complete-action
      (partial game/choose-action-field game :to))
     open-spaces)))

(defn circulate-from-choices
  [game elements]
  (let [fed (filter game/fed-element? elements)]
    (mapv
     (comp
      (partial game/choose-action-field game :from)
      :space)
     fed)))

(defn circulate-to-choices
  [game elements]
  (let [from (game/get-action-field game :from)
        open (filter
              (fn [element]
                (and
                 (game/open? element)
                 (not= (:space element) from)))
              elements)]
    (mapv
     (comp
      game/complete-action
      (partial game/choose-action-field game :to)
      :space)
     open)))

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
      (let [game (game/award-center game player)]
        (if (> (count organisms) 1)
          (choose-organism-choices game (keys organisms))
          (choose-action-type-choices
           (game/choose-organism
            game
            (-> organisms keys first)))))

      :else
      (let [{:keys [organism choice actions] :as organism-turn} (last organism-turns)
            elements (get organisms organism)
            types (group-by :type elements)]

        (println "organism" organism)
        (println "organisms" organisms)
        (println "organism turn" organism-turn)
        (println "elements" elements)
        (println "types" types)

        (cond
          (nil? choice) (choose-action-type-choices game)

          (every? game/complete-action? actions)
          (cond
            (< (count actions) (count (get types choice)))
            (choose-action-choices game choice)

            (< (count organism-turns) (count organisms))
            (let [acted (set (map :organism organism-turns))
                  missing (remove acted (keys organisms))]
              (if (> (count missing) 1)
                (choose-organism-choices game missing)
                (choose-action-type-choices
                 (game/choose-organism
                  game
                  (first missing)))))

            ;; move on to the next player
            :else
            (find-choices
             (game/finish-turn game)))

          :else
          (let [{:keys [type action]} (last actions)
                fields (get game/action-fields type)
                fields-present (-> action keys set)
                next-field (first
                            (filter
                             (fn [field]
                               (not (fields-present field)))
                             fields))
                next-choices (get action-choices [type next-field])]
            (println "type" type)
            (println "action" action)
            (println "fields" fields)
            (println "fields present" fields-present)
            (println "next-field" next-field)
            (println "next-choices" next-choices)
            (next-choices game elements)))))))

(defn take-path
  [game path]
  (reduce
   (fn [game choice]
     (pprint (:state game))
     (let [choices (find-choices game)]
       (nth choices choice)))
   game path))
