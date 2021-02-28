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
  (let [elements (game/current-organism-elements game)
        food (reduce + 0 (map :food elements))
        types (if (zero? food)
                [:eat]
                element-types)])
  (mapv
   (partial game/choose-action-type game)
   element-types))

(defn eat-filter
  [game]
  (let [elements (game/current-organism-elements game)
        types (group-by :type elements)
        eaters (get types :eat)]
    (not (every? game/full? eaters))))

(defn grow-filter
  [game]
  (let [elements (game/current-organism-elements game)
        types (group-by :type elements)
        growers (get types :grow)
        food (reduce + 0 (map :food growers))
        existing (map count (vals types))
        least (apply min existing)]
    (>= food least)))

(defn move-filter
  [game]
  (let [elements (game/current-organism-elements game)
        mobile (filter
                (comp
                 (partial game/can-move? game)
                 :space)
                elements)]
    (> (count mobile) 0)))

(defn circulate-filter
  [game]
  (let [elements (game/current-organism-elements game)
        food (reduce + 0 (map :food elements))]
    (> food 0)))

(def action-filters
  {:eat eat-filter
   :move move-filter
   :grow grow-filter
   :circulate circulate-filter})

(defn action-filter
  [game action-type]
  (let [filter-action (get action-filters action-type)]
    (filter-action game)))

(defn choose-action-choices
  [game action-type]
  (mapv
   (partial game/choose-action game)
   (filter
    (partial action-filter game)
    [action-type :circulate])))

(defn eat-to-choices
  [game elements]
  (let [open-eaters
        (filter
         (fn [element]
           (and
            (= :eat (:type element))
            (game/open? element)))
         elements)]
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
        growers (get types :grow)
        contributions (food-contributions growers existing)]
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
      (let [{:keys [organism choice num-actions actions] :as organism-turn} (last organism-turns)
            elements (get organisms organism)
            types (group-by :type elements)]

        (cond
          (nil? choice) (choose-action-type-choices game)

          (every? game/complete-action? actions)
          (cond
            (< (count actions) num-actions)
            (let [choices (choose-action-choices game choice)]
              (if (empty? choices)
                (list (game/pass-action game))
                choices))

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
                next-choices (get action-choices [type next-field])
                choices (next-choices game elements)]
            (if (empty? choices)
              (list (game/pass-action game))
              choices)))))))

(defn take-path
  [game path]
  (reduce
   (fn [game choice]
     (let [choices (find-choices game)]
       (nth choices choice)))
   game path))

(defn random-walk
  [game]
  (iterate
   (fn [game]
     (let [choices (find-choices game)
           num-choices (count choices)
           choice (rand-int num-choices)
           chosen (nth choices choice)]
       (println "CHOICES")
       (pprint (map (comp :player-turn :state) choices))
       (println "CHOICE")
       (pprint (get-in chosen [:state :player-turn]))
       chosen))
   game))
