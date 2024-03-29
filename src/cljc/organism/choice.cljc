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

(def element-combinations
  (combine/permuted-combinations
   element-types
   3))

(defn partial-map
  [f s]
  (into
   {}
   (map
    (juxt identity f)
    s)))

(defn full-introduce-permutations
  "assume we want the elements to be as balanced as possible"
  [elements n]
  (let [count-elements (count elements)
        balance (* count-elements (int (Math/ceil (/ n count-elements))))
        options (take balance (cycle elements))]
    (combine/permuted-combinations options n)))

(defn introduce-permutations
  [elements group-count]
  (let [groups
        (apply
         combine/cartesian-product
         (repeat group-count element-combinations))]
    (map
     (fn [group]
       (apply concat group))
     groups)))

(defn introduce-choices
  [{:keys [state players] :as game}]
  (let [{:keys [player-turn]} state
        {:keys [player]} player-turn
        organism 0
        starting (-> players (get player) :starting-spaces)
        groups-count (/ (count starting) (count element-types))
        orders
        (if (> groups-count 3)
          [(base/map-cat
            (fn [_]
              (shuffle element-types))
            (range groups-count))]
          (introduce-permutations element-types groups-count))
        _ (println "ORDERS" orders)
        introductions
        (mapv
         (fn [order]
           {:spaces
            (into
             {}
             (map
              vector
              starting
              order))
            :organism organism})
         orders)]
    (partial-map
     (partial game/introduce game player)
     introductions)))

(defn choose-organism-choices
  [game organisms]
  (partial-map
   (partial game/choose-organism game)
   organisms))

(defn choose-action-type-choices
  [game]
  (let [elements (game/current-organism-elements game)
        food (reduce + 0 (map :food elements))
        types (if (zero? food)
                [:eat]
                element-types)])
  (partial-map
   (partial game/choose-action-type game)
   element-types))

(defn eat-filter
  [game]
  (let [elements (game/current-organism-elements game)
        types (group-by :type elements)
        eaters (get types :eat)
        able (filter (partial game/can-eat? game) eaters)]
    (> (count able) 0)))

(defn grow-filter
  [game]
  (let [elements (game/current-organism-elements game)
        types (group-by :type elements)
        growers (get types :grow)
        food (reduce + 0 (map :food growers))
        existing (map count (vals types))
        least (if (< (count types) 3)
                0
                (apply min existing))
        growable (game/growable-spaces game (map :space growers))]
    (println "grow filter" food least existing growable)
    (and
     (>= food least)
     (not (empty? growable)))))

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
  (partial-map
   (partial game/choose-action game)
   (filter
    (partial action-filter game)
    [action-type :circulate])))

(defn eat-to-choices
  [game elements _]
  (let [open-eaters
        (filter
         (fn [element]
           (and
            (= :eat (:type element))
            (game/can-eat? game element)))
         elements)]
    (partial-map
     (partial game/choose-action-field game :to)
     (map :space open-eaters))))

(defn eat-from-choices
  [game elements _]
  (let [to-choice (game/get-action-field game :to)
        options (game/open-spaces game to-choice)
        distinct (vals (group-by (partial game/free-food-present game) options))
        spaces (map first distinct)]
    (partial-map
     (comp
      game/complete-action
      (partial game/choose-action-field game :from))
     spaces)))

(defn grow-element-choices
  [game elements _]
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
    (println "grow element choices" grower-food types available)
    (partial-map
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
  [game elements _]
  (let [types (group-by :type elements)
        element-choice (game/get-action-field game :element)
        existing (count (get types element-choice))
        growers (get types :grow)
        contributions (food-contributions growers existing)]
    (partial-map
     (partial game/choose-action-field game :from)
     contributions)))

(defn grow-to-choices
  [game elements _]
  (let [types (group-by :type elements)
        growers (get types :grow)
        growable (game/growable-spaces game (map :space growers))]
    (partial-map
     (comp
      game/complete-action
      (partial game/choose-action-field game :to))
     growable)))

(defn move-from-choices
  [game elements _]
  (let [mobile-elements
        (filter
         (partial game/can-move? game)
         (map :space elements))]
    (partial-map
     (partial game/choose-action-field game :from)
     mobile-elements)))

(defn move-to-choices
  [game elements _]
  (let [from (game/get-action-field game :from)
        open-spaces (game/available-spaces game from)]
    (partial-map
     (comp
      game/complete-action
      (partial game/choose-action-field game :to))
     open-spaces)))

(defn circulate-from-choices
  [game elements _]
  (let [fed (filter game/fed-element? elements)]
    (partial-map
     (partial game/choose-action-field game :from)
     (map :space fed))))

(defn circulate-to-choices
  [game elements extended]
  (let [from (game/get-action-field game :from)
        open (filter
              (fn [element]
                (and
                 (game/open-element? element)
                 (not= (:space element) from)))
              extended)]
    (partial-map
     (comp
      game/complete-action
      (partial game/choose-action-field game :to))
     (map :space open))))

(def action-choices
  {[:eat :to] eat-to-choices
   [:eat :from] eat-from-choices
   [:grow :element] grow-element-choices
   [:grow :from] grow-from-choices
   [:grow :to] grow-to-choices
   [:move :from] move-from-choices
   [:move :to] move-to-choices
   [:circulate :from] circulate-from-choices
   [:circulate :to] circulate-to-choices})

(defn find-state
  [{:keys [state] :as game}]
  (let [{:keys [elements captures player-turn]} state
        {:keys [player introduction organism-turns]} player-turn
        organisms (game/player-organisms game player)
        all-extended (game/extended-organisms game)
        extended (get all-extended player)
        winner (game/victory? game)]

    (cond
      (= (:advance player-turn) :resolve-conflicts)
      [:resolve-conflicts {:advance (game/check-integrity game player)}]

      winner
      [:player-victory {:advance (game/declare-victory game winner)}]

      (= (:advance player-turn) :check-integrity)
      [:check-integrity {:advance (game/start-next-turn game)}]

      (empty? organisms)
      (let [choices (introduce-choices game)]
        [:introduce choices])

      (empty? organism-turns)
      ;; find organisms again to avoid finding for each introduction
      (let [game (game/find-organisms game)
            organisms (game/player-organisms game player)]

        (if (> (count organisms) 1)
          [:choose-organism (choose-organism-choices game (keys organisms))]
          [:choose-action-type
           (choose-action-type-choices
            (game/choose-organism
             game
             (-> organisms keys first)))]))

      :else
      (let [{:keys [organism choice num-actions actions] :as organism-turn} (last organism-turns)
            elements (get organisms organism)
            extended-elements (get extended organism)
            types (group-by :type elements)]

        (cond
          (nil? choice) [:choose-action-type (choose-action-type-choices game)]

          (every? game/complete-action? actions)
          (cond
            (< (count actions) num-actions)
            (let [choices (choose-action-choices game choice)
                  pass {:pass
                        (-> game
                            (game/choose-action :circulate)
                            game/pass-action)}]
              (if (empty? choices)
                [:pass pass]
                [:choose-action (merge choices pass)]))

            (< (count organism-turns) (count organisms))
            (let [acted (set (map :organism organism-turns))
                  missing (remove acted (keys organisms))]
              (println "REMAINING ORGANISMS" missing)
              [:choose-organism (choose-organism-choices game missing)])

            :else [:actions-complete {:advance (game/resolve-conflicts game player)}])

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
                choices (next-choices game elements extended-elements)
                action-key (keyword (str (name type) "-" (name next-field)))]
            (if (empty? choices)
              [:pass {:pass (game/pass-action game)}]
              [action-key choices])))))))

(defn find-choices
  [game]
  (vals (last (find-state game))))

(defn ignore-organism-id
  [elements]
  (into
   {}
   (map
    (fn [space element]
      [space (dissoc element :organism)])
    elements)))

(defn elements=
  [a b]
  (=
   (keys a)
   (keys b)))

(defn find-next-choices
  [initial-game]
  (loop [game initial-game]
    (let [[turn choices] (find-state game)
          game-elements (get-in game [:state :elements])
          choice-elements (get-in (:advance choices) [:state :elements])]
      (if (or
           (empty? choices)
           (= turn :check-integrity)
           (= turn :player-victory)
           (< 1 (count choices))
           (and
            (or
             (= turn :actions-complete)
             (= turn :resolve-conflicts))
            (not (elements= game-elements choice-elements))))
        [game turn choices]
        (recur (first (vals choices)))))))

(defn take-path
  [game path]
  (reduce
   (fn [game choice]
     (let [choices (find-choices game)]
       (nth choices choice)))
   game path))

;; levels of challenge
;; * random walk
;; * won't die immediately
;; * some idea of what's going on
;; * competent
;; * invincible

(defn random-walk
  [game]
  (iterate
   (fn [game]
     (let [choices (find-choices game)
           choice (rand-int (count choices))
           chosen (nth choices choice)]
       (println "CHOICES")
       (pprint (map (comp :player-turn :state) choices))
       (println "CHOICE")
       (pprint (get-in chosen [:state :player-turn]))
       chosen))
   game))
