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

(defn declarable?
  "Whether declaring this type could still accomplish something this turn.

   Not the same question as `action-filter`, which asks whether the action can
   be taken *right now*. A turn is several actions and any of them may be a
   circulate, so an organism can move food where it is needed and then act. Food
   sitting on the wrong element is a detour, not a wall.

   Getting this wrong took a real turn away from a real player: an organism with
   two growers, three spaces to grow into and four food — all of it on its eat
   and move elements — was not offered GROW at all, because the food was not yet
   on a grower. It could have circulated and grown; instead the turn could not
   be played.

   So the food side of each test is asked of the whole organism:

     eat    an eater with an empty neighbour. Food never stops eating — the
            threshold does, and circulating food away relieves it.
     grow   somewhere to grow, and enough food anywhere in the organism.
     move   something mobile with somewhere to go, and any food at all."
  ([game type]
   (declarable? game (game/current-organism-elements game) type))
  ([game elements type]
  (let [present (group-by :type elements)
        held (reduce + 0 (map :food elements))]
    (and
     (seq (get present type))
     (case type
       :eat (boolean (some (fn [{:keys [space]}] (seq (game/open-spaces game space)))
                           (get present :eat)))
       :grow (let [growers (get present :grow)
                   least (if (< (count present) 3)
                           0
                           (apply min (map count (vals present))))]
               (and (seq (game/growable-spaces game (map :space growers)))
                    (>= held least)))
       :move (boolean (and (pos? held)
                           (some (fn [{:keys [space]}]
                                   (and (game/mobile? game space)
                                        (seq (game/available-spaces game space))))
                                 elements)))
       false)))))

(defn declarable-types
  "The types an organism may declare: every type it has an element of.

   Declaring is never filtered by what the turn could accomplish. A GROW
   declared with too little food to grow is still a turn — circulate the food
   to where it will be needed — and whether a declared type was worth it is
   the player's call, not the rules'. (It once was filtered, by `declarable?`
   under *require-useful-action*, to stop a trained agent passing in disguise;
   that hid GROW from a real player holding one food short.) What stops a pass
   is choose-action-state, which offers :pass only when nothing can be done.
   An organism with none of any type — never on a real board — gets all three."
  [elements]
  (let [present (set (map :type elements))]
    (or (seq (filter present element-types)) element-types)))

(defn choose-action-type-choices
  "Which action an organism declares for its turn. See declarable-types."
  [game]
  (partial-map
   (partial game/choose-action-type game)
   (declarable-types (game/current-organism-elements game))))

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
        any-food? (some #(pos? (game/free-food-present game %)) options)
        ;; When any adjacent space has food, present every adjacent open
        ;; space as a distinct choice so the UI can highlight them all.
        ;; When none have food, collapse to a single representative so the
        ;; player auto-advances past :eat-from instead of being forced to
        ;; pick between equivalent empty spaces.
        spaces (if any-food?
                 options
                 (take 1 options))]
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

;; FLOW CHOICES --------------------
;;
;; The same fields as any action, less whatever this action's other choices
;; have claimed. Everything else is read from the board as the action began,
;; which under FLOW is simply the board: nothing moves until the commit.

(defn- except-spaces [choices spaces] (apply dissoc choices spaces))

(defn flow-eat-sources
  [game claims eater]
  (let [taken (game/flow-taken-spaces game claims)]
    (remove (fn [space]
              (and (pos? (game/free-food-present game space)) (taken space)))
            (game/open-spaces game eater))))

(defn- flow-eat-to
  [game elements extended]
  (let [claims (game/flow-claims game)]
    (select-keys (eat-to-choices game elements extended)
                 (filter #(seq (flow-eat-sources game claims %))
                         (map :space elements)))))

(defn- flow-eat-from
  [game _ _]
  (let [options (flow-eat-sources game (game/flow-claims game)
                                  (game/get-action-field game :to))
        any-food? (some #(pos? (game/free-food-present game %)) options)]
    (partial-map
     (comp game/complete-action (partial game/choose-action-field game :from))
     (if any-food? options (take 1 options)))))

(defn- with-spare-food
  [game elements]
  (let [claims (game/flow-claims game)]
    (map #(assoc % :food (game/flow-spare-food claims %)) elements)))

(defn- flow-grow-to
  [game elements extended]
  (except-spaces (grow-to-choices game elements extended)
                 (game/flow-taken-spaces game (game/flow-claims game))))

(defn- flow-move-from
  [game elements extended]
  (except-spaces (move-from-choices game elements extended)
                 (:moved (game/flow-claims game))))

(defn- flow-move-to
  [game elements extended]
  (except-spaces (move-to-choices game elements extended)
                 (game/flow-taken-spaces game (game/flow-claims game))))

(defn- flow-circulate-from
  [game elements extended]
  (let [claims (game/flow-claims game)]
    (select-keys (circulate-from-choices game elements extended)
                 (keep (fn [element]
                         (when (<= (game/circulation game (:space element))
                                   (game/flow-spare-food claims element))
                           (:space element)))
                       elements))))

(def flow-action-choices
  {[:eat :to] flow-eat-to
   [:eat :from] flow-eat-from
   [:grow :element] (fn [game elements extended]
                      (grow-element-choices game (with-spare-food game elements) extended))
   [:grow :from] (fn [game elements extended]
                   (grow-from-choices game (with-spare-food game elements) extended))
   [:grow :to] flow-grow-to
   [:move :from] flow-move-from
   [:move :to] flow-move-to
   [:circulate :from] flow-circulate-from
   [:circulate :to] circulate-to-choices})

;; FIND STATE ---------------------

(declare flow-completes?)

(defn action-field-state
  "The next field of the action underway, or a pass when it has no options."
  [game]
  (let [player (game/current-player game)
        organism (game/current-organism game)
        elements (game/current-organism-elements game)
        extended-elements (get-in (game/extended-organisms game) [player organism])
        {:keys [type action]} (game/get-current-action game)
        fields (get game/action-fields type)
        fields-present (-> action keys set)
        next-field (first
                    (filter
                     (fn [field]
                       (not (fields-present field)))
                     fields))
        next-choices (get (if (game/flow? game) flow-action-choices action-choices)
                          [type next-field])
        choices (next-choices game elements extended-elements)
        ;; Under FLOW only offer what can be finished — a mover with nowhere to
        ;; go is not a choice. Every way of paying for a growth finishes alike.
        choices (if (and (game/flow? game) (not= [:grow :from] [type next-field]))
                  (into {} (filter (comp flow-completes? val) choices))
                  choices)
        action-key (keyword (str (name type) "-" (name next-field)))]
    (cond
      (seq choices) [action-key choices]
      ;; Offers are only made for choices that can be finished, so this is a
      ;; dead end that should not arise; back out rather than pass.
      (game/flow? game) [:flow-cancel
                         {:cancel (game/flow-cancel
                                   game (game/organism-turn-index game))}]
      :else [:pass {:pass (game/pass-action game)}])))

(defn next-action-state
  "An organism about to take its next action: the action or circulate, or a
   pass when neither is possible."
  [game choice]
  ;; Passing is what is left when nothing else can be done, not a move to be
  ;; preferred over doing something — unless the rule is off, which is the
  ;; original game, where it was always on offer.
  (let [choices (choose-action-choices game choice)
        pass {:pass
              (-> game
                  (game/choose-action :circulate)
                  game/pass-action)}]
    (cond
      (empty? choices) [:pass pass]
      game/*require-useful-action* [:choose-action choices]
      :else [:choose-action (merge choices pass)])))

(declare action-field-state)

(defn- flow-completes?
  "Whether a choice being built can be finished. Which food pays for a growth
   never decides whether it can happen, so one way of paying is enough to try."
  [game]
  (if-not (game/flow-building? game)
    true
    (let [[phase choices] (action-field-state game)]
      (and (not= :flow-cancel phase)
           (boolean
            (some flow-completes?
                  (if (= :grow-from phase) (take 1 (vals choices)) (vals choices))))))))

(defn- flow-types-for
  "What may be chosen in an organism: circulate, and the type of every
   declaration acting through it."
  [turns active organism]
  (conj (set (keep (fn [index]
                     (let [turn (nth turns index)]
                       (when (contains? (:organisms turn) organism)
                         (:choice turn))))
                   active))
        :circulate))

(defn flow-offers
  "Every choice that could be started now and finished: {[organism type] game}."
  [game]
  (let [turns (vec (get-in game [:state :player-turn :organism-turns]))
        active (game/flow-active turns)
        organisms (game/player-organisms game (game/current-player game))]
    (into
     {}
     (for [organism (distinct (mapcat #(:organisms (nth turns %)) active))
           type (sort (flow-types-for turns active organism))
           :let [begun (game/flow-begin game organism type)]
           :when (and begun (flow-completes? begun))]
       [[(game/organism-name (get organisms organism)) type] begun]))))

(defn flow-stuck?
  "Whether declaration `index` has nothing it could do this action, whatever
   else is chosen: nothing of its type and no circulate in any organism it acts
   through. It passes without being asked."
  [game index]
  (let [bare (game/flow-strip game)
        turn (get-in bare [:state :player-turn :organism-turns index])]
    (not-any?
     (fn [[organism type]]
       (some-> (game/flow-begin bare organism type) flow-completes?))
     (for [organism (:organisms turn)
           type [(:choice turn) :circulate]]
       [organism type]))))

(defn flow-ready?
  "Whether every declaration that can choose has chosen."
  [game]
  (let [turns (vec (get-in game [:state :player-turn :organism-turns]))]
    (every?
     (fn [index]
       (let [pending (get-in turns [index :pending])]
         (if pending
           (game/complete-action? pending)
           (flow-stuck? game index))))
     (game/flow-active turns))))

(defn- named
  "The id of the current player's organism with this name."
  [game name]
  (some (fn [[id elements]] (when (= name (game/organism-name elements)) id))
        (game/player-organisms game (game/current-player game))))

(defn- flow-compatible-offer?
  [turn organism type]
  (and (contains? (:organisms turn) organism)
       (or (= :circulate type) (= (:choice turn) type))))

(def ^:private plan-budget 2000)

(defn- flow-plan-exists?
  "Whether any whole set of choices could be made this action, from nothing
   chosen. Searched declaration by declaration — every plan answers the first
   unanswered one somehow — and given up as no after `plan-budget` positions,
   so a board too large to settle never leaves a player unable to go on."
  [game]
  (let [budget (atom plan-budget)]
    (letfn [(leaves [game]
              (if-not (game/flow-building? game)
                [game]
                (let [[phase choices] (action-field-state game)]
                  (when-not (= :flow-cancel phase)
                    (mapcat leaves (vals choices))))))
            (search [game]
              (and (pos? (swap! budget dec))
                   (or (flow-ready? game)
                       (let [turns (vec (get-in game [:state :player-turn :organism-turns]))
                             index (first (remove
                                           #(or (get-in turns [% :pending]) (flow-stuck? game %))
                                           (game/flow-active turns)))
                             turn (when index (nth turns index))]
                         (some
                          (fn [[[organism type] begun]]
                            (when (and turn (flow-compatible-offer?
                                             turn (named game organism) type))
                              (some search (leaves begun))))
                          (flow-offers game))))))]
      (boolean (search (game/flow-strip game))))))

(defn flow-state
  "A FLOW turn, once introductions are done. See game/flow? and
   docs/flow-rules.md.

     :flow-declare  click an element of each organism, in any order, to declare
                    that element's type — keyed [organism-name type]
     :flow-choose   make a choice in any organism — keyed [organism-name type],
                    the name being its first space (game/organism-name) — or
                    take one back, keyed [:cancel index]; a declaration left
                    with nothing possible in any complete plan may pass,
                    keyed [:pass index]
     :flow-commit   every choice is made: the action resolves"
  [game]
  (let [player (game/current-player game)
        turns (vec (get-in game [:state :player-turn :organism-turns]))]
    (cond
      (not (game/flow-declared? turns))
      (let [game (if (empty? turns) (game/find-organisms game) game)
            organisms (game/player-organisms game player)]
        [:flow-declare
         (into
          {}
          (for [[organism elements] organisms
                type (declarable-types elements)]
            [[(game/organism-name elements) type]
             (game/flow-declare game organism type)]))])

      (empty? (game/flow-active turns))
      [:actions-complete {:advance (game/resolve-conflicts game player)}]

      (game/flow-building? game)
      (action-field-state game)

      (flow-ready? game)
      [:flow-commit {:advance (game/flow-commit game)}]

      :else
      (let [offers (flow-offers game)
            cancels (into {}
                          (keep-indexed
                           (fn [index {:keys [pending]}]
                             (when pending
                               [[:cancel index] (game/flow-cancel game index)]))
                           turns))
            active (game/flow-active turns)
            blocked (filter
                     (fn [index]
                       (let [turn (nth turns index)]
                         (and (nil? (:pending turn))
                              (not (flow-stuck? game index))
                              (not-any? (fn [[organism type]]
                                          (flow-compatible-offer?
                                           turn (named game organism) type))
                                        (keys offers)))))
                     active)
            passes (when (and (seq blocked) (not (flow-plan-exists? game)))
                     (into {} (for [index blocked]
                                [[:pass index] (game/flow-pass game index)])))]
        [:flow-choose (merge offers cancels passes)]))))

(declare find-state)

(defn state-path
  "The choice keys that lead to this state from the position its choices were
   first asked of -- the present a browser was sent. A browser sends this,
   never the state: the server replays it through the rules and keeps what
   the rules make of it (see organism.game-log)."
  [state]
  (::path (meta state)))

(defn- find-state*
  [{:keys [state] :as game}]
  (let [{:keys [elements captures player-turn]} state
        {:keys [player introduction organism-turns]} player-turn
        organisms (game/player-organisms game player)
        winner (when-not (game/flow-underway? game)
                 (game/victory? game))]

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

      (game/flow? game)
      (flow-state game)

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
      (let [{:keys [choice num-actions actions]} (last organism-turns)]
        (cond
          (nil? choice) [:choose-action-type (choose-action-type-choices game)]

          (every? game/complete-action? actions)
          (cond
            (< (count actions) num-actions)
            (next-action-state game choice)

            (< (count organism-turns) (count organisms))
            (let [acted (set (map :organism organism-turns))
                  missing (remove acted (keys organisms))]
              (println "REMAINING ORGANISMS" missing)
              [:choose-organism (choose-organism-choices game missing)])

            :else [:actions-complete {:advance (game/resolve-conflicts game player)}])

          :else (action-field-state game))))))

(defn find-state
  "[phase choices] for this game: what may happen next, as a map of key to
   the game that key leads to. Each of those games' states carries its path --
   the keys taken to reach it -- so whatever a browser ends up choosing,
   however many steps deep, knows how it got there."
  [game]
  (let [[phase choices] (find-state* game)
        base (or (state-path (:state game)) [])]
    [phase
     (reduce-kv
      (fn [m k v]
        (assoc m k (if (map? (:state v))
                     (update v :state vary-meta assoc ::path (conj base k))
                     v)))
      {} choices)]))

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
  (loop [game initial-game
         n 0]
    (let [[turn choices] (find-state game)
          game-elements (get-in game [:state :elements])
          choice-elements (get-in (:advance choices) [:state :elements])]
      (if (or
           ;; Safety bound: a diverged or cyclic state must never spin the
           ;; (client) main thread and freeze the tab. Legitimate auto-advance
           ;; chains skip only a handful of trivial single-choice phases.
           (> n 1000)
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
        (recur (first (vals choices)) (inc n))))))

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
