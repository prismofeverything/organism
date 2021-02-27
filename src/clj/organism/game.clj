(ns organism.game
  (:require
   [clojure.set :as set]
   [organism.base :as base]
   [organism.graph :as graph]))

(def ^:dynamic *food-limit* 3)

;; BOARD ----------------------

(defn build-ring
  "build the spaces in a ring"
  [symmetry color level]
  (mapv
   (fn [step]
     [color step])
   (range (* level symmetry))))

(defn build-rings
  "build rings of all the colors with the given symmetry"
  [symmetry colors]
  (let [core-color (first colors)
        core (list [core-color 0])]
    (concat
     [[core-color core]]
     (mapv
      (fn [color level]
        [color
         (build-ring
          symmetry
          color
          level)])
      (rest colors)
      (map inc (range))))))

(defn rings->spaces
  "get just the list of spaces from the nested rings strcture"
  [rings]
  (apply
   concat
   (mapv second rings)))

(defn mod-space
  "contain the step within the given ring of spaces"
  [color spaces step]
  [color (mod step spaces)])

(defn space-adjacencies
  "find all adjacencies in these rings for the given space"
  [rings space]
  (let [[color step] space
        level (.indexOf (map first rings) color)
        same-ring (nth rings level)
        same-spaces (count (last same-ring))
        same (map (partial + step) [-1 1])
        same-neighbors [[color same-spaces] same]        

        along (mod step level)
        axis? (zero? along)
        cycle (quot step level)

        inner-ring (nth rings (dec level))
        inner-color (first inner-ring)
        inner-spaces (count (last inner-ring))
        inner-ratio (* (dec level) cycle)
        inner-along (dec along)
        inner-space (+ inner-ratio inner-along)
        inner (if axis?
                [inner-ratio]
                [inner-space (inc inner-space)])
        inner-neighbors [[inner-color inner-spaces] inner]

        outer? (< level (dec (count rings)))
        outer (if outer?
                (let [outer-ring (nth rings (inc level))
                      outer-color (first outer-ring)
                      outer-spaces (count (last outer-ring))
                      outer-ratio (* (inc level) cycle)
                      outer-along (+ along outer-ratio)]
                  [[outer-color outer-spaces]
                   (if axis?
                     (map (partial + outer-ratio) [-1 0 1])
                     (map (partial + outer-along) [0 1]))]))

        neighbors [same-neighbors inner-neighbors]
        neighbors (if outer
                    (conj neighbors outer)
                    neighbors)

        adjacent-spaces (base/map-cat
                         (fn [[[color spaces] adjacent]]
                           (map
                            (partial mod-space color spaces)
                            adjacent))
                         neighbors)]

    adjacent-spaces))

(defn ring-adjacencies
  "find all adjacencies for all spaces in the ring of the given color"
  [rings color]
  (let [spaces (get (into {} rings) color)]
    (mapv
     (juxt
      identity
      (partial
       space-adjacencies
       rings))
     spaces)))

(defn find-adjacencies
  "find all adjacencies for all rings"
  [rings]
  (let [colors (map first rings)
        [core-color core-spaces] (first rings)
        core (first core-spaces)
        adjacent {core (second (second rings))}
        others (base/map-cat
                (partial ring-adjacencies rings)
                (rest colors))]
    (into adjacent others)))

(defn find-corners
  [adjacencies outer-ring symmetry]
  (let [outer (filter
               (comp
                (partial = outer-ring)
                first)
               (keys adjacencies))
        total (count outer)
        jump (quot total symmetry)
        corners (map
                 (comp
                  (partial conj [outer-ring])
                  (partial * jump))
                 (range symmetry))]
    corners))

(defn remove-space
  [adjacencies space]
  (let [adjacent (get adjacencies space)]
    (reduce
     (fn [adjacencies neighbor]
       (update adjacencies neighbor remove #{space}))
     (dissoc adjacencies space)
     adjacent)))

(defn corner-notches
  [adjacencies outer-ring symmetry]
  (let [corners (find-corners adjacencies outer-ring symmetry)]
    (reduce remove-space adjacencies corners)))

;; STATE ---------------------------------

(def phases
  [:introduce
   :choose-organism
   :choose-action
   :eat
   :move-from
   :move-to
   :grow-type
   :grow-source
   :grow-to
   :circulate-from
   :circulate-to])

(defrecord Action [type action])
(defrecord OrganismTurn [organism choice actions])
(defrecord PlayerTurn [player introduction organism-turns])

(defrecord Player [name starting-spaces])
(defrecord Element [player organism type space food captures])
(defrecord State [elements captures player-turn])
(defrecord Game
    [adjacencies center capture-limit
     players turn-order round history
     state])

(defn initial-state
  "create the initial state for the game from the given adjacencies and player info"
  [adjacencies center player-info]
  (let [capture-limit 5
        players
        (map
         (fn [[player-name starting-spaces]]
           [player-name
            (Player. player-name starting-spaces)])
         player-info)
        turn-order (map first players)
        empty-captures
        (into {} (map vector turn-order (repeat [])))
        first-player (first turn-order)
        state
        (State.
         {}
         empty-captures
         (PlayerTurn. first-player {} []))]
    (Game.
     adjacencies
     center
     capture-limit
     (into {} players)
     (map first players)
     0 []
     state)))

(defn create-game
  "generate adjacencies for a given symmetry with a ring for each color,
   and the given players"
  [symmetry colors player-info remove-notches?]
  (let [rings (build-rings symmetry colors)
        adjacencies (find-adjacencies rings)
        adjacencies (if remove-notches?
                      (corner-notches
                       adjacencies
                       (last colors)
                       symmetry)
                      adjacencies)]
    (initial-state adjacencies (-> rings first last last) player-info)))

(defn adjacent-to
  [game space]
  (get-in game [:adjacencies space]))

(defn active-player
  [{:keys [state] :as game}]
  (:active-player state))

(defn get-player
  [game player]
  (get-in game [:players player]))

(defn get-element
  [game space]
  (get-in game [:state :elements space]))

(defn get-captures
  [game player]
  (get-in game [:state :captures player]))

(defn add-element
  [game player organism type space food]
  (let [element (Element. player organism type space food [])]
    (assoc-in game [:state :elements space] element)))

(defn remove-element
  [game space]
  (update-in
   game
   [:state :elements]
   dissoc space))

(defn adjust-food
  [game space amount]
  (update-in
   game
   [:state :elements space :food]
   (partial + amount)))

(defn adjacent-elements
  [state space]
  (remove
   empty?
   (map
    (partial get-element state)
    (adjacent-to state space))))

(defn open?
  [element]
  (> *food-limit* (:food element)))

(defn open-spaces
  [game space]
  (filter
   (fn [adjacent]
     (empty? (get-element game adjacent)))
   (adjacent-to game space)))

(defn available-spaces
  [game space]
  (let [element (get-element game space)
        open (open-spaces game space)]
    (remove
     (fn [open-space]
       (let [adjacent (adjacent-elements game open-space)
             elements (map (partial get-element game) adjacent)]
         (some
          (fn [adjacent-element]
            (and
             (= (:type adjacent-element) (:type element))
             (not= (:player adjacent-element) (:player element))))
          elements)))
     open)))

(defn growable-adjacent
  [game space]
  (let [element (get-element game space)
        open (open-spaces game space)]
    (remove
     (fn [open-space]
       (let [adjacent (adjacent-elements game open-space)]
         (some
          (fn [adjacent-element]
            (not= (:player adjacent-element) (:player element)))
          adjacent)))
     open)))

(defn growable-spaces
  [game spaces]
  (set
   (base/map-cat
    (partial growable-adjacent game)
    spaces)))

(defn adjacent-element-spaces
  [game space]
  (filter
   (fn [adjacent]
     (get-element game adjacent))
   (adjacent-to game space)))

(defn contiguous-elements
  [game space]
  (loop [spaces [space]
         visited #{}
         contiguous []]
    (if (empty? spaces)
      contiguous
      (let [space (first spaces)
            contiguous (conj contiguous space)
            adjacent (adjacent-element-spaces game space)
            unseen (remove visited adjacent)]
        (recur
         (concat (rest spaces) unseen)
         (conj visited space)
         contiguous)))))

(defn fed-element?
  [element]
  (> (:food element) 0))

(defn fed?
  [game space]
  (let [element (get-element game space)]
    (fed-element? element)))

(defn mobile?
  [game space]
  (let [element (get-element game space)
        adjacent (adjacent-elements game space)
        all-elements (conj adjacent element)]
    (some
     (comp (partial = :move) :type)
     all-elements)))

(defn alive-elements?
  [elements]
  (let [by-type (group-by :type elements)]
    (>= (count by-type) 3)))

(defn alive?
  [game space]
  (let [contiguous (contiguous-elements game space)
        elements
        (map
         (partial get-element game)
         contiguous)]
    (alive-elements? elements)))

(defn can-move?
  [game space]
  (and
   (fed? game space)
   (mobile? game space)
   (alive? game space)))

;; ACTIONS -----------------------

(defn award-center
  [game player]
  (let [center (:center game)
        center-element (get-element game center)]
    (if (and
         center-element
         (= player (:player center-element)))
      (update-in
       game
       [:state :captures player]
       conj (Element. player -1 :center center 0 []))
      game)))

(defn start-turn
  [game player]
  (-> game
      (assoc-in
       [:state :player-turn]
       (PlayerTurn. player {} []))
      (award-center player)))

(defn introduce
  [game player {:keys [organism eat grow move] :as introduction}]
  (-> game
      (add-element player organism :eat eat 1)
      (add-element player organism :grow grow 1)
      (add-element player organism :move move 1)
      (assoc-in [:state :player-turn :introduction] introduction)))

(defn choose-organism
  [game organism]
  (update-in
   game
   [:state :player-turn :organism-turns]
   conj (OrganismTurn. organism nil [])))

(defn update-organism-turn
  [game f]
  (update-in
   game
   [:state :player-turn :organism-turns]
   (fn [turns]
     (let [end (-> turns count dec)]
       (update turns end f)))))

(defn choose-action-type
  [game type]
  (update-organism-turn
   game
   (fn [organism-turn]
     (assoc organism-turn :choice type))))

(defn choose-action
  [game type]
  (update-organism-turn
   game
   (fn [organism-turn]
     (update
      organism-turn
      :actions
      conj (Action. type {})))))

(defn update-action
  [game f]
  (update-organism-turn
   game
   (fn [organism-turn]
     (let [end (-> organism-turn :actions count dec)]
       (update-in organism-turn [:actions end] f)))))

(def action-fields
  {:eat [:to]
   :grow [:element :from :to]
   :move [:from :to]
   :circulate [:from :to]})

(defn get-organism-turn
  [game]
  (let [organism-turns (get-in game [:state :player-turn :organism-turns])]
    (last organism-turns)))

(defn get-current-action
  [game]
  (let [organism-turn (get-organism-turn game)
        actions (get organism-turn :actions)]
    (last actions)))

(defn get-action-field
  [game field]
  (get-in
   (get-current-action game)
   [:action field]))

(defn choose-action-field
  [game field value]
  (update-action
   game
   (fn [action]
     (assoc-in action [:action field] value))))

(defn apply-action-fields
  [game fields]
  (update-action
   game
   (fn [action]
     (update action :action merge fields))))

(defn record-action
  [game action fields]
  (let [game (choose-action game action)]
    (reduce
     (fn [game [field value]]
       (choose-action-field game field value))
     game fields)))

(defn complete-action?
  [{:keys [type action]}]
  (let [fields (get action-fields type)]
    (every? action fields)))

(defn eat
  [game {:keys [to] :as fields}]
  (-> game
      (adjust-food to 1)))

(defn grow
  [game {:keys [element from to] :as fields}]
  (let [space (-> from first first)
        {:keys [organism player]} (get-element game space)
        game
        (reduce
         (fn [game [space food]]
           (adjust-food game space (* food -1)))
         game
         from)]
    (add-element game player organism element to 0)))

(defn move
  [game {:keys [from to] :as fields}]
  (let [element (get-element game from)
        element (assoc element :space to)]
    (-> game
        (remove-element from)
        (assoc-in
         [:state :elements to]
         element))))

(defn circulate
  [game {:keys [from to] :as fields}]
  (-> game
      (adjust-food from -1)
      (adjust-food to 1)))

(defn player-elements
  [game]
  (reduce
   (fn [elements element]
     (if element
       (update elements (:player element) conj element)
       elements))
   {}
   (-> game :state :elements vals)))

;; CONFLICTS ------------------

(def heterarchy
  {:eat :grow
   :grow :move
   :move :eat})

(defn heterarchy-sort
  [a b]
  (if (= (get heterarchy (:type a))
         (:type b))
    [a b]
    [b a]))

(defn element-conflicts
  [game {:keys [space player] :as element}]
  (let [adjacents (adjacent-to game space)]
    (base/map-cat
     (fn [adjacent]
       (let [adjacent-element (get-element game adjacent)]
         (if (and
              adjacent-element
              (not= (:player adjacent-element) player))
           [(heterarchy-sort element adjacent-element)])))
     adjacents)))

(defn player-conflicts
  [game player]
  (let [all-elements (player-elements game)
        elements (get all-elements player)]
    (base/map-cat
     (partial element-conflicts game)
     elements)))

(defn cap-food
  [game space]
  (update-in
   game
   [:state :elements space :food]
   (fn [food]
     (if (> food *food-limit*)
       *food-limit*
       food))))

(defn mark-capture
  [game space capture]
  (update-in
   game
   [:state :elements space :captures]
   conj capture))

(defn award-capture
  [game player element]
  (update-in
   game
   [:state :captures player]
   conj element))

(defn resolve-conflict
  [game rise fall]
  (-> game
      (adjust-food (:space rise) (:food fall))
      (cap-food (:space rise))
      (remove-element (:space fall))
      (mark-capture (:space rise) fall)
      (award-capture (:player rise) fall)))

(defn set-add
  [s el]
  (if (not s)
    #{el}
    (conj s el)))

(defn resolve-conflicts
  [game player]
  (let [conflicting-elements (player-conflicts game player)
        conflicts (reduce
                   (fn [conflicts [from to]]
                     (update conflicts from set-add to))
                   {} conflicting-elements)
        up (reduce
            (fn [up [from to]]
              (assoc up (:space to) from))
            {} conflicting-elements)
        order (graph/kahn-sort conflicts)]
    (reduce
     (fn [game fall]
       (let [rise (get up (:space fall))]
         (if rise
           (resolve-conflict
            game
            (get-element game (:space rise))
            (get-element game (:space fall)))
           game)))
     game (reverse order))))

;; INTEGRITY -----------------------

(defn clear-organisms
  [game]
  (reduce
   (fn [game element]
     (if element
       (assoc-in
        game
        [:state :elements (:space element) :organism]
        nil)
       game))
   game
   (-> game :state :elements vals)))

(defn set-organism
  [game space organism]
  (assoc-in
   game
   [:state :elements space :organism]
   organism))

(defn group-organisms
  [game]
  (reduce
   (fn [organisms element]
     (if element
       (update
        organisms
        [(:player element) (:organism element)]
        conj element)
       organisms))
   {}
   (-> game :state :elements vals)))

(defn player-organisms
  [game player]
  (reduce
   (fn [organisms element]
     (if (and element (= player (:player element)))
       (update
        organisms
        (:organism element)
        conj element)
       organisms))
   {}
   (-> game :state :elements vals)))

(defn trace-organism
  [game center-space organism]
  (let [spaces (contiguous-elements game center-space)]
    (reduce
     (fn [game space]
       (set-organism game space organism))
     game spaces)))

(defn find-organism
  [game element organism]
  (if element
    (if (:organism element)
      [game organism]
      [(trace-organism game (:space element) organism)
       (inc organism)])
    [game organism]))

(defn find-organisms
  [game]
  (let [game (clear-organisms game)
        [game _]
        (reduce
         (fn [[game organism] element]
           (find-organism game element organism))
         [game 0]
         (-> game :state :elements vals))]
    game))

(defn evaluate-survival
  [organisms]
  (into
   {}
   (map
    (fn [[key elements]]
      [key (alive-elements? elements)])
    organisms)))

(defn players-captured
  [elements]
  (reduce
   (fn [players element]
     (set/union
      players
      (set
       (map
        :player
        (:captures element)))))
   #{}
   elements))

(defn check-integrity
  [game active-player]
  (let [game (find-organisms game)
        organisms (group-organisms game)]
    (reduce
     (fn [game [[player organism] elements]]
       (if (alive-elements? elements)
         game
         (let [spaces (map :space elements)
               game
               (if (= active-player player)
                 (let [captures (players-captured elements)
                       sacrifice (assoc (first elements) :type :sacrifice)]
                   (reduce
                    (fn [game player]
                      (award-capture game player sacrifice))
                    game captures))
                 (let [capture (assoc (first elements) :type :integrity)]
                   (award-capture game active-player capture)))]
           (reduce remove-element game spaces))))
     game organisms)))

(def action-map
  {:eat eat
   :grow grow
   :move move
   :circulate circulate})

(defn perform-action
  [game {:keys [type action]}]
  (if-let [perform (get action-map type)]
    (perform game action)
    (str "unknown action type " type " " (:state game))))

(defn complete-action
  [game]
  (let [action (get-current-action game)]
    (perform-action game action)))

(defn perform-actions
  [game actions]
  (reduce
   (fn [game {:keys [type action] :as action-turn}]
     (-> game
         (record-action type action)
         (perform-action action-turn)))
   game actions))

(defn next-player
  [{:keys [state turn-order] :as game}]
  (let [{:keys [player-turn]} state
        {:keys [player]} player-turn
        index (.indexOf turn-order player)
        next-index (mod (inc index) (count turn-order))]
    [next-index (nth turn-order next-index)]))

(defn finish-turn
  [{:keys [state turn-order] :as game}]
  (let [{:keys [player-turn]} state
        {:keys [player]} player-turn
        [index next] (next-player game)
        game
        (-> game
            (resolve-conflicts player)
            (check-integrity player)
            (update :history conj player-turn)
            (start-turn next))]
    (if (zero? index)
      (update game :round inc)
      game)))

(defn apply-turn
  [game {:keys [player introduction organism-turns] :as player-turn}]
  (let [game (start-turn game player)
        game (if introduction
               (introduce game player introduction)
               game)
        game
        (reduce
         (fn [game {:keys [organism choice actions] :as organism-turn}]
           (-> game
               (choose-organism organism)
               (choose-action-type choice)
               (perform-actions actions)))
         game organism-turns)]
    (finish-turn game)))

(defn enough-captures?
  [game player]
  (>=
   (count (get-in game [:state :captures player]))
   (:capture-limit game)))

(defn three-organisms?
  [game player]
  (>= (count (player-organisms game player)) 3))

(defn player-wins?
  [game player]
  (or
   (enough-captures? game player)
   (three-organisms? game player)))
