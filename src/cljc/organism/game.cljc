(ns organism.game
  (:require
   [clojure.set :as set]
   [organism.base :as base]
   [organism.graph :as graph]))

(def ^:dynamic *food-limit* 111)
(def observer-key "--observer--")

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
     (map
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
   (map second rings)))

(defn mod-space
  "contain the step within the given ring of spaces"
  [color spaces step]
  [color (mod step spaces)])

(defn space-adjacencies
  "find all adjacencies in these rings for the given space"
  [rings space]
  (let [[color step] space
        level (.indexOf (mapv first rings) color)
        same-ring (nth rings level)
        same-spaces (count (last same-ring))
        same (mapv (partial + step) [-1 1])
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
                     (mapv (partial + outer-ratio) [-1 0 1])
                     (mapv (partial + outer-along) [0 1]))]))

        neighbors [same-neighbors inner-neighbors]
        neighbors (if outer
                    (conj neighbors outer)
                    neighbors)

        adjacent-spaces (base/map-cat
                         (fn [[[color spaces] adjacent]]
                           (mapv
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
  (let [colors (mapv first rings)
        [core-color core-spaces] (first rings)
        core (first core-spaces)
        adjacent {core (second (second rings))}
        others (base/map-cat
                (partial ring-adjacencies rings)
                (rest colors))]
    (into adjacent others)))

(defn mod6
  [n]
  (mod n 6))

(defn mod-symmetry
  [symmetry space]
  (let [[ring step] space]
    (if (zero? ring)
      space
      (update
       space
       1
       (fn [step]
         (mod step (* ring symmetry)))))))

(defn apply-direction
  "direction will be in mod symmetry only for center, otherwise mod 6"
  [symmetry space direction]
  (if (= space [0 0])
    [1 direction]
    (let [[ring step] space
          off-axis (mod step ring)
          on-axis? (zero? off-axis)
          axis (quot step ring)
          rotation (mod6 (- direction axis))
          towards
          (if on-axis?
            (cond
              (= rotation 3) [(dec ring) (* axis (dec ring))]
              (#{2 4} rotation) [ring (mod (+ (- 3 rotation) step) (* ring symmetry))]
              :else ;; #{0 1 5}
              (let [bump (* axis (inc ring))
                    offset (if (= 5 rotation) -1 rotation)]
                [(inc ring) (+ bump offset)]))
            (cond
              (#{5 2} rotation) [ring (if (= 2 rotation) (inc step) (dec step))]
              (#{0 1} rotation) [(inc ring) (+ rotation off-axis (* axis (inc ring)))]
              :else ;; #{3 4}
              [(dec ring) (+ off-axis (- 3 rotation) (* axis (dec ring)))]))]
      (mod-symmetry symmetry towards))))

(defn discover-adjacencies
  [rings]
  (let [colors (mapv first rings)
        symmetry (count (last (first (drop 1 rings))))]))

(defn find-corners
  [adjacencies outer-ring symmetry]
  (let [outer (filter
               (comp
                (partial = outer-ring)
                first)
               (keys adjacencies))
        total (count outer)
        jump (quot total symmetry)
        corners (mapv
                 (comp
                  (partial conj [outer-ring])
                  (partial * jump))
                 (range symmetry))]
    corners))

(defn remove-space
  [adjacencies space]
  (let [adjacent (get adjacencies space)
        cut (comp vec (partial remove #{space}))]
    (reduce
     (fn [adjacencies neighbor]
       (update adjacencies neighbor cut))
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
   :eat-from
   :eat-to
   :move-from
   :move-to
   :grow-type
   :grow-source
   :grow-to
   :circulate-from
   :circulate-to])

(defrecord Action [type action])
(defrecord OrganismTurn [organism choice num-actions actions])
(defrecord PlayerTurn [player introduction organism-turns advance])

(defrecord Player [name starting-spaces])
(defrecord Element [player organism type space food captures])
(defrecord State [round elements captures player-turn winner])
(defrecord Game
    [rings adjacencies center capture-limit
     players turn-order organism-victory
     state])

(defn initial-players
  [starting-spaces player-captures]
  (mapv
   (fn [[player spaces] captures]
     [player
      {:starting-spaces spaces
       :capture-limit captures}])
   starting-spaces
   player-captures))

(defn rain-generate
  [mutation-state state]
  (assoc
   state
   :RAIN
   {}))

(def mutation-generate-initial
  {:RAIN rain-generate})

(defn mutation-initial-state
  [mutation mutation-state state]
  (if-let [generate (mutation-generate-initial mutation)]
    (generate mutation-state state)
    state))

(defn initial-state
  [turn-order mutations]
  (let [first-player (first turn-order)
        empty-captures
        (into
         {}
         (mapv
          vector
          turn-order
          (repeat [])))]
    (reduce
     (fn [state [mutation mutation-state]]
       (mutation-initial-state mutation mutation-state state))
     ;; State
     {:round 0
      :elements {}
      :food {}
      :captures empty-captures
      :player-turn
      ;; PlayerTurn
      {:player first-player
       :introduction {}
       :organism-turns []
       :advance nil}}
     mutations)))

(defn initial-game
  "create the initial state for the game from the given adjacencies and player info"
  [rings adjacencies center player-info organism-victory mutations]
  (let [capture-limit 5
        players (into {} player-info)
        turn-order (mapv first player-info)
        state (initial-state turn-order mutations)]
    ;; Game
    {:rings rings
     :adjacencies adjacencies
     :center center
     :capture-limit capture-limit
     :players players
     :turn-order turn-order
     :organism-victory organism-victory
     :mutations mutations
     :state state}))

(defn create-game
  "generate adjacencies for a given symmetry with a ring for each color,
   and the given players"
  ([symmetry colors player-info organism-victory remove-notches?]
   (create-game symmetry colors player-info organism-victory remove-notches? {}))
  ([symmetry colors player-info organism-victory remove-notches? mutations]
   (let [rings (build-rings symmetry colors)
         adjacencies (find-adjacencies rings)
         adjacencies (if remove-notches?
                       (corner-notches
                        adjacencies
                        (last colors)
                        symmetry)
                       adjacencies)]
     (initial-game
      colors adjacencies
      (-> rings first last last)
      player-info organism-victory
      mutations))))

(defn find-mutation
  [game mutation]
  (get-in game [:mutations mutation]))

(defn adjacent-to
  [game space]
  (get-in game [:adjacencies space]))

(defn get-player-turn
  [game]
  (get-in game [:state :player-turn]))

(defn current-round
  [game]
  (get-in game [:state :round]))

(defn current-player
  [{:keys [state] :as game}]
  (get-in state [:player-turn :player]))

(defn get-player
  [game player]
  (get-in game [:players player]))

(defn get-element
  [game space]
  (get-in game [:state :elements space]))

(defn get-captures
  [game player]
  (get-in game [:state :captures player]))

(defn beginning-of-turn?
  [{:keys [state] :as game}]
  (let [{:keys [introduction organism-turns]} (:player-turn state)]
    (and
     (empty? introduction)
     (empty? organism-turns))))

(defn add-element
  [game player organism type space food]
  (let [element
        ;; Element
        {:player player
         :organism organism
         :type type
         :space space
         :food food
         :captures []}]
    (assoc-in game [:state :elements space] element)))

(defn remove-element
  [game space]
  (update-in
   game
   [:state :elements]
   dissoc space))

(defn free-food-present
  [game space]
  (or
   (get-in game [:state :food space])
   0))

(defn add-empty
  [present adding]
  (if-not present
    adding
    (+ present adding)))

(defn drop-free-food
  [game space food]
  (update-in
   game
   [:state :food space]
   add-empty food))

(defn remove-free-food
  [game space]
  (update-in game [:state :food] dissoc space))

(defn claim-free-food
  [game space]
  (let [food (free-food-present game space)]
    (if (zero? food)
      [game 0]
      [(remove-free-food game space) food])))

(defn adjust-food
  [game space amount]
  (update-in
   game
   [:state :elements space :food]
   (partial + amount)))

(defn deconstruct-element
  [game space]
  (let [dropped (inc (get-in game [:state :elements space :food]))]
    (-> game
        (remove-element space)
        (drop-free-food space dropped))))

(defn lose-element
  [game space]
  (if (find-mutation game :EXTRACT)
    (remove-element game space)
    (deconstruct-element game space)))

(defn adjacent-elements
  [state space]
  (remove
   empty?
   (mapv
    (partial get-element state)
    (adjacent-to state space))))

(defn open-element?
  [element]
  (> *food-limit* (:food element)))

(defn full?
  [element]
  (= *food-limit* (:food element)))

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
       (let [adjacent (adjacent-elements game open-space)]
         (some
          (fn [adjacent-element]
            (and
             (= (:type adjacent-element) (:type element))
             (not= (:player adjacent-element) (:player element))))
          adjacent)))
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

(defn element-spaces
  [game]
  (-> game :state :elements keys))

(defn adjacent-element-spaces
  [game space player]
  (filter
   (fn [adjacent]
     (if-let [element (get-element game adjacent)]
       (= player (:player element))))
   (adjacent-to game space)))

(defn contiguous-elements
  [game space]
  (let [element (get-element game space)]
    (loop [spaces [space]
           visited #{}
           contiguous []]
      (if (empty? spaces)
        contiguous
        (let [space (first spaces)
              contiguous (conj contiguous space)
              adjacent (adjacent-element-spaces game space (:player element))
              unseen (remove visited adjacent)]
          (recur
           (concat (rest spaces) unseen)
           (conj visited space)
           contiguous))))))

(defn friendly-adjacent-elements
  [game space]
  (let [element (get-element game space)
        adjacent (adjacent-elements game space)]
    (filter
     (fn [other]
       (= (:player element) (:player other)))
     adjacent)))

(defn fed-element?
  [element]
  (> (:food element) 0))

(defn unfed?
  [element]
  (zero? (:food element)))

(defn commune?
  [game space]
  (let [element (get-element game space)]
    (let [adjacent (friendly-adjacent-elements game space)
          fed (filter fed-element? adjacent)]
      (>= (count fed) 2))))

(defn fed?
  [game space]
  (let [element (get-element game space)]
    (if (find-mutation game :COMMUNE)
      (or
       (fed-element? element)
       (commune? game space))
      (fed-element? element))))

(defn boost?
  [element other]
  (or
   (= :move (:type other))
   (and
    (not= (:space element) (:space other))
    (> (:food other) 0))))

(defn mobile?
  [game space]
  (let [element (get-element game space)
        adjacent (friendly-adjacent-elements game space)
        orbit (conj adjacent element)

        condition?
        (if (find-mutation game :COMMUNE)
          (fn [element]
            (or
             (= :move (:type element))
             (commune? game (:space element))))
          (comp (partial = :move) :type))]
    (some condition? orbit)))

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

(defn can-eat?
  [game element]
  (and
   (open-element? element)
   (> (count (open-spaces game (:space element))) 0)))

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
       conj
       {:player player
        :organism -1
        :type :center
        :space center
        :food 0
        :captures[]})
      game)))

(defn start-turn
  [game player]
  (-> game
      (assoc-in
       [:state :player-turn]
       ;; PlayerTurn
       {:player player
        :introduction {}
        :organism-turns []
        :advance nil})
      (award-center player)))

(defn clear-space
  [game space]
  (-> game
      (remove-element space)
      (remove-free-food space)))

(defn clear-spaces
  [game spaces]
  (reduce clear-space game spaces))

(defn surrounding-spaces
  [game spaces]
  (set
   (base/map-cat
    (:adjacencies game)
    spaces)))

(defn add-elements
  [game player organism food elements]
  (reduce
   (fn [game [space type]]
     (add-element game player organism type space food))
   game elements))

(defn introduce-elements
  [game player {:keys [organism eat grow move] :as introduction}]
  (let [surrounding (surrounding-spaces game [eat grow move])
        spaces {eat :eat grow :grow move :move}]
    (-> game
        (clear-spaces surrounding)
        (add-elements player organism 1 spaces)
        (assoc-in [:state :player-turn :introduction] introduction))))

(defn player-starting-spaces
  [game player]
  (get-in game [:players player :starting-spaces]))

(defn introduce-spaces
  [game player {:keys [organism spaces] :as introduction}]
  (let [starting (player-starting-spaces game player)
        surrounding (surrounding-spaces game starting)]
    (-> game
        (clear-spaces surrounding)
        (add-elements player organism 1 spaces)
        (assoc-in [:state :player-turn :introduction] introduction))))

(defn choose-organism
  [game organism]
  (update-in
   game
   [:state :player-turn :organism-turns]
   conj
   ;; OrganismTurn
   {:organism organism
    :choice nil
    :num-actions -1
    :actions []}))

(defn update-organism-turn
  [game f]
  (update-in
   game
   [:state :player-turn :organism-turns]
   (fn [turns]
     (let [end (-> turns count dec)]
       (update (vec turns) end f)))))

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

(defn all-organisms
  [game]
  (reduce
   (fn [organisms element]
     (if element
       (update-in
        organisms
        [(:player element)
         (:organism element)]
        conj element)
       organisms))
   {}
   (-> game :state :elements vals)))

(defn choose-action-type
  [game type]
  (let [player (current-player game)
        organisms (player-organisms game player)]
    (update-organism-turn
     game
     (fn [{:keys [organism] :as organism-turn}]
       (let [elements (get organisms organism)
             types (group-by :type elements)
             num-actions (count (get types type))]
         (assoc
          organism-turn
          :choice type
          :num-actions num-actions))))))

(defn choose-action
  [game type]
  (update-organism-turn
   game
   (fn [organism-turn]
     (update
      organism-turn
      :actions
      conj {:type type :action {}}))))

(defn update-action
  [game f]
  (update-organism-turn
   game
   (fn [organism-turn]
     (let [end (-> organism-turn :actions count dec)]
       (update-in organism-turn [:actions end] f)))))

(defn pass-action
  [game]
  (update-action
   game
   (fn [action]
     (assoc-in action [:action :pass] true))))

(def action-fields
  {:eat [:to :from]
   :grow [:element :from :to]
   :move [:from :to]
   :circulate [:from :to]})

(defn advance-player-turn
  [game advance]
  (assoc-in game [:state :player-turn :advance] advance))

(defn get-organism-turn
  [game]
  (let [organism-turns (get-in game [:state :player-turn :organism-turns])]
    (last organism-turns)))

(defn get-action-type
  [game]
  (let [organism-turn (get-organism-turn game)]
    (:choice organism-turn)))

(defn get-current-action
  [game]
  (let [organism-turn (get-organism-turn game)
        actions (get organism-turn :actions)]
    (last actions)))

(defn current-organism
  [game]
  (:organism (get-organism-turn game)))

(defn current-organism-elements
  [game]
  (let [player (current-player game)
        organism (current-organism game)
        organisms (player-organisms game player)]
    (get organisms organism)))

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
    (or
     (every? action fields)
     (:pass action))))

(defn eat
  [game {:keys [from to] :as fields}]
  (let [amount (inc (free-food-present game from))]
    (-> game
        (remove-free-food from)
        (adjust-food to amount))))

(defn grow
  [game {:keys [element from to] :as fields}]
  (let [player (current-player game)
        organism (current-organism game)

        game
        (reduce
         (fn [game [space food]]
           (adjust-food game space (* food -1)))
         game
         from)

        [game food]
        (if (find-mutation game :EXTRACT)
          [game 0]
          (claim-free-food game to))]
    (add-element game player organism element to food)))

(defn move
  [game {:keys [from to] :as fields}]
  (let [element (get-element game from)
        element (assoc element :space to)
        game
        (-> game
            (remove-element from)
            (assoc-in
             [:state :elements to]
             element))]
    (if (find-mutation game :EXTRACT)
      game
      (let [[game food] (claim-free-food game to)]
        (adjust-food game to food)))))

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

(defn clear-element-captures
  [game]
  (reduce
   (fn [game space]
     (update-in game [:state :elements space] assoc :captures []))
   game
   (element-spaces game)))

(defn award-capture
  [game player element]
  (update-in
   game
   [:state :captures player]
   conj element))

(defn resolve-conflict
  [game rise fall]
  (let [game
        (if (find-mutation game :EXTRACT)
          (-> game
              (adjust-food (:space rise) (:food fall))
              (cap-food (:space rise)))
          game)]
    (-> game
        (lose-element (:space fall))
        (mark-capture (:space rise) fall)
        (award-capture (:player rise) fall))))

(defn set-add
  [s el]
  (if (not s)
    #{el}
    (conj s el)))

(defn resolve-conflicts
  [game player]
  (let [game (clear-element-captures game)
        conflicting-elements (player-conflicts game player)
        conflicts (reduce
                   (fn [conflicts [from to]]
                     (update conflicts from set-add to))
                   {} conflicting-elements)
        up (reduce
            (fn [up [from to]]
              (assoc up (:space to) from))
            {} conflicting-elements)
        order (graph/kahn-sort conflicts)
        resolved
        (reduce
         (fn [game fall]
           (let [rise (get up (:space fall))]
             (if rise
               (resolve-conflict
                game
                (get-element game (:space rise))
                (get-element game (:space fall)))
               game)))
         game (reverse order))]
    (advance-player-turn resolved :resolve-conflicts)))

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
       (update-in
        organisms
        [(:player element) (:organism element)]
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

(defn persist-integrity
  [game active-player]
  (let [game (find-organisms game)
        organisms (group-organisms game)
        lost-players
        (map
         first
         (remove
          (fn [[player player-organisms]]
            (some (comp alive-elements? last) player-organisms))
          organisms))
        integrity
        (reduce
         (fn [game lost-player]
           (let [lost-organisms (get organisms lost-player)
                 elements
                 (base/map-cat
                  (fn [[organism elements]]
                    elements)
                  lost-organisms)
                 game (reduce lose-element game (map :space elements))]
             (if (= lost-player active-player)
               game
               (award-capture game active-player (assoc (first elements) :type :integrity)))))
         game lost-players)]
    integrity))

(defn base-integrity
  [game active-player]
  (let [game (find-organisms game)
        organisms (group-organisms game)
        organisms-lost
        (reduce
         (fn [lost [player player-organisms]]
           (if (and
                (find-mutation game :RAIN)
                (= player (last (get game :turn-order))))
             lost
             (let [lost-organisms (remove (comp alive-elements? last) player-organisms)]
               (if (empty? lost-organisms)
                 lost
                 (assoc lost player lost-organisms)))))
         {} organisms)
        players-lost (keys organisms-lost)
        other-players (vec (remove #{active-player} players-lost))
        sacrifice
        (reduce
         (fn [game [player player-organisms]]
           (reduce
            (fn [game [organism elements]]
              (let [spaces (map :space elements)
                    game
                    (if (= active-player player)
                      (let [captures (players-captured elements)
                            sacrifice (assoc (first elements) :type :sacrifice)]
                        (reduce
                         (fn [game player]
                           (award-capture game player sacrifice))
                         game captures))
                      game)]
                (reduce lose-element game spaces)))
            game player-organisms))
         game organisms-lost)
        integrity
        (reduce
         (fn [game other-player]
           (award-capture game active-player {:type :integrity :player other-player}))
         sacrifice other-players)]
    integrity))

(defn check-integrity
  [game active-player]
  (let [integrity
        (if (find-mutation game :PERSIST)
          (persist-integrity game active-player)
          (base-integrity game active-player))]
    (advance-player-turn integrity :check-integrity)))

(defn introduce
  [game player {:keys [organism] :as introduction}]
  (let [game
        (if (:spaces introduction)
          (introduce-spaces game player introduction)
          (introduce-elements game player introduction))]
    game))

(def action-map
  {:eat eat
   :grow grow
   :move move
   :circulate circulate})

(defn perform-action
  [game {:keys [type action]}]
  (if (:pass action)
    game
    (if-let [perform (get action-map type)]
      (perform game action)
      (str "unknown action type " type " " (:state game)))))

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

(def rain-symmetry 6)
(def rain-direction 1)
(def rain-interval 5)

(defn grow-rain
  [game rain-player]
  (let [starting (player-starting-spaces game rain-player)
        space (rand-nth starting)
        type (rand-nth [:eat :move :grow])]
    (println "GROW RAIN" rain-player type space starting)
    (println "GAME PLAYERS" (:players game))
    (add-element game rain-player 0 type space 0)))

(defn ring-index
  [indexes [ring step]]
  [(get indexes ring) step])

(defn rain-turn
  [game rain-player]
  (let [round (current-round game)
        rain-elements (get (player-elements game) rain-player)
        adding-rain (inc (quot round rain-interval))
        [index next] (next-player game)
        ring-indexes (into {} (map vector (:rings game) (range)))
        ring-names (into {} (map vector (range) (:rings game)))
        fall
        (reduce
         (fn [game element]
           (move
            game
            {:from (:space element)
             :to (ring-index
                  ring-names
                  (apply-direction
                   rain-symmetry
                   (ring-index
                    ring-indexes
                    (:space element))
                   rain-direction))}))
         game rain-elements)
        _ (println "ADDING RAIN" adding-rain round rain-interval (quot round rain-interval))
        appear
        (reduce
         (fn [game adding]
           (grow-rain game rain-player))
         fall (range adding-rain))]
    (-> appear
        (resolve-conflicts rain-player)
        (check-integrity rain-player)
        (update-in [:state :round] inc)
        (start-turn next))))

(defn start-next-turn
  [game]
  (let [[index next] (next-player game)
        game (start-turn game next)]
    (cond
      (zero? index)
      (update-in game [:state :round] inc)

      (and
       (find-mutation game :RAIN)
       (= index (dec (count (:players game)))))
      (rain-turn game next)

      :else game)))

(defn finish-turn
  [{:keys [state turn-order] :as game}]
  (let [{:keys [player-turn]} state
        {:keys [player]} player-turn
        [index next] (next-player game)]
    (-> game
        (resolve-conflicts player)
        (check-integrity player)
        (start-next-turn))))

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

(defn relative-captures
  [game player]
  (- 
   (count (get-in game [:state :captures player]))
   (-> game :players (get player) (get :capture-limit 5))))

(defn enough-player-captures?
  [game player]
  (>= (relative-captures game player) 0))

(defn player-organism-victory?
  [game player]
  (let [organism-victory (:organism-victory game)
        organisms (player-organisms game player)
        living-organisms
        (filter
         (fn [[organism elements]]
           (alive-elements? elements))
         organisms)
        organism-count (count living-organisms)]
    (>= organism-count organism-victory)))

(defn player-wins?
  [game player]
  (or
   (enough-player-captures? game player)
   (player-organism-victory? game player)))

(defn all-relative-captures
  [game]
  (let [players (:turn-order game)]
    (into
     {}
     (map
      (juxt
       identity
       (partial relative-captures game))
      players))))

(defn find-leader
  [score-map]
  (let [largest-lead (apply max (map last score-map))
        lead-map (group-by last score-map)
        leaders (get lead-map largest-lead)]
    (when (= 1 (count leaders))
      (-> leaders first first))))

(defn capture-victory?
  [game]
  (let [player-captures (all-relative-captures game)
        enough
        (filter
         (fn [[player captures]]
           (>= captures 0))
         player-captures)]
    (when-not (empty? enough)
      (find-leader enough))))

(defn organism-victory?
  [game]
  (let [organism-victory (:organism-victory game)
        organisms (all-organisms game)
        organism-counts
        (map
         (fn [[player organisms]]
           (let [living-organisms
                 (filter
                  (fn [[organism elements]]
                    (alive-elements? elements))
                  organisms)]
             [player (count living-organisms)]))
         organisms)
        enough
        (filter
         (fn [[player organism-count]]
           (>= organism-count organism-victory))
         organism-counts)]
    (when-not (empty? enough)
      (find-leader enough))))

(defn victory?
  [game]
  (or
   (organism-victory? game)
   (capture-victory? game)))

(defn declare-victory
  [game winner]
  (assoc-in
   game
   [:state :winner]
   winner))

(defn check-victory
  [game]
  (if-let [winner (victory? game)]
    (declare-victory game winner)
    game))
