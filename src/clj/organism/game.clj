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

(defrecord Player [name starting-spaces captures])
(defrecord Element [player organism type space food captures])
(defrecord Space [space element])
(defrecord State [adjacencies center players spaces turn round history])

(defn initial-state
  "create the initial state for the game from the given adjacencies and player info"
  [adjacencies center player-info]
  (let [spaces
        (map
         (fn [space]
           [space (Space. space nil)])
         (keys adjacencies))
        players
        (map
         (fn [[player-name starting-spaces]]
           [player-name
            (Player. player-name starting-spaces [])])
         player-info)]
    (State.
     adjacencies
     center
     (into {} players)
     (into {} spaces)
     0 0 [])))

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
  [state space]
  (get-in state [:adjacencies space]))

(defn get-element
  [state space]
  (get-in state [:spaces space :element]))

(defn add-element
  [state player organism type space food]
  (let [element (Element. player organism type space food [])]
    (assoc-in state [:spaces space :element] element)))

(defn remove-element
  [state space]
  (assoc-in state [:spaces space :element] nil))

(defn adjust-food
  [state space amount]
  (update-in
   state
   [:spaces space :element :food]
   (partial + amount)))

;; ACTIONS -----------------------

(defn introduce
  [state player {:keys [organism eat grow move]}]
  (-> state
      (add-element player organism :eat eat 1)
      (add-element player organism :grow grow 1)
      (add-element player organism :move move 1)))

(defn eat
  [state player space]
  (adjust-food state space 1))

(defn grow
  [state player source target type]
  (let [space (-> source first first)
        element (get-element state space)
        organism (:organism element)
        state
        (reduce
         (fn [state [space food]]
           (adjust-food state space (* food -1)))
         state
         source)]
    (add-element state player organism type target 0)))

(defn move
  [state player from to]
  (let [element (get-in state [:spaces from :element])
        element (assoc element :space to)]
    (-> state
        (remove-element from)
        (assoc-in
         [:spaces to :element]
         element))))

(defn circulate
  [state player from to]
  (-> state
      (adjust-food from -1)
      (adjust-food to 1)))

(defn player-elements
  [state]
  (reduce
   (fn [elements {:keys [space element]}]
     (if element
       (update elements (:player element) conj element)
       elements))
   {}
   (-> state :spaces vals)))

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
  [state {:keys [space player] :as element}]
  (let [adjacents (adjacent-to state space)]
    (base/map-cat
     (fn [adjacent]
       (let [adjacent-element (get-in state [:spaces adjacent :element])]
         (if (and
              adjacent-element
              (not= (:player adjacent-element) player))
           [(heterarchy-sort element adjacent-element)])))
     adjacents)))

(defn player-conflicts
  [state player]
  (let [all-elements (player-elements state)
        elements (get all-elements player)]
    (base/map-cat
     (partial element-conflicts state)
     elements)))

(defn cap-food
  [state space]
  (update-in
   state
   [:spaces space :element :food]
   (fn [food]
     (if (> food *food-limit*)
       *food-limit*
       food))))

(defn mark-capture
  [state space capture]
  (update-in
   state
   [:spaces space :element :captures]
   conj capture))

(defn award-capture
  [state player element]
  (update-in state [:players player :captures] conj element))

(defn resolve-conflict
  [state rise fall]
  (-> state
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
  [state player]
  (let [conflicting-elements (player-conflicts state player)
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
     (fn [state fall]
       (let [rise (get up (:space fall))]
         (if rise
           (resolve-conflict
            state
            (get-element state (:space rise))
            (get-element state (:space fall)))
           state)))
     state (reverse order))))

;; INTEGRITY -----------------------

(defn clear-organisms
  [state]
  (reduce
   (fn [state {:keys [space element]}]
     (if element
       (assoc-in state [:spaces space :element :organism] nil)
       state))
   state
   (-> state :spaces vals)))

(defn set-organism
  [state space organism]
  (assoc-in
   state
   [:spaces space :element :organism]
   organism))

(defn player-organisms
  [state]
  (reduce
   (fn [elements {:keys [space element]}]
     (if element
       (do
         (update
          elements
          [(:player element) (:organism element)]
          conj element))
       elements))
   {}
   (-> state :spaces vals)))

(defn adjacent-elements
  [state space]
  (filter
   (fn [adjacent]
     (get-element state adjacent))
   (adjacent-to state space)))

(defn trace-organism
  [state space organism]
  (loop [state state
         spaces [space]
         visited #{}]
    (if (empty? spaces)
      state
      (let [space (first spaces)
            state (set-organism state space organism)
            adjacent (adjacent-elements state space)
            unseen (remove visited adjacent)]
        (recur state (concat (rest spaces) unseen) (conj visited space))))))

(defn find-organism
  [state space element organism]
  (if element
    (if (:organism element)
      [state organism]
      [(trace-organism state space organism) (inc organism)])
    [state organism]))

(defn find-organisms
  [state]
  (let [state (clear-organisms state)
        [state _]
        (reduce
         (fn [[state organism] {:keys [space element]}]
           (find-organism state space element organism))
         [state 0]
         (-> state :spaces vals))]
    state))

(defn alive?
  [elements]
  (let [by-type (group-by :type elements)]
     (>= (count by-type) 3)))

(defn evaluate-survival
  [organisms]
  (into
   {}
   (map
    (fn [[key elements]]
      [key (alive? elements)])
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
  [state active-player]
  (let [state (find-organisms state)
        organisms (player-organisms state)]
    (reduce
     (fn [state [[player organism] elements]]
       (if (alive? elements)
         state
         (let [spaces (map :space elements)
               state
               (if (= active-player player)
                 (let [captures (players-captured elements)
                       sacrifice (assoc (first elements) :type :sacrifice)]
                   (reduce
                    (fn [state player]
                      (award-capture state player sacrifice))
                    state captures))
                 (let [capture (assoc (first elements) :type :integrity)]
                   (award-capture state active-player capture)))]
           (reduce remove-element state spaces))))
     state organisms)))

(defrecord Action [type action])
(defrecord OrganismTurn [organism choice actions])
(defrecord PlayerTurn [player introduction organism-turns])

(defn perform-action
  [state player {:keys [type action]}]
  (condp = type
    :eat (eat state player (:to action))
    :grow (grow state player (:from action) (:to action) (:element action))
    :move (move state player (:from action) (:to action))
    :circulate (circulate state player (:from action) (:to action))
    (str "unknown action type " type)))

(defn perform-actions
  [state player actions]
  (reduce
   (fn [state action]
     (perform-action state player action))
   state actions))

(defn award-center
  [state player]
  (let [center (:center state)
        center-element (get-element state center)]
    (if (and
         center-element
         (= player (:player center-element)))
      (update-in
       state
       [:players player :captures]
       conj (Element. player -1 :center center 0 []))
      state)))

(defn take-turn
  [state {:keys [player introduction organism-turns] :as turn}]
  (let [state (award-center state player)
        state (if introduction
                (introduce state player introduction)
                state)
        state (reduce
               (fn [state actions]
                 (perform-actions state player actions))
               state (map :actions organism-turns))]
    (-> state
        (resolve-conflicts player)
        (check-integrity player))))

