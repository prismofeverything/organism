(ns organism.game
  (:require
   [organism.graph :as graph]))

(def ^:dynamic *food-limit* 3)

(defn map-cat
  "non-lazy mapcat"
  [f s]
  (reduce into [] (mapv f s)))

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

        adjacent-spaces (map-cat
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
        others (map-cat
                (partial ring-adjacencies rings)
                (rest colors))]
    (into adjacent others)))

(defrecord Player [name starting-spaces captures])
(defrecord Element [player type space food captured?])
(defrecord Space [name element])
(defrecord State [adjacencies players spaces turn round history])

(defn initial-state
  "create the initial state for the game from the given adjacencies and player info"
  [adjacencies player-info]
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
     (into {} players)
     (into {} spaces)
     0 0 [])))

(defn create-game
  "generate adjacencies for a given symmetry with a ring for each color,
   and the given players"
  [symmetry colors player-info]
  (let [rings (build-rings symmetry colors)
        adjacencies (find-adjacencies rings)]
    (initial-state adjacencies player-info)))

(defn add-element
  [state player type space food]
  (let [element (Element. player type space food [])]
    (assoc-in state [:spaces space :element] element)))

(defn remove-element
  [state space]
  (assoc-in state [:spaces space :element] nil))

(defn introduce
  [state player {:keys [eat grow move]}]
  (-> state
      (add-element player :eat eat 1)
      (add-element player :grow grow 1)
      (add-element player :move move 1)))

(defn adjust-food
  [state space amount]
  (update-in
   state
   [:spaces space :element :food]
   (partial + amount)))

(defn eat
  [state player space]
  (adjust-food state space 1))

(defn grow
  [state player source target type]
  (let [state
        (reduce
         (fn [state [space food]]
           (adjust-food state space (* food -1)))
         state
         source)]
    (add-element state player type target 0)))

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
   (fn [elements [space {:keys [element]}]]
     (if element
       (update elements (:player element) conj element)
       elements))
   {}
   (:spaces state)))

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
  (let [adjacents (get-in state [:adjacencies space])]
    (map-cat
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
    (map-cat
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

(defn resolve-conflict
  [state rise fall]
  (-> state
      (adjust-food (:space rise) (:food fall))
      (cap-food (:space rise))
      (remove-element (:space fall))
      (mark-capture (:space rise) fall)
      (update-in [:players (:player rise) :captures] conj fall)))

(defn set-add
  [s el]
  (if (not s)
    #{el}
    (conj s el)))

(defn get-element
  [state space]
  (get-in state [:spaces space :element]))

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

