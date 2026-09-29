(ns organism.game
  (:require
   [clojure.set :as set]
   [organism.base :as base]
   [organism.graph :as graph]
   [organism.random :as random]))

(def ^:dynamic *food-limit* 111)

;; ── Rules ───────────────────────────────────────────────────────────────────
;;
;; Four tightenings, each a flag, all on. Three close holes that trained agents
;; found in the game as first written; the fourth ends a board nothing can
;; change. Each closed a real exploit and each could be wrong about how it does
;; it — one of them withheld a legal turn for six days without anything noticing,
;; because there was nothing to compare the tightened game against.
;;
;; That is what these flags are for. Turned off, this is the original game, and a
;; change to a rule can be diffed against it: every move the tightened game
;; removes should be attributable to a named rule, and a removal no rule claims
;; is a regression. A baseline you cannot run is not a baseline.
;;
;;   (binding [game/*require-useful-action* false] ...)   one rule off
;;   (binding-original-rules ...)                        the game as written

;; How much food an element may hold and still eat. Without a ceiling an eater
;; on a food-rich space can sit and eat for the rest of the game, which is both
;; a dominant strategy and an endless one; five is enough to feed any growth the
;; organism can pay for. Set to *food-limit* for the original game.
(def ^:dynamic *eat-threshold* 5)

;; Whether an organism may declare a turn it cannot use, and pass when it has
;; something to do. Off, every action type is always offered and passing is
;; always available — which is how a deliberate pass used to be half of every
;; action a trained agent selected.
(def ^:dynamic *require-useful-action* true)

;; Whether a player wiped off the board on their own turn leaves their food
;; behind. Off, it drops food+1 on every space they held, which made walking
;; off the map a better food source than eating.
(def ^:dynamic *sacrifice-yields-nothing* true)

;; Whether a board no player can ever change ends the game, against whoever
;; locked it. Off, such a game runs forever.
(def ^:dynamic *stalemate-ends-game* true)
#?(:clj
   (defmacro with-original-rules
     "Run `body` on the game as first written, every tightening off.

      The baseline a rules change is diffed against: a move the tightened game
      removes should be attributable to a named rule, and one no rule claims is a
      regression. Clojure-only because the diffing is done by tests and tooling;
      a browser plays the real game."
     [& body]
     `(binding [*eat-threshold* *food-limit*
                *require-useful-action* false
                *sacrifice-yields-nothing* false
                *stalemate-ends-game* false]
        ~@body)))

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

(defn initial-state
  [turn-order]
  (let [first-player (first turn-order)
        empty-captures
        (into
         {}
         (mapv
          vector
          turn-order
          (repeat [])))]
    {:round 0
     :elements {}
     :food {}
     :captures empty-captures
     :player-turn
     ;; PlayerTurn
     {:player first-player
      :introduction {}
      :organism-turns []
      :advance nil}}))

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

(defn player-starting-spaces
  [game player]
  (get-in game [:players player :starting-spaces]))

(defn rain-player
  [game]
  (-> game :turn-order last))

(defn introduce-rain
  [game]
  (let [rain (rain-player game)
        starting (player-starting-spaces game rain)
        entropy (get-in game [:mutation-state :RAIN :entropy])
        space (rand-nth starting)
        type (rand-nth [:eat :move :grow])]

        ;; space (random/choose entropy starting)
        ;; type (random/choose entropy [:eat :move :grow])

    (add-element game rain 0 type space 0)))

(defn add-rain
  [game rain]
  (reduce
   (fn [game _]
     (introduce-rain game))
   game
   (range rain)))

(defn rain-generate
  [rain-state game]
  (let [rain-state (or rain-state {})
        seed-phrase (get rain-state :seed-phrase)
        ;; entropy (random/phrase->rand seed-phrase)
        initial-rain
        (or
         (:initial-rain rain-state)
         (-> game :turn-order count dec))]
    (-> game
        ;; (assoc-in [:mutation-state :RAIN :entropy] entropy)
        (add-rain initial-rain))))

(def mutation-generate-initial
  {:RAIN rain-generate})

(defn mutation-initial-game
  [mutation mutation-state game]
  (if-let [generate (mutation-generate-initial mutation)]
    (generate mutation-state game)
    game))

(defn initial-game
  "create the initial state for the game from the given adjacencies and player info"
  [rings adjacencies center player-info organism-victory mutations]
  (let [capture-limit 5
        players (into {} player-info)
        turn-order (mapv first player-info)
        state (initial-state turn-order)]
    ;; Game
    (reduce
     (fn [game [mutation mutation-state]]
       (mutation-initial-game mutation mutation-state game))
     {:rings rings
      :adjacencies adjacencies
      :center center
      :capture-limit capture-limit
      :players players
      :turn-order turn-order
      :organism-victory organism-victory
      :mutations mutations
      :state state}
     mutations)))

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

;; FLOW transposes a player's turn. Every organism declares its type before
;; anything happens, and then the turn proceeds one action at a time for all of
;; them at once: each organism with actions left makes its choice, every choice
;; judged against the board as the action began, and only when the last is made
;; does the action commit — all choices together, in a merge whose outcome
;; cannot depend on the order they were clicked in. Between actions the board
;; is regrouped: what touches is one organism, what has parted is two. The
;; rules are written out in docs/flow-rules.md.
;;
;; A FLOW turn keeps its declarations in :organism-turns, one per organism:
;;
;;   {:organism   id the organism had when it declared
;;    :organisms  #{ids} the organisms its elements belong to now
;;    :choice     its declared type
;;    :num-actions
;;    :actions    [committed choices, one per action]
;;    :pending    this action's choice, until the action commits}
;;
;; A choice is {:organism id :type t :action {fields}}. It belongs to the
;; organism it was made in; which declaration it answers is settled by matching.
(defn flow?
  [game]
  (some? (find-mutation game :FLOW)))

(declare complete-action?)

(defn flow-building-index
  "The declaration whose choice is still being filled in, if any."
  [organism-turns]
  (some
   (fn [[index {:keys [pending]}]]
     (when (and pending (not (complete-action? pending)))
       index))
   (map-indexed vector organism-turns)))

(defn organism-turn-index
  "Which organism turn the current choices belong to: the last one, or under
   FLOW the declaration whose choice is being built (-1 when none is)."
  [game]
  (let [turns (vec (get-in game [:state :player-turn :organism-turns]))]
    (if (flow? game)
      (or (flow-building-index turns) -1)
      (dec (count turns)))))

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
  (if (or
       (find-mutation game :EXTRACT)
       (and
        (find-mutation game :RAIN)
        (= (rain-player game)
           (get-in game [:state :elements space :player]))))
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
             (not= (:player adjacent-element) (:player element))
             (or
              (empty? (find-mutation game :RAIN))
              (= (rain-player game) (:player adjacent-element)))))
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
            (if (find-mutation game :RAIN)
              (= (:player adjacent-element) (rain-player game))
              (not= (:player adjacent-element) (:player element))))
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
       (or
        (= player (:player element))
        (and
         (find-mutation game :RAIN)
         (not= (:player element) (rain-player game))))))
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
       (if (find-mutation game :RAIN)
         (not= (:player other) (rain-player game))
         (= (:player element) (:player other))))
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
   (> *eat-threshold* (:food element))
   (> (count (open-spaces game (:space element))) 0)))

;; ACTIONS -----------------------

(defn award-center
  [game player]
  (let [center (:center game)
        center-element (get-element game center)]
    (if (and
         center-element
         (= player (:player center-element))
         (or
          (not (find-mutation game :RAIN))
          (not= player (rain-player game))))
      (update-in
       game
       [:state :captures player]
       conj
       {:player player
        :organism -1
        :type :center
        :space center
        :food 0
        :captures []})
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

(defn add-introduction-elements
  "Clear free food on introduction spaces and place pieces with their normal
   starting food. Free food on neighboring spaces is left alone."
  [game player organism food elements]
  (reduce
   (fn [game [space type]]
     (-> game
         (remove-free-food space)
         (add-element player organism type space food)))
   game elements))

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
        ;; Clear surrounding elements, preserve adjacent food, and reset home food.
        (#(reduce remove-element % surrounding))
        (add-introduction-elements player organism 1 spaces)
        (assoc-in [:state :player-turn :introduction] introduction))))

(defn introduce-spaces
  [game player {:keys [organism spaces] :as introduction}]
  (let [starting (player-starting-spaces game player)
        surrounding (surrounding-spaces game starting)]
    (-> game
        ;; Clear surrounding elements, preserve adjacent food, and reset home food.
        (#(reduce remove-element % surrounding))
        (add-introduction-elements player organism 1 spaces)
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
  (let [index (organism-turn-index game)]
    (update-in
     game
     [:state :player-turn :organism-turns]
     (fn [turns]
       (update (vec turns) index f)))))

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

(defn contiguous-organisms
  [game]
  (reduce
   (fn [organisms element]
     (update
      organisms
      (:organism element)
      conj element))
   {}
   (-> game :state :elements vals)))

(defn organism-players
  [elements]
  (reduce
   (fn [players element]
     (conj players (:player element)))
   #{}
   elements))

(defn extended-organisms
  "organisms including elements belonging to other players (if merged)"
  [game]
  (let [contiguous (contiguous-organisms game)]
    (reduce
     (fn [extended [organism elements]]
       (let [players (seq (organism-players elements))]
         (reduce
          (fn [extended player]
            (update extended player assoc organism elements))
          extended
          players)))
     {}
     contiguous)))

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
     (if (flow? game)
       (update organism-turn :pending f)
       (let [end (-> organism-turn :actions count dec)]
         (update-in organism-turn [:actions end] f))))))

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
  (let [organism-turns (vec (get-in game [:state :player-turn :organism-turns]))
        index (organism-turn-index game)]
    (when (>= index 0)
      (nth organism-turns index))))

(defn get-action-type
  [game]
  (let [organism-turn (get-organism-turn game)]
    (:choice organism-turn)))

(defn get-current-action
  [game]
  (let [organism-turn (get-organism-turn game)]
    (if (flow? game)
      (:pending organism-turn)
      (last (get organism-turn :actions)))))

(defn current-organism
  "The organism acting now. Under FLOW that is where the choice being built
   was made, which need not be where its declaration was."
  [game]
  (if (flow? game)
    (:organism (get-current-action game))
    (:organism (get-organism-turn game))))

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
  "Transfer half the food (rounded up) from `from` to `to`."
  [game {:keys [from to] :as fields}]
  (let [from-element (get-element game from)
        from-food (or (:food from-element) 0)
        amount (long (Math/ceil (/ from-food 2.0)))]
    (-> game
        (adjust-food from (- amount))
        (adjust-food to amount))))

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
  (if (= (:type rise) (:type fall))
    (-> game
        (lose-element (:space rise))
        (lose-element (:space fall))
        (award-capture (:player fall) rise))
    (let [game
          (if (find-mutation game :EXTRACT)
            (-> game
                (adjust-food (:space rise) (:food fall))
                (cap-food (:space rise)))
            game)]
      (-> game
          (lose-element (:space fall))
          (mark-capture (:space rise) fall)
          (award-capture (:player rise) fall)))))

(defn set-add
  [s el]
  (if (not s)
    #{el}
    (conj s el)))

(defn resolve-conflicts
  [game player]
  (let [game (clear-element-captures game)
        conflicting-elements (player-conflicts game player)
        conflicting-elements
        (if (find-mutation game :RAIN)
          (filter
           (fn [conflict]
             (let [conflicting-players (set (map :player conflict))
                   rain (rain-player game)]
               (conflicting-players rain)))
           conflicting-elements)
          conflicting-elements)
        annihilations (filter (fn [[a b]] (= (:type a) (:type b))) conflicting-elements)
        conflicting (remove (fn [[a b]] (= (:type a) (:type b))) conflicting-elements)
        conflicts (reduce
                   (fn [conflicts [from to]]
                     (update conflicts from set-add to))
                   {} conflicting)
        settle (reduce
                (fn [game [a b]]
                  (resolve-conflict game a b))
                game annihilations)
        up (reduce
            (fn [up [from to]]
              (assoc up (:space to) from))
            {} conflicting)
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
         settle (reverse order))]
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

;; FLOW ----------------------------

(declare flow-regroup)

(defn flow-underway?
  "Whether a FLOW turn has begun and not yet been resolved. Organisms are
   regrouped between its actions, so a split can show more living organisms
   mid-turn than the turn will end with once conflicts are settled; victory
   waits."
  [game]
  (let [{:keys [organism-turns advance]} (get-player-turn game)]
    (boolean
     (and (flow? game)
          (seq organism-turns)
          (nil? advance)))))

(defn organism-name
  "An organism named by its first space. Organism ids are numbered as the
   elements happen to be walked, which differs between the server and the
   browser — and the browser replays a bot's turn from the choice keys the
   server sent. A space is the same everywhere, so FLOW keys by this."
  [elements]
  (first (sort (map :space elements))))

(defn flow-declared?
  [organism-turns]
  (boolean (and (seq organism-turns) (every? :choice organism-turns))))

(defn flow-declare
  "Declare `type` for `organism`. Organisms declare in any order and may change
   their minds until the last has declared; then each is given one action per
   element of its type."
  [game organism type]
  (let [player (current-player game)
        organisms (player-organisms game player)
        turns (get-in game [:state :player-turn :organism-turns])
        turns (if (seq turns)
                turns
                (mapv (fn [id]
                        {:organism id :organisms #{id} :choice nil
                         :num-actions -1 :actions []})
                      (sort-by (comp organism-name organisms) (keys organisms))))
        turns (mapv (fn [turn]
                      (if (= organism (:organism turn))
                        (assoc turn :choice type)
                        turn))
                    turns)
        turns (if (flow-declared? turns)
                (mapv (fn [{:keys [organism choice] :as turn}]
                        (assoc turn :num-actions
                               (count (filter #(= choice (:type %))
                                              (get organisms organism)))))
                      turns)
                turns)]
    (assoc-in game [:state :player-turn :organism-turns] turns)))

(defn flow-action-index
  "How many actions have committed this turn."
  [organism-turns]
  (apply max 0 (map (comp count :actions) organism-turns)))

(defn flow-active
  "The declarations with a choice to make in the action now under way."
  [organism-turns]
  (let [index (flow-action-index organism-turns)]
    (keep-indexed
     (fn [i {:keys [num-actions]}]
       (when (> num-actions index) i))
     organism-turns)))

(defn flow-building?
  [game]
  (boolean
   (and (flow? game)
        (flow-building-index (get-in game [:state :player-turn :organism-turns])))))

(defn flow-choices
  "This action's choices so far, in declaration order."
  [organism-turns]
  (keep :pending organism-turns))

(defn flow-compatible?
  "Whether a choice may answer a declaration: made in an organism the
   declaration now acts through, and of its type or a circulate. A pass is made
   for one declaration and answers only that one."
  [turn index choice]
  (if (contains? choice :for)
    (= index (:for choice))
    (and (contains? (:organisms turn) (:organism choice))
         (or (= :circulate (:type choice))
             (= (:choice turn) (:type choice))))))

(defn flow-match
  "Give each choice its own declaration among `active`, or nil if they cannot
   all be answered at once. Small enough to search outright."
  [organism-turns active choices]
  (letfn [(assign [choices free]
            (if (empty? choices)
              []
              (some
               (fn [index]
                 (when (flow-compatible? (nth organism-turns index) index (first choices))
                   (when-let [others (assign (rest choices) (disj free index))]
                     (into [index] others))))
               (sort free))))]
    (assign choices (set active))))

(defn flow-place
  "Set this action's choices, matched to declarations, or nil if they cannot
   all be answered."
  [game choices]
  (let [turns (vec (get-in game [:state :player-turn :organism-turns]))]
    (when-let [assigned (flow-match turns (flow-active turns) choices)]
      (assoc-in
       game [:state :player-turn :organism-turns]
       (reduce
        (fn [turns [index choice]]
          (assoc-in turns [index :pending] choice))
        (mapv #(dissoc % :pending) turns)
        (map vector assigned choices))))))

(defn flow-begin
  "Start a choice of `type` in `organism`, or nil if no declaration is left to
   answer it."
  [game organism type]
  (let [turns (get-in game [:state :player-turn :organism-turns])]
    (flow-place game (conj (vec (flow-choices turns))
                           {:organism organism :type type :action {}}))))

(defn flow-cancel
  "Take back the choice answering declaration `index`."
  [game index]
  (let [turns (vec (get-in game [:state :player-turn :organism-turns]))]
    (flow-place game (flow-choices (update turns index dissoc :pending)))))

(defn flow-pass
  "Declaration `index` makes no choice this action."
  [game index]
  (let [turns (get-in game [:state :player-turn :organism-turns])]
    (flow-place game (conj (vec (flow-choices turns))
                           {:type :circulate :action {:pass true} :for index}))))

(defn flow-strip
  "The action with no choices made yet."
  [game]
  (update-in game [:state :player-turn :organism-turns]
             (partial mapv #(dissoc % :pending))))

(defn circulation
  "How much food a circulate sends: half, rounded up."
  [game space]
  (long (Math/ceil (/ (or (:food (get-element game space)) 0) 2.0))))

(defn flow-claims
  "What the choices already made in this action have claimed, so that no two
   choices take the same thing. The choice being built claims nothing yet.

     :destinations  empty spaces something will move or grow into
     :fed-from      spaces whose free food an eater will take
     :moved         elements that will move
     :drawn         food each element will give up, {space amount}"
  [game]
  (let [turns (get-in game [:state :player-turn :organism-turns])
        made (filter complete-action? (flow-choices turns))]
    (reduce
     (fn [claims {:keys [type action]}]
       (if (:pass action)
         claims
         (case type
           :move (-> claims
                     (update :destinations conj (:to action))
                     (update :moved conj (:from action)))
           :grow (-> claims
                     (update :destinations conj (:to action))
                     (update :drawn #(merge-with + % (:from action))))
           :eat (if (pos? (free-food-present game (:from action)))
                  (update claims :fed-from conj (:from action))
                  claims)
           :circulate (update claims :drawn
                              #(merge-with + % {(:from action)
                                                (circulation game (:from action))})))))
     {:destinations #{} :fed-from #{} :moved #{} :drawn {}}
     made)))

(defn flow-taken-spaces
  "Spaces no further choice may move, grow or eat into: claimed destinations,
   and spaces whose free food is already spoken for."
  [game claims]
  (into (:destinations claims)
        (filter #(pos? (free-food-present game %)) (:fed-from claims))))

(defn flow-spare-food
  "What an element has left to give once this action's choices have drawn on it."
  [claims element]
  (- (:food element) (get-in claims [:drawn (:space element)] 0)))

(defn flow-commit
  "Resolve the action: every choice at once, from the board as it began.

   Each step is a sum over choices, so the order they were clicked in cannot
   change the outcome:

     debit   growth payments and circulation leave their elements
     move    moved elements are lifted together and set down together,
             carrying their food and gathering free food where they land
     grow    grown elements appear, with any free food on their space
     credit  eaten food and circulated food arrive — at the element, wherever
             it now stands

   Then every declaration records its choice (a pass if it had none to make),
   and the board is regrouped."
  [game]
  (let [player (current-player game)
        board game
        turns (vec (get-in game [:state :player-turn :organism-turns]))
        active (set (flow-active turns))
        live (remove #(get-in % [:action :pass])
                     (keep (fn [index] (get-in turns [index :pending])) (sort active)))
        of-type (fn [type] (filter #(= type (:type %)) live))
        extract? (find-mutation game :EXTRACT)

        game
        (reduce
         (fn [game {:keys [type action]}]
           (case type
             :grow (reduce (fn [game [space food]] (adjust-food game space (- food)))
                           game (:from action))
             :circulate (adjust-food game (:from action)
                                     (- (circulation board (:from action))))
             game))
         game live)

        moved (into {} (map (fn [{{:keys [from to]} :action}] [from to]) (of-type :move)))
        lifted (mapv (fn [[from to]] [to (get-element game from)]) moved)
        game (reduce remove-element game (keys moved))
        game
        (reduce
         (fn [game [to element]]
           (let [game (assoc-in game [:state :elements to] (assoc element :space to))]
             (if extract?
               game
               (let [[game food] (claim-free-food game to)]
                 (adjust-food game to food)))))
         game lifted)

        game
        (reduce
         (fn [game {:keys [organism action]}]
           (let [[game food] (if extract? [game 0] (claim-free-food game (:to action)))]
             (add-element game player organism (:element action) (:to action) food)))
         game (of-type :grow))

        now (fn [space] (get moved space space))
        credited
        (reduce
         (fn [game {:keys [type action]}]
           (case type
             :eat (let [amount (inc (free-food-present game (:from action)))]
                    (-> game
                        (remove-free-food (:from action))
                        (adjust-food (now (:to action)) amount)))
             :circulate (adjust-food game (now (:to action))
                                     (circulation board (:from action)))
             game))
         game live)
        game (reduce cap-food credited
                     (distinct (keep (fn [{:keys [type action]}]
                                       (when (#{:eat :circulate} type) (now (:to action))))
                                     live)))

        recorded
        (vec
         (map-indexed
          (fn [index turn]
            (if (active index)
              (-> turn
                  (update :actions (fnil conj [])
                          (or (:pending turn) {:type :circulate :action {:pass true}}))
                  (dissoc :pending))
              (dissoc turn :pending)))
          turns))]
    (flow-regroup
     (assoc-in game [:state :player-turn :organism-turns] recorded))))

(defn flow-regroup
  "Settle what counts as an organism now, between actions of a FLOW turn.

   The current player's elements are grouped by what touches what. A group
   keeps its organism id when it is the only group holding that id and holds no
   other; anything that merged or split gets a fresh one, above every id in
   use, handed out in space order so the result depends on the board alone.
   Each declaration then follows its elements: it acts through every organism
   they now belong to, and two declarations whose organisms joined act through
   the same one."
  [game]
  (let [player (current-player game)
        elements (get-in game [:state :elements])
        groups
        (loop [[space & more :as todo]
               (sort (keep (fn [[space element]]
                             (when (= player (:player element)) space))
                           elements))
               seen #{}
               groups []]
          (cond
            (empty? todo) groups
            (seen space) (recur more seen groups)
            :else
            (let [group (set (contiguous-elements game space))]
              (recur more (into seen group) (conj groups group)))))
        ids-of (fn [group] (set (map #(get-in elements [% :organism]) group)))
        holding (frequencies (mapcat ids-of groups))
        top (apply max -1 (keep :organism (vals elements)))
        assigned
        (first
         (reduce
          (fn [[assigned fresh] group]
            (let [ids (ids-of group)
                  id (first ids)]
              (if (and (= 1 (count ids)) (= 1 (holding id)))
                [(conj assigned [group id]) fresh]
                [(conj assigned [group fresh]) (inc fresh)])))
          [[] (inc top)]
          groups))
        lineage
        (reduce
         (fn [lineage [group id]]
           (reduce
            (fn [lineage space]
              (update lineage (get-in elements [space :organism]) (fnil conj #{}) id))
            lineage group))
         {} assigned)
        game
        (reduce
         (fn [game [group id]]
           (reduce #(set-organism %1 %2 id) game group))
         game assigned)]
    (update-in
     game
     [:state :player-turn :organism-turns]
     (fn [turns]
       (mapv
        (fn [{:keys [organism organisms] :as turn}]
          (let [now (set (mapcat lineage (or organisms [organism])))]
            (assoc turn
                   :organisms now
                   :organism (if (= 1 (count now)) (first now) organism))))
        turns)))))

(defn group-organisms
  "Elements grouped by organism id: {organism-id [element ...]}.

   Keyed by the id alone, which is only unambiguous once find-organisms has
   numbered them — it numbers across every player, so ids are unique. On a
   hand-built position where two players happen to carry the same id, their
   elements land in the same group."
  [game]
  (reduce
   (fn [organisms element]
     (if element
       (update
        organisms
        (:organism element)
        conj element)
       organisms))
   {}
   (-> game :state :elements vals)))

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

        lost-players
        (remove
         (fn [player]
           (some
            (comp alive-elements? last)
            (player-organisms game player)))
         (:turn-order game))

        integrity
        (reduce
         (fn [game lost-player]
           (let [lost-organisms (player-organisms game lost-player)
                 elements (base/map-cat last lost-organisms)
                 game (reduce lose-element game (map :space elements))]
             (if (= lost-player active-player)
               game
               (award-capture
                game
                active-player
                (assoc (first elements) :type :integrity)))))
         game
         lost-players)]
    integrity))

(defn base-integrity
  [game active-player]
  (let [game (find-organisms game)
        organisms (group-organisms game)

        organisms-lost
        (reduce
         (fn [lost [organism-id elements]]
           (let [organism-player?
                 (reduce
                  (fn [players element]
                    (conj players (:player element)))
                  #{}
                  elements)]
             (if (and
                  (find-mutation game :RAIN)
                  (organism-player? (rain-player game)))
               lost
               (if (alive-elements? elements)
                 lost
                 (reduce
                  (fn [lost player]
                    (assoc-in lost [player organism-id] elements))
                  lost
                  (seq organism-player?))))))
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
         game
         organisms-lost)

        ;; Walking yourself off the map used to pay. A lost element leaves its
        ;; food behind, so surrendering on purpose banked more food per turn
        ;; than eating did, several turns running. A player who is gone
        ;; entirely takes their food with them.
        surrendered (map :space (base/map-cat last (get organisms-lost active-player)))

        emptied
        (if (and
             *sacrifice-yields-nothing*
             (seq surrendered)
             (not-any?
              (fn [element] (= active-player (:player element)))
              (vals (get-in sacrifice [:state :elements]))))
          (reduce remove-free-food sacrifice surrendered)
          sacrifice)

        integrity
        (reduce
         (fn [game other-player]
           (award-capture game active-player {:type :integrity :player other-player}))
         emptied other-players)]
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
  ;; Under FLOW nothing is performed until the action commits.
  (if (flow? game)
    game
    (perform-action game (get-current-action game))))

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

(def default-mutation-state
  {:RAIN
   {:initial-rain 2
    :rain-interval 5
    :rain-direction 1
    :seed-phrase "hello world!"}})

(defn ring-index
  [indexes [ring step]]
  [(get indexes ring) step])

(defn project-towards
  [name->index index->name symmetry space direction]
  (let [index-space (ring-index name->index space)
        towards (apply-direction symmetry index-space direction)]
    (ring-index index->name towards)))

(defn rain-turn
  [game rain-player]
  (let [round (current-round game)
        rain-symmetry 6
        rain-state (get-in game [:mutations :RAIN])
        rain-state (if (or (nil? rain-state) (= true rain-state))
                     (:RAIN default-mutation-state)
                     rain-state)
        {:keys [initial-rain rain-interval rain-direction]} rain-state
        rain-elements (get (player-elements game) rain-player)
        adding-rain (+ initial-rain (quot round rain-interval))
        [index next] (next-player game)
        name->index (into {} (map vector (:rings game) (range)))
        index->name (into {} (map vector (range) (:rings game)))
        space->element (into {} (map (juxt :space identity) rain-elements))
        project (partial project-towards name->index index->name rain-symmetry)

        towards
        (into
         {}
         (map
          (fn [rain]
            [(:space rain) #{(project (:space rain) rain-direction)}])
          rain-elements))

        order (reverse (graph/kahn-sort towards))

        fall
        (reduce
         (fn [game space]
           (let [element (space->element space)
                 destination (first (get towards space))]
             (if destination
               (move
                game
                {:from space
                 :to destination})
               game)))
         game order)

        appear (add-rain fall adding-rain)]
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
  "Whether this player alone meets a victory condition.

   Not the victory check — it cannot see anyone else, so it says yes to both
   sides of a tie. victory?/find-leader is what actually decides a game."
  [game player]
  (or
   (enough-player-captures? game player)
   (player-organism-victory? game player)))

(defn all-relative-captures
  [game]
  (let [players
        (if (get-in game [:mutations :RAIN])
          [(rain-player game)]
          (:turn-order game))]
    (into
     {}
     (map
      (juxt
       identity
       (partial relative-captures game))
      players))))

(defn find-leader
  "The single player at the top of a score map of already-qualifying players.

   A tie is settled against whoever's turn produced it — causing a tie loses —
   so the acting player is dropped from the tied leaders and the win goes to
   whoever is left standing. This is the whole of the tie rule: without it a
   tie returned nobody and the game simply carried on past its own ending.

   `acting` is the player whose turn is being finished. The victory check in
   choice/find-state runs after resolve-conflicts and check-integrity but
   before the :check-integrity branch hands the turn on with start-next-turn,
   so player-turn still names the player who just moved.

   If two players who did NOT act are tied at the top there is nobody to hold
   responsible, and no winner is declared."
  ([score-map] (find-leader score-map nil))
  ([score-map acting]
   (let [largest-lead (apply max (map last score-map))
         lead-map (group-by last score-map)
         leaders (map first (get lead-map largest-lead))]
     (if (= 1 (count leaders))
       (first leaders)
       (let [blameless (remove #{acting} leaders)]
         (when (= 1 (count blameless))
           (first blameless)))))))

(defn capture-victory?
  [game]
  (let [player-captures (all-relative-captures game)
        enough
        (filter
         (fn [[player captures]]
           (>= captures 0))
         player-captures)]
    (when-not (empty? enough)
      (find-leader enough (current-player game)))))

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
      (find-leader enough (current-player game)))))

(defn organism-can-feed?
  "Whether this organism could ever have food to spend.

   Food is never what permanently stops a player: eating an adjacent space
   yields a food even when the space is empty, and circulation moves food
   anywhere within the organism. So the question is only whether it holds any
   food already or has somewhere to eat from."
  [game elements]
  (boolean
   (or
    (some (comp pos? :food) elements)
    (some (fn [{:keys [space]}] (seq (open-spaces game space))) elements))))

(defn organism-can-act?
  "Whether this organism could ever move an element or grow a new one — the
   only two things that change which spaces are occupied.

   Both are geometric. Where a piece may move and where one may be grown depend
   on the layout, and by the time food matters `organism-can-feed?` has already
   said whether any is reachable. Deliberately generous: saying yes only means
   the game carries on, while a wrong no would end a game that still had moves
   in it."
  [game elements]
  (cond
    ;; Not a whole organism — integrity is about to take it off the board,
    ;; which is itself a change to the layout.
    (not (alive-elements? elements)) true
    (not (organism-can-feed? game elements)) false
    :else
    (boolean
     (or
      ;; Growth reaches out from the grow elements, not from the whole organism.
      (seq (growable-spaces game (map :space (filter (comp #{:grow} :type) elements))))
      (some
       (fn [{:keys [space]}]
         (and (mobile? game space) (seq (available-spaces game space))))
       elements)))))

(defn player-can-act?
  [game player]
  (let [organisms (player-organisms game player)]
    (if (empty? organisms)
      true                              ; still to introduce
      (boolean (some (fn [elements] (organism-can-act? game elements))
                     (vals organisms))))))

(defn stalemate?
  "True when no player can ever change which spaces are occupied again.

   Elements only appear or move through growth and movement; captures and
   integrity losses follow from those. So when every player is stuck at the
   same time, the layout can never change — which means nobody ever becomes
   unstuck. That the condition holds for everyone simultaneously is what makes
   it permanent rather than one bad turn, and it is why this is checked for the
   whole table rather than per player.

   The board really does lock like this: a lone mover walled in behind its own
   organism, the pieces beside it unable to move because an enemy of the same
   type sits next to the only gap, and the pieces with room to move not mobile
   because no mover is adjacent to them. Nothing either player does afterwards
   can change any of it.

   An occupied centre is the exception: its owner is handed a capture at the
   start of every turn, so that game ends on its own and is not a stalemate
   however frozen the board looks."
  [game]
  (let [game (find-organisms game)]
    (boolean
     (and
      (seq (get-in game [:state :elements]))
      (nil? (get-element game (:center game)))
      (not-any? (partial player-can-act? game) (:turn-order game))))))

(defn stalemate-victory?
  "Who wins a locked board: anybody but whoever locked it.

   The same rule the game already applies to ties. Causing a tie loses, because
   a player who can see the ending coming should not be able to take everyone
   down with them; walking the board into a position nothing can ever change is
   the same act, and costs the same. Among the players left the leader takes it,
   by the relative-capture count ties are settled on.

   Checked after the real victories, so a game that was won on its merits is
   never reinterpreted as a lock."
  [game]
  (when (and *stalemate-ends-game* (stalemate? game))
    (let [blameless (dissoc (all-relative-captures game) (current-player game))]
      (when (seq blameless)
        (find-leader blameless)))))

(defn victory?
  [game]
  (or
   (organism-victory? game)
   (capture-victory? game)
   (stalemate-victory? game)))

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
