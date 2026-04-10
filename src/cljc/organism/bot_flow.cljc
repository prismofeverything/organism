(ns organism.bot-flow
  "Flowchart-based bot for organism.

   Like journey.bot-flow but tailored to organism's game structure:
   - Operates on the full `game` map (state + adjacencies), not just state
   - Uses organism.choice/find-state for phase+choices
   - Vocabulary covers organism's phases: introduce, choose-action-type,
     choose-action, move-from/to, grow-element/from/to, eat-from/to,
     circulate-from/to

   The scoring heuristics from organism-bot.clj are packaged as 'best-of'
   effects so the flowchart can be a drop-in replacement."
  (:require
   [organism.board :as board]
   [organism.bot-flow-core :as core]
   [organism.choice :as choice]
   [organism.game :as game]))

;; ── Helpers (ported from organism-bot.clj) ──────────────────────────────────

(defn- space-distance-to-center [[ring _step]]
  (cond
    (= 0 ring) 0
    (nil? ring) 99
    :else
    (let [idx (.indexOf board/total-rings ring)]
      (if (neg? idx) 99 (inc idx)))))

(def ^:private beats {:eat :grow :grow :move :move :eat})

(defn- captures? [attacker-type defender-type]
  (= defender-type (get beats attacker-type)))

(defn- enemy-adjacent? [game space player]
  (let [elements (get-in game [:state :elements])
        adjacencies (get (:adjacencies game) space #{})]
    (some (fn [adj]
            (let [el (get elements adj)]
              (and el (not= (:player el) player))))
          adjacencies)))

(defn- friendly-adjacent-count [game space player exclude]
  (let [elements (get-in game [:state :elements])
        adjacencies (get (:adjacencies game) space #{})]
    (count
     (filter (fn [adj]
               (and (not= adj exclude)
                    (let [el (get elements adj)]
                      (and el (= (:player el) player)))))
             adjacencies))))

(defn- has-favorable-capture-opportunity? [game player]
  (let [elements (get-in game [:state :elements])
        player-els (filter #(= player (:player (val %))) elements)]
    (some
     (fn [[space attacker]]
       (let [adjacencies (get (:adjacencies game) space #{})]
         (some
          (fn [adj]
            (let [defender (get elements adj)]
              (and defender
                   (not= (:player defender) player)
                   (captures? (:type attacker) (:type defender)))))
          adjacencies)))
     player-els)))

(defn- action-feasible? [game action-type]
  (try
    (let [f (get choice/action-filters action-type)]
      (boolean (and f (f game))))
    (catch #?(:clj Exception :cljs :default) _ false)))

(defn- organism-food-stats [game]
  (let [elements (try (game/current-organism-elements game)
                      (catch #?(:clj Exception :cljs :default) _ []))
        by-type  (group-by :type elements)
        grower-food (reduce + 0 (map #(get % :food 0) (get by-type :grow [])))
        eater-food  (reduce + 0 (map #(get % :food 0) (get by-type :eat [])))
        total-food  (+ grower-food eater-food
                       (reduce + 0 (map #(get % :food 0) (get by-type :move []))))]
    {:grower-food grower-food :eater-food eater-food :total-food total-food
     :by-type by-type}))

(defn- count-player-types [game]
  (let [elements (game/current-organism-elements game)
        by-type  (group-by :type elements)]
    {:eat  (count (get by-type :eat []))
     :grow (count (get by-type :grow []))
     :move (count (get by-type :move []))}))

;; ── Conditions ──────────────────────────────────────────────────────────────

(def conditions
  {:grow-feasible?
   {:label "can grow"
    :description "True if the grow action is currently feasible"
    :params [] :outputs [:true :false]
    :eval (fn [game _] (action-feasible? game :grow))}

   :has-capture?
   {:label "capture available"
    :description "True if any of our elements can capture an adjacent enemy via heterarchy"
    :params [] :outputs [:true :false]
    :eval (fn [game _]
            (let [player (get-in game [:state :player-turn :player])]
              (boolean (has-favorable-capture-opportunity? game player))))}

   :grower-starving?
   {:label "grower starving"
    :description "True if grower food is 0 but eater food > 0 (need to circulate)"
    :params [] :outputs [:true :false]
    :eval (fn [game _]
            (let [{:keys [grower-food eater-food]} (organism-food-stats game)]
              (and (zero? grower-food) (pos? eater-food))))}

   :total-food-low?
   {:label "food low"
    :description "Total organism food is at or below a threshold"
    :params [{:key :threshold :type :int :default 2}]
    :outputs [:true :false]
    :eval (fn [game {:keys [threshold]}]
            (<= (:total-food (organism-food-stats game)) (or threshold 2)))}

   :phase-is?
   {:label "phase is"
    :description "Current phase matches"
    :params [{:key :phase :type :enum :default :choose-action-type
              :options [:introduce :choose-organism :choose-action-type :choose-action
                        :move-from :move-to :grow-element :grow-from :grow-to
                        :eat-from :eat-to :circulate-from :circulate-to]}]
    :outputs [:true :false]
    :eval (fn [game {:keys [phase]}]
            (= phase (get-in game [:state :player-turn :phase])))}})

;; ── Effects ─────────────────────────────────────────────────────────────────

(defn- pick-keyword* [choices kw]
  (when (contains? choices kw)
    [kw (get choices kw)]))

(defn- score-position
  "Score a hex position by center-distance + enemy adjacency + capture potential."
  [game choice-key player & {:keys [capture-type enemy-weight from-space friend-weight]
                             :or {enemy-weight 30 friend-weight 5}}]
  (let [dist-score (- 20 (space-distance-to-center choice-key))
        enemy-bonus (if (enemy-adjacent? game choice-key player) enemy-weight 0)
        n-friends (if from-space
                    (friendly-adjacent-count game choice-key player from-space)
                    0)
        capture-bonus
        (if capture-type
          (let [elements (get-in game [:state :elements])
                adjacencies (get (:adjacencies game) choice-key #{})]
            (->> adjacencies
                 (keep (fn [adj]
                         (let [def-el (get elements adj)]
                           (when (and def-el
                                      (not= (:player def-el) player)
                                      (captures? capture-type (:type def-el)))
                             200))))
                 (reduce + 0)))
          0)]
    (+ capture-bonus dist-score enemy-bonus (* friend-weight n-friends))))

(defn- pick-best-scored
  "Pick the choice key with the highest score. Adds jitter for tie-breaking."
  [choices score-fn]
  (when (seq choices)
    (let [scored (map (fn [[k v]]
                        [(+ (score-fn k v) (rand 10)) k v])
                      choices)
          [_ k v] (first (sort-by (comp - first) scored))]
      [k v])))

(def effects
  {:pick-action
   {:label "pick action"
    :description "Choose a named action type"
    :params [{:key :action :type :enum :default :grow
              :options [:grow :move :eat :circulate :pass]}]
    :inputs [:in] :outputs []
    :apply (fn [_game choices {:keys [action]}]
             (pick-keyword* choices (or action :grow)))}

   :pick-action-type-best
   {:label "action type (best)"
    :description "Score-based action type selection matching the hard-coded heuristic"
    :params [] :inputs [:in] :outputs []
    :apply
    (fn [game choices _]
      (let [player (get-in game [:state :player-turn :player])
            {:keys [grower-food eater-food total-food]} (organism-food-stats game)
            grow-feasible? (action-feasible? game :grow)
            has-capture? (has-favorable-capture-opportunity? game player)
            score-fn
            (fn [k _v]
              (case k
                :grow (if grow-feasible? (if has-capture? 180 200) -1000)
                :move (cond has-capture? 250
                            grow-feasible? 5
                            (> total-food 2) 140
                            :else 40)
                :circulate (if (and (zero? grower-food) (pos? eater-food)) 150 20)
                :eat (cond has-capture? 15
                           (and (not grow-feasible?) (> total-food 3)) 10
                           (and (not grow-feasible?) (zero? eater-food)) 100
                           :else 30)
                0))]
        (pick-best-scored (dissoc choices :pass) score-fn)))}

   :pick-choose-action-best
   {:label "confirm action (best)"
    :description "At :choose-action, pick circulate if growers starving, else confirm"
    :params [] :inputs [:in] :outputs []
    :apply
    (fn [game choices _]
      (let [{:keys [grower-food eater-food]} (organism-food-stats game)
            need-circulate? (and (zero? grower-food) (pos? eater-food))
            score-fn (fn [k _]
                       (cond (= k :circulate) (if need-circulate? 200 30)
                             (= k :pass) -1000
                             :else 50))]
        (pick-best-scored choices score-fn)))}

   :pick-move-to-best
   {:label "move dest (best)"
    :description "Score positions by connectivity + capture + center + friends"
    :params [] :inputs [:in] :outputs []
    :apply
    (fn [game choices _]
      (let [player (get-in game [:state :player-turn :player])
            from-space (try (game/get-action-field game :from)
                            (catch #?(:clj Exception :cljs :default) _ nil))
            from-el (when from-space (get-in game [:state :elements from-space]))
            score-fn
            (fn [k _]
              (if (and (vector? k) (= 2 (count k)))
                (let [connected? (pos? (friendly-adjacent-count game k player from-space))]
                  (if connected?
                    (score-position game k player
                                   :capture-type (:type from-el)
                                   :from-space from-space)
                    -1000))
                0))]
        (pick-best-scored choices score-fn)))}

   :pick-move-from-best
   {:label "move source (best)"
    :description "Prefer elements that can capture within 2 hops"
    :params [] :inputs [:in] :outputs []
    :apply
    (fn [game choices _]
      (let [player (get-in game [:state :player-turn :player])
            elements (get-in game [:state :elements])
            score-fn
            (fn [k _]
              (if (and (vector? k) (= 2 (count k)))
                (let [el (get elements k)
                      attacker-type (:type el)
                      adj-of-adj (->> (get (:adjacencies game) k #{})
                                      (mapcat #(get (:adjacencies game) % #{}))
                                      distinct)
                      capture-bonus
                      (if attacker-type
                        (->> adj-of-adj
                             (filter (fn [sp]
                                       (let [d (get elements sp)]
                                         (and d (not= (:player d) player)
                                              (captures? attacker-type (:type d))))))
                             count (* 100))
                        0)]
                  (+ capture-bonus (- 10 (space-distance-to-center k))))
                0))]
        (pick-best-scored choices score-fn)))}

   :pick-grow-to-best
   {:label "grow dest (best)"
    :description "Center-seeking + enemy adjacency for grow placement"
    :params [] :inputs [:in] :outputs []
    :apply
    (fn [game choices _]
      (let [player (get-in game [:state :player-turn :player])
            score-fn (fn [k _]
                       (if (and (vector? k) (= 2 (count k)))
                         (score-position game k player :enemy-weight 60)
                         0))]
        (pick-best-scored choices score-fn)))}

   :pick-grow-element-best
   {:label "grow element (best)"
    :description "Prefer element types missing from the organism"
    :params [] :inputs [:in] :outputs []
    :apply
    (fn [game choices _]
      (let [types (count-player-types game)
            score-fn (fn [k _]
                       (let [cnt (get types k 0)]
                         (cond (zero? cnt) 50 (= cnt 1) 20 :else 5)))]
        (pick-best-scored choices score-fn)))}

   :pick-eat-best
   {:label "eat pos (best)"
    :description "Score eat positions by center + enemy adjacency"
    :params [] :inputs [:in] :outputs []
    :apply
    (fn [game choices _]
      (let [player (get-in game [:state :player-turn :player])
            score-fn (fn [k _]
                       (if (and (vector? k) (= 2 (count k)))
                         (score-position game k player)
                         0))]
        (pick-best-scored choices score-fn)))}

   :pick-circulate-from-best
   {:label "circ source (best)"
    :description "Prefer eaters with the most food"
    :params [] :inputs [:in] :outputs []
    :apply
    (fn [game choices _]
      (let [score-fn (fn [k _]
                       (if (and (vector? k) (= 2 (count k)))
                         (let [el (get-in game [:state :elements k])
                               food (or (:food el) 0)]
                           (if (= :eat (:type el)) (+ 50 food) food))
                         0))]
        (pick-best-scored choices score-fn)))}

   :pick-circulate-to-best
   {:label "circ dest (best)"
    :description "Prefer growers with empty food"
    :params [] :inputs [:in] :outputs []
    :apply
    (fn [game choices _]
      (let [score-fn (fn [k _]
                       (if (and (vector? k) (= 2 (count k)))
                         (let [el (get-in game [:state :elements k])
                               grower? (= :grow (:type el))
                               empty? (zero? (or (:food el) 0))
                               dist (- 20 (space-distance-to-center k))]
                           (cond (and grower? empty?) (+ 100 dist)
                                 grower? (+ 50 dist)
                                 :else dist))
                         0))]
        (pick-best-scored choices score-fn)))}

   :pick-first
   {:label "pick first"
    :description "Fallback — pick the first available choice"
    :params [] :inputs [:in] :outputs []
    :apply (fn [_ choices _]
             (when (seq choices)
               [(first (keys choices)) (first (vals choices))]))}

   :jump
   {:label "jump to diagram"
    :params [{:key :diagram :type :diagram-ref :default nil}]
    :inputs [:in] :outputs []
    :apply nil}})

;; ── Vocab bundle (passed to shared interpreter + editor) ────────────────────

(def vocab
  {:conditions conditions
   :effects    effects
   :best-of-effects
   #{:pick-action-type-best :pick-choose-action-best
     :pick-move-to-best :pick-move-from-best
     :pick-grow-to-best :pick-grow-element-best
     :pick-eat-best
     :pick-circulate-from-best :pick-circulate-to-best}})

(defn tile-spec [tile] (core/tile-spec vocab tile))
(defn all-categories [] (core/all-categories vocab))

(def default-bot
  {:start-diagram :main
   :diagrams
   {:main
    {:name "main" :color "#1e3a5a" :collapsed? false
     :origin [80 80] :start-tile :start
     :tiles {:start    {:id :start    :kind :start  :type :start
                        :pos [40 80] :params {}}
             :fallback {:id :fallback :kind :effect :type :pick-first
                        :pos [220 80] :params {}}}
     :links [{:from {:tile :start    :port :out}
              :to   {:tile :fallback :port :in}}]}}})

;; ── Interpreter ─────────────────────────────────────────────────────────────

(defn agent-step
  "Top-level: given a bot definition and an organism game, return
   [choice-key next-game] for the current phase."
  [definition game]
  (let [[_phase choices] (choice/find-state game)]
    (when (seq choices)
      ;; Handle trivial auto-advance cases the same way the hard-coded bot does
      (cond
        (contains? choices :advance)
        [:advance (:advance choices)]

        (and (contains? choices :pass) (= 1 (count choices)))
        [:pass (:pass choices)]

        :else
        (let [real-choices (dissoc choices :pass)
              candidates   (if (seq real-choices) real-choices choices)]
          (core/agent-step vocab definition game candidates))))))
