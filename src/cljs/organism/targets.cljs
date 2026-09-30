(ns organism.targets
  "What a player can do right now, in the abstract -- which pieces, food and
   spaces can be chosen, and what choosing each does -- for any view to show
   in its own way. The 3D view lights pieces up and rings spaces on the board;
   the 2D board draws its own halos. Neither decides what the moves are: they
   come from the rules' choice tree, walked here.

   A target is a map:

     :kind    :piece  an element on the board        (:space)
              :food   food on an element            (:space, :index)
              :space  a space                        (:space)
              :option one of several types to pick  (:space, :type)
              :ghost  a choice already made, not yet committed (FLOW)
                                                      (:space, :type)
     :tone    how it should look: :act (something to do), :dest (a
              destination), :pending, :pass
     :label   what it is, said out loud
     :group   targets that light together when one is under the pointer
     :send    choosing it makes this choice: a game state reached through the
              choice tree, sent as its path (see choice/state-path)
     :next    choosing it reveals these instead
     :pay     choosing it starts paying for a growth: {:cost n :variants
              [{:spent {grower-space amount} :state s} ...]}, settled by
              choosing food on the growers"
  (:require
   [clojure.string :as string]
   [organism.choice :as choice]
   [organism.game :as game]))

;; ── Walking the choice tree (shared with the 2D board) ────────────────────

(defn compute-from-spaces-and-options
  "Given the post-:choose-action game wrap, return a map
   {space → [{:label ... :destinations [...] :next-state ...
              :sub-options [...]} ...]}
   For grow, the top-level option for each grower has :sub-options listing
   the available element-types as a nested popup."
  [post-action-game-wrap label-prefix]
  (try
    (let [[phase from-choices] (choice/find-state post-action-game-wrap)]
      (cond
        ;; Move/eat/circulate: from-choices is keyed by space directly
        (#{:move-from :eat-to :circulate-from} phase)
        (into {}
              (map
               (fn [space]
                 (let [from-state (get-in from-choices [space :state])
                       from-wrap (assoc post-action-game-wrap :state from-state)
                       dests (try
                               (let [[_ to-choices] (choice/find-state from-wrap)]
                                 (filter vector? (keys to-choices)))
                               (catch :default _ nil))
                       ;; For :eat, suppress the preview entirely when no
                       ;; adjacent space has food — the server auto-advances
                       ;; past :eat-from in that case, so we shouldn't
                       ;; highlight an arbitrary empty source either.
                       dests (if (and (= phase :eat-to) (seq dests))
                               (let [food-map (get-in from-wrap [:state :food] {})
                                     any-food? (some #(pos? (get food-map % 0)) dests)]
                                 (if any-food? dests []))
                               dests)]
                   [space [{:label label-prefix
                            :destinations (or dests [])
                            :next-state from-state}]]))
               (filter vector? (keys from-choices))))

        ;; Grow: top-level option is "GROW", nested sub-options are element types
        (= phase :grow-element)
        (let [type-keys (keys from-choices)
              ;; For each grower space, collect its sub-options (one per type)
              ;; sub-options-by-space: {grower-space [{:label :destinations :next-state} ...]}
              sub-by-space
              (reduce
               (fn [acc type-key]
                 (try
                   (let [type-state (get-in from-choices [type-key :state])
                         type-wrap (assoc post-action-game-wrap :state type-state)
                         [grow-from-phase grow-from-choices] (choice/find-state type-wrap)
                         sub-label (clojure.string/upper-case (name type-key))]
                     (if (= grow-from-phase :grow-from)
                       (reduce
                        (fn [acc contribution]
                          (let [contrib-state (get-in grow-from-choices [contribution :state])
                                contrib-wrap (assoc type-wrap :state contrib-state)
                                [_ to-choices] (choice/find-state contrib-wrap)
                                dests (filter vector? (keys to-choices))
                                sub-opt {:label sub-label
                                         :type type-key
                                         :destinations (or dests [])
                                         :next-state contrib-state}]
                            (reduce
                             (fn [acc space]
                               (update acc space (fnil conj []) sub-opt))
                             acc
                             (keys contribution))))
                        acc
                        (keys grow-from-choices))
                       acc))
                   (catch :default _ acc)))
               {} type-keys)]
          ;; Wrap each grower's sub-options in a single top-level GROW option
          (into {}
                (map
                 (fn [[space subs]]
                   [space [{:label label-prefix
                            :destinations (->> subs
                                                (mapcat :destinations)
                                                distinct
                                                vec)
                            :sub-options subs}]])
                 sub-by-space)))

        :else {}))
    (catch :default _ {})))

(defn compute-move-options
  "Walk the choice tree from the post-:choose-action game wrap (phase
   :move-from) through :move-from → :move-to to build
     {mover-space {dest-space <committed-state>}}
   so clicking a destination commits the full move in one step."
  [post-action-game-wrap]
  (try
    (let [[phase from-choices] (choice/find-state post-action-game-wrap)]
      (if (not= phase :move-from)
        {}
        (reduce
         (fn [acc mover-space]
           (try
             (let [from-state (get-in from-choices [mover-space :state])
                   from-wrap  (assoc post-action-game-wrap :state from-state)
                   [_ to-choices] (choice/find-state from-wrap)]
               (reduce
                (fn [acc dest-space]
                  (let [committed (get-in to-choices [dest-space :state])]
                    (update acc mover-space (fnil assoc {}) dest-space committed)))
                acc
                (filter vector? (keys to-choices))))
             (catch :default _ acc)))
         {}
         (filter vector? (keys from-choices)))))
    (catch :default _ {})))

(defn compute-grow-options
  "Walk the game's choice tree from the post-:choose-action game wrap
   (phase :grow-element) through :grow-element → :grow-from → :grow-to to
   build a nested map
     {grower-space {dest-space [{:type <el-type> :next-state <committed>} ...]}}
   where each committed state has :element, :from, and :to already chosen
   so sending it commits the full grow action in one step."
  [post-action-game-wrap]
  (try
    (let [[phase type-choices] (choice/find-state post-action-game-wrap)]
      (if (not= phase :grow-element)
        {}
        (reduce
         (fn [acc type-key]
           (try
             (let [type-state (get-in type-choices [type-key :state])
                   type-wrap  (assoc post-action-game-wrap :state type-state)
                   [_ contrib-choices] (choice/find-state type-wrap)]
               (reduce
                (fn [acc contribution]
                  (try
                    (let [contrib-state (get-in contrib-choices [contribution :state])
                          contrib-wrap  (assoc post-action-game-wrap :state contrib-state)
                          [_ dest-choices] (choice/find-state contrib-wrap)]
                      (reduce
                       (fn [acc dest-space]
                         (let [committed (get-in dest-choices [dest-space :state])]
                           (reduce
                            (fn [acc grower-space]
                              (update-in acc [grower-space dest-space]
                                         (fnil conj [])
                                         {:type type-key
                                          :next-state committed}))
                            acc
                            (keys contribution))))
                       acc
                       (filter vector? (keys dest-choices))))
                    (catch :default _ acc)))
                acc
                (filter map? (keys contrib-choices))))
             (catch :default _ acc)))
         {} (keys type-choices))))
    (catch :default _ {})))

(defn grow-spent-food
  "Map of {grower-space amount} for elements whose food decreases from
   current-state to next-state — i.e. how much food each space spends for a
   given grow option."
  [current-state next-state]
  (let [nxt (:elements next-state)]
    (into {}
     (keep
      (fn [[space el]]
        (let [spent (- (or (:food el) 0) (or (:food (get nxt space)) 0))]
          (when (pos? spent) [space spent])))
      (:elements current-state)))))

(defn grow-dest-options
  "All grow variants for `dest-space`, pooled across every grower and grouped by
   element type: {type [{:spent {grower-space amount} :state next-state} ...]}.
   Food is a shared pool, so variants that resolve to the same committed state
   (listed under different growers) are deduped."
  [grow-options current-state dest-space]
  (let [variants (distinct
                  (mapcat #(get-in grow-options [% dest-space])
                          (keys grow-options)))]
    (reduce
     (fn [acc {:keys [type next-state]}]
       (update acc type (fnil conj [])
               {:spent (grow-spent-food current-state next-state)
                :state next-state}))
     {}
     variants)))


;; ── Building targets ───────────────────────────────────────────────────────

(defn- state-of [choices key] (get-in choices [key :state]))

(defn- leaf-states
  "The states each next choice from `wrap` leads to: {key state}."
  [wrap]
  (try
    (let [[_ choices] (choice/find-state wrap)]
      (into {} (for [[k v] choices :when (:state v)] [k (:state v)])))
    (catch :default _ {})))

(defn- action-label [type n]
  (str (string/upper-case (name type)) (when n (str ": " n " action" (when (not= n 1) "s")))))

(defn- move-targets [wrap]
  (for [[mover dests] (compute-move-options wrap)]
    {:kind :piece :space mover :tone :act :label "MOVE"
     :next (vec (for [[dest state] dests]
                  {:kind :space :space dest :tone :dest :label "move here" :send state}))}))

(defn- grow-targets
  "A grower, then where to grow, then which type -- and when the food could be
   paid more than one way, which growers pay it."
  [wrap]
  (let [options (compute-grow-options wrap)
        current (:state wrap)
        dests (distinct (mapcat keys (vals options)))
        dest-targets
        (vec (for [dest dests
                   :let [by-type (grow-dest-options options current dest)]]
               {:kind :space :space dest :tone :dest :label "grow here"
                :next (vec (for [[type variants] (sort-by key by-type)
                                 :let [cost (reduce + 0 (vals (:spent (first variants))))]]
                             (cond-> {:kind :option :space dest :type type :tone :act
                                      :label (string/upper-case (name type))}
                               (or (zero? cost) (= 1 (count variants)))
                               (assoc :send (:state (first variants)))
                               (not (or (zero? cost) (= 1 (count variants))))
                               (assoc :pay {:cost cost :variants variants}))))}))]
    ;; Food is pooled, so every destination is reachable from any grower.
    (for [grower (keys options)]
      {:kind :piece :space grower :tone :act :label "GROW" :group :growers
       :next dest-targets})))

(defn- eat-targets [wrap]
  (for [[eater [opt]] (compute-from-spaces-and-options wrap "EAT")
        :let [leaves (leaf-states (assoc wrap :state (:next-state opt)))]]
    (if (seq (:destinations opt))
      {:kind :piece :space eater :tone :act :label "EAT"
       :next (vec (for [dest (:destinations opt)
                        :let [state (get leaves dest)]
                        :when state]
                    {:kind :space :space dest :tone :dest :label "eat from here" :send state}))}
      ;; nothing to eat but the element's own share: choosing it is the choice
      {:kind :piece :space eater :tone :act :label "EAT" :send (:next-state opt)})))

(defn- circulate-targets
  "The food on an element, then the element it goes to."
  [wrap food-on]
  (for [[from [opt]] (compute-from-spaces-and-options wrap "CIRCULATE")
        :let [leaves (leaf-states (assoc wrap :state (:next-state opt)))
              next (vec (for [dest (:destinations opt)
                              :let [state (get leaves dest)]
                              :when state]
                          {:kind :piece :space dest :tone :dest :label "circulate here" :send state}))]
        index (range (max 1 (food-on from)))]
    {:kind :food :space from :index index :tone :act :label "CIRCULATE" :group [:food from]
     :next next}))

(defn- action-targets
  "Everything an organism can do with an action of `type`, from the game the
   choice of that type leads to."
  [type wrap food-on]
  (when wrap
    (case type
      :move (move-targets wrap)
      :grow (grow-targets wrap)
      :eat (eat-targets wrap)
      :circulate (circulate-targets wrap food-on)
      nil)))

(defn- introduce-targets
  "Starting spaces, each given a type, until every space has one. Built from
   the introductions the rules offer, so it only ever reaches one of them; a
   group with one space left fills itself, as on the 2D board."
  [choices]
  (let [offered (keep (fn [k] (when (map? k) (:spaces k))) (keys choices))
        spaces (distinct (mapcat keys offered))
        key-of (into {} (for [k (keys choices) :when (map? k)] [(:spaces k) k]))]
    (letfn [(consistent [progress]
              (filter (fn [o] (every? (fn [[s t]] (= t (get o s))) progress)) offered))
            (settle [progress]
              ;; a space every remaining introduction agrees on is decided
              (let [left (consistent progress)
                    forced (for [s spaces
                                 :when (not (contains? progress s))
                                 :let [types (distinct (map #(get % s) left))]
                                 :when (= 1 (count types))]
                             [s (first types)])]
                (if (seq forced) (settle (into progress forced)) progress)))
            (level [progress]
              (vec (for [s spaces
                         :when (not (contains? progress s))]
                     {:kind :space :space s :tone :act :label "introduce here"
                      :next (vec (for [t (sort (distinct (map #(get % s) (consistent progress))))
                                       :let [progress' (settle (assoc progress s t))]]
                                   (if (= (count progress') (count spaces))
                                     {:kind :option :space s :type t :tone :act
                                      :label (string/upper-case (name t))
                                      :send (state-of choices (key-of progress'))}
                                     {:kind :option :space s :type t :tone :act
                                      :label (string/upper-case (name t))
                                      :placed progress'
                                      :next (level progress')})))})))]
      (level {}))))

(defn- organism-of [state player]
  (fn [id] (keep (fn [[space el]] (when (and (= player (:player el)) (= id (:organism el))) space))
                 (:elements state))))

(defn- pending-targets
  "FLOW: choices made this action, not yet committed. Choosing one takes it
   back."
  [game choices]
  (for [[index {:keys [pending]}] (map-indexed vector (get-in game [:state :player-turn :organism-turns]))
        :when (and pending (not (get-in pending [:action :pass])) (contains? choices [:cancel index]))
        :let [{:keys [type action]} pending
              elements (get-in game [:state :elements])
              [space piece] (case type
                              :move [(:to action) (get-in elements [(:from action) :type])]
                              :grow [(:to action) (:element action)]
                              :eat [(:to action) nil]
                              :circulate [(:to action) nil]
                              [nil nil])]
        :when space]
    {:kind :ghost :space space :type piece :tone :pending
     :label (str "take back " (name type)) :send (state-of choices [:cancel index])}))

(defn- pass-targets [game choices]
  (let [turns (get-in game [:state :player-turn :organism-turns])
        player (game/current-player game)]
    (for [key (keys choices)
          :when (and (vector? key) (= :pass (first key)))
          organism (:organisms (nth turns (second key)))
          space ((organism-of (:state game) player) organism)]
      {:kind :piece :space space :tone :pass :label "nothing is possible: pass"
       :group key :send (state-of choices key)})))

(defn targets
  "What can be chosen now, given the phase and choices find-state offered."
  [game turn choices]
  (let [state (:state game)
        food-on (fn [space] (get-in state [:elements space :food] 0))
        wrap (fn [key] (when-let [s (state-of choices key)] (assoc game :state s)))]
    (vec
     (case turn
       :introduce (introduce-targets choices)

       :choose-organism
       (for [[id v] choices
             :let [chosen (:state v)
                   player (get-in chosen [:player-turn :player])]
             space ((organism-of chosen player) id)]
         {:kind :piece :space space :tone :act :label "act with this organism"
          :group [:organism id] :send chosen})

       :choose-action-type
       (let [numbered (some-> (first (vals choices)) :state)
             elements (when numbered (game/current-organism-elements (assoc game :state numbered)))]
         (for [{:keys [space type]} elements
               :when (contains? choices type)
               :let [n (count (filter #(= type (:type %)) elements))]]
           {:kind :piece :space space :tone :act :label (action-label type n)
            :group [:type type] :send (state-of choices type)}))

       :flow-declare
       (let [numbered (some-> (first (vals choices)) :state)
             player (game/current-player game)]
         (for [[key v] choices
               :let [[name type] key]
               :when (vector? name)
               [space el] (:elements numbered)
               :when (and (= player (:player el)) (= type (:type el))
                          (= name (first (sort ((organism-of numbered player) (:organism el))))))
               :let [n (count (filter #(and (= type (:type %)) (= (:organism el) (:organism %)))
                                      (vals (:elements numbered))))]]
           {:kind :piece :space space :tone :act :label (action-label type n)
            :group key :send (:state v)}))

       :choose-action
       (let [type (:choice (game/get-organism-turn game))]
         (concat (action-targets type (wrap type) food-on)
                 (action-targets :circulate (wrap :circulate) food-on)))

       :flow-choose
       (concat
        (mapcat (fn [[key v]]
                  (when (and (vector? key) (vector? (first key)))
                    (action-targets (second key) (assoc game :state (:state v)) food-on)))
                choices)
        (pending-targets game choices)
        (pass-targets game choices))

       ;; a single field left to fill: whatever spaces it offers
       (for [[key v] choices
             :when (and (vector? key) (string? (first key)) (:state v))]
         {:kind (if (get-in state [:elements key]) :piece :space)
          :space key :tone :dest :label (clojure.core/name turn) :send (:state v)})))))

;; ── Replaying history ─────────────────────────────────────────────────────

(defn- same-position? [a b]
  (and a b (= (select-keys a [:elements :food :player-turn :captures])
              (select-keys b [:elements :food :player-turn :captures]))))

(defn- taken-path
  "The chain of targets, top level down, whose choice made `next-state`: a
   piece, then where it went, then which type, as a player would have
   clicked them. The position stored after a choice can be a few steps on
   from it -- the rules take any step with only one way to go by themselves
   -- so a choice matches if it leads there either way."
  [game targets next-state]
  (let [leads? (fn [state]
                 (and state
                      (or (same-position? state next-state)
                          (same-position?
                           (try (:state (first (choice/find-next-choices (assoc game :state state))))
                                (catch :default _ nil))
                           next-state))))]
    (letfn [(walk [targets]
              (some (fn [t]
                      (cond
                        (leads? (:send t)) [t]
                        (some #(leads? (:state %)) (get-in t [:pay :variants])) [t]
                        (seq (:next t)) (when-let [rest (walk (:next t))] (cons t rest))
                        :else nil))
                    targets))]
      (walk targets))))

(defn replay
  "A position in the history as it was played: what was on offer there, and
   the choice that was made -- found by what each offered choice leads to,
   matched against the position that followed. Nothing in it can be chosen.
   Where the rules took several steps at once there may be no one choice
   that leads to the next position; then only what was offered shows."
  [game next-state]
  (let [[game turn choices] (choice/find-next-choices game)
        offered (targets game turn choices)]
    {:turn turn
     :offered offered
     :taken (when next-state (vec (taken-path game offered next-state)))}))
