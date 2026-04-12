(ns organism.bot-flow-core
  "Shared flowchart interpreter for all games.

   The interpreter walks a bot definition (diagrams → tiles → links) and
   evaluates conditions, follows links, and applies effects.  Game-specific
   vocabulary (conditions, effects) is passed in as data so this module has
   no game-specific dependencies.

   A bot definition is:
     {:start-diagram :main
      :diagrams {:main {:start-tile :start
                        :tiles {id {:id :kind :type :pos :params}}
                        :links [{:from {:tile :port} :to {:tile :port}}]}}}

   The interpreter takes:
     - `vocab`       — {:conditions {type spec} :effects {type spec}}
     - `definition`  — the bot definition
     - `game-state`  — opaque state passed to condition :eval and effect :apply fns
     - `choices`     — the current choices map {choice-key → next-state}")

;; ── Spec lookups (used by editor) ────────────────────────────────────────────

(defn tile-spec
  "Return the spec map for a tile from the given vocab."
  [vocab tile]
  (case (:kind tile)
    :condition (get-in vocab [:conditions (:type tile)])
    :logic     (get-in vocab [:logic (:type tile)])
    :effect    (get-in vocab [:effects (:type tile)])
    :jump      (get-in vocab [:effects :jump])
    :start     {:label "start" :outputs [:out]}
    nil))

(defn all-categories
  "Return editor palette categories from the given vocab."
  [vocab]
  (let [best-of (or (:best-of-effects vocab) #{})]
    (cond-> [{:category :conditions
              :tiles (for [[k v] (:conditions vocab)]
                       {:kind :condition :type k :label (:label v)
                        :description (:description v)})}]
      (:logic vocab)
      (conj {:category :logic
             :tiles (for [[k v] (:logic vocab)]
                      {:kind :logic :type k :label (:label v)
                       :description (:description v)})})
      true
      (conj {:category :effects
             :tiles (for [[k v] (:effects vocab)
                          :when (and (not= k :jump)
                                     (not (contains? best-of k)))]
                      {:kind :effect :type k :label (:label v)
                       :description (:description v)})})
      (seq best-of)
      (conj {:category :best-of
             :tiles (for [[k v] (:effects vocab)
                          :when (contains? best-of k)]
                      {:kind :effect :type k :label (:label v)
                       :description (:description v)})})
      true
      (conj {:category :flow
             :tiles [{:kind :jump :type :jump :label "jump"
                      :description "Jump to another diagram"}]}))))

;; ── Interpreter ─────────────────────────────────────────────────────────────

(declare eval-tile)

(defn- find-link
  [diagram from-id port]
  (some (fn [l]
          (when (and (= (get-in l [:from :tile]) from-id)
                     (= (get-in l [:from :port]) port))
            l))
        (:links diagram)))

(defn- follow-port
  [vocab game-state choices definition diagram-name from-id port visited]
  (let [diagram (get-in definition [:diagrams diagram-name])
        link    (find-link diagram from-id port)]
    (when link
      (eval-tile vocab game-state choices definition diagram-name
                 (get-in link [:to :tile]) visited))))

(defn- run-diagram
  [vocab game-state choices definition diagram-name visited]
  (when-let [diagram (get-in definition [:diagrams diagram-name])]
    (when-let [start (or (:start-tile diagram)
                         (some (fn [[id t]] (when (= :start (:kind t)) id))
                               (:tiles diagram)))]
      (eval-tile vocab game-state choices definition diagram-name start visited))))

(defn- eval-tile
  [vocab game-state choices definition diagram-name tile-id visited]
  (let [token [diagram-name tile-id]]
    (when-not (contains? visited token)
      (let [visited' (conj visited token)
            tile     (get-in definition [:diagrams diagram-name :tiles tile-id])
            kind     (:kind tile)]
        (case kind
          :start
          (follow-port vocab game-state choices definition diagram-name
                       tile-id :out visited')

          :condition
          (let [spec (get-in vocab [:conditions (:type tile)])
                v    (when spec ((:eval spec) game-state (:params tile)))
                port (if v :true :false)]
            (or (follow-port vocab game-state choices definition diagram-name
                             tile-id port visited')
                (follow-port vocab game-state choices definition diagram-name
                             tile-id (if (= port :true) :false :true) visited')))

          :logic
          (case (:type tile)
            :branch
            (or (follow-port vocab game-state choices definition diagram-name
                             tile-id :a visited')
                (follow-port vocab game-state choices definition diagram-name
                             tile-id :b visited'))
            ;; default: forward through :out
            (follow-port vocab game-state choices definition diagram-name
                         tile-id :out visited'))

          :effect
          (let [spec (get-in vocab [:effects (:type tile)])]
            (when-let [f (:apply spec)]
              (f game-state choices (:params tile))))

          :jump
          (when-let [target (get-in tile [:params :diagram])]
            (run-diagram vocab game-state choices definition target visited'))

          nil)))))

(defn interpret
  "Walk the flowchart from the start diagram and return [choice-key next-state],
   or nil if no path resolves.  `vocab` is {:conditions {...} :effects {...}},
   `game-state` is the opaque state passed to eval/apply fns."
  [vocab definition game-state choices]
  (run-diagram vocab game-state choices definition
               (or (:start-diagram definition) :main) #{}))

(defn agent-step
  "Convenience: interpret with a fallback to first available choice."
  [vocab definition game-state choices]
  (or (interpret vocab definition game-state choices)
      (when (seq choices)
        [(first (keys choices)) (first (vals choices))])))
