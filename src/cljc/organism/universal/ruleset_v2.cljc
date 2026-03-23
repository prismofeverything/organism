(ns organism.universal.ruleset-v2
  "V2 ruleset: compositional game description with action catalog.
   Supports square, hex, torus topologies and programmatic action types."
  (:require
   [clojure.string :as str]))

;; ── Action catalog ───────────────────────────────────────────────────────────────
;; Each action type is described declaratively. The game engine evaluates these
;; generically rather than with hardcoded per-action logic.

(def action-catalog
  {"move"      {:target-owner "empty"  :max-range 1 :can-jump false :effect "relocate"
                :resource-cost 0 :requires-beats false :requires-piece true
                :label "M" :color "#66aaff"}
   "eat"       {:target-owner "empty"  :max-range 1 :can-jump false :effect "collect"
                :resource-cost 0 :requires-beats false :requires-piece true
                :label "E" :color "#66ee66"}
   "grow"      {:target-owner "empty"  :max-range 1 :can-jump false :effect "spawn"
                :resource-cost 1 :requires-beats false :requires-piece true
                :label "G" :color "#ffcc00"}
   "capture"   {:target-owner "enemy"  :max-range 1 :can-jump false :effect "remove_target"
                :resource-cost 0 :requires-beats true  :requires-piece true
                :label "X" :color "#ff4444"}
   "circulate" {:target-owner "self"   :max-range 1 :can-jump false :effect "transfer_resource"
                :resource-cost 1 :requires-beats false :requires-piece true
                :label "C" :color "#cc88ff"}
   "place"     {:target-owner "empty"  :max-range 1 :can-jump false :effect "place"
                :resource-cost 0 :requires-beats false :requires-piece false
                :label "P" :color "#88cccc"}
   "push"      {:target-owner "enemy"  :max-range 1 :can-jump false :effect "push"
                :resource-cost 0 :requires-beats false :requires-piece true
                :label ">" :color "#ff8844"}
   "swap"      {:target-owner "any"    :max-range 1 :can-jump false :effect "swap"
                :resource-cost 0 :requires-beats false :requires-piece true
                :label "S" :color "#44cccc"}
   "convert"   {:target-owner "enemy"  :max-range 1 :can-jump false :effect "convert"
                :resource-cost 2 :requires-beats true  :requires-piece true
                :label "V" :color "#cc44cc"}
   "snipe"     {:target-owner "enemy"  :max-range 2 :can-jump false :effect "remove_target"
                :resource-cost 0 :requires-beats true  :requires-piece true
                :label "!" :color "#ff6666"}
   "leap"      {:target-owner "empty"  :max-range 2 :can-jump true  :effect "relocate"
                :resource-cost 0 :requires-beats false :requires-piece true
                :label "L" :color "#88aaff"}
   "charge"    {:target-owner "enemy"  :max-range 2 :can-jump false :effect "remove_target"
                :resource-cost 0 :requires-beats false :requires-piece true
                :label "^" :color "#ff4466"}})

(defn get-action-type
  "Look up an action type by name."
  [action-name]
  (get action-catalog action-name))

(defn action-types-for
  "Get the list of action type descriptors for a v2 ruleset."
  [ruleset]
  (mapv (fn [name] (assoc (get action-catalog name) :name name))
        (:action-names ruleset)))

;; ── Conflict resolution ──────────────────────────────────────────────────────────

(defn conflict-index
  "Index into the conflict vector for pair (i, j) where i < j."
  [num-types i j]
  (let [a (min i j)
        b (max i j)]
    (loop [idx 0 x 0]
      (if (= x a)
        (+ idx (- b a 1))
        (recur (+ idx (- num-types 1 x)) (inc x))))))

(defn outcome-for
  "Returns :wins :loses :coexist or :mutual for attacker vs defender."
  [num-types conflict attacker defender]
  (if (= attacker defender)
    :coexist
    (let [idx (conflict-index num-types attacker defender)
          raw (get conflict idx 0)]
      (case (int raw)
        0 :coexist
        3 :mutual
        (let [i (min attacker defender)
              winner (if (= raw 1) i (max attacker defender))]
          (if (= winner attacker) :wins :loses))))))

;; ── V2 ruleset helpers ───────────────────────────────────────────────────────────

(defn v2-ruleset?
  "Detect v2 format: has nested :board map."
  [rs]
  (map? (:board rs)))

(defn actions-per-turn
  "How many actions the current player gets per turn."
  [ruleset]
  (let [turns (:turns ruleset)]
    (case (:structure turns)
      "multi"         (get turns :actions-per-turn 1)
      "action_points" (get turns :action-points 1)
      1)))

(defn has-resources?
  "Whether this ruleset uses a resource system."
  [ruleset]
  (not= "none" (get-in ruleset [:resources :system])))

;; ── JSON conversion ──────────────────────────────────────────────────────────────

(defn- snake->kebab
  "Convert snake_case string to kebab-case keyword."
  [s]
  (keyword (str/replace (name s) "_" "-")))

;; ── Discovered v2 games ───────────────────────────────────────────────────────

(def discovered-v2-games
  [{:name "Grid Leaper"
    :richness 98
    :description "5x5 square grid with fog. 4 types, move+leap, capture to win."
    :ruleset
    {:board {:topology "square" :size 5 :symmetry 4}
     :pieces {:num-types 4 :elements-per-player 3}
     :interactions {:conflict [0 1 3 1 1 3] :trigger "move_into"}
     :turns {:structure "action_points" :actions-per-turn 2 :action-points 2}
     :win {:condition "captures" :threshold 2 :turn-limit 50}
     :info {:visibility "fog" :fog-radius 2}
     :resources {:system "energy" :initial-amount 3 :regen-rate 1}
     :num-players 2 :action-names ["move" "leap"]}}

   {:name "Hex Chase"
    :richness 78
    :description "Hex board, 1 type, move+leap, eliminate to win. Near-perfect balance."
    :ruleset
    {:board {:topology "hex" :size 4 :symmetry 4}
     :pieces {:num-types 1 :elements-per-player 3}
     :interactions {:conflict [] :trigger "adjacency"}
     :turns {:structure "action_points" :actions-per-turn 2 :action-points 2}
     :win {:condition "elimination" :threshold 2 :turn-limit 30}
     :info {:visibility "perfect" :fog-radius 3}
     :resources {:system "energy" :initial-amount 3 :regen-rate 1}
     :num-players 2 :action-names ["move" "leap"]}}

   {:name "Torus Arena"
    :richness 73
    :description "Wrapping hex, 3 types with heterarchy, score at time limit."
    :ruleset
    {:board {:topology "torus_hex" :size 3 :symmetry 4}
     :pieces {:num-types 3 :elements-per-player 3}
     :interactions {:conflict [1 1 2] :trigger "explicit"}
     :turns {:structure "action_points" :actions-per-turn 2 :action-points 2}
     :win {:condition "score_at_limit" :threshold 7 :turn-limit 30}
     :info {:visibility "perfect" :fog-radius 2}
     :resources {:system "energy" :initial-amount 3 :regen-rate 1}
     :num-players 2 :action-names ["move" "leap"]}}

   {:name "Torus Clash"
    :richness 72
    :description "Wrapping hex r=4, 3 types, move-into conflicts, elimination."
    :ruleset
    {:board {:topology "torus_hex" :size 4 :symmetry 4}
     :pieces {:num-types 3 :elements-per-player 3}
     :interactions {:conflict [0 1 3] :trigger "move_into"}
     :turns {:structure "action_points" :actions-per-turn 2 :action-points 2}
     :win {:condition "elimination" :threshold 2 :turn-limit 30}
     :info {:visibility "perfect" :fog-radius 3}
     :resources {:system "energy" :initial-amount 3 :regen-rate 1}
     :num-players 2 :action-names ["move" "leap"]}}

   {:name "Hex Swap"
    :richness 60
    :description "Hex board, move+swap+eat. Position swapping creates tactics."
    :ruleset
    {:board {:topology "hex" :size 4 :symmetry 4}
     :pieces {:num-types 1 :elements-per-player 3}
     :interactions {:conflict [] :trigger "move_into"}
     :turns {:structure "single" :actions-per-turn 1 :action-points 1}
     :win {:condition "captures" :threshold 2 :turn-limit 50}
     :info {:visibility "perfect" :fog-radius 3}
     :resources {:system "none" :initial-amount 0 :regen-rate 0}
     :num-players 2 :action-names ["move" "swap" "eat"]}}

   {:name "Square Tactics"
    :richness 55
    :description "5x5 grid, 4 types, perfectly balanced elimination game."
    :ruleset
    {:board {:topology "square" :size 5 :symmetry 4}
     :pieces {:num-types 4 :elements-per-player 3}
     :interactions {:conflict [0 1 3 1 3 3] :trigger "move_into"}
     :turns {:structure "action_points" :actions-per-turn 2 :action-points 2}
     :win {:condition "elimination" :threshold 2 :turn-limit 30}
     :info {:visibility "perfect" :fog-radius 3}
     :resources {:system "energy" :initial-amount 3 :regen-rate 1}
     :num-players 2 :action-names ["move" "leap"]}}])

(defn json->ruleset
  "Convert a JSON-style map (string keys, snake_case) to a Clojure v2 ruleset."
  [m]
  (let [conv (fn [sub-map]
               (into {} (map (fn [[k v]] [(snake->kebab k) v]) sub-map)))]
    {:board        (conv (get m "board" (get m :board {})))
     :pieces       (conv (get m "pieces" (get m :pieces {})))
     :interactions (let [i (conv (get m "interactions" (get m :interactions {})))]
                     (update i :conflict vec))
     :turns        (conv (get m "turns" (get m :turns {})))
     :win          (conv (get m "win" (get m :win {})))
     :info         (conv (get m "info" (get m :info {})))
     :resources    (conv (get m "resources" (get m :resources {})))
     :num-players  (or (get m "num_players") (get m :num-players) 2)
     :action-names (vec (or (get m "action_names") (get m :action-names) ["move"]))}))
