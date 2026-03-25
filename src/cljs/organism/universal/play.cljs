(ns organism.universal.play
  "Universal game frontend: renders any ring board game and handles interaction."
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.websockets :as ws]
   [organism.universal.game :as game]
   [organism.universal.game-v2 :as game-v2]
   [organism.universal.game-v3 :as game-v3]
   [organism.universal.ruleset :as rs]
   [organism.universal.ruleset-v2 :as rs2]
   [organism.universal.topology :as topo]
   [organism.universal.describe :as desc]))

;; ── State atoms ─────────────────────────────────────────────────────────────────

(defonce game-state (r/atom nil))
(defonce ruleset-atom (r/atom nil))
(defonce board-atom (r/atom nil))
(defonce player-key (r/atom nil))
(defonce selected-element (r/atom nil))
(defonce legal-actions-atom (r/atom {}))
;; When multiple actions to same target, show a picker
(defonce action-picker (r/atom nil)) ; {:from space :to space :actions [...]}

(defn- v3-ruleset? [rs] (= "v3" (:version rs)))

(defn- v3-elements
  "Extract elements from v3 state in a format compatible with the renderer.
   Returns {node-id → {:player \"0\" :type 0 :food 0}}."
  [state]
  (when-let [nodes (:nodes state)]
    (reduce-kv
     (fn [m node nd]
       (if (and nd (>= (get nd :owner -1) 0))
         (assoc m node {:player (str (:owner nd))
                        :type (get nd :piece_type 0)
                        :food (get nd "resource_0" (get nd :resource_0 0))})
         m))
     {} nodes)))

(defn- v3-free-food
  "Extract free resources from v3 state (resources on unoccupied nodes)."
  [state]
  (when-let [nodes (:nodes state)]
    (reduce-kv
     (fn [m node nd]
       (if (and nd (neg? (get nd :owner -1)))
         (let [food (get nd "resource_0" (get nd :resource_0 0))]
           (if (pos? food) (assoc m node food) m))
         m))
     {} nodes)))

(defn- get-current-player [state ruleset]
  (if (and ruleset (v3-ruleset? ruleset))
    (str (get-in state [:globals "current_player"] 0))
    (:current-player state)))
(defonce history-atom (r/atom []))  ; [{:step :player :action :round} ...]
(defonce history-selected (r/atom nil))

;; ── Player colors ───────────────────────────────────────────────────────────────

(def player-colors
  ["#4488ff" "#ff4444" "#44cc44" "#ff9900" "#cc44cc"
   "#44cccc" "#ffcc00" "#ff66aa"])

(def player-glow-colors
  ["#66aaff" "#ff6666" "#66ee66" "#ffbb22" "#ee66ee"
   "#66eeee" "#ffee22" "#ff88cc"])

(def action-type-colors
  {;; V1 actions (keyword keys for v1 compat)
   :move      "#66aaff"  :eat       "#66ee66"  :grow      "#ffcc00"
   :capture   "#ff4444"  :circulate "#cc88ff"  :pass      "#666666"
   ;; V2 actions (string keys)
   "move" "#66aaff"  "eat" "#66ee66"  "grow" "#ffcc00"  "capture" "#ff4444"
   "circulate" "#cc88ff"  "place" "#88cccc"  "push" "#ff8844"
   "swap" "#44cccc"  "convert" "#cc44cc"  "snipe" "#ff6666"
   "leap" "#88aaff"  "charge" "#ff4466"  "pass" "#666666"})

(defn player-color [player]
  (let [idx (js/parseInt player 10)]
    (nth player-colors (mod idx (count player-colors)))))

(defn player-glow [player]
  (let [idx (js/parseInt player 10)]
    (nth player-glow-colors (mod idx (count player-glow-colors)))))

;; ── Board geometry ────────────────────────────────────────────────────────────────

(def board-cx 400)
(def board-cy 400)
(def ring-spacing 55)
(def space-radius 18)

;; V1 ring board coordinate mapping
(defn ring-space->pixel
  [rings space]
  (let [[color step] space
        ring-names (mapv first rings)
        level (.indexOf ring-names color)]
    (if (zero? level)
      [board-cx board-cy]
      (let [ring-data (nth rings level)
            ring-spaces (second ring-data)
            total (count ring-spaces)
            angle (- (* step (/ (* 2 js/Math.PI) total)) (/ js/Math.PI 2))
            rad (* level ring-spacing)]
        [(+ board-cx (* rad (js/Math.cos angle)))
         (+ board-cy (* rad (js/Math.sin angle)))]))))

;; V2 topology coordinate mapping
(defn- square-node->pixel
  [coords grid-size node]
  (let [[row col] (get coords node [0 0])
        spacing (/ 700 (max grid-size 1))
        margin 50]
    [(+ margin (* col spacing) (/ spacing 2))
     (+ margin (* row spacing) (/ spacing 2))]))

(defn- hex-node->pixel
  [coords grid-size node]
  (let [[row col] (get coords node [0 0])
        hex-size 38
        ;; Offset hex layout
        x (* hex-size 1.5 col)
        y (* hex-size (js/Math.sqrt 3) (+ row (* 0.5 (mod col 2))))
        ;; Center in viewport
        cx (+ 80 x)
        cy (+ 80 y)]
    [cx cy]))

(defn compute-locations
  "Compute pixel positions for all board spaces. Dispatches on topology type."
  [topology-type coords grid-size rings adjacencies]
  (if (and coords (seq coords))
    ;; Coord-based rendering (v2/v3 — all topologies including radial)
    (let [pixel-fn (case topology-type
                     ("hex" "torus_hex" "radial" "ring" "triangle")
                     (partial hex-node->pixel coords grid-size)
                     ;; default: square layout
                     (partial square-node->pixel coords grid-size))]
      (reduce (fn [m node] (assoc m node (pixel-fn node)))
              {} (keys coords)))
    ;; V1 ring topology (no coords — uses [color step] tuples)
    (reduce (fn [m space] (assoc m space (ring-space->pixel rings space)))
            {} (keys adjacencies))))

;; ── Action analysis ─────────────────────────────────────────────────────────────

(defn index-actions
  "Pre-index actions for fast lookup. Returns:
   {:from-spaces #{spaces that can act}
    :by-from {from-space → [action-entries...]}
    :by-target {to-space → [action-entries...]}
    :by-from-to {[from to] → [action-entries...]}
    :all-targets #{all target spaces}}"
  [actions]
  (let [entries (vec actions)] ;; [[key next-state] ...]
    {:from-spaces (set (map (comp second first) entries))
     :by-from (group-by (comp second first) entries)
     :by-target (group-by (comp #(nth % 2) first) entries)
     :by-from-to (group-by (fn [[k _]] [(second k) (nth k 2)]) entries)
     :all-targets (set (map (comp #(nth % 2) first) entries))}))

(defn action-types-at
  "Distinct action types available from a source to a target."
  [indexed from-sp to-sp]
  (let [entries (get-in indexed [:by-from-to [from-sp to-sp]])]
    (distinct (map (comp first first) entries))))

;; ── SVG rendering ───────────────────────────────────────────────────────────────

(defn render-type-shape
  [x y radius etype color stroke-color]
  (let [shapes [:star5 :diamond :triangle :square]
        shape (nth shapes (mod etype (count shapes)))]
    (case shape
      :star5
      (let [points (str/join " "
                     (for [i (range 10)
                           :let [r (if (even? i) radius (* radius 0.5))
                                 a (- (* i (/ js/Math.PI 5)) (/ js/Math.PI 2))]]
                       (str (+ x (* r (js/Math.cos a)))
                            "," (+ y (* r (js/Math.sin a))))))]
        [:polygon {:points points :fill color :stroke stroke-color :stroke-width 1.5}])

      :diamond
      (let [r (* radius 0.9)
            points (str x "," (- y r) " " (+ x r) "," y " " x "," (+ y r) " " (- x r) "," y)]
        [:polygon {:points points :fill color :stroke stroke-color :stroke-width 1.5}])

      :triangle
      (let [r radius
            points (str x "," (- y r) " "
                        (+ x (* r 0.87)) "," (+ y (* r 0.5)) " "
                        (- x (* r 0.87)) "," (+ y (* r 0.5)))]
        [:polygon {:points points :fill color :stroke stroke-color :stroke-width 1.5}])

      :square
      (let [r (* radius 0.75)]
        [:rect {:x (- x r) :y (- y r) :width (* 2 r) :height (* 2 r)
                :fill color :stroke stroke-color :stroke-width 1.5}]))))

(defn render-glow
  "Animated glow ring around a space."
  [x y radius color]
  [:circle {:cx x :cy y :r (+ radius 4)
            :fill "none" :stroke color :stroke-width 2.5
            :opacity 0.7
            :style {:animation "pulse 1.2s ease-in-out infinite"}}])

(defn render-action-pip
  "Small colored pip indicating an available action type, offset around the space."
  [x y idx total action-type]
  (let [pip-r 5
        orbit (+ space-radius 10)
        angle (+ (* (/ (* 2 js/Math.PI) (max total 1)) idx) (/ js/Math.PI -2))
        px (+ x (* orbit (js/Math.cos angle)))
        py (+ y (* orbit (js/Math.sin angle)))
        color (get action-type-colors action-type "#888")]
    [:circle {:cx px :cy py :r pip-r
              :fill color :stroke "#111" :stroke-width 1
              :opacity 0.9}]))

(defn render-action-label
  "Text label for an action type near a target space."
  [x y idx total action-type]
  (let [orbit (+ space-radius 18)
        angle (+ (* (/ (* 2 js/Math.PI) (max total 1)) idx) (/ js/Math.PI -2))
        lx (+ x (* orbit (js/Math.cos angle)))
        ly (+ y (* orbit (js/Math.sin angle)))
        color (get action-type-colors action-type "#aaa")
        label (case action-type
                :move "M" :eat "E" :grow "G" :capture "X" :circulate "C" "?")]
    [:g
     [:circle {:cx lx :cy ly :r 8
               :fill color :stroke "#111" :stroke-width 1 :opacity 0.85
               :cursor "pointer"}]
     [:text {:x lx :y (+ ly 4)
             :text-anchor "middle" :font-size "9" :font-weight "bold"
             :fill "#111" :pointer-events "none"}
      label]]))

(defn render-adjacency-lines
  [adjacencies locations]
  [:g {:opacity 0.12}
   (for [[space neighbors] adjacencies
         neighbor neighbors
         :when (< (compare (pr-str space) (pr-str neighbor)) 0)
         :let [[x1 y1] (get locations space)
               [x2 y2] (get locations neighbor)]
         :when (and x1 x2)]
     ^{:key (str (pr-str space) "-" (pr-str neighbor))}
     [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
             :stroke "#444" :stroke-width 1}])])

(defn render-element-shape
  [space el [x y] is-mine? glow?]
  (let [color (player-color (:player el))
        dark (if glow? "#fff" "#222")
        food (:food el 0)]
    ^{:key (str "el-" (pr-str space))}
    [:g {:cursor (when is-mine? "pointer")}
     ;; Glow ring for actionable pieces
     (when glow?
       (render-glow x y (* space-radius 0.8) (player-glow (:player el))))
     ;; Element shape
     (render-type-shape x y (* space-radius 0.75) (:type el) color dark)
     ;; Food dots
     (when (pos? food)
       [:g
        (for [i (range (min food 5))
              :let [a (* i (/ (* 2 js/Math.PI) (max (min food 5) 1)))
                    fx (+ x (* (+ space-radius 5) (js/Math.cos a)))
                    fy (+ y (* (+ space-radius 5) (js/Math.sin a)))]]
          ^{:key (str "food-" i)}
          [:circle {:cx fx :cy fy :r 3
                    :fill "#ffe066" :stroke "#aa8800" :stroke-width 0.5}])])]))

;; ── Interaction ─────────────────────────────────────────────────────────────────

(defn send-action! [action-key]
  (ws/send-transit-message!
   {"type" "action"
    "choice" (pr-str action-key)}))

(defn handle-click! [space]
  (let [state @game-state
        actions @legal-actions-atom
        sel @selected-element
        player @player-key
        picker @action-picker]

    ;; If picker is open and we clicked a label, it's handled by the label click
    ;; Reset picker on any board click
    (reset! action-picker nil)

    (cond
      ;; Selected element + clicked target → execute
      (and sel (not= sel space))
      (let [indexed (index-actions actions)
            matching (get-in indexed [:by-from-to [sel space]])]
        (cond
          ;; Single action → execute
          (= 1 (count matching))
          (do (send-action! (first (first matching)))
              (reset! selected-element nil))

          ;; Multiple → show picker
          (seq matching)
          (reset! action-picker {:from sel :to space :actions matching})

          ;; Not a valid target → try selecting
          :else
          (let [el (get-in state [:elements space])]
            (if (and el (= (:player el) player))
              (reset! selected-element space)
              (reset! selected-element nil)))))

      ;; Clicked own element → select
      (let [el (get-in state [:elements space])]
        (and el (= (:player el) player)
             (seq (filter (fn [[k _]] (= (second k) space)) actions))))
      (reset! selected-element space)

      ;; Deselect
      :else
      (reset! selected-element nil))))

(defn handle-action-label-click! [action-entry]
  (send-action! (first action-entry))
  (reset! selected-element nil)
  (reset! action-picker nil))

;; ── Game board component ────────────────────────────────────────────────────────

(defn game-board []
  (let [state @game-state
        ruleset @ruleset-atom
        board @board-atom]
    (when (and state ruleset board)
      (let [non-v1? (or (rs2/v2-ruleset? ruleset) (v3-ruleset? ruleset))
            adjacencies (:adjacencies board)
            topology-type (cond
                            (v3-ruleset? ruleset) (get-in ruleset [:schema :topology_type] "hex")
                            (rs2/v2-ruleset? ruleset) (get-in ruleset [:board :topology])
                            :else "ring")
            coords (when non-v1? (:coords board))
            grid-size (when non-v1? (:grid-size board))
            rings (when-not non-v1? (:rings board))
            locations (compute-locations topology-type coords grid-size rings adjacencies)
            current-p (if (v3-ruleset? ruleset)
                        (str (get-in state [:globals "current_player"] 0))
                        (:current-player state))
            my-turn? (= current-p @player-key)
            actions (when my-turn? @legal-actions-atom)
            indexed (when (seq actions) (index-actions actions))
            sel @selected-element
            sel-targets (when (and sel indexed)
                          (set (map (comp #(nth % 2) first)
                                    (get (:by-from indexed) sel))))
            picker @action-picker
            view-size (+ (* 2 board-cx) 50)]
        [:svg {:width "100%" :height "100%"
               :viewBox (str "0 0 " view-size " " view-size)
               :style {:background "#111" :max-height "100vh"}}

         ;; CSS animation for glow pulse
         [:defs
          [:style
           "@keyframes pulse { 0%,100% { opacity: 0.4; } 50% { opacity: 1.0; } }"]]

         ;; Adjacency lines
         [render-adjacency-lines adjacencies locations]

         ;; Board spaces
         (for [[space [x y]] locations
               :let [is-sel? (= space sel)
                     is-target? (and sel-targets (contains? sel-targets space))
                     can-act? (and indexed (contains? (:from-spaces indexed) space))
                     ;; Highlight targets even without selection — show all reachable
                     is-any-target? (and indexed (not sel)
                                        (contains? (:all-targets indexed) space))]]
           ^{:key (str "sp-" (pr-str space))}
           [:circle
            {:cx x :cy y :r space-radius
             :fill (cond is-sel? "#444"
                         is-target? "#2a3a2a"
                         :else "#1a1a2e")
             :stroke (cond is-sel? "#fff"
                           is-target? (get action-type-colors :move "#6a6")
                           can-act? "#556"
                           is-any-target? "#333"
                           :else "#282838")
             :stroke-width (cond is-sel? 2.5
                                 is-target? 2
                                 can-act? 1.5
                                 :else 0.8)
             :cursor (when (or is-target? can-act?) "pointer")
             :on-click #(handle-click! space)}])

         ;; Target highlight rings (when element selected)
         (when sel-targets
           (for [target sel-targets
                 :let [[tx ty] (get locations target)]
                 :when (and tx (not= target sel))]
             (let [types (when indexed (action-types-at indexed sel target))
                   n (count types)]
               ^{:key (str "tgt-" (pr-str target))}
               [:g
                ;; Glow ring on target
                [:circle {:cx tx :cy ty :r (+ space-radius 3)
                          :fill "none" :stroke "#6a6" :stroke-width 2
                          :opacity 0.6
                          :stroke-dasharray "4 3"}]
                ;; Action type labels around target
                (for [[i atype] (map-indexed vector types)]
                  ^{:key (str "al-" i "-" (name atype))}
                  [render-action-label tx ty i n atype])])))

         ;; Free food
         (let [free-food (if (v3-ruleset? ruleset) (v3-free-food state) (:free-food state))]
           (for [[space amount] free-food
                 :when (and amount (pos? amount))
                 :let [[x y] (get locations space)]
                 :when x]
             ^{:key (str "ff-" (pr-str space))}
             [:g
              [:circle {:cx x :cy (+ y 8) :r 5
                        :fill "#ffe066" :opacity 0.6}]
              (when (> amount 1)
                [:text {:x (+ x 7) :y (+ y 12) :font-size "8" :fill "#aa8" :opacity 0.8}
                 (str amount)])]))

         ;; Elements
         (let [elements (if (v3-ruleset? ruleset) (v3-elements state) (:elements state))]
           (for [[space el] elements
                 :let [[x y] (get locations space)
                       is-mine? (= (:player el) @player-key)
                       can-act? (and indexed (contains? (:from-spaces indexed) space))
                       is-sel? (= space sel)]
                 :when x]
           ^{:key (str "el-" (pr-str space))}
           [:g {:on-click #(handle-click! space)
                :cursor (when can-act? "pointer")}
            [render-element-shape space el [x y] is-mine? (and is-mine? can-act?)]
            ;; Selected indicator
            (when is-sel?
              [:circle {:cx x :cy y :r (+ space-radius 1)
                        :fill "none" :stroke "#fff" :stroke-width 2.5}])]))

         ;; Unselected: show small pips on all actionable pieces showing what they can do
         (when (and indexed (not sel))
           (for [from-sp (:from-spaces indexed)
                 :let [[fx fy] (get locations from-sp)
                       from-actions (get (:by-from indexed) from-sp)
                       types (distinct (map (comp first first) from-actions))
                       types (remove #{:pass} types)
                       n (count types)]
                 :when (and fx (pos? n))]
             ^{:key (str "pips-" (pr-str from-sp))}
             [:g
              (for [[i atype] (map-indexed vector types)]
                ^{:key (str "pip-" i)}
                [render-action-pip fx fy i n atype])]))

         ;; Action picker overlay (when multiple actions to same target)
         (when picker
           (let [{:keys [to actions]} picker
                 [tx ty] (get locations to)
                 grouped (group-by (comp first first) actions)]
             (when tx
               [:g
                ;; Backdrop
                [:circle {:cx tx :cy ty :r 35
                          :fill "rgba(0,0,0,0.8)" :stroke "#fff" :stroke-width 1}]
                ;; One button per action type
                (for [[i [atype entries]] (map-indexed vector grouped)
                      :let [n (count grouped)
                            angle (+ (* (/ (* 2 js/Math.PI) (max n 1)) i) (/ js/Math.PI -2))
                            bx (+ tx (* 22 (js/Math.cos angle)))
                            by (+ ty (* 22 (js/Math.sin angle)))
                            color (get action-type-colors atype "#888")
                            label (case atype
                                    :move "M" :eat "E" :grow "G"
                                    :capture "X" :circulate "C" "?")]]
                  ^{:key (str "pick-" (name atype))}
                  [:g {:cursor "pointer"
                       :on-click (fn [e]
                                   (.stopPropagation e)
                                   (handle-action-label-click! (first entries)))}
                   [:circle {:cx bx :cy by :r 11
                             :fill color :stroke "#fff" :stroke-width 1.5}]
                   [:text {:x bx :y (+ by 4)
                           :text-anchor "middle" :font-size "11" :font-weight "bold"
                           :fill "#111" :pointer-events "none"}
                    label]])])))]))))

;; ── Rules panel (left) ───────────────────────────────────────────────────────────

(defn conflict-explanation
  "Human-readable conflict table."
  [ruleset]
  (let [v2? (or (rs2/v2-ruleset? ruleset) (v3-ruleset? ruleset))
        n (cond (v3-ruleset? ruleset) (get-in ruleset [:schema :num_piece_types])
                v2? (get-in ruleset [:pieces :num-types])
                :else (:num-types ruleset))
        conflict (cond (v3-ruleset? ruleset) (vec (:conflict_table ruleset))
                       v2? (get-in ruleset [:interactions :conflict])
                       :else nil)
        type-name (fn [t] (str "Type " t))]
    (when (and n (> n 1))
      [:div {:style {:margin-top "8px"}}
       [:div {:style {:color "#888" :font-size "10px" :margin-bottom "4px"
                      :text-transform "uppercase" :letter-spacing "1px"}}
        "Conflict resolution"]
       (for [i (range n)
             j (range (inc i) n)
             :let [outcome (if v2?
                             (rs2/outcome-for n conflict i j)
                             (rs/outcome-for ruleset i j))]]
         ^{:key (str i "-" j)}
         [:div {:style {:font-size "11px" :padding "1px 0"}}
          [:span {:style {:color (get action-type-colors "capture" "#aaa")}}
           (case outcome
             :wins    (str (type-name i) " beats " (type-name j))
             :loses   (str (type-name j) " beats " (type-name i))
             :coexist (str (type-name i) " & " (type-name j) " coexist")
             :mutual  (str (type-name i) " & " (type-name j) " mutual destruction")
             (str i " vs " j ": " (pr-str outcome)))]])])))

(defn rules-panel []
  (let [ruleset @ruleset-atom
        state @game-state]
    (when ruleset
      (let [current (when state (get-current-player state ruleset))
            winner (when state (:winner state))
            my-turn? (= (str current) @player-key)]
        [:div {:style {:width "240px" :background "#0a0a14"
                       :border-right "1px solid #1a1a2e"
                       :overflow-y "auto" :padding "12px"
                       :color "#ccc" :font-family "monospace" :font-size "12px"
                       :flex-shrink "0"}}

         ;; Game title
         [:div {:style {:font-size "14px" :color "#fff" :margin-bottom "10px"
                        :border-bottom "1px solid #222" :padding-bottom "8px"}}
          (cond
            (v3-ruleset? ruleset)
            (str (get-in ruleset [:schema :topology_type]) " "
                 (get-in ruleset [:schema :topology_size]) ", "
                 (get-in ruleset [:schema :num_piece_types]) " types")
            (rs2/v2-ruleset? ruleset)
            (str (get-in ruleset [:board :topology]) " "
                 (get-in ruleset [:board :size]) ", "
                 (get-in ruleset [:pieces :num-types]) " types")
            :else
            (str (:board-symmetry ruleset) "-fold, "
                 (:num-rings ruleset) " rings, "
                 (:num-types ruleset) " types"))]

         ;; Turn status
         (when state
           [:div {:style {:margin-bottom "10px" :padding "6px 8px" :border-radius "4px"
                          :background (if my-turn? "#1a2a1a" "#1a1a2a")
                          :border (str "1px solid " (if my-turn? "#2a4a2a" "#222"))}}
            (when winner
              [:div {:style {:color "#ffe066" :font-size "16px" :text-align "center"
                             :margin-bottom "4px"}}
               (str "Player " winner " wins!")])
            [:div {:style {:color (if my-turn? "#66ff66" "#888")}}
             (str "Round " (:round state) " — "
                  (if my-turn? "YOUR TURN" (str "P" current "'s turn")))]])

         ;; Players
         (when state
           [:div {:style {:margin-bottom "10px"}}
            [:div {:style {:color "#666" :font-size "10px" :margin-bottom "4px"
                           :text-transform "uppercase" :letter-spacing "1px"}}
             "Players"]
            (let [players (if (v3-ruleset? ruleset)
                           (mapv str (range (get-in ruleset [:schema :num_players] 2)))
                           (or (:turn-order state) (:turn_order state) ["0" "1"]))]
              (for [p players]
                (let [is-me? (= (str p) @player-key)
                      is-current? (= (str p) (str current))
                      elements (if (v3-ruleset? ruleset) (v3-elements state) (:elements state))
                      pop (count (filter #(= (str p) (str (:player %))) (vals elements)))
                      caps (if (v3-ruleset? ruleset)
                             (get-in state [:globals (str "counter_" p)] 0)
                             (get-in state [:captures p] 0))]
                ^{:key p}
                [:div {:style {:color (player-color p) :padding "2px 0"
                               :font-weight (if is-current? "bold" "normal")
                               :opacity (if is-current? 1.0 0.6)}}
                 (str (if is-me? "▸ YOU" (str "  P" p))
                      "  " pop " units  " caps " captures"
                      (when (and (not (v3-ruleset? ruleset))
                                  (= (:win-type ruleset) :captures))
                        (str "/" (:win-threshold ruleset))))])))])

         ;; Rules explanation
         (let [v2? (or (rs2/v2-ruleset? ruleset) (v3-ruleset? ruleset))]
           [:div {:style {:border-top "1px solid #222" :padding-top "8px" :margin-top "4px"}}
            [:div {:style {:color "#666" :font-size "10px" :margin-bottom "6px"
                           :text-transform "uppercase" :letter-spacing "1px"}}
             "Rules"]

            [:div {:style {:margin-bottom "6px" :color "#aaa" :font-size "11px"}}
             (cond
               (v3-ruleset? ruleset)
               (str "Board: " (get-in ruleset [:schema :topology_type]) " size "
                    (get-in ruleset [:schema :topology_size]))
               (rs2/v2-ruleset? ruleset)
               (str "Board: " (get-in ruleset [:board :topology]) " size "
                    (get-in ruleset [:board :size]))
               :else
               (str "Board: " (:board-symmetry ruleset) "-fold symmetry with "
                    (:num-rings ruleset) " rings"))]

            [:div {:style {:margin-bottom "6px" :color "#aaa" :font-size "11px"}}
             (let [nt (cond (v3-ruleset? ruleset) (get-in ruleset [:schema :num_piece_types])
                            v2? (get-in ruleset [:pieces :num-types])
                            :else (:num-types ruleset))]
               (str nt " piece types, " (get-in ruleset [:schema :num_players]
                                                (or (:num-players ruleset) 2)) " players"))]

            [:div {:style {:margin-bottom "6px" :color "#aaa" :font-size "11px"}}
             (let [wc (cond (v3-ruleset? ruleset) "turn limit"
                            v2? (get-in ruleset [:win :condition])
                            :else (some-> (:win-type ruleset) name))
                   wt (cond (v3-ruleset? ruleset) (:turn_limit ruleset)
                            v2? (get-in ruleset [:win :threshold])
                            :else (:win-threshold ruleset))
                   tl (cond (v3-ruleset? ruleset) (:turn_limit ruleset)
                            v2? (get-in ruleset [:win :turn-limit])
                            :else nil)]
               (str "Win by: "
                    (case wc
                      "captures"       (str "capturing " wt " enemy pieces")
                      "population"     (str "growing to " wt " pieces")
                      "elimination"    "eliminating all opponents"
                      "territory"      (str "controlling " wt "% of board")
                      "score_at_limit" (str "highest score at round " tl)
                      wc)
                    (when tl (str " (limit: " tl " rounds)"))))]

            ;; Turns info
            (when v2?
              (let [ts (get-in ruleset [:turns :structure])]
                (when (not= ts "single")
                  [:div {:style {:margin-bottom "6px" :color "#aaa" :font-size "11px"}}
                   (str "Turns: " (rs2/actions-per-turn ruleset) " actions per turn")])))

            ;; Available actions
            [:div {:style {:margin-top "8px"}}
             [:div {:style {:color "#666" :font-size "10px" :margin-bottom "4px"
                            :text-transform "uppercase" :letter-spacing "1px"}}
              "Actions"]
             (let [action-names (if v2?
                                  (:action-names ruleset)
                                  (cond-> []
                                    (:can-move ruleset) (conj "move")
                                    (:can-eat ruleset) (conj "eat")
                                    (:can-grow ruleset) (conj "grow")
                                    (:can-capture ruleset) (conj "capture")
                                    (:can-circulate ruleset) (conj "circulate")))]
               (for [aname action-names
                     :let [at (rs2/get-action-type aname)
                           label (or (:label at) (str (first aname)))
                           color (or (:color at) "#888")]]
                 ^{:key aname}
                 [:div {:style {:font-size "11px" :padding "2px 0"}}
                  [:span {:style {:color color}} (str label " ")]
                  [:span {:style {:color "#999"}} aname]]))]

            ;; Conflict table
            [conflict-explanation ruleset]

            ;; Resource info
            (let [res-sys (if v2? (get-in ruleset [:resources :system]) (when (:food-enabled ruleset) "food"))]
              (when (and res-sys (not= res-sys "none"))
                [:div {:style {:margin-top "8px" :color "#999" :font-size "11px"}}
                 (str (str/capitalize res-sys) " system"
                      (when v2?
                        (str " (start: " (get-in ruleset [:resources :initial-amount] 0)
                             (when (= res-sys "energy")
                               (str ", regen: " (get-in ruleset [:resources :regen-rate] 0) "/round"))
                             ")")))]))])

         ;; Pass button
         (when (and state my-turn? (not winner))
           [:button {:style {:margin-top "12px" :padding "6px 16px" :width "100%"
                             :background "#222" :color "#ccc" :border "1px solid #444"
                             :cursor "pointer" :border-radius "4px" :font-family "monospace"}
                     :on-click #(send-action! (if (or (rs2/v2-ruleset? ruleset)
                                                        (v3-ruleset? ruleset))
                                                ["pass" nil nil]
                                                [:pass nil nil]))}
            "Pass turn"])]))))

;; ── History panel (right) ────────────────────────────────────────────────────────

(defn format-action
  "Human-readable description of an action."
  [[action-type from-sp to-sp & args]]
  (let [fmt-sp (fn [sp] (cond (nil? sp) "—"
                               (number? sp) (str "node " sp)
                               :else (str (first sp) ":" (second sp))))]
    (cond
      ;; V3: action-type is an integer (rule slot index) or "pass"
      (number? action-type)
      (str "Rule " action-type ": " (fmt-sp from-sp) " → " (fmt-sp to-sp))
      (= action-type "pass")
      "Pass"
      ;; V2: action-type is a string
      (string? action-type)
      (str action-type " " (fmt-sp from-sp) " → " (fmt-sp to-sp))
      ;; V1: action-type is a keyword
      :else
      (case action-type
        :move      (str "Move " (fmt-sp from-sp) " → " (fmt-sp to-sp))
        :eat       (str "Eat at " (fmt-sp to-sp))
        :grow      (str "Grow type " (first args) " at " (fmt-sp to-sp))
        :capture   (str "Capture at " (fmt-sp to-sp))
      :circulate (str "Feed " (fmt-sp from-sp) " → " (fmt-sp to-sp))
      :pass      "Pass"
      (str action-type)))))


(defn history-panel []
  (let [entries @history-atom]
    [:div {:style {:width "220px" :background "#0a0a14"
                   :border-left "1px solid #1a1a2e"
                   :display "flex" :flex-direction "column"
                   :flex-shrink "0"}}

     ;; Header
     [:div {:style {:padding "8px 12px" :border-bottom "1px solid #1a1a2e"
                    :color "#666" :font-family "monospace" :font-size "10px"
                    :text-transform "uppercase" :letter-spacing "1.5px"}}
      (str "History (" (count entries) " moves)")]

     ;; Scrollable list (newest on top)
     [:div {:style {:flex "1" :overflow-y "auto" :padding "4px 0"
                    :font-family "monospace" :font-size "11px"}}
      (for [i (range (dec (count entries)) -1 -1)
            :let [{:keys [player action]} (nth entries i)
                  is-me? (= player @player-key)
                  color (player-color player)]]
        ^{:key i}
        [:div {:style {:padding "3px 10px" :border-left (str "3px solid " color)
                       :margin-bottom "1px"
                       :background (if is-me? "rgba(68,136,255,0.08)" "transparent")}}
         [:span {:style {:color color :font-size "11px"}}
          (str (if is-me? "You" (str "P" player)) ": ")]
         [:span {:style {:color "#999" :font-size "11px"}}
          (format-action action)]])]

     ;; Empty state
     (when (empty? entries)
       [:div {:style {:padding "20px" :color "#444" :font-family "monospace"
                      :font-size "11px" :text-align "center"}}
        "No moves yet"])]))

;; ── Create game view ────────────────────────────────────────────────────────────

(defonce create-ruleset (r/atom (:ruleset (first rs2/discovered-v2-games))))

(defn game-card
  "A clickable card for a discovered game."
  [{:keys [richness description ruleset] game-name :name} selected?]
  [:div {:style {:padding "10px 14px" :margin-bottom "6px" :cursor "pointer"
                 :border-radius "6px"
                 :background (if selected? "#1a2a3a" "#111118")
                 :border (str "1px solid " (if selected? "#4488ff" "#222"))
                 :transition "all 0.15s"}
         :on-click #(reset! create-ruleset ruleset)}
   [:div {:style {:display "flex" :justify-content "space-between" :align-items "baseline"}}
    [:span {:style {:color (if selected? "#fff" "#ccc") :font-size "14px"
                    :font-weight (if selected? "bold" "normal")}}
     game-name]
    [:span {:style {:color "#666" :font-size "11px"}}
     (str "score " richness)]]
   [:div {:style {:color "#777" :font-size "11px" :margin-top "3px"}}
    description]
   [:div {:style {:color "#555" :font-size "10px" :margin-top "3px"}}
    (cond
      (v3-ruleset? ruleset)
      (let [s (:schema ruleset)]
        (str (or (:topology_type s) "?") " "
             (or (:topology_size s) "") "  "
             (:num_piece_types s) "t  "
             (:num_players s) "p  "
             (count (:rules ruleset)) " rules  "
             (or (:turn_limit ruleset) "") " limit"))
      (rs2/v2-ruleset? ruleset)
      (str (get-in ruleset [:board :topology]) " "
           (get-in ruleset [:board :size]) "  "
           (get-in ruleset [:pieces :num-types]) "t  "
           (:num-players ruleset) "p  "
           (str/join "+" (:action-names ruleset)) "  "
           (get-in ruleset [:win :condition]) ">" (get-in ruleset [:win :threshold]))
      :else
      (str (:board-symmetry ruleset) "-fold  "
           (:num-rings ruleset) "r  "
           (:num-types ruleset) "t  "
           (:num-players ruleset) "p  "
           (some-> (:win-type ruleset) name) ">" (:win-threshold ruleset)))]])

(defn create-view []
  (let [rs @create-ruleset
        game-id (str (random-uuid))]
    [:div {:style {:color "#ccc" :font-family "monospace" :padding "30px 40px"
                   :max-width "700px" :margin "0 auto"}}

     [:h1 {:style {:color "#fff" :margin-bottom "4px"}} "Universal Game Player"]
     [:div {:style {:color "#666" :font-size "12px" :margin-bottom "20px"}}
      "Games discovered by evolutionary search + AlphaZero depth evaluation"]

     ;; V3 games (rules as programs — Go, Organism)
     [:div {:style {:margin-bottom "16px"}}
      [:div {:style {:color "#cc88ff" :font-size "10px" :margin-bottom "8px"
                     :text-transform "uppercase" :letter-spacing "1.5px"}}
       (str (count rs2/discovered-v3-games) " canonical games (rules as programs)")]
      (for [{:keys [ruleset] game-name :name :as game} rs2/discovered-v3-games]
        ^{:key (str "v3-" game-name)}
        [game-card game (= ruleset rs)])]

     ;; V2 discovered games (new topologies)
     [:div {:style {:margin-bottom "16px"}}
      [:div {:style {:color "#88aaff" :font-size "10px" :margin-bottom "8px"
                     :text-transform "uppercase" :letter-spacing "1.5px"}}
       (str (count rs2/discovered-v2-games) " new-topology games (square, hex, torus)")]
      (for [{:keys [ruleset] game-name :name :as game} rs2/discovered-v2-games]
        ^{:key (str "v2-" game-name)}
        [game-card game (= ruleset rs)])]

     ;; V1 discovered games (ring boards)
     [:div {:style {:margin-bottom "16px"}}
      [:div {:style {:color "#888" :font-size "10px" :margin-bottom "8px"
                     :text-transform "uppercase" :letter-spacing "1.5px"}}
       (str (count rs/discovered-games) " ring-board games")]
      (for [{:keys [name ruleset] :as game} rs/discovered-games]
        ^{:key name}
        [game-card game (= ruleset rs)])]

     ;; Classic presets
     [:div {:style {:margin "16px 0 8px" :color "#888" :font-size "10px"
                    :text-transform "uppercase" :letter-spacing "1.5px"}}
      "Hand-designed presets"]
     [:div {:style {:display "flex" :gap "8px" :margin-bottom "16px"}}
      [:button {:style {:padding "6px 14px"
                        :background (if (= rs rs/heterarchy-minimal) "#1a2a3a" "#111")
                        :color "#ccc" :border "1px solid #333"
                        :cursor "pointer" :border-radius "4px" :font-family "monospace"}
                :on-click #(reset! create-ruleset rs/heterarchy-minimal)}
       "Heterarchy Minimal"]
      [:button {:style {:padding "6px 14px"
                        :background (if (= rs rs/organism-like) "#1a2a3a" "#111")
                        :color "#ccc" :border "1px solid #333"
                        :cursor "pointer" :border-radius "4px" :font-family "monospace"}
                :on-click #(reset! create-ruleset rs/organism-like)}
       "Organism-like"]]

     ;; Selected ruleset detail
     (when rs
       [:div {:style {:background "#0a0a14" :border "1px solid #222"
                      :border-radius "6px" :padding "12px" :margin-bottom "16px"}}
        [:div {:style {:color "#666" :font-size "10px" :margin-bottom "6px"
                       :text-transform "uppercase" :letter-spacing "1px"}}
         "Selected ruleset"]
        [:div {:style {:display "flex" :flex-wrap "wrap" :gap "4px 16px"}}
         (for [[k v] (sort-by key rs)]
           ^{:key k}
           [:div {:style {:font-size "11px"}}
            [:span {:style {:color "#666"}} (clojure.core/name k) " "]
            [:span {:style {:color "#ddd"}} (pr-str v)]])]])

     ;; Play button
     [:a {:href (str "/universal/play/" game-id)
          :on-click (fn [_]
                      ;; Store selected ruleset so play page can use it
                      (.setItem js/localStorage "universal-ruleset" (pr-str rs)))
          :style {:display "inline-block" :padding "10px 28px"
                  :background "#4488ff" :color "#fff"
                  :text-decoration "none" :border-radius "6px"
                  :font-size "16px" :font-family "monospace"}}
      "Play this game"]]))

;; ── WebSocket handling ──────────────────────────────────────────────────────────

(defn- build-board-for [rs]
  "Build board data for v1, v2, or v3 ruleset."
  (cond
    (v3-ruleset? rs)
    (let [schema (:schema rs)]
      (topo/build-topology (or (:topology_type schema) "hex")
                           (or (:topology_size schema) 4)
                           (or (:topology_symmetry schema) 4)))
    (rs2/v2-ruleset? rs)
    (topo/build-topology (get-in rs [:board :topology])
                         (get-in rs [:board :size])
                         (get-in rs [:board :symmetry] 4))
    :else
    (game/build-board rs)))

(defn- compute-legal-actions [rs board state]
  "Compute legal actions for v1, v2, or v3."
  (cond
    (v3-ruleset? rs)  (game-v3/legal-actions rs board state)
    (rs2/v2-ruleset? rs) (game-v2/legal-actions rs board state)
    :else             (game/legal-actions rs board state)))

(defn- process-state-update! [state action-str]
  (let [me @player-key
        current (get-current-player state @ruleset-atom)
        my-turn? (= current me)
        prev-state @game-state
        acted-player (when prev-state (get-current-player prev-state @ruleset-atom))]
    ;; Add to history
    (when (and prev-state acted-player
               (or (not= current acted-player) (:winner state)))
      (swap! history-atom conj
             {:step (count @history-atom)
              :player acted-player
              :round (:round prev-state)
              :action (if action-str
                        (reader/read-string action-str)
                        ["unknown" nil nil])}))
    (reset! game-state state)
    (reset! selected-element nil)
    (reset! action-picker nil)
    (js/console.log "state update: my-turn?=" my-turn? "current=" current "me=" me
                    "has-ruleset?=" (boolean @ruleset-atom) "has-board?=" (boolean @board-atom)
                    "winner=" (:winner state) "elements=" (count (:elements state)))
    (if (and my-turn? @ruleset-atom @board-atom (not (:winner state)))
      (try
        (let [actions (compute-legal-actions @ruleset-atom @board-atom state)]
          (js/console.log "legal actions:" (count actions))
          (reset! legal-actions-atom actions))
        (catch js/Error e
          (js/console.error "Failed to compute legal actions:" (.-message e))
          (reset! legal-actions-atom {})))
      (reset! legal-actions-atom {}))))

(defn receive-message! [message]
  (let [msg-type (get message "type")]
    (case msg-type
      "initialize"
      (do
        (when-let [rs-str (get message "ruleset")]
          (let [rs (reader/read-string rs-str)]
            (reset! ruleset-atom rs)
            (reset! board-atom (build-board-for rs))))
        (when-let [state-str (get message "state")]
          (process-state-update! (reader/read-string state-str) nil)))

      "game-state"
      (do
        (when-let [rs-str (get message "ruleset")]
          (let [rs (reader/read-string rs-str)]
            (reset! ruleset-atom rs)
            (reset! board-atom (build-board-for rs))))
        (when-let [state-str (get message "state")]
          (process-state-update! (reader/read-string state-str) (get message "action"))))

      nil)))

;; ── Mount ───────────────────────────────────────────────────────────────────────

(defn game-page []
  [:div {:style {:display "flex" :width "100vw" :height "100vh"
                 :overflow "hidden" :background "#111"}}
   [rules-panel]
   [:div {:style {:flex "1" :overflow "hidden"}}
    [game-board]]
   [history-panel]])

;; ── Rules catalog page ─────────────────────────────────────────────────────────

(defn- rule-card-view
  "Render a single rule as a card."
  [idx {:keys [predicate effect requires-piece label]}]
  (let [card (desc/rule->card {:predicate predicate :effect effect :requires-piece requires-piece})]
    [:div {:style {:background "#151520" :border "1px solid #222" :border-radius "6px"
                   :padding "10px 14px" :margin-bottom "6px"}}
     [:div {:style {:display "flex" :align-items "center" :margin-bottom "4px"}}
      [:span {:style {:background "#333" :color "#aaa" :border-radius "3px"
                      :padding "1px 6px" :font-size "10px" :margin-right "8px"}}
       (str "Rule " (inc idx))]
      (when requires-piece
        [:span {:style {:color "#666" :font-size "10px"}} "requires own piece"])]
     (when label
       [:div {:style {:color "#ccbb88" :font-size "12px" :margin-bottom "4px"
                      :font-style "italic"}}
        label])
     [:div {:style {:color "#88aacc" :font-size "12px" :margin-bottom "2px"}}
      (str "When: " (or (:when card) "always"))]
     [:div {:style {:color "#aaccaa" :font-size "12px"}}
      (str "Then: " (or (:then card) "nothing"))]]))

(defn- interaction-card-view
  [{:keys [trigger predicate effect label]}]
  [:div {:style {:background "#1a1518" :border "1px solid #2a2025" :border-radius "6px"
                 :padding "10px 14px" :margin-bottom "6px"}}
   [:div {:style {:display "flex" :align-items "center" :margin-bottom "4px"}}
    [:span {:style {:background "#442233" :color "#cc88aa" :border-radius "3px"
                    :padding "1px 6px" :font-size "10px" :margin-right "8px"}}
     "Auto"]
    [:span {:style {:color "#666" :font-size "10px"}} (str "trigger: " trigger)]]
   (when label
     [:div {:style {:color "#ccbb88" :font-size "12px" :margin-bottom "4px"
                    :font-style "italic"}}
      label])
   [:div {:style {:color "#cc88aa" :font-size "12px" :margin-bottom "2px"}}
    (str "When: " (or (desc/pred->text predicate) "always"))]
   [:div {:style {:color "#aaccaa" :font-size "12px"}}
    (str "Then: " (or (desc/effect->text effect) "nothing"))]])

;; ── Visual diagrams for rules page ───────────────────────────────────────────

(def ^:private rule-colors
  ["#4488ff" "#44cc88" "#ffaa44" "#ff4488" "#aa44ff"
   "#44cccc" "#cccc44" "#cc8844" "#88ff44"])

(defn- turn-flow-diagram
  "SVG turn flow: phases as nodes with arrows."
  [game]
  (let [phases (or (:phases game)
                   [{:name "Act" :description "Choose and execute one action"}])
        n (count phases)
        w 320
        h (+ 60 (* n 50))
        node-x 160
        start-y 35]
    [:svg {:width w :height h :viewBox (str "0 0 " w " " h)
           :style {:background "none"}}
     ;; Phase nodes + arrows
     (for [[idx phase] (map-indexed vector phases)
           :let [y (+ start-y (* idx 50))
                 color (nth rule-colors (mod idx (count rule-colors)))]]
       ^{:key idx}
       [:g
        ;; Arrow from previous
        (when (pos? idx)
          [:line {:x1 node-x :y1 (- y 28) :x2 node-x :y2 (- y 12)
                  :stroke "#333" :stroke-width 1.5
                  :marker-end "url(#arrowhead)"}])
        ;; Node
        [:rect {:x 30 :y (- y 12) :width 260 :height 26 :rx 6
                :fill "#111118" :stroke color :stroke-width 1.5 :opacity 0.9}]
        [:text {:x 45 :y (+ y 4) :font-size "11" :fill color
                :font-family "monospace"}
         (:name phase)]
        (when-let [d (:decider phase)]
          (when (not= d "current player")
            [:text {:x 275 :y (+ y 4) :font-size "9" :fill "#666"
                    :text-anchor "end" :font-family "monospace"}
             d]))])
     ;; Loop-back arrow for simple games
     (when (<= n 2)
       [:g
        [:path {:d (str "M " (+ node-x 130) " " (+ start-y (* (dec n) 50) 14)
                    " C " (+ node-x 160) " " (+ start-y (* (dec n) 50) 40)
                    " " (+ node-x 160) " " (- start-y 30)
                    " " (+ node-x 130) " " (- start-y 12))
                :fill "none" :stroke "#333" :stroke-width 1
                :stroke-dasharray "4 3"}]
        [:text {:x (+ node-x 155) :y (+ start-y (* (dec n) 50) -5)
                :font-size "9" :fill "#444" :font-family "monospace"}
         "next player"]])
     ;; Arrowhead marker
     [:defs
      [:marker {:id "arrowhead" :markerWidth 8 :markerHeight 6
                :refX 8 :refY 3 :orient "auto"}
       [:path {:d "M0,0 L8,3 L0,6 Z" :fill "#555"}]]]]))

(defn- conflict-grid
  "SVG colored grid for type conflicts."
  [conflict num-types]
  (when (and (seq conflict) (> num-types 1))
    (let [cell 28 pad 40
          w (+ pad (* num-types cell))
          h (+ pad (* num-types cell))
          colors {0 "#222" 1 "#2a4a2a" 2 "#4a2a2a" 3 "#4a3a1a"}
          labels {0 "" 1 ">" 2 "<" 3 "X"}]
      [:svg {:width w :height h :viewBox (str "0 0 " w " " h)
             :style {:background "none"}}
       ;; Header labels
       (for [i (range num-types)]
         ^{:key (str "h" i)}
         [:g
          [:text {:x (+ pad (* i cell) (/ cell 2)) :y 12
                  :text-anchor "middle" :font-size "10" :fill "#888"
                  :font-family "monospace"}
           (str "T" i)]
          [:text {:x 12 :y (+ pad (* i cell) (/ cell 2) 4)
                  :text-anchor "middle" :font-size "10" :fill "#888"
                  :font-family "monospace"}
           (str "T" i)]])
       ;; Cells
       (for [i (range num-types)
             j (range num-types)
             :let [raw (if (= i j) 0
                         (let [a (min i j) b (max i j)
                               idx (loop [x 0 k 0]
                                     (if (= k a) (+ x (- b a 1))
                                       (recur (+ x (- num-types 1 k)) (inc k))))]
                           (nth conflict idx 0)))
                   ;; Flip display: if raw=2 and i>j, attacker wins
                   display (cond (= i j) 0
                                 (= raw 1) (if (< i j) 1 2)
                                 (= raw 2) (if (< i j) 2 1)
                                 :else raw)]]
         ^{:key (str i "-" j)}
         [:g
          [:rect {:x (+ pad (* j cell)) :y (+ pad (* i cell) -12)
                  :width (dec cell) :height (dec cell) :rx 3
                  :fill (get colors display "#222")}]
          [:text {:x (+ pad (* j cell) (/ cell 2))
                  :y (+ pad (* i cell) (/ cell 2) -2)
                  :text-anchor "middle" :font-size "12" :fill "#aaa"
                  :font-family "monospace"}
           (get labels display "")]])])))

(defn- infer-rule-labels
  "Infer meaningful source/target/action labels from the predicate+effect ASTs."
  [{:keys [predicate effect requires-piece]}]
  (let [;; Walk predicate to find target type
        tgt-label (cond
                    (some #{desc/P_EMPTY} (flatten predicate)) "empty"
                    (and (some #{desc/P_OWNER_IS} (flatten predicate))
                         (some #{desc/ENEMY} (flatten predicate))) "enemy"
                    (and (some #{desc/P_OWNER_IS} (flatten predicate))
                         (not (some #{desc/ENEMY} (flatten predicate)))) "ally"
                    :else "space")
        ;; Infer action from effect
        eff-op (if (sequential? effect) (first effect) 0)
        action (case (int eff-op)
                 1 "remove" 2 "place" 3 "move" 4 "swap" 5 "push"
                 6 "resource" 7 "transfer" 8 "collect" 9 "score"
                 11 "capture" 12 "score" 13 "draw" 17 "cipher" 19 "link"
                 20 (let [sub-op (first (second effect))]
                      (case (int (or sub-op 0))
                        1 "capture" 3 "move" 6 "grow" 11 "capture"
                        "combo"))
                 21 "conditional" 22 "phase"
                 "act")
        src-label (if requires-piece "yours" "board")]
    {:src src-label :tgt tgt-label :action action}))

(defn- rule-mini-diagram
  "Small SVG showing a rule's source → effect → target pattern."
  [idx rule]
  (let [color (nth rule-colors (mod idx (count rule-colors)))
        {:keys [src tgt action]} (infer-rule-labels rule)
        requires-piece (:requires-piece rule)
        w 160 h 52]
    [:svg {:width w :height h :viewBox (str "0 0 " w " " h)
           :style {:background "none"}}
     ;; Source node
     [:g
      [:circle {:cx 28 :cy 26 :r 16
                :fill (if requires-piece "#1a2030" "#111118")
                :stroke (if requires-piece color "#444") :stroke-width 1.5}]
      [:text {:x 28 :y 30 :text-anchor "middle" :font-size "8" :fill (if requires-piece color "#888")
              :font-family "monospace"} src]]
     ;; Arrow with action label
     [:line {:x1 48 :y1 26 :x2 104 :y2 26
             :stroke color :stroke-width 1.5 :stroke-dasharray "4 2"
             :marker-end "url(#arrowhead)"}]
     [:text {:x 76 :y 18 :text-anchor "middle" :font-size "9" :fill color
             :font-weight "bold" :font-family "monospace"}
      action]
     ;; Target node
     [:circle {:cx 120 :cy 26 :r 16 :fill "#111118"
               :stroke (case tgt "enemy" "#ff6666" "ally" "#66ff66" "empty" "#666" "#444")
               :stroke-width 1.5
               :stroke-dasharray (when (= tgt "empty") "3 2")}]
     [:text {:x 120 :y 30 :text-anchor "middle" :font-size "8"
             :fill (case tgt "enemy" "#ff8888" "ally" "#88ff88" "#888")
             :font-family "monospace"} tgt]
     ;; Arrowhead
     [:defs
      [:marker {:id "arrowhead" :markerWidth 8 :markerHeight 6
                :refX 8 :refY 3 :orient "auto"}
       [:path {:d "M0,0 L8,3 L0,6 Z" :fill "#555"}]]]]))

(defn- board-topology-preview
  "Small SVG preview of the board shape."
  [schema]
  (let [topo-str (str (:topology schema))
        w 320 h 200]
    [:svg {:width w :height h :viewBox (str "0 0 " w " " h)
           :style {:background "none"}}
     (cond
       ;; Square grid
       (str/includes? topo-str "square")
       (let [n (min 7 (or (when (number? (:size schema)) (:size schema)) 5))
             cell (/ 160 n) off 80]
         [:g
          ;; Grid lines
          (for [i (range (inc n))]
            ^{:key (str "h" i)}
            [:line {:x1 off :y1 (+ 20 (* i cell)) :x2 (+ off (* n cell)) :y2 (+ 20 (* i cell))
                    :stroke "#333" :stroke-width 0.5}])
          (for [i (range (inc n))]
            ^{:key (str "v" i)}
            [:line {:x1 (+ off (* i cell)) :y1 20 :x2 (+ off (* i cell)) :y2 (+ 20 (* n cell))
                    :stroke "#333" :stroke-width 0.5}])
          ;; Intersections
          (for [r (range n) c (range n)]
            ^{:key (str r "-" c)}
            [:circle {:cx (+ off (* c cell) (/ cell 2))
                      :cy (+ 20 (* r cell) (/ cell 2))
                      :r 3 :fill "#2a2a3e"}])])

       ;; Hex grid
       (or (str/includes? topo-str "hex") (str/includes? topo-str "infinite"))
       (let [radius 3 s 22 cx 160 cy 100]
         [:g
          (for [q (range (- radius) radius)
                r (range (- radius) radius)
                :when (< (Math/abs (+ q r)) radius)
                :let [x (+ cx (* s 1.5 q))
                      y (+ cy (* s (js/Math.sqrt 3) (+ r (* 0.5 q))))]]
            ^{:key (str q "," r)}
            [:g
             [:circle {:cx x :cy y :r 8 :fill "#1a1a2e" :stroke "#333" :stroke-width 0.5}]
             ;; Lines to neighbors
             (for [[dq dr] [[1 0] [0 1] [1 -1]]
                   :let [nq (+ q dq) nr (+ r dr)]
                   :when (< (Math/abs (+ nq nr)) radius)
                   :let [nx (+ cx (* s 1.5 nq))
                         ny (+ cy (* s (js/Math.sqrt 3) (+ nr (* 0.5 nq))))]]
               ^{:key (str dq "," dr)}
               [:line {:x1 x :y1 y :x2 nx :y2 ny
                       :stroke "#222" :stroke-width 0.5}])])])

       ;; Ring/radial
       (str/includes? topo-str "radial")
       (let [cx 160 cy 100 rings 4 sym 5]
         [:g
          ;; Center
          [:circle {:cx cx :cy cy :r 4 :fill "#2a2a3e" :stroke "#444" :stroke-width 0.5}]
          ;; Rings
          (for [ring (range 1 rings)
                :let [radius (* ring 22)
                      n-spaces (* ring sym)]]
            ^{:key ring}
            [:g
             [:circle {:cx cx :cy cy :r radius :fill "none" :stroke "#1a1a2e" :stroke-width 0.5}]
             (for [step (range n-spaces)
                   :let [angle (- (* step (/ (* 2 js/Math.PI) n-spaces)) (/ js/Math.PI 2))
                         x (+ cx (* radius (js/Math.cos angle)))
                         y (+ cy (* radius (js/Math.sin angle)))]]
               ^{:key step}
               [:circle {:cx x :cy y :r 3 :fill "#2a2a3e" :stroke "#444" :stroke-width 0.5}])])])

       ;; Triangle
       (str/includes? topo-str "triangle")
       (let [n 5 cell 30 off 80]
         [:g
          (for [r (range n) c (range n)
                :let [x (+ off (* c cell) (if (odd? r) (/ cell 2) 0))
                      y (+ 30 (* r cell 0.87))]]
            ^{:key (str r "-" c)}
            [:circle {:cx x :cy y :r 3 :fill "#2a2a3e" :stroke "#444" :stroke-width 0.5}])])

       ;; Default: simple nodes
       :else
       [:g
        (for [i (range 12)
              :let [angle (* i (/ (* 2 js/Math.PI) 12))
                    x (+ 160 (* 60 (js/Math.cos angle)))
                    y (+ 100 (* 60 (js/Math.sin angle)))]]
          ^{:key i}
          [:circle {:cx x :cy y :r 4 :fill "#2a2a3e" :stroke "#444" :stroke-width 0.5}])])

     ;; Label
     [:text {:x 160 :y (- h 4) :text-anchor "middle" :font-size "10" :fill "#555"
             :font-family "monospace"}
      (str (:topology schema))]]))

(defn- game-diagrams
  "Right column: visual diagrams for a game encoding."
  [game]
  (let [{:keys [rules conflict schema]} game
        num-types (or (:pieces schema) 1)]
    [:div {:style {:display "flex" :flex-direction "column" :gap "16px"}}
     ;; Board topology preview
     [:div
      [:div {:style {:color "#555" :font-size "10px" :text-transform "uppercase"
                     :letter-spacing "1px" :margin-bottom "6px"}} "Board"]
      [board-topology-preview schema]]

     ;; Turn flow
     [:div
      [:div {:style {:color "#555" :font-size "10px" :text-transform "uppercase"
                     :letter-spacing "1px" :margin-bottom "6px"}} "Turn flow"]
      [turn-flow-diagram game]]

     ;; Rule patterns
     (when (seq rules)
       [:div
        [:div {:style {:color "#555" :font-size "10px" :text-transform "uppercase"
                       :letter-spacing "1px" :margin-bottom "6px"}} "Rule patterns"]
        [:div {:style {:display "flex" :flex-wrap "wrap" :gap "4px"}}
         (for [[idx rule] (map-indexed vector rules)]
           ^{:key idx}
           [rule-mini-diagram idx rule])]])

     ;; Conflict matrix
     (when (seq conflict)
       [:div
        [:div {:style {:color "#555" :font-size "10px" :text-transform "uppercase"
                       :letter-spacing "1px" :margin-bottom "6px"}} "Conflict matrix"]
        ;; Convert conflict list to flat vector for the grid
        (let [n (if (number? num-types) num-types
                  (+ 1 (apply max (map :b conflict))))
              flat (vec (repeat (* n (dec n) (/ 1 2)) 0))
              ;; fill from conflict descriptions
              lookup {"coexist" 0 "type 0 beats type 2" 1
                      "type A wins" 1 "type B wins" 2
                      "mutual destruction" 3}]
          [conflict-grid
           (mapv (fn [{:keys [outcome]}]
                   (cond
                     (str/includes? (str outcome) "beats") 1
                     (str/includes? (str outcome) "mutual") 3
                     (str/includes? (str outcome) "coexist") 0
                     :else 0))
                 conflict)
           n])])

     ;; Description length bar
     [:div
      [:div {:style {:color "#555" :font-size "10px" :text-transform "uppercase"
                     :letter-spacing "1px" :margin-bottom "6px"}} "Parsimony"]
      (let [bits (:description-bits game 100)
            max-bits 800
            pct (min 100 (* 100 (/ bits max-bits)))]
        [:div {:style {:position "relative" :height "24px" :background "#111118"
                       :border-radius "4px" :border "1px solid #222" :overflow "hidden"}}
         [:div {:style {:position "absolute" :left 0 :top 0 :height "100%"
                        :width (str pct "%")
                        :background (cond (< bits 150) "#2a4a2a"
                                          (< bits 400) "#3a3a1a"
                                          :else "#4a2a2a")
                        :border-radius "4px"}}]
         [:span {:style {:position "absolute" :left "8px" :top "4px"
                         :font-size "11px" :color "#ccc" :font-family "monospace"}}
          (str bits " bits")]
         [:span {:style {:position "absolute" :right "8px" :top "4px"
                         :font-size "10px" :color "#666" :font-family "monospace"}}
          (cond (< bits 150) "minimal"
                (< bits 300) "compact"
                (< bits 500) "moderate"
                :else "complex")]])]]))

(defn- game-encoding-view
  "Full visualization of one game's encoding."
  [{:keys [subtitle description schema rules interactions
           terminations conflict description-bits notes] game-name :name :as game}]
  [:div {:style {:background "#0d0d18" :border "1px solid #1a1a2e" :border-radius "8px"
                 :padding "20px" :margin-bottom "20px"}}
   ;; Header
   [:div {:style {:display "flex" :justify-content "space-between" :align-items "baseline"
                  :margin-bottom "4px"}}
    [:h2 {:style {:color "#fff" :font-size "20px" :margin 0}} game-name]
    [:span {:style {:color "#555" :font-size "12px"}}
     (str description-bits " bits")]]
   [:div {:style {:color "#888" :font-size "13px" :margin-bottom "8px"}} subtitle]
   [:div {:style {:color "#777" :font-size "12px" :margin-bottom "14px"
                  :line-height "1.5"}}
    description]

   ;; Two-column layout: text (left) + diagrams (right)
   [:div {:style {:display "flex" :gap "24px"}}
    ;; LEFT COLUMN: text descriptions
    [:div {:style {:flex "1" :min-width "300px"}}

   ;; Schema summary
   [:div {:style {:display "flex" :gap "16px" :margin-bottom "14px" :flex-wrap "wrap"}}
    (for [[k v] [["Board" (:topology schema)]
                 ["Nodes" (:size schema)]
                 ["Types" (:pieces schema)]
                 ["Players" (:players schema)]
                 ["Resources" (:resources schema 0)]]]
      ^{:key k}
      [:div {:style {:background "#111118" :border "1px solid #222" :border-radius "4px"
                     :padding "4px 10px"}}
       [:span {:style {:color "#666" :font-size "10px" :text-transform "uppercase"
                       :letter-spacing "1px"}} k]
       [:div {:style {:color "#ddd" :font-size "14px"}} (str v)]])]

   ;; Extended state structures
   (when-let [ext (:extended schema)]
     (when (and ext (not= ext "none"))
       [:div {:style {:margin-bottom "10px" :color "#8888cc" :font-size "12px"
                      :padding "6px 10px" :background "#12121e" :border-radius "4px"
                      :border "1px solid #1a1a30"}}
        ext]))

   ;; Turn phases (for complex games like Journey)
   (when-let [phases (:phases game)]
     [:div {:style {:margin-bottom "14px"}}
      [:div {:style {:color "#666" :font-size "10px" :text-transform "uppercase"
                     :letter-spacing "1px" :margin-bottom "6px"}}
       (str (count phases) " turn phases")]
      (for [[idx {:keys [phase-name decider description] :as phase}] (map-indexed vector phases)]
        ^{:key idx}
        [:div {:style {:background "#111520" :border "1px solid #1a2030" :border-radius "6px"
                       :padding "8px 12px" :margin-bottom "4px"}}
         [:div {:style {:display "flex" :align-items "center" :gap "8px" :margin-bottom "3px"}}
          [:span {:style {:background "#1a2a4a" :color "#88aadd" :border-radius "3px"
                          :padding "1px 6px" :font-size "10px"}}
           (str "Phase " (inc idx))]
          [:span {:style {:color "#ccc" :font-size "13px"}} (:name phase)]
          (when (and decider (not= decider "current player"))
            [:span {:style {:color "#666" :font-size "10px"}} (str "(" decider ")")])]
         [:div {:style {:color "#888" :font-size "11px"}} description]])])

   ;; Rules
   [:div {:style {:margin-bottom "12px"}}
    [:div {:style {:color "#666" :font-size "10px" :text-transform "uppercase"
                   :letter-spacing "1px" :margin-bottom "6px"}}
     (str (count rules) " player action" (when (> (count rules) 1) "s"))]
    (for [[idx rule] (map-indexed vector rules)]
      ^{:key idx}
      [rule-card-view idx rule])]

   ;; Interactions
   (when (seq interactions)
     [:div {:style {:margin-bottom "12px"}}
      [:div {:style {:color "#666" :font-size "10px" :text-transform "uppercase"
                     :letter-spacing "1px" :margin-bottom "6px"}}
       "Automatic interactions"]
      (for [[idx ir] (map-indexed vector interactions)]
        ^{:key idx}
        [interaction-card-view ir])])

   ;; Conflict table
   (when (seq conflict)
     [:div {:style {:margin-bottom "12px"}}
      [:div {:style {:color "#666" :font-size "10px" :text-transform "uppercase"
                     :letter-spacing "1px" :margin-bottom "6px"}}
       "Type conflicts"]
      (for [{:keys [a b outcome]} conflict]
        ^{:key (str a "-" b)}
        [:div {:style {:color "#999" :font-size "12px" :padding "2px 0"}}
         (str "Type " a " vs Type " b ": " outcome)])])

   ;; Win conditions
   [:div {:style {:margin-bottom "12px"}}
    [:div {:style {:color "#666" :font-size "10px" :text-transform "uppercase"
                   :letter-spacing "1px" :margin-bottom "6px"}}
     "Win conditions"]
    (for [[idx t] (map-indexed vector terminations)]
      ^{:key idx}
      [:div {:style {:color "#ccaa66" :font-size "12px" :padding "2px 0"}}
       (or (:text t) (desc/term->text (:predicate t)))])]

   ;; Notes (if any)
   (when-let [notes (:notes game)]
     [:div {:style {:margin-top "8px" :padding "10px 14px" :background "#111118"
                    :border "1px solid #1a1a22" :border-radius "6px"
                    :color "#666" :font-size "11px" :line-height "1.5"
                    :font-style "italic"}}
      notes])]

    ;; RIGHT COLUMN: visual diagrams
    [:div {:style {:width "340px" :flex-shrink "0"}}
     [game-diagrams game]]]])

(defn rules-catalog-view []
  [:div {:style {:color "#ccc" :font-family "monospace" :padding "30px 40px"
                 :max-width "1100px" :margin "0 auto" :min-height "100vh"
                 :background "#111"}}
   [:h1 {:style {:color "#fff" :margin-bottom "4px"}} "Game Encodings"]
   [:div {:style {:color "#666" :font-size "12px" :margin-bottom "24px"}}
    "Each game is encoded as (state schema, rules, interactions, win conditions). "
    "Rules are predicate/effect programs composed from universal primitives."]

   ;; Navigation
   [:div {:style {:margin-bottom "20px" :display "flex" :gap "8px"}}
    [:a {:href "/universal" :style {:color "#4488ff" :font-size "12px"}} "< back to games"]]

   ;; Game list
   (for [game desc/all-encodings]
     ^{:key (:name game)}
     [game-encoding-view game])])

(defn mount-components []
  (let [play-key (.-playKey js/window)
        is-create (.-isCreate js/window)
        is-rules (.-isRules js/window)]

    ;; Human is always player "0" in universal games
    (reset! player-key "0")

    (cond
      is-rules
      (rdom/render [rules-catalog-view]
                (.getElementById js/document "universal"))

      is-create
      (rdom/render [create-view]
                (.getElementById js/document "universal"))

      :else
      (do (let [host (.-host js/location)
              protocol (if (= "https:" (.-protocol js/location)) "wss" "ws")
              url (str protocol "://" host "/ws/universal/play/" play-key)]
          (ws/make-websocket! url receive-message!))

        ;; Auto-create game if none exists, using stored ruleset or default
        (js/setTimeout
         (fn []
           (when-not @game-state
             (let [stored (.getItem js/localStorage "universal-ruleset")
                   ruleset-str (or stored (pr-str (:ruleset (first rs/discovered-games))))]
               (ws/send-transit-message!
                {"type" "create"
                 "ruleset" ruleset-str}))))
         500)

        (rdom/render [game-page]
                  (.getElementById js/document "universal"))))))
