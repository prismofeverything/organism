(ns organism.universal.play
  "Universal game frontend: renders any ring board game and handles interaction."
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.websockets :as ws]
   [organism.universal.game :as game]
   [organism.universal.ruleset :as rs]))

;; ── State atoms ─────────────────────────────────────────────────────────────────

(defonce game-state (r/atom nil))
(defonce ruleset-atom (r/atom nil))
(defonce board-atom (r/atom nil))
(defonce player-key (r/atom nil))
(defonce selected-element (r/atom nil))
(defonce legal-actions-atom (r/atom {}))
;; When multiple actions to same target, show a picker
(defonce action-picker (r/atom nil)) ; {:from space :to space :actions [...]}
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
  {:move      "#66aaff"
   :eat       "#66ee66"
   :grow      "#ffcc00"
   :capture   "#ff4444"
   :circulate "#cc88ff"
   :pass      "#666666"})

(defn player-color [player]
  (let [idx (js/parseInt player 10)]
    (nth player-colors (mod idx (count player-colors)))))

(defn player-glow [player]
  (let [idx (js/parseInt player 10)]
    (nth player-glow-colors (mod idx (count player-glow-colors)))))

;; ── Board geometry (ring → pixel) ───────────────────────────────────────────────

(def board-cx 400)
(def board-cy 400)
(def ring-spacing 55)
(def space-radius 18)

(defn space->pixel
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

(defn compute-locations
  [rings adjacencies]
  (reduce
   (fn [locs space]
     (assoc locs space (space->pixel rings space)))
   {} (keys adjacencies)))

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
      (let [{:keys [adjacencies rings]} board
            locations (compute-locations rings adjacencies)
            my-turn? (= (:current-player state) @player-key)
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
         (for [[space amount] (:free-food state)
               :when (pos? amount)
               :let [[x y] (get locations space)]
               :when x]
           ^{:key (str "ff-" (pr-str space))}
           [:g
            [:circle {:cx x :cy (+ y 8) :r 5
                      :fill "#ffe066" :opacity 0.6}]
            (when (> amount 1)
              [:text {:x (+ x 7) :y (+ y 12) :font-size "8" :fill "#aa8" :opacity 0.8}
               (str amount)])])

         ;; Elements
         (for [[space el] (:elements state)
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
                        :fill "none" :stroke "#fff" :stroke-width 2.5}])])

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
  (let [n (:num-types ruleset)
        type-name (fn [t] (str "Type " t))]
    [:div {:style {:margin-top "8px"}}
     [:div {:style {:color "#888" :font-size "10px" :margin-bottom "4px"
                    :text-transform "uppercase" :letter-spacing "1px"}}
      "Conflict resolution"]
     (for [i (range n)
           j (range (inc i) n)
           :let [outcome (rs/outcome-for ruleset i j)]]
       ^{:key (str i "-" j)}
       [:div {:style {:font-size "11px" :padding "1px 0"}}
        [:span {:style {:color (get action-type-colors :capture "#aaa")}}
         (case outcome
           :wins    (str (type-name i) " beats " (type-name j))
           :loses   (str (type-name j) " beats " (type-name i))
           :coexist (str (type-name i) " & " (type-name j) " coexist")
           :mutual  (str (type-name i) " & " (type-name j) " mutual destruction")
           (str i " vs " j ": " (pr-str outcome)))]])]))

(defn rules-panel []
  (let [ruleset @ruleset-atom
        state @game-state]
    (when ruleset
      (let [current (when state (:current-player state))
            winner (when state (:winner state))
            my-turn? (= current @player-key)]
        [:div {:style {:width "240px" :background "#0a0a14"
                       :border-right "1px solid #1a1a2e"
                       :overflow-y "auto" :padding "12px"
                       :color "#ccc" :font-family "monospace" :font-size "12px"
                       :flex-shrink "0"}}

         ;; Game title
         [:div {:style {:font-size "14px" :color "#fff" :margin-bottom "10px"
                        :border-bottom "1px solid #222" :padding-bottom "8px"}}
          (str (:board-symmetry ruleset) "-fold, "
               (:num-rings ruleset) " rings, "
               (:num-types ruleset) " types")]

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
            (for [p (:turn-order state)]
              (let [is-me? (= p @player-key)
                    is-current? (= p current)
                    pop (count (filter #(= p (:player %)) (vals (:elements state))))
                    caps (get-in state [:captures p] 0)]
                ^{:key p}
                [:div {:style {:color (player-color p) :padding "2px 0"
                               :font-weight (if is-current? "bold" "normal")
                               :opacity (if is-current? 1.0 0.6)}}
                 (str (if is-me? "▸ YOU" (str "  P" p))
                      "  " pop " units  " caps " captures"
                      (when (= (:win-type ruleset) :captures)
                        (str "/" (:win-threshold ruleset))))]))])

         ;; Rules explanation
         [:div {:style {:border-top "1px solid #222" :padding-top "8px" :margin-top "4px"}}
          [:div {:style {:color "#666" :font-size "10px" :margin-bottom "6px"
                         :text-transform "uppercase" :letter-spacing "1px"}}
           "Rules"]

          [:div {:style {:margin-bottom "6px" :color "#aaa" :font-size "11px"}}
           (str "Board: " (:board-symmetry ruleset) "-fold symmetry with "
                (:num-rings ruleset) " rings")]

          [:div {:style {:margin-bottom "6px" :color "#aaa" :font-size "11px"}}
           (str "Each player starts with " (:elements-per-player ruleset)
                " pieces (" (:num-types ruleset) " types)")]

          [:div {:style {:margin-bottom "6px" :color "#aaa" :font-size "11px"}}
           (str "Win by: "
                (case (:win-type ruleset)
                  :captures (str "capturing " (:win-threshold ruleset) " enemy pieces")
                  :population (str "growing to " (:win-threshold ruleset) " pieces")
                  "unknown"))]

          ;; Available actions
          [:div {:style {:margin-top "8px"}}
           [:div {:style {:color "#666" :font-size "10px" :margin-bottom "4px"
                          :text-transform "uppercase" :letter-spacing "1px"}}
            "Actions"]
           (when (:can-move ruleset)
             [:div {:style {:font-size "11px" :padding "2px 0"}}
              [:span {:style {:color (action-type-colors :move)}} "M "]
              [:span {:style {:color "#999"}} "Move to empty adjacent space"]])
           (when (:can-eat ruleset)
             [:div {:style {:font-size "11px" :padding "2px 0"}}
              [:span {:style {:color (action-type-colors :eat)}} "E "]
              [:span {:style {:color "#999"}} "Eat food from adjacent space"]])
           (when (:can-grow ruleset)
             [:div {:style {:font-size "11px" :padding "2px 0"}}
              [:span {:style {:color (action-type-colors :grow)}} "G "]
              [:span {:style {:color "#999"}} "Spend food to grow new piece"]])
           (when (:can-capture ruleset)
             [:div {:style {:font-size "11px" :padding "2px 0"}}
              [:span {:style {:color (action-type-colors :capture)}} "X "]
              [:span {:style {:color "#999"}} "Capture weaker adjacent enemy"]])
           (when (:can-circulate ruleset)
             [:div {:style {:font-size "11px" :padding "2px 0"}}
              [:span {:style {:color (action-type-colors :circulate)}} "C "]
              [:span {:style {:color "#999"}} "Pass food to friendly neighbor"]])]

          ;; Conflict table
          [conflict-explanation ruleset]

          ;; Food info
          (when (:food-enabled ruleset)
            [:div {:style {:margin-top "8px" :color "#999" :font-size "11px"}}
             (str "Food enabled (start: " (:food-initial ruleset) " per piece)")
             [:br]
             "Captured pieces drop food on the board"])]

         ;; Pass button
         (when (and state my-turn? (not winner))
           [:button {:style {:margin-top "12px" :padding "6px 16px" :width "100%"
                             :background "#222" :color "#ccc" :border "1px solid #444"
                             :cursor "pointer" :border-radius "4px" :font-family "monospace"}
                     :on-click #(send-action! [:pass nil nil])}
            "Pass turn"])]))))

;; ── History panel (right) ────────────────────────────────────────────────────────

(defn format-action
  "Human-readable description of an action."
  [[action-type from-sp to-sp & args]]
  (let [fmt-sp (fn [sp] (if sp (str (first sp) ":" (second sp)) "—"))]
    (case action-type
      :move      (str "Move " (fmt-sp from-sp) " → " (fmt-sp to-sp))
      :eat       (str "Eat at " (fmt-sp to-sp))
      :grow      (str "Grow type " (first args) " at " (fmt-sp to-sp))
      :capture   (str "Capture at " (fmt-sp to-sp))
      :circulate (str "Feed " (fmt-sp from-sp) " → " (fmt-sp to-sp))
      :pass      "Pass"
      (str action-type))))

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

(defonce create-ruleset (r/atom (:ruleset (first rs/discovered-games))))

(defn game-card
  "A clickable card for a discovered game."
  [{:keys [name richness description ruleset]} selected?]
  [:div {:style {:padding "10px 14px" :margin-bottom "6px" :cursor "pointer"
                 :border-radius "6px"
                 :background (if selected? "#1a2a3a" "#111118")
                 :border (str "1px solid " (if selected? "#4488ff" "#222"))
                 :transition "all 0.15s"}
         :on-click #(reset! create-ruleset ruleset)}
   [:div {:style {:display "flex" :justify-content "space-between" :align-items "baseline"}}
    [:span {:style {:color (if selected? "#fff" "#ccc") :font-size "14px"
                    :font-weight (if selected? "bold" "normal")}}
     name]
    [:span {:style {:color "#666" :font-size "11px"}}
     (str "score " richness)]]
   [:div {:style {:color "#777" :font-size "11px" :margin-top "3px"}}
    description]
   [:div {:style {:color "#555" :font-size "10px" :margin-top "3px"}}
    (str (:board-symmetry ruleset) "-fold  "
         (:num-rings ruleset) "r  "
         (:num-types ruleset) "t  "
         (:num-players ruleset) "p  "
         (clojure.core/name (:win-type ruleset)) ">" (:win-threshold ruleset))]])

(defn create-view []
  (let [rs @create-ruleset
        game-id (str (random-uuid))]
    [:div {:style {:color "#ccc" :font-family "monospace" :padding "30px 40px"
                   :max-width "700px" :margin "0 auto"}}

     [:h1 {:style {:color "#fff" :margin-bottom "4px"}} "Universal Game Player"]
     [:div {:style {:color "#666" :font-size "12px" :margin-bottom "20px"}}
      "Games discovered by evolutionary search + AlphaZero depth evaluation"]

     ;; Discovered games list
     [:div {:style {:margin-bottom "16px"}}
      [:div {:style {:color "#888" :font-size "10px" :margin-bottom "8px"
                     :text-transform "uppercase" :letter-spacing "1.5px"}}
       (str (count rs/discovered-games) " discovered games (ranked by strategic depth)")]
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

(defn receive-message! [message]
  (let [msg-type (get message "type")]
    (case msg-type
      "initialize"
      (do
        (js/console.log "initialize msg, has state?" (boolean (get message "state"))
                        "has ruleset?" (boolean (get message "ruleset")))
        (when-let [rs-str (get message "ruleset")]
          (let [rs (reader/read-string rs-str)]
            (reset! ruleset-atom rs)
            (reset! board-atom (game/build-board rs))))
        (when-let [state-str (get message "state")]
          (let [state (reader/read-string state-str)
                me @player-key
                my-turn? (= (:current-player state) me)]
            (js/console.log "init state: current=" (:current-player state) "me=" me)
            (reset! game-state state)
            (when (and my-turn? @ruleset-atom @board-atom (not (:winner state)))
              (let [actions (game/legal-actions @ruleset-atom @board-atom state)]
                (js/console.log "init legal actions:" (count actions))
                (reset! legal-actions-atom actions))))))

      "game-state"
      (do
        (when-let [rs-str (get message "ruleset")]
          (let [rs (reader/read-string rs-str)]
            (reset! ruleset-atom rs)
            (reset! board-atom (game/build-board rs))))
        (when-let [state-str (get message "state")]
          (let [state (reader/read-string state-str)
                me @player-key
                current (:current-player state)
                my-turn? (= current me)
                ;; Detect the action: server sends the *result* state, so the
                ;; player who just acted is the one *before* current in turn order
                prev-state @game-state
                acted-player (when prev-state (:current-player prev-state))
                action-str (get message "action")]

            ;; Add to history if a turn actually changed
            (when (and prev-state acted-player
                       (or (not= current acted-player)
                           (:winner state)))
              (swap! history-atom conj
                     {:step (count @history-atom)
                      :player acted-player
                      :round (:round prev-state)
                      :action (if action-str
                                (reader/read-string action-str)
                                [:unknown nil nil])}))

            (reset! game-state state)
            (reset! selected-element nil)
            (reset! action-picker nil)
            (if (and my-turn? @ruleset-atom @board-atom (not (:winner state)))
              (let [actions (game/legal-actions @ruleset-atom @board-atom state)]
                (reset! legal-actions-atom actions))
              (reset! legal-actions-atom {})))))

      nil)))

;; ── Mount ───────────────────────────────────────────────────────────────────────

(defn game-page []
  [:div {:style {:display "flex" :width "100vw" :height "100vh"
                 :overflow "hidden" :background "#111"}}
   [rules-panel]
   [:div {:style {:flex "1" :overflow "hidden"}}
    [game-board]]
   [history-panel]])

(defn mount-components []
  (let [play-key (.-playKey js/window)
        is-create (.-isCreate js/window)]

    ;; Human is always player "0" in universal games
    (reset! player-key "0")

    (if is-create
      (rdom/render [create-view]
                (.getElementById js/document "universal"))

      (do
        (let [host (.-host js/location)
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
