(ns table.input
  "Mouse and keyboard. Left drags things (or draws, in paint mode), right drags
   the view round and right-click opens a menu, the wheel zooms to the cursor,
   middle or shift-right pans, WASD slides. Keys act on whatever is under the
   cursor, or on the selection when the cursor is on it or on nothing."
  (:require [clojure.string :as str]
            [table.actions :as act]
            [table.mesh :as mesh]
            [table.state :as st]
            [table.world :as world]
            [reagent.core :as r]))

(defonce gesture (atom nil))
(defonce held-keys (atom #{}))
(defonce mouse (r/atom [0 0]))   ; reactive: the tooltip follows it
(def lift 18)   ; how high a held thing floats over what's under it

(defn- hover-id [] (:hover @st/ui))

(defn targets
  "What a key or menu acts on: the thing under the cursor, unless that is part
   of the selection, or locked (a board under everything) while something is
   selected -- then the selection."
  []
  (let [{:keys [selected hover]} @st/ui
        locked? (:locked (st/obj hover))]
    (cond (and hover (not (contains? selected hover)) (not (and locked? (seq selected)))) [hover]
          (seq selected) (vec selected)
          :else [])))

(defn- update-hover! [x y]
  (let [id (:id (world/pick x y))]
    (when (not= id (:hover @st/ui))
      (swap! st/ui assoc :hover id)
      (world/sync!))))

(defn- cursor-point []
  (let [[x y] @mouse]
    (or (world/table-point x y 0) (let [[cx cz] (world/view-centre)] #js {:x cx :z cz}))))

;; ── painting ────────────────────────────────────────────────────────────────

(defn- paint-hit
  "{:id :uv} when x y is on the top face of something you can draw on."
  [x y & [only-id]]
  (when-let [{:keys [id uv material]} (world/pick x y)]
    (let [o (st/obj id)]
      (when (and o uv (mesh/can-paint? o)
                 (or (nil? only-id) (= id only-id))
                 (= material (mesh/top-index (= "round" (:shape o)))))
        {:id id :uv [(/ (js/Math.round (* 1e5 (.-x uv))) 1e5)
                     (/ (js/Math.round (* 1e5 (.-y uv))) 1e5)]}))))

(defn- start-paint! [x y]
  (when-let [{:keys [id uv]} (paint-hit x y)]
    (let [{:keys [tool brush]} @st/ui
          stroke (merge (select-keys brush [:color :size :alpha :fill]) {:tool tool :pts [uv]})]
      (if (= tool :text)
        (when-let [t (not-empty (js/prompt "Text to write here:"))]
          (act/add-stroke! id (assoc stroke :text t)))
        (do (reset! gesture {:type :paint :id id :stroke stroke})
            ;; a board must have a canvas before the first live stroke shows
            (when-not (mesh/paintable? (st/obj id))
              (act/add-stroke! id (assoc stroke :pts [])))
            (world/live-stroke! id stroke)))
      true)))

(defn- continue-paint! [x y]
  (let [{:keys [id stroke]} @gesture]
    (when-let [{:keys [uv]} (paint-hit x y id)]
      (let [o (st/obj id)
            [w _ d] (:size o)
            last-uv (peek (:pts stroke))
            dist (js/Math.hypot (* w (- (first uv) (first last-uv))) (* d (- (second uv) (second last-uv))))
            stroke (if (#{:pen :erase} (:tool stroke))
                     (if (> dist 0.4) (update stroke :pts conj uv) stroke)
                     (assoc stroke :pts [(first (:pts stroke)) uv]))]
        (swap! gesture assoc :stroke stroke)
        (world/live-stroke! id stroke)))))

(defn- end-paint! [{:keys [id stroke]}]
  (let [strokes (:strokes (st/obj id))]
    (if (and (seq strokes) (empty? (:pts (peek strokes))))
      ;; the placeholder start-paint! put down to give a tile its canvas
      (st/update-obj! id assoc :strokes (conj (pop strokes) stroke))
      (act/add-stroke! id stroke))))

;; ── dragging ────────────────────────────────────────────────────────────────

(defn- start-drag! [ids ^js grab-point x y & [checkpointed?]]
  (reset! gesture {:type :drag :ids (vec ids) :x0 x :y0 y
                   :grab-h (.-y grab-point)
                   :grab (world/table-point x y (.-y grab-point))
                   :start (into {} (map (fn [id] [id (:pos (st/obj id))])) ids)
                   :moved? false :checkpointed? checkpointed?}))

(defn- clamp [v lo hi] (max lo (min hi v)))

(defn- continue-drag! [x y]
  (let [{:keys [ids grab grab-h start moved? x0 y0 checkpointed?]} @gesture]
    (when (or moved? (> (js/Math.hypot (- x x0) (- y y0)) 3))
      (when-not (or moved? checkpointed?) (st/checkpoint!))
      (swap! gesture assoc :moved? true)
      (when-let [p (and grab (world/table-point x y grab-h))]
        (let [dx (- (.-x p) (.-x grab)) dz (- (.-z p) (.-z grab))
              excl (set ids)
              hw (/ world/table-w 2) hd (/ world/table-d 2)
              moved (into {} (for [id ids :let [[sx _ sz] (start id)
                                                nx (clamp (+ sx dx) (- hw) hw)
                                                nz (clamp (+ sz dz) (- hd) hd)]]
                               [id [nx (+ (world/rest-y id nx nz excl) lift) nz]]))]
          (swap! st/doc update :objects
                 (fn [objs] (reduce-kv (fn [m id pos] (if (m id) (assoc-in m [id :pos] pos) m))
                                       objs moved))))))))

(defn- end-drag! [{:keys [ids moved?]}]
  (when moved?
    (act/settle! ids)
    (when (= 1 (count ids))
      (let [id (first ids) o (st/obj id)]
        (when (act/cards? o)
          (when-let [under (st/obj (world/beneath id))]
            (when (and (act/same-cards? o under) (not (:locked under)))
                (act/merge-cards! id (:id under)))))))))

;; ── box select ──────────────────────────────────────────────────────────────

(defn- end-box! [{:keys [x0 y0 x1 y1 click-id shift?]}]
  (let [moved? (and x1 (> (js/Math.hypot (- x1 x0) (- y1 y0)) 4))]
    (if moved?
      (let [[l r] (sort [x0 x1]) [t b] (sort [y0 y1])
            hit (for [[id o] (st/objects)
                      :when (not (:locked o))
                      :let [[sx sy] (world/screen-pos id)]
                      :when (and sx (<= l sx r) (<= t sy b))]
                  id)]
        (swap! st/ui update :selected #(into (if shift? % #{}) hit)))
      (swap! st/ui assoc :selected (if click-id #{click-id} (if shift? (:selected @st/ui) #{}))))
    (swap! st/ui dissoc :box)
    (world/sync!)))

;; ── pointer events ──────────────────────────────────────────────────────────

(defn on-pointer-down [^js e]
  (let [x (.-clientX e) y (.-clientY e)]
    ;; the keys belong to the table again, not the last box typed in
    (some-> js/document.activeElement .blur)
    (swap! st/ui assoc :menu nil)
    (case (.-button e)
      0 (when-not (and (= :paint (:mode @st/ui)) (start-paint! x y))
          (let [hit (world/pick x y)
                o (some-> hit :id st/obj)]
            (cond
              (or (nil? o) (:locked o))
              (reset! gesture {:type :box :x0 x :y0 y :click-id (:id o) :shift? (.-shiftKey e)})

              (and (.-altKey e) (act/cards? o) (> (count (:cards o)) 1))
              (do (st/checkpoint!)
                  (let [nid (act/take-top! (:id o))]
                    (world/sync!)
                    (swap! st/ui assoc :selected #{nid})
                    (start-drag! [nid] (:point hit) x y true)))

              :else
              (let [id (:id o)
                    sel (:selected @st/ui)
                    sel (cond (.-shiftKey e) (if (sel id) (disj sel id) (conj sel id))
                              (sel id) sel
                              :else #{id})]
                (swap! st/ui assoc :selected sel)
                (world/sync!)
                (when (sel id)
                  (start-drag! (remove #(:locked (st/obj %)) sel) (:point hit) x y))))))
      2 (reset! gesture {:type :right :x0 x :y0 y})
      nil)))

(defn on-pointer-move [^js e]
  (let [x (.-clientX e) y (.-clientY e)]
    (reset! mouse [x y])
    (case (:type @gesture)
      :drag (continue-drag! x y)
      :paint (continue-paint! x y)
      :box (do (swap! gesture assoc :x1 x :y1 y)
               (when (> (js/Math.hypot (- x (:x0 @gesture)) (- y (:y0 @gesture))) 4)
                 (swap! st/ui assoc :box (select-keys @gesture [:x0 :y0 :x1 :y1]))))
      :right nil
      (update-hover! x y))
    (when (and (.-altKey e) (nil? @gesture)) (swap! st/ui assoc :peek (hover-id)))))

(defn on-pointer-up [^js e]
  (let [g @gesture x (.-clientX e) y (.-clientY e)]
    (reset! gesture nil)
    (case (:type g)
      :drag (do (end-drag! g) (update-hover! x y))
      :paint (end-paint! g)
      :box (end-box! (assoc g :x1 x :y1 y))
      :right (when (< (js/Math.hypot (- x (:x0 g)) (- y (:y0 g))) 4)
               (let [id (:id (world/pick x y))]
                 (when (and id (not (contains? (:selected @st/ui) id)))
                   (swap! st/ui assoc :selected #{id}))
                 (world/sync!)
                 (swap! st/ui assoc :menu {:x x :y y :id id})))
      nil)))

(defn on-double-click [^js e]
  (when-let [{:keys [point]} (world/pick (.-clientX e) (.-clientY e))]
    (world/focus! point)))

;; ── keys ────────────────────────────────────────────────────────────────────

(defn- typing? [^js e]
  (let [t (.-target e)]
    (or (#{"INPUT" "TEXTAREA" "SELECT"} (.-tagName t)) (.-isContentEditable t))))

(defn on-key-down [^js e]
  (when-not (typing? e)
    (let [k (str/lower-case (.-key e))
          mod? (or (.-ctrlKey e) (.-metaKey e))
          shift? (.-shiftKey e)
          ts (targets)
          hit! (fn [f] (.preventDefault e) (f))]
      (swap! held-keys conj k)
      (cond
        (and mod? (= k "z")) (hit! (if shift? st/redo! st/undo!))
        (and mod? (= k "y")) (hit! st/redo!)
        (and mod? (= k "c")) (hit! #(act/copy! ts))
        (and mod? (= k "x")) (hit! #(do (act/copy! ts) (act/delete! ts)))
        (and mod? (= k "v")) (hit! #(let [p (cursor-point)] (act/paste! (.-x p) (.-z p))))
        (and mod? (= k "d")) (hit! #(act/duplicate! ts))
        (and mod? (= k "a")) (hit! #(swap! st/ui assoc :selected
                                           (set (keep (fn [[id o]] (when-not (:locked o) id)) (st/objects)))))
        mod? nil
        (= k "alt") (do (.preventDefault e) (swap! st/ui assoc :peek (hover-id)))
        (= k "escape") (do (swap! st/ui assoc :selected #{} :menu nil :help? false :mode :select :import nil)
                           (world/sync!))
        (#{"delete" "backspace"} k) (hit! #(act/delete! ts))
        (= k "f") (act/flip! ts)
        (= k "q") (act/rotate! ts (if shift? 90 15))
        (= k "e") (act/rotate! ts (if shift? -90 -15))
        (= k "r") (act/roll! ts)
        (= k "t") (act/tilt! ts)
        (= k "u") (act/upright! ts)
        (= k "l") (act/toggle-lock! ts)
        (= k "g") (act/gather! (:selected @st/ui))
        (= k "p") (swap! st/ui update :mode #(if (= % :paint) :select :paint))
        (= k "h") (swap! st/ui update :help? not)
        (= k "?") (swap! st/ui update :help? not)
        (re-matches #"[1-9]" k) (when-let [id (first ts)] (act/deal! id (js/parseInt k)))
        :else nil))))

(defn on-key-up [^js e]
  (let [k (str/lower-case (.-key e))]
    (swap! held-keys disj k)
    (when (= k "alt") (swap! st/ui assoc :peek nil))))

(defn tick
  "Every frame: slide the view while WASD / arrows are held."
  []
  (let [ks @held-keys]
    (when (seq ks)
      (let [dx (+ (if (or (ks "d") (ks "arrowright")) 1 0) (if (or (ks "a") (ks "arrowleft")) -1 0))
            dz (+ (if (or (ks "w") (ks "arrowup")) 1 0) (if (or (ks "s") (ks "arrowdown")) -1 0))]
        (when (or (not= 0 dx) (not= 0 dz))
          (world/pan! dx dz))))))
