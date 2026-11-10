(ns table.actions
  "Everything a player can do to the things on the table. Each action takes one
   checkpoint first, so it is one undo step."
  (:require ["three" :as THREE]
            [table.mesh :as mesh]
            [table.state :as st]
            [table.world :as world]))

;; ── turning ─────────────────────────────────────────────────────────────────

(def identity-rot [0 0 0 1])

(defn- quat [o] (let [[x y z w] (or (:rot o) identity-rot)] (THREE/Quaternion. x y z w)))
(defn- rot-of [^js q] [(.-x q) (.-y q) (.-z q) (.-w q)])
(defn- axis-quat [x y z a] (.setFromAxisAngle (THREE/Quaternion.) (THREE/Vector3. x y z) a))

(defn flipped?
  "Is o upside down -- its own top facing the table?"
  [o]
  (neg? (.-y (.applyQuaternion (THREE/Vector3. 0 1 0) (quat o)))))

;; turned over end to end, like a page, so a back with writing on it reads the right way up
(def face-down-rot (rot-of (axis-quat 1 0 0 js/Math.PI)))

(defn- turned [o local? ^js q]
  (assoc o :rot (rot-of (if local? (.multiply (quat o) q) (.premultiply (quat o) q)))))

;; ── resting ─────────────────────────────────────────────────────────────────

(defn settle!
  "Drop ids onto whatever is under each, lowest first, so a stack lands as one;
   then lift onto them any small things they were put down over."
  ([ids] (settle! ids 0))
  ([ids depth]
   (let [ids (sort-by #(get-in (st/obj %) [:pos 1]) (filter st/obj ids))]
     (loop [[id & more] ids]
       (when id
         (let [{[x _ z] :pos} (st/obj id)]
           (st/update-obj! id assoc-in [:pos 1] (world/rest-y id x z more))
           (recur more))))
     (when (< depth 3)
       (let [riders (distinct (mapcat #(world/riders % (set ids)) ids))]
         (when (seq riders) (settle! riders (inc depth))))))))

;; ── cards ───────────────────────────────────────────────────────────────────

(defn world-cards
  "o's cards bottom to top as they lie on the table, :down meaning face down."
  [o]
  (let [f (flipped? o)]
    (mapv #(assoc % :down (not= (boolean (:down %)) f))
          (if f (rseq (:cards o)) (:cards o)))))

(defn- local-cards
  "world-cards, put back into the frame of a stack that is (or isn't) flipped."
  [cs flipped]
  (mapv #(assoc % :down (not= (boolean (:down %)) flipped))
        (if flipped (vec (rseq (vec cs))) (vec cs))))

(defn cards? [o] (= "cards" (:kind o)))

(defn same-cards? [a b]
  (and (cards? a) (cards? b) (= (:shape a) (:shape b))
       (every? #(< (js/Math.abs %) 1.5) (map - (:size a) (:size b)))))

(defn merge-cards!
  "Put stack `src` on top of stack `dst`."
  [src-id dst-id]
  (let [s (st/obj src-id) d (st/obj dst-id)
        cs (into (world-cards d) (world-cards s))]
    (st/remove-objs! [src-id])
    (st/put! (assoc d :cards (local-cards cs (flipped? d))))
    (settle! [dst-id])
    (swap! st/ui assoc :selected #{dst-id})))

(defn take-top!
  "Lift the top card off stack id as an object of its own; returns its id."
  [id & [pos]]
  (let [o (st/obj id) cs (world-cards o)]
    (if (< (count cs) 2)
      id
      (let [f (flipped? o)
            nid (st/new-id)
            [x y z] (:pos o)]
        (st/put! (assoc o :cards (local-cards (pop cs) f)))
        (st/put! (assoc o :id nid :cards (local-cards [(peek cs)] f) :label (:name (peek cs))
                        :locked false
                        :pos (or pos [x (+ y (mesh/card-thickness (count cs))) z])))
        nid))))

(defn deal!
  "Deal n cards off stack id into a row beside it."
  [id n]
  (when-let [o (st/obj id)]
    (when (cards? o)
      (st/checkpoint!)
      (let [[w] (:size o) [x _ z] (:pos o)
            ids (doall (for [i (range (min n (dec (count (:cards o)))))]
                         (take-top! id [(+ x (* (+ w 6) (inc i))) 0 z])))]
        (settle! ids)))))

(defn shuffle! [id]
  (st/update-obj! id update :cards (comp vec shuffle)))

(defn spread!
  "Lay every card of stack id out in rows."
  [id]
  (when-let [o (st/obj id)]
    (st/checkpoint!)
    (let [[w l] (:size o) [x _ z] (:pos o)
          cs (rseq (world-cards o))
          f (flipped? o)
          per 13
          ids (doall (for [[i c] (map-indexed vector cs)]
                       (let [nid (st/new-id)]
                         (st/put! (assoc o :id nid :cards (local-cards [c] f) :label (:name c)
                                         :pos [(+ x (* (+ w 5) (mod i per))) 0
                                               (+ z (* (+ l 8) (quot i per)))]))
                         nid)))]
      (st/remove-objs! [id])
      (settle! ids)
      (swap! st/ui assoc :selected (set ids)))))

(defn gather!
  "Stack every selected card object into one, the lowest at the bottom."
  [ids]
  (let [objs (->> ids (keep st/obj) (filter cards?) (sort-by #(get-in % [:pos 1])))]
    (when (> (count objs) 1)
      (st/checkpoint!)
      (let [base (first objs)
            cs (vec (mapcat world-cards objs))]
        (st/remove-objs! (map :id (rest objs)))
        (st/put! (assoc base :cards (local-cards cs (flipped? base))))
        (settle! [(:id base)])
        (swap! st/ui assoc :selected #{(:id base)})))))

;; ── everything ──────────────────────────────────────────────────────────────

(defn flip! [ids]
  (st/checkpoint!)
  (doseq [id ids] (st/update-obj! id turned true (axis-quat 1 0 0 js/Math.PI)))
  (settle! ids))

(defn rotate! [ids degrees]
  (st/checkpoint! :rotate)
  (doseq [id ids] (st/update-obj! id turned false (axis-quat 0 1 0 (* degrees (/ js/Math.PI 180)))))
  (settle! ids))

(defn tilt! [ids]
  (st/checkpoint!)
  (doseq [id ids] (st/update-obj! id turned true (axis-quat 1 0 0 (/ js/Math.PI 2))))
  (settle! ids))

(defn upright! [ids]
  (st/checkpoint!)
  (doseq [id ids] (st/update-obj! id assoc :rot identity-rot))
  (settle! ids))

(def ^:private die-ups
  "Turns that bring each face of a die up."
  [[1 0 0 0] [1 0 0 js/Math.PI] [0 0 1 (/ js/Math.PI 2)] [0 0 1 (- (/ js/Math.PI 2))]
   [1 0 0 (/ js/Math.PI 2)] [1 0 0 (- (/ js/Math.PI 2))]])

(defn roll!
  "Dice land on a random face; stacks of cards are shuffled."
  [ids]
  (st/checkpoint!)
  (doseq [id ids]
    (let [o (st/obj id)]
      (cond
        (= "d6" (:shape o))
        (let [[x y z a] (rand-nth die-ups)
              q (.premultiply (axis-quat x y z a) (axis-quat 0 1 0 (* 2 js/Math.PI (rand))))]
          (st/update-obj! id assoc :rot (rot-of q)))
        (cards? o) (shuffle! id))))
  (settle! ids)
  (st/status! (if (some #(cards? (st/obj %)) ids) "Shuffled" "Rolled")))

(defn delete! [ids]
  (when (seq ids)
    (st/checkpoint!)
    (st/remove-objs! ids)))

(defn toggle-lock! [ids]
  (st/checkpoint!)
  (let [lock? (not (every? #(:locked (st/obj %)) ids))]
    (doseq [id ids] (st/update-obj! id assoc :locked lock?))
    (st/status! (if lock? "Locked in place" "Unlocked"))))

(defn set-prop! [ids k v]
  (st/checkpoint! [:prop k])
  (doseq [id ids] (st/update-obj! id assoc k v)))

(defn set-size!
  "Set one axis of :size (or :scale for models), then let it rest again."
  [ids axis v]
  (st/checkpoint! [:size axis])
  (doseq [id ids]
    (let [o (st/obj id)]
      (if (= "model" (:kind o))
        (st/update-obj! id assoc-in [:scale axis] v)
        (st/update-obj! id assoc-in [:size axis] v))))
  (settle! ids))

;; ── making things ───────────────────────────────────────────────────────────

(defn- place-new!
  "Put o (no :id yet) at x z -- or, `free?`, the nearest clear spot to it --
   resting on what's there. Returns its id."
  [o x z & [free?]]
  (let [id (st/new-id)]
    (st/put! (merge {:rot identity-rot} o {:id id :pos [x 0 z]}))
    (when free?
      (let [[fx fz] (world/free-spot id x z)]
        (st/update-obj! id assoc :pos [fx 0 fz])))
    (settle! [id])
    id))

(defn item->obj
  "A library item as a table object (still without :id or :pos)."
  [item]
  (let [base (select-keys item [:label :color])]
    (case (:kind item)
      "shape" (merge base (select-keys item [:shape :size]) {:kind "shape"})
      "model" (merge base {:kind "model" :model (:model item) :z-up (boolean (:zUp item))
                           :model-kind (:model-kind item)
                           :base-scale (or (:scale item) 1) :scale [1 1 1]})
      "tile" (merge base (select-keys item [:shape :size :face :back :edge]) {:kind "tile"})
      "board" (merge base (select-keys item [:shape :size :bg :grid :image :edge :back :strokes])
                     {:kind "board"})
      "cards" (merge base {:kind "cards" :shape (or (:shape item) "rect") :size (:size item)
                           :cards (mapv #(select-keys % [:name :face :back]) (:cards item))
                           :rot face-down-rot})
      ;; a saved component: already an object
      "object" (dissoc (:object item) :id :pos)
      nil)))

(defn- ready
  "A promise that resolves when o can be built at its true size."
  [o]
  (if (= "model" (:kind o))
    (mesh/model-ready o)
    (js/Promise.resolve o)))

(defn spawn!
  "Put a library item on the table at x z -- or, given none, at the clear spot
   nearest the middle of the view."
  ([item] (let [[x z] (world/view-centre)] (spawn! item x z true)))
  ([item x z] (spawn! item x z false))
  ([item x z free?]
   (st/checkpoint!)
   (if (= "set" (:kind item))
     (let [items (:items item)
           n (count items)
           cols (js/Math.ceil (js/Math.sqrt n))
           step (+ 6 (apply max (map #(apply max (:size %)) items)))
           ids (doall (for [[i it] (map-indexed vector items)]
                        (place-new! (item->obj it)
                                    (+ x (* step (- (mod i cols) (/ (dec cols) 2))))
                                    (+ z (* step (- (quot i cols) (/ (dec cols) 2))))
                                    free?)))]
       (swap! st/ui assoc :selected (set ids))
       (js/Promise.resolve ids))
     (let [o (item->obj item)]
       (when o
         (-> (ready o)
             (.then (fn [_]
                      (let [id (place-new! o x z free?)]
                        (swap! st/ui assoc :selected #{id})
                        [id])))))))))

(defn- fresh-copies
  "Copies of objs with new ids, moved by dx dz."
  [objs dx dz]
  (for [o objs]
    (let [[x y z] (:pos o)]
      (assoc o :id (st/new-id) :pos [(+ x dx) (+ y 400) (+ z dz)] :locked false))))

(defn- put-all! [objs]
  (doseq [o objs] (st/put! o))
  (let [ids (map :id objs)]
    (-> (js/Promise.all (clj->js (map ready objs)))
        (.then (fn [_]
                 (world/sync!)
                 (settle! ids)
                 (swap! st/ui assoc :selected (set ids)))))))

(defn duplicate! [ids]
  (let [objs (keep st/obj ids)]
    (when (seq objs)
      (st/checkpoint!)
      (let [b (reduce (fn [^js b id] (if-let [bb (world/bounds id)] (.union b bb) b))
                      (THREE/Box3.) ids)
            w (- (.. b -max -x) (.. b -min -x))]
        (put-all! (fresh-copies objs (+ w 10) 0))))))

(defn copy! [ids]
  (when (seq ids)
    (swap! st/ui assoc :clipboard (vec (keep st/obj ids)))
    (st/status! (str "Copied " (count ids)))))

(defn paste! [x z]
  (when-let [objs (seq (:clipboard @st/ui))]
    (st/checkpoint!)
    (let [cx (/ (reduce + (map #(get-in % [:pos 0]) objs)) (count objs))
          cz (/ (reduce + (map #(get-in % [:pos 2]) objs)) (count objs))]
      (put-all! (fresh-copies objs (- x cx) (- z cz))))))

(defn add-objects!
  "Put a list of saved objects (from a file or a saved component) on the table
   around x z, keeping their arrangement."
  [objs x z]
  (st/checkpoint!)
  (let [objs (vec objs)
        cx (/ (reduce + (map #(get-in % [:pos 0] 0) objs)) (max 1 (count objs)))
        cz (/ (reduce + (map #(get-in % [:pos 2] 0) objs)) (max 1 (count objs)))]
    (put-all! (fresh-copies (map #(merge {:pos [0 0 0] :rot identity-rot} %) objs)
                            (- x cx) (- z cz)))))

(defn clear-drawing! [ids]
  (st/checkpoint!)
  (doseq [id ids] (st/update-obj! id dissoc :strokes)))

(defn add-stroke! [id stroke]
  (st/checkpoint!)
  (st/update-obj! id update :strokes (fnil conj []) stroke))
