(ns table.patch
  "Changes to a shared table, as data -- the one thing the server and every
   browser at the table must agree on, so there is one copy of it.

   A table is {id object} (see table.state for objects). A patch is

     {:remove #{id}               gone
      :put    {id object}         new, or changed in a way that isn't below
      :move   {id [pos rot]}      only picked up and put down -- most traffic
      :stroke {id [stroke ...]}   drawn on: strokes added to the end
      :trim   {id n}              drawing undone: keep the first n strokes}

   applied in that order. Moves and strokes are split out because they are
   nearly everything that happens and an object can be large (a deck, a board
   with a long drawing on it), while the site's WebSocket frames are capped
   at 64 KB.")

(defn apply-patch
  "objects with patch p applied. Moves, strokes and trims of objects that
   aren't there are ignored: someone deleted it first."
  [objects {:keys [remove put move stroke trim]}]
  (as-> objects m
    (apply dissoc m remove)
    (merge m put)
    (reduce-kv (fn [m id [pos rot]]
                 (if (contains? m id) (update m id assoc :pos pos :rot rot) m))
               m move)
    (reduce-kv (fn [m id strokes]
                 (if (contains? m id) (update-in m [id :strokes] (fnil into []) strokes) m))
               m stroke)
    (reduce-kv (fn [m id n]
                 (cond (not (contains? m id)) m
                       (pos? n) (update-in m [id :strokes] #(subvec (vec %) 0 (min n (count %))))
                       :else (update m id dissoc :strokes)))
               m trim)))

(defn- prefix? [a b]
  (and (<= (count a) (count b)) (= a (subvec b 0 (count a)))))

(defn diff
  "The patch that takes table a to table b."
  [a b]
  (let [removed (into #{} (remove #(contains? b %)) (keys a))
        p (reduce-kv
           (fn [p id o2]
             (let [o1 (get a id)]
               (cond
                 (identical? o1 o2) p
                 (nil? o1) (assoc-in p [:put id] o2)
                 :else
                 (let [rest1 (dissoc o1 :pos :rot :strokes)
                       rest2 (dissoc o2 :pos :rot :strokes)
                       s1 (vec (:strokes o1)) s2 (vec (:strokes o2))
                       moved? (or (not= (:pos o1) (:pos o2)) (not= (:rot o1) (:rot o2)))]
                   (cond
                     (not= rest1 rest2) (assoc-in p [:put id] o2)
                     (= s1 s2) (if moved? (assoc-in p [:move id] [(:pos o2) (:rot o2)]) p)
                     (prefix? s1 s2) (cond-> (assoc-in p [:stroke id] (subvec s2 (count s1)))
                                       moved? (assoc-in [:move id] [(:pos o2) (:rot o2)]))
                     (prefix? s2 s1) (cond-> (assoc-in p [:trim id] (count s2))
                                       moved? (assoc-in [:move id] [(:pos o2) (:rot o2)]))
                     :else (assoc-in p [:put id] o2))))))
           {} b)]
    (cond-> p (seq removed) (assoc :remove removed))))

(defn empty-patch? [p]
  (every? empty? (vals p)))

(defn ids
  "Every object a patch touches."
  [p]
  (into (set (:remove p)) (mapcat keys) (vals (dissoc p :remove))))

(defn only
  "The part of patch p that touches ids satisfying keep?."
  [p keep?]
  (into {} (keep (fn [[k v]]
                   (let [v (if (= k :remove)
                             (into #{} (filter keep?) v)
                             (into {} (filter (comp keep? key)) v))]
                     (when (seq v) [k v]))))
        p))

(defn split
  "p cut into patches whose printed size stays under `limit` characters,
   object by object. One object bigger than that still goes alone."
  [p limit]
  (if (< (count (pr-str p)) limit)
    [p]
    (let [entries (for [[k v] p
                        e (if (= k :remove) (map (fn [id] [k id]) v) v)]
                    (if (= k :remove) e [k e]))
          add (fn [patch [k e]]
                (if (= k :remove)
                  (update patch :remove (fnil conj #{}) e)
                  (assoc-in patch [k (key e)] (val e))))
          size (fn [[k e]] (count (pr-str (if (= k :remove) e (val e)))))]
      (loop [[e & more] entries cur {} n 0 out []]
        (cond
          (nil? e) (if (seq cur) (conj out cur) out)
          (and (seq cur) (> (+ n (size e)) limit)) (recur more (add {} e) (size e) (conj out cur))
          :else (recur more (add cur e) (+ n (size e)) out))))))
