(ns table.state
  "What is on the table, and the undo history of it.

   `doc` holds {:objects {id obj}}: plain data, the whole table. Every object
   has :id :kind :pos [x y z] (its centre, mm, Y up) :rot [x y z w] (a
   quaternion) and whatever its kind needs:

     \"shape\"  :shape (cube cylinder disk meeple pawn sphere cone hex tri d6)
                :size [w h d] :color
     \"model\"  :model url :z-up :scale [sx sy sz] :base-scale :color
     \"tile\"   :shape (rect round) :size [w h d] :face url :back url :edge
     \"board\"  a tile you draw on: :bg :grid {:type :cell :color} :image
     \"cards\"  :shape :size [w l] :cards [{:name :face :back :down}], index 0
                the bottom of the stack in the object's own frame

   Tiles and boards both keep :strokes, the drawing on their top face, as
   data -- so drawing is undone, saved and redrawn at any size like the rest.

   Undo is cheap because the doc is persistent: a checkpoint is the old map."
  (:require [clojure.string :as str]
            [reagent.core :as r]))

(defonce doc (r/atom {:objects {}}))

;; everything about the session that is not the table itself
(defonce ui (r/atom {:selected #{}
                   :hover nil
                   :mode :select              ; or :paint
                   :tool :pen
                   :brush {:color "#1d1d1d" :size 2.0 :alpha 1.0 :fill false}
                   :grid? true
                   :library-open? true
                   :help? false
                   :menu nil                  ; {:x :y :id}
                   :peek nil
                   :clipboard nil
                   :status nil}))

(defonce history (atom {:past [] :future [] :tag nil :at 0}))

(defn new-id []
  (subs (str/replace (str (random-uuid)) "-" "") 0 12))

(defn objects [] (:objects @doc))
(defn obj [id] (get-in @doc [:objects id]))

(defn checkpoint!
  "Remember the table as it is, before a change. Repeated changes with the same
   tag inside a second and a half -- dragging a colour picker, typing a size --
   fold into one undo step."
  ([] (checkpoint! nil))
  ([tag]
   (let [now (js/Date.now)
         {:keys [past] :as h} @history
         cur (:objects @doc)]
     (when-not (or (and tag (= tag (:tag h)) (< (- now (:at h)) 1500))
                   (identical? cur (peek past)))
       (swap! history (fn [h] {:past (conj (if (> (count (:past h)) 200)
                                             (subvec (:past h) 1)
                                             (:past h))
                                           cur)
                               :future [] :tag tag :at now})))
     (when tag (swap! history assoc :tag tag :at now)))))

(defn undo! []
  (let [{:keys [past]} @history]
    (when (seq past)
      (let [cur (:objects @doc)]
        (swap! history (fn [h] (-> h (update :past pop) (update :future conj cur) (assoc :tag nil))))
        (swap! doc assoc :objects (peek past))
        (swap! ui update :selected #(set (filter (:objects @doc) %)))))))

(defn redo! []
  (let [{:keys [future]} @history]
    (when (seq future)
      (let [cur (:objects @doc)]
        (swap! history (fn [h] (-> h (update :future pop) (update :past conj cur) (assoc :tag nil))))
        (swap! doc assoc :objects (peek future))))))

(defn put! [o] (swap! doc assoc-in [:objects (:id o)] o))
(defn update-obj! [id f & args]
  (swap! doc (fn [d] (if (get-in d [:objects id]) (apply update-in d [:objects id] f args) d))))
(defn remove-objs! [ids]
  (swap! doc update :objects #(apply dissoc % ids))
  (swap! ui update :selected #(reduce disj % ids)))

(defn selected-objs []
  (keep obj (:selected @ui)))

(defn status! [msg]
  (swap! ui assoc :status msg)
  (js/setTimeout #(swap! ui (fn [u] (if (= (:status u) msg) (assoc u :status nil) u))) 2500))

(defn rebase-history!
  "Someone else changed the table: change every undo snapshot the same way,
   so undoing takes back only what this player did."
  [f]
  (swap! history (fn [h] (-> h (update :past #(mapv f %)) (update :future #(mapv f %))))))
