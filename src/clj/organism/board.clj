(ns organism.board
  (:require [hiccup.core :as up]))

(def tau (* 2 Math/PI))

(def organism-colors
  [[:yellow "#fff88c"]
   [:red "#da6558"]
   [:blue "#849cd5"]
   [:orange "#febe48"]
   [:green "#a6cd7a"]
   [:purple "#9c6d8e"]
   [:grey "#3b545c"]])

(defn zero-dec
  [n]
  (let [lower (dec n)]
    (if (< lower 0)
      0
      lower)))

(defn circle
  [[x y radius color]]
  [:circle {:cx x :cy y :r radius :fill color}])

(defn circle-index
  [[x-axis y-axis] [x-offset y-offset] radius buffer color index]
  (let [x (+ x-offset (* x-axis index radius buffer))
        y (+ y-offset (* y-axis index radius buffer))]
    [x y radius color]))

(defn radial-axis
  [symmetry index]
  (let [ratio (/ index symmetry)
        unit (* ratio tau)
        x-axis (Math/cos unit)
        y-axis (Math/sin unit)]
    [x-axis y-axis]))

(defn beam
  [radius buffer colors axis]
  (let [rings (count colors)
        offset (* rings radius buffer)]
    (map
     (partial circle-index axis [offset offset] radius buffer)
     colors
     (range (count colors)))))

(defn find-beams
  [symmetry radius buffer colors]
  (let [axes (map
              (partial radial-axis symmetry)
              (range symmetry))]
    (map
     (partial beam radius buffer colors)
     axes)))

(defn linear-ring
  [[start-x start-y radius color] [end-x end-y] between]
  (let [total (inc between)
        spaces (reverse (drop 1 (range total)))]
    (map
     (fn [space]
       (let [ratio (/ space total)
             inverse (- 1 ratio)]
         [(+ (* start-x ratio) (* end-x inverse))
          (+ (* start-y ratio) (* end-y inverse))
          radius
          color]))
     spaces)))

(defn linear-rings
  [start-beam end-beam]
  (let [between (map zero-dec (range (count start-beam)))]
    (concat
     start-beam
     (mapcat linear-ring start-beam end-beam between))))

(defn find-rings
  [symmetry radius buffer colors]
  (let [beams (find-beams symmetry radius buffer colors)
        next-beams (drop 1 (cycle beams))
        rings (mapcat linear-rings beams next-beams)]
    rings))

(defn ring-map
  [symmetry radius buffer colors]
  (let [spaces (find-rings symmetry radius buffer (map last colors))
        inverse-colors (into {} (map (fn [[a b]] [b a]) colors))]
    (reduce
     (fn [rings space]
       (let [hex (last space)
             color (get inverse-colors hex)]
         (if (get rings color)
           (update rings color conj space)
           (assoc rings color [space]))))
     {} spaces)))

(defn rings->locations
  [rings]
  (reduce
   (fn [locations [color spaces]]
     (reduce
      (fn [locations [space n]]
        (assoc locations [color n] space))
      locations (map vector spaces (range))))
   {} rings))

(defn board-locations
  [symmetry radius buffer colors]
  (let [rings (ring-map symmetry radius buffer colors)]
    (rings->locations rings)))

(defn layout
  [symmetry radius buffer colors]
  (let [field (* 2 radius buffer (count colors))]
    [:svg {:width field :height field}
     [:g (map circle (find-rings symmetry radius buffer colors))]]))

(defn render
  [symmetry radius buffer colors]
  (let [radiate (layout symmetry radius buffer colors)]
    (up/html radiate)))

;; (defn render-locations
;;   [locations]
;;   (let [radiate (layout symmetry radius buffer colors)]
;;     (up/html radiate)))

(defn export
  [symmetry radius buffer colors path]
  (let [radiate (layout symmetry radius buffer colors)
        out (up/html radiate)]
    (spit path out)))

