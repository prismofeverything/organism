(ns organism.board
  (:require
   [clojure.string :as string]
   [hiccup.core :as up]
   [organism.base :as base]))

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
  [:circle
   {:cx x
    :cy y
    :r radius
    ;; :stroke "#888"
    ;; :stroke-width (* radius 0.07)
    :fill color}])

(defn circle-index
  [[x-axis y-axis] [x-offset y-offset] radius buffer color index]
  (let [x (+ x-offset (* x-axis index radius buffer))
        y (+ y-offset (* y-axis index radius buffer))]
    [x y radius color]))

(defn radial-axis
  [symmetry radius phase index]
  (let [ratio (/ index symmetry)
        unit (+ (* ratio tau) phase)
        x-axis (* radius (Math/cos unit))
        y-axis (* radius (Math/sin unit))]
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
              (partial radial-axis symmetry 1 0)
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

(defn board-layout
  [symmetry radius buffer colors]
  (println "layout colors" colors)
  (let [field (* 2 radius buffer (count colors))]
    [:svg {:width field :height field}
     [:g (map circle (find-rings symmetry radius buffer colors))]]))

(defn render
  [symmetry radius buffer colors]
  (let [radiate (board-layout symmetry radius buffer colors)]
    (up/html radiate)))

(defrecord Board [symmetry radius buffer colors layout locations player-colors])

(defn build-board
  [symmetry radius buffer colors players]
  (Board.
   symmetry
   radius
   buffer
   colors
   (board-layout symmetry radius buffer (map last colors))
   (board-locations symmetry radius buffer colors)
   (into {} (map vector players (rest (reverse (map last colors)))))))

(defn add-vector
  [a b]
  (mapv + a b))

(defn render-eat
  [color [x y] radius food]
  (let [outer (map (partial radial-axis 5 radius 0) (range 5))
        inner (map (partial radial-axis 5 (* 0.5 radius) (* tau 0.1)) (range 5))
        points (mapv (partial add-vector [x y]) (interleave outer inner))
        head (first points)
        steps ["M " (first head) " " (last head)]
        steps (concat
               steps
               (map
                (fn [[ox oy]]
                  (str "L " ox " " oy))
                (rest points)))
        path (string/join " " steps)]
    [:path
     {:d (str path " Z")
      :fill color
      :stroke "white"
      :stroke-width (* radius 0.07)}]))

(defn render-grow
  [color [x y] radius food]
  (let [outer (map (partial radial-axis 4 radius 0) (range 4))
        inner (map (partial radial-axis 4 (* 0.5 radius) (* tau 0.1)) (range 4))
        points (mapv (partial add-vector [x y]) (interleave outer inner))
        head (first points)
        steps ["M " (first head) " " (last head)]
        steps (concat
               steps
               (map
                (fn [[ox oy]]
                  (str "L " ox " " oy))
                (rest points)))
        path (string/join " " steps)]
    [:path
     {:d (str path " Z")
      :fill color
      :stroke "white"
      :stroke-width (* radius 0.07)}]))

(defn render-move
  [color [x y] radius food]
  (let [outer (map (partial radial-axis 3 radius 0) (range 3))
        inner (map (partial radial-axis 3 (* 0.3 radius) (* tau 0.1)) (range 3))
        points (mapv (partial add-vector [x y]) (interleave outer inner))
        head (first points)
        steps ["M " (first head) " " (last head)]
        steps (concat
               steps
               (map
                (fn [[ox oy]]
                  (str "L " ox " " oy))
                (rest points)))
        path (string/join " " steps)]
    [:path
     {:d (str path " Z")
      :fill color
      :stroke "white"
      :stroke-width (* radius 0.07)}]))

(defn render-element
  [color [x y] radius element]
  (condp = (:type element)
    :eat (render-eat color [x y] radius (:food element))
    :grow (render-grow color [x y] radius (:food element))
    :move (render-move color [x y] radius (:food element))
    [:circle
     {:cx x
      :cy y
      :r (* radius 0.8)
      :fill color
      :stroke "white"
      :stroke-width (* radius 0.07)}]))

(defn render-organism
  [locations color radius spaces]
  (map
   (fn [{:keys [space element]}]
     (let [location (get locations space)]
       (render-element color location radius element)))
   spaces))

(defn render-game
  [{:keys [colors radius layout locations player-colors] :as board} game]
  (let [element-spaces (filter :element (vals (:spaces game)))
        _ (println "element spaces" element-spaces)
        organisms (group-by
                   (juxt
                    (comp :player :element)
                    (comp :organism :element))
                   element-spaces)
        _ (println "organisms" organisms)
        elements (mapv
                  (fn [[[player organism] spaces]]
                    [:g
                     (let [color (get player-colors player)]
                       (render-organism locations color radius spaces))])
                  organisms)
        _ (println "elements" elements)
        svg (apply conj layout elements)]
    _ (println "svg" svg)
    (up/html svg)))

(defn export
  [symmetry radius buffer colors path]
  (let [radiate (board-layout symmetry radius buffer colors)
        out (up/html radiate)]
    (spit path out)))

