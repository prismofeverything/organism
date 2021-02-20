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

(defn circle
  [[x y radius color]]
  [:circle
   {:cx x
    :cy y
    :r radius
    ;; :stroke "#888"
    ;; :stroke-width (* radius 0.07)
    :fill color}])

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

(defn curve?
  [point]
  (= 3 (count point)))

(defn make-path
  [color points]
  (let [first-point (first points)
        rest-points (concat (rest points) [first-point])
        starting-point (if (curve? first-point)
                         (last first-point)
                         first-point)
        first-step (concat ["M"] starting-point)
        steps (base/map-cat
               (fn [point]
                 (if (curve? point)
                   (let [[[sx sy] [ex ey] [px py]] point]
                     ["C" (str sx "," sy) (str ex "," ey) (str px "," py)])
                   (let [[x y] point]
                     ["L" (str x "," y)])))
               rest-points)
        all-steps (concat first-step steps ["Z"])]
    [:path
     {:d (string/join " " all-steps)}]))

(defn line
  [[x1 y1] [x2 y2] color]
  [:line
   {:x1 x1
    :y1 y1
    :x2 x2
    :y2 y2
    :stroke color}])

(defn make-circle
  [radius color [cx cy]]
  (circle [cx cy radius color]))

(defn make-control
  [radius point-color line-color previous control]
  (let [[_ _ previous-point] previous
        [start end point] control
        points (map
                (partial make-circle radius)
                [line-color line-color point-color]
                control)
        lines (list
               (line start previous-point line-color)
               (line end point line-color))]
    [lines points]))

(defn make-controls
  [radius point-color line-color controls]
  (let [control-points
        (map
         (partial make-control radius point-color line-color)
         controls
         (drop 1 (cycle controls)))

        [lines points]
        (reduce
         (fn [[lines points] [new-lines new-points]]
           [(concat lines new-lines)
            (concat points new-points)])
         [[] []] control-points)]
    [:g lines points]))

(defn render-eat
  [color [x y] radius food]
  (let [outer-radius 1.00
        outer-arc 0.03
        inner-radius 0.5
        inner-arc 0.12
        oc-start (map (partial radial-axis 5 (* radius (+ inner-radius 0.1)) (* tau (- 0.1 inner-arc))) (range 5))
        oc-end (map (partial radial-axis 5 (* radius outer-radius) (* tau -1 outer-arc)) (range 5))
        outer (map (partial radial-axis 5 radius 0) (range 5))
        ic-start (map (partial radial-axis 5 (* radius outer-radius) (* tau outer-arc)) (range 5))
        ic-end (map (partial radial-axis 5 (* radius (+ inner-radius 0.1)) (* tau (+ -0.1 inner-arc))) (range 5))
        inner (map (partial radial-axis 5 (* inner-radius radius) (* tau 0.1)) (range 5))
        points (mapv
                (partial add-vector [x y])
                (interleave
                 oc-start oc-end outer ic-start ic-end inner))
        path (make-path color (partition 3 points))
        controls (make-controls 5 "black" "grey" (partition 3 points))]
    [:g
     (update
      path 1 merge
      {:fill color
       :stroke "white"
       :stroke-width (* radius 0.07)})
     ;; controls
     ]))

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

