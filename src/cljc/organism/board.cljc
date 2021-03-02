(ns organism.board
  (:require
   [clojure.string :as string]
   [clojure.pprint :refer (pprint)]
   #?(:clj [hiccup.core :as up])
   [thi.ng.color.core :as color]
   [organism.base :as base]))

(def tau (* 2 Math/PI))

(defn generate-colors
  [rings]
  (let [num-rings (count rings)
        base 0.1
        max 0.9
        factor (/ (- max base) num-rings)
        jump-base 0.2
        jump-factor 0.6]
    (first
     (reduce
      (fn [[colors hue] [ring index]]
        [(conj
          colors
          [ring
           (-> (color/hsla
                hue
                (- max (+ base (rand (- max base))))
                (- max (* index factor))
                1.0)
               color/as-css
               :col)])
         (mod (+ hue (+ jump-base (- (rand (* 2 jump-factor)) jump-factor))) 1.0)])
      [[] (rand)]
      (map vector rings (range))))))

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
  [notch? start-beam end-beam]
  (let [between (map zero-dec (range (count start-beam)))]
    (concat
     (if notch?
       (butlast start-beam)
       start-beam)
     (mapcat linear-ring start-beam end-beam between))))

(defn find-rings
  [symmetry radius buffer colors notches?]
  (let [beams (find-beams symmetry radius buffer colors)
        next-beams (drop 1 (cycle beams))
        rings (mapcat
               (partial linear-rings notches?)
               beams
               next-beams)]
    rings))

(defn ring-map
  [symmetry radius buffer colors notches?]
  (let [spaces (find-rings symmetry radius buffer (map last colors) notches?)
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
  (let [rings (ring-map symmetry radius buffer colors false)]
    (rings->locations rings)))

(defn circle
  [[x y radius color]]
  [:circle
   {:cx x
    :cy y
    :r radius
    :fill color}])

(defn make-circle
  [radius color [cx cy]]
  (circle [cx cy radius color]))

(defn build-background
  [radius buffer colors]
  (let [num-rings (count colors)
        field (* radius buffer num-rings)
        center [field field]]
    [(concat
      [:defs]
      (apply
       concat
       (map
        (fn [[color-key color] index]
          [[:radialGradient {:id color-key}
            [:stop {:offset "0%" :stop-color "black"}]
            [:stop {:offset "100%" :stop-color color}]]
           [:radialGradient {:id (str (name color-key) "-element")}
            [:stop {:offset "0%" :stop-color color}]
            [:stop {:offset "50%" :stop-color color}]
            [:stop {:offset "100%" :stop-color "black"}]]])
        (rest colors)
        (map (comp inc inc) (range)))))
     [:g
      (concat
       [(make-circle (* field 0.93) "black" center)]
       (map
        (fn [[color-key color] index]
          (circle
           [field field
            (* 1.85 (- (+ num-rings 1) index) (+ radius buffer))
            (str "url(#" (name color-key) ")")]))
        (reverse (rest colors))
        (map (partial + 1.9) (range))))]]))

(defn board-layout
  [symmetry radius buffer colors notches?]
  (let [field (* 2 radius buffer (count colors))]
    [:svg {:width field :height field}
     (concat
      [:g]
      (build-background radius buffer colors)
      [(map circle (find-rings symmetry radius buffer (map last colors) notches?))])]))

(defrecord Board [symmetry radius buffer colors layout locations player-colors])

(defn build-board
  [symmetry radius buffer colors players notches?]
  (let [num-rings (count colors)
        outer-color (-> colors last first)
        notches (map (fn [n] [outer-color (* n (dec num-rings))]) (range symmetry))
        locations (board-locations symmetry radius buffer colors)
        locations (if notches?
                    (reduce
                     (fn [locations notch]
                       (dissoc locations notch))
                     locations
                     notches)
                    locations)]
    (Board.
     symmetry
     radius
     buffer
     colors
     (board-layout symmetry radius buffer colors notches?)
     locations
     (into
      {}
      (map
       vector
       players
       (reverse
        (rest
         (map
          (fn [[color-key color-str]]
            color-str)
          ;; (str "url(#" (name color-key) "-element)")
          colors))))))))

(defn get-ring
  [board color]
  (let [spaces (filter
                (fn [[space-color number]]
                  (= color space-color))
                (-> board :locations keys))]))

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
  [color stroke-ratio [x y] radius food]
  (let [symmetry 5
        half (/ 0.5 symmetry)
        outer-radius 1.00
        outer-arc 0.03
        inner-radius 0.5
        inner-arc 0.12

        oc-start (map (partial radial-axis symmetry (* radius (+ inner-radius half)) (* tau -1 (- inner-arc half))) (range symmetry))
        oc-end (map (partial radial-axis symmetry (* radius outer-radius) (* tau -1 outer-arc)) (range symmetry))
        outer (map (partial radial-axis symmetry radius 0) (range symmetry))
        ic-start (map (partial radial-axis symmetry (* radius outer-radius) (* tau outer-arc)) (range symmetry))
        ic-end (map (partial radial-axis symmetry (* radius (+ inner-radius half)) (* tau (- inner-arc half))) (range symmetry))
        inner (map (partial radial-axis symmetry (* inner-radius radius) (* tau half)) (range symmetry))
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
       :stroke "#333"
       :stroke-width (* radius stroke-ratio)})
     ;; controls
     ]))

(defn render-grow
  [color stroke-ratio [x y] radius food]
  (let [symmetry 4
        half (/ 0.5 symmetry)
        outer-radius 1.1
        outer-arc 0.07
        inner-radius 0.5
        inner-arc 0.23

        oc-start (map (partial radial-axis symmetry (* radius (+ inner-radius 0.3)) (* tau -1 (- inner-arc half))) (range symmetry))
        oc-end (map (partial radial-axis symmetry (* radius outer-radius) (* tau -1 outer-arc)) (range symmetry))
        outer (map (partial radial-axis symmetry radius 0) (range symmetry))
        ic-start (map (partial radial-axis symmetry (* radius outer-radius) (* tau outer-arc)) (range symmetry))
        ic-end (map (partial radial-axis symmetry (* radius (+ inner-radius 0.3)) (* tau (- inner-arc half))) (range symmetry))
        inner (map (partial radial-axis symmetry (* inner-radius radius) (* tau half)) (range symmetry))

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
       :stroke "#333"
       :stroke-width (* radius stroke-ratio)})
     ;; controls
     ]))

(defn render-move
  [color stroke-ratio [x y] radius food]
  (let [symmetry 3
        half (/ 0.5 symmetry)
        outer-radius 1.1
        outer-arc 0.07
        mid-radius 1.1
        mid-arc 0.22
        under-radius 0.75
        under-arc 0.13
        inner-radius 0.3
        inner-arc 0.3

        oc-start (map (partial radial-axis symmetry (* radius (+ inner-radius 0.4)) (* tau -1 1.1 (- inner-arc half))) (range symmetry))
        oc-end (map (partial radial-axis symmetry (* radius outer-radius) (* tau -1 outer-arc)) (range symmetry))
        outer (map (partial radial-axis symmetry radius 0) (range symmetry))

        mc-start (map (partial radial-axis symmetry (* radius outer-radius) (* tau outer-arc)) (range symmetry))
        mc-end (map (partial radial-axis symmetry (* radius mid-radius) (* tau (- mid-arc 0.05))) (range symmetry))
        mid (map (partial radial-axis symmetry radius (* tau mid-arc)) (range symmetry))

        uc-start (map (partial radial-axis symmetry (* radius outer-radius 0.9) (* tau mid-arc 1.2)) (range symmetry))
        uc-end (map (partial radial-axis symmetry (* radius 1.1 under-radius) (* tau (- mid-arc 0.03))) (range symmetry))
        under (map (partial radial-axis symmetry (* radius under-radius) (* tau under-arc)) (range symmetry))

        ic-start (map (partial radial-axis symmetry (* radius under-radius 1.2) (* tau (- under-arc 0.11))) (range symmetry))
        ic-end (map (partial radial-axis symmetry (* radius (+ inner-radius 0.5)) (* tau (- (* 0.3 inner-arc) half))) (range symmetry))
        inner (map (partial radial-axis symmetry (* inner-radius radius) (* tau (- half 0.09))) (range symmetry))

        points (mapv
                (partial add-vector [x y])
                (interleave
                 oc-start oc-end outer
                 mc-start mc-end mid
                 uc-start uc-end under
                 ic-start ic-end inner))

        path (make-path color (partition 3 points))
        controls (make-controls 5 "black" "grey" (partition 3 points))]
    [:g
     (update
      path 1 merge
      {:fill color
       :stroke "#333"
       :stroke-width (* radius stroke-ratio)})
     ;; controls
     ]))

(defn render-single-food
  [color radius [x y]]
  [:circle
   {:cx x
    :cy y
    :r radius
    :fill color
    :stroke "#777"
    :stroke-width (* radius 0.15)}])

(defn render-food
  [position beam radius color food]
  (let [symmetry 3
        points
        (map
         (comp
          (partial add-vector position)
          (partial radial-axis symmetry beam (* tau -0.25)))
         (range food))]
    [:g
     (map
      (partial render-single-food color radius)
      points)]))

(defn brighten
  [color-str factor]
  (-> color-str
      color/css
      color/as-hsva
      (update :v + factor)
      color/as-css
      :col))

(defn render-element
  [color food-color [x y] radius element]
  (let [subradius (* 0.87 radius)
        stroke-ratio 0.007
        bright (brighten color 0.2)
        icon
        (condp = (:type element)
          :eat (render-eat bright stroke-ratio [x y] subradius (:food element))
          :grow (render-grow bright stroke-ratio [x y] subradius (:food element))
          :move (render-move bright stroke-ratio [x y] subradius (:food element))
          [:circle
           {:cx x
            :cy y
            :r (* radius 0.8)
            :fill bright
            :stroke "#333"
            :stroke-width (* radius stroke-ratio)}])
        food (render-food [x y] (* radius 0.3) (* radius 0.2) food-color (:food element))]
    [:g icon food]))

(defn render-organism
  [locations color food-color radius elements]
  (map
   (fn [{:keys [space] :as element}]
     (let [location (get locations space)]
       (render-element color food-color location radius element)))
   elements))

(defn render-game
  [{:keys [colors radius layout background locations player-colors] :as board} game]
  (let [all-elements (-> game :state :elements vals)
        food-color (-> colors first last)
        organisms (group-by
                   (juxt
                    :player
                    :organism)
                   all-elements)
        element-icons
        (mapv
         (fn [[[player organism] elements]]
           [:g
            (let [color (get player-colors player)]
              (render-organism locations color food-color radius elements))])
         organisms)
        svg (apply conj layout element-icons)]
    svg))

#?(:clj
   (defn export
     [symmetry radius buffer colors path]
     (let [radiate (board-layout symmetry radius buffer colors)
           out (up/html radiate)]
       (spit path out))))

