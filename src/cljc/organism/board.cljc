(ns organism.board
  (:require
   [clojure.string :as string]
   [clojure.pprint :refer (pprint)]
   #?(:clj [hiccup.core :as up])
   [thi.ng.color.core :as color]
   [organism.base :as base]))

(def tau (* 2 Math/PI))
(def total-board-radius 440)

(defn random-color
  [low high]
  (-> (color/hsla
       (rand)
       (rand)
       (+ low (rand (- high low)))
       1.0)
      color/as-css
      :col))

;; (defn generate-colors
;;   [rings]
;;   (let [num-rings (count rings)
;;         base 0.1
;;         saturation-base 0.1
;;         max 0.8
;;         factor (/ (- max base) num-rings)
;;         jump-base 0.2
;;         jump-factor 0.6]
;;     (first
;;      (reduce
;;       (fn [[colors hue] [ring index]]
;;         [(conj
;;           colors
;;           [ring
;;            (-> (color/hsla
;;                 hue
;;                 (- max (+ saturation-base (rand (- max saturation-base))))
;;                 (- max (* index factor))
;;                 1.0)
;;                color/as-css
;;                :col)])
;;          (mod (+ hue (+ jump-base (- (rand (* 2 jump-factor)) jump-factor))) 1.0)])
;;       [[] (rand)]
;;       (map vector rings (range))))))

(defn generate-colors
  [rings]
  (let [num-rings (count rings)
        saturation-base 0.3
        saturation-max 0.9
        lightness-base 0.1
        lightness-max 0.8
        factor (/ (- lightness-max lightness-base) num-rings)
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
                (+ saturation-base (rand (- saturation-max saturation-base)))
                (+ lightness-base (* index factor) (rand factor))
                1.0)
               color/as-css
               :col)])
         (mod (+ hue (+ jump-base (- (rand (* 2 jump-factor)) jump-factor))) 1.0)])
      [[] (rand)]
      (map vector rings (reverse (range (count rings))))))))

(defn generate-random-colors
  [labels low high]
  (map
   (fn [label]
     [label
      (random-color low high)])
   labels))

(defn generate-colors-buffer
  [all-rings num-rings total]
  (let [ring-colors (generate-colors (take num-rings all-rings))
        buffer (- total num-rings)
        buffer-labels (take buffer (drop num-rings all-rings))
        extra (generate-random-colors buffer-labels 0.1 0.9)]
    (vec (concat ring-colors extra))))

(def total-rings
  ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"])

(def default-player-order
  ["orb" "mass" "brone" "laam" "stuk" "faast" "lelon" "insim" "plun" "zdio" "maa" "ninon" "pana"])

(def default-player-captures 5)

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
              (partial radial-axis symmetry 1 (/ tau 12.0))
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

(defn symmetry-background
  [symmetry]
  (cond
    (= symmetry 5) [1.85 1.9] ;; [1.65 1.7]
    (= symmetry 7) [1.9 1.5]
    :else [1.7 1.6]))

(defn build-background
  [symmetry radius buffer colors]
  (let [num-rings (count colors)
        field (* radius buffer num-rings)
        center [field field]
        [pull tie] (symmetry-background symmetry)]
    [:g
     (vec
      (concat
       [:defs]
       (apply
        concat
        (map
         (fn [[color-key color] index]
           ^{:key color-key}
           [[:radialGradient {:id color-key}
             ^{:key (str color-key "black")} [:stop {:offset "0%" :stop-color "black"}]
             ^{:key (str color-key "color")} [:stop {:offset "100%" :stop-color color}]]])
         ;; [:radialGradient {:id (str (name color-key) "-element")}
         ;;  [:stop {:offset "0%" :stop-color color}]
         ;;  [:stop {:offset "50%" :stop-color color}]
         ;;  [:stop {:offset "100%" :stop-color "black"}]]
         (rest colors)
         (map (comp inc inc) (range))))))
     (vec
      (concat
       [:g
        (make-circle (* field 0.93) "#111" center)]
       (map
        (fn [[color-key color] index]
          ^{:key color}
          (circle
           [field field
            (* pull (- (+ num-rings 1) index) (+ radius buffer))
            (str "url(#" (name color-key) ")")]))
        (reverse (rest colors))
        (map (partial + tie) (range)))))]))

(defn board-layout
  [symmetry radius buffer colors notches?]
  (let [field (* 2 radius buffer (count colors))]
    [:svg
     {:width field :height field}
     (vec
      (concat
       (build-background symmetry radius buffer colors)
       (vec
        (map-indexed
         (fn [i spec]
           ^{:key i}
           (circle spec))
         (find-rings symmetry radius buffer (map last colors) notches?)))))]))

(defrecord Board [symmetry radius buffer colors layout locations player-colors])

(defn find-player-colors
  [players colors]
  (let [difference (- (count colors) (count players))]
    (into
     {}
     (map
      vector
      players
      (drop
       difference
       (reverse colors))))))

(defn build-board
  [symmetry radius buffer all-colors rings players notches?]
  (let [num-rings (count rings)
        colors (take num-rings all-colors)
        outer-color (-> colors last first)
        notches (map (fn [n] [outer-color (* n (dec num-rings))]) (range symmetry))
        locations (board-locations symmetry radius buffer colors)
        locations (if notches?
                    (reduce
                     (fn [locations notch]
                       (dissoc locations notch))
                     locations
                     notches)
                    locations)
        player-colors
        (find-player-colors players (map last all-colors))]
    (Board.
     symmetry
     radius
     buffer
     colors
     (board-layout symmetry radius buffer colors notches?)
     locations
     player-colors)))

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
  [color stroke [x y] radius food]
  (let [symmetry 5
        half (/ 0.5 symmetry)
        outer-radius 1.00
        outer-arc 0.03
        inner-radius 0.5
        inner-arc 0.12
        stroke-ratio (:ratio stroke)
        stroke-color (:color stroke)

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
       :stroke stroke-color
       :stroke-width (* radius stroke-ratio)})
     ;; controls
     ]))

(defn render-grow
  [color stroke [x y] radius food]
  (let [symmetry 4
        half (/ 0.5 symmetry)
        outer-radius 1.1
        outer-arc 0.07
        inner-radius 0.5
        inner-arc 0.23
        stroke-ratio (:ratio stroke)
        stroke-color (:color stroke)

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
       :stroke stroke-color
       :stroke-width (* radius stroke-ratio)})
     ;; controls
     ]))

(defn render-move
  [color stroke [x y] radius food]
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
        stroke-ratio (:ratio stroke)
        stroke-color (:color stroke)

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
       :stroke stroke-color
       :stroke-width (* radius stroke-ratio)})
     ;; controls
     ]))

(defn render-single-food
  [color radius [x y]]
  ^{:key [x y]}
  [:circle
   {:cx x
    :cy y
    :r radius
    :fill color
    :stroke "#777"
    :stroke-width (* radius 0.15)}])

(defn render-food
  [position beam radius color food]
  (when-not (zero? food)
    (let [symmetry food
          points
          (map
           (comp
            (partial add-vector position)
            (partial radial-axis symmetry beam (* tau -0.25)))
           (range food))]
      [:g
       (map
        (partial render-single-food color radius)
        points)])))

(defn sanify
  [color]
  (let [sane
        (map
         (fn [key]
           (let [v (get color key)]
             (cond
               (< v 0) (* v -1)
               (> v 1) (- 1 (- v 1))
               :else v)))
         [:h :s :v :a])]
    (apply
     color/hsva
     sane)))

(defn zeno
  "magnitude is a number between zero and one, which is approached by portion"
  [magnitude portion]
  (let [remaining (- 1.0 magnitude)
        advance (* portion remaining)]
    (+ magnitude advance)))

(defn fade
  "magnitude is a number between zero and one, which is approached by portion"
  [magnitude portion]
  (let [reduction (- 1.0 portion)]
    (* magnitude reduction)))

(defn brighten
  [color-str factor]
  (if color-str
    (-> color-str
        color/css
        color/as-hsva
        ;; (update :v + factor)
        (update :v zeno factor)
        (update :s fade factor)
        (sanify)
        color/as-css
        :col)))

(def default-stroke
  {:ratio 0.02
   :color "#555"})

(defn render-element
  ([color food-color [x y] radius element]
   (render-element color food-color default-stroke [x y] radius element))
  ([color food-color stroke [x y] radius element]
   (let [subradius (* 0.87 radius)
         bright (brighten color 0.2)
         icon
         (condp = (:type element)
           :eat (render-eat bright stroke [x y] subradius (:food element))
           :grow (render-grow bright stroke [x y] subradius (:food element))
           :move (render-move bright stroke [x y] subradius (:food element))
           [:circle
            {:cx x
             :cy y
             :r (* radius 0.8)
             :fill bright
             :stroke (:color stroke)
             :stroke-width (* radius (:ratio stroke))}])
         food (render-food [x y] (* radius 0.3) (* radius 0.2) food-color (:food element))]
     (if food
       [:g icon food]
       [:g icon]))))

(defn render-organism
  [locations color food-color radius elements]
  (vec
   (concat
    [:g]
    (map
     (fn [{:keys [space] :as element}]
       (let [location (get locations space)]
         ^{:key space}
         (render-element color food-color location radius element)))
     elements))))

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
           ^{:key [player organism]}
           (let [color (get player-colors player)]
             (render-organism locations color food-color radius elements)))
         organisms)

        free-food
        (mapv
         (fn [[space food]]
           ^{:key [space food]}
           (render-food
            (get locations space)
            (* radius 0.3) (* radius 0.2) food-color food))
         (get-in game [:state :food]))

        svg (apply conj layout (concat element-icons free-food))]
    svg))

(defn empty-invocation
  ([] (empty-invocation "orb"))
  ([player]
   (let [players [player] ;; (vec (take 1 default-player-order))
         player-captures (take (count players) (repeat default-player-captures))]
     {:player-count 1
      :ring-count 3
      :organism-victory 3
      :players players
      :player-captures player-captures
      :description ""
      :mutations {}
      :colors (generate-colors (take 3 total-rings))})))

(defn invocation-player-colors
  [invocation]
  (into
   {}
   (map
    (fn [player [ring color]]
      [player color])
    (:players invocation)
    (:colors invocation))))

(defn valid-invocation?
  [invocation]
  (let [players (:players invocation)
        players-set (set players)]
    (println "valid invocation?" players players-set      (every? (comp not empty?) players)      (= (count players) (count players-set)))
    (and
     (every? (comp not empty?) players)
     (= (count players) (count players-set)))))

(defn player-symmetry
  [player-count]
  (cond
    (< player-count 5) 6
    (= player-count 5) 5
    (= player-count 7) 7
    :else 6))

(defn ring-radius
  [ring-count]
  (let [total (dec (* 2 ring-count))]
    (quot total-board-radius total)))

(defn cut-notches?
  [ring-count player-count mutations]
  (and
   (> ring-count 4)
   (not (:RAIN mutations))
   (not= 4 player-count)
   (< player-count 8)))

(defn find-starting-spaces
  [symmetry rings players]
  (let [starting-ring (last rings)
        ring-count (count rings)
        player-count (count players)
        total (* (dec ring-count) symmetry)
        interval (/ total player-count)
        difference (- (dec ring-count) 3)
        offset (Math/ceil (/ difference 2))
        offset
        (if (= 4 player-count)
          (inc offset)
          offset)]
    (println "STARTING" starting-ring ring-count player-count total interval difference offset)
    (map-indexed
     (fn [player-index player]
       [player
        (mapv
         (fn [element-index]
           [starting-ring
            (mod
             (int
              (Math/ceil
               (+ (* player-index interval)
                  element-index
                  offset)))
             total)])
         (range 3))])
     players)))

(defn find-rain-spaces
  [symmetry rings players-rain]
  (let [players (take (dec (count players-rain)) players-rain)
        rain (last players-rain)
        starting-ring (last rings)
        ring-count (count rings)
        player-count (dec (count players))
        total (inc (* (dec ring-count) (- symmetry 4)))
        num-organisms (int (Math/floor (/ (inc total) 4)))
        player-cycle (cycle players)
        between (- total (* num-organisms 3))
        offset (int (Math/floor (/ (dec between) num-organisms)))
        player-spaces
        (reduce
         (fn [spaces [player organism]]
           (update
            spaces player concat
            (mapv
             (fn [element-index]
               [starting-ring
                (mod
                 (int
                  (Math/ceil
                   (+ (* organism 4)
                      element-index
                      offset)))
                 total)])
             (range 3))))
         {} (map vector player-cycle (range num-organisms)))
        _ (println "PLAYER SPACES BEFORE" player-spaces)
        player-spaces
        (mapv
         (fn [[player spaces]]
           [player spaces])
         player-spaces)
        _ (println "PLAYER SPACES AFTER" player-spaces)
        rain-spaces (inc (* 2 (dec ring-count)))
        first-rain-space (* (- symmetry 3) (dec ring-count))
        starting-rain
        (mapv
         (fn [rain-space]
           [starting-ring rain-space])
         (range
          first-rain-space
          (+ first-rain-space rain-spaces)))]
    (println "starting RAIN" starting-ring ring-count player-count total between offset num-organisms starting-rain)
    (conj player-spaces [rain starting-rain])))

(defn starting-spaces
  [ring-count player-count players total-rings mutations]
  (let [symmetry (player-symmetry player-count)
        rings (take ring-count total-rings)]
    (if (:RAIN mutations)
      (find-rain-spaces symmetry rings players)
      (find-starting-spaces symmetry rings players))))

(defn generate-board
  [colors players rings mutations]
  (let [player-count (count players)
        ring-count (count rings)
        symmetry (player-symmetry player-count)
        radius (ring-radius ring-count)
        radius (if (= symmetry 7) (* 0.9 radius) radius)
        buffer (if (= symmetry 7) 2.4 2.1)
        notches? (cut-notches? ring-count player-count mutations)]
    (build-board
     symmetry
     radius
     buffer
     colors
     rings
     players
     notches?)))


#?(:clj
   (defn export
     [symmetry radius buffer colors path]
     (let [radiate (board-layout symmetry radius buffer colors)
           out (up/html radiate)]
       (spit path out))))

