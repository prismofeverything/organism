(set! *warn-on-reflection* true)
(require '[organism.board :as board] '[organism.game :as game] '[hiccup.core :as up])

(def out-dir (first *command-line-args*))
(def symmetry 4)
(def rings ["A" "B" "C" "D" "E" "F" "G"])
(def players ["orb" "mass" "brone" "laam"])
;; ring palette sampled from organism-five-player.png, then player colors
(def colors
  [["A" "#ebe0e4"] ["B" "#b2b1e2"] ["C" "#be97b3"] ["D" "#57adc7"]
   ["E" "#969f41"] ["F" "#5e7236"] ["G" "#531842"]
   ["H" "#6aa0b8"] ["I" "#8e5578"] ["J" "#a8b050"] ["K" "#3fbfe6"]])

(def relax-params
  {:iterations 4000   ; spring steps, then `:settle` spacing-only passes
   :settle 300
   :step 0.05
   :apart 2.2         ; non-adjacent pairs pushed out to this many spacings
   :ring-pull 3.0     ; pull toward each ring's mean radius, so rings read round
   :min 1.0})         ; hard floor: no two spaces closer than one spacing

(defn relax
  "Afterpass on the space positions. The square rings put a corner's
   same-ring neighbors ~1.41 spacings away, as far as spaces it does not
   touch, so rings read as concentric squares. Pull adjacent spaces to one
   spacing, push non-adjacent ones apart, round each ring toward a circle,
   and keep the board's 4-fold rotational symmetry exact every step."
  [locations adjacencies center]
  (let [{:keys [iterations settle step apart ring-pull min]} relax-params
        names (vec (sort (keys adjacencies)))
        n (int (count names))
        idx (zipmap names (range))
        [cx cy] (take 2 (get locations center))
        ^doubles xs (double-array (map #(- (first (get locations %)) cx) names))
        ^doubles ys (double-array (map #(- (second (get locations %)) cy) names))
        c (int (idx center))
        spacing (Math/hypot (aget xs (idx ["B" 0])) (aget ys (idx ["B" 0])))
        ^booleans adj (let [a (boolean-array (* n n))]
                        (doseq [[s ns] adjacencies t ns]
                          (aset a (+ (* n (idx s)) (idx t)) true))
                        a)
        ^ints level (int-array (map #(- (int (first (first %))) 65) names))
        ;; the space a quarter turn on: ring k, space i -> i+k
        ^ints nxt (int-array (map (fn [[color i]]
                              (let [k (- (int (first color)) 65)]
                                (if (zero? k) c
                                    (get idx [color (mod (+ i k) (* symmetry k))] (idx [color i])))))
                            names))
        ;; which way a quarter turn goes in screen space
        sgn (let [b0 (idx ["B" 0]) b1 (aget nxt b0)]
              (if (pos? (- (* (aget xs b0) (aget ys b1)) (* (aget ys b0) (aget xs b1)))) 1.0 -1.0))
        ^doubles fx (double-array n) ^doubles fy (double-array n)
        symmetrize!
        (fn []
          (let [^doubles ax (double-array n) ^doubles ay (double-array n)]
            (dotimes [i n]
              (loop [j i r 0 x (aget xs i) y (aget ys i)]
                (if (< r 4)
                  (do (aset ax i (+ (aget ax i) x)) (aset ay i (+ (aget ay i) y))
                      ;; next orbit member, rotated back a quarter turn
                      (let [j' (aget nxt j) rot (inc r)
                            [x' y'] (loop [x (aget xs j') y (aget ys j') t rot]
                                      (if (zero? t) [x y] (recur (* sgn y) (- (* sgn x)) (dec t))))]
                        (recur j' rot x' y')))
                  nil)))
            (dotimes [i n]
              (aset xs i (/ (aget ax i) 4)) (aset ys i (/ (aget ay i) 4)))))
        push-apart!
        (fn []
          (dotimes [i n]
            (dotimes [j n]
              (when (< i j)
                (let [dx (- (aget xs i) (aget xs j)) dy (- (aget ys i) (aget ys j))
                      d (Math/hypot dx dy) short (- (* min spacing) d)]
                  (when (pos? short)
                    (let [m (/ (* 0.25 short) d)]
                      (aset xs i (+ (aget xs i) (* m dx))) (aset ys i (+ (aget ys i) (* m dy)))
                      (aset xs j (- (aget xs j) (* m dx))) (aset ys j (- (aget ys j) (* m dy)))))))))
          (aset xs c 0.0) (aset ys c 0.0))]
    (dotimes [it (+ iterations settle)]
      (when (< it iterations)
        (java.util.Arrays/fill fx 0.0) (java.util.Arrays/fill fy 0.0)
        (dotimes [i n]
          (dotimes [j n]
            (when (not= i j)
              (let [dx (- (aget xs i) (aget xs j)) dy (- (aget ys i) (aget ys j))
                    d (Math/hypot dx dy)
                    f (if (aget adj (+ (* n i) j))
                        (- d spacing)
                        (clojure.core/min 0.0 (- d (* apart spacing))))]
                (aset fx i (- (aget fx i) (/ (* f dx) d)))
                (aset fy i (- (aget fy i) (/ (* f dy) d)))))))
        (let [radii (group-by first (map (fn [i] [(aget level i) (Math/hypot (aget xs i) (aget ys i))]) (range n)))
              mean (into {} (map (fn [[k rs]] [k (/ (reduce + (map second rs)) (count rs))]) radii))]
          (dotimes [i n]
            (when-not (= i c)
              (let [r (Math/hypot (aget xs i) (aget ys i))
                    pull (/ (* ring-pull (- r (mean (aget level i)))) r)]
                (aset fx i (- (aget fx i) (* pull (aget xs i))))
                (aset fy i (- (aget fy i) (* pull (aget ys i))))))))
        (dotimes [i n]
          (when-not (= i c)
            (aset xs i (+ (aget xs i) (* step (aget fx i))))
            (aset ys i (+ (aget ys i) (* step (aget fy i)))))))
      (dotimes [_ 3] (push-apart!))
      (symmetrize!))
    {:spacing spacing
     :positions (into {} (map (fn [s] [s [(aget xs (idx s)) (aget ys (idx s))]]) names))}))

(defn relaxed-board
  "Rebuild the board's layout around relaxed positions: same space circles,
   and one gradient disc per ring sized to that ring's new radius."
  [b g]
  (let [{:keys [radius buffer colors]} b
        field (* radius buffer (count colors))
        {:keys [spacing positions]} (relax (:locations b) (:adjacencies g) ["A" 0])
        color-of (into {} colors)
        ring-r (into {} (map (fn [[k ps]] [k (/ (reduce + (map (fn [[_ p]] (Math/hypot (first p) (second p))) ps)) (count ps))])
                             (group-by (comp first first) positions)))
        outer (apply max (map (fn [[_ [x y]]] (Math/hypot x y)) positions))
        ;; fit the outer ring where the square board's corners reached
        scale (/ (- field radius (* 0.9 radius buffer)) (+ outer radius))
        at (fn [[x y]] [(+ field (* scale x)) (+ field (* scale y))])
        locations (into {} (map (fn [[s p]] [s (conj (at p) radius (color-of (first s)))]) positions))
        discs (for [[k _] (reverse (rest colors))]
                (board/circle [field field (* scale (+ (ring-r k) (* 0.55 spacing))) (str "url(#" k ")")]))
        background [:g
                    (into [:defs]
                          (for [[k col] (rest colors)]
                            [:radialGradient {:id k}
                             [:stop {:offset "0%" :stop-color "black"}]
                             [:stop {:offset "100%" :stop-color col}]]))
                    (into [:g (board/make-circle (* field 0.93) "#111" [field field])] discs)]]
    (println "relaxed: spacing" spacing "scale" scale)
    (assoc b
           :locations (into {} (map (fn [[s p]] [s (vec (take 2 p))]) locations))
           :layout (into [:svg {:width (* 2 field) :height (* 2 field)} background]
                         (map board/circle (vals locations))))))

(defn render [notches? file]
  (let [radius (board/ring-radius (count rings))
        buffer 2.1
        b (board/build-board symmetry radius buffer colors rings players notches?)
        b (assoc b :player-colors {"orb" "#5ec8ea" "mass" "#e0b8d4" "brone" "#b4be50" "laam" "#7a9450"})
        ;; center each organism on its side: a side is the corner plus five
        ;; spaces, so the middle three are 2-4 past each corner
        side (dec (count rings))
        starting (map-indexed
                  (fn [i player]
                    [player (mapv (fn [k] [(last rings) (+ (* i side) (quot side 2) -1 k)])
                                  (range 3))])
                  players)
        info (game/initial-players starting (repeat 4 5))
        g (game/create-game symmetry rings info 3 notches? {})
        ;; give each starting organism one of each element, like a fresh board
        g (reduce
           (fn [g [player spaces]]
             (reduce (fn [g [space type]]
                       (assoc-in g [:state :elements space]
                                 (game/->Element player 0 type space 1 [])))
                     g (map vector spaces [:eat :grow :move])))
           g starting)
        b (relaxed-board b g)
        svg (board/render-game b g)
        svg (update svg 1 assoc :xmlns "http://www.w3.org/2000/svg"
                    :style "background:#fff")]
    (println file "spaces:" (count (:adjacencies g))
             "center adj:" (get (:adjacencies g) ["A" 0])
             "starting:" starting)
    (spit (str out-dir "/" file) (up/html svg))))

(render true "organism-four-player.svg")
