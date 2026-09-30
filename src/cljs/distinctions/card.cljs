(ns distinctions.card
  "One DISTINCTIONS card as SVG, drawn the way distinctions/make_key.py draws
   the key, back to front: field, bar, rays, eye, circle.

   The rays and the eye take the colours the card is not using, and follow the
   inversion in opposite directions: plain, the rays are the other of red/blue
   and the eye the other of black/white; inverted, the reverse."
  (:require
   [distinctions.game :as game]))

(def bw {"black" "#16141a" "white" "#ffffff"})
(def rb {"red" "#d42a20" "blue" "#1f4fc0"})
(def other {"black" "white" "white" "black" "red" "blue" "blue" "red"})

;; geometry in a 100 x 140 box, same proportions as the key
(def ^:private W 100)
(def ^:private H 140)
(def ^:private cx 50)
(def ^:private cy 70)

(def ^:private rays-points
  "Sixteen thin triangles, each with its base at the centre."
  (vec (for [i (range 16)]
         (let [a  (- (* 2 js/Math.PI (/ i 16)) (/ js/Math.PI 2))
               ux (js/Math.cos a) uy (js/Math.sin a)
               bw 7.5 L 100]
           (str (- cx (* uy bw)) "," (+ cy (* ux bw)) " "
                (+ cx (* ux L)) "," (+ cy (* uy L)) " "
                (+ cx (* uy bw)) "," (- cy (* ux bw)))))))

(def ^:private eye-path
  (str "M4," cy " Q" cx "," (- cy 52) " 96," cy " Q" cx "," (+ cy 52) " 4," cy "Z"))

(defn card
  "The face of `value` (0..63), `width` pixels wide. Extra props are merged
   onto the svg element (on-click, style)."
  [{:keys [value width props]}]
  (let [a      (game/attrs value)
        bg     (:background a)
        fg     (:foreground a)
        inv?   (= 1 (game/value value :inversion))
        field  (if inv? (rb fg) (bw bg))
        figure (if inv? (bw bg) (rb fg))
        ray-c  (if inv? (bw (other bg)) (rb (other fg)))
        eye-c  (if inv? (rb (other fg)) (bw (other bg)))]
    [:svg (merge-with merge
                      {:viewBox (str "0 0 " W " " H)
                       :width width :height (* width 1.4)
                       :style {:border-radius (str (* 0.05 width) "px")
                               :display "block" :flex-shrink 0
                               :box-shadow (if (= field (bw "white"))
                                             "inset 0 0 0 1px #d8d6dc, 0 1px 3px rgba(0,0,0,.4)"
                                             "0 1px 3px rgba(0,0,0,.4)")}}
                      props)
     [:title (game/describe value)]
     [:rect {:width W :height H :fill field}]
     (when (= "bar" (:composition a))
       [:rect {:x 36 :y 0 :width 28 :height H :fill figure}])
     (when (= 1 (game/value value :rays))
       (for [[i pts] (map-indexed vector rays-points)]
         ^{:key i} [:polygon {:points pts :fill ray-c}]))
     (when (= 1 (game/value value :eye))
       [:path {:d eye-path :fill eye-c}])
     (when (= "circle" (:composition a))
       [:circle {:cx cx :cy cy :r 19 :fill figure}])
     ;; a white field is drawn with a hairline so it reads on any ground
     (when (= field (bw "white"))
       [:rect {:x 0.5 :y 0.5 :width (dec W) :height (dec H) :rx 5
               :fill "none" :stroke "#d8d6dc" :stroke-width 1}])]))

(defn back
  "The back: a split disc, black and white over red and blue, on grey."
  [{:keys [width props]}]
  [:svg (merge-with merge
                    {:viewBox (str "0 0 " W " " H) :width width :height (* width 1.4)
                     :style {:border-radius (str (* 0.05 width) "px") :display "block"
                             :flex-shrink 0 :box-shadow "0 1px 3px rgba(0,0,0,.5)"}}
                    props)
   [:rect {:width W :height H :fill "#2a2830"}]
   [:rect {:x 6 :y 6 :width 88 :height 128 :rx 4 :fill "none"
           :stroke "#4a4752" :stroke-width 1.5}]
   [:path {:d "M50,40 A30,30 0 0,0 50,100 Z" :fill (bw "white")}]
   [:path {:d "M50,40 A30,30 0 0,1 50,100 Z" :fill (bw "black")}]
   [:circle {:cx 50 :cy 70 :r 11 :fill (rb "red")}]
   [:path {:d "M50,59 A11,11 0 0,1 50,81 Z" :fill (rb "blue")}]])
