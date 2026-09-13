(ns organism.viewer
  "Read-only OGF adapter to the same board renderer used by organism.play."
  (:require [clojure.string :as string]
            [organism.board :as board]
            [organism.ogf :as ogf]
            [thi.ng.color.core :as color]
            [reagent.dom :as rdom]))

(def space ogf/parse-space)

(defn distances [adj center]
  (loop [front [center] result {center 0}]
    (if (empty? front)
      result
      (let [[next-front next-result]
            (reduce (fn [[next-front result] s]
                      (reduce (fn [[next-front result] neighbor]
                                (if (contains? result neighbor)
                                  [next-front result]
                                  [(conj next-front neighbor) (assoc result neighbor (inc (get result s)))]))
                              [next-front result] (get adj s)))
                    [[] result] front)]
        (recur next-front next-result)))))

(defn label-color [css]
  (-> css color/css color/as-hsva
      (update :v * 0.55)
      color/as-css :col))

(defn ogf-board [ogf]
  (let [spec (get ogf "board")
        spaces (get spec "spaces")
        labels (mapv ogf/ring-label (range (count (get spec "ring-colors"))))
        radius (board/ring-radius (count labels))
        buffer 2.1
        palette (get spec "ring-colors")
        colors (mapv vector labels palette)
        all-colors (concat palette (get spec "palette-tail"))
        locations (into {}
                        (map (fn [[s [x y r _]]]
                               [s [x y r (nth palette (ogf/ring-index (first s)))]]))
                        (select-keys (board/board-locations (get ogf "symmetry") radius buffer
                                                          (mapv (fn [label] [label label]) labels))
                                     (map space spaces)))
        field (* 2 radius buffer (count labels))
        layout [:svg {:viewBox (str "0 0 " field " " field)
                      :width "100%" :height "100%" :role "img"
                      :aria-label "Organism game board"}
                (board/build-background (get ogf "symmetry") radius buffer colors)
                (into [:g] (map (fn [[s spec]] ^{:key s} (board/circle spec)) locations))]]
    {:radius radius :buffer buffer :colors colors :locations locations :layout layout
     :center [(/ field 2) (/ field 2)]
     :label-colors (into {} (map (fn [[label css]] [label (label-color css)]) colors))
     :player-colors (board/find-player-colors (get ogf "players") all-colors)}))

(defn ogf-state [frame]
  {:elements (into {} (map (fn [[player type id food]]
                             (let [s (space id)]
                               [s {:player player :type (keyword type) :space s :food food}]))
                           (get frame "elements")))
   :food (into {} (map (fn [[id amount]] [(space id) amount]) (get frame "food")))})

(defonce cached-board (atom nil))

(defn render-frame [target raw-game raw-frame changed]
  ;; Avoid converting thousands of history frames just to display one position.
  (let [ogf (js->clj #js {:name (aget raw-game "name")
                         :board (aget raw-game "board")
                         :symmetry (aget raw-game "symmetry")
                         :players (aget raw-game "players")})
        prior @cached-board
        b (if (= ogf (:key prior)) (:board prior) (ogf-board ogf))
        _ (reset! cached-board {:key ogf :board b})
        frame (js->clj raw-frame)
        svg (board/render-game (assoc b :content-rotation -60) {:state (ogf-state frame)})
        labels (into [:g {:pointer-events "none" :aria-label "Space coordinates"}]
                     (map (fn [[s [x y]]]
                            (let [[cx cy] (:center b)
                                  ;; The center follows the shared 30-degree zero ray too.
                                  rotation (if (= s ["A" 0]) -60
                                               (- (* (js/Math.atan2 (- y cy) (- x cx))
                                                     (/ 180 js/Math.PI)) 90))]
                              ^{:key (str s)}
                              [:text {:x 0 :y (* (:radius b) 0.87)
                                      :transform (str "translate(" x " " y ") rotate(" rotation ")")
                                      :data-space (str (first s) (second s))
                                      :text-anchor "middle" :font-size (* (:radius b) 0.30)
                                      :font-weight "normal"
                                      :fill (get (:label-colors b) (first s))}
                               (str (first s) (second s))])) (:locations b)))
        highlights (into [:g {:pointer-events "none"}]
                         (keep (fn [id]
                                 (when-let [[x y] (get (:locations b) (space id))]
                                   ^{:key id}
                                   [:circle {:cx x :cy y :r (* (:radius b) 1.02)
                                             :fill "none" :stroke "#ffdd86" :stroke-width 2}]))
                               (js->clj changed)))]
    (let [[cx cy] (:center b)]
      (rdom/render
       [(first svg) (second svg)
        (into [:g {:transform (str "rotate(60 " cx " " cy ")")
                   :data-board-rotation 60}]
              (concat (drop 2 svg) [highlights labels]))]
       target))
    (clj->js (:player-colors b))))

(defn generate-palette [rings players]
  (let [total (max rings players)
        labels (map ogf/ring-label (range total))]
    (clj->js (mapv last (board/generate-colors-buffer labels rings total)))))

(defn init! []
  (aset js/window "generateOrganismPalette" generate-palette)
  (aset js/window "renderOrganismFrame" render-frame)
  (aset js/window "resetOrganismColors" #(reset! cached-board nil)))
