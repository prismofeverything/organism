(ns organism.viewer
  "Read-only OGF adapter to the same board renderer used by organism.play."
  (:require [clojure.string :as string]
            [organism.board :as board]
            [reagent.dom :as rdom]))

(defn space [id]
  (let [i (string/last-index-of id ":")]
    [(subs id 0 i) (js/parseInt (subs id (inc i)) 10)]))

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

(defn ogf-board [ogf]
  (let [spec (get ogf "board")
        spaces (get spec "spaces")
        dist (distances (get spec "adjacencies") (get spec "center"))
        labels (->> spaces (sort-by dist) (map (comp first space)) distinct vec)
        radius (board/ring-radius (count labels))
        buffer 2.1
        ;; Use the creation page's palette generation and player/ring mapping.
        all-labels (concat labels (map #(str "viewer-extra-" %) (range 10)))
        all-colors (board/generate-colors-buffer all-labels (count labels) 10)
        colors (take (count labels) all-colors)
        locations (select-keys (board/board-locations (get ogf "symmetry") radius buffer colors)
                               (map space spaces))
        field (* 2 radius buffer (count labels))
        layout [:svg {:viewBox (str "0 0 " field " " field)
                      :width "100%" :height "100%" :role "img"
                      :aria-label "Organism game board"}
                (board/build-background (get ogf "symmetry") radius buffer colors)
                (into [:g] (map (fn [[s spec]] ^{:key s} (board/circle spec)) locations))]]
    {:radius radius :buffer buffer :colors colors :locations locations :layout layout
     :player-colors (board/find-player-colors (get ogf "players") (map last all-colors))}))

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
        svg (board/render-game b {:state (ogf-state frame)})
        highlights (into [:g {:pointer-events "none"}]
                         (keep (fn [id]
                                 (when-let [[x y] (get (:locations b) (space id))]
                                   ^{:key id}
                                   [:circle {:cx x :cy y :r (* (:radius b) 1.02)
                                             :fill "none" :stroke "#ffdd86" :stroke-width 2}]))
                               (js->clj changed)))]
    (rdom/render (conj svg highlights) target)
    (clj->js (:player-colors b))))

(defn init! []
  (aset js/window "renderOrganismFrame" render-frame)
  (aset js/window "resetOrganismColors" #(reset! cached-board nil)))
