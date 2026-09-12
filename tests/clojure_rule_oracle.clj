;; Live reference results from the application's current base rules.
(require '[clojure.edn :as edn]
         '[clojure.data.json :as json]
         '[organism.game :as g]
         '[organism.choice :as c])

(defn snapshot [game]
  (let [state (:state game)]
    {:elements (->> (:elements state) vals
                    (map #(select-keys % [:player :type :space :food]))
                    (sort-by :space) vec)
     :food (vec (sort-by first (:food state)))
     :captures (into (sorted-map)
                     (for [[player captures] (:captures state)]
                       [player (vec (sort-by (juxt :player :type)
                                           (map #(select-keys % [:player :type]) captures)))]))
     :winner (:winner state)}))

(defn result [{:keys [op game fields player symmetry rings notches] :as request}]
  (case op
    :victory (g/victory? game)
    :board (let [board (g/create-game symmetry (vec (range rings)) [] 3 notches)]
             (vec (sort-by first (map (fn [[space adjacent]] [space (vec (sort adjacent))])
                                      (:adjacencies board)))))
    :legal-probes
    (let [elements (sort-by :space (vals (get-in game [:state :elements])))
          growers (filter #(= :grow (:type %)) (g/current-organism-elements game))]
      {:eat (mapv #(vector (:space %) (boolean (g/can-eat? game %))) elements)
       :move (mapv #(vector (:space %) (boolean (g/can-move? game (:space %)))) elements)
       :destinations (mapv #(vector (:space %) (vec (sort (g/available-spaces game (:space %))))) elements)
       :growth (vec (sort (g/growable-spaces game (map :space growers))))})
    :circulate-choices (vec (sort (keys (c/circulate-to-choices
                                       game (g/current-organism-elements game)
                                       (g/current-organism-elements game)))))
    :action-choices (vec (sort (conj (mapv name (keys (c/choose-action-choices game (:type request)))) "pass")))
    (snapshot
     (case op
       :circulate (g/circulate game fields)
       :eat (g/eat game fields)
       :grow (g/grow game fields)
       :move (g/move game fields)
       :introduce (g/introduce-spaces game player fields)
       :resolve-conflicts (g/resolve-conflicts game player)
       :check-integrity (g/check-integrity game player)))))

(let [requests (edn/read (java.io.PushbackReader. *in*))
      results (binding [*out* (java.io.StringWriter.)] (mapv result requests))]
  (println (json/write-str results)))
