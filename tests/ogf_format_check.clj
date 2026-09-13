(require '[organism.format :as f]
         '[organism.game :as game]
         '[organism.ogf :as o]
         '[organism.board :as b])
(assert (= ["A" "Z" "AA" "AB"] (mapv o/ring-label [0 25 26 27])))
(assert (= ["AA" 12] (o/parse-space "AA12")))
(doseq [symmetry [5 6 7]]
  (let [palette [[:yellow "#fff88c"] [:red "#da6558"] [:blue "#849cd5"] [:orange "#febe48"]]
        colors (mapv first palette)
        g (game/create-game symmetry colors [] 3 false)
        state {:round 0 :player-turn {:player :p0} :elements {} :food {[:red 0] 2} :captures {:p0 [] :p1 []}}
        record (f/game->ogf {:key "format-check" :invocation {:players [:p0 :p1] :colors palette}
                            :game g :history [state]})]
    (assert (= 2 (:version record)))
    (assert (= "A0" (get-in record [:board :center])))
    (assert (= (mapv second palette) (get-in record [:board :ring-colors])))
    (assert (= {"B0" 2} (get-in record [:frames 0 :food])))
    (assert (nil? (:colors record)))
    (assert (= symmetry (:symmetry record)))
    (f/write-ogf! record (str "/tmp/ogf-format-" symmetry ".json"))
    (let [restored (f/ogf->game (f/read-ogf (str "/tmp/ogf-format-" symmetry ".json")))]
      (assert (= [:A 0] (get-in restored [:board :center])))
      (assert (= {:p0 "#da6558" :p1 "#fff88c"} (:colors restored))))))
(println "Clojure OGF v2 ring coordinates, palette and legacy parsing passed")
