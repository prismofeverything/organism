;; Every space of a full seven-ring board, for each board symmetry, from the
;; game's own geometry -- so make_web3d.py lines the printed art up with the
;; rules' layout rather than a copy of it.
;;
;;   lein run -m clojure.main pieces/export_board_layout.clj OUT.json
(require '[organism.board :as board] '[clojure.data.json :as json])

(let [out (first *command-line-args*)
      colors (map vector (take 7 board/total-rings) (map #(str "#" %) (range 7)))]
  (spit out
        (json/write-str
         (into {}
               (for [symmetry [6 5]]
                 [symmetry
                  (distinct
                   (for [[[ring n] [x y _ _]] (board/board-locations symmetry 1.0 1.0 colors)]
                     {:ring ring :n n :x x :y y}))])))))
