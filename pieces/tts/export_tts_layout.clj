;; The spaces and home spaces of every board the Tabletop Simulator mod uses,
;; from the game's own geometry, so the mod's snap points and setup follow the
;; rules rather than a second copy of them.
;;
;;   lein run -m clojure.main pieces/tts/export_tts_layout.clj OUT_DIR
;;
;; writes OUT_DIR/layout.json and OUT_DIR/four-player-board.svg (the
;; four-player board without pieces, which the mod uses as its art).
;;
;; Boards, keyed as the mod names them:
;;   hex    six-fold, 7 rings -- the 54 cm board's hex face (6 players)
;;   penta  five-fold, 7 rings -- its other face (5 players)
;;   small  six-fold, 5 rings -- the 40 cm 2-3 player board
;;   four   four-fold, 7 rings, relaxed -- the four-player board
;; Positions are in board units (one space apart); each board says which
;; spaces it has (notched corners are gone) and each player count its homes.
(require '[organism.board :as board] '[organism.game :as game]
         '[clojure.data.json :as json] '[hiccup.core :as up])

(def out-dir (first *command-line-args*))

;; Loading the four-player script renders its two SVGs into out-dir as a side
;; effect; what we want from it is `relaxed-board`, the layout the art uses.
(load-file "pieces/render_four_player_board.clj")
(declare symmetry rings players classic relaxed-board)

(defn spaces-of
  [symmetry ring-count notches?]
  (let [rings (vec (take ring-count board/total-rings))
        colors (map vector rings (map #(str "#" %) (range)))
        locations (board/board-locations symmetry 1.0 1.0 colors)
        g (game/create-game symmetry rings [] 3 notches? {})
        [cx cy] (get locations ["A" 0])]
    (for [space (sort (keys (:adjacencies g)))
          :let [[x y] (get locations space)]]
      {:ring (first space) :n (second space) :x (- x cx) :y (- y cy)
       :adjacent (map (fn [[r n]] (str r "-" n)) (get-in g [:adjacencies space]))})))

(defn homes
  [symmetry ring-count player-count]
  (let [rings (vec (take ring-count board/total-rings))
        players (mapv str (range player-count))]
    (for [[_ spaces] (board/find-starting-spaces symmetry rings players)]
      (map (fn [[r n]] (str r "-" n)) spaces))))

(defn standard
  [symmetry ring-count player-counts]
  {:symmetry symmetry
   :rings ring-count
   :spaces (spaces-of symmetry ring-count (board/cut-notches? ring-count (first player-counts) {}))
   :homes (into {} (for [p player-counts] [p (homes symmetry ring-count p)]))})

;; The four-player board: the same construction as render_four_player_board's
;; `render`, without pieces, and its relaxed positions in place of the lattice.
(defn four-player
  []
  (let [radius (board/ring-radius (count rings))
        b (board/build-board symmetry radius 2.1 (:colors classic) rings players true)
        g (game/create-game symmetry rings (game/initial-players [] []) 3 true {})
        r (relaxed-board b g (:discs classic))
        {:keys [locations field]} r
        [cx cy] (get locations ["A" 0])
        [bx by] (get locations ["B" 0])
        unit (Math/hypot (- bx cx) (- by cy))
        side (dec (count rings))
        ;; the homes render_four_player_board gives each player
        homes (for [i (range 4)]
                (map (fn [k] (str (last rings) "-" (+ (* i side) (quot side 2) -1 k))) (range 3)))
        svg (update (:layout r) 1 assoc :xmlns "http://www.w3.org/2000/svg")]
    (spit (str out-dir "/four-player-board.svg") (up/html svg))
    {:symmetry symmetry
     :rings (count rings)
     ;; the art: a square SVG 2*field across, centred on the centre space
     :art {:px (* 2 field) :centre [cx cy] :unit unit}
     :spaces (for [space (sort (keys (:adjacencies g)))
                   :let [[x y] (get locations space)]]
               {:ring (first space) :n (second space)
                :x (/ (- x cx) unit) :y (/ (- y cy) unit)
                :adjacent (map (fn [[r n]] (str r "-" n)) (get-in g [:adjacencies space]))})
     :homes {4 homes}}))

(spit (str out-dir "/layout.json")
      (json/write-str
       {:hex (standard 6 7 [6])
        :penta (standard 5 7 [5])
        :small (standard 6 5 [2 3])
        :four (four-player)}))
(println "wrote" (str out-dir "/layout.json"))
