(ns organism.scripts.install-organism-obo
  "Install the organism OBO bot as a flowchart.

   Run with: lein with-profile uberjar run -m organism.scripts.install-organism-obo

   Three diagrams:
     :main   — phase dispatcher
     :action — choose-action-type visible decision tree
     :confirm — choose-action (circulate override)"
  (:require
   [organism.mongo :as db]
   [organism.persist-journey-bots :as bots-db]))

;; ── action sub-diagram ──────────────────────────────────────────────────────
;; Mirrors the scoring heuristic:
;;   has-capture? → move (250)
;;   grow-feasible? → grow (200)
;;   grower-starving? → circulate (150)
;;   total-food > 2 → move (140)  [reposition]
;;   eater-food = 0 → eat (100)   [critical]
;;   else → eat (30)

(def action-diagram
  {:name "action"
   :color "#5a3a1e"
   :collapsed? false
   :origin [40 40]
   :start-tile :start
   :tiles
   {:start         {:id :start :kind :start :type :start
                    :pos [40 20] :params {}}
    :has-capture   {:id :has-capture :kind :condition :type :has-capture?
                    :pos [40 110] :params {}}
    :do-move-cap   {:id :do-move-cap :kind :effect :type :pick-action
                    :pos [240 60] :params {:action :move}}
    :grow-ok       {:id :grow-ok :kind :condition :type :grow-feasible?
                    :pos [240 200] :params {}}
    :do-grow       {:id :do-grow :kind :effect :type :pick-action
                    :pos [440 150] :params {:action :grow}}
    :grower-hungry {:id :grower-hungry :kind :condition :type :grower-starving?
                    :pos [440 300] :params {}}
    :do-circ       {:id :do-circ :kind :effect :type :pick-action
                    :pos [640 250] :params {:action :circulate}}
    :food-ok       {:id :food-ok :kind :condition :type :total-food-low?
                    :pos [640 400] :params {:threshold 2}}
    :do-move-repo  {:id :do-move-repo :kind :effect :type :pick-action
                    :pos [840 350] :params {:action :move}}
    :do-eat        {:id :do-eat :kind :effect :type :pick-action
                    :pos [840 480] :params {:action :eat}}}
   :links
   [{:from {:tile :start :port :out}           :to {:tile :has-capture :port :in}}
    {:from {:tile :has-capture :port :true}    :to {:tile :do-move-cap :port :in}}
    {:from {:tile :has-capture :port :false}   :to {:tile :grow-ok :port :in}}
    {:from {:tile :grow-ok :port :true}        :to {:tile :do-grow :port :in}}
    {:from {:tile :grow-ok :port :false}       :to {:tile :grower-hungry :port :in}}
    {:from {:tile :grower-hungry :port :true}  :to {:tile :do-circ :port :in}}
    {:from {:tile :grower-hungry :port :false} :to {:tile :food-ok :port :in}}
    ;; food-ok: true means food IS low (<=2), so move to reposition is less useful → eat
    ;; false means food > 2, so move to reposition
    {:from {:tile :food-ok :port :true}        :to {:tile :do-eat :port :in}}
    {:from {:tile :food-ok :port :false}       :to {:tile :do-move-repo :port :in}}]})

;; ── confirm sub-diagram ─────────────────────────────────────────────────────
;; At :choose-action, if growers are starving override to circulate.

(def confirm-diagram
  {:name "confirm"
   :color "#1e5a3a"
   :collapsed? false
   :origin [40 40]
   :start-tile :start
   :tiles
   {:start          {:id :start :kind :start :type :start
                     :pos [40 20] :params {}}
    :grower-hungry  {:id :grower-hungry :kind :condition :type :grower-starving?
                     :pos [40 110] :params {}}
    :do-circ        {:id :do-circ :kind :effect :type :pick-action
                     :pos [240 60] :params {:action :circulate}}
    :confirm-action {:id :confirm-action :kind :effect :type :pick-choose-action-best
                     :pos [240 200] :params {}}}
   :links
   [{:from {:tile :start :port :out}            :to {:tile :grower-hungry :port :in}}
    {:from {:tile :grower-hungry :port :true}   :to {:tile :do-circ :port :in}}
    {:from {:tile :grower-hungry :port :false}  :to {:tile :confirm-action :port :in}}]})

;; ── main: phase dispatcher ──────────────────────────────────────────────────

(def main-diagram
  {:name "main"
   :color "#1e3a5a"
   :collapsed? false
   :origin [60 40]
   :start-tile :start
   :tiles
   {:start          {:id :start :kind :start :type :start
                     :pos [40 20] :params {}}
    :p-action-type  {:id :p-action-type :kind :condition :type :phase-is?
                     :pos [40 100] :params {:phase :choose-action-type}}
    :p-action       {:id :p-action :kind :condition :type :phase-is?
                     :pos [40 200] :params {:phase :choose-action}}
    :p-move-from    {:id :p-move-from :kind :condition :type :phase-is?
                     :pos [40 300] :params {:phase :move-from}}
    :p-move-to      {:id :p-move-to :kind :condition :type :phase-is?
                     :pos [40 400] :params {:phase :move-to}}
    :p-grow-elem    {:id :p-grow-elem :kind :condition :type :phase-is?
                     :pos [40 500] :params {:phase :grow-element}}
    :p-grow-to      {:id :p-grow-to :kind :condition :type :phase-is?
                     :pos [40 600] :params {:phase :grow-to}}
    :p-eat-to       {:id :p-eat-to :kind :condition :type :phase-is?
                     :pos [40 700] :params {:phase :eat-to}}
    :p-eat-from     {:id :p-eat-from :kind :condition :type :phase-is?
                     :pos [40 800] :params {:phase :eat-from}}
    :p-circ-from    {:id :p-circ-from :kind :condition :type :phase-is?
                     :pos [40 900] :params {:phase :circulate-from}}
    :p-circ-to      {:id :p-circ-to :kind :condition :type :phase-is?
                     :pos [40 1000] :params {:phase :circulate-to}}
    :fallback       {:id :fallback :kind :effect :type :pick-first
                     :pos [40 1100] :params {}}

    ;; right column: effects/jumps
    :j-action-type  {:id :j-action-type :kind :jump :type :jump
                     :pos [260 100] :params {:diagram :action}}
    :j-action       {:id :j-action :kind :jump :type :jump
                     :pos [260 200] :params {:diagram :confirm}}
    :e-move-from    {:id :e-move-from :kind :effect :type :pick-move-from-best
                     :pos [260 300] :params {}}
    :e-move-to      {:id :e-move-to :kind :effect :type :pick-move-to-best
                     :pos [260 400] :params {}}
    :e-grow-elem    {:id :e-grow-elem :kind :effect :type :pick-grow-element-best
                     :pos [260 500] :params {}}
    :e-grow-to      {:id :e-grow-to :kind :effect :type :pick-grow-to-best
                     :pos [260 600] :params {}}
    :e-eat-to       {:id :e-eat-to :kind :effect :type :pick-eat-best
                     :pos [260 700] :params {}}
    :e-eat-from     {:id :e-eat-from :kind :effect :type :pick-eat-best
                     :pos [260 800] :params {}}
    :e-circ-from    {:id :e-circ-from :kind :effect :type :pick-circulate-from-best
                     :pos [260 900] :params {}}
    :e-circ-to      {:id :e-circ-to :kind :effect :type :pick-circulate-to-best
                     :pos [260 1000] :params {}}}
   :links
   ;; spine
   [{:from {:tile :start         :port :out}   :to {:tile :p-action-type :port :in}}
    {:from {:tile :p-action-type :port :false} :to {:tile :p-action      :port :in}}
    {:from {:tile :p-action      :port :false} :to {:tile :p-move-from   :port :in}}
    {:from {:tile :p-move-from   :port :false} :to {:tile :p-move-to     :port :in}}
    {:from {:tile :p-move-to     :port :false} :to {:tile :p-grow-elem   :port :in}}
    {:from {:tile :p-grow-elem   :port :false} :to {:tile :p-grow-to     :port :in}}
    {:from {:tile :p-grow-to     :port :false} :to {:tile :p-eat-to      :port :in}}
    {:from {:tile :p-eat-to      :port :false} :to {:tile :p-eat-from    :port :in}}
    {:from {:tile :p-eat-from    :port :false} :to {:tile :p-circ-from   :port :in}}
    {:from {:tile :p-circ-from   :port :false} :to {:tile :p-circ-to     :port :in}}
    {:from {:tile :p-circ-to     :port :false} :to {:tile :fallback      :port :in}}
    ;; true branches
    {:from {:tile :p-action-type :port :true}  :to {:tile :j-action-type :port :in}}
    {:from {:tile :p-action      :port :true}  :to {:tile :j-action      :port :in}}
    {:from {:tile :p-move-from   :port :true}  :to {:tile :e-move-from   :port :in}}
    {:from {:tile :p-move-to     :port :true}  :to {:tile :e-move-to     :port :in}}
    {:from {:tile :p-grow-elem   :port :true}  :to {:tile :e-grow-elem   :port :in}}
    {:from {:tile :p-grow-to     :port :true}  :to {:tile :e-grow-to     :port :in}}
    {:from {:tile :p-eat-to      :port :true}  :to {:tile :e-eat-to      :port :in}}
    {:from {:tile :p-eat-from    :port :true}  :to {:tile :e-eat-from    :port :in}}
    {:from {:tile :p-circ-from   :port :true}  :to {:tile :e-circ-from   :port :in}}
    {:from {:tile :p-circ-to     :port :true}  :to {:tile :e-circ-to     :port :in}}]})

;; ── Layout + save ───────────────────────────────────────────────────────────

;; Reuse the layout algorithm from install_obo.clj
(def sqrt3 (Math/sqrt 3))
(def hex-r 50)
(def grid-gap 16)
(def grid-r (+ hex-r grid-gap))
(def grid-dx (* 1.5 grid-r))
(def grid-dy (* grid-r sqrt3))
(def ne-step [grid-dx (- (/ grid-dy 2))])
(def se-step [grid-dx (/ grid-dy 2)])
(def s-step  [0       grid-dy])

(defn- grid->px [[col row]]
  [(* col grid-dx)
   (+ (* row grid-dy) (if (odd? col) (/ grid-dy 2) 0))])

(defn- px->grid [[x y]]
  (let [try-col (fn [c]
                  (let [yo (if (odd? c) (/ grid-dy 2) 0)
                        r  (Math/round (/ (- y yo) grid-dy))
                        [gx gy] (grid->px [c r])
                        d (+ (* (- x gx) (- x gx)) (* (- y gy) (- y gy)))]
                    {:col c :row r :dist d}))
        cl (int (Math/floor (/ x grid-dx)))
        a  (try-col cl)
        b  (try-col (inc cl))]
    (if (< (:dist a) (:dist b))
      [(:col a) (:row a)]
      [(:col b) (:row b)])))

(defn- build-adj [links]
  (reduce (fn [acc l]
            (update acc (get-in l [:from :tile])
                    (fnil conj []) {:tile (get-in l [:to :tile])
                                   :port (get-in l [:from :port])}))
          {} links))

(defn layout-diagram [diagram]
  (let [tiles    (:tiles diagram)
        links    (:links diagram)
        tile-ids (vec (keys tiles))
        start-id (or (:start-tile diagram)
                     (some (fn [[id t]] (when (= :start (:kind t)) id)) tiles)
                     (first (keys tiles)))
        adj      (build-adj links)
        edges    (vec (for [l links]
                        [(get-in l [:from :tile]) (get-in l [:to :tile]) (get-in l [:from :port])]))
        [pos _ edge-steps]
        (loop [queue [start-id], pos {start-id [0.0 0.0]}, seen #{start-id}
               false-depth {start-id 0}, edge-steps {}]
          (if (empty? queue)
            [pos seen edge-steps]
            (let [u     (first queue)
                  queue (vec (rest queue))
                  [ux uy] (get pos u [0 0])
                  u-fd  (get false-depth u 0)
                  succs (sort-by #(if (#{:false :b} (:port %)) 1 0) (get adj u []))
                  [p' q' s' fd' es']
                  (reduce (fn [[p q s fd es] {:keys [tile port]}]
                            (if (or (not (some #{tile} tile-ids)) (s tile)) [p q s fd es]
                                (let [down? (#{:false :b} port)
                                      step (cond (not down?) ne-step
                                                 (even? u-fd) s-step
                                                 :else se-step)
                                      [dx dy] step]
                                  [(assoc p tile [(+ ux dx) (+ uy dy)])
                                   (conj q tile) (conj s tile)
                                   (assoc fd tile (if down? (inc u-fd) 0))
                                   (assoc es [u tile] step)])))
                          [pos queue seen false-depth edge-steps] succs)]
              (recur q' p' s' fd' es'))))
        my (+ 200 (apply max 0 (map second (vals pos))))
        pos (reduce (fn [p id] (if (p id) p (assoc p id [0 my]))) pos tile-ids)
        k-spring 0.18 k-repulse 5000.0 min-dist (+ (* 2 hex-r) grid-gap)
        pos (loop [p pos, vel (into {} (map #(vector % [0.0 0.0]) tile-ids)), iter 0]
              (if (>= iter 60) p
                  (let [forces
                        (reduce
                         (fn [f [fi ti port]]
                           (let [[x1 y1] (get p fi [0 0]) [x2 y2] (get p ti [0 0])
                                 [idx idy] (or (get edge-steps [fi ti])
                                               (if (#{:false :b} port) se-step ne-step))
                                 ex (- (- x2 x1) idx) ey (- (- y2 y1) idy)
                                 fx (* k-spring ex) fy (* k-spring ey)]
                             (-> f (update fi (fn [[a b]] [(+ a fx) (+ b fy)]))
                                   (update ti (fn [[a b]] [(- a fx) (- b fy)])))))
                         (into {} (map #(vector % [0.0 0.0]) tile-ids)) edges)
                        forces
                        (reduce
                         (fn [f i]
                           (reduce
                            (fn [f2 j]
                              (let [[x1 y1] (get p i [0 0]) [x2 y2] (get p j [0 0])
                                    dx (- x1 x2) dy (- y1 y2) d2 (+ (* dx dx) (* dy dy) 1)
                                    d (Math/sqrt d2)]
                                (if (> d (* 3 min-dist)) f2
                                    (let [force (/ k-repulse d2)
                                          fx (* force (/ dx d)) fy (* force (/ dy d))]
                                      (-> f2 (update i (fn [[a b]] [(+ a fx) (+ b fy)]))
                                              (update j (fn [[a b]] [(- a fx) (- b fy)])))))))
                            f (subvec tile-ids (inc (.indexOf tile-ids i)))))
                         forces tile-ids)
                        [p' v']
                        (reduce
                         (fn [[pp vv] id]
                           (if (= id start-id) [pp vv]
                               (let [[fx fy] (get forces id [0 0])
                                     [vx vy] (get vv id [0 0])
                                     nvx (* 0.82 (+ vx fx)) nvy (* 0.82 (+ vy fy))
                                     [px py] (get pp id [0 0])]
                                 [(assoc pp id [(+ px nvx) (+ py nvy)]) (assoc vv id [nvx nvy])])))
                         [p vel] tile-ids)]
                    (recur p' v' (inc iter)))))
        grid (into {} (map (fn [[id xy]] [id (px->grid xy)]) pos))
        grid (reduce (fn [g id]
                       (let [[c r] (get g id)]
                         (if (some (fn [[oid [oc or']]] (and (not= oid id) (= oc c) (= or' r))) g)
                           (loop [dr 1]
                             (if (not (some (fn [[oid [oc or']]] (and (not= oid id) (= oc c) (= or' (+ r dr)))) g))
                               (assoc g id [c (+ r dr)]) (recur (inc dr))))
                           g)))
                     grid tile-ids)
        sp (grid->px (get grid start-id [0 0]))
        ox (- 40 (first sp)) oy (- 40 (second sp))
        new-tiles (reduce-kv
                   (fn [ts id gc]
                     (if-not (ts id) ts
                             (let [[px py] (grid->px gc)]
                               (assoc-in ts [id :pos] [(+ px ox) (+ py oy)]))))
                   tiles grid)]
    (assoc diagram :tiles new-tiles)))

(def obo-bot
  (let [raw {:start-diagram :main
             :diagrams {:main    main-diagram
                        :action  action-diagram
                        :confirm confirm-diagram}}]
    (update raw :diagrams
            (fn [ds] (into {} (map (fn [[k d]] [k (layout-diagram d)]) ds))))))

;; ── Entry point ─────────────────────────────────────────────────────────────

(def mongo-connection
  {:host "localhost" :port 27017 :database "organism"})

(defn -main [& _]
  (let [db   (db/connect! mongo-connection)
        name (bots-db/save-bot!
              db {:name        "OBO"
                  :game-type   "organism"
                  :owner       "prismofeverything"
                  :description "Flowchart organism bot — grows aggressively, captures opportunistically"
                  :definition  obo-bot})]
    (println "saved bot:" name "(game=organism)")
    (println "diagrams:" (mapv first (:diagrams obo-bot)))
    (System/exit 0)))
