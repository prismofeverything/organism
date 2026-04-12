(ns organism.scripts.install-obo
  "Install the journey OBO bot — a flowchart translation of the hard-coded
   `organism.routes.journey/agent-step` heuristic — and save it under owner
   'prismofeverything'.

   Run with:  lein with-profile uberjar run -m organism.scripts.install-obo

   The flowchart has four diagrams:
     :main        — phase dispatcher (every phase the hard-coded bot handles)
     :action      — :choose-action-type strategy with visible branching
     :convert     — :choose-convert strategy with visible branching
     :pick-target — landings → beacons → near-landable, used for any phase
                    that picks a hex position by simple proximity

   Phases with no special branching jump directly to a per-phase 'best-of'
   effect that internalizes the hard-coded heuristic verbatim."
  (:require
   [organism.mongo :as db]
   [organism.persist-journey-bots :as bots-db]))

;; ── main: full phase dispatcher ─────────────────────────────────────────────

(def main-diagram
  {:name        "main"
   :color       "#1e3a5a"
   :collapsed?  false
   :origin      [60 40]
   :start-tile  :start
   :tiles
   {:start          {:id :start :kind :start :type :start
                     :pos [40 20]   :params {}}
    :p-action       {:id :p-action :kind :condition :type :phase-is?
                     :pos [40 100]  :params {:phase :choose-action-type}}
    :p-convert      {:id :p-convert :kind :condition :type :phase-is?
                     :pos [40 200]  :params {:phase :choose-convert}}
    :p-move         {:id :p-move :kind :condition :type :phase-is?
                     :pos [40 300]  :params {:phase :choose-move}}
    :p-launch       {:id :p-launch :kind :condition :type :phase-is?
                     :pos [40 400]  :params {:phase :choose-launch-destination}}
    :p-fly-from     {:id :p-fly-from :kind :condition :type :phase-is?
                     :pos [40 500]  :params {:phase :choose-fly-from}}
    :p-fly-to       {:id :p-fly-to :kind :condition :type :phase-is?
                     :pos [40 600]  :params {:phase :choose-fly-to}}
    :p-self-bonus   {:id :p-self-bonus :kind :condition :type :phase-is?
                     :pos [40 700]  :params {:phase :choose-activate-self-bonus}}
    :p-owner-bonus  {:id :p-owner-bonus :kind :condition :type :phase-is?
                     :pos [40 800]  :params {:phase :choose-activate-owner-bonus}}
    :p-tower-join   {:id :p-tower-join :kind :condition :type :phase-is?
                     :pos [40 900]  :params {:phase :choose-activate-tower-join}}
    :p-tower-head   {:id :p-tower-head :kind :condition :type :phase-is?
                     :pos [40 1000] :params {:phase :choose-activate-tower-heading}}
    :p-act-station  {:id :p-act-station :kind :condition :type :phase-is?
                     :pos [40 1100] :params {:phase :choose-activate-station}}
    :p-matrix-bcn   {:id :p-matrix-bcn :kind :condition :type :phase-is?
                     :pos [40 1200] :params {:phase :choose-activate-matrix-beacon}}
    :p-land         {:id :p-land :kind :condition :type :phase-is?
                     :pos [40 1300] :params {:phase :choose-land}}
    :p-cipher       {:id :p-cipher :kind :condition :type :phase-is?
                     :pos [40 1400] :params {:phase :cipher}}
    :p-ark          {:id :p-ark :kind :condition :type :phase-is?
                     :pos [40 1500] :params {:phase :choose-ark-advance}}
    :p-flare        {:id :p-flare :kind :condition :type :phase-is?
                     :pos [40 1600] :params {:phase :choose-flare-advance}}
    :p-drift-flare  {:id :p-drift-flare :kind :condition :type :phase-is?
                     :pos [40 1700] :params {:phase :choose-drift-flare-advance}}
    :p-drift        {:id :p-drift :kind :condition :type :phase-is?
                     :pos [40 1800] :params {:phase :choose-captain-drift}}
    :p-keep-card    {:id :p-keep-card :kind :condition :type :phase-is?
                     :pos [40 1900] :params {:phase :keep-card}}
    :p-flare-join   {:id :p-flare-join :kind :condition :type :phase-is?
                     :pos [40 2000] :params {:phase :flare-beacon-join}}
    :p-cap-join     {:id :p-cap-join :kind :condition :type :phase-is?
                     :pos [40 2100] :params {:phase :captain-beacon-join}}
    :fallback       {:id :fallback :kind :effect :type :pick-first
                     :pos [40 2200] :params {}}

    ;; right column: terminal effects / jumps for each :true branch
    :j-action       {:id :j-action :kind :jump :type :jump
                     :pos [260 100] :params {:diagram :action}}
    :j-convert      {:id :j-convert :kind :jump :type :jump
                     :pos [260 200] :params {:diagram :convert}}
    :e-move         {:id :e-move :kind :effect :type :pick-move-best
                     :pos [260 300] :params {}}
    :e-launch       {:id :e-launch :kind :effect :type :pick-launch-best
                     :pos [260 400] :params {}}
    :e-fly-from     {:id :e-fly-from :kind :effect :type :pick-fly-from-best
                     :pos [260 500] :params {}}
    :e-fly-to       {:id :e-fly-to :kind :effect :type :pick-fly-to-best
                     :pos [260 600] :params {}}
    :e-self-bonus   {:id :e-self-bonus :kind :effect :type :take-max-bonus
                     :pos [260 700] :params {}}
    :e-owner-bonus  {:id :e-owner-bonus :kind :effect :type :take-max-bonus
                     :pos [260 800] :params {}}
    :e-tower-join   {:id :e-tower-join :kind :effect :type :pick-tower-join-best
                     :pos [260 900] :params {}}
    :e-tower-head   {:id :e-tower-head :kind :effect :type :pick-position-closest
                     :pos [260 1000] :params {}}
    :e-act-station  {:id :e-act-station :kind :effect :type :pick-activate-station-first
                     :pos [260 1100] :params {}}
    :e-matrix-bcn   {:id :e-matrix-bcn :kind :effect :type :pick-matrix-beacon-best
                     :pos [260 1200] :params {}}
    :e-land         {:id :e-land :kind :effect :type :pick-named
                     :pos [260 1300] :params {:choice :land}}
    :e-cipher       {:id :e-cipher :kind :effect :type :pick-cipher-best
                     :pos [260 1400] :params {}}
    :e-ark          {:id :e-ark :kind :effect :type :pick-ark-advance-best
                     :pos [260 1500] :params {}}
    :e-flare        {:id :e-flare :kind :effect :type :pick-named
                     :pos [260 1600] :params {:choice :direct}}
    :e-drift-flare  {:id :e-drift-flare :kind :effect :type :pick-named
                     :pos [260 1700] :params {:choice :direct}}
    :e-drift        {:id :e-drift :kind :effect :type :pick-position-closest
                     :pos [260 1800] :params {}}
    :e-keep-card    {:id :e-keep-card :kind :effect :type :pick-keep-card-best
                     :pos [260 1900] :params {}}
    :e-flare-join   {:id :e-flare-join :kind :effect :type :pick-tower-join-best
                     :pos [260 2000] :params {}}
    :e-cap-join     {:id :e-cap-join :kind :effect :type :pick-tower-join-best
                     :pos [260 2100] :params {}}}
   :links
   ;; spine — each :false continues to the next phase check
   [{:from {:tile :start          :port :out}   :to {:tile :p-action       :port :in}}
    {:from {:tile :p-action       :port :false} :to {:tile :p-convert      :port :in}}
    {:from {:tile :p-convert      :port :false} :to {:tile :p-move         :port :in}}
    {:from {:tile :p-move         :port :false} :to {:tile :p-launch       :port :in}}
    {:from {:tile :p-launch       :port :false} :to {:tile :p-fly-from     :port :in}}
    {:from {:tile :p-fly-from     :port :false} :to {:tile :p-fly-to       :port :in}}
    {:from {:tile :p-fly-to       :port :false} :to {:tile :p-self-bonus   :port :in}}
    {:from {:tile :p-self-bonus   :port :false} :to {:tile :p-owner-bonus  :port :in}}
    {:from {:tile :p-owner-bonus  :port :false} :to {:tile :p-tower-join   :port :in}}
    {:from {:tile :p-tower-join   :port :false} :to {:tile :p-tower-head   :port :in}}
    {:from {:tile :p-tower-head   :port :false} :to {:tile :p-act-station  :port :in}}
    {:from {:tile :p-act-station  :port :false} :to {:tile :p-matrix-bcn   :port :in}}
    {:from {:tile :p-matrix-bcn   :port :false} :to {:tile :p-land         :port :in}}
    {:from {:tile :p-land         :port :false} :to {:tile :p-cipher       :port :in}}
    {:from {:tile :p-cipher       :port :false} :to {:tile :p-ark          :port :in}}
    {:from {:tile :p-ark          :port :false} :to {:tile :p-flare        :port :in}}
    {:from {:tile :p-flare        :port :false} :to {:tile :p-drift-flare  :port :in}}
    {:from {:tile :p-drift-flare  :port :false} :to {:tile :p-drift        :port :in}}
    {:from {:tile :p-drift        :port :false} :to {:tile :p-keep-card    :port :in}}
    {:from {:tile :p-keep-card    :port :false} :to {:tile :p-flare-join   :port :in}}
    {:from {:tile :p-flare-join   :port :false} :to {:tile :p-cap-join     :port :in}}
    {:from {:tile :p-cap-join     :port :false} :to {:tile :fallback       :port :in}}

    ;; each :true → its action / jump
    {:from {:tile :p-action       :port :true}  :to {:tile :j-action       :port :in}}
    {:from {:tile :p-convert      :port :true}  :to {:tile :j-convert      :port :in}}
    {:from {:tile :p-move         :port :true}  :to {:tile :e-move         :port :in}}
    {:from {:tile :p-launch       :port :true}  :to {:tile :e-launch       :port :in}}
    {:from {:tile :p-fly-from     :port :true}  :to {:tile :e-fly-from     :port :in}}
    {:from {:tile :p-fly-to       :port :true}  :to {:tile :e-fly-to       :port :in}}
    {:from {:tile :p-self-bonus   :port :true}  :to {:tile :e-self-bonus   :port :in}}
    {:from {:tile :p-owner-bonus  :port :true}  :to {:tile :e-owner-bonus  :port :in}}
    {:from {:tile :p-tower-join   :port :true}  :to {:tile :e-tower-join   :port :in}}
    {:from {:tile :p-tower-head   :port :true}  :to {:tile :e-tower-head   :port :in}}
    {:from {:tile :p-act-station  :port :true}  :to {:tile :e-act-station  :port :in}}
    {:from {:tile :p-matrix-bcn   :port :true}  :to {:tile :e-matrix-bcn   :port :in}}
    {:from {:tile :p-land         :port :true}  :to {:tile :e-land         :port :in}}
    {:from {:tile :p-cipher       :port :true}  :to {:tile :e-cipher       :port :in}}
    {:from {:tile :p-ark          :port :true}  :to {:tile :e-ark          :port :in}}
    {:from {:tile :p-flare        :port :true}  :to {:tile :e-flare        :port :in}}
    {:from {:tile :p-drift-flare  :port :true}  :to {:tile :e-drift-flare  :port :in}}
    {:from {:tile :p-drift        :port :true}  :to {:tile :e-drift        :port :in}}
    {:from {:tile :p-keep-card    :port :true}  :to {:tile :e-keep-card    :port :in}}
    {:from {:tile :p-flare-join   :port :true}  :to {:tile :e-flare-join   :port :in}}
    {:from {:tile :p-cap-join     :port :true}  :to {:tile :e-cap-join     :port :in}}]})

;; ── action sub-diagram ──────────────────────────────────────────────────────
;; Mirrors the hard-coded :choose-action-type strategy:
;;   need-foundry? = sundivers-low? AND has-foundry-conversion?  → convert
;;   has-stations?                                                → activate
;;   has-conversion? AND not skip-convert-for-tower?              → convert
;;   else                                                         → move
;;
;; The "skip-convert-for-tower?" lookahead is not expressible with current
;; vocabulary; the divergence is rare and is verified by the comparison harness.

(def action-diagram
  {:name "action"
   :color "#3a1e5a"
   :collapsed? false
   :origin [560 40]
   :start-tile :start
   :tiles
   {:start         {:id :start :kind :start :type :start
                    :pos [40 20] :params {}}
    :sundivers-low {:id :sundivers-low :kind :condition :type :sundivers-low?
                    :pos [40 110] :params {:compare :<= :threshold 4}}
    :has-foundry   {:id :has-foundry :kind :condition :type :has-conversion?
                    :pos [240 60]  :params {:type :foundry}}
    :do-convert-1  {:id :do-convert-1 :kind :effect :type :pick-action
                    :pos [440 60]  :params {:action :convert}}
    :has-stations  {:id :has-stations :kind :condition :type :has-stations?
                    :pos [240 200] :params {:type :any}}
    :do-activate   {:id :do-activate :kind :effect :type :pick-action
                    :pos [440 180] :params {:action :activate}}
    :move-sets-up  {:id :move-sets-up :kind :condition :type :move-sets-up-tower?
                    :pos [240 320] :params {}}
    :has-conv      {:id :has-conv :kind :condition :type :has-conversion?
                    :pos [440 380] :params {:type :any}}
    :do-convert-2  {:id :do-convert-2 :kind :effect :type :pick-action
                    :pos [640 360] :params {:action :convert}}
    :do-move       {:id :do-move :kind :effect :type :pick-action
                    :pos [640 460] :params {:action :move}}}
   :links
   [{:from {:tile :start :port :out}            :to {:tile :sundivers-low :port :in}}
    {:from {:tile :sundivers-low :port :true}   :to {:tile :has-foundry :port :in}}
    {:from {:tile :sundivers-low :port :false}  :to {:tile :has-stations :port :in}}
    {:from {:tile :has-foundry :port :true}     :to {:tile :do-convert-1 :port :in}}
    {:from {:tile :has-foundry :port :false}    :to {:tile :has-stations :port :in}}
    {:from {:tile :has-stations :port :true}    :to {:tile :do-activate :port :in}}
    {:from {:tile :has-stations :port :false}   :to {:tile :move-sets-up :port :in}}
    {:from {:tile :move-sets-up :port :true}    :to {:tile :do-move :port :in}}
    {:from {:tile :move-sets-up :port :false}   :to {:tile :has-conv :port :in}}
    {:from {:tile :has-conv :port :true}        :to {:tile :do-convert-2 :port :in}}
    {:from {:tile :has-conv :port :false}       :to {:tile :do-move :port :in}}]})

;; ── convert sub-diagram ─────────────────────────────────────────────────────
;; Mirrors the hard-coded :choose-convert strategy:
;;   sundivers-low? → foundry > tower > matrix
;;   has-beacons?   → tower   > matrix > foundry
;;   else           → matrix  > tower  > foundry

(def convert-diagram
  {:name "convert"
   :color "#1e5a3a"
   :collapsed? false
   :origin [560 600]
   :start-tile :start
   :tiles
   {:start         {:id :start :kind :start :type :start
                    :pos [40 20] :params {}}
    :sundivers-low {:id :sundivers-low :kind :condition :type :sundivers-low?
                    :pos [40 110] :params {:compare :<= :threshold 4}}
    :prefer-found  {:id :prefer-found :kind :effect :type :pick-convert
                    :pos [240 60]
                    :params {:prefer-1 :foundry :prefer-2 :tower :prefer-3 :matrix}}
    :has-beacons   {:id :has-beacons :kind :condition :type :has-beacons?
                    :pos [240 200] :params {}}
    :prefer-tower  {:id :prefer-tower :kind :effect :type :pick-convert
                    :pos [440 160]
                    :params {:prefer-1 :tower :prefer-2 :matrix :prefer-3 :foundry}}
    :prefer-matrix {:id :prefer-matrix :kind :effect :type :pick-convert
                    :pos [440 280]
                    :params {:prefer-1 :matrix :prefer-2 :tower :prefer-3 :foundry}}}
   :links
   [{:from {:tile :start :port :out}           :to {:tile :sundivers-low :port :in}}
    {:from {:tile :sundivers-low :port :true}  :to {:tile :prefer-found :port :in}}
    {:from {:tile :sundivers-low :port :false} :to {:tile :has-beacons :port :in}}
    {:from {:tile :has-beacons :port :true}    :to {:tile :prefer-tower :port :in}}
    {:from {:tile :has-beacons :port :false}   :to {:tile :prefer-matrix :port :in}}]})

;; ── Server-side auto-layout (mirrors the cljs algorithm) ────────────────────

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
        ;; Phase 1: BFS pixel placement — staircase false chains (S, SE, S, SE...)
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
                                      step  (cond (not down?) ne-step
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
        ;; Phase 2: force-directed
        k-spring 0.18, k-repulse 5000.0, min-dist (+ (* 2 hex-r) grid-gap)
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
        ;; Phase 3: snap to hex grid
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
                        :convert convert-diagram}}]
    (update raw :diagrams
            (fn [ds] (into {} (map (fn [[k d]] [k (layout-diagram d)]) ds))))))

;; ── Entry point ─────────────────────────────────────────────────────────────

(def mongo-connection
  {:host "localhost"
   :port 27017
   :database "organism"})

(defn -main [& _]
  (let [db (db/connect! mongo-connection)
        name (bots-db/save-bot!
              db {:name        "OBO"
                  :game-type   "journey"
                  :owner       "prismofeverything"
                  :description "Flowchart translation of the hard-coded journey heuristic"
                  :definition  obo-bot})]
    (println "saved bot:" name)
    (println "diagrams:" (mapv first (:diagrams obo-bot)))
    (println "main tiles:" (count (:tiles main-diagram)))
    (System/exit 0)))
