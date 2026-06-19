(require '[organism.mongo :as db]
         '[organism.persist-eridu :as pe]
         '[eridu.game :as game]
         '[eridu.choice :as choice]
         '[clojure.string :as str])

(def conn (db/connect! {:host "localhost" :port 27017 :database "organism"}))
(def game-key "PRIEST MASTER")

(def doc (first (db/query conn :eridu-games {:key game-key})))
(def actions (pe/load-actions conn game-key))

(println "=== GAME:" game-key " actions:" (count actions) "===")
;; The actual final state as the live game recorded it (no fragile replay).
(def final (read-string (:state doc)))

;; Final scores
(println "\n=== FINAL SCORES ===")
(doseq [[pk pd] (:players final)]
  (println (format "  %-14s amity=%2d glory=%2d rep=%2d  roles=%s"
                   pk (:amity pd 0) (:glory pd 0)
                   (min (:amity pd 0) (:glory pd 0))
                   (:roles pd))))

;; Per-player temple geography (clustering + flip completion)
(println "\n=== TEMPLE GEOGRAPHY (placement spread + flip rate) ===")
(doseq [[pk pd] (:players final)]
  (let [temples (:temples pd)
        cities (keys temples)
        all-states (mapcat val temples)
        up (count (filter #{:face-up} all-states))
        down (count (filter #{:face-down} all-states))
        ;; adjacency: how many temple-cities are graph-neighbors of another temple-city
        graph (:city-graph final)
        clustered (count (filter (fn [c]
                                   (some #(contains? (get graph c #{}) %)
                                         (remove #{c} cities)))
                                 cities))]
    (println (format "  %-14s temples in %s  | face-up=%d face-down(scored)=%d | cities adjacent-to-another-temple=%d/%d"
                     pk (vec cities) up down clustered (count cities)))))

;; The move log — temple / influence / travel / sell lines
(println "\n=== FULL MOVE LOG ===")
(doseq [[i e] (map-indexed vector (:log final))]
  (when (:message e)
    (println (format "  %3d R%s [%s] %s" i (:round e) (name (or (:type e) :?)) (:message e)))))

(println "\n=== FEAT CLAIMS ===")
(println "  contest-claims:" (:contest-claims final))
(println "  contests on offer:" (mapv :id (:contests final)))

(println "\n=== INVARIANT / BUG SCAN ===")
(doseq [[pk pd] (:players final)]
  (let [res (:resources pd)
        neg-res (filter (fn [[_ v]] (neg? v)) res)
        rs (:raiders-supply pd) ts (:temples-supply pd)
        wild (:wild-points pd 0)]
    (println (format "  %-14s resources=%s raiders-supply=%s temples-supply=%s wild=%s"
                     pk res rs ts wild))
    (when (seq neg-res) (println "    !! NEGATIVE RESOURCES:" neg-res))
    (when (and rs (neg? rs)) (println "    !! NEGATIVE raiders-supply"))
    (when (and ts (neg? ts)) (println "    !! NEGATIVE temples-supply"))))
;; sells that found no demand (possible mis-offered action)
(println "\n  sell-with-no-demand log lines:")
(doseq [e (:log final)]
  (when (and (:message e) (re-find #"(?i)no sellable|no demand|already" (:message e)))
    (println "   " (:message e))))

(System/exit 0)
