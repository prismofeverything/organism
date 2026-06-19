(require '[organism.mongo :as db] '[clojure.string :as str])
(def conn (db/connect! {:host "localhost" :port 27017 :database "organism"}))

(defn analyze [game-key]
  (let [doc (first (db/query conn :eridu-games {:key game-key}))
        final (read-string (:state doc))
        log (:log final)]
    (println "\n══════════════════════════════════════════════════════")
    (println "GAME:" game-key)
    (println "final scores:")
    (doseq [[pk pd] (:players final)]
      (println (format "  %-14s amity=%2d glory=%2d rep=%2d wild=%2d  claims=%s"
                       pk (:amity pd 0) (:glory pd 0)
                       (min (:amity pd 0) (:glory pd 0)) (:wild-points pd 0)
                       (vec (for [[cid claimers] (:contest-claims final)
                                  :when (some #{pk} claimers)] cid)))))
    (println "contests on offer:" (mapv :id (:contests final)))
    ;; Chronological log of the chaining-relevant events, with running round
    (println "\n  -- fzghoul-relevant chain (claims, bonus effects, the sells/temples/influence around them) --")
    (doseq [e log]
      (let [t (:type e) msg (:message e)]
        (when (and msg (#{:feat-claim :bonus-effect :sell :temple :temple-visit
                          :influence :deploy :raider-flip :travel} t))
          ;; mark claims/bonus prominently
          (let [tag (case t :feat-claim ">>> CLAIM " :bonus-effect "    bonus " "      ")]
            (println (format "  R%s %s%s" (:round e) tag msg))))))))

(analyze "PRIEST MASTER")
(analyze "boardtest")
(System/exit 0)
