(ns eridu.bench-diag
  "Stage 0 diagnostic variant of eridu.bench: 1 run x 3 gens across all
   player counts. Just enough to cross the Gen-0 → Gen-1 boundary where the
   full bench has been dying silently."
  (:require
   [eridu.bench :as b]))

(defn -main [& _args]
  (let [diag-config (assoc b/config-all
                           :gens-per-run 3
                           :total-runs 1
                           :weight-snapshot-every 1)]
    (println "=== DIAG config: 1 run x 3 gens, player-counts [1 2 3 4] ===")
    (b/run-bench! diag-config :fresh? false)
    (shutdown-agents)))
