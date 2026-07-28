(ns bench-future
  "Full-pipeline server-side benchmark for the Future game.

   Measures per-action cost of:
     - game/legal-actions (enumerate)
     - game/force-choice  (apply the chosen next-state)
     - pr-str             (server-to-wire serialization, minus :board)
     - edn read-string    (wire-to-client parse)

   Run:  lein run -m clojure.main dev/bench_future.clj"
  (:require [future.game :as g]
            [clojure.edn :as edn]))

(defn- ns->ms [x] (/ x 1e6))

(defn- bench-random-game [n-actions n-players]
  (let [state0 (g/create-game (mapv str (range n-players)))
        la  (atom 0)
        fc  (atom 0)
        prs (atom 0)
        rd  (atom 0)
        moves (atom 0)
        done? (atom false)]
    (loop [s state0 i 0]
      (cond
        (or @done? (>= i n-actions) (:winner s))
        (do (reset! done? true) s)

        :else
        (let [t0 (System/nanoTime)
              acts (g/legal-actions s)
              t1 (System/nanoTime)
              _ (swap! la + (- t1 t0))]
          (if (empty? acts)
            (do (reset! done? true) s)
            (let [[_ck th] (rand-nth (vec acts))
                  t2 (System/nanoTime)
                  nxt (g/force-choice th)
                  t3 (System/nanoTime)
                  _ (swap! fc + (- t3 t2))
                  ;; Simulate broadcast: pr-str the state minus :board
                  serialized (pr-str (dissoc nxt :board))
                  t4 (System/nanoTime)
                  _ (swap! prs + (- t4 t3))
                  parsed (edn/read-string serialized)
                  t5 (System/nanoTime)
                  _ (swap! rd + (- t5 t4))
                  _ (swap! moves inc)]
              (recur nxt (inc i)))))))
    (let [n @moves
          per (fn [total-ns] (if (pos? n) (ns->ms (/ total-ns n)) 0.0))]
      {:moves n
       :legal-actions-ms-total (ns->ms @la)
       :force-choice-ms-total  (ns->ms @fc)
       :pr-str-ms-total        (ns->ms @prs)
       :read-string-ms-total   (ns->ms @rd)
       :per-action {:legal-actions (per @la)
                    :force-choice  (per @fc)
                    :pr-str        (per @prs)
                    :read-string   (per @rd)}
       :sum-per-action-ms (per (+ @la @fc @prs @rd))})))

(defn- print-report [label result]
  (println "\n===" label "===")
  (println "moves played:" (:moves result))
  (println "totals (ms):")
  (println (format "  legal-actions:  %8.1f" (:legal-actions-ms-total result)))
  (println (format "  force-choice:   %8.1f" (:force-choice-ms-total result)))
  (println (format "  pr-str:         %8.1f" (:pr-str-ms-total result)))
  (println (format "  read-string:    %8.1f" (:read-string-ms-total result)))
  (println "per-action (μs):")
  (doseq [[k v] (:per-action result)]
    (println (format "  %-14s  %8.1f μs" (name k) (* 1000.0 v))))
  (println (format "SUM per-action:   %8.1f μs  (%.2f ms)"
                   (* 1000.0 (:sum-per-action-ms result))
                   (:sum-per-action-ms result))))

(println "\nBenchmarking Future game server pipeline…")
(println "(warm-up run first, then measured runs)")

;; Warmup
(bench-random-game 200 3)

(let [r1 (bench-random-game 200 3)
      r2 (bench-random-game 500 3)
      r3 (bench-random-game 200 5)]
  (print-report "200 actions, 3 players" r1)
  (print-report "500 actions, 3 players" r2)
  (print-report "200 actions, 5 players" r3))

(println "\ndone.")
