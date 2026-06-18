(ns eridu-lookahead-ab
  "A/B validation for the :feat-lookahead horizon fix.

   Each game seats 4 copies of the SAME evolved baseline personality, differing
   ONLY in the :feat-lookahead gene (2 treatment seats at a positive value, 2
   control seats at 0.0). Treatment/control seat positions swap on alternate
   games to cancel any seat/turn-order bias. We aggregate feats-claimed and
   reputation by condition — isolating the causal effect of the forecast term.

   Run: lein run -m clojure.main eridu_lookahead_ab.clj [N-GAMES] [LOOKAHEAD]"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [eridu.simulate :as sim]
   [eridu.personality :as pers]))

(defn- base-personality []
  (let [data (edn/read-string (slurp (io/resource "eridu/evolved-baseline.edn")))]
    (->> (:organisms data) (sort-by :elo >) first :personality)))

(defn- mean [xs] (if (seq xs) (double (/ (reduce + xs) (count xs))) 0.0))

(defn -main [& args]
  (let [n        (Integer/parseInt (or (first args) "300"))
        la       (Double/parseDouble (or (second args) "0.8"))
        base     (or (base-personality) pers/default-weights)
        control  (assoc base :feat-lookahead 0.0)
        treat    (assoc base :feat-lookahead la)
        outfile  "/home/m/organism/eridu-lookahead-ab-results.txt"]
    (println (format "A/B: %d games, lookahead=%.2f, base=%s" n la (:name base "?")))
    (loop [g 0
           t-feats [] c-feats []
           t-rep [] c-rep []
           t-wins 0 c-wins 0]
      (if (>= g n)
        (let [summary
              (format
               (str "=== Eridu :feat-lookahead A/B ===\n"
                    "games=%d  lookahead=%.2f  base-personality=%s\n"
                    "seats: 2 treatment vs 2 control per game (positions swapped each game)\n\n"
                    "FEATS CLAIMED / seat-game   treatment=%.3f   control=%.3f   (lift %+.1f%%)\n"
                    "REPUTATION    / seat-game   treatment=%.3f   control=%.3f   (lift %+.1f%%)\n"
                    "GAME WINS (by reputation)   treatment=%d      control=%d\n\n"
                    "n treatment seat-games=%d  n control seat-games=%d\n")
               n la (:name base "?")
               (mean t-feats) (mean c-feats)
               (if (pos? (mean c-feats)) (* 100.0 (dec (/ (mean t-feats) (max 1e-9 (mean c-feats))))) 0.0)
               (mean t-rep) (mean c-rep)
               (if (pos? (mean c-rep)) (* 100.0 (dec (/ (mean t-rep) (max 1e-9 (mean c-rep))))) 0.0)
               t-wins c-wins
               (count t-feats) (count c-feats))]
          (spit outfile summary)
          (println summary)
          (println "Wrote" outfile))
        (let [;; swap treatment seats each game to cancel seat bias
              even? (zero? (mod g 2))
              configs [{:key :p1 :personality (if even? treat control)}
                       {:key :p2 :personality (if even? control treat)}
                       {:key :p3 :personality (if even? treat control)}
                       {:key :p4 :personality (if even? control treat)}]
              treat-keys (set (for [c configs
                                    :when (= la (:feat-lookahead (:personality c)))]
                                (:key c)))
              res  (sim/run-game configs)
              rows (sim/game-result-summary res configs)
              t-rows (filter #(treat-keys (:player %)) rows)
              c-rows (remove #(treat-keys (:player %)) rows)
              best-rep (apply max (map :reputation rows))
              t-won? (some #(= best-rep (:reputation %)) t-rows)
              c-won? (some #(= best-rep (:reputation %)) c-rows)]
          (when (zero? (mod g 50)) (println "  game" g))
          (recur (inc g)
                 (into t-feats (map :feats-claimed t-rows))
                 (into c-feats (map :feats-claimed c-rows))
                 (into t-rep (map :reputation t-rows))
                 (into c-rep (map :reputation c-rows))
                 (+ t-wins (if t-won? 1 0))
                 (+ c-wins (if c-won? 1 0))))))))

(-main)
