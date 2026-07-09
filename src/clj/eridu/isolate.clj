(ns eridu.isolate
  "Isolate-one-gene A/B harness for Eridu — ported from the Chroma lesson.

   The transferable insight (chroma.md §21): a single gene's true value is best
   measured by a *controlled paired-seed A/B that isolates that one gene* against
   a fixed baseline — NOT free-for-all arena win-rate, where a strong backbone
   confounds the read (in Chroma, early-swap looked top-tier in the arena but was
   neutral-to-negative once isolated).

   Method here, mirroring that design:
     • Baseline B = a fixed reference personality (default = neutral default-weights,
       or the current champion from the evolved population via `champion`).
     • Variant V = B with exactly ONE gene overridden to a test value.
     • For each seed s: run a P-player game with the variant in seat 0 vs (P-1)
       plain baselines, AND a control game with B in seat 0 vs the same baselines,
       under the SAME seed. Same seed ⇒ identical board, dice, draws, bag — so the
       only thing that moves seat-0's win-share is the single gene. Pairing on the
       seed cancels board variance (variance reduction), exactly like chroma's
       3,000 paired seeds.
     • Δ = mean(variant seat-0 win-share) − mean(control seat-0 win-share).

   Determinism: Eridu's engine uses clojure.core/rand* and shuffle, which are not
   seedable out of the box. We rebind them to a seeded java.util.Random for the
   extent of each game (single-threaded), making seeds reproducible.

   Run:  lein run -m eridu.isolate              ;; default table, N=150, P=3
         lein run -m eridu.isolate 300 4         ;; N=300 seeds, 4-player
         lein run -m eridu.isolate 300 3 champion ;; baseline = evolved champion"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [eridu.personality :as pers]
   [eridu.simulate :as sim]))

;; =============================================================================
;; Seeded determinism — rebind rand/shuffle to a per-game seeded RNG
;; =============================================================================

(def ^:dynamic *rng* nil)

;; NOTE: clojure.core is compiled with :direct-linking, so rand-int and rand-nth
;; call rand via a *direct* static invoke that ignores a with-redefs on #'rand.
;; The engine calls rand-int / rand-nth / shuffle directly, so we must rebind
;; exactly those three (plus rand for completeness).

(defn- s-rand
  ([] (if-let [^java.util.Random r *rng*] (.nextDouble r) (clojure.core/rand)))
  ([n] (* n (s-rand))))

(defn- s-rand-int [n]
  (if-let [^java.util.Random r *rng*] (int (* n (.nextDouble r))) (clojure.core/rand-int n)))

(defn- s-rand-nth [coll]
  (if *rng* (nth coll (s-rand-int (count coll))) (clojure.core/rand-nth coll)))

(defn- s-shuffle [coll]
  (if-let [^java.util.Random r *rng*]
    (let [al (java.util.ArrayList. ^java.util.Collection coll)]
      (java.util.Collections/shuffle al r)
      (vec al))
    (clojure.core/shuffle coll)))

(defn run-seeded
  "Run one game under a fixed seed with fully deterministic RNG."
  [configs seed]
  (binding [*rng* (java.util.Random. seed)]
    (with-redefs [rand     s-rand
                  rand-int s-rand-int
                  rand-nth s-rand-nth
                  shuffle  s-shuffle]
      (sim/run-game configs :seed seed))))

;; =============================================================================
;; Win-share scoring (seat 0)
;; =============================================================================

(defn seat0-winshare
  "Fraction of the win credited to seat 0 (the first config) under tie-splitting.
   Winner = max reputation (= min(amity,glory)). Ties split the win evenly.
   Neutral expectation = 1/P."
  [summary]
  (let [reps    (map :reputation summary)
        mx      (apply max reps)
        winners (count (filter #(= % mx) reps))
        seat0   (:reputation (first summary))]
    (if (= seat0 mx) (/ 1.0 winners) 0.0)))

;; =============================================================================
;; Baselines and configs
;; =============================================================================

(def population-path "output/bench/evolved-population.edn")

(defn champion
  "Top evolved personality from the persisted bench population, or nil."
  []
  (when (.exists (io/file population-path))
    (try
      (let [orgs (:organisms (edn/read-string (slurp population-path)))]
        (some-> (->> orgs (sort-by #(- (:elo % 0))) first :personality)
                (assoc :name "Champion")))
      (catch Exception _ nil))))

(defn- mk-configs
  "P seats: seat 0 = seat0-weights, seats 1..P-1 = baseline."
  [p seat0-weights baseline]
  (into [{:key :p0 :personality seat0-weights}]
        (for [i (range 1 p)]
          {:key (keyword (str "p" i))
           :personality (assoc baseline :name (str "Base" i))})))

(defn variant
  "Baseline with one gene overridden."
  [baseline gene value]
  (assoc baseline gene value :name (str "V-" (name gene) "=" value)))

;; =============================================================================
;; Paired-seed isolate
;; =============================================================================

(defn isolate-gene
  "Paired-seed A/B for a single gene at a single value.
   Returns {:gene :value :n :variant-ws :baseline-ws :delta :se}."
  [{:keys [baseline p n gene value seeds]}]
  (let [v-weights (variant baseline gene value)
        deltas
        (mapv (fn [seed]
                (let [v-cfg (mk-configs p v-weights baseline)
                      b-cfg (mk-configs p baseline baseline)
                      v-sum (sim/game-result-summary (run-seeded v-cfg seed) v-cfg)
                      b-sum (sim/game-result-summary (run-seeded b-cfg seed) b-cfg)]
                  [(seat0-winshare v-sum) (seat0-winshare b-sum)]))
              seeds)
        v-mean (/ (reduce + (map first deltas)) (double n))
        b-mean (/ (reduce + (map second deltas)) (double n))
        diffs  (map (fn [[v b]] (- v b)) deltas)
        d-mean (/ (reduce + diffs) (double n))
        var    (/ (reduce + (map #(let [x (- % d-mean)] (* x x)) diffs))
                  (double (max 1 (dec n))))
        se     (Math/sqrt (/ var (double n)))]
    {:gene gene :value value :n n
     :variant-ws v-mean :baseline-ws b-mean :delta d-mean :se se}))

;; =============================================================================
;; The trait test table — the genes we most need to read cleanly
;; =============================================================================

(def test-table
  "Each gene tested at a LOW and HIGH setting vs the neutral baseline.
   Chosen for strategic centrality + how often they confound arena reads."
  [{:gene :feat-awareness        :values [0.05 0.9] :note "steer toward fulfilling feats"}
   {:gene :tempo                  :values [0.05 0.9] :note "claim feats now vs hold for timing"}
   {:gene :feat-rush             :values [0.05 0.9] :note "rush claims vs steady build"}
   {:gene :feat-closure-urgency  :values [0.1  0.9] :note "prioritize near-complete feats"}
   {:gene :score-balance-target  :values [0.2  0.8] :note "amity-tilt vs glory-tilt"}
   {:gene :glory-path            :values [0.1  0.9] :note "raiders vs role-5 endgame bonuses"}
   {:gene :early-role-bias       :values [0.3  1.0] :note "early role investment"}
   {:gene :endgame-role-push     :values [0.1  0.9] :note "round-3 role priority"}
   {:gene :contest-focus         :values [0.05 0.9] :note "steer toward contest conditions"}
   {:gene :board-exploitation    :values [0.1  0.9] :note "uncover bonus board slots"}
   {:gene :competitive-roles     :values [0.1  0.9] :note "race opponents for roles"}
   {:gene :resource-hoard        :values [0.05 0.9] :note "avoid spending resources"}])

(defn run-table
  [{:keys [baseline p n]}]
  (let [seeds (vec (range 1 (inc n)))
        rows  (for [{:keys [gene values note]} test-table
                    value values]
                (let [r (isolate-gene {:baseline baseline :p p :n n
                                       :gene gene :value value :seeds seeds})]
                  (println (format "  %-22s = %-4s  varWS=%.3f  Δ=%+.3f  ±%.3f%s"
                                   (name gene) (str value)
                                   (:variant-ws r) (:delta r) (* 1.96 (:se r))
                                   (let [z (if (pos? (:se r)) (/ (Math/abs (:delta r)) (:se r)) 0)]
                                     (cond (>= z 2.58) "  ***"
                                           (>= z 1.96) "  **"
                                           (>= z 1.64) "  *"
                                           :else ""))))
                  (assoc r :note note)))]
    (vec rows)))

;; =============================================================================
;; Entry point
;; =============================================================================

(defn probe!
  "Determinism check: same config+seed twice must yield identical reputations."
  []
  (let [cfg (mk-configs 3 pers/default-weights pers/default-weights)]
    (doseq [seed [1 2 3]]
      (let [r1 (mapv :reputation (sim/game-result-summary (run-seeded cfg seed) cfg))
            r2 (mapv :reputation (sim/game-result-summary (run-seeded cfg seed) cfg))]
        (println (format "  seed %d: run1=%s run2=%s  %s"
                         seed r1 r2 (if (= r1 r2) "OK" "*** NONDETERMINISTIC")))))
    (shutdown-agents)))

(defn -main [& args]
  (when (= (first args) "probe") (probe!) (System/exit 0))
  (let [n        (or (some-> (first args) Integer/parseInt) 150)
        p        (or (some-> (second args) Integer/parseInt) 3)
        base-arg (nth args 2 nil)
        baseline (if (= base-arg "champion")
                   (or (champion)
                       (do (println "  (no champion found; using neutral baseline)")
                           pers/default-weights))
                   pers/default-weights)
        t0       (System/currentTimeMillis)]
    (println "Eridu isolate-one-gene A/B harness")
    (println "==================================")
    (println (format "  baseline=%s  players=%d  paired-seeds=%d  (neutral win-share=%.3f)"
                     (:name baseline "Default") p n (/ 1.0 p)))
    (println (format "  %d genes × 2 settings = %d isolates, %d games total"
                     (count test-table) (* 2 (count test-table))
                     (* 2 (count test-table) n 2)))
    (println "  Δ = variant seat-0 win-share − baseline seat-0 win-share (paired). * p<.1 ** p<.05 *** p<.01")
    (println "")
    (let [rows    (run-table {:baseline baseline :p p :n n})
          sorted  (sort-by #(- (:delta %)) rows)
          elapsed (/ (- (System/currentTimeMillis) t0) 1000.0)
          out     {:baseline (:name baseline "Default")
                   :players p :paired-seeds n
                   :neutral-win-share (/ 1.0 p)
                   :elapsed-seconds elapsed
                   :rows (mapv (fn [r] (-> r (update :gene name))) sorted)}]
      (.mkdirs (io/file "output/bench"))
      (spit "output/bench/isolate-results.edn" (pr-str out))
      ;; lightweight JSON for the email/design log
      (spit "output/bench/isolate-results.json"
            (str "{\n"
                 "  \"baseline\": \"" (:name baseline "Default") "\",\n"
                 "  \"players\": " p ", \"pairedSeeds\": " n
                 ", \"neutralWinShare\": " (format "%.3f" (/ 1.0 p)) ",\n"
                 "  \"rows\": [\n"
                 (str/join ",\n"
                   (for [r sorted]
                     (format "    {\"gene\": \"%s\", \"value\": %s, \"varWS\": %.3f, \"delta\": %+.4f, \"ci95\": %.4f}"
                             (name (:gene r)) (:value r) (:variant-ws r) (:delta r) (* 1.96 (:se r)))))
                 "\n  ]\n}\n"))
      (println "")
      (println (format "=== Done in %.0fs. Top movers (by |Δ| win-share) ===" elapsed))
      (doseq [r (take 6 (sort-by #(- (Math/abs (:delta %))) rows))]
        (println (format "  %-22s = %-4s  Δ=%+.3f ±%.3f"
                         (name (:gene r)) (str (:value r)) (:delta r) (* 1.96 (:se r)))))
      (println "")
      (println "  Wrote output/bench/isolate-results.edn + .json")
      (shutdown-agents))))
