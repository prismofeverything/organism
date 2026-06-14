(ns eridu.bench
  "Large-scale bench runner with population persistence.
   Evolves AI personalities across runs, carrying improved weights forward.
   Run with: lein run -m eridu.bench
   Options: lein run -m eridu.bench 5p       — 5-player only, 1000 gens
            lein run -m eridu.bench all       — all player counts, 1000 gens
            lein run -m eridu.bench fresh     — ignore saved population, start fresh"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [eridu.evolve :as evolve]
   [eridu.simulate :as sim]
   [eridu.personality :as pers]))

;; Output dir is run_id-namespaced when ERIDU_BENCH_OUTPUT_DIR is set by the
;; launcher, so parallel/sequential runs never overwrite each other's CSVs the
;; way the 0427 deepdive ate the 0419 run's data. Unset → legacy shared dir.
(def output-dir (or (System/getenv "ERIDU_BENCH_OUTPUT_DIR") "output/bench"))
(def population-path (str output-dir "/evolved-population.edn"))

(defn ensure-output-dir! []
  (.mkdirs (io/file output-dir)))

;; =============================================================================
;; Configuration profiles
;; =============================================================================

(def base-config
  {:pop-size            20
   :mutation-rate       0.12
   :elite-count         4
   :games-per-matchup   2
   ;; Inter-run shifts — keep most of the population, inject few fresh
   :inter-mutation-rate 0.2
   :inter-fresh-fraction 0.15
   ;; Weight snapshot interval
   :weight-snapshot-every 50})

(def config-5p
  "1000 generations, 5-player only."
  (merge base-config
         {:gens-per-run        100
          :total-runs          10
          :player-counts       [5]}))

(def config-4p
  "1000 generations, 4-player (the standard game)."
  (merge base-config
         {:gens-per-run        100
          :total-runs          10
          :player-counts       [4]}))

(def config-all
  "1000 generations across real player counts (2, 3, 4) plus solo."
  (merge base-config
         {:gens-per-run        100
          :total-runs          10
          :player-counts       [1 2 3 4]}))

(def config-default config-all)

(def config-smoke
  "Tiny end-to-end run for verifying wiring (esp. the live auditor). Seconds, not hours."
  (merge base-config
         {:pop-size            6
          :gens-per-run        3
          :total-runs          1
          :player-counts       [2 3]}))

;; =============================================================================
;; Population persistence
;; =============================================================================

(defn save-population!
  "Save evolved population to EDN for future runs."
  [organisms]
  (ensure-output-dir!)
  (let [serializable
        (mapv (fn [o]
                {:name (:name o)
                 :elo (:elo o evolve/initial-elo)
                 :avg-reputation (:avg-reputation o 0.0)
                 :games (:games o 0)
                 :age (:age o 0)
                 :region (:region o)
                 :delta (:delta o 0.0)
                 :best-delta (:best-delta o 0.0)
                 :parent-name (:parent-name o)
                 :parent-richness (:parent-richness o 0.0)
                 :personality (:personality o)})
              organisms)]
    (spit population-path (pr-str {:saved-at (java.util.Date.)
                                    :count (count serializable)
                                    :organisms serializable}))
    (println (format "  >> Population saved: %d organisms to %s"
                     (count serializable) population-path))))

(defn load-population
  "Load saved population from EDN. Returns organisms or nil."
  []
  (when (.exists (io/file population-path))
    (try
      (let [data (edn/read-string (slurp population-path))
            organisms (:organisms data)]
        (when (seq organisms)
          (println (format "  >> Loaded saved population: %d organisms (saved %s)"
                           (count organisms) (:saved-at data)))
          (mapv (fn [o]
                  (evolve/make-organism
                   (:personality o)
                   :parent-richness (:parent-richness o 0.0)
                   :parent-name (:parent-name o)))
                organisms)))
      (catch Exception e
        (println (str "  >> Warning: failed to load population: " (.getMessage e)))
        nil))))

;; =============================================================================
;; Incremental CSV writers
;; =============================================================================

(def csv-sep ",")

(defn csv-header [columns]
  (str (str/join csv-sep (map name columns)) "\n"))

(defn csv-row [columns row]
  (str (str/join csv-sep
                 (for [col columns]
                   (let [v (get row col "")]
                     (cond
                       (keyword? v) (name v)
                       (double? v) (format "%.6f" v)
                       (float? v) (format "%.6f" (double v))
                       :else (str v)))))
       "\n"))

(defn open-csv-writer!
  "Open a writer and write the header. Returns the writer."
  [path columns]
  (let [w (io/writer (io/file path))]
    (.write w (csv-header columns))
    w))

(defn append-rows!
  "Append rows to an open CSV writer."
  [^java.io.Writer writer columns rows]
  (doseq [row rows]
    (.write writer (csv-row columns row)))
  (.flush writer))

;; Column definitions
(def generation-columns
  [:run :generation :best-name :best-elo :best-avg-rep :best-region
   :pop-avg-elo :pop-min-elo :pop-max-elo :elo-spread
   :total-games :cumulative-games :unique-regions])

(def game-summary-columns
  (into [:run :generation] sim/summary-columns))

(def snapshot-columns
  (into [:run :generation] sim/snapshot-columns))

(def weight-columns
  [:run :generation :organism-name :elo :region :age
   :sell-weight :temple-weight :deploy-weight :influence-weight
   :travel-weight :take-weight :track-balance :early-role-bias
   :chain-weight :contest-focus :resource-hoard :excess-penalty
   :temple-in-demand-city :deploy-near-opponents
   :travel-for-temple :travel-for-sell :influence-flip-raider
   :role-action-coupling :resource-planning :score-balance-target
   :feat-awareness :tempo :endgame-role-push
   :feat-sequence :feat-closure-urgency
   :role-priority-str])

(def run-summary-columns
  [:run :best-name :best-elo :best-avg-rep :best-region
   :pop-avg-elo :elo-spread :unique-regions :total-games-in-run
   :elapsed-seconds])

;; =============================================================================
;; Live board-effect auditor
;; =============================================================================
;; Runs in parallel with the GA bench: every game played by the evolving bots
;; is traced (eridu.simulate/*audit?*), and each bonus board-effect application
;; is folded — per [board-id slot-idx] — into a running attempts/failures tally.
;; The play-by-play is discarded after each game; only the tally rolls up.
;;
;; "Fails to launch" uses Muhammad's stated default: a triggered effect that
;; produces no board change at all (every delta zero/empty). Attempts are
;; counted alongside failures so a position with few attempts reads as UNTESTED
;; (low coverage), not as a healthy "0% failure" — that's the discrepancy fix.

(def ^:private untested-attempt-threshold
  "Below this many attempts a position is flagged UNTESTED rather than trusted."
  5)

(defn- trace-failed?
  "Default audit definition: the triggered board effect produced no board
   change (all deltas zero/empty)."
  [t]
  (and (zero? (:delta-amity t 0))
       (zero? (:delta-glory t 0))
       (zero? (:delta-temples t 0))
       (zero? (:delta-raiders t 0))
       (every? zero? (vals (:delta-roles t {})))
       (every? zero? (vals (:delta-resources t {})))))

(defn- fold-traces
  "Fold one game's coverage traces into the per-[board-id slot-idx] tally atom."
  [tally traces]
  (doseq [t traces]
    (let [k [(:board-id t) (:slot-idx t)]
          failed? (trace-failed? t)]
      (swap! tally update k
             (fn [m]
               {:attempts    (inc (:attempts m 0))
                :failures    (+ (:failures m 0) (if failed? 1 0))
                :impl-status (or (:impl-status m) (:impl-status t))})))))

(def coverage-columns
  [:board-id :slot-idx :attempts :failures :failure-rate :coverage-flag :impl-status])

(defn- coverage-rows
  "Materialize the tally into sorted CSV rows."
  [tally]
  (for [[[board-id slot-idx] m] (sort-by key @tally)
        :let [att (:attempts m 0)
              fail (:failures m 0)]]
    {:board-id      board-id
     :slot-idx      slot-idx
     :attempts      att
     :failures      fail
     :failure-rate  (if (pos? att) (double (/ fail att)) 0.0)
     :coverage-flag (cond
                      (< att untested-attempt-threshold) "UNTESTED"
                      (pos? fail)                         "FAILING"
                      :else                               "ok")
     :impl-status   (:impl-status m :unknown)}))

;; =============================================================================
;; Tournament with full data capture
;; =============================================================================

(defn run-tournament-with-capture
  "Like evolve/run-tournament but captures game summaries + snapshots.
   Calls `on-game` callback for each completed game with full data."
  [organisms {:keys [games-per-matchup player-counts on-game]
              :or {games-per-matchup 1 player-counts [2 3 4]}}]
  (let [elo-map (atom (into {} (map (juxt :name :elo) organisms)))
        rep-totals (atom (into {} (map (juxt :name (constantly {:sum 0.0 :n 0 :feats 0}))
                                       organisms)))
        game-count (atom 0)
        pop-size (count organisms)]

    (doseq [pc player-counts]
      (let [n-games (if (= pc 1)
                       ;; Solo: each organism plays a solo game
                       (* games-per-matchup pop-size)
                       (* games-per-matchup (max 1 (int (/ (* pop-size 3) pc)))))]
        (dotimes [_ n-games]
          (let [selected (take pc (shuffle organisms))
                configs (mapv (fn [o] {:key (:name o) :personality (:personality o)}) selected)
                result (sim/run-game configs)
                summary (sim/game-result-summary result configs)
                gid (str "g" @game-count "-" pc "p")]

            ;; Elo update
            (swap! elo-map evolve/update-elo summary)
            (doseq [{:keys [personality reputation feats-claimed]} summary]
              (swap! rep-totals update personality
                     (fn [m] {:sum (+ (:sum m 0.0) reputation)
                              :n (inc (:n m 0))
                              :feats (+ (:feats m 0) (or feats-claimed 0))})))

            ;; Callback with full data
            (when on-game
              (on-game {:game-id gid
                        :player-count pc
                        :configs configs
                        :summary summary
                        :snapshots (:snapshots result)
                        :coverage-traces (:coverage-traces result)}))

            (swap! game-count inc)))))

    ;; Update organisms with tournament results
    (let [final-elo @elo-map
          final-rep @rep-totals]
      {:organisms
       (mapv (fn [o]
               (let [rep-data (get final-rep (:name o) {:sum 0 :n 0 :feats 0})
                     new-elo (get final-elo (:name o) evolve/initial-elo)
                     avg-rep (if (pos? (:n rep-data))
                               (/ (:sum rep-data) (:n rep-data))
                               0.0)
                     avg-feats (if (pos? (:n rep-data))
                                 (/ (double (:feats rep-data)) (:n rep-data))
                                 0.0)
                     delta (- new-elo (:parent-richness o 0))]
                 (assoc o
                        :elo new-elo
                        :avg-reputation avg-rep
                        :avg-feats avg-feats
                        :games (+ (:games o 0) (:n rep-data 0))
                        :evaluated? true
                        :delta delta
                        :best-delta (max (:best-delta o 0) delta))))
             organisms)
       :total-games @game-count})))

;; =============================================================================
;; Inter-run population shifts
;; =============================================================================

(defn inject-fresh-blood
  "Between runs: keep top performers, replace bottom fraction with
   heavily-mutated variants + fresh randoms."
  [organisms config]
  (let [fresh-frac (:inter-fresh-fraction config)
        mut-rate (:inter-mutation-rate config)
        sorted (sort-by #(- (:avg-reputation % 0)) organisms)
        n (count sorted)
        n-keep (int (* n (- 1.0 fresh-frac)))
        n-fresh (- n n-keep)
        survivors (take n-keep sorted)
        n-random (int (/ n-fresh 2))
        n-mutated (- n-fresh n-random)
        top-parents (take (max 1 (int (/ n-keep 2))) survivors)
        mutated-children
        (for [i (range n-mutated)
              :let [parent (nth top-parents (mod i (count top-parents)))]]
          (evolve/make-organism
           (pers/mutate-personality (:personality parent) mut-rate)
           :parent-richness (:elo parent evolve/initial-elo)
           :parent-name (:name parent)))
        fresh (repeatedly n-random #(evolve/make-organism (pers/random-personality)))]
    (vec (concat survivors mutated-children fresh))))

;; =============================================================================
;; Weight snapshot helper
;; =============================================================================

(defn organism->weight-row [run-idx gen-idx organism]
  (let [p (:personality organism)]
    {:run run-idx
     :generation gen-idx
     :organism-name (:name organism)
     :elo (:elo organism evolve/initial-elo)
     :region (:region organism)
     :age (:age organism 0)
     :sell-weight (:sell-weight p 1.0)
     :temple-weight (:temple-weight p 1.0)
     :deploy-weight (:deploy-weight p 1.0)
     :influence-weight (:influence-weight p 1.0)
     :travel-weight (:travel-weight p 0.5)
     :take-weight (:take-weight p 1.0)
     :track-balance (:track-balance p 0.5)
     :early-role-bias (:early-role-bias p 0.7)
     :chain-weight (:chain-weight p 0.5)
     :contest-focus (:contest-focus p 0.3)
     :resource-hoard (:resource-hoard p 0.3)
     :excess-penalty (:excess-penalty p 3.0)
     :temple-in-demand-city (:temple-in-demand-city p 1.5)
     :deploy-near-opponents (:deploy-near-opponents p 1.5)
     :travel-for-temple (:travel-for-temple p 2.0)
     :travel-for-sell (:travel-for-sell p 1.8)
     :influence-flip-raider (:influence-flip-raider p 2.5)
     :role-action-coupling (:role-action-coupling p 0.5)
     :resource-planning (:resource-planning p 0.5)
     :score-balance-target (:score-balance-target p 0.5)
     :feat-awareness (:feat-awareness p 0.3)
     :tempo (:tempo p 0.3)
     :endgame-role-push (:endgame-role-push p 0.5)
     :feat-sequence (:feat-sequence p 0.4)
     :feat-closure-urgency (:feat-closure-urgency p 0.5)
     :role-priority-str (str/join ">" (map name (:role-priority p)))}))

;; =============================================================================
;; Main bench loop
;; =============================================================================

(defn run-bench! [config & {:keys [fresh?]}]
  (ensure-output-dir!)
  ;; Merge over base-config so every field consumed by run-bench!/the banner has a
  ;; default — a hand-built or REPL config that omits e.g. :inter-fresh-fraction can
  ;; never produce a nil that crashes arithmetic/printing downstream.
  (let [config (merge base-config config)
        {:keys [pop-size gens-per-run total-runs mutation-rate elite-count
                player-counts games-per-matchup weight-snapshot-every]} config
        start-time (System/currentTimeMillis)
        ;; Load or create initial population
        saved-pop (when-not fresh? (load-population))
        initial-pop (if saved-pop
                      ;; Resize to match config if needed
                      (let [n (count saved-pop)]
                        (cond
                          (= n pop-size) saved-pop
                          (> n pop-size)
                          (do (println (format "  !! Saved population (%d) > pop-size (%d); keeping top %d by Elo."
                                               n pop-size pop-size))
                              (vec (take pop-size (sort-by #(- (:elo %)) saved-pop))))
                          :else
                          (do (println (format "  !! Saved population (%d) < pop-size (%d); injecting %d FRESH random organisms."
                                               n pop-size (- pop-size n)))
                              (println "     A large top-up dilutes the evolved basin with random weights — prefer a fresh run if this gap is big.")
                              (into (vec saved-pop)
                                    (repeatedly (- pop-size n)
                                                #(evolve/make-organism
                                                  (pers/random-personality)))))))
                      (evolve/initial-population pop-size))
        pc-label (str/join "+" (map str player-counts))]

    (println "Eridu Evolution Bench Runner")
    (println "============================")
    (println (format "  %d runs x %d gens, pop=%d, player counts=%s"
                     total-runs gens-per-run pop-size pc-label))
    (println (format "  ~%d games/gen, ~%d total games"
                     (* games-per-matchup
                        (reduce + (map #(int (/ (* pop-size 3) %)) player-counts)))
                     (* total-runs gens-per-run games-per-matchup
                        (reduce + (map #(int (/ (* pop-size 3) %)) player-counts)))))
    (println (format "  Intra-run mutation=%.2f, inter-run mutation=%.2f, fresh=%.0f%%"
                     mutation-rate (:inter-mutation-rate config)
                     (* 100.0 (:inter-fresh-fraction config))))
    (println (format "  Population source: %s"
                     (if saved-pop "LOADED from evolved-population.edn" "FRESH")))
    (println "")

    ;; Open incremental CSV writers with player-count suffix
    (let [suffix (str "-" pc-label "p")
          gen-w (open-csv-writer! (str output-dir "/generation-stats" suffix ".csv") generation-columns)
          game-w (open-csv-writer! (str output-dir "/game-summaries" suffix ".csv") game-summary-columns)
          snap-w (open-csv-writer! (str output-dir "/snapshots" suffix ".csv") snapshot-columns)
          wgt-w (open-csv-writer! (str output-dir "/population-weights" suffix ".csv") weight-columns)
          run-w (open-csv-writer! (str output-dir "/run-summaries" suffix ".csv") run-summary-columns)
          ;; Live board auditor (only when ERIDU_AUDIT enabled it via -main)
          audit? sim/*audit?*
          cov-tally (atom {})
          cumulative-games (atom 0)
          final-population (atom nil)]
      (when audit?
        (println "  Live board auditor: ON (per-position attempts/failures tally)"))

      (try
        (loop [run-idx 1
               population initial-pop]
          (if (> run-idx total-runs)
            ;; Done — save evolved population
            (let [elapsed (/ (- (System/currentTimeMillis) start-time) 1000.0)]
              (println (format "\n=== Complete: %d runs, %d total games in %.0f seconds ==="
                               total-runs @cumulative-games elapsed))
              (save-population! @final-population))

            ;; Run
            (let [run-start (System/currentTimeMillis)
                  _ (println (format "=== Run %d/%d (%s) ===" run-idx total-runs pc-label))
                  run-games (atom 0)]

              (let [final-pop
                    (loop [pop population
                           gen 0]
                      (if (>= gen gens-per-run)
                        pop

                        ;; One generation
                        (let [on-game
                              (fn [{:keys [game-id player-count summary snapshots coverage-traces]}]
                                ;; Write game summaries
                                (append-rows! game-w game-summary-columns
                                              (map #(assoc %
                                                           :run run-idx
                                                           :generation gen
                                                           :game-id game-id
                                                           :player-count player-count)
                                                   summary))
                                ;; Write snapshots
                                (when (seq snapshots)
                                  (append-rows! snap-w snapshot-columns
                                                (map #(assoc %
                                                             :run run-idx
                                                             :generation gen
                                                             :game-id game-id
                                                             :player-count player-count)
                                                     snapshots)))
                                ;; Live audit: fold this game's traces into the
                                ;; per-position tally, then drop the play-by-play.
                                (when (and audit? (seq coverage-traces))
                                  (fold-traces cov-tally coverage-traces))
                                (swap! run-games inc)
                                (swap! cumulative-games inc))

                              {:keys [organisms total-games]}
                              (run-tournament-with-capture
                               pop
                               {:games-per-matchup games-per-matchup
                                :player-counts player-counts
                                :on-game on-game})

                              best (first (sort-by #(- (:elo %)) organisms))
                              avg-elo (/ (reduce + (map :elo organisms))
                                         (max 1 (count organisms)))
                              gen-row {:run run-idx
                                       :generation gen
                                       :best-name (:name best)
                                       :best-elo (:elo best)
                                       :best-avg-rep (:avg-reputation best)
                                       :best-region (:region best)
                                       :pop-avg-elo (double avg-elo)
                                       :pop-min-elo (double (apply min (map :elo organisms)))
                                       :pop-max-elo (double (apply max (map :elo organisms)))
                                       :elo-spread (double (- (apply max (map :elo organisms))
                                                              (apply min (map :elo organisms))))
                                       :total-games total-games
                                       :cumulative-games @cumulative-games
                                       :unique-regions (count (distinct (map :region organisms)))}]

                          ;; Write generation stats
                          (append-rows! gen-w generation-columns [gen-row])

                          ;; Write weight snapshots at intervals
                          (when (zero? (mod gen weight-snapshot-every))
                            (append-rows! wgt-w weight-columns
                                          (map #(organism->weight-row run-idx gen %)
                                               organisms)))

                          ;; Progress
                          (when (zero? (mod gen 20))
                            (println (format "    Gen %d/%d: best=%s elo=%.0f avg=%.0f spread=%.0f regions=%d games=%d"
                                             gen gens-per-run (:name best) (:elo best)
                                             avg-elo (:elo-spread gen-row)
                                             (:unique-regions gen-row) @cumulative-games)))

                          ;; Evolve
                          (let [next-pop (evolve/evolve-generation
                                          organisms
                                          {:pop-size pop-size
                                           :mutation-rate mutation-rate
                                           :elite-count elite-count})]
                            (recur next-pop (inc gen))))))]

                ;; Save reference to final pop for persistence
                (reset! final-population final-pop)

                ;; Run summary
                (let [run-elapsed (/ (- (System/currentTimeMillis) run-start) 1000.0)]
                  (println (format "  Run %d done: %d games in %.0fs, total=%d"
                                   run-idx @run-games run-elapsed @cumulative-games))

                  ;; Find best from final pop
                  (let [best (first (sort-by #(- (:elo %)) final-pop))
                        avg-elo (double (/ (reduce + (map :elo final-pop))
                                           (max 1 (count final-pop))))]
                    (append-rows! run-w run-summary-columns
                                  [{:run run-idx
                                    :best-name (:name best)
                                    :best-elo (:elo best)
                                    :best-avg-rep (:avg-reputation best)
                                    :best-region (:region best)
                                    :pop-avg-elo avg-elo
                                    :elo-spread (double (- (apply max (map :elo final-pop))
                                                           (apply min (map :elo final-pop))))
                                    :unique-regions (count (distinct (map :region final-pop)))
                                    :total-games-in-run @run-games
                                    :elapsed-seconds run-elapsed}]))

                  ;; Inter-run shift
                  (let [next-pop (if (< run-idx total-runs)
                                   (let [shifted (inject-fresh-blood final-pop config)]
                                     (println (format "  >> Inter-run shift: %d fresh organisms injected"
                                                      (int (* (count final-pop)
                                                              (:inter-fresh-fraction config)))))
                                     ;; Save after each run for crash recovery
                                     (save-population! final-pop)
                                     shifted)
                                   final-pop)]
                    (println "")
                    (recur (inc run-idx) next-pop)))))))

        (finally
          ;; Flush + close the live audit summary (always, even on early exit so
          ;; a partial run still leaves an honest per-position table).
          (when audit?
            (let [rows (coverage-rows cov-tally)
                  cov-w (open-csv-writer! (str output-dir "/board-coverage-summary.csv")
                                          coverage-columns)]
              (append-rows! cov-w coverage-columns rows)
              (.close cov-w)
              (let [positions (count rows)
                    attempts (reduce + (map :attempts rows))
                    failures (reduce + (map :failures rows))
                    untested (count (filter #(= "UNTESTED" (:coverage-flag %)) rows))]
                (println (format "  >> Board audit: %d positions seen, %d attempts, %d failures, %d UNTESTED"
                                 positions attempts failures untested)))))
          ;; Close all writers
          (.close gen-w)
          (.close game-w)
          (.close snap-w)
          (.close wgt-w)
          (.close run-w))))

    (println (str "\nOutput files in: " output-dir "/"))
    (println (str "  generation-stats-" pc-label "p.csv"))
    (println (str "  game-summaries-" pc-label "p.csv"))
    (println (str "  snapshots-" pc-label "p.csv"))
    (println (str "  population-weights-" pc-label "p.csv"))
    (println (str "  run-summaries-" pc-label "p.csv"))
    (when sim/*audit?*
      (println "  board-coverage-summary.csv  — live per-position attempts/failures"))
    (println "  evolved-population.edn  — carry forward to next run")))

;; =============================================================================
;; Entry point
;; =============================================================================

(defn -main [& args]
  (let [mode (first args)
        fresh? (some #{"fresh"} args)
        ;; Live board auditor on when ERIDU_AUDIT is set or "audit" passed.
        audit? (or (some #{"audit"} args)
                   (#{"1" "true" "yes" "on"} (str (System/getenv "ERIDU_AUDIT"))))
        config (case mode
                 "5p"    config-5p
                 "4p"    config-4p
                 "all"   config-all
                 "smoke" config-smoke
                 config-default)]
    (binding [sim/*audit?* (boolean audit?)]
      (run-bench! config :fresh? fresh?))
    (shutdown-agents)))
