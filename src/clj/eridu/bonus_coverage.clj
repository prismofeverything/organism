(ns eridu.bonus-coverage
  "Bonus-board coverage bench mode. Drives play that deterministically
   rotates bonus-board assignments to cover every [board-id slot-idx]
   combination, instruments slot triggers so each fires a trace record,
   and diffs actual-vs-expected against eridu.bonus-oracle/expectations.

   Entry: (run-coverage-bench! {:games-per-slot 30 :output-dir ...})

   Output:
   - <output-dir>/coverage-traces.edn   — per-trigger {pre, post, choice, deltas}
   - <output-dir>/coverage-summary.csv  — per-(board, slot) trigger + class counts
   - <output-dir>/classified-errors.edn — error records grouped by error-class

   Choice-needed slots are routed through apply-bonus-with-choice with a
   deterministic resolver (:tools / first magistrate city / lowest role)
   via with-redefs around the natural game loop."
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [eridu.game :as game]
   [eridu.simulate :as sim]
   [eridu.personality :as pers]
   [eridu.bonus-oracle :as oracle]))

(def error-classes
  "Taxonomy from the approved proposal."
  #{:missing-effect     ;; nothing happened
    :partial-effect     ;; subset of expected delta applied
    :wrong-target       ;; effect hit wrong city/role/player
    :sequencing         ;; intervening effect clobbered the result
    :choice-missing     ;; bonus-needs-choice? should have surfaced and didn't
    :persistent-misfire ;; slot-0 passive didn't trigger on its event
    :oracle-unpopulated ;; oracle has no entry yet (not a real bug, just coverage gap)
    })

(defn classify-trace
  "Given a trace record and the oracle's expected delta, return an error
   class keyword or :ok."
  [trace expected]
  (cond
    (nil? expected) :oracle-unpopulated
    (and (zero? (:delta-amity trace 0))
         (zero? (:delta-glory trace 0))
         (empty? (remove zero? (vals (:delta-roles trace {}))))
         (empty? (remove zero? (vals (:delta-resources trace {}))))
         (zero? (:delta-temples trace 0))
         (zero? (:delta-raiders trace 0))
         (or (pos? (:delta-amity expected 0))
             (pos? (:delta-glory expected 0))
             (seq (:delta-roles expected))
             (seq (:delta-resources expected))
             (pos? (:delta-temples expected 0))
             (pos? (:delta-raiders expected 0))))
    :missing-effect

    (and (:requires-choice? expected) (nil? (:choice-value trace)))
    :choice-missing

    ;; Totals match but per-key don't: wrong-target
    (and (= (:delta-amity trace 0) (:delta-amity expected 0))
         (= (:delta-glory trace 0) (:delta-glory expected 0))
         (not= (:delta-resources trace {}) (:delta-resources expected {})))
    :wrong-target

    ;; Some but not all axes match: partial-effect
    (or (not= (:delta-amity trace 0) (:delta-amity expected 0))
        (not= (:delta-glory trace 0) (:delta-glory expected 0))
        (not= (:delta-resources trace {}) (:delta-resources expected {}))
        (not= (:delta-roles trace {}) (:delta-roles expected {}))
        (not= (:delta-temples trace 0) (:delta-temples expected 0))
        (not= (:delta-raiders trace 0) (:delta-raiders expected 0)))
    :partial-effect

    :else :ok))

;; ─── Deterministic choice resolver ──────────────────────────────────────────

(defn deterministic-choice
  "Resolve a :pick-resource | :pick-city | :pick-role choice to a fixed value
   so trace generation is reproducible.

   :pick-resource → :tools (or [:tools :tools :tools] for count=3)
   :pick-city     → first magistrate city (sorted by name) or :uruk
   :pick-role     → lowest-leveled role for the player"
  [state player-key choice-desc]
  (case (:type choice-desc)
    :pick-resource (if (and (:count choice-desc)
                            (> (:count choice-desc) 1))
                     (vec (repeat (:count choice-desc) :tools))
                     :tools)
    :pick-city     (or (first (sort (keys (:magistrates state {}))))
                       :uruk)
    :pick-role     (let [pdata (get-in state [:players player-key])]
                     (first (sort-by #(get-in pdata [:roles %] 1) game/roles)))))

;; ─── Deterministic board rotation ───────────────────────────────────────────

(defn rotate-board-id
  "Deterministic board assignment: for game-idx g, player slot p, returns
   board-id in 1..35. Stripes assignments so consecutive games shift the
   board mapping by one across all 35 boards."
  [game-idx player-slot]
  (inc (mod (+ game-idx player-slot) 35)))

(defn- assign-bonus-boards
  "Override [:bonus-boards player-key] for each player and reset their
   bonus-board grid to all-covered."
  [state board-by-player]
  (reduce-kv (fn [s pk bid]
               (-> s
                   (assoc-in [:bonus-boards pk] bid)
                   (assoc-in [:players pk :bonus-board] (vec (repeat 5 :covered)))))
             state board-by-player))

(defn- enable-coverage-trace
  "Turn on the :coverage-trace? flag and seed an empty :coverage-traces."
  [state]
  (assoc state :coverage-trace? true :coverage-traces []))

;; ─── Player config builder ──────────────────────────────────────────────────

(defn- make-configs
  "Build {:key :personality} configs for `player-count` players, drawing
   deterministically from the personality pool."
  [player-count personalities game-idx]
  (let [n (count personalities)]
    (vec
     (for [i (range player-count)]
       {:key (keyword (str "p" (inc i)))
        :personality (nth personalities (mod (+ game-idx i) n))}))))

;; ─── One-game runner ────────────────────────────────────────────────────────

(defn run-one-coverage-game
  "Run a single coverage-bench game. Returns the seq of :coverage-traces
   appended to the game's final state."
  [{:keys [player-count game-idx personalities seed]}]
  (let [pers-pool (or personalities pers/archetypes)
        configs (make-configs player-count pers-pool game-idx)
        player-keys (mapv :key configs)
        board-assignments (zipmap player-keys
                                  (map #(rotate-board-id game-idx %)
                                       (range player-count)))
        orig-initial-state game/initial-state
        orig-initial-solo-state game/initial-solo-state
        orig-apply-bonus-effect game/apply-bonus-effect
        coverage-apply-bonus-effect
        (fn [state pk bid sid]
          (if-let [need (game/bonus-needs-choice? bid sid)]
            (game/apply-bonus-with-choice
             state pk bid sid
             (deterministic-choice state pk need))
            (orig-apply-bonus-effect state pk bid sid)))
        coverage-initial-state
        (fn [pks]
          (-> (orig-initial-state pks)
              (assign-bonus-boards board-assignments)
              enable-coverage-trace))
        coverage-initial-solo-state
        (fn [pk]
          (-> (orig-initial-solo-state pk)
              (assign-bonus-boards {pk (rotate-board-id game-idx 0)})
              enable-coverage-trace))]
    (with-redefs [game/initial-state coverage-initial-state
                  game/initial-solo-state coverage-initial-solo-state
                  game/apply-bonus-effect coverage-apply-bonus-effect]
      (let [result (sim/run-game configs :seed seed)]
        (-> result :final-state :coverage-traces (or []))))))

;; ─── Trace classification + writing ─────────────────────────────────────────

(defn- classify-traces
  "Annotate each trace with :expected-delta and :error-class."
  [traces]
  (mapv (fn [t]
          (let [expected (oracle/expected-delta
                          (:board-id t) (:slot-idx t)
                          (:pre-snapshot t) (:player t))
                klass (classify-trace t expected)]
            (assoc t :expected-delta expected :error-class klass)))
        traces))

(defn- write-traces-edn!
  [classified output-dir]
  (let [f (io/file output-dir "coverage-traces.edn")]
    (with-open [w (io/writer f)]
      (binding [*out* w *print-length* nil *print-level* nil]
        (doseq [t classified]
          (pp/pprint t))))))

(defn- summary-csv-rows
  [classified]
  (let [by-slot (group-by (juxt :board-id :slot-idx) classified)]
    (for [bid (range 1 36)
          sid (range 5)
          :let [k [bid sid]
                ts (get by-slot k [])
                by-class (frequencies (map :error-class ts))]]
      [bid sid (count ts)
       (get by-class :ok 0)
       (get by-class :missing-effect 0)
       (get by-class :partial-effect 0)
       (get by-class :wrong-target 0)
       (get by-class :sequencing 0)
       (get by-class :choice-missing 0)
       (get by-class :persistent-misfire 0)
       (get by-class :oracle-unpopulated 0)])))

(defn- write-summary-csv!
  [classified output-dir]
  (let [f (io/file output-dir "coverage-summary.csv")
        header "board-id,slot-idx,triggers,ok,missing-effect,partial-effect,wrong-target,sequencing,choice-missing,persistent-misfire,oracle-unpopulated"
        rows (summary-csv-rows classified)]
    (spit f (str header "\n"
                 (str/join "\n"
                           (for [r rows]
                             (str/join "," r)))
                 "\n"))))

(defn- write-classified-errors-edn!
  [classified output-dir]
  (let [f (io/file output-dir "classified-errors.edn")
        errors (remove #(= :ok (:error-class %)) classified)
        grouped (group-by :error-class errors)]
    (with-open [w (io/writer f)]
      (binding [*out* w *print-length* nil *print-level* nil]
        (pp/pprint grouped)))))

;; ─── Main bench driver ──────────────────────────────────────────────────────

(defn- games-needed-for-coverage
  "Estimate games-per-player-count to hit games-per-slot triggers per
   [board-id slot-idx]. Each game uncovers ~4 one-time slots + 1 passive,
   spread across 35 boards; we want each combination ≥ games-per-slot.

   Rule of thumb: 400 games/player-count for games-per-slot=30. Scale linearly."
  [games-per-slot]
  (max 50
       (long (Math/ceil (* 400.0 (/ (double games-per-slot) 30.0))))))

(defn run-coverage-bench!
  "Run a bonus-coverage bench: for each player-count in `player-counts`,
   simulate `games-per-pc` games with deterministic board rotation, capture
   per-trigger traces from instrumented apply-bonus-effect/-with-choice,
   classify against eridu.bonus-oracle, and write three artifacts under
   `output-dir`.

   Config keys:
   - :output-dir       (required) — where artifacts go
   - :games-per-slot   N (default 30) — triggers wanted per [board, slot]
   - :games-per-pc     N (override) — games per player-count
   - :player-counts    [2 3 4] (default)
   - :seed             nano-time by default; set for reproducibility
   - :personalities    pool override (default eridu.personality/archetypes)
   - :on-progress      (fn [pc game-idx total-pc-games trace-count]) optional"
  [{:keys [output-dir games-per-slot games-per-pc player-counts
           seed personalities on-progress]
    :or   {games-per-slot 30
           player-counts  [2 3 4]}}]
  (when-not output-dir
    (throw (ex-info "run-coverage-bench! requires :output-dir" {})))
  (.mkdirs (io/file output-dir))
  (let [seed (or seed (System/nanoTime))
        games-per-pc (or games-per-pc (games-needed-for-coverage games-per-slot))
        all-traces (volatile! (transient []))]
    (doseq [pc player-counts
            g  (range games-per-pc)]
      (let [game-seed (+ seed (* 1000000 pc) g)
            traces (run-one-coverage-game
                    {:player-count pc
                     :game-idx g
                     :seed game-seed
                     :personalities personalities})]
        (doseq [t traces]
          (vswap! all-traces conj! t))
        (when on-progress
          (on-progress pc g games-per-pc (count traces)))))
    (let [traces (persistent! @all-traces)
          classified (classify-traces traces)]
      (write-traces-edn! classified output-dir)
      (write-summary-csv! classified output-dir)
      (write-classified-errors-edn! classified output-dir)
      {:output-dir   output-dir
       :total-traces (count traces)
       :games-per-pc games-per-pc
       :player-counts player-counts
       :seed         seed})))

(defn -main
  "Entry point for `lein run -m eridu.bonus-coverage <output-dir> [games-per-slot]`."
  [& args]
  (let [output-dir (or (first args)
                       (str "/home/m/organism/output/bonus-coverage/run-"
                            (.format (java.text.SimpleDateFormat. "yyyyMMdd-HHmmss")
                                     (java.util.Date.))))
        games-per-slot (Integer/parseInt (or (second args) "30"))
        progress (fn [pc g total cnt]
                   (when (zero? (mod g 25))
                     (println (format "[bonus-coverage] pc=%d game %d/%d (+%d traces)"
                                      pc g total cnt))))
        result (run-coverage-bench!
                {:output-dir     output-dir
                 :games-per-slot games-per-slot
                 :on-progress    progress})]
    (println "[bonus-coverage] done →" (:output-dir result))
    (println "  total-traces:" (:total-traces result))
    (println "  games-per-pc:" (:games-per-pc result))
    (println "  seed:        " (:seed result))
    (shutdown-agents)))
