(ns eridu.diagnose
  "Diagnostic tool: cycle through board/feat combos, identify broken bonuses,
   track seed performance, and iterate toward minimum 2-rep for all players."
  (:require [eridu.game :as game]
            [eridu.simulate :as sim]
            [eridu.personality :as pers]
            [eridu.choice :as choice]
            [clojure.string :as str]))

;; =============================================================================
;; Board/feat affinity analysis
;; =============================================================================

(defn run-diagnostic-game
  "Run a single game and return detailed results."
  [player-configs]
  (let [result (sim/run-game player-configs)
        state (:final-state result)]
    {:steps (:steps result)
     :players
     (into {}
           (for [[pk pd] (:players state)]
             [pk {:amity (:amity pd 0)
                  :glory (:glory pd 0)
                  :rep (min (:amity pd 0) (:glory pd 0))
                  :roles (:roles pd)
                  :temples-placed (game/count-temples-placed pd)
                  :temples-flipped (game/count-face-down-temples pd)
                  :raiders-deployed (game/count-raiders-deployed pd)
                  :demands (count (:demand-tokens pd []))
                  :wild-points (:wild-points pd 0)
                  :feats-claimed (count (filter #(some #{pk} (val %))
                                                (:contest-claims state {})))
                  :target-feats (mapv :id (:target-feats pd []))
                  :board-id (get-in state [:bonus-boards pk])
                  :board-effects-fired (count (filter :changed
                                                      (:board-effects-log pd [])))
                  :board-effects-noop (count (remove :changed
                                                     (:board-effects-log pd [])))
                  :total-travels (:total-travels pd 0)
                  :dice-stats (:dice-stats pd)}]))
     :contests (mapv :id (:contests state []))
     :contest-claims (:contest-claims state {})}))

(defn board-effect-report
  "Analyze which board effects actually fire vs no-op across many games."
  [n-games]
  (let [effect-counts (atom {})
        effect-fires (atom {})]
    (dotimes [_ n-games]
      (let [cfgs (mapv (fn [j] {:key (keyword (str "p" j))
                                :personality (pers/random-personality)})
                       (range 5))
            result (sim/run-game cfgs)
            state (:final-state result)]
        (doseq [[_pk pd] (:players state)
                entry (:board-effects-log pd [])]
          (let [k [(:board-id entry) (:slot entry)]]
            (swap! effect-counts update k (fnil inc 0))
            (when (:changed entry)
              (swap! effect-fires update k (fnil inc 0)))))))
    ;; Build report
    (let [all-keys (sort (distinct (concat (keys @effect-counts) (keys @effect-fires))))]
      (for [k all-keys]
        (let [total (get @effect-counts k 0)
              fired (get @effect-fires k 0)
              rate (if (pos? total) (double (/ fired total)) 0.0)
              impl (get game/effect-implementation-status k :unknown)]
          {:board (first k)
           :slot (second k)
           :total total
           :fired fired
           :rate rate
           :impl impl})))))

(defn feat-board-affinity
  "Measure affinity between feat categories and board IDs by running games
   and correlating which board+feat combos lead to higher reputation."
  [n-games]
  (let [results (atom [])]
    (dotimes [_ n-games]
      (let [cfgs (mapv (fn [j] {:key (keyword (str "p" j))
                                :personality (pers/random-personality)})
                       (range 5))
            r (run-diagnostic-game cfgs)]
        (doseq [[_pk pdata] (:players r)]
          (swap! results conj pdata))))
    ;; Group by board-id and target feat, compute average rep
    (let [by-board (group-by :board-id @results)
          board-stats (into (sorted-map)
                            (for [[bid entries] by-board]
                              [bid {:avg-rep (double (/ (reduce + (map :rep entries))
                                                       (count entries)))
                                    :avg-feats (double (/ (reduce + (map :feats-claimed entries))
                                                          (count entries)))
                                    :avg-effects (double (/ (reduce + (map :board-effects-fired entries))
                                                            (count entries)))
                                    :n (count entries)}]))
          ;; Feat affinity: which target feats correlate with high rep?
          by-feat (group-by #(first (:target-feats %)) @results)
          feat-stats (into (sorted-map)
                           (for [[fid entries] by-feat
                                 :when fid]
                             [fid {:avg-rep (double (/ (reduce + (map :rep entries))
                                                      (count entries)))
                                   :claimed-rate (double (/ (count (filter #(pos? (:feats-claimed %)) entries))
                                                            (count entries)))
                                   :n (count entries)}]))]
      {:board-stats board-stats
       :feat-stats feat-stats})))

(defn run-stress-test
  "Run n-games and return stats: zero-rep count, avg rep, min rep, feat count."
  [n-games & {:keys [personality-fn] :or {personality-fn pers/random-personality}}]
  (let [all-reps (atom [])
        all-feats (atom 0)
        min-reps (atom [])]
    (dotimes [_ n-games]
      (let [cfgs (mapv (fn [j] {:key (keyword (str "p" j))
                                :personality (personality-fn)})
                       (range 5))
            r (run-diagnostic-game cfgs)
            reps (mapv :rep (vals (:players r)))
            feats (reduce + (map :feats-claimed (vals (:players r))))]
        (swap! all-reps into reps)
        (swap! all-feats + feats)
        (swap! min-reps conj (apply min reps))))
    (let [reps @all-reps
          mins @min-reps]
      {:n-players (count reps)
       :n-games n-games
       :zero-reps (count (filter zero? reps))
       :zero-pct (format "%.1f%%" (* 100.0 (/ (count (filter zero? reps)) (count reps))))
       :sub2-reps (count (filter #(< % 2) reps))
       :sub2-pct (format "%.1f%%" (* 100.0 (/ (count (filter #(< % 2) reps)) (count reps))))
       :avg-rep (format "%.1f" (double (/ (reduce + reps) (count reps))))
       :min-rep (apply min reps)
       :max-rep (apply max reps)
       :total-feats @all-feats
       :feats-per-game (format "%.1f" (double (/ @all-feats n-games)))
       ;; Games where ALL players had >= 2 rep
       :clean-games (count (filter #(>= % 2) mins))
       :clean-pct (format "%.1f%%" (* 100.0 (/ (count (filter #(>= % 2) mins)) n-games)))})))

(defn find-broken-boards
  "Identify boards with low effect-fire rates."
  [n-games]
  (let [report (board-effect-report n-games)
        broken (filter #(and (pos? (:total %))
                             (< (:rate %) 0.1)
                             (not= :persistent (:impl %)))
                       report)]
    (println "=== Broken/low-fire board effects ===")
    (doseq [b (sort-by (juxt :board :slot) broken)]
      (println (format "  Board %2d Slot %d: %d/%d fired (%.0f%%) — %s"
                       (:board b) (:slot b)
                       (:fired b) (:total b)
                       (* 100 (:rate b))
                       (name (:impl b)))))
    broken))

(defn -main [& _args]
  (println "=== Eridu Board/Feat Diagnostic ===")
  (println)

  ;; Phase 1: Stress test current state
  (println "--- Phase 1: Baseline stress test (50 games) ---")
  (let [stats (run-stress-test 50)]
    (doseq [[k v] (sort stats)]
      (println (format "  %-15s %s" (name k) v))))

  (println)

  ;; Phase 2: Board effect analysis
  (println "--- Phase 2: Board effect fire rates (30 games) ---")
  (find-broken-boards 30)

  (println)

  ;; Phase 3: Feat/board affinity
  (println "--- Phase 3: Feat/board affinity (30 games) ---")
  (let [{:keys [board-stats feat-stats]} (feat-board-affinity 30)]
    (println "  Top 5 boards by avg rep:")
    (doseq [[bid stats] (take 5 (reverse (sort-by (comp :avg-rep second) board-stats)))]
      (println (format "    Board %2d: avg-rep=%.1f avg-feats=%.1f avg-effects=%.1f (n=%d)"
                       bid (:avg-rep stats) (:avg-feats stats) (:avg-effects stats) (:n stats))))
    (println "  Bottom 5 boards by avg rep:")
    (doseq [[bid stats] (take 5 (sort-by (comp :avg-rep second) board-stats))]
      (println (format "    Board %2d: avg-rep=%.1f avg-feats=%.1f avg-effects=%.1f (n=%d)"
                       bid (:avg-rep stats) (:avg-feats stats) (:avg-effects stats) (:n stats))))
    (println)
    (println "  Top feats by avg rep:")
    (doseq [[fid stats] (take 5 (reverse (sort-by (comp :avg-rep second) feat-stats)))]
      (println (format "    %-3s: avg-rep=%.1f claim-rate=%.0f%% (n=%d)"
                       (name fid) (:avg-rep stats) (* 100 (:claimed-rate stats)) (:n stats))))
    (println "  Bottom feats by avg rep:")
    (doseq [[fid stats] (take 5 (sort-by (comp :avg-rep second) feat-stats))]
      (println (format "    %-3s: avg-rep=%.1f claim-rate=%.0f%% (n=%d)"
                       (name fid) (:avg-rep stats) (* 100 (:claimed-rate stats)) (:n stats))))))

;; =============================================================================
;; Replay: re-run a game with full action-by-action logging
;; =============================================================================

(def ^:private replay-protected-phases game/human-protected-phases)

(defn replay-game
  "Run a game with full action logging for forensic analysis.
   Pass the same player-configs used in the bench.
   Prints every choice made, every scoring event, every phase transition."
  [player-configs & {:keys [seed]}]
  (let [seed (or seed (System/nanoTime))
        player-keys (mapv :key player-configs)
        personality-map (into {} (map (juxt :key :personality) player-configs))
        initial (if (= 1 (count player-configs))
                  (game/initial-solo-state (first player-keys))
                  (game/initial-state player-keys))
        initial (reduce (fn [s [pk weights]]
                          (assoc-in s [:players pk :personality-cache]
                                    (select-keys weights [:tempo :feat-awareness
                                                          :prefer-onetime-bonus
                                                          :feat-sequence :feat-closure-urgency])))
                        initial personality-map)]
    (println "═══════════════════════════════════════════════════════════")
    (println (str "  REPLAY — seed: " seed " | players: " (count player-keys)))
    (println "═══════════════════════════════════════════════════════════")
    (println)
    ;; Print setup
    (doseq [[pk pd] (:players initial)]
      (let [card (:starting-card pd)
            bid (get-in initial [:bonus-boards pk])]
        (println (format "  %s: card#%s city=%s role=%s resource=%s board=%s dice=%s"
                         pk (:number card) (name (:city card))
                         (name (:role card)) (name (:resource card))
                         bid (:dice-available pd)))))
    (println (str "  Contests: " (str/join ", " (map #(name (:id %)) (:contests initial)))))
    (println)

    (loop [state initial
           step 0
           prev-round 0
           prev-turn 0]
      (if (or (:game-over state) (> step 5000))
        (do
          (println)
          (println "═══ GAME OVER ═══════════════════════════════════════════")
          (doseq [[pk pd] (:players state)]
            (println (format "  %s: ♥%d ⚡%d ★%d | wild=%d demands=%d"
                             pk (:amity pd 0) (:glory pd 0)
                             (min (:amity pd 0) (:glory pd 0))
                             (:wild-points pd 0)
                             (count (:demand-tokens pd [])))))
          (println (str "  Claims: " (pr-str (:contest-claims state))))
          (println (str "  Result: " (:game-over state)))
          state)

        (let [[phase choices] (choice/find-state-raw state)
              player (game/current-player state)
              round (:round state 1)
              turn (:turn-in-round state 1)]

          (when (or (not= round prev-round) (not= turn prev-turn))
            (println (format "─── Round %d Turn %d ──────────────────────────" round turn)))

          (if (or (= phase :game-over) (empty? choices))
            (recur (assoc state :game-over {:reason :stuck}) step round turn)

            (let [weights (get personality-map player pers/default-weights)
                  [ck next-state] (or (pers/personality-step state weights)
                                       (when (seq choices)
                                         [(first (keys choices)) (first (vals choices))]))
                  ;; Compute scoring deltas
                  pd-before (game/player-data state player)
                  pd-after (when next-state (game/player-data next-state player))
                  d-amity (when pd-after (- (:amity pd-after 0) (:amity pd-before 0)))
                  d-glory (when pd-after (- (:glory pd-after 0) (:glory pd-before 0)))]

              (println (format "  [%d] %s %s → %s%s%s"
                               step player (name phase) (pr-str ck)
                               (if (and d-amity (pos? d-amity)) (str " +♥" d-amity) "")
                               (if (and d-glory (pos? d-glory)) (str " +⚡" d-glory) "")))

              (if next-state
                (let [effective (choice/advance-through-trivial next-state replay-protected-phases)]
                  (recur effective (inc step) round turn))
                (recur state (inc step) round turn)))))))))

