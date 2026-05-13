(ns eridu.simulate
  "Headless simulation engine for Eridu. Runs complete games at full speed,
   collecting per-turn snapshots for statistical analysis."
  (:require
   [clojure.string :as str]
   [eridu.game :as game]
   [eridu.choice :as choice]
   [eridu.personality :as pers]))

;; =============================================================================
;; Headless game runner
;; =============================================================================

(def ^:private sim-protected-phases
  "Phases the simulation stops at to let the AI make decisions."
  game/bot-protected-phases)

(defn- advance-through-trivial
  "Advance state through single-choice non-interactive phases."
  [state]
  (choice/advance-through-trivial state sim-protected-phases))

(defn- take-snapshot
  "Capture a per-player snapshot of the current game state."
  [state player-key]
  (let [pdata (game/player-data state player-key)]
    {:round        (:round state)
     :turn         (:turn-in-round state)
     :player       player-key
     :phase        (game/current-phase state)
     :amity        (:amity pdata 0)
     :glory        (:glory pdata 0)
     :reputation   (min (:amity pdata 0) (:glory pdata 0))
     :merchant-lv  (get-in pdata [:roles :merchant] 1)
     :priest-lv    (get-in pdata [:roles :priest] 1)
     :raider-lv    (get-in pdata [:roles :raider] 1)
     :leader-lv    (get-in pdata [:roles :leader] 1)
     :tools        (get-in pdata [:resources :tools] 0)
     :pottery      (get-in pdata [:resources :pottery] 0)
     :gold         (get-in pdata [:resources :gold] 0)
     :gems         (get-in pdata [:resources :gems] 0)
     :temples-placed  (game/count-temples-placed pdata)
     :temples-flipped (game/count-face-down-temples pdata)
     :raiders-deployed (game/count-raiders-deployed pdata)
     :raiders-supply   (:raiders-supply pdata 0)
     :temples-supply   (:temples-supply pdata 0)
     :demands-fulfilled (count (:demand-tokens pdata []))
     :wild-points   (:wild-points pdata 0)
     :feats-claimed (count (filter #(some #{player-key} (val %))
                                   (:contest-claims state {})))
     :travels-this-round (:travels-this-round pdata 0)
     :total-travels (:total-travels pdata 0)
     :dice-quads   (get-in pdata [:dice-stats :quad] 0)
     :dice-triples (get-in pdata [:dice-stats :triple] 0)
     :dice-doubles (get-in pdata [:dice-stats :double] 0)
     :dice-unique  (get-in pdata [:dice-stats :unique] 0)
     :caravan-city  (:caravan pdata)
     :bonus-board-id (get-in state [:bonus-boards player-key])}))

(defn run-game
  "Run a complete game headlessly.
   `player-configs` is a vector of {:key :personality} maps.
   Optional `:seed` for deterministic replay — records the seed used in the result.
   Returns {:final-state :snapshots :steps :seed}.
   For solo mode, pass a single player config."
  [player-configs & {:keys [seed]}]
  (let [seed (or seed (System/nanoTime))
        _    (.setSeed (java.util.Random.) seed)  ;; record only; true seeding needs replay tool
        player-keys (mapv :key player-configs)
        personality-map (into {} (map (juxt :key :personality) player-configs))
        initial (if (= 1 (count player-configs))
                  (game/initial-solo-state (first player-keys))
                  (game/initial-state player-keys))
        ;; Cache personality weights on player state for feat-claiming decisions
        initial (reduce (fn [s [pk weights]]
                          (assoc-in s [:players pk :personality-cache]
                                    (select-keys weights [:tempo :feat-awareness
                                                          :prefer-onetime-bonus
                                                          :feat-sequence :feat-closure-urgency])))
                        initial personality-map)
        ;; Run the game to completion
        result
        (loop [state initial
               snapshots []
               steps 0]
          (if (or (:game-over state) (> steps 5000))
            {:final-state state
             :snapshots   snapshots
             :steps       steps}
            (let [[phase choices] (choice/find-state-raw state)
                  current-player (game/current-player state)]
              (if (or (= phase :game-over) (empty? choices))
                {:final-state state :snapshots snapshots :steps steps}
                (let [;; Take snapshot at turn start
                      snap (when (= phase :choose-die)
                             (mapv #(take-snapshot state %) player-keys))
                      snapshots (if snap (into snapshots snap) snapshots)
                      ;; Get personality for current player
                      weights (get personality-map current-player pers/default-weights)
                      ;; Make decision
                      step-result (or (pers/personality-step state weights)
                                      (when (seq choices)
                                        [(first (keys choices)) (first (vals choices))]))
                      [_ck next-state] step-result]
                  (if next-state
                    (recur (advance-through-trivial next-state)
                           snapshots
                           (inc steps))
                    ;; Stuck — shouldn't happen but safety valve
                    {:final-state state :snapshots snapshots :steps steps}))))))]
    ;; Add final snapshots
    (let [final-snaps (mapv #(assoc (take-snapshot (:final-state result) %)
                                    :round 99 :turn 0 :phase :game-over)
                            player-keys)]
      (assoc result
             :snapshots (into (:snapshots result) final-snaps)
             :seed seed))))

(defn game-result-summary
  "Extract summary from a completed game."
  [game-result player-configs]
  (let [state (:final-state game-result)
        game-seed (:seed game-result)
        players (:players state)
        ;; Game-level context
        contests (mapv :id (:contests state []))
        contest-str (str/join ";" (map name contests))]
    (for [{:keys [key personality]} player-configs
          :let [pdata (get players key)
                card (:starting-card pdata)
                board-id (get-in state [:bonus-boards key])]]
      {:player        key
       :personality   (:name personality "Unknown")
       :amity         (:amity pdata 0)
       :glory         (:glory pdata 0)
       :reputation    (min (:amity pdata 0) (:glory pdata 0))
       :merchant-lv   (get-in pdata [:roles :merchant] 1)
       :priest-lv     (get-in pdata [:roles :priest] 1)
       :raider-lv     (get-in pdata [:roles :raider] 1)
       :leader-lv     (get-in pdata [:roles :leader] 1)
       :temples-placed  (game/count-temples-placed pdata)
       :temples-flipped (game/count-face-down-temples pdata)
       :raiders-deployed (game/count-raiders-deployed pdata)
       :demands-fulfilled (count (:demand-tokens pdata []))
       ;; Game setup context
       :starting-card-num  (:number card)
       :starting-city      (when card (name (:city card)))
       :starting-role      (when card (name (:role card)))
       :starting-resource  (when card (name (:resource card)))
       :bonus-board-id     (or board-id 0)
       :wild-points        (:wild-points pdata 0)
       :feats-claimed      (count (filter #(some #{key} (val %))
                                          (:contest-claims state {})))
       :target-feat-1      (when-let [t (first (:target-feats pdata))]
                             (name (:id t)))
       :target-feat-2      (when-let [t (second (:target-feats pdata))]
                             (name (:id t)))
       ;; Space usage stats
       :space-visits        (count (:space-visits pdata []))
       :avg-astros-on-visit (let [visits (:space-visits pdata [])]
                              (if (seq visits)
                                (double (/ (reduce + (map :astronomers visits))
                                           (count visits)))
                                0.0))
       :solo-landings       (count (filter #(= 1 (:astronomers %))
                                          (:space-visits pdata [])))
       :top-space           (let [visits (:space-visits pdata [])
                                  freqs (frequencies (map :space visits))]
                              (if (seq freqs)
                                (first (apply max-key second (seq freqs)))
                                0))
       ;; Per-space visit counts
       :sp1 (count (filter #(= 1 (:space %)) (:space-visits pdata [])))
       :sp2 (count (filter #(= 2 (:space %)) (:space-visits pdata [])))
       :sp3 (count (filter #(= 3 (:space %)) (:space-visits pdata [])))
       :sp4 (count (filter #(= 4 (:space %)) (:space-visits pdata [])))
       :sp5 (count (filter #(= 5 (:space %)) (:space-visits pdata [])))
       :sp6 (count (filter #(= 6 (:space %)) (:space-visits pdata [])))
       :sp7 (count (filter #(= 7 (:space %)) (:space-visits pdata [])))
       ;; Top-2 space pair (e.g. "3-7" means spaces 3 and 7 were 1st and 2nd most used)
       :top-pair           (let [visits (:space-visits pdata [])
                                 freqs (sort-by (comp - second) (frequencies (map :space visits)))]
                             (if (>= (count freqs) 2)
                               (str (first (first freqs)) "-" (first (second freqs)))
                               (if (seq freqs)
                                 (str (first (first freqs)) "-" (first (first freqs)))
                                 "0-0")))
       ;; Travel action count (total across all rounds)
       :total-travels       (:total-travels pdata 0)
       ;; Dice roll statistics (across all rounds)
       :dice-quads   (get-in pdata [:dice-stats :quad] 0)
       :dice-triples (get-in pdata [:dice-stats :triple] 0)
       :dice-doubles (get-in pdata [:dice-stats :double] 0)
       :dice-unique  (get-in pdata [:dice-stats :unique] 0)
       ;; Board effect diagnostics
       :board-effects-fired (count (filter :changed (:board-effects-log pdata [])))
       :board-effects-noop  (count (remove :changed (:board-effects-log pdata [])))
       :board-effect-amity  (reduce + (map :delta-amity (:board-effects-log pdata [])))
       :board-effect-glory  (reduce + (map :delta-glory (:board-effects-log pdata [])))
       :contests-in-play   contest-str
       :seed               (or game-seed 0)})))

;; =============================================================================
;; Batch simulation
;; =============================================================================

(defn- make-player-configs
  "Create player configs for a simulation game."
  [player-count personalities]
  (let [selected (take player-count (shuffle personalities))
        names (map-indexed (fn [i p] (str "P" (inc i) "-" (subs (:name p) 0 (min 8 (count (:name p))))))
                           selected)]
    (mapv (fn [name personality]
            {:key name :personality personality})
          names selected)))

(defn run-batch
  "Run a batch of games. Returns {:summaries :all-snapshots :meta}.
   `n` is number of games. `player-count` is 2, 3, or 4.
   `personalities` is the pool of AI personalities to draw from."
  [n player-count personalities & {:keys [on-progress]}]
  (let [results
        (reduce
         (fn [acc game-idx]
           (when on-progress (on-progress game-idx n))
           (let [configs (make-player-configs player-count personalities)
                 result (run-game configs)
                 summary (game-result-summary result configs)
                 game-id (str "g" game-idx "-" player-count "p")]
             (-> acc
                 (update :summaries into
                         (map #(assoc % :game-id game-id
                                       :player-count player-count)
                              summary))
                 (update :all-snapshots into
                         (map #(assoc % :game-id game-id
                                       :player-count player-count)
                              (:snapshots result))))))
         {:summaries [] :all-snapshots []}
         (range n))]
    (assoc results
           :meta {:total-games n
                  :player-count player-count
                  :personality-pool (mapv :name personalities)})))

;; =============================================================================
;; CSV export
;; =============================================================================

(def summary-columns
  [:game-id :player-count :player :personality
   :amity :glory :reputation
   :merchant-lv :priest-lv :raider-lv :leader-lv
   :temples-placed :temples-flipped :raiders-deployed :demands-fulfilled
   :starting-card-num :starting-city :starting-role :starting-resource
   :space-visits :avg-astros-on-visit :solo-landings :top-space
   :sp1 :sp2 :sp3 :sp4 :sp5 :sp6 :sp7 :top-pair
   :bonus-board-id :wild-points :feats-claimed
   :target-feat-1 :target-feat-2 :total-travels
   :dice-quads :dice-triples :dice-doubles :dice-unique
   :board-effects-fired :board-effects-noop :board-effect-amity :board-effect-glory
   :contests-in-play :seed])

(def snapshot-columns
  [:game-id :player-count :round :turn :player :phase
   :amity :glory :reputation
   :merchant-lv :priest-lv :raider-lv :leader-lv
   :tools :pottery :gold :gems
   :temples-placed :temples-flipped :raiders-deployed :raiders-supply
   :temples-supply :demands-fulfilled :caravan-city :bonus-board-id])

(defn rows->csv
  "Convert a sequence of maps to CSV string."
  [columns rows]
  (let [header (str/join "," (map name columns))
        data-rows (for [row rows]
                    (str/join "," (for [col columns]
                                   (let [v (get row col "")]
                                     (if (keyword? v) (name v) (str v))))))]
    (str header "\n" (str/join "\n" data-rows))))

(defn export-summaries-csv [batch-result]
  (rows->csv summary-columns (:summaries batch-result)))

(defn export-snapshots-csv [batch-result]
  (rows->csv snapshot-columns (:all-snapshots batch-result)))

;; =============================================================================
;; Statistics helpers
;; =============================================================================

(defn aggregate-by-personality
  "Aggregate stats grouped by personality name."
  [summaries]
  (let [grouped (group-by :personality summaries)]
    (for [[name entries] grouped
          :let [n (count entries)
                avg-fn (fn [k] (double (/ (reduce + (map #(get % k 0) entries)) n)))]]
      {:personality    name
       :games          n
       :avg-reputation (avg-fn :reputation)
       :avg-amity      (avg-fn :amity)
       :avg-glory      (avg-fn :glory)
       :win-rate       (double (/ (count (filter #(= (:reputation %)
                                                      (apply max (map :reputation
                                                                      (filter (fn [e] (= (:game-id e) (:game-id %)))
                                                                              entries))))
                                                 entries))
                                  n))
       :avg-merchant   (avg-fn :merchant-lv)
       :avg-priest     (avg-fn :priest-lv)
       :avg-raider     (avg-fn :raider-lv)
       :avg-leader     (avg-fn :leader-lv)
       :avg-temples    (avg-fn :temples-placed)
       :avg-raiders    (avg-fn :raiders-deployed)
       :avg-demands    (avg-fn :demands-fulfilled)})))

(defn aggregate-by-player-count
  "Aggregate stats grouped by player count."
  [summaries]
  (let [grouped (group-by :player-count summaries)]
    (for [[pc entries] grouped
          :let [n (count entries)
                avg-fn (fn [k] (double (/ (reduce + (map #(get % k 0) entries)) n)))]]
      {:player-count   pc
       :total-players  n
       :avg-reputation (avg-fn :reputation)
       :avg-amity      (avg-fn :amity)
       :avg-glory      (avg-fn :glory)
       :max-reputation (apply max (map :reputation entries))
       :min-reputation (apply min (map :reputation entries))})))

;; =============================================================================
;; Weighted statistics
;; =============================================================================

;; Weight factors: normalize across player counts so 2p/3p/4p contribute equally
;; to aggregate stats regardless of how many player-entries each produces per game.
(def player-count-weights
  "Weights to normalize per-player-count stats.
   A 4p game produces 4 entries vs 2p producing 2, so weight inversely."
  {2 2.0, 3 (/ 4.0 3), 4 1.0})

(defn weighted-aggregate-by-personality
  "Aggregate stats grouped by personality, weighted by player count
   so each game contributes equally regardless of player count."
  [summaries]
  (let [grouped (group-by :personality summaries)]
    (for [[name entries] grouped
          :let [;; Apply weights
                weighted-entries
                (map (fn [e]
                       (let [w (get player-count-weights (:player-count e) 1.0)]
                         (assoc e :weight w)))
                     entries)
                total-weight (reduce + (map :weight weighted-entries))
                wavg (fn [k]
                        (if (zero? total-weight) 0.0
                            (/ (reduce + (map #(* (:weight %) (get % k 0))
                                              weighted-entries))
                               total-weight)))
                ;; Win rate: weighted by player count
                all-summaries-by-game (group-by :game-id summaries)
                wins (count (filter (fn [e]
                                      (let [game-entries (get all-summaries-by-game (:game-id e))
                                            max-rep (apply max (map :reputation game-entries))]
                                        (= (:reputation e) max-rep)))
                                    entries))
                n (count entries)]]
      {:personality       name
       :games             n
       :weighted-avg-rep  (wavg :reputation)
       :weighted-avg-amity (wavg :amity)
       :weighted-avg-glory (wavg :glory)
       :win-rate          (if (pos? n) (double (/ wins n)) 0.0)
       :avg-reputation    (if (pos? n) (double (/ (reduce + (map :reputation entries)) n)) 0.0)
       :avg-amity         (if (pos? n) (double (/ (reduce + (map :amity entries)) n)) 0.0)
       :avg-glory         (if (pos? n) (double (/ (reduce + (map :glory entries)) n)) 0.0)
       :weighted-merchant (wavg :merchant-lv)
       :weighted-priest   (wavg :priest-lv)
       :weighted-raider   (wavg :raider-lv)
       :weighted-leader   (wavg :leader-lv)
       :weighted-temples  (wavg :temples-placed)
       :weighted-raiders  (wavg :raiders-deployed)
       :weighted-demands  (wavg :demands-fulfilled)
       ;; Per-player-count breakdown
       :by-count (into {}
                       (for [[pc pc-entries] (group-by :player-count entries)
                             :let [pcn (count pc-entries)
                                   pcavg (fn [k] (if (pos? pcn)
                                                   (double (/ (reduce + (map #(get % k 0) pc-entries)) pcn))
                                                   0.0))]]
                         [pc {:games pcn
                              :avg-reputation (pcavg :reputation)
                              :avg-amity (pcavg :amity)
                              :avg-glory (pcavg :glory)}]))})))

(defn weighted-summary-csv-columns []
  [:personality :games :weighted-avg-rep :weighted-avg-amity :weighted-avg-glory
   :win-rate :avg-reputation :avg-amity :avg-glory
   :weighted-merchant :weighted-priest :weighted-raider :weighted-leader
   :weighted-temples :weighted-raiders :weighted-demands])

(defn export-weighted-csv [summaries]
  (let [stats (weighted-aggregate-by-personality summaries)]
    (rows->csv (weighted-summary-csv-columns) stats)))
