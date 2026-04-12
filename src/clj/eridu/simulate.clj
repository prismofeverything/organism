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

(defn- advance-through-trivial
  "Advance state through single-choice non-interactive phases."
  [state]
  (loop [s state n 0]
    (if (> n 200) s ;; safety valve
      (let [[p cs] (choice/find-state-raw s)]
        (if (and (= 1 (count cs))
                 (not (contains? #{:choose-die :choose-action :resolve-landing :game-over} p)))
          (let [ns (first (vals cs))]
            (if ns (recur ns (inc n)) s))
          s)))))

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
     :caravan-city  (:caravan pdata)
     :bonus-board-id (get-in state [:bonus-boards player-key])}))

(defn run-game
  "Run a complete game headlessly.
   `player-configs` is a vector of {:key :personality} maps.
   Returns {:final-state :snapshots :result}."
  [player-configs]
  (let [player-keys (mapv :key player-configs)
        personality-map (into {} (map (juxt :key :personality) player-configs))
        initial (game/initial-state player-keys)
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
      (assoc result :snapshots (into (:snapshots result) final-snaps)))))

(defn game-result-summary
  "Extract summary from a completed game."
  [game-result player-configs]
  (let [state (:final-state game-result)
        players (:players state)]
    (for [{:keys [key personality]} player-configs
          :let [pdata (get players key)]]
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
       :demands-fulfilled (count (:demand-tokens pdata []))})))

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
   :temples-placed :temples-flipped :raiders-deployed :demands-fulfilled])

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
