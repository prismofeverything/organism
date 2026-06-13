(ns eridu.evolve
  "Genetic algorithm for evolving AI personalities in Eridu.
   Architecture inspired by the Oroboros Aquarium pattern:
   niche fitness, adaptive cull, archive of peak performers,
   gradient tracking, and diversity-aware selection.

   Runs as a background process, maintaining a population of weight vectors
   and evolving them through tournament selection + Elo rating."
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [eridu.personality :as pers]
   [eridu.simulate :as sim]
   [eridu.game :as game]
   [eridu.choice :as choice]))

;; =============================================================================
;; Elo rating system
;; =============================================================================

(def initial-elo 1500)
(def k-factor 32)

(defn expected-score [elo-a elo-b]
  (/ 1.0 (+ 1.0 (Math/pow 10 (/ (- elo-b elo-a) 400.0)))))

(defn update-elo
  "Update Elo ratings for all players in a game based on reputation scores."
  [elo-map game-results]
  (let [n (count game-results)
        ranked (sort-by #(- (:reputation %)) game-results)]
    (reduce
     (fn [elos {:keys [personality reputation]}]
       (let [current-elo (get elos personality initial-elo)
             adjustments
             (for [other ranked
                   :when (not= (:personality other) personality)
                   :let [other-elo (get elos (:personality other) initial-elo)
                         expected (expected-score current-elo other-elo)
                         actual (cond
                                  (> reputation (:reputation other)) 1.0
                                  (= reputation (:reputation other)) 0.5
                                  :else 0.0)]]
               (* (/ k-factor (dec n)) (- actual expected)))
             total-adj (reduce + adjustments)]
         (assoc elos personality (+ current-elo total-adj))))
     elo-map
     ranked)))

;; =============================================================================
;; Game metrics (ported from metrics.py patterns)
;; =============================================================================

(defn measure-game-metrics
  "Compute per-game metrics: branching factor, mechanism coverage, balance.
   Runs a single game and collects stats during play."
  [game-result player-configs]
  (let [state (:final-state game-result)
        summaries (sim/game-result-summary game-result player-configs)
        reps (map :reputation summaries)
        n (count reps)
        ;; Balance: 1 - max deviation from equal win expectation
        ideal (/ 1.0 n)
        max-rep (apply max reps)
        win-shares (map #(if (pos? max-rep) (/ (double %) max-rep) ideal) reps)
        max-dev (apply max (map #(Math/abs (- % ideal)) win-shares))
        balance (max 0.0 (- 1.0 (/ max-dev (max ideal 0.001))))
        ;; Mechanism coverage: count which action types were used (from log)
        log (:log state [])
        action-types-used (set (keep #(when (= :action-select (:type %))
                                         (:action-type %))
                                     log))
        coverage (/ (double (count action-types-used))
                    (double (count game/action-types)))]
    {:balance balance
     :mechanism-coverage coverage
     :action-types-used action-types-used
     :avg-reputation (/ (double (reduce + reps)) n)}))

;; =============================================================================
;; Organism: wraps a personality with evolution metadata
;; =============================================================================

(defn make-organism
  "Create an organism from a personality with tracking metadata."
  [personality & {:keys [parent-richness parent-name lineage-id]
                  :or {parent-richness 0.0 parent-name nil lineage-id -1}}]
  {:personality      personality
   :name             (:name personality)
   :elo              initial-elo
   :avg-reputation   0.0
   :games            0
   :age              0
   :evaluated?       false
   :parent-richness  parent-richness
   :parent-name      parent-name
   :delta            0.0        ;; richness - parent_richness
   :best-delta       0.0
   ;; Archetype region for diversity tracking
   :region           (let [p personality
                           top-role (first (:role-priority p))]
                       (str (name top-role) "-"
                            (cond (> (:sell-weight p 1) 1.5) "sell"
                                  (> (:deploy-weight p 1) 1.5) "deploy"
                                  (> (:temple-weight p 1) 1.5) "temple"
                                  (> (:influence-weight p 1) 1.5) "influence"
                                  :else "balanced")))})

;; =============================================================================
;; Niche fitness (ported from Aquarium._niche_fitness)
;; =============================================================================

(defn niche-fitness
  "Fitness combining absolute quality, gradient, and diversity.
   - Richness (Elo): how good is this personality absolutely?
   - Gradient: did this mutation improve over its parent?
   - Diversity: fewer neighbors in same region = bonus."
  [organism population]
  (let [region (:region organism)
        same-region (count (filter #(= (:region %) region) population))

        ;; 1. Richness (50%): Elo rating
        richness-score (:elo organism initial-elo)

        ;; 2. Gradient (30%): reward positive improvement
        gradient-score (cond
                         (pos? (:delta organism 0)) (* (:delta organism) 3.0)
                         (pos? (:best-delta organism 0)) (* (:best-delta organism) 0.5)
                         :else 0.0)

        ;; 3. Diversity (20%): lonely regions get a bonus
        diversity-score (/ 80.0 (max same-region 1))]

    (+ richness-score gradient-score diversity-score)))

;; =============================================================================
;; Population management
;; =============================================================================

(defn initial-population
  "Create initial population: 60% archetypes (with variation), 40% random."
  [size]
  (let [archetype-count (int (* size 0.6))
        random-count (- size archetype-count)
        per-archetype (max 1 (int (/ archetype-count (count pers/archetypes))))
        archetype-pool
        (take archetype-count
              (for [arch pers/archetypes
                    i (range per-archetype)]
                (make-organism
                 (if (zero? i) arch (pers/mutate-personality arch 0.3)))))
        random-pool (repeatedly random-count
                                #(make-organism (pers/random-personality)))]
    (vec (concat archetype-pool random-pool))))

;; =============================================================================
;; Tournament evaluation
;; =============================================================================

(defn run-tournament
  "Run round-robin tournament. Returns updated organisms with Elo + stats."
  [organisms {:keys [games-per-matchup player-counts]
              :or {games-per-matchup 3 player-counts [2 3 4]}}]
  (let [elo-map (atom (into {} (map (juxt :name :elo) organisms)))
        rep-totals (atom (into {} (map (juxt :name (constantly {:sum 0.0 :n 0 :feats 0}))
                                       organisms)))
        game-count (atom 0)
        pop-size (count organisms)]

    (doseq [pc player-counts]
      (let [n-games (* games-per-matchup (max 1 (int (/ (* pop-size 3) pc))))]
        (dotimes [_ n-games]
          (let [selected (take pc (shuffle organisms))
                configs (mapv (fn [o] {:key (:name o) :personality (:personality o)}) selected)
                result (sim/run-game configs)
                summary (sim/game-result-summary result configs)]
            (swap! elo-map update-elo summary)
            (doseq [{:keys [personality reputation feats-claimed]} summary]
              (swap! rep-totals update personality
                     (fn [m] {:sum (+ (:sum m 0.0) reputation)
                              :n (inc (:n m 0))
                              :feats (+ (:feats m 0) (or feats-claimed 0))})))
            (swap! game-count inc)))))

    ;; Update organisms with tournament results
    (let [final-elo @elo-map
          final-rep @rep-totals]
      {:organisms
       (mapv (fn [o]
               (let [rep-data (get final-rep (:name o) {:sum 0 :n 0 :feats 0})
                     new-elo (get final-elo (:name o) initial-elo)
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
;; Adaptive evolution (ported from Aquarium._evolve)
;; =============================================================================

(defn evolve-generation
  "Create next generation using reputation-based selection.
   1. Remove organisms with avg-feats=0, but only if enough feat-achievers
      exist to maintain 60% of population. Otherwise mark for lower priority.
   2. Cut bottom 10% by avg reputation, replace with top 10% clones.
   3. Apply crossover + mutation to produce children."
  [organisms {:keys [pop-size mutation-rate elite-count]
              :or {pop-size 30 mutation-rate 0.3 elite-count 4}}]
  (let [n (count organisms)
        ;; Sort by avg reputation (the actual score, not Elo)
        sorted-orgs (sort-by #(- (:avg-reputation % 0)) organisms)

        ;; Phase 1: Remove feat-less organisms — but preserve population diversity
        ;; Only replace feat-failures if enough feat-achievers to fill 60% of population
        feat-achievers (filterv #(pos? (:avg-feats % 0)) sorted-orgs)
        feat-failures  (filterv #(zero? (:avg-feats % 0)) sorted-orgs)
        min-achiever-count (int (* n 0.6))
        phase1-orgs
        (if (and (seq feat-achievers)
                 (seq feat-failures)
                 (>= (count feat-achievers) min-achiever-count))
          ;; Enough feat-achievers: replace all failures
          (let [replacements
                (mapv (fn [i]
                        (let [parent (nth feat-achievers
                                         (mod i (count feat-achievers)))]
                          (make-organism
                           (pers/mutate-personality (:personality parent)
                                                    (* mutation-rate 1.5))
                           :parent-richness (:avg-reputation parent 0)
                           :parent-name (:name parent))))
                      (range (count feat-failures)))]
            (into (vec feat-achievers) replacements))
          ;; Not enough achievers: keep everyone, just sort feat-failures to bottom
          (vec (concat feat-achievers
                       (sort-by #(- (:avg-reputation % 0)) feat-failures))))

        ;; Phase 2: Cut bottom 10% by avg reputation, replace with top 10%
        phase2-sorted (sort-by #(- (:avg-reputation % 0)) phase1-orgs)
        n2 (count phase2-sorted)
        n-cut (max 1 (int (* n2 0.1)))
        n-keep (- n2 n-cut)
        survivors (vec (take n-keep phase2-sorted))
        top-parents (take (max 1 (int (* n2 0.1))) phase2-sorted)

        ;; Phase 3: Generate children — mix of evolved + fresh diversity
        children-needed (- pop-size (count survivors))
        n-fresh (max 1 (int (* children-needed 0.3)))  ;; 30% fresh randoms
        n-evolved (- children-needed n-fresh)
        ;; Use wider parent pool — top 50% of survivors, not just top 10%
        wide-parents (take (max 2 (int (* (count survivors) 0.5))) survivors)
        evolved-children
        (vec
         (for [i (range (max 0 n-evolved))
               :let [pa (nth wide-parents (mod i (count wide-parents)))
                     pb (nth wide-parents (mod (inc i) (count wide-parents)))
                     ;; Gentle mutation for high-rep organisms
                     gentle? (> (:avg-reputation pa 0) 8)
                     mut-rate (if gentle? (* mutation-rate 0.5) mutation-rate)
                     child-personality (-> (pers/crossover (:personality pa)
                                                           (:personality pb))
                                          (pers/mutate-personality mut-rate))]]
           (make-organism child-personality
                          :parent-richness (:avg-reputation pa 0)
                          :parent-name (:name pa))))
        ;; Fresh randoms to prevent population collapse
        fresh-children (vec (repeatedly n-fresh
                                        #(make-organism (pers/random-personality))))
        children (into evolved-children fresh-children)]

    (log/info (str "Evolution: feat-failures=" (count feat-failures)
                   " cut=" n-cut
                   " survivors=" (count survivors)
                   " children=" (count children)
                   " top-rep=" (format "%.1f" (:avg-reputation (first sorted-orgs) 0.0))))

    ;; Age everyone and combine
    (vec (concat
          (map #(update % :age inc) survivors)
          children))))

;; =============================================================================
;; Archive: hall of fame of peak performers
;; =============================================================================

(defn update-archive
  "Keep peak representatives per region, ensuring diversity."
  [archive organisms max-archive max-per-region]
  (let [candidates (concat archive (filter :evaluated? organisms))
        by-region (group-by :region candidates)
        ;; Keep top N per region
        new-archive (vec (mapcat (fn [[_ orgs]]
                                   (take max-per-region
                                         (sort-by #(- (:elo % 0)) orgs)))
                                 by-region))
        ;; Also keep global top 10
        global-top (take 10 (sort-by #(- (:elo % 0)) candidates))
        combined (distinct (concat new-archive global-top))]
    (vec (take max-archive (sort-by #(- (:elo % 0)) combined)))))

;; =============================================================================
;; Persistence: save/load state to EDN file
;; =============================================================================

(def state-path "eridu-evolution-state.edn")

(defn save-evolution-state! [state]
  ;; Persist FULL organisms — including their evolved :personality weight
  ;; vectors. Previously these were stripped, so every resume reattached a
  ;; fresh random personality and silently threw away all accumulated learning.
  ;; Personalities are plain EDN (numbers, keywords, vectors), so round-trip
  ;; cleanly. This is what makes evolution actually carry the baseline forward.
  (let [serializable (dissoc state :running?)]
    (spit state-path (pr-str serializable))))

(defn load-evolution-state []
  (when (.exists (io/file state-path))
    (try
      (edn/read-string (slurp state-path))
      (catch Exception e
        (log/warn "Failed to load evolution state:" (.getMessage e))
        nil))))

;; =============================================================================
;; Evolution loop
;; =============================================================================

(defonce evolution-state
  (atom {:running?    false
         :generation  0
         :population  []
         :archive     []
         :elo-history []
         :best-ever   nil
         :config      {}}))

(defn start-evolution!
  "Start the evolution process in a background thread."
  [config]
  (when (:running? @evolution-state)
    (throw (ex-info "Evolution already running" {})))

  (let [{:keys [pop-size generations mutation-rate elite-count player-counts
                games-per-matchup max-archive max-per-region]
         :or {pop-size 30 generations 100 mutation-rate 0.3
              elite-count 4 player-counts [2 3 4]
              games-per-matchup 3 max-archive 50 max-per-region 3}} config]

    ;; Try to resume from saved state
    (let [saved (load-evolution-state)
          initial-pop (if (and saved (seq (:population saved)))
                        (do (log/info "Resuming evolution from saved state, gen"
                                      (:generation saved))
                            ;; Carry the evolved weights forward. New states embed
                            ;; :personality directly; only fall back to archetype
                            ;; lookup / random for legacy states that lack it.
                            (let [by-name (into {} (map (juxt :name identity)
                                                        pers/archetypes))]
                              (mapv (fn [o]
                                      (if (:personality o)
                                        o
                                        (assoc o :personality
                                               (or (get by-name (:name o))
                                                   (pers/random-personality)))))
                                    (:population saved))))
                        (initial-population pop-size))]

      (swap! evolution-state assoc
             :running? true
             :generation (or (:generation saved) 0)
             :population initial-pop
             :archive (or (:archive saved) [])
             :elo-history (or (:elo-history saved) [])
             :best-ever (:best-ever saved)
             :config config)

      (future
        (try
          (loop [gen (or (:generation saved) 0)]
            (when (and (:running? @evolution-state) (< gen generations))
              (let [pop (:population @evolution-state)
                    _ (log/info (str "Evolution gen " gen
                                     " — evaluating " (count pop) " personalities"))

                    ;; Run tournament
                    {:keys [organisms total-games]}
                    (run-tournament pop {:games-per-matchup games-per-matchup
                                         :player-counts player-counts})

                    ;; Find best
                    best (first (sort-by #(- (:elo %)) organisms))

                    ;; Update archive
                    new-archive (update-archive (:archive @evolution-state) organisms
                                                max-archive max-per-region)

                    ;; Evolve next generation
                    next-pop (evolve-generation organisms
                                                 {:pop-size pop-size
                                                  :mutation-rate mutation-rate
                                                  :elite-count elite-count})]

                (swap! evolution-state
                       (fn [s]
                         (-> s
                             (assoc :generation gen
                                    :population next-pop
                                    :archive new-archive)
                             (update :elo-history conj
                                     {:gen gen
                                      :best-name (:name best)
                                      :best-elo (:elo best)
                                      :best-avg-rep (:avg-reputation best)
                                      :pop-avg-elo (double (/ (reduce + (map :elo organisms))
                                                              (max 1 (count organisms))))
                                      :total-games total-games
                                      :archive-size (count new-archive)})
                             (assoc :best-ever
                                    (if (or (nil? (:best-ever s))
                                            (> (:elo best) (get-in s [:best-ever :elo] 0)))
                                      (select-keys best [:name :elo :avg-reputation
                                                          :region :games :personality])
                                      (:best-ever s))))))

                ;; Persist state every generation
                (save-evolution-state! @evolution-state)

                (log/info (str "Gen " gen " complete — best: " (:name best)
                               " Elo: " (int (:elo best))
                               " avg-rep: " (format "%.1f" (:avg-reputation best))
                               " archive: " (count new-archive)))
                (recur (inc gen)))))

          (log/info "Evolution complete.")
          (catch Exception e
            (log/error "Evolution error:" (.getMessage e)))
          (finally
            (swap! evolution-state assoc :running? false)))))))

(defn stop-evolution! []
  (swap! evolution-state assoc :running? false))

(defn evolution-status []
  (let [s @evolution-state]
    {:running?   (:running? s)
     :generation (:generation s)
     :pop-size   (count (:population s))
     :archive-size (count (:archive s))
     :best-ever  (:best-ever s)
     :latest     (last (:elo-history s))
     :config     (:config s)}))

(defn top-personalities
  "Return the current top N personalities with their weights."
  [n]
  (let [s @evolution-state
        ;; Combine population + archive, deduplicate by name
        all-orgs (vals (into {} (map (juxt :name identity)
                                     (concat (:population s) (:archive s)))))
        sorted (take n (sort-by #(- (:elo % 0)) all-orgs))]
    (for [o sorted]
      (merge (select-keys o [:name :elo :avg-reputation :games :region :age
                              :delta :best-delta :parent-name])
             (when-let [p (:personality o)]
               (select-keys p [:sell-weight :temple-weight :deploy-weight
                                :influence-weight :travel-weight :take-weight
                                :role-priority :track-balance :early-role-bias
                                :chain-weight :contest-focus :resource-hoard
                                :excess-penalty :temple-in-demand-city
                                :deploy-near-opponents :travel-for-temple
                                :travel-for-sell :influence-flip-raider]))))))
