(ns eridu.evolve
  "Genetic algorithm for evolving AI personalities in Eridu.
   Runs as a background process, maintaining a population of weight vectors
   and evolving them through tournament selection."
  (:require
   [clojure.tools.logging :as log]
   [eridu.personality :as pers]
   [eridu.simulate :as sim]))

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
        ;; Rank players by reputation (higher = better)
        ranked (sort-by #(- (:reputation %)) game-results)]
    (reduce
     (fn [elos {:keys [personality reputation]}]
       (let [current-elo (get elos personality initial-elo)
             ;; Compare against each other player
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
;; Population management
;; =============================================================================

(defn initial-population
  "Create initial population: 60% archetypes (with variation), 40% random."
  [size]
  (let [archetype-count (int (* size 0.6))
        random-count (- size archetype-count)
        ;; Create archetype variants — each archetype gets multiple slots with small mutations
        per-archetype (max 1 (int (/ archetype-count (count pers/archetypes))))
        archetype-pool
        (take archetype-count
              (for [arch pers/archetypes
                    i (range per-archetype)]
                (if (zero? i)
                  arch  ;; keep one pure copy
                  (pers/mutate-personality arch 0.3))))
        ;; Random personalities
        random-pool (repeatedly random-count pers/random-personality)]
    (vec (concat archetype-pool random-pool))))

;; =============================================================================
;; Tournament evaluation
;; =============================================================================

(defn run-tournament
  "Run a round-robin tournament for a population.
   Returns {:elos :avg-reputation :results}."
  [population {:keys [games-per-matchup player-counts]
               :or {games-per-matchup 3
                    player-counts [2 3 4]}}]
  (let [pop-size (count population)
        elo-map (atom (zipmap (map :name population) (repeat initial-elo)))
        all-results (atom [])

        ;; For each player count, run games
        _ (doseq [pc player-counts]
            (let [n-games (* games-per-matchup
                             (max 1 (int (/ (* pop-size 3) pc))))]
              (dotimes [_ n-games]
                (let [configs (mapv (fn [p] {:key (:name p) :personality p})
                                   (take pc (shuffle population)))
                      result (sim/run-game configs)
                      summary (sim/game-result-summary result configs)]
                  (swap! all-results into summary)
                  (swap! elo-map update-elo summary)))))

        ;; Aggregate stats per personality
        results-by-name (group-by :personality @all-results)
        stats (for [[name entries] results-by-name
                    :let [n (count entries)
                          avg-rep (double (/ (reduce + (map :reputation entries)) n))]]
                {:name name
                 :games n
                 :avg-reputation avg-rep
                 :elo (get @elo-map name initial-elo)})]
    {:elos @elo-map
     :stats (sort-by #(- (:elo %)) stats)
     :total-games (count @all-results)}))

;; =============================================================================
;; Selection and breeding
;; =============================================================================

(defn select-parents
  "Tournament selection: pick `n` parents from population based on Elo."
  [population elo-map n]
  (let [sorted (sort-by #(- (get elo-map (:name %) initial-elo)) population)]
    ;; Top 50% are eligible parents, select randomly from them
    (let [eligible (take (max 2 (int (* (count sorted) 0.5))) sorted)]
      (repeatedly n #(rand-nth eligible)))))

(defn evolve-generation
  "Create next generation from current population + tournament results."
  [population elo-map {:keys [pop-size mutation-rate elite-count]
                        :or {pop-size 30 mutation-rate 0.3 elite-count 4}}]
  (let [;; Keep top N as elites (unchanged)
        sorted (sort-by #(- (get elo-map (:name %) initial-elo)) population)
        elites (take elite-count sorted)
        ;; Breed the rest
        children-needed (- pop-size elite-count)
        parents (select-parents population elo-map (* children-needed 2))
        children (for [i (range children-needed)
                       :let [pa (nth parents (* i 2) (first parents))
                             pb (nth parents (inc (* i 2)) (second parents))]]
                   (-> (pers/crossover pa pb)
                       (pers/mutate-personality mutation-rate)))]
    (vec (concat elites children))))

;; =============================================================================
;; Evolution loop (runs as background process)
;; =============================================================================

(defonce evolution-state
  (atom {:running?    false
         :generation  0
         :population  []
         :elo-history []
         :best-ever   nil
         :config      {}}))

(defn start-evolution!
  "Start the evolution process in a background thread.
   Config options:
     :pop-size          — population size (default 30)
     :generations       — max generations to run (default 100)
     :games-per-matchup — games per matchup in tournament (default 3)
     :mutation-rate     — mutation probability per weight (default 0.3)
     :elite-count       — top N kept unchanged (default 4)
     :player-counts     — which player counts to test (default [2 3 4])"
  [config]
  (when (:running? @evolution-state)
    (log/warn "Evolution already running!")
    (throw (ex-info "Evolution already running" {})))

  (let [{:keys [pop-size generations mutation-rate elite-count player-counts
                games-per-matchup]
         :or {pop-size 30 generations 100 mutation-rate 0.3
              elite-count 4 player-counts [2 3 4]
              games-per-matchup 3}} config]

    (swap! evolution-state assoc
           :running? true :generation 0
           :population (initial-population pop-size)
           :elo-history [] :best-ever nil
           :config config)

    (future
      (try
        (loop [gen 0]
          (when (and (:running? @evolution-state) (< gen generations))
            (let [pop (:population @evolution-state)
                  _ (log/info (str "Evolution gen " gen " — evaluating " (count pop) " personalities"))

                  ;; Run tournament
                  tournament (run-tournament pop {:games-per-matchup games-per-matchup
                                                  :player-counts player-counts})
                  best (first (:stats tournament))

                  ;; Evolve next generation
                  next-pop (evolve-generation pop (:elos tournament)
                                               {:pop-size pop-size
                                                :mutation-rate mutation-rate
                                                :elite-count elite-count})]

              (swap! evolution-state
                     (fn [s]
                       (-> s
                           (assoc :generation gen
                                  :population next-pop)
                           (update :elo-history conj
                                   {:gen gen
                                    :best-name (:name best)
                                    :best-elo (:elo best)
                                    :best-avg-rep (:avg-reputation best)
                                    :pop-avg-elo (double (/ (reduce + (map :elo (:stats tournament)))
                                                            (max 1 (count (:stats tournament)))))
                                    :stats (:stats tournament)})
                           (assoc :best-ever
                                  (if (or (nil? (:best-ever s))
                                          (> (:elo best) (:elo (:best-ever s))))
                                    best best)))))

              (log/info (str "Gen " gen " complete — best: " (:name best)
                             " Elo: " (int (:elo best))
                             " avg-rep: " (format "%.1f" (:avg-reputation best))))
              (recur (inc gen)))))

        (log/info "Evolution complete.")
        (catch Exception e
          (log/error "Evolution error:" (.getMessage e)))
        (finally
          (swap! evolution-state assoc :running? false))))))

(defn stop-evolution! []
  (swap! evolution-state assoc :running? false))

(defn evolution-status []
  (let [s @evolution-state]
    {:running?   (:running? s)
     :generation (:generation s)
     :pop-size   (count (:population s))
     :best-ever  (:best-ever s)
     :latest     (last (:elo-history s))
     :config     (:config s)}))

(defn top-personalities
  "Return the current top N personalities with their weights."
  [n]
  (let [s @evolution-state
        latest (last (:elo-history s))
        stats (:stats latest)
        pop (:population s)
        pop-by-name (into {} (map (juxt :name identity) pop))]
    (for [st (take n stats)
          :let [p (get pop-by-name (:name st))]]
      (merge st (select-keys p [:sell-weight :temple-weight :deploy-weight
                                 :influence-weight :travel-weight :take-weight
                                 :role-priority :track-balance :early-role-bias
                                 :chain-weight :contest-focus :resource-hoard
                                 :excess-penalty :temple-in-demand-city
                                 :deploy-near-opponents :travel-for-temple
                                 :travel-for-sell :influence-flip-raider])))))
