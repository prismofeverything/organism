(ns organism.scripts.test-obo-flow
  "Equivalence harness: play a long journey game with the hard-coded
   organism.routes.journey/agent-step bot, capturing every (state, choice)
   pair as we go. Then re-run the saved flowchart OBO bot on each captured
   state and assert it produces the same choice. Print divergences.

   Run with: lein with-profile uberjar run -m organism.scripts.test-obo-flow"
  (:require
   [journey.bot-flow :as bf]
   [journey.choice :as choice]
   [journey.game :as game]
   [organism.mongo :as db]
   [organism.persist-journey-bots :as bots-db]
   [organism.routes.journey :as journey]))

(def mongo-connection
  {:host "localhost"
   :port 27017
   :database "organism"})

(defn- step-effective
  "Same auto-advance as the server: walk past trivial single-choice phases
   except a fixed protected set, so the comparison happens at the same
   choice points the server would actually broadcast."
  [state]
  (let [protected #{:choose-action-type :choose-move :choose-convert
                    :choose-activate :choose-activate-station
                    :choose-activate-self-bonus :choose-activate-owner-bonus
                    :choose-land :game-over}]
    (loop [s state]
      (let [[phase cs] (choice/find-state-raw s)]
        (if (and (= 1 (count cs))
                 (not (contains? protected phase)))
          (let [ns (first (vals cs))]
            (if ns (recur ns) s))
          s)))))

(defn- play-with-hardcoded
  "Play one journey game with the hard-coded agent-step.
   Returns a vector of {:state :phase :choice} entries — one per choice made."
  [seed-players max-steps]
  (loop [s        (game/initial-state seed-players)
         visited  []
         n        0]
    (if (or (>= n max-steps)
            (:game-over s))
      visited
      (let [s        (step-effective s)
            [phase _] (choice/find-state-raw s)
            ;; Track bot-fly-visited the same way the server does — see
            ;; journey_ws/run-bot-turns!
            result   (journey/agent-step s)]
        (if-let [[ck next-state] result]
          (let [tagged (cond
                         (and (vector? ck) (= :fly (first ck)))
                         (update-in next-state [:player-turn :action :bot-fly-visited]
                                    (fnil conj #{}) (second ck))
                         (= phase :choose-fly-to)
                         (update-in next-state [:player-turn :action :bot-fly-visited]
                                    (fnil conj #{}) ck)
                         :else next-state)]
            (recur tagged
                   (conj visited {:state s :phase phase :choice ck})
                   (inc n)))
          visited)))))

(defn- compare-choices
  "Run the flowchart bot on each captured state and report any divergences."
  [definition trace]
  (let [results
        (for [{:keys [state choice] :as entry} trace]
          (let [[_ choices-map]  (choice/find-state-raw state)
                flow-result      (bf/agent-step definition state)
                flow-ck          (first flow-result)
                ;; Consider matching if keys equal OR both produce the same next-state
                same-key?        (= choice flow-ck)
                same-state?      (= (get choices-map choice)
                                    (get choices-map flow-ck))]
            (assoc entry :flow-choice flow-ck
                         :match? (or same-key? same-state?))))
        divergences (remove :match? results)]
    {:total       (count results)
     :matches     (count (filter :match? results))
     :divergences divergences}))

(defn- short-state
  "Compact summary for divergence reports — just the player + a few resources."
  [s]
  {:player (game/current-player s)
   :ark    (:ark s)
   :round  (:round s)})

(defn- choices-summary [state]
  (let [[phase choices] (choice/find-state-raw state)]
    {:phase phase
     :n     (count choices)
     :keys  (vec (take 12 (keys choices)))}))

(defn- detailed-choice-comparison
  "For a divergent state, recompute the choices on both bot calls and verify
   they see exactly the same map. Prints any mismatch."
  [definition state]
  (let [[h-phase h-choices] (choice/find-state-raw state)]
    (println "  phase           :" h-phase)
    (println "  (keys c)        :" (pr-str (vec (keys h-choices))))
    (println "  (first (keys c)):" (pr-str (first (keys h-choices))))
    (println "  first val =     :" (= (first (vals h-choices))
                                       (get h-choices (first (keys h-choices)))))
    (println "  duplicate vals? :"
             (let [v (vals h-choices)]
               (- (count v) (count (set v)))))
    (println "  hard ck         :" (pr-str (first (journey/agent-step state))))
    (println "  flow ck         :" (pr-str (first (bf/agent-step definition state))))
    (let [hard-next-s (case h-phase
                        :choose-activate-tower-spend (first (vals h-choices))
                        :choose-activate-matrix-spend (first (vals h-choices))
                        nil)
          rev (when hard-next-s
                (some (fn [[k v]] (when (= v hard-next-s) k)) h-choices))]
      (println "  rev-lookup      :" (pr-str rev)))))

(defn -main [& _]
  (let [conn  (db/connect! mongo-connection)
        saved (bots-db/find-bot conn "OBO")
        defn  (:definition saved)
        _     (println "loaded OBO bot — diagrams:" (mapv first (:diagrams defn)))
        trace (play-with-hardcoded ["A" "B"] 1500)
        _     (println "captured" (count trace) "decision points from hard-coded agent")
        {:keys [total matches divergences]} (compare-choices defn trace)]
    (println)
    (println "=== EQUIVALENCE REPORT ===")
    (println "total decisions :" total)
    (println "matching        :" matches)
    (println "divergences     :" (count divergences))
    (println)
    (when (seq divergences)
      (println "first 20 divergences:")
      (doseq [d (take 20 divergences)]
        (println (format "  phase=%-32s hard=%-30s flow=%-30s state=%s"
                         (str (:phase d))
                         (pr-str (:choice d))
                         (pr-str (:flow-choice d))
                         (pr-str (short-state (:state d)))))
        (println "    choices:" (pr-str (choices-summary (:state d))))
        (detailed-choice-comparison defn (:state d))))
    (System/exit (if (zero? (count divergences)) 0 1))))
