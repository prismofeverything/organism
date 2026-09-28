(ns organism.scripts.rules-attribution
  "Every move the tightened rules take away must be a move a named rule meant to
   take away.

   This is the check that was missing. The cross-engine parity suite proves the
   three implementations agree, and they agreed perfectly while all three withheld
   a legal turn for six days — agreement is not correctness. The rule tests assert
   the behaviour intended by whoever wrote the rule, which is the thing that can
   be wrong. Neither can see a rule quietly doing more than it claims.

   This can, because it has a baseline to compare against. For each position it
   asks the game twice, once as originally written and once with every tightening
   on, and for every difference it asks which single rule accounts for it: turn
   that rule off, leave the others on, and see whether the move comes back. A
   difference no rule claims is a regression, established rather than argued.

   The positions come from walking the ORIGINAL game at random, so the corpus is
   shaped by the game as written rather than by the rules under test — a position
   only reachable because a rule was wrong would be worse than no position at all.

     lein run -m organism.scripts.rules-attribution [--games N] [--steps N] [--verbose]

   Exits non-zero if any difference is unattributed."
  (:require
   [clojure.set :as set]
   [organism.board :as board]
   [organism.choice :as choice]
   [organism.game :as game]))

(def tightenings
  "Each rule, and the value that turns it off. Keyed by the name used in reports
   so an unattributed difference names something a person can go and read."
  [[:require-useful-action    #'game/*require-useful-action*    false]
   [:eat-threshold            #'game/*eat-threshold*            game/*food-limit*]
   [:sacrifice-yields-nothing #'game/*sacrifice-yields-nothing*  false]
   [:stalemate-ends-game      #'game/*stalemate-ends-game*      false]])

(def all-off
  (into {} (map (fn [[_ v off]] [v off])) tightenings))

(defn without
  "Every tightening on except this one."
  [rule]
  (into {} (keep (fn [[name v off]] (when (= name rule) [v off]))) tightenings))

(defn- decision
  "The phase and the choice keys on offer, under these rule bindings."
  [bindings game]
  (with-bindings bindings
    (let [[phase choices] (choice/find-state game)]
      [phase (set (keys choices))])))

(defn- resulting
  "What a given choice leads to, under these rule bindings — the position only,
   since ids and bookkeeping differ without the position differing."
  [bindings game key]
  (with-bindings bindings
    (let [[_ choices] (choice/find-state game)]
      (when-let [next-game (get choices key)]
        (select-keys (:state next-game) [:elements :food :captures :winner])))))

(defn with-organism
  "The game `find-state` actually reported choices for.

   When a player has one organism, find-state chooses it internally and offers
   the action types for it — so the game handed back to us still has no organism
   turn on it, and declaring a type against it does nothing. Missing this made
   the usability search answer \"no\" to everything, which is the worst possible
   failure for a check whose whole job is to say when a rule went too far."
  [game]
  (if (seq (get-in game [:state :player-turn :organism-turns]))
    game
    (let [found (game/find-organisms game)
          organisms (game/player-organisms found (get-in found [:state :player-turn :player]))]
      (when (= 1 (count organisms))
        (game/choose-organism found (first (keys organisms)))))))

(defn performed-type?
  "Whether an action of `wanted` has actually been carried out this turn — a
   completed action of that type that is not a pass."
  [game wanted]
  (boolean
   (some (fn [{action-type :type :keys [action] :as record}]
           (and (= action-type wanted)
                (game/complete-action? record)
                (not (:pass action))))
         (:actions (game/get-organism-turn game)))))

(defn type-usable?
  "Whether declaring `wanted` could actually perform an action of that type.

   Answered by searching the turn in the ORIGINAL game, which is the whole point:
   a rule claiming a type is unusable has to be checked against the game, not
   against the predicate that made the claim. Asking the predicate would be the
   same circle that let a wrong one stand for six days.

   The search has to look past the first move, because a turn is several actions
   and any of them may be a circulate: an organism whose growers are empty may
   circulate food onto one and grow afterwards. That sequence is exactly what the
   broken rule could not see.

   Gives up rather than guessing when the turn tree exceeds `budget` nodes — a
   search that ran out reports nothing, never \"unusable\"."
  [game wanted budget]
  (game/with-original-rules
    (if-let [declared (some-> (with-organism game)
                              (as-> g (try (game/choose-action-type g wanted)
                                           (catch Exception _ nil))))]
      (loop [frontier [declared] seen 0]
        (cond
          (empty? frontier) false
          (> seen budget) false
          (performed-type? (first frontier) wanted) true
          :else
          (let [[phase choices] (choice/find-state (first frontier))]
            (recur (if (or (contains? #{:actions-complete :resolve-conflicts
                                        :check-integrity :player-victory} phase)
                           (empty? choices))
                     (vec (rest frontier))
                     (into (vec (rest frontier)) (vals choices)))
                   (inc seen)))))
      false)))

(defn examine
  "Compare the original game with the tightened one at this position, and
   attribute every difference."
  [game]
  (let [[base-phase base] (decision all-off game)
        [tight-phase tight] (decision {} game)
        removed (set/difference base tight)
        added (set/difference tight base)
        blame (fn [key]
                (first (keep (fn [[name _ _]]
                               (when (contains? (second (decision (without name) game)) key)
                                 name))
                             tightenings)))
        changed (keep (fn [key]
                        (let [b (resulting all-off game key)
                              t (resulting {} game key)]
                          (when (and b t (not= b t))
                            {:key key
                             :rule (first (keep (fn [[name _ _]]
                                                  (when (= b (resulting (without name) game key))
                                                    name))
                                                tightenings))})))
                      (set/intersection base tight))]
    {:phase base-phase
     :phase-shift (when (not= base-phase tight-phase) [base-phase tight-phase])
     ;; A move the original game offered and the tightened one does not.
     ;; Attribution alone is not enough. `require-useful-action` claims only to
     ;; remove types the organism cannot use; a removal it cannot justify is a
     ;; regression even though the rule owns it. That is the shape of the bug
     ;; this harness exists for, and the only check that sees it is a search of
     ;; the base game rather than a second opinion from the same predicate.
     :removed (mapv (fn [key]
                      (let [rule (blame key)]
                        (cond-> {:key key :rule rule}
                          (and (= rule :require-useful-action)
                               (contains? (set choice/element-types) key)
                               (type-usable? game key 400))
                          (assoc :unjustified
                                 "the organism can perform this type within the turn"))))
                    removed)
     ;; The tightened rules should never invent a move.
     :added (vec added)
     ;; Same move, different result — the outcome rules live here.
     :changed (vec changed)}))

(defn walk
  "Positions from one game of the ORIGINAL rules, examined as we go."
  [seed steps]
  (let [random (java.util.Random. seed)
        players ["orb" "mass"]
        starting (board/starting-spaces 4 2 players board/total-rings {})
        info (game/initial-players starting (vec (repeat 2 board/default-player-captures)))
        start (game/create-game (board/player-symmetry 2)
                                (vec (take 4 board/total-rings)) info 3 false {})]
    (loop [g start n 0 found []]
      (let [choices (game/with-original-rules (choice/find-choices g))]
        (if (or (>= n steps) (empty? choices)
                (game/with-original-rules (game/victory? g)))
          found
          (let [report (examine g)
                interesting (or (seq (:removed report)) (seq (:added report))
                                (seq (:changed report)) (:phase-shift report))]
            (recur (nth (vec choices) (.nextInt random (count choices)))
                   (inc n)
                   (cond-> found interesting (conj (assoc report :step n))))))))))

(defn -main
  [& args]
  (let [arg (fn [flag default]
              (if-let [v (second (drop-while #(not= flag %) args))]
                (Integer/parseInt v) default))
        games (arg "--games" 6)
        steps (arg "--steps" 400)
        verbose? (boolean (some #{"--verbose"} args))
        reports (mapcat #(walk % steps) (range games))
        by-rule (frequencies (keep :rule (mapcat :removed reports)))
        unattributed (filter (comp nil? :rule) (mapcat :removed reports))
        invented (filter (comp seq :added) reports)
        outcome (frequencies (keep :rule (mapcat :changed reports)))
        outcome-unattributed (filter (comp nil? :rule) (mapcat :changed reports))]
    (println (format "walked %d games of the original rules, %d steps each" games steps))
    (println (format "%d positions where the tightened rules differ\n" (count reports)))

    (println "moves removed, by the rule that accounts for them:")
    (doseq [[rule n] (sort-by (comp - val) by-rule)]
      (println (format "  %-26s %5d" (name rule) n)))
    (when (seq outcome)
      (println "\nsame move, different outcome:")
      (doseq [[rule n] (sort-by (comp - val) outcome)]
        (println (format "  %-26s %5d" (name rule) n))))

    (when verbose?
      (doseq [r (take 8 reports)]
        (println (format "\n  step %d at %s%s" (:step r) (:phase r)
                         (if-let [s (:phase-shift r)] (str " -> " (second s)) "")))
        (doseq [{:keys [key rule]} (:removed r)]
          (println (format "    removed %-28s by %s" (pr-str key) (or rule "NOTHING"))))))

    (let [unjustified (filter :unjustified (mapcat :removed reports))
          problems (concat unattributed outcome-unattributed unjustified
                           (mapcat :added invented))]
      (when (seq unjustified)
        (println (format "\n%d removal(s) a rule owns but cannot justify:" (count unjustified)))
        (doseq [u (take 6 unjustified)]
          (println (format "    %s removed by %s — %s"
                           (pr-str (:key u)) (name (:rule u)) (:unjustified u)))))
      (if (seq problems)
        (do
          (println (format "\nFAILED: %d difference(s) no rule accounts for" (count problems)))
          (doseq [p (take 10 problems)] (println "   " (pr-str p)))
          (println "\nA move the original game allows, that no named rule removes, is a")
          (println "regression. A move a rule removes but cannot justify is the same")
          (println "thing wearing a name. A move the tightened rules invent is worse.")
          (System/exit 1))
        (do (println "\nevery difference is accounted for by a named rule")
            (System/exit 0))))))
