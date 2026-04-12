(ns organism.routes.organism-bot
  "Bot AI for organism. Picks a choice key from the current choice map
   using heuristics:
   - Variety: try to rotate through action types
   - Center-seeking: prefer positions closer to [0 0]
   - Capture: prefer actions that land next to enemy elements
   - Grow organisms: prefer missing element types"
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [organism.board :as board]
   [organism.bot-flow :as bot-flow]
   [organism.bots :as bots]
   [organism.choice :as choice]
   [organism.game :as game]
   [organism.persist-journey-bots :as bots-db]))

;; ── Helpers ──────────────────────────────────────────────────────────────

(defn- space-distance-to-center
  "Distance from a hex space [ring step] to center [:0 0]. Smaller = closer."
  [[ring _step]]
  (cond
    (= 0 ring) 0
    (nil? ring) 99
    :else
    ;; ring is a keyword like :A, :B, :C... use total-rings index
    (let [idx (.indexOf board/total-rings ring)]
      (if (neg? idx) 99 (inc idx)))))

(defn- enemy-adjacent?
  "True if the given space has an adjacent enemy element."
  [game space player]
  (let [elements (get-in game [:state :elements])
        adjacencies (get (:adjacencies game) space #{})]
    (some
     (fn [adj]
       (let [el (get elements adj)]
         (and el (not= (:player el) player))))
     adjacencies)))

(defn- friendly-adjacent-count
  "Count adjacent elements belonging to player at space (excluding `exclude` space)."
  [game space player exclude]
  (let [elements (get-in game [:state :elements])
        adjacencies (get (:adjacencies game) space #{})]
    (count
     (filter
      (fn [adj]
        (and (not= adj exclude)
             (let [el (get elements adj)]
               (and el (= (:player el) player)))))
      adjacencies))))

;; Heterarchy: eat beats grow, grow beats move, move beats eat
(def ^:private beats {:eat :grow, :grow :move, :move :eat})

(defn- captures?
  "True if an element of attacker-type would capture a defender-type."
  [attacker-type defender-type]
  (= defender-type (get beats attacker-type)))

(defn- favorable-capture-at?
  "If the player has an element of this type adjacent to `space`, and that type
   beats the element at space, return the attacker's space. Otherwise nil."
  [game space player]
  (let [elements (get-in game [:state :elements])
        defender (get elements space)
        adjacencies (get (:adjacencies game) space #{})]
    (when (and defender (not= (:player defender) player))
      (some
       (fn [adj]
         (let [attacker (get elements adj)]
           (when (and attacker
                      (= (:player attacker) player)
                      (captures? (:type attacker) (:type defender)))
             adj)))
       adjacencies))))

(defn- has-favorable-capture-opportunity?
  "True if any enemy element adjacent to one of our elements would be captured
   in a favorable heterarchy matchup."
  [game player]
  (let [elements (get-in game [:state :elements])
        player-els (filter #(= player (:player (val %))) elements)]
    (some
     (fn [[space attacker]]
       (let [adjacencies (get (:adjacencies game) space #{})]
         (some
          (fn [adj]
            (let [defender (get elements adj)]
              (and defender
                   (not= (:player defender) player)
                   (captures? (:type attacker) (:type defender)))))
          adjacencies)))
     player-els)))

(defn- nearest-enemy-distance
  "Approximate distance from player's elements to the nearest enemy element.
   Uses hex-distance over space coordinates. Returns 99 if no enemies."
  [game player]
  (let [elements (get-in game [:state :elements])
        player-spaces (keep (fn [[s e]] (when (= player (:player e)) s)) elements)
        enemy-spaces  (keep (fn [[s e]] (when (not= player (:player e)) s)) elements)
        ;; Simple heuristic: count ring differences for any pair
        ring-idx (fn [[r _]]
                   (let [i (.indexOf board/total-rings r)]
                     (if (neg? i) 99 i)))]
    (if (or (empty? player-spaces) (empty? enemy-spaces))
      99
      (apply min
             (for [p player-spaces
                   e enemy-spaces]
               (Math/abs ^long (- (ring-idx p) (ring-idx e))))))))

(defn- count-player-types
  "Return {:eat n :grow n :move n} for the current organism's elements."
  [game]
  (let [elements (game/current-organism-elements game)
        by-type  (group-by :type elements)]
    {:eat  (count (get by-type :eat []))
     :grow (count (get by-type :grow []))
     :move (count (get by-type :move []))}))

(defn- organism-count
  "Count of living organisms the player has."
  [game player]
  (count (keys (game/player-organisms game player))))

;; ── Choice scoring ────────────────────────────────────────────────────────

(defn- action-feasible?
  "Check if an action type is actually feasible right now."
  [state-game action-type]
  (try
    (let [f (get choice/action-filters action-type)]
      (boolean (and f (f state-game))))
    (catch Exception _ false)))

(defn- score-choice-key
  "Score a choice key for the given phase. Higher = better.
   `next-game` is the game state resulting from this choice (has current organism set)."
  [phase choice-key game player next-game]
  (case phase
    :introduce
    0

    :choose-organism
    0

    :choose-action-type
    (if-not (action-feasible? next-game choice-key)
      -1000
      (let [elements (try (game/current-organism-elements next-game) (catch Exception _ []))
            by-type (group-by :type elements)
            grower-food (reduce + 0 (map #(get % :food 0) (get by-type :grow [])))
            eater-food  (reduce + 0 (map #(get % :food 0) (get by-type :eat [])))
            total-food  (+ grower-food eater-food
                           (reduce + 0 (map #(get % :food 0) (get by-type :move []))))
            grow-feasible? (action-feasible? next-game :grow)
            has-capture? (has-favorable-capture-opportunity? game player)]
        (cond
          ;; GROW — always strong when feasible
          (= choice-key :grow)
          (if grow-feasible?
            (if has-capture? 180 200)
            -1000)

          ;; MOVE — massively promoted when capture is reachable, and when
          ;; grow is blocked (we need to reposition to keep the game going).
          (= choice-key :move)
          (cond
            has-capture?     250  ;; go capture enemies!
            grow-feasible?   5    ;; grow instead
            ;; Grow is blocked: move is now the only way to make progress.
            ;; Only eat more if we're actually starving.
            (> total-food 2) 140
            :else            40)

          ;; CIRCULATE as action-type is mostly a fallback
          (= choice-key :circulate)
          (cond
            (and (zero? grower-food) (pos? eater-food)) 150
            :else 20)

          ;; EAT — only when hungry or grow needs food routing
          (= choice-key :eat)
          (cond
            has-capture? 15
            ;; Grow blocked and we already have plenty of food — stop eating!
            (and (not grow-feasible?) (> total-food 3)) 10
            ;; Need eater food to later feed growers
            (and (not grow-feasible?) (zero? eater-food)) 100
            :else 30)

          :else 0)))

    :choose-action
    ;; At :choose-action, the options are the picked action-type (e.g. :eat)
    ;; AND :circulate (as an alternative) AND :pass. Pick circulate when
    ;; growers are starving but other elements have food.
    (let [elements (try (game/current-organism-elements game) (catch Exception _ []))
          by-type (group-by :type elements)
          grower-food (reduce + 0 (map #(get % :food 0) (get by-type :grow [])))
          eater-food  (reduce + 0 (map #(get % :food 0) (get by-type :eat [])))
          need-circulate? (and (zero? grower-food) (pos? eater-food))]
      (cond
        (= choice-key :circulate)
        (if need-circulate? 200 30)
        (= choice-key :pass) -1000
        :else 50))

    ;; Move-to: requires connectivity AND prefer landing next to an enemy
    ;; we can capture via heterarchy.
    :move-to
    (if (and (vector? choice-key) (= 2 (count choice-key)))
      (let [from-space (try (game/get-action-field game :from) (catch Exception _ nil))
            from-el (when from-space (get-in game [:state :elements from-space]))
            attacker-type (:type from-el)
            n-friends (friendly-adjacent-count game choice-key player from-space)
            connected? (pos? n-friends)
            ;; Check if landing here puts us adjacent to an enemy we beat
            elements (get-in game [:state :elements])
            adjacencies (get (:adjacencies game) choice-key #{})
            capture-bonus
            (if (and attacker-type (seq adjacencies))
              (->> adjacencies
                   (keep
                    (fn [adj]
                      (let [def-el (get elements adj)]
                        (when (and def-el
                                   (not= (:player def-el) player)
                                   (captures? attacker-type (:type def-el)))
                          200))))
                   (reduce + 0))
              0)
            dist-score (- 20 (space-distance-to-center choice-key))
            enemy-bonus (if (enemy-adjacent? game choice-key player) 30 0)]
        (if connected?
          (+ capture-bonus dist-score enemy-bonus (* 5 n-friends))
          -1000))
      0)

    ;; Circulate-to: prefer sending food to growers with empty food
    :circulate-to
    (if (and (vector? choice-key) (= 2 (count choice-key)))
      (let [el (get-in game [:state :elements choice-key])
            grower? (= :grow (:type el))
            empty?  (zero? (or (:food el) 0))
            dist-score (- 20 (space-distance-to-center choice-key))]
        (cond
          (and grower? empty?) (+ 100 dist-score)
          grower?              (+ 50 dist-score)
          :else                dist-score))
      0)

    ;; Circulate-from: prefer taking food from eaters with the most food
    :circulate-from
    (if (and (vector? choice-key) (= 2 (count choice-key)))
      (let [el (get-in game [:state :elements choice-key])
            eater? (= :eat (:type el))
            food   (or (:food el) 0)]
        (if eater? (+ 50 food) food))
      0)

    ;; Grow-to: prefer placing a new element where it borders an enemy
    ;; (to set up captures) or toward the center.
    :grow-to
    (if (and (vector? choice-key) (= 2 (count choice-key)))
      (let [dist-score (- 20 (space-distance-to-center choice-key))
            enemy-bonus (if (enemy-adjacent? game choice-key player) 60 0)]
        (+ dist-score enemy-bonus))
      0)

    ;; Other position-based choices
    (:eat-to :eat-from)
    (if (and (vector? choice-key) (= 2 (count choice-key)))
      (let [dist-score (- 20 (space-distance-to-center choice-key))
            enemy-bonus (if (enemy-adjacent? game choice-key player) 30 0)]
        (+ dist-score enemy-bonus))
      0)

    :grow-element
    ;; Prefer types missing from the organism
    (let [types (count-player-types game)
          cnt (get types choice-key 0)]
      (cond
        (zero? cnt) 50
        (= cnt 1) 20
        :else 5))

    :grow-from
    0

    ;; Move-from: prefer an element whose type can capture an adjacent enemy
    ;; (or reach one via a short move)
    :move-from
    (if (and (vector? choice-key) (= 2 (count choice-key)))
      (let [el (get-in game [:state :elements choice-key])
            attacker-type (:type el)
            elements (get-in game [:state :elements])
            ;; Does this element already have a favorable target within 2 hops?
            adj-of-adj
            (->> (get (:adjacencies game) choice-key #{})
                 (mapcat #(get (:adjacencies game) % #{}))
                 distinct)
            capture-bonus
            (if attacker-type
              (->> adj-of-adj
                   (filter
                    (fn [sp]
                      (let [def-el (get elements sp)]
                        (and def-el
                             (not= (:player def-el) player)
                             (captures? attacker-type (:type def-el))))))
                   count
                   (* 100))
              0)]
        (+ capture-bonus (- 10 (space-distance-to-center choice-key))))
      0)

    ;; Default: no preference
    0))

(defn- pick-choice
  "Given a choices map, pick the best key by heuristic score.
   Adds a small random jitter so equal/near-equal scores tie-break randomly,
   giving each generated game some non-determinism."
  [phase choices game player]
  (when (seq choices)
    (let [scored (map (fn [[k v]]
                        (let [s (score-choice-key phase k game player v)
                              jitter (rand 10)]
                          [(+ s jitter) k v]))
                      choices)
          sorted (sort-by (comp - first) scored)
          [_ k v] (first sorted)]
      [k v])))

;; ── Main step function ───────────────────────────────────────────────────

(defn agent-step+key
  "Advance the game by one bot decision. Returns [choice-key next-game]
   so callers can record the chosen key for event-sourced replay."
  [game]
  (try
    (let [[phase choices] (choice/find-state game)
          player (get-in game [:state :player-turn :player])]
      (cond
        (empty? choices)
        (do (println "BOT: no choices at phase" phase "player" player) nil)

        (contains? choices :advance)
        [:advance (:advance choices)]

        (and (contains? choices :pass) (= 1 (count choices)))
        [:pass (:pass choices)]

        :else
        (let [real-choices (dissoc choices :pass)
              candidates   (if (seq real-choices) real-choices choices)
              picked       (pick-choice phase candidates game player)]
          picked)))
    (catch Exception e
      (println "BOT ERROR at phase"
               (try (first (choice/find-state game))
                    (catch Exception _ :unknown))
               ":" (.getMessage e))
      (.printStackTrace e)
      nil)))

(defn agent-step
  "Advance the game by one bot decision. Returns the next game state."
  [game]
  (when-let [[_k next-game] (agent-step+key game)]
    next-game))

;; ── DB-aware agent step (tries flowchart bots first) ────────────────────────

(defn- resolve-flowchart-step
  "Look up a saved flowchart bot for the current player. Returns an
   agent-step+key fn or nil."
  [db player-name]
  (let [base (str/replace (or player-name "") #"-(?:[A-Z]|\d+)$" "")]
    (or (when-let [saved (bots-db/find-bot db "organism" player-name)]
          (when-let [d (:definition saved)]
            (fn [game] (bot-flow/agent-step d game))))
        (when (not= base player-name)
          (when-let [saved (bots-db/find-bot db "organism" base)]
            (when-let [d (:definition saved)]
              (fn [game] (bot-flow/agent-step d game))))))))

(defn make-agent-step+key
  "Return an agent-step+key fn that checks the DB for a flowchart bot
   matching the current player, falling back to the hard-coded heuristic."
  [db]
  (let [cache (atom {})]  ;; {player-name → step-fn or ::none}
    (fn [game]
      (let [player (get-in game [:state :player-turn :player])
            cached (get @cache player ::miss)]
        (if (not= cached ::miss)
          (if (= cached ::none)
            (agent-step+key game)
            (cached game))
          (let [flow-fn (resolve-flowchart-step db player)]
            (swap! cache assoc player (or flow-fn ::none))
            (if flow-fn
              (flow-fn game)
              (agent-step+key game))))))))

;; ── Organism bot loop config (uses shared bots/run-bot-loop!) ──────────────

(defn- organism-turn-changed?
  "Turn boundary for organism: player change OR round change."
  [a b]
  (or (not= (get-in a [:state :player-turn :player])
            (get-in b [:state :player-turn :player]))
      (not= (get-in a [:state :round])
            (get-in b [:state :round]))))

(defn- organism-bot-config
  [games-atom game-key humans delay-ms on-turn & [db]]
  {:label          (str "ORGANISM BOT " game-key)
   :get-game       (fn [] (:game (get-in @games-atom [:games game-key])))
   :put-game!      (fn [next-game]
                     (swap! games-atom assoc-in [:games game-key :game] next-game))
   :agent-step+key (if db (make-agent-step+key db) agent-step+key)
   :victory?       game/victory?
   :current-player game/current-player
   :turn-changed?  organism-turn-changed?
   :on-turn        on-turn
   :humans         humans
   :delay-ms       delay-ms})

(defn run-bot-turns!
  "All-bot game runner — used by /organism/generate. No humans."
  [games-atom game-key delay-ms broadcast-fn persist-fn & [db]]
  (bots/run-bot-loop!
   (organism-bot-config
    games-atom game-key #{} delay-ms
    (fn [choice-keys next-game]
      (when broadcast-fn (broadcast-fn choice-keys next-game))
      (when persist-fn (persist-fn (:state next-game))))
    db)))

(defn play-until-human-or-done
  "Run bot steps until it's a human's turn or the game is over.
   Returns [final-game history] where history is a vector of game states
   snapshotted at each turn boundary."
  [initial-game humans]
  (println "BOT starting, player:"
           (get-in initial-game [:state :player-turn :player]))
  (loop [game initial-game
         history [(:state initial-game)]
         prev-player (get-in initial-game [:state :player-turn :player])
         prev-round (get-in initial-game [:state :round])
         n 0]
    (let [player (get-in game [:state :player-turn :player])
          round (get-in game [:state :round])
          winner (game/victory? game)
          turn-changed? (or (not= prev-player player) (not= prev-round round))
          new-history (if turn-changed? (conj history (:state game)) history)]
      (cond
        winner
        (do (println "BOT: winner" winner "after" n "steps, history"
                     (count new-history) "round" round)
            [game new-history])

        (contains? humans player)
        [game new-history]

        (> n 3000)
        (do (println "BOT: step cap reached at round" round
                     "elements" (count (get-in game [:state :elements]))
                     "history" (count new-history))
            [game new-history])

        :else
        (if-let [next-game (agent-step game)]
          (recur next-game new-history player round (inc n))
          (do (println "BOT: no progress at step" n "round" round)
              [game new-history]))))))

(defn run-bot-until-human!
  "Mixed game runner — runs bots until a human's turn comes up."
  [games-atom game-key human-players delay-ms broadcast-fn persist-fn & [db]]
  (bots/run-bot-loop!
   (organism-bot-config
    games-atom game-key human-players delay-ms
    (fn [choice-keys next-game]
      (when broadcast-fn (broadcast-fn choice-keys next-game))
      (when persist-fn (persist-fn (:state next-game))))
    db)))

;; ── Bot registration ────────────────────────────────────────────────────────

(bots/register-bot!
 "organism" "OBO"
 {:agent-step  agent-step
  :description "Heuristic organism bot — grows aggressively, captures opportunistically, and circulates food to feed growers."})
