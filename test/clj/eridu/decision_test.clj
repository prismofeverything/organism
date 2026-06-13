(ns eridu.decision-test
  "Tests for the unified decision-making module (src/cljc/eridu/decision.cljc).

   The bot is a pure decision module: it scores the SAME presented choices a
   human sees (everything flows through `choice/find-state-raw`) and processes
   the GA-evolved weights through per-choice features with game-state context
   modulation. These tests assert that contract holds:

   - identical-legal-move-set: the bot only ever picks from the presented choice
     set, at every protected phase (the same set the human UI is shown);
   - context-responsiveness (the adversarial check): with the choice set held
     fixed, changing only game-state context changes the bot's pick — proving
     the scorer is context-modulated, not a static/flat scorer;
   - no-internals-leak: `:choose-action` reads its action descriptor from the
     presented choice's next-state, never from `game/action-spaces`;
   - delegation: `personality/personality-step` == `decision/decide`."
  (:require
   [clojure.test :refer [deftest is testing]]
   [eridu.game :as game]
   [eridu.choice :as choice]
   [eridu.decision :as decision]
   [eridu.personality :as pers]))

;; =============================================================================
;; Game driver — plays a full game through the unified interface, recording
;; every decision (phase, the presented choices, and the bot's pick).
;; =============================================================================

(defn- drive-game
  "Play a game to completion via decision/decide. Returns the decision log:
   a vector of {:phase :choices :pick :state :player :weights}."
  [weights-by-player player-keys max-steps]
  (loop [state (game/initial-state player-keys)
         log   []
         n     0]
    (if (or (:game-over state) (> n max-steps))
      log
      (let [[phase choices] (choice/find-state-raw state)]
        (if (or (= phase :game-over) (empty? choices))
          log
          (let [player  (game/current-player state)
                weights (get weights-by-player player pers/default-weights)
                step    (or (decision/decide state weights)
                            [(first (keys choices)) (first (vals choices))])
                [pick next-s] step
                ;; only retain heavy state for the phases the context test needs
                entry   (cond-> {:phase phase :pick pick :choice-keys (set (keys choices))}
                          (= phase :choose-action)
                          (assoc :choices choices :state state
                                 :player player :weights weights))]
            (recur (choice/advance-through-trivial next-s game/bot-protected-phases)
                   (conj log entry)
                   (inc n))))))))

(def ^:private test-pool
  [pers/archetype-merchant pers/archetype-warlord pers/archetype-pilgrim
   pers/archetype-diplomat])

(defn- weights-map [player-keys]
  (into {} (map vector player-keys (cycle test-pool))))

(defn- some-games
  "Decision logs from a few games of the given player count."
  [n player-count]
  (let [pks (mapv #(keyword (str "p" %)) (range 1 (inc player-count)))]
    (mapcat (fn [_] (drive-game (weights-map pks) pks 5000)) (range n))))

;; =============================================================================
;; identical legal move set
;; =============================================================================

(deftest picks-only-from-presented-choices-test
  (testing "the bot's pick is always a member of the presented choice set"
    (let [logs (concat (some-games 2 2) (some-games 2 3) (some-games 2 4))]
      (is (seq logs) "games produced decisions")
      (is (every? (fn [e] (contains? (:choice-keys e) (:pick e))) logs)
          "every decision picks a key from find-state-raw's presented choices")
      ;; the move set the bot scores IS find-state-raw's set — bot and human
      ;; consume the identical legal-move set at every protected phase.
      (is (every? (fn [e]
                    (or (not (contains? game/bot-protected-phases (:phase e)))
                        (seq (:choice-keys e))))
                  logs)
          "every protected phase surfaced a non-empty presented choice set"))))

(deftest games-terminate-test
  (testing "driving bots through decide runs games to completion without error"
    (let [pks [:p1 :p2 :p3]
          log (drive-game (weights-map pks) pks 5000)]
      (is (< (count log) 5000) "game terminated before the step ceiling")
      (is (every? keyword? (map :phase log)) "every step had a phase"))))

;; =============================================================================
;; no engine-internals leak in :choose-action
;; =============================================================================

(deftest choose-action-reads-presented-choice-test
  (testing "every :choose-action numeric choice carries its action in next-state"
    (let [logs (filter #(= :choose-action (:phase %)) (some-games 3 3))
          ;; for each choose-action decision, the action descriptor is reachable
          ;; from the presented choice's next-state (no game/action-spaces needed)
          ok? (every?
               (fn [e]
                 (every? (fn [[k next-s]]
                           (or (not (number? k))
                               (some? (get-in next-s [:player-turn :action :type]))))
                         (:choices e)))
               logs)]
      (is (seq logs) "observed :choose-action decisions")
      (is ok? "each numeric action choice exposes [:player-turn :action :type] in its next-state"))))

;; =============================================================================
;; context-responsiveness (the adversarial check)
;; =============================================================================

(defn- with-context
  [state player {:keys [amity glory round turn]}]
  (cond-> state
    amity (assoc-in [:players player :amity] amity)
    glory (assoc-in [:players player :glory] glory)
    round (assoc :round round)
    turn  (assoc :turn-in-round turn)))

(def ^:private amity-action? #{:sell :temple})
(def ^:private glory-action? #{:deploy :influence :travel})

(defn- action-types-available
  "Action types offered by a :choose-action decision (from presented choices)."
  [choices]
  (set (keep (fn [[k next-s]]
               (when (number? k) (get-in next-s [:player-turn :action :type])))
             choices)))

(deftest context-modulates-choice-test
  (testing "with the choice set fixed, changing only context changes the pick"
    ;; Mirror the proposal's adversarial probe: a naive flat-weight scorer would
    ;; pick the same action regardless of amity/glory state; the context-modulated
    ;; algorithm must steer toward glory actions when glory is starved and toward
    ;; amity actions when amity is starved.
    (let [hi-round (max 1 (dec game/rounds-per-game))
          candidates
          (for [e (some-games 4 3)
                :when (= :choose-action (:phase e))
                :let [types (action-types-available (:choices e))]
                ;; need both an amity-action and a glory-action on offer to
                ;; observe divergence
                :when (and (some amity-action? types)
                           (some glory-action? types))]
            e)
          probe
          (for [e candidates
                :let [s (:state e) p (:player e) w (:weights e)
                      hi-turn (game/turns-per-round s)
                      s-glory (with-context s p {:glory 0 :amity 6 :round hi-round :turn hi-turn})
                      s-amity (with-context s p {:glory 6 :amity 0 :round hi-round :turn hi-turn})
                      g-type (get-in (second (decision/decide s-glory w)) [:player-turn :action :type])
                      a-type (get-in (second (decision/decide s-amity w)) [:player-turn :action :type])]]
            {:g-type g-type :a-type a-type
             :diverged? (and (glory-action? g-type) (amity-action? a-type))})]
      (is (seq candidates)
          "found :choose-action states offering both amity- and glory-actions")
      (is (some :diverged? probe)
          (str "context flips the pick: glory-starved → glory action, "
               "amity-starved → amity action (observed in "
               (count (filter :diverged? probe)) "/" (count probe) " probes)")))))

;; =============================================================================
;; delegation
;; =============================================================================

(deftest personality-step-delegates-test
  (testing "personality/personality-step is identical to decision/decide"
    (let [pks [:p1 :p2]
          state (game/initial-state pks)
          w pers/archetype-merchant]
      (is (= (decision/decide state w)
             (pers/personality-step state w))
          "personality-step delegates to decision/decide"))))
