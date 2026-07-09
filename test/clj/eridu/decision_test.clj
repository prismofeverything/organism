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

;; =============================================================================
;; New GA traits: neutral-at-default + live-when-nonzero
;;
;; :standing-awareness and :supply-conservation were ADDED to the genome with
;; NEUTRAL (0.0) defaults so the committed archetypes and any saved population
;; evolve unchanged, while the GA can discover their value. These guards pin
;; both halves of that contract:
;;   - neutrality: a bot decides identically whether the keys are present-at-0.0
;;     or absent entirely (proves every new scoring term is exactly 0 at default,
;;     so committing them changes no existing behavior);
;;   - liveness: when a key is raised AND its trigger condition holds, the action
;;     priority actually changes (proves the wiring is live, not dead code).
;; =============================================================================

(def ^:private wap #'decision/weighted-action-priority)
(def ^:private fab #'decision/feat-action-boost)
(def ^:private new-trait-keys
  [:standing-awareness :supply-conservation :feat-race-urgency :temple-engine])

(defn- choose-action-states
  "Collect the (state, weights) at every :choose-action decision point across a
   few real games — fully valid states for exercising weighted-action-priority."
  [games player-count]
  (->> (some-games games player-count)
       (filter #(= :choose-action (:phase %)))
       (filter :state)))

(deftest new-traits-neutral-at-default-test
  ;; The new traits live entirely inside weighted-action-priority, which is a
  ;; PURE function of (weights, state, player, pdata) — no RNG (unlike `decide`,
  ;; whose returned next-state embeds randomly-drawn demand tokens). So we pin
  ;; neutrality where the change actually is: the priority map is byte-identical
  ;; whether the new keys are present-at-0.0 or absent.
  (testing "action priority is invariant to the new keys at 0.0"
    (let [states (choose-action-states 3 4)]
      (is (seq states) "collected some :choose-action decision points")
      (doseq [{:keys [state weights]} states]
        (let [player (game/current-player state)
              pdata (game/player-data state player)
              stripped (apply dissoc weights new-trait-keys)]
          (is (= (wap weights state player pdata)
                 (wap stripped state player pdata))
              "present-at-0.0 == absent"))))))

(deftest new-traits-live-when-nonzero-test
  (let [states (choose-action-states 3 4)
        {:keys [state weights]} (first states)
        player (game/current-player state)]
    (is (seq states))
    (testing "supply-conservation lowers deploy priority when raider stock is low"
      (let [low-state (assoc-in state [:players player :raiders-supply] 1)
            pdata (game/player-data low-state player)]
        (is (< (:deploy (wap (assoc weights :supply-conservation 1.0)
                             low-state player pdata))
               (:deploy (wap weights low-state player pdata))))))
    (testing "standing-awareness changes priority when the field leads on reputation"
      (let [opp (first (filter #(not= % player) (keys (:players state))))
            hi-state (-> state
                         (assoc-in [:players opp :amity] 30)
                         (assoc-in [:players opp :glory] 30))
            pdata (game/player-data hi-state player)]
        (is (not= (wap weights hi-state player pdata)
                  (wap (assoc weights :standing-awareness 1.0)
                       hi-state player pdata)))))
    (testing "temple-engine raises temple priority (values building the base)"
      (let [pdata (game/player-data state player)]
        (is (> (:temple (wap (assoc weights :temple-engine 1.0) state player pdata))
               (:temple (wap (assoc weights :temple-engine 0.0) state player pdata))))))))

;; =============================================================================
;; Feat lookahead (the horizon fix): neutral-at-default + live-when-nonzero
;;
;; :feat-lookahead adds a potential-based forecast (ΔΦ over the planned feat
;; chain) into every RESOLUTION phase of `decide`. Unlike the other new traits
;; it lives in `decide` itself (not weighted-action-priority), so these guards
;; pin the contract at the `decide` level. `decide` is a deterministic function
;; of (state, weights) — find-state-raw is deterministic; RNG only enters when a
;; chosen next-state is ADVANCED — so equality of picks is a sound neutrality
;; probe (cf. personality-step-delegates-test, which asserts decide-equality).
;; =============================================================================

(defn- drive-keep-all-states
  "Like drive-game but retains (state, weights) at EVERY decision point, so the
   feat-lookahead guards can sample resolution phases (temple/deploy/sell/...),
   not just :choose-action."
  [weights-by-player player-keys max-steps]
  (loop [state (game/initial-state player-keys) log [] n 0]
    (if (or (:game-over state) (> n max-steps))
      log
      (let [[phase choices] (choice/find-state-raw state)]
        (if (or (= phase :game-over) (empty? choices))
          log
          (let [player  (game/current-player state)
                weights (get weights-by-player player pers/default-weights)
                step    (or (decision/decide state weights)
                            [(first (keys choices)) (first (vals choices))])
                [_ next-s] step]
            (recur (choice/advance-through-trivial next-s game/bot-protected-phases)
                   (conj log {:phase phase :state state :weights weights})
                   (inc n))))))))

(defn- all-phase-states [games player-count]
  (let [pks (mapv #(keyword (str "p" %)) (range 1 (inc player-count)))]
    (mapcat (fn [_] (drive-keep-all-states (weights-map pks) pks 5000)) (range games))))

(deftest feat-lookahead-neutral-at-default-test
  (testing "at the 0.0 default, feat-lookahead adds exactly nothing — the PICK is
            identical whether the key is present-at-0.0 or absent, at every phase"
    ;; Compare the chosen choice-KEY, not [pick next-s]: find-state-raw rolls fresh
    ;; dice into the returned next-state, so the next-state is RNG-nondeterministic,
    ;; but the pick is a pure function of (state, weights) and is the thing the
    ;; neutrality contract is about.
    (let [states (all-phase-states 4 4)]
      (is (seq states) "collected decision points across all phases")
      (doseq [{:keys [state weights]} states]
        (is (= (first (decision/decide state (assoc weights :feat-lookahead 0.0)))
               (first (decision/decide state (dissoc weights :feat-lookahead))))
            (str "present-at-0.0 == absent at phase "
                 (first (choice/find-state-raw state))))))))

(deftest feat-lookahead-live-test
  ;; Liveness: somewhere in real play, raising feat-lookahead must change a pick
  ;; — proving the ΔΦ wiring actually feeds the scorer (not dead code). It can
  ;; only bite at states where the player has a planned feat chain AND a presented
  ;; resolution differs in realized feat-progress, so we scan many states.
  (testing "raising feat-lookahead changes decide's pick on some real state"
    (let [flipped?
          (some (fn [{:keys [state weights]}]
                  (not= (decision/decide state (assoc weights :feat-lookahead 0.0))
                        (decision/decide state (assoc weights :feat-lookahead 1.5))))
                (all-phase-states 8 4))]
      (is flipped? "feat-lookahead 1.5 diverges from 0.0 on at least one decision"))))

(deftest feat-potential-monotone-test
  ;; The potential Φ must be a genuine gradient: more progress toward a targeted
  ;; feat ⇒ strictly higher potential. Pin it directly on the private helper.
  (testing "feat-potential rises with feat progress"
    (let [fp #'decision/feat-potential
          state (game/initial-state [:p1 :p2])
          player (game/current-player state)
          ;; Pin the plan to a SINGLE feat deterministically: feat-potential reads
          ;; :feat-chain first (initial-state pre-rolls a random one), so we
          ;; overwrite it and clear any pre-placed temples. C1 = four face-up
          ;; temples; progress = face-up count / 4.
          contest {:id :C1}
          base (-> state
                   (assoc-in [:players player :feat-chain] [contest])
                   (assoc-in [:players player :temples] {}))
          pdata0 (game/player-data base player)
          ;; Two face-up temples ⇒ progress 0.5 vs none ⇒ 0.0.
          two-up (assoc-in base [:players player :temples]
                           {:uruk [:face-up] :kish [:face-up]})
          pdata2 (game/player-data two-up player)
          with-target base]
      (is (zero? (fp with-target player pdata0)) "no progress ⇒ Φ = 0")
      (is (pos? (fp two-up player pdata2)) "partial progress ⇒ Φ > 0")
      (is (> (fp two-up player pdata2) (fp with-target player pdata0))
          "Φ is monotone in realized feat progress"))))

(deftest feat-race-urgency-live-test
  ;; Find a real state where the player pursues a contest with positive
  ;; progress, mirror the player's board onto an opponent seat (so the opponent
  ;; now contests the SAME feat with the same progress), and confirm that
  ;; raising feat-race-urgency lifts the feat-action boost. This proves the
  ;; opponent-feat-race wiring is live without depending on a lucky random race.
  (testing "feat-race-urgency boosts feat actions when an opponent contests the same feat"
    (let [hit
          (some
           (fn [{:keys [state weights]}]
             (let [player (game/current-player state)
                   pdata (game/player-data state player)
                   chain (or (seq (:feat-chain pdata)) (:target-feats pdata []))
                   opp (first (filter #(not= % player) (keys (:players state))))
                   ;; the chain contest the player is furthest along on
                   contest (when (seq chain)
                             (apply max-key
                                    #(first (game/feat-progress state player %))
                                    chain))]
               (when (and opp contest
                          (pos? (first (game/feat-progress state player contest))))
                 (let [mstate (assoc-in state [:players opp]
                                        (get-in state [:players player]))
                       mpdata (game/player-data mstate player)
                       b0 (fab (assoc weights :feat-race-urgency 0.0) mstate player mpdata)
                       b1 (fab (assoc weights :feat-race-urgency 1.0) mstate player mpdata)]
                   (when (not= b0 b1) true)))))
           (choose-action-states 6 4))]
      (is hit "raising feat-race-urgency changes the feat boost under contention"))))
