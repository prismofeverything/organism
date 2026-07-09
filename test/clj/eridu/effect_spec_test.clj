(ns eridu.effect-spec-test
  "Build-time checks for the bonus-effect prevention scaffold (eridu.effect-spec).

   Makes the four root-pattern gaps from the bonus-walkthrough audit into a
   regression gate over all 175 slots. Strategy: HARD-assert the structural
   invariants (canonical index, coverage, choice/interactivity consistency,
   passive-stub surfacing, and the surprising 'code better than its label'
   direction), and REPORT the large, expected Gap-1 truncation backlog
   informationally. A new structural divergence fails; the backlog is the
   worklist the fix passes burn down."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [eridu.effect-spec :as spec]
   [eridu.bonus :as bonus]
   [eridu.bonus-oracle :as oracle]
   [eridu.cards :as cards]))

(def authored (sort spec/authored-slots))
(def instant (filter (fn [[_ s]] (pos? s)) authored))
(def passive (filter (fn [[_ s]] (zero? s)) authored))

(defn- hand-status [slot] (get bonus/effect-implementation-status slot :unknown))
(defn- status-rank [st] (case st :stub 0 :partial 1 :implemented 2 nil))
(defn- card-text [[b s]] (get-in cards/bonus-boards-by-id [b :effects s]))

;; ─── Gap 4 — canonical index ────────────────────────────────────────────────

(deftest canonical-index-test
  (testing "the universe is exactly 35×5 = 175 slots"
    (is (= 175 (count spec/all-slots))))
  (testing "slot 0 ⇔ passive, slots 1-4 ⇔ instant"
    (doseq [[[_ idx :as slot] {:keys [category]}] spec/effect-specs]
      (is (= (if (zero? idx) :passive :instant) category)
          (str slot " category must follow the canonical index (0=passive)")))))

;; ─── Coverage — full 175, all grounded in canonical text + status map ───────

(deftest full-coverage-test
  (testing "all 175 slots are authored"
    (is (= {:total 175 :authored 175 :remaining 0} (spec/coverage)))
    (is (= spec/all-slots spec/authored-slots))))

(deftest authored-slots-are-grounded-test
  (testing "every slot has canonical printed text in cards.cljc"
    (doseq [slot authored]
      (is (string? (card-text slot)) (str slot " has no printed text"))))
  (testing "every slot has a legacy status entry to reconcile against"
    (doseq [slot authored]
      (is (not= :unknown (hand-status slot)) (str slot " missing from effect-implementation-status")))))

;; ─── Gap 1 & 3 — choice/interactivity consistency ───────────────────────────
;; The human/UI path (bonus-needs-choice?) must agree with the spec's interactive
;; flag. Disagreement = a slot whose auto and choice paths can diverge.

(def known-choice-gaps
  "Frozen instant slots where bonus-needs-choice? disagrees with spec interactivity.
   De-list ONLY when the underlying gap is fixed. Now EMPTY: [17 1] (the former
   Gap-3 poster child — tagged :pick-resource but really a raider placement) was
   fixed (arm places a raider; descriptor de-tagged). Every interactive instant
   slot now agrees with its spec."
  #{})

(deftest choice-path-consistency-test
  (let [live (set (for [slot instant
                        :when (not= (some? (apply bonus/bonus-needs-choice? slot))
                                    (spec/slot-interactive? (spec/effect-specs slot)))]
                    slot))]
    (testing "bonus-needs-choice? agrees with spec interactivity, except frozen gaps"
      (is (= known-choice-gaps live)
          (str "choice/interactivity mismatch set changed.\n"
               "  newly broken (fix, or add to known-choice-gaps): " (sort (set/difference live known-choice-gaps)) "\n"
               "  newly fixed (remove from known-choice-gaps): " (sort (set/difference known-choice-gaps live)))))))

;; ─── Gap 1 — status reconciliation ──────────────────────────────────────────
;; The legacy one-axis map is OPTIMISTIC on many slots (proxies blessed as
;; :implemented) and CONSERVATIVE on a few (faithful code labelled :partial).
;; We hard-assert only the surprising direction — scaffold rates HIGHER than the
;; legacy map — which must be deliberate and flagged :hand-conservative?. The
;; optimistic direction is the Gap-1 backlog, reported below.

(deftest status-reconciliation-test
  (let [conservative (set (for [slot instant
                                :when (> (status-rank (spec/slot-status (spec/effect-specs slot)))
                                         (status-rank (hand-status slot)))]
                            slot))
        flagged (set (for [slot instant
                           :when (:hand-conservative? (spec/effect-specs slot))]
                       slot))]
    (testing "every slot the scaffold rates higher than the legacy map is flagged :hand-conservative?"
      (is (= flagged conservative)
          (str "unflagged 'code better than its label' slots: " (sort (set/difference conservative flagged))
               "\nstale flags (no longer conservative): " (sort (set/difference flagged conservative)))))))

;; ─── Gap 2 — no-op passives surface as :passive-stub, never hide as persistent ─

(def known-passive-stubs
  "Passives whose dispatch arm is a literal no-op. The legacy map calls all of
   these :persistent. De-list when actually implemented.
   [21 0] de-listed in the multi-temple refactor: its 'place an additional
   facedown temple in that city' passive is now genuinely implemented (conj into
   the trigger city), so it surfaces as :persistent, not :passive-stub.
   [5 0] [24 0] [31 0] [33 0] de-listed when their slot-0 passives were
   implemented in apply-passive-dispatch (caravan-follows-magistrate,
   sell-in-surrounded-city, other-astronomer-on-7-bonus-travel, and
   influence-adjacent-magistrate respectively); they now surface as :persistent.
   [10 0] [14 0] [30 0] de-listed when their slot-0 passives were implemented as
   action-choice rules in eridu.choice (sell-gold-to-demand-free-city,
   uruk-bonus-move, and take-from-other-astronomer-location respectively) rather
   than via a post-event dispatch arm; they now surface as :persistent.
   The set is now empty: every authored passive is implemented (none remain a
   literal no-op)."
  #{})

(deftest passive-stubs-surface-test
  (let [stubs (set (for [slot passive
                         :when (= :passive-stub (spec/slot-status (spec/effect-specs slot)))]
                     slot))]
    (testing "no authored passive remains a no-op :passive-stub"
      (is (= known-passive-stubs stubs) (str "passive-stub set changed: " (sort stubs))))
    (testing "the legacy map hid any historical stubs as :persistent (the Gap-2 symptom)"
      (doseq [slot known-passive-stubs]
        (is (= :persistent (hand-status slot)) (str slot))))))

;; ─── Backlog sanity + the [17 1] anchor ─────────────────────────────────────

(deftest backlog-is-actionable-test
  (let [bl (spec/backlog)]
    (testing "the scaffold surfaces a concrete backlog (the remaining numeric-proxy
              approximations) and stays within sane bounds as gaps close"
      (is (<= 1 (count bl) 90) (str "backlog size = " (count bl))))
    (testing "every backlog entry names at least one non-done clause"
      (is (every? #(or (seq (:missing-clauses %)) (seq (:partial-clauses %))
                       (seq (:approximation-clauses %)))
                  bl)))
    (testing "oracle [17 1] expects a raider placement (now matches the fixed code)"
      (is (= 1 (:delta-raiders (get oracle/expectations [17 1])))))))

;; ─── Informational report (prints the Gap-1 worklist; never fails) ──────────

(deftest ^:report scaffold-report-test
  (let [optimistic (sort (for [slot instant
                               :when (< (status-rank (spec/slot-status (spec/effect-specs slot)))
                                        (status-rank (hand-status slot)))]
                           slot))
        approximations (sort (set (for [[slot {:keys [clauses]}] spec/effect-specs
                                        c clauses :when (:approximation? c)]
                                    slot)))]
    (println "\n── effect-spec scaffold report ──")
    (println "coverage:" (spec/coverage))
    (println "Gap-1 backlog (legacy map OPTIMISTIC — code does less than its :implemented/:partial label says):")
    (println "  " (str/join " " (map str optimistic)) "\n  (" (count optimistic) "slots )")
    (println "Numeric-proxy clauses (done but not faithful — sell/influence/demand approximations):")
    (println "  " (str/join " " (map str approximations)) "\n  (" (count approximations) "slots )")
    (is true)))
