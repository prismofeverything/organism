(ns eridu.effect-spec
  "Structural effect schema for the 175 bonus-board slots — the *prevention
   scaffold* distilled from the bonus-walkthrough audit. Pure data + lookups,
   no game-state dependency (sibling of eridu.bonus / eridu.cards).

   WHY THIS EXISTS
   ---------------
   The walkthrough found that 58 of 175 slots were non-clean, clustering into
   four root patterns that are *traceability* failures, not coding mistakes:

     Gap 1  Compound-effect truncation — printed 'do X then Y', code does only X.
            The dropped clause is almost always the interactive tail (sell /
            influence / travel / place).
     Gap 2  'Persistent' hid un-built work — a no-op passive looked identical to
            a working one because one enum carried both 'what kind' and 'is it done'.
     Gap 3  Dual-path divergence — the auto (bot) path and the human (choice) path
            were authored separately and drifted (e.g. [17 1]: auto flips a raider,
            human grants a resource, rule says place a raider).
     Gap 4  Coordinate mismatch — 'slot 5' (player) vs code index 4.

   The fix for all four is the same: make the printed effect a *machine-checkable
   structured spec*, not a prose blob. This namespace is that spec for Eridu.

   THE SCHEMA
   ----------
   `effect-specs` maps [board-id slot-idx] → {:category :clauses ...}.

   :category    :passive (slot 0)  |  :instant (slots 1-4)        ← Gap 2: separate
                                                                     'kind' from 'state'
   :clauses     ordered vector of typed clauses; order = printed order. Each:
       :kind          a verb keyword from the clause taxonomy below
       :interactive?  true if the clause needs a player decision (Gap 1's tail)
       :state         :done | :partial | :stub                    ← per-clause, so
                      'is every clause implemented?' is a lookup, not an audit
       :approximation? (optional) true = done, but via a numeric proxy that is
                      NOT faithful to the printed text (e.g. sell → flat +N amity).
                      Counts as :done for status, but flagged for faithfulness review.
       :note          (optional) free text — what the code actually does / why a gap.
   :hand-conservative? (optional) true = the legacy effect-implementation-status map
                      rates this slot LOWER than the code actually deserves (the
                      opposite of truncation; flagged for re-blessing, not as a bug).

   CLAUSE TAXONOMY (the verbs that appear on Eridu bonus cards)
     :travel :travel-anywhere :sell :deploy :place-raider :place-temple
     :increase-role :increase-role-free :decrease-role
     :gain-resource :gain-resource-choice
     :score-amity :score-glory
     :influence-magistrate :move-magistrate :influence-follow-travel
     :place-demand :flip-raider :flip-temple
     :grant-free-action :grant-extra-action :modify-scoring :bonus-move-action
   (Reuse an existing keyword before coining a new one; the set is meant to stay small.)

   CANONICAL INDEX (Gap 4) — decided once, here:
     slot-idx is 0-based. Slot 0 is ALWAYS the persistent passive. Slots 1-4 are the
     instant bonus slots. Player-facing 'slot N' = code index N-1. Any UI/report layer
     that speaks 1-based numbering must translate at its boundary, never internally.

   COVERAGE: authored incrementally. `coverage` reports authored-vs-175. The
   eridu.effect-spec-test suite enforces the cross-source invariants over the
   authored slots and freezes known mismatches, so a NEW divergence fails loudly
   and a RESOLVED one (e.g. when the Gap-3 [17 1] fix lands) must be de-listed."
  (:require [clojure.set :as set]))

;; ─────────────────────────────────────────────────────────────────────────────
;; The spec data — all 175 slots, authored from the live dispatch arms in
;; eridu.game (apply-bonus-dispatch / apply-bonus-with-choice / apply-passive-
;; dispatch / apply-passive-choice) and the canonical text in eridu.cards.
;;
;; Flags used below:
;;   :hand-conservative? — legacy effect-implementation-status rates this LOWER
;;                         than the code earns (re-bless candidate, not a bug).
;;   :hand-optimistic?   — legacy map rates this HIGHER than the code earns
;;                         (the Gap-1 truncation hidden behind a label).
;;   :dual-path?         — HISTORICAL marker: this slot's auto (bot) and human
;;                         (choice) paths USED to diverge (the Gap-3 class). The
;;                         d1b10d8 unification collapsed both into one choice-aware
;;                         apply-bonus-dispatch, so the divergence no longer exists;
;;                         the flag is kept to mark which slots carried it. Any clause
;;                         :note here still phrased as "auto vs human" describes that
;;                         pre-unification history, not current behavior.
;; ─────────────────────────────────────────────────────────────────────────────

(def effect-specs
  {;; ══ Board 1 — Shield of Gilgamesh (clean) ══════════════════════════════
   [1 0] {:category :passive :trigger :surround-city
          :clauses [{:kind :place-temple :state :done :note "temple in surrounded city"}]}
   [1 1] {:category :instant :clauses [{:kind :travel :target :kish :state :done}]}
   [1 2] {:category :instant :clauses [{:kind :increase-role :target :raider :state :done}
                                       {:kind :increase-role :target :leader :state :done}]}
   [1 3] {:category :instant :clauses [{:kind :place-raider :target :lagash :count 2 :state :done}]}
   [1 4] {:category :instant :clauses [{:kind :score-glory :basis :demands-fulfilled :state :done}]}

   ;; ══ Board 2 — Seal of Enmerkar (clean) ═════════════════════════════════
   [2 0] {:category :passive :trigger :raider-scored
          :clauses [{:kind :increase-role :target :priest :state :done}]}
   [2 1] {:category :instant :clauses [{:kind :increase-role :target :merchant :state :done}
                                       {:kind :increase-role :target :raider :state :done}]}
   [2 2] {:category :instant :clauses [{:kind :score-amity :amount 5 :condition :at-magistrate :state :done}]}
   [2 3] {:category :instant :clauses [{:kind :place-temple :target :magistrate-city :interactive? true :state :done}]}
   [2 4] {:category :instant :clauses [{:kind :score-glory :basis :facedown-temples :state :done}]}

   ;; ══ Board 3 — Voyage of Ziusudra ═══════════════════════════════════════
   [3 0] {:category :passive :trigger :river-crossed
          :clauses [{:kind :gain-resource :resource :gems :state :done
                     :note "endgame 'gems worth amity' is a scoring-time rule, handled at scoring"}]}
   [3 1] {:category :instant :clauses [{:kind :increase-role-free :target :leader :state :done}]}
   [3 2] {:category :instant :clauses [{:kind :place-temple :target :lagash :state :done}]}
   [3 3] {:category :instant :clauses [{:kind :place-raider :target :eridu :state :done}
                                       {:kind :gain-resource-choice :interactive? true :state :done
                                        :note "auto path grants fixed :tools instead of choice"}]}
   [3 4] {:category :instant :dual-path? true
          :clauses [{:kind :travel :interactive? true :state :done}
                    {:kind :sell :state :partial :approximation? true
                     :note "auto path = travel :eridu +2 amity proxy; human path does a real sell"}]}

   ;; ══ Board 4 — Blessing of Inanna (clean) ═══════════════════════════════
   [4 0] {:category :passive :trigger :temple-flipped
          :clauses [{:kind :sell :optional? true :state :done :note "sell in flipped city if demand matches"}]}
   [4 1] {:category :instant :clauses [{:kind :place-temple :target :eridu :state :done}]}
   [4 2] {:category :instant :clauses [{:kind :gain-resource :resource :tools :state :done}
                                       {:kind :gain-resource :resource :gems :state :done}
                                       {:kind :gain-resource :resource :gold :state :done}]}
   [4 3] {:category :instant :clauses [{:kind :score-amity :basis :leader-x2 :state :done}]}
   [4 4] {:category :instant :clauses [{:kind :score-amity :basis :raiders :state :done}]}

   ;; ══ Board 5 — Wisdom of Adapa (slot 0 STUB) ════════════════════════════
   [5 0] {:category :passive :trigger :magistrate-influenced
          :clauses [{:kind :influence-follow-travel :state :done
                     :note "fires :magistrate-moved from resolve-influence-choices; caravan follows the magistrate out of its origin city"}]}
   [5 1] {:category :instant :clauses [{:kind :increase-role-free :target :priest :state :done}]}
   [5 2] {:category :instant :clauses [{:kind :place-demand :target :uruk :count 2 :state :done}
                                       {:kind :gain-resource :basis :matching-demands :state :done}]}
   [5 3] {:category :instant :clauses [{:kind :deploy :state :done}
                                       {:kind :place-temple :state :done}]}
   [5 4] {:category :instant :clauses [{:kind :score-amity :basis :raiders :state :done}]}

   ;; ══ Board 6 — Trade of Dumuzid ═════════════════════════════════════════
   [6 0] {:category :passive :trigger :action-space-7
          :clauses [{:kind :grant-free-action :action :travel :state :done :note "sets :pending-free-travel"}]}
   [6 1] {:category :instant :clauses [{:kind :increase-role :target :merchant :state :done}
                                       {:kind :increase-role :target :priest :state :done}]}
   [6 2] {:category :instant :clauses [{:kind :place-temple :target :each-magistrate-city :state :done}]}
   [6 3] {:category :instant :hand-optimistic? true
          :clauses [{:kind :sell :target :babylon :modifier :double :state :partial :approximation? true
                     :note "flat +4 amity proxy; ALSO wrongly moves caravan to :babylon though card says 'you don't need to be there'"}]}
   [6 4] {:category :instant
          :clauses [{:kind :place-raider :target :lagash :state :stub :note "raider NOT placed — silent truncation"}
                    {:kind :gain-resource :resource :tools :count 2 :state :done}]}

   ;; ══ Board 7 — March of Lugalbanda (clean) ══════════════════════════════
   [7 0] {:category :passive :trigger :deployed
          :clauses [{:kind :place-raider :target :near-magistrate :state :done}]}
   [7 1] {:category :instant :clauses [{:kind :increase-role :target :merchant :state :done}
                                       {:kind :increase-role :target :leader :state :done}]}
   [7 2] {:category :instant :clauses [{:kind :place-temple :target :magistrate-city :interactive? true :state :done}]}
   [7 3] {:category :instant :clauses [{:kind :travel :interactive? true :state :done}
                                       {:kind :score-glory :amount 3 :condition :at-eridu :state :done
                                        :note "auto travels to Eridu first to satisfy condition"}]}
   [7 4] {:category :instant :clauses [{:kind :travel :interactive? true :state :done}
                                       {:kind :score-amity :amount 3 :condition :at-kish :state :done}]}

   ;; ══ Board 8 — Fury of Enkidu ═══════════════════════════════════════════
   [8 0] {:category :passive :trigger :raider-scored
          :clauses [{:kind :modify-scoring :state :done :note "sets :keep-scored-raider"}]}
   [8 1] {:category :instant :clauses [{:kind :increase-role :target :raider :state :done}
                                       {:kind :increase-role :target :priest :state :done}]}
   [8 2] {:category :instant :hand-optimistic? true
          :clauses [{:kind :place-demand :target :nippur :state :stub}
                    {:kind :place-demand :target :babylon :state :stub}
                    {:kind :sell :optional? true :state :partial :approximation? true
                     :note "whole effect proxied as flat +3 amity; demands never placed"}]}
   [8 3] {:category :instant
          :clauses [{:kind :gain-resource :resource :gold :state :done}
                    {:kind :gain-resource :resource :gems :state :done}
                    {:kind :gain-resource :resource :pottery :state :done}
                    {:kind :sell :optional? true :state :stub :note "'then you may sell' dropped"}]}
   [8 4] {:category :instant :clauses [{:kind :flip-raider :to :point :scope :all :state :done}]}

   ;; ══ Board 9 — Rites of Ninhursag ═══════════════════════════════════════
   [9 0] {:category :passive :trigger :temple-flipped
          :clauses [{:kind :increase-role :interactive? true :state :done :note "deferred :pick-role"}]}
   [9 1] {:category :instant :clauses [{:kind :gain-resource :resource :tools :state :done}
                                       {:kind :gain-resource :resource :gold :state :done}
                                       {:kind :gain-resource :resource :pottery :state :done}
                                       {:kind :score-amity :basis :leader-level :state :done}]}
   [9 2] {:category :instant :clauses [{:kind :increase-role :target :priest :state :done}
                                       {:kind :increase-role :target :leader :state :done}]}
   [9 3] {:category :instant :clauses [{:kind :place-raider :target :each-river :state :done}]}
   [9 4] {:category :instant :dual-path? true
          :clauses [{:kind :sell :target :magistrate-city :interactive? true :state :partial :approximation? true
                     :note "auto = travel + flat +2 amity proxy; human = real auto-sell-in"}
                    {:kind :place-temple :optional? true :state :partial
                     :note "auto places temple; human path drops the temple step"}]}

   ;; ══ Board 10 — Wealth of Meskalamdug (slot 0 STUB) ═════════════════════
   [10 0] {:category :passive :trigger :sold
           :clauses [{:kind :sell :state :done
                      :note "action-choice rule: :sell-gold-empty option in resolve-sell-choices (sell gold to demand-free city, place a random demand token)"}]}
   [10 1] {:category :instant :clauses [{:kind :increase-role-free :target :merchant :state :done}]}
   [10 2] {:category :instant :clauses [{:kind :increase-role-free :target :merchant :state :done}]}
   [10 3] {:category :instant
           :clauses [{:kind :place-raider :target :near-magistrate :state :done}
                     {:kind :score-amity :basis :leader-level :state :partial :approximation? true
                      :note "CODE BUG (audit): card = amity by LEADER LEVEL; code grants flat +2"}]}
   [10 4] {:category :instant :clauses [{:kind :place-temple :target :nippur :state :done}]}

   ;; ══ Board 11 — Ambition of Sargon ══════════════════════════════════════
   [11 0] {:category :passive :trigger :feat-claimed
           :clauses [{:kind :score-glory :basis :leader-level :state :done}]}
   [11 1] {:category :instant :hand-optimistic? true
           :clauses [{:kind :place-demand :target :lagash :count 2 :state :partial :approximation? true
                      :note "proxied as travel :lagash + fixed +1 gold +1 pottery; no demand tokens placed"}]}
   [11 2] {:category :instant :hand-optimistic? true
           :clauses [{:kind :sell :target :lagash :modifier :double-glory :state :partial :approximation? true
                      :note "flat +4 glory proxy + caravan move"}]}
   [11 3] {:category :instant :clauses [{:kind :increase-role-free :target :raider :state :done}]}
   [11 4] {:category :instant :clauses [{:kind :score-glory :basis :facedown-temples :state :done}]}

   ;; ══ Board 12 — Currents of Enki ════════════════════════════════════════
   [12 0] {:category :passive :trigger :river-crossed
           :clauses [{:kind :place-raider :target :that-river :state :done}]}
   [12 1] {:category :instant :clauses [{:kind :increase-role :basis :all-level-1 :state :done}]}
   [12 2] {:category :instant :clauses [{:kind :gain-resource :resource :gold :count 3 :state :done}
                                        {:kind :gain-resource :resource :gems :state :done}]}
   [12 3] {:category :instant :hand-optimistic? true
           :clauses [{:kind :increase-role :target :merchant :state :done}
                     {:kind :sell :yields :glory :state :partial :approximation? true
                      :note "sell-for-glory proxied as flat +3 glory; sells CURRENT city, no city pick"}]}
   [12 4] {:category :instant :clauses [{:kind :score-glory :basis :facedown-temples :state :done}]}

   ;; ══ Board 13 — Pillars of Etana (clean) ════════════════════════════════
   [13 0] {:category :passive :trigger :temple-placed
           :clauses [{:kind :place-raider :target :adjacent-to-temple :state :done}]}
   [13 1] {:category :instant :clauses [{:kind :gain-resource :resource :tools :count 3 :state :done}
                                        {:kind :score-glory :basis :leader-level :state :done}]}
   [13 2] {:category :instant :clauses [{:kind :gain-resource :resource :pottery :count 3 :state :done}
                                        {:kind :score-glory :basis :leader-level :state :done}]}
   [13 3] {:category :instant :clauses [{:kind :increase-role :basis :all-level-3 :state :done}]}
   [13 4] {:category :instant :clauses [{:kind :place-temple :target :adjacent-to-raider :interactive? true :state :done}]}

   ;; ══ Board 14 — Roads of Shulgi (slot 0 STUB; 14/1,14/3 conservative) ═══
   [14 0] {:category :passive :trigger :turn-start
           :clauses [{:kind :bonus-move-action :state :done
                      :note "action-choice rule: [:uruk-move dest] bonus moves in choose-action-choices (discard a good to move between Uruk and an adjacent city; once/turn via :used-uruk-travel)"}]}
   [14 1] {:category :instant :hand-conservative? true
           :clauses [{:kind :place-raider :target :lagash :state :done}
                     {:kind :score-glory :basis :raiders :state :done}]
           :note "code places raider then scores glory/raider — faithful; hand-map says :partial"}
   [14 2] {:category :instant
           :clauses [{:kind :move-magistrate :target :uruk :state :stub}
                     {:kind :gain-resource :basis :matching-uruk-demands :state :partial :approximation? true
                      :note "fixed +1 tools +1 pottery proxy; no magistrate move"}]}
   [14 3] {:category :instant :hand-conservative? true
           :clauses [{:kind :place-demand :target :eridu :count 2 :state :done}
                     {:kind :travel :target :eridu :state :done}]
           :note "places 2 demands AND travels — faithful; hand-map says :partial"}
   [14 4] {:category :instant :clauses [{:kind :place-temple :target :babylon :state :done}]}

   ;; ══ Board 15 — Ascent of Ur-Nammu ══════════════════════════════════════
   [15 0] {:category :passive :trigger :role-increased
           :clauses [{:kind :modify-cost :state :done :note "sets :free-role-increase"}]}
   [15 1] {:category :instant :clauses [{:kind :gain-resource :basis :per-demand-fulfilled :state :done}]}
   [15 2] {:category :instant :clauses [{:kind :increase-role :target :priest :state :done}
                                        {:kind :score-glory :amount 4 :condition :babylon-facedown-temple :state :done}]}
   [15 3] {:category :instant
           :clauses [{:kind :increase-role-free :target :lowest :interactive? true :state :done}
                     {:kind :travel :state :stub :note "'then take a Travel action' dropped"}]}
   [15 4] {:category :instant :hand-conservative? true
           :clauses [{:kind :score-amity :basis :raiders-adjacent-magistrate :state :done}]
           :note "3 amity per raider adjacent to a magistrate — code computes adjacency; hand-map says :partial"}

   ;; ══ Board 16 — Dominion of Hammurabi ═══════════════════════════════════
   [16 0] {:category :passive :trigger :landing
           :clauses [{:kind :grant-extra-action :condition :two-astronomers :state :done}]}
   [16 1] {:category :instant :clauses [{:kind :gain-resource :resource :pottery :basis :per-temple :state :done}]}
   [16 2] {:category :instant :clauses [{:kind :deploy :state :done}
                                        {:kind :score-amity :basis :raiders :state :done}]}
   [16 3] {:category :instant :clauses [{:kind :increase-role :target :leader :count 2 :state :done}]}
   [16 4] {:category :instant :hand-optimistic? true
           :clauses [{:kind :place-demand :target :caravan :count 2 :state :stub}
                     {:kind :sell :optional? true :state :partial :approximation? true
                      :note "proxied as +1 tools +3 amity; demands never placed"}]}

   ;; ══ Board 17 — Cunning of Kubaba (17/1 = Gap-3 divergence) ═════════════
   [17 0] {:category :passive :trigger :action-space-7
           :clauses [{:kind :gain-resource-choice :interactive? true :state :done :note "deferred :pick-resource"}]}
   [17 1] {:category :instant
           :clauses [{:kind :place-raider :target :eridu :side :point :state :done
                      :note "places a raider point-side on a free route touching Eridu (Gap-3 fixed: was flip/resource-pick; descriptor de-tagged from :pick-resource)"}]}
   [17 2] {:category :instant :hand-conservative? true
           :clauses [{:kind :place-temple :target :each-magistrate-city :side :face-down :state :done}]
           :note "face-down temple per magistrate city, supply-gated — faithful; hand-map says :partial"}
   [17 3] {:category :instant
           :clauses [{:kind :score-amity :amount 8 :condition :uruk-surrounded :state :done}
                     {:kind :flip-raider :optional? true :state :stub :note "'then you may flip' dropped"}]}
   [17 4] {:category :instant
           :clauses [{:kind :sell :yields :glory :state :partial :approximation? true
                      :note "card = SELL in caravan city for glory; code grants flat glory = merchant level"}]}

   ;; ══ Board 18 — Forge of Tubal-Cain ═════════════════════════════════════
   [18 0] {:category :passive :trigger :resource-spent
           :clauses [{:kind :modify-cost :state :done :note "tools refunded when spent; +glory per tool at end-game"}]}
   [18 1] {:category :instant :dual-path? true
           :clauses [{:kind :move-magistrate :interactive? true :state :partial
                      :note "auto grants spurious +2 tools; human does do-influence (move) + sell"}
                     {:kind :sell :optional? true :state :partial}]}
   [18 2] {:category :instant
           :clauses [{:kind :travel :interactive? true :state :done}
                     {:kind :score-glory :amount 5 :condition :samarra-facedown-temple :state :partial
                      :note "grants +2 glory even when condition unmet — not faithful"}]}
   [18 3] {:category :instant
           :clauses [{:kind :score-amity :amount 6 :condition :kish-surrounded :state :done}
                     {:kind :flip-raider :optional? true :state :stub :note "'then you may flip' dropped"}]}
   [18 4] {:category :instant :clauses [{:kind :score-amity :basis :point-raiders-x4 :state :done}
                                        {:kind :remove-raider :scope :point :state :done :note "QA lesson 8 score-then-remove"}]}

   ;; ══ Board 19 — Kilns of Ninkasi ════════════════════════════════════════
   [19 0] {:category :passive :trigger :goods-taken
           :clauses [{:kind :gain-resource :resource :pottery :count 2 :condition :took-pottery :state :done}]}
   [19 1] {:category :instant :clauses [{:kind :increase-role :target :priest :count 2 :state :done}]}
   [19 2] {:category :instant :hand-optimistic? true
           :clauses [{:kind :sell :target :pottery-cities :count 2 :state :partial :approximation? true
                      :note "proxied as +1 pottery +3 amity; no real two-city sell"}]}
   [19 3] {:category :instant :dual-path? true
           :clauses [{:kind :gain-resource-choice :verb :discard :interactive? true :state :done}
                     {:kind :move-magistrate :state :partial :note "human path do-influence; auto just +3 glory"}
                     {:kind :sell :state :partial}]}
   [19 4] {:category :instant :clauses [{:kind :flip-raider :to :point :scope :all :state :done}]}

   ;; ══ Board 20 — Vision of Rimush ════════════════════════════════════════
   [20 0] {:category :passive :trigger :temple-flipped
           :clauses [{:kind :score-glory :amount 3 :verb :discard-pottery :interactive? true :state :done :note "deferred :yes-no"}]}
   [20 1] {:category :instant :hand-optimistic? true
           :clauses [{:kind :place-raider :target :opposing-routes :state :partial
                      :note "card = each route with an OPPOSING raider; code places on routes from caravan, ignoring the opposing check"}]}
   [20 2] {:category :instant :clauses [{:kind :increase-role :target :merchant :count 2 :state :done}]}
   [20 3] {:category :instant :dual-path? true
           :clauses [{:kind :influence-magistrate :interactive? true :state :partial
                      :note "auto skips influence; human do-influence"}
                     {:kind :score-amity :basis :leader-level :state :partial
                      :note "human path sells instead of scoring leader-level amity — wrong tail"}]}
   [20 4] {:category :instant
           :clauses [{:kind :gain-resource :basis :astronomer-spaces :state :partial :approximation? true
                      :note "proxied as +1 tools +1 gold"}]}

   ;; ══ Board 21 — Legacy of Eannatum ═════════════════════════════════════
   [21 0] {:category :passive :trigger :temple-placed
           :clauses [{:kind :place-temple :side :face-down :state :done
                      :note "multi-temple model: conj a 2nd facedown temple into the city that triggered the placement (no re-trigger)"}]}
   [21 1] {:category :instant :clauses [{:kind :travel-anywhere :interactive? true :condition :at-eridu :state :done}]}
   [21 2] {:category :instant :clauses [{:kind :increase-role :target :raider :state :done}
                                        {:kind :increase-role :target :leader :state :done}]}
   [21 3] {:category :instant :dual-path? true
           :clauses [{:kind :travel :interactive? true :state :done}
                     {:kind :sell :optional? true :state :partial :note "auto travels only; human travel + sell"}]}
   [21 4] {:category :instant :clauses [{:kind :score-glory :basis :demands-fulfilled :state :done}]}

   ;; ══ Board 22 — Strategy of Naram-Sin ═══════════════════════════════════
   [22 0] {:category :passive :trigger :action-space-7
           :clauses [{:kind :grant-extra-action :verb :repeat :state :done :note "sets :bonus-repeat-action"}]}
   [22 1] {:category :instant :clauses [{:kind :increase-role :target :raider :state :done}
                                        {:kind :increase-role :target :merchant :state :done}]}
   [22 2] {:category :instant :hand-optimistic? true
           :clauses [{:kind :place-demand :target :facedown-temples :state :partial :approximation? true
                      :note "card = demand token on each facedown temple; code proxies +amity = facedown count"}]}
   [22 3] {:category :instant :hand-optimistic? true
           :clauses [{:kind :gain-resource-choice :interactive? true :state :done}
                     {:kind :travel :state :stub :note "'then take a travel action' dropped on both paths"}]}
   [22 4] {:category :instant :hand-conservative? true
           :clauses [{:kind :score-amity :basis :raiders :state :done}
                     {:kind :travel :interactive? true :state :done}]
           :note "amity per raider + human travel — faithful; hand-map says :partial"}

   ;; ══ Board 23 — Market of Puabi ═════════════════════════════════════════
   [23 0] {:category :passive :trigger :sold
           :clauses [{:kind :modify-scoring :state :done :note "sell scores glory instead of amity"}]}
   [23 1] {:category :instant :clauses [{:kind :increase-role :target :priest :state :done}
                                        {:kind :increase-role :target :merchant :state :done}]}
   [23 2] {:category :instant :hand-optimistic? true
           :clauses [{:kind :sell :target :eridu :count 2 :state :partial :approximation? true
                      :note "proxied as travel + flat +4 amity"}]}
   [23 3] {:category :instant :hand-optimistic? true
           :clauses [{:kind :gain-resource-choice :interactive? true :state :done}
                     {:kind :travel :state :stub :note "travel dropped"}
                     {:kind :increase-role :target :merchant :state :done :note "auto only"}]}
   [23 4] {:category :instant :clauses [{:kind :place-temple :target :magistrate-city :interactive? true :state :done}]}

   ;; ══ Board 24 — Siege of Shulme (slot 0 STUB) ═══════════════════════════
   [24 0] {:category :passive :trigger :deployed
           :clauses [{:kind :sell :condition :surrounded-city :state :done
                      :note "auto-sells in the surrounded city via bonus-sell-in (no travel; no-ops if no matching good)"}]}
   [24 1] {:category :instant :clauses [{:kind :increase-role :target :raider :state :done}
                                        {:kind :increase-role :target :leader :state :done}]}
   [24 2] {:category :instant :hand-optimistic? true
           :clauses [{:kind :place-demand :target :magistrates :state :partial :approximation? true
                      :note "proxied as flat +2 glory; no demand tokens placed"}]}
   [24 3] {:category :instant :clauses [{:kind :score-glory :basis :demands-fulfilled :state :done}]}
   [24 4] {:category :instant
           :clauses [{:kind :gain-resource :basis :demands-at-magistrates :state :partial :approximation? true
                      :note "grants up to 2 matching demand resources"}]}

   ;; ══ Board 25 — Command of Mesannepada ══════════════════════════════════
   [25 0] {:category :passive :trigger :deployed
           :clauses [{:kind :modify-deploy :state :done :note "sets :allow-double-raiders"}]}
   [25 1] {:category :instant :dual-path? true
           :clauses [{:kind :influence-magistrate :interactive? true :state :partial
                      :note "auto skips influence; human do-influence"}
                     {:kind :score-glory :basis :raiders-moved-through :state :partial
                      :note "auto proxies glory = 2 + point-count; human path sells instead — wrong tail"}]}
   [25 2] {:category :instant :clauses [{:kind :increase-role :target :merchant :state :done}
                                        {:kind :increase-role :target :leader :state :done}]}
   [25 3] {:category :instant
           :clauses [{:kind :place-temple :side :face-down :count 2 :state :done
                      :note "multi-temple model: conj two facedown temples into the caravan city (supply-gated)"}]}
   [25 4] {:category :instant :hand-optimistic? true
           :clauses [{:kind :gain-resource-choice :interactive? true :state :done}
                     {:kind :travel :state :stub :note "'then travel' dropped"}]}

   ;; ══ Board 26 — Court of Enshakushanna ══════════════════════════════════
   [26 0] {:category :passive :trigger :sold
           :clauses [{:kind :score-amity :amount 2 :condition :magistrate-bonus :state :partial :approximation? true
                      :note "audit: fires on :sold when glory-scored>0 (sell adjacent to magistrate), a proxy for the card's 'score Magistrate bonus points' trigger"}]}
   [26 1] {:category :instant :clauses [{:kind :increase-role :target :priest :state :done}
                                        {:kind :increase-role :target :leader :state :done}]}
   [26 2] {:category :instant :clauses [{:kind :increase-role :target :priest :state :done}
                                        {:kind :increase-role :target :raider :state :done}]}
   [26 3] {:category :instant
           :clauses [{:kind :sell :state :partial :approximation? true :note "flat +2 amity proxy"}
                     {:kind :place-temple :condition :sold-tools-or-pottery :state :partial
                      :note "code places temple unconditionally rather than gating on what was sold"}]}
   [26 4] {:category :instant
           :clauses [{:kind :place-raider :target :adjacent-to-caravan :state :done}
                     {:kind :place-temple :condition :surrounded :optional? true :state :stub
                      :note "'if you surround it, place a temple' dropped"}]}

   ;; ══ Board 27 — Path of Alulim (clean once choice path runs) ════════════
   [27 0] {:category :passive :trigger :role-increased
           :clauses [{:kind :increase-role :modifier :double-cost :interactive? true :state :done :note "deferred :pick-role"}]}
   [27 1] {:category :instant :clauses [{:kind :travel :interactive? true :state :done}
                                        {:kind :sell :optional? true :state :partial :approximation? true
                                         :note "auto = flat +3 amity; human travel + sell"}]}
   [27 2] {:category :instant :clauses [{:kind :travel :interactive? true :state :done}
                                        {:kind :deploy :optional? true :state :done}]}
   [27 3] {:category :instant :clauses [{:kind :travel :interactive? true :state :done}
                                        {:kind :place-temple :optional? true :state :done}]}
   [27 4] {:category :instant :clauses [{:kind :gain-resource-choice :count 3 :interactive? true :state :done}]}

   ;; ══ Board 28 — Stars of Sin-Kashid ═════════════════════════════════════
   [28 0] {:category :passive :trigger :landing
           :clauses [{:kind :increase-role :condition :four-astronomers :state :done :note "sets :bonus-role-increase"}]}
   [28 1] {:category :instant :clauses [{:kind :travel :interactive? true :state :done}
                                        {:kind :place-temple :optional? true :state :done}]}
   [28 2] {:category :instant :clauses [{:kind :travel :interactive? true :state :done}
                                        {:kind :place-temple :optional? true :state :done}]}
   [28 3] {:category :instant
           :clauses [{:kind :sell :target :empty-demand-city :state :partial :approximation? true
                      :note "dec gold + flat +4 amity proxy"}
                     {:kind :place-demand :state :stub :note "'then place a random demand on it' dropped"}]}
   [28 4] {:category :instant :clauses [{:kind :place-raider :target :kish :side :point :state :done}]}

   ;; ══ Board 29 — Treasury of Ibbi-Sin ════════════════════════════════════
   [29 0] {:category :passive :trigger :resource-spent
           :clauses [{:kind :score-amity :amount 2 :condition :paid-gold :state :done}]}
   [29 1] {:category :instant
           :clauses [{:kind :decrease-role :target :leader :state :done}
                     {:kind :increase-role-free :target :merchant :state :done}
                     {:kind :increase-role-free :target :priest :state :done}
                     {:kind :increase-role-free :target :raider :state :done
                      :note "fixed in unification: 'all OTHER roles' now includes raider"}]}
   [29 2] {:category :instant :clauses [{:kind :travel :interactive? true :state :done}
                                        {:kind :sell :optional? true :state :partial :approximation? true
                                         :note "auto = flat +3 amity; human travel + sell"}]}
   [29 3] {:category :instant :clauses [{:kind :place-raider :target :each-river :state :done}]}
   [29 4] {:category :instant
           :clauses [{:kind :place-temple :target :each-surrounded-city :state :partial :approximation? true
                      :note "code places one temple at caravan; card = each surrounded city"}]}

   ;; ══ Board 30 — Council of Amar-Sin (slot 0 STUB; whole row Gap-1) ══════
   [30 0] {:category :passive :trigger :goods-taken
           :clauses [{:kind :gain-resource :basis :other-astronomer-location :state :done
                      :note "action-choice rule: [:alt-take space] options in resolve-take-choices (take goods from another of your astronomers' wheel positions instead)"}]}
   [30 1] {:category :instant :dual-path? true
           :clauses [{:kind :influence-magistrate :state :stub :note "influence dropped both paths"}
                     {:kind :travel :interactive? true :state :partial :note "auto proxies +glory=leader; human travels"}]}
   [30 2] {:category :instant :dual-path? true
           :clauses [{:kind :influence-magistrate :interactive? true :state :partial :note "auto skips; human do-influence"}
                     {:kind :sell :state :partial :note "auto proxies +amity=leader"}]}
   [30 3] {:category :instant :dual-path? true
           :clauses [{:kind :deploy :state :partial :note "auto proxies +glory=raider; human deploys"}
                     {:kind :influence-magistrate :interactive? true :state :partial}]}
   [30 4] {:category :instant :dual-path? true
           :clauses [{:kind :influence-magistrate :interactive? true :state :partial :note "auto skips; human do-influence"}
                     {:kind :place-temple :state :partial :note "auto proxies +amity=priest"}]}

   ;; ══ Board 31 — Horizon of Sharkalisharri (slot 0 STUB) ═════════════════
   [31 0] {:category :passive :trigger :landing
           :clauses [{:kind :grant-free-action :action :travel :state :done :note "if another of your astronomers is on space 7, grants :pending-free-travel (bonus travel)"}]}
   [31 1] {:category :instant :clauses [{:kind :increase-role :basis :all-level-1 :state :done}]}
   [31 2] {:category :instant :clauses [{:kind :increase-role :basis :all-level-3 :state :done}]}
   [31 3] {:category :instant :clauses [{:kind :gain-resource-choice :interactive? true :state :done}
                                        {:kind :place-temple :side :face-down :state :done}]}
   [31 4] {:category :instant :clauses [{:kind :gain-resource-choice :interactive? true :state :done}
                                        {:kind :deploy :state :done}]}

   ;; ══ Board 32 — Jewel of Ku-Bau ═════════════════════════════════════════
   [32 0] {:category :passive :trigger :sold
           :clauses [{:kind :modify-scoring :verb :discard-gem :interactive? true :state :done :note "deferred :yes-no priest-vs-merchant"}]}
   [32 1] {:category :instant
           :clauses [{:kind :sell :state :stub :note "'Sell in your city' not performed"}
                     {:kind :score-glory :basis :demands-fulfilled :state :done}]}
   [32 2] {:category :instant
           :clauses [{:kind :gain-resource :resource :gems :state :done}
                     {:kind :travel :count 2 :interactive? true :state :partial :note "card = two travel actions; one travel via human path"}]}
   [32 3] {:category :instant :clauses [{:kind :place-raider :target :between-temple-cities :state :done}]}
   [32 4] {:category :instant :hand-optimistic? true :dual-path? true
           :clauses [{:kind :influence-magistrate :interactive? true :state :partial :note "auto proxies +2 amity +2 glory; human do-influence"}
                     {:kind :sell :optional? true :state :partial}]}

   ;; ══ Board 33 — Vanguard of Enmebaragesi (slot 0 STUB) ═════════════════
   [33 0] {:category :passive :trigger :deployed
           :clauses [{:kind :influence-magistrate :state :done :note "auto-influences a magistrate on a deploy-route endpoint 1 step clockwise via perform-influence (heuristic minimal beneficial default)"}]}
   [33 1] {:category :instant
           :clauses [{:kind :decrease-role :target :merchant :state :done}
                     {:kind :increase-role-free :target :raider :state :done}
                     {:kind :increase-role-free :target :priest :state :done}
                     {:kind :increase-role-free :target :leader :state :done
                      :note "fixed in unification: 'all OTHER roles' now includes leader"}]}
   [33 2] {:category :instant :dual-path? true
           :clauses [{:kind :place-temple :side :face-down :state :done :note "multi-temple model: conj a facedown temple into the (pre-travel) caravan city"}
                     {:kind :travel :interactive? true :state :done}]}
   [33 3] {:category :instant :clauses [{:kind :place-temple :target :uruk :state :done}]}
   [33 4] {:category :instant :clauses [{:kind :deploy :interactive? true :state :done}
                                        {:kind :travel :state :done}]}

   ;; ══ Board 34 — Honor of Agga (34/3,34/4 fixed to multi-pick 2026-05-23) ═
   [34 0] {:category :passive :trigger :raider-scored
           :clauses [{:kind :modify-scoring :state :done :note "sets :raider-score-amity"}]}
   [34 1] {:category :instant
           :clauses [{:kind :place-raider :target :around-uruk :cost :tools :count 4 :state :partial
                      :note "CODE BUG (audit): card = a raider on EACH of Uruk's 4 routes for 2 tools; code caps at 2 and pays per-raider"}]}
   [34 2] {:category :instant
           :clauses [{:kind :place-raider :target :each-existing-route :state :partial :approximation? true
                      :note "approximated as up to 2 raiders"}]}
   [34 3] {:category :instant :clauses [{:kind :sell :target :magistrate-and-my-temple :multi? true :no-travel? true :interactive? true :state :done}]}
   [34 4] {:category :instant :clauses [{:kind :sell :target :magistrate-and-my-temple :multi? true :no-travel? true :interactive? true :state :done}]}

   ;; ══ Board 35 — Wanderer of Dumuzi ═════════════════════════════════════
   [35 0] {:category :passive :trigger :turn-start
           :clauses [{:kind :gain-resource-choice :condition :no-goods :interactive? true :state :done :note "deferred :pick-resource"}]}
   [35 1] {:category :instant :clauses [{:kind :travel :interactive? true :state :done}
                                        {:kind :sell :optional? true :state :partial :approximation? true
                                         :note "auto = flat +3 amity; human travel + sell"}]}
   [35 2] {:category :instant
           :clauses [{:kind :place-temple :cost :pottery :basis :per-pottery-paid
                      :target :city-you-have-temple :state :done
                      :note "multi-temple model: pay N pottery, conj a (face-up) temple into a city you already hold, one per pottery (supply-capped, round-robin over owned cities)"}]}
   [35 3] {:category :instant :clauses [{:kind :increase-role-choice :interactive? true :state :done}]}
   [35 4] {:category :instant
           :clauses [{:kind :influence-magistrate :interactive? true :state :done
                      :note "descriptor fixed to :pick-city :magistrate (was wrongly :pick-role); influences the chosen/auto magistrate"}
                     {:kind :score-glory :basis :raiders-moved-through :state :partial :approximation? true
                      :note "proxied as +glory = 2 + point-raider count; not literally the raiders the magistrate moved through"}]}})

;; ─────────────────────────────────────────────────────────────────────────────
;; Derived views — the build-time checks read these.
;; ─────────────────────────────────────────────────────────────────────────────

(def all-slots
  "Canonical key set: every [board-id slot-idx], 35 boards × 5 slots = 175."
  (set (for [b (range 1 36) s (range 0 5)] [b s])))

(def authored-slots (set (keys effect-specs)))

(defn coverage []
  {:total (count all-slots)
   :authored (count authored-slots)
   :remaining (- (count all-slots) (count authored-slots))})

(defn derive-state
  "Roll per-clause :state up to a slot implementation-state.
   :approximation? clauses still count as their :state (usually :done)."
  [clauses]
  (let [ss (set (map :state clauses))]
    (cond
      (= ss #{:done})            :implemented
      (every? #{:stub} ss)       :stub
      :else                      :partial)))

(defn slot-status
  "Two-axis status (Gap 2): category is independent of implementation-state.
   Passive + all-stub clauses ⇒ :passive-stub (the un-built passive that used
   to hide as :persistent). Passive otherwise ⇒ :persistent. Instant ⇒ the
   rolled-up implementation-state."
  [{:keys [category clauses]}]
  (if (= category :passive)
    (if (= :stub (derive-state clauses)) :passive-stub :persistent)
    (derive-state clauses)))

(defn slot-interactive?
  "Does any clause need a player decision? (instant slots only — passive
   choices route through apply-passive-choice, not bonus-needs-choice?)."
  [{:keys [clauses]}]
  (boolean (some :interactive? clauses)))

(defn backlog
  "Every authored slot that is not fully faithful: the missing/lossy clauses.
   This is the executable form of the walkthrough's gap list."
  []
  (for [[slot {:keys [clauses] :as spec}] (sort effect-specs)
        :let [missing  (->> clauses (filter #(= :stub (:state %)))    (mapv :kind))
              partials (->> clauses (filter #(= :partial (:state %))) (mapv :kind))
              approx   (->> clauses (filter :approximation?)          (mapv :kind))]
        :when (or (seq missing) (seq partials) (seq approx))]
    {:slot slot
     :status (slot-status spec)
     :missing-clauses missing
     :partial-clauses partials
     :approximation-clauses approx}))
