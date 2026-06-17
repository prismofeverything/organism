(ns eridu.bonus-effects-test
  "Regression unit tests for the bonus-effect subsystem after the dual-path
   unification + temple-multi-per-city refactor (src/cljc/eridu/game.cljc,
   eridu.bonus). Each test guards one of the specific bugs that were fixed:

   - INSTANT slots applied via game/apply-bonus-effect (auto/bot, no choice)
     and game/apply-bonus-with-choice (human path, with a choice value).
   - PASSIVES applied via game/apply-passive (trigger + context).
   - TEMPLE multi-per-city semantics (place-temple-in allow-duplicate?,
     count-temples-placed, contest :D2).

   States are hand-built (deterministic) in the style of game_test.clj's
   make-player / make-state helpers, rather than via the randomized
   initial-state. The player's bonus board is set with
   (assoc-in state [:bonus-boards pk] BOARD-ID); passives also need slot 0
   uncovered (assoc-in state [:players pk :bonus-board 0] :uncovered)."
  (:require
   [clojure.test :refer [deftest is testing]]
   [eridu.game :as game]))

;; -----------------------------------------------------------------------------
;; Minimal builders (same dumb-helper philosophy as game_test.clj)
;; -----------------------------------------------------------------------------

(defn- make-player
  "Build a minimal player record. Pass overrides as a map. Defaults give all
   four roles level 1 and zeroed resources/scores."
  [overrides]
  (merge {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
          :resources {:tools 0 :pottery 0 :gold 0 :gems 0}
          :temples {} :raiders {} :astronomers []
          :amity 0 :glory 0
          :temples-supply 7 :raiders-supply 6
          :bonus-board (vec (repeat 5 :covered))}
         overrides))

(defn- state-with
  "Build a minimal 2-player game state holding :alice (from `player-overrides`),
   with :alice assigned `board-id`. turn-order length 2 → active-routes 2
   (Samarra removed). Extra top-level keys via `extra`."
  ([board-id player-overrides] (state-with board-id player-overrides {}))
  ([board-id player-overrides extra]
   (merge {:players {:alice (make-player player-overrides)}
           :turn-order [:alice :bob]
           :magistrates {}
           :bonus-boards {:alice board-id}}
          extra)))

(defn- uncover-passive
  "Mark :alice's slot-0 passive uncovered so apply-passive fires."
  [state]
  (assoc-in state [:players :alice :bonus-board 0] :uncovered))

(defn- amity [s] (get-in s [:players :alice :amity]))
(defn- glory [s] (get-in s [:players :alice :glory]))
(defn- role [s r] (get-in s [:players :alice :roles r]))
(defn- res [s r] (get-in s [:players :alice :resources r] 0))
(defn- raiders [s] (get-in s [:players :alice :raiders]))
(defn- temples-of [s] (get-in s [:players :alice :temples]))

;; =============================================================================
;; INSTANT-SLOT FAITHFULNESS (auto path — apply-bonus-effect, no choice)
;; =============================================================================

(deftest board-29-slot-1-decrease-leader-increase-all-three-others-test
  ;; "Decrease your Leader role to increase ALL of your OTHER roles."
  ;; Bug: only increased two of the three other roles. Fix: merchant, priest
  ;; AND raider each +1, leader -1.
  (testing "leader L>1 → leader-1 and all THREE others +1"
    (let [s  (state-with 29 {:roles {:leader 3 :merchant 1 :priest 1 :raider 1}})
          s' (game/apply-bonus-effect s :alice 29 1)]
      (is (= 2 (role s' :leader))   "leader 3 → 2")
      (is (= 2 (role s' :merchant)) "merchant +1")
      (is (= 2 (role s' :priest))   "priest +1")
      (is (= 2 (role s' :raider))   "raider +1 (the third 'other' — the bug)")))
  (testing "leader already at 1 → no-op (can't decrease below 1)"
    (let [s  (state-with 29 {:roles {:leader 1 :merchant 1 :priest 1 :raider 1}})
          s' (game/apply-bonus-effect s :alice 29 1)]
      (is (= 1 (role s' :leader)))
      (is (= 1 (role s' :merchant)) "no increase when leader can't be decreased"))))

(deftest board-33-slot-1-decrease-merchant-increase-all-three-others-test
  ;; "Decrease your Merchant role to increase ALL of your OTHER roles."
  ;; Other roles are raider, priest AND leader.
  (testing "merchant M>1 → merchant-1 and raider/priest/leader each +1"
    (let [s  (state-with 33 {:roles {:merchant 4 :raider 1 :priest 1 :leader 1}})
          s' (game/apply-bonus-effect s :alice 33 1)]
      (is (= 3 (role s' :merchant)) "merchant 4 → 3")
      (is (= 2 (role s' :raider))   "raider +1")
      (is (= 2 (role s' :priest))   "priest +1")
      (is (= 2 (role s' :leader))   "leader +1 (the third 'other')"))))

(deftest board-10-slot-3-amity-equals-leader-level-test
  ;; "Place a Raider adjacent to a Magistrate. Score Amity based on your Leader
  ;; level." Bug: scored a flat +2. Fix: amity == leader level.
  (testing "amity gained == leader level (4), not a flat +2"
    ;; No magistrate cities → no route to place on, but the amity (leader level)
    ;; is granted unconditionally in the else branch. Deterministic delta.
    (let [s  (state-with 10 {:roles {:leader 4 :merchant 1 :priest 1 :raider 1}
                             :amity 0})
          s' (game/apply-bonus-effect s :alice 10 3)]
      (is (= 4 (amity s')) "leader level 4 → +4 amity (NOT flat +2)")))
  (testing "scales with a different leader level (2 → +2)"
    (let [s  (state-with 10 {:roles {:leader 2 :merchant 1 :priest 1 :raider 1}})
          s' (game/apply-bonus-effect s :alice 10 3)]
      (is (= 2 (amity s')) "leader level 2 → +2 amity (coincidentally matches the old flat value)"))))

(deftest board-17-slot-1-places-point-raider-near-eridu-test
  ;; "Place a Raider next to Eridu on its point side." Bug: merely flipped an
  ;; existing raider / placed nothing. Fix: PLACE a new raider on a free route
  ;; touching Eridu, point-side up.
  (testing "places a NEW raider on an Eridu route, state :point"
    (let [s  (state-with 17 {:roles {:raider 4 :merchant 1 :priest 1 :leader 1}
                             :raiders {} :raiders-supply 6})
          s' (game/apply-bonus-effect s :alice 17 1)
          rs (raiders s')]
      (is (= 1 (game/count-raiders-deployed (get-in s' [:players :alice])))
          "raider-count +1 (a placement, not a flip)")
      (let [[rk statuses] (first rs)]
        (is (= [:point] statuses) "placed on its POINT side")
        (is (or (contains? (set rk) :eridu)) "route touches Eridu"))))
  (testing "all Eridu routes already occupied → no-op (nothing to place)"
    ;; Eridu's routes in a 2p game: eridu-uruk (road), eridu-lagash (road).
    (let [s  (state-with 17 {:roles {:raider 4 :merchant 1 :priest 1 :leader 1}
                             :raiders {[:eridu :uruk] [:raiding]
                                       [:eridu :lagash] [:raiding]}
                             :raiders-supply 6})
          s' (game/apply-bonus-effect s :alice 17 1)]
      (is (= 2 (count (raiders s'))) "no free Eridu route → raider count unchanged"))))

(deftest board-34-slot-1-raiders-on-all-uruk-routes-test
  ;; "Pay Tools, Tools to place a Raider on each space surrounding Uruk."
  ;; Bug: capped at 2 raiders. Fix: place on ALL available Uruk routes (up to 4)
  ;; for a fixed cost of exactly 2 tools.
  (testing ">=2 tools + free Uruk routes → raiders on ALL of them, spends 2 tools"
    ;; Uruk's 4 routes (2p): uruk-babylon, uruk-eridu (roads); uruk-nippur,
    ;; uruk-lagash (rivers). Raider level 4 → max 6 deployed; supply 6.
    (let [s  (state-with 34 {:roles {:raider 4 :merchant 1 :priest 1 :leader 1}
                             :resources {:tools 3 :pottery 0 :gold 0 :gems 0}
                             :raiders {} :raiders-supply 6})
          s' (game/apply-bonus-effect s :alice 34 1)
          rs (raiders s')]
      (is (= 4 (count rs)) "all 4 Uruk routes get a raider (NOT capped at 2)")
      (is (every? #(contains? (set %) :uruk) (keys rs)) "every placement touches Uruk")
      (is (= 1 (res s' :tools)) "spent exactly 2 tools (3 → 1), regardless of raider count")))
  (testing "fewer than 2 tools → no-op (no placement, no spend)"
    (let [s  (state-with 34 {:roles {:raider 4 :merchant 1 :priest 1 :leader 1}
                             :resources {:tools 1 :pottery 0 :gold 0 :gems 0}
                             :raiders {} :raiders-supply 6})
          s' (game/apply-bonus-effect s :alice 34 1)]
      (is (= 0 (count (raiders s'))) "can't pay → no raiders placed")
      (is (= 1 (res s' :tools)) "tools untouched"))))

(deftest board-18-slot-2-glory-only-with-facedown-samarra-temple-test
  ;; "Take a travel action then score 5 Glory IF you have a facedown temple in
  ;; Samarra." Bug: granted a spurious +2 when the condition was unmet. Fix:
  ;; 0 glory with no facedown Samarra temple, +5 with one.
  (testing "no facedown temple in Samarra → +0 glory (NOT +2)"
    (let [s  (state-with 18 {:temples {} :glory 0})
          s' (game/apply-bonus-effect s :alice 18 2)]
      (is (= 0 (glory s')) "condition unmet → no glory at all")))
  (testing "a facedown temple in Samarra → +5 glory"
    (let [s  (state-with 18 {:temples {:samarra [:face-down]} :glory 0})
          s' (game/apply-bonus-effect s :alice 18 2)]
      (is (= 5 (glory s')) "facedown Samarra temple → +5 glory")))
  (testing "a FACE-UP temple in Samarra does not satisfy the condition"
    (let [s  (state-with 18 {:temples {:samarra [:face-up]} :glory 0})
          s' (game/apply-bonus-effect s :alice 18 2)]
      (is (= 0 (glory s')) "only a FACEDOWN Samarra temple counts"))))

;; =============================================================================
;; DUAL-PATH — human path (apply-bonus-with-choice) must equal the card,
;; not a divergent effect
;; =============================================================================

(deftest board-12-slot-3-human-path-merchant-and-current-city-glory-test
  ;; "Increase your Merchant level (paying any costs). Then Sell to the city you
  ;; are IN for Glory instead." Old human arm sold in a magistrate city for amity
  ;; and dropped the merchant increase. Fix: increase merchant AND score glory in
  ;; the CURRENT city; the chosen city is ignored (no sell-for-amity).
  (testing "human path increases merchant + scores glory; chosen city ignored"
    (let [s  (state-with 12 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             ;; give a sellable good + a magistrate-city demand so a
                             ;; wrong sell-for-amity path would be detectable
                             :resources {:tools 0 :pottery 0 :gold 0 :gems 0}
                             :amity 0 :glory 0 :caravan :uruk}
                          {:magistrates {:mag-0 :babylon}
                           :city-demands {:babylon [:gems]}})
          ;; human passes a city (a magistrate city) as their "choice"
          s' (game/apply-bonus-with-choice s :alice 12 3 :babylon)]
      (is (= 2 (role s' :merchant)) "merchant increased (level 1→2 is free)")
      (is (= 3 (glory s')) "glory scored in the current city (proxy +3)")
      (is (= 0 (amity s')) "amity NOT scored — this is a glory sell, not an amity sell")
      (is (empty? (raiders s')) "no side effects")
      ;; the chosen magistrate city's demand must be untouched (no sell happened there)
      (is (= [:gems] (get-in s' [:city-demands :babylon]))
          "the chosen city is ignored — its demand caravan is unchanged"))))

(deftest board-22-slot-4-human-path-amity-and-travel-test
  ;; "Score 2 Amity for each of your Raiders. Then take a travel action."
  ;; Old human arm dropped the amity entirely. Fix: score 2*raider-count amity
  ;; AND travel to the chosen city.
  (testing "human path scores 2*raider-count amity AND moves the caravan"
    (let [s  (state-with 22 {:raiders {[:eridu :uruk] [:raiding]
                                       [:kish :nippur] [:point]
                                       [:lagash :nippur] [:raiding]}
                             :amity 0 :caravan :uruk})
          s' (game/apply-bonus-with-choice s :alice 22 4 :eridu)]
      (is (= 6 (amity s')) "3 raiders × 2 amity = 6 (the amity is NOT dropped)")
      (is (= :eridu (get-in s' [:players :alice :caravan])) "travelled to the chosen city")))
  (testing "auto path (no choice) also scores the amity"
    (let [s  (state-with 22 {:raiders {[:eridu :uruk] [:raiding]} :amity 0 :caravan :uruk})
          s' (game/apply-bonus-effect s :alice 22 4)]
      (is (= 2 (amity s')) "1 raider × 2 amity = 2 on the auto arm too"))))

;; =============================================================================
;; TEMPLE MULTI-PER-CITY
;; =============================================================================
;; place-temple-in is private; exercise it through the bonus slots that call it.
;; [33 3] "Temple in Uruk" calls (place-temple-in state pk :uruk true) and
;; [10 4] "Temple in Nippur" calls (place-temple-in state pk :nippur true) —
;; both allow-duplicate? = true. There is no bonus slot that places with
;; allow-duplicate? = false, so the "normal action stays 1/city" case is
;; covered via the public normal temple-action path instead (see note below).

(deftest place-temple-in-allow-duplicate-adds-second-test
  ;; allow-duplicate? true into a city you already hold ADDS a second temple,
  ;; does NOT overwrite, and spends exactly one from supply.
  (testing "[33 3] placing in Uruk where alice already has a temple → 2 in Uruk"
    (let [s  (state-with 33 {:temples {:uruk [:face-up]}
                             :temples-supply 7})
          s' (game/apply-bonus-effect s :alice 33 3)]
      (is (= 2 (count (get (temples-of s') :uruk)))
          "Uruk now holds TWO temples (second added, not overwritten)")
      (is (= [:face-up :face-up] (get (temples-of s') :uruk))
          "the original temple is preserved alongside the new one")
      (is (= 2 (game/count-temples-placed (get-in s' [:players :alice])))
          "total temple count went 1 → 2 (the second temple is a real add)")
      (is (= 6 (get-in s' [:players :alice :temples-supply]))
          "supply decremented by exactly 1 (7 → 6)"))))

(deftest place-temple-in-no-duplicate-stays-one-per-city-test
  ;; The NORMAL temple action passes allow-duplicate? false and must stay 1/city.
  ;; No bonus slot calls place-temple-in with false (every slot passes true), so
  ;; we exercise the private fn directly via its var — it is the source of truth
  ;; for the 1/city rule, and the normal-action choice layer relies on it.
  (let [place-temple-in @#'game/place-temple-in]
    (testing "allow-duplicate? false into a city you already hold → no-op"
      (let [s  (state-with 99 {:temples {:uruk [:face-up]} :temples-supply 7})
            s' (place-temple-in s :alice :uruk false)]
        (is (= [:face-up] (get (temples-of s') :uruk))
            "no second temple added when duplicates are disallowed")
        (is (= 7 (get-in s' [:players :alice :temples-supply]))
            "no supply spent on the disallowed duplicate")))
    (testing "allow-duplicate? false into a FRESH city still places one"
      (let [s  (state-with 99 {:temples {:uruk [:face-up]} :temples-supply 7})
            s' (place-temple-in s :alice :nippur false)]
        (is (= [:face-up] (get (temples-of s') :nippur)) "fresh city gets its first temple")
        (is (= 6 (get-in s' [:players :alice :temples-supply])) "supply spent once")))))

(deftest place-temple-in-no-duplicate-when-supply-exhausted-test
  ;; Guard the supply gate: allow-duplicate? true is still no-op with empty supply.
  (testing "[33 3] with 0 temple supply → no-op (no add, no negative supply)"
    (let [s  (state-with 33 {:temples {:uruk [:face-up]} :temples-supply 0})
          s' (game/apply-bonus-effect s :alice 33 3)]
      (is (= [:face-up] (get (temples-of s') :uruk)) "no second temple added")
      (is (= 0 (get-in s' [:players :alice :temples-supply])) "supply stays 0"))))

(deftest count-temples-placed-counts-temples-not-cities-test
  ;; count-temples-placed must count temple PIECES, not city keys.
  (testing "2 temples in one city + 1 in another → 3 (not 2 cities)"
    (let [p (make-player {:temples {:uruk [:face-up :face-down]
                                    :nippur [:face-up]}})]
      (is (= 3 (game/count-temples-placed p))
          "two-in-Uruk + one-in-Nippur = 3 temples (NOT 2 cities)")))
  (testing "empty → 0"
    (is (= 0 (game/count-temples-placed (make-player {}))))))

(deftest contest-D2-counts-distinct-river-cities-not-temples-test
  ;; Contest D2: "a temple in four river CITIES". Must count distinct river
  ;; cities, NOT total temples. 4 temples spread over 3 river cities fails;
  ;; 4 distinct river cities passes.
  (let [check (fn [temples]
                (game/evaluate-contest
                 {:players {:alice (make-player {:temples temples})}
                  :turn-order [:alice]
                  :magistrates {}}
                 :alice {:id :D2}))]
    (testing "2 temples in one river city + temples in 2 others (4 temples, 3 cities) → FALSE"
      ;; river-cities = #{:babylon :kish :uruk :nippur :lagash}
      (is (= false (check {:uruk [:face-up :face-up]
                           :nippur [:face-up]
                           :lagash [:face-up]}))
          "4 temples but only 3 distinct river cities does NOT satisfy D2"))
    (testing "4 distinct river cities → TRUE"
      (is (= true (check {:uruk [:face-up]
                          :nippur [:face-up]
                          :lagash [:face-up]
                          :kish [:face-up]}))
          "4 distinct river cities satisfies D2"))
    (testing "non-river cities don't count toward D2"
      ;; eridu and nineveh are NOT river cities.
      (is (= false (check {:uruk [:face-up] :nippur [:face-up]
                           :eridu [:face-up] :nineveh [:face-up]}))
          "only 2 of the 4 cities are river cities → fails"))))

;; =============================================================================
;; PASSIVES (apply-passive: trigger + context; board set + slot 0 uncovered)
;; =============================================================================

(deftest board-24-passive-deployed-sells-to-surrounded-city-test
  ;; [24 0] :deployed with {:surrounded-city C}: if C demands a good the player
  ;; holds, that good is spent and merchant-level amity is scored (sell, no travel).
  (testing "surrounded city demands a held good → good spent, amity gained"
    (let [s  (-> (state-with 24 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                                 :resources {:pottery 1 :tools 0 :gold 0 :gems 0}
                                 :amity 0}
                             {:city-demands {:lagash [:pottery]}})
                 uncover-passive)
          s' (game/apply-passive s :alice :deployed {:surrounded-city :lagash})]
      (is (= 0 (res s' :pottery)) "the matching good was sold (spent)")
      (is (= 2 (amity s')) "merchant level 1 → +2 amity scored for the sell")
      (is (= [] (get-in s' [:city-demands :lagash])) "the demand token was consumed")))
  (testing "surrounded city demands nothing the player holds → no-op"
    (let [s  (-> (state-with 24 {:resources {:gems 1} :amity 0}
                             {:city-demands {:lagash [:pottery]}})
                 uncover-passive)
          s' (game/apply-passive s :alice :deployed {:surrounded-city :lagash})]
      (is (= 0 (amity s')) "nothing sellable → no amity")
      (is (= 1 (res s' :gems)) "good untouched"))))

(deftest board-5-passive-magistrate-moved-travels-with-it-test
  ;; [5 0] :magistrate-moved with {:from F :to T}: caravan moves to T only when
  ;; F == the player's caravan (the magistrate was pushed out of the city the
  ;; caravan stood in).
  (testing ":from == caravan city → caravan travels with the magistrate"
    (let [s  (-> (state-with 5 {:caravan :babylon}) uncover-passive)
          s' (game/apply-passive s :alice :magistrate-moved {:from :babylon :to :uruk})]
      (is (= :uruk (get-in s' [:players :alice :caravan]))
          "caravan followed the magistrate from babylon → uruk")))
  (testing ":from != caravan city → caravan stays put"
    (let [s  (-> (state-with 5 {:caravan :eridu}) uncover-passive)
          s' (game/apply-passive s :alice :magistrate-moved {:from :babylon :to :uruk})]
      (is (= :eridu (get-in s' [:players :alice :caravan]))
          "magistrate moved out of a city the caravan wasn't in → no follow"))))

(deftest board-31-passive-landing-two-astronomers-on-7-test
  ;; [31 0] :landing: if one of the player's OTHER astronomers is on space 7, set
  ;; :pending-free-travel. The astronomer that just landed (when :space == 7) is
  ;; the "current" one, not an "other".
  (testing "two astronomers on 7 (one just landed) → pending-free-travel true"
    (let [s  (-> (state-with 31 {:astronomers [7 7]}) uncover-passive)
          s' (game/apply-passive s :alice :landing {:space 7})]
      (is (true? (get-in s' [:players :alice :pending-free-travel]))
          "the OTHER astronomer on 7 grants a free travel")))
  (testing "only the one just-landed on 7 (no other) → stays unset"
    (let [s  (-> (state-with 31 {:astronomers [7 3]}) uncover-passive)
          s' (game/apply-passive s :alice :landing {:space 7})]
      (is (nil? (get-in s' [:players :alice :pending-free-travel]))
          "the just-landed astronomer is the 'current' one, not an 'other'")))
  (testing "passive gated off (slot 0 covered) → no flag even with two on 7"
    (let [s  (state-with 31 {:astronomers [7 7]})  ;; NOT uncovered
          s' (game/apply-passive s :alice :landing {:space 7})]
      (is (nil? (get-in s' [:players :alice :pending-free-travel]))
          "covered slot 0 → passive does not fire"))))

;; =============================================================================
;; FIX 1 — bonus TRAVEL is REAL travel resolution (not a caravan teleport).
;; Every traversed hop must fire the genuine travel side-effects: own point
;; raider pickup+score, enemy-raider flips, temple visits, :river-crossed.
;; =============================================================================

(defn- board-state
  "A real board state: city-graph + active routes for player-count, with :alice
   on `board-id` and the given player overrides. Lets bonus-travel-to BFS the
   graph and travel-to-city fire route effects (instead of the no-city-graph
   fallback used by the minimal builders above)."
  [player-count board-id player-overrides]
  (-> (state-with board-id player-overrides
                  {:city-graph (game/city-graph player-count)
                   :routes (game/active-routes player-count)})))

(deftest fix1-bonus-travel-scores-own-point-raider-on-route-test
  ;; [21 1] "If you are in Eridu, travel anywhere" (choice = uruk). The card is
  ;; GATED on being in Eridu, so the caravan starts in Eridu with a face-up
  ;; (point) raider on the traversed road [:eridu :uruk]. A teleport would
  ;; silently skip it; real travel resolution scores it: +4 glory and the raider
  ;; returns to supply.
  (testing "bonus travel over a route with own point raider → +4 glory, raider returned"
    (let [s  (board-state 4 21 {:caravan :eridu
                                :raiders {[:eridu :uruk] [:point]}
                                :raiders-supply 5 :glory 0})
          s' (game/apply-bonus-with-choice s :alice 21 1 :uruk)]
      (is (= :uruk (get-in s' [:players :alice :caravan])) "caravan reached the destination")
      (is (= 4 (glory s')) "scored +4 glory for the own point raider on the traversed route")
      (is (nil? (get-in s' [:players :alice :raiders [:eridu :uruk]]))
          "the scored raider was removed (no longer on the route)")
      (is (= 6 (get-in s' [:players :alice :raiders-supply]))
          "the scored raider returned to supply (5 → 6)")))
  (testing "enemy raider on the traversed route is flipped to point (real resolution)"
    (let [s  (-> (board-state 4 21 {:caravan :eridu :raiders {}})
                 (assoc-in [:players :bob] {:raiders {[:eridu :uruk] [:raiding]}}))
          s' (game/apply-bonus-with-choice s :alice 21 1 :uruk)]
      (is (= [:point] (get-in s' [:players :bob :raiders [:eridu :uruk]]))
          "the enemy's raider on the route the caravan crossed was flipped to point"))))

;; =============================================================================
;; FIX 2 — [18 1] "Move a Magistrate across a river" honors route :type.
;; A river move must flip the raider on the RIVER edge and fire :river-crossed —
;; NOT trace a road-clockwise path (which flips the wrong raider, never a river).
;; =============================================================================

(deftest fix2-river-influence-flips-river-edge-raider-and-fires-river-crossed-test
  ;; Magistrate at uruk; dest = nippur (the RIVER edge [:nippur :uruk]). Place an
  ;; enemy :raiding raider on BOTH the river edge AND a uruk ROAD edge
  ;; ([:babylon :uruk]). The river move must flip the RIVER raider only.
  (testing "river move flips the river-edge raider, leaves the road raider untouched"
    (let [s  (-> (board-state 4 18 {:caravan :eridu})
                 (assoc :magistrates {:m1 :uruk})
                 (assoc-in [:players :bob]
                           {:raiders {[:nippur :uruk] [:raiding]    ;; river edge
                                      [:babylon :uruk] [:raiding]}}));; road edge
          s' (game/perform-river-influence s :alice :nippur)]
      (is (= :nippur (get-in s' [:magistrates :m1])) "magistrate moved across the river")
      (is (= [:point] (get-in s' [:players :bob :raiders [:nippur :uruk]]))
          "the RIVER-edge raider was flipped (the bug: it used to flip a road raider)")
      (is (= [:raiding] (get-in s' [:players :bob :raiders [:babylon :uruk]]))
          "the ROAD-edge raider is NOT touched by a river move")))
  (testing ":river-crossed passive fires for the mover (board 3 → +1 gem)"
    ;; Board 3 slot 0 :river-crossed grants a gem; a road influence would never
    ;; fire it. Proves the typed (river) edge triggered the river-crossing passive.
    (let [s  (-> (board-state 4 3 {:caravan :eridu :resources {:tools 0 :pottery 0 :gold 0 :gems 0}})
                 uncover-passive
                 (assoc :magistrates {:m1 :uruk}))
          s' (game/perform-river-influence s :alice :nippur)]
      (is (= 1 (res s' :gems))
          "river-crossed passive fired (board 3: +1 gem) — a road move never would")))
  (testing "[18 1] full arm: human picks a river destination → river raider flips"
    (let [s  (-> (board-state 4 18 {:caravan :eridu})
                 (assoc :magistrates {:m1 :uruk})
                 (assoc-in [:players :bob] {:raiders {[:nippur :uruk] [:raiding]}}))
          s' (game/apply-bonus-with-choice s :alice 18 1 :nippur)]
      (is (= :nippur (get-in s' [:magistrates :m1])) "magistrate moved across the river")
      (is (= [:point] (get-in s' [:players :bob :raiders [:nippur :uruk]]))
          "[18 1] flipped the river-edge raider via real typed movement")))
  (testing "no magistrate one river edge from dest → no-op"
    (let [s  (-> (board-state 4 18 {:caravan :eridu})
                 (assoc :magistrates {:m1 :kish}))  ;; kish has no river edge to nippur
          s' (game/perform-river-influence s :alice :nippur)]
      (is (= :kish (get-in s' [:magistrates :m1])) "magistrate not on a river edge to dest → unchanged"))))

;; =============================================================================
;; FIX 3 — eligible-cities-for-filter covers ALL state-dependent :pick-city
;; filters so the WS surfaces a concrete picker for each (was only 2 of them).
;; =============================================================================

(deftest fix3-eligible-cities-for-filter-covers-all-filters-test
  (testing ":adjacent-to-raider → cities at either end of the player's raider routes"
    (let [s  (board-state 4 13 {:caravan :eridu
                                :raiders {[:babylon :uruk] [:raiding]
                                          [:nippur :uruk] [:point]}})
          cs (set (game/eligible-cities-for-filter s :alice :adjacent-to-raider))]
      (is (= #{:babylon :uruk :nippur} cs)
          "the union of both endpoints of every route the player has a raider on")))
  (testing ":adjacent → the caravan city's graph neighbours"
    (let [s  (board-state 4 27 {:caravan :uruk})
          cs (set (game/eligible-cities-for-filter s :alice :adjacent))]
      (is (= #{:babylon :eridu :lagash :nippur} cs) "uruk's neighbours")))
  (testing ":magistrate → cities hosting a magistrate"
    (let [s  (-> (board-state 4 30 {:caravan :eridu}) (assoc :magistrates {:m1 :kish :m2 :uruk}))
          cs (set (game/eligible-cities-for-filter s :alice :magistrate))]
      (is (= #{:kish :uruk} cs) "the set of magistrate cities")))
  (testing ":any → every city in the graph"
    (let [s  (board-state 4 21 {:caravan :eridu})
          cs (set (game/eligible-cities-for-filter s :alice :any))]
      (is (= (set (keys (game/city-graph 4))) cs) "all cities")))
  (testing ":magistrate-and-my-temple → cities with my temple AND a magistrate"
    (let [s  (-> (board-state 4 34 {:caravan :eridu :temples {:kish [:face-up] :uruk [:face-up]}})
                 (assoc :magistrates {:m1 :kish}))
          cs (set (game/eligible-cities-for-filter s :alice :magistrate-and-my-temple))]
      (is (= #{:kish} cs) "only kish has both my temple and a magistrate")))
  (testing ":magistrate-river → cities one river edge from a magistrate"
    (let [s  (-> (board-state 4 18 {:caravan :eridu}) (assoc :magistrates {:m1 :uruk}))
          cs (set (game/eligible-cities-for-filter s :alice :magistrate-river))]
      ;; uruk's river edges: uruk↔nippur, uruk↔lagash
      (is (= #{:nippur :lagash} cs) "river-reachable destinations from the magistrate at uruk"))))

;; =============================================================================
;; Designer-reported bug fixes (from the "PRIEST MASTER" game)
;; =============================================================================

(deftest board-11-slot-0-feat-claim-glory-passive-test
  ;; Bug 1: the [11 0] passive ("score Glory = Leader level when you meet a
  ;; contest") never fired on a HUMAN's claim — the human claim path omitted
  ;; apply-passive. Pin the passive itself; the WS path now calls it.
  (testing "claiming a contest with slot 0 uncovered scores glory = Leader level"
    (let [s  (-> (state-with 11 {:roles {:merchant 1 :priest 1 :raider 1 :leader 3}})
                 uncover-passive)
          s' (game/apply-passive s :alice :feat-claimed {:contest-id :M1 :slot 2})]
      (is (= 3 (glory s')) "leader level 3 → +3 glory")))
  (testing "no glory when slot 0 is still covered (player lacks the passive)"
    (let [s  (state-with 11 {:roles {:merchant 1 :priest 1 :raider 1 :leader 3}})
          s' (game/apply-passive s :alice :feat-claimed {:contest-id :M1})]
      (is (= 0 (glory s')) "passive not active → no glory"))))

(deftest board-11-slot-1-no-teleport-test
  ;; Bug 2a: "Place two Demand Tokens in Lagash. Gain matching resources" used to
  ;; bonus-travel-to and teleport the caravan. It must NOT move the caravan.
  (testing "placing demand tokens in Lagash does not move the caravan"
    (let [s  (state-with 11 {:caravan :kish}
                         {:demand-bag {:gold 3 :tools 2 :pottery 2 :gems 1}
                          :city-demands {:lagash []}})
          s' (game/apply-bonus-effect s :alice 11 1)]
      (is (= :kish (get-in s' [:players :alice :caravan])) "caravan stays at kish")
      (is (pos? (reduce + (vals (get-in s' [:players :alice :resources]))))
          "gained the matching goods")
      (is (seq (get-in s' [:city-demands :lagash])) "tokens were placed in Lagash"))))

(deftest bonus-sell-in-magistrate-glory-at-sell-city-test
  ;; Bug 2b: a sell-at-a-distance must earn the magistrate glory bonus at the
  ;; SELL city, not the caravan.
  (let [sell-in #'eridu.game/bonus-sell-in]
    (testing "magistrate at the sell city → leader-level glory bonus"
      (let [s  (state-with 11 {:caravan :kish
                               :roles {:merchant 2 :priest 1 :raider 1 :leader 5}
                               :resources {:gold 1 :tools 0 :pottery 0 :gems 0}}
                           {:city-demands {:lagash [:gold]} :magistrates {0 :lagash}})
            s' (sell-in s :alice :lagash)]
        (is (pos? (amity s')) "scored merchant amity for the sell")
        (is (= (get game/leader-bonus 5) (glory s'))
            "magistrate at Lagash → +leader-bonus(5)=3 glory, even at a distance")))
    (testing "no magistrate at the sell city → no glory bonus"
      (let [s  (state-with 11 {:roles {:merchant 2 :priest 1 :raider 1 :leader 5}
                               :resources {:gold 1 :tools 0 :pottery 0 :gems 0}}
                           {:city-demands {:lagash [:gold]} :magistrates {0 :kish}})
            s' (sell-in s :alice :lagash)]
        (is (zero? (glory s')) "magistrate elsewhere → no sell glory")))))

;; =============================================================================
;; Bot/human path MERGE — one claim primitive, no drift (systemic fix)
;; =============================================================================

(deftest apply-feat-claim-shared-primitive-test
  (testing "apply-feat-claim! records claim + wild points + fires the :feat-claimed passive"
    (let [s  (-> (state-with 11 {:roles {:merchant 1 :priest 1 :raider 1 :leader 3}})
                 (assoc :contests [{:id :M1}]))
          ;; claim M1 uncovering slot 0 (the passive); slot 0 has no instant arm → identity
          s' (game/apply-feat-claim! s :alice :M1 0 3 identity)]
      (is (= [:alice] (get-in s' [:contest-claims :M1])) "claim recorded")
      (is (= 3 (get-in s' [:players :alice :wild-points])) "wild points added")
      (is (= :uncovered (get-in s' [:players :alice :bonus-board 0])) "slot uncovered")
      (is (= 3 (glory s')) "slot-0 passive fired: +Leader-level(3) glory"))))

(deftest bot-resolves-interactive-bonus-like-human-test
  ;; The merge's core invariant: a bot resolves an interactive bonus slot through
  ;; the SAME apply-bonus-with-choice dispatch a human uses (with a scored pick),
  ;; NOT the old apply-bonus-effect nil-default. So bot and human reach the same
  ;; state for the same pick — drift is structurally impossible.
  (let [base (-> (state-with 2 {:caravan :kish :temples-supply 7})
                 (assoc :magistrates {0 :uruk}))
        ;; [2 3] = "place a temple in a magistrate city" (:pick-city :magistrate)
        bot-state   (game/bot-resolve-bonus base :alice 2 3)
        human-state (game/apply-bonus-with-choice base :alice 2 3 :uruk)]
    (testing "bot picks the (only) magistrate city and actually places the temple"
      (is (game/has-temple? (get-in bot-state [:players :alice]) :uruk)
          "bot placed a real temple — not a nil-default no-op"))
    (testing "bot and human reach the same placement"
      (is (= (get-in bot-state   [:players :alice :temples])
             (get-in human-state [:players :alice :temples]))))))


;; =============================================================================
;; Faithful-effect fixes (C3/C4) — forced-scenario regressions per slot
;; =============================================================================

(deftest board-3-slot-4-travel-then-sell-test
  ;; "Take a travel action then a Sell action." Old arm teleported to Eridu and
  ;; granted a flat +2 amity. Fix: travel to the chosen adjacent city, then a real
  ;; sell there (merchant-level amity, demand token consumed) — no flat amity.
  (testing "travel to chosen adjacent city + real sell there (no flat +2)"
    (let [s  (board-state 4 3 {:caravan :uruk
                               :roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                               :resources {:gold 1 :tools 0 :pottery 0 :gems 0}
                               :amity 0})
          s  (assoc-in s [:city-demands :eridu] [:gold])
          s' (game/apply-bonus-with-choice s :alice 3 4 :eridu)]
      (is (= :eridu (get-in s' [:players :alice :caravan])) "travelled to the chosen adjacent city")
      (is (= 0 (res s' :gold)) "the matching good was sold (spent)")
      (is (= 2 (amity s')) "merchant level 1 → +2 amity for the sell (NOT a flat +2 plus a sell)")
      (is (= [] (get-in s' [:city-demands :eridu])) "demand token consumed at the sell city")))
  (testing "no choice (auto/bot) → stay at caravan, sell there"
    (let [s  (board-state 4 3 {:caravan :uruk
                               :roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                               :resources {:pottery 1 :tools 0 :gold 0 :gems 0}
                               :amity 0})
          s  (assoc-in s [:city-demands :uruk] [:pottery])
          s' (game/apply-bonus-effect s :alice 3 4)]
      (is (= :uruk (get-in s' [:players :alice :caravan])) "no move (default = stay at caravan)")
      (is (= 0 (res s' :pottery)) "sold at the current city")
      (is (= 2 (amity s')) "merchant amity for the sell only")))
  (testing "nothing sellable at the destination → travel only, no amity"
    (let [s  (board-state 4 3 {:caravan :uruk
                               :resources {:tools 0 :pottery 0 :gold 0 :gems 0}
                               :amity 0})
          s  (assoc-in s [:city-demands :eridu] [:gold])
          s' (game/apply-bonus-with-choice s :alice 3 4 :eridu)]
      (is (= :eridu (get-in s' [:players :alice :caravan])) "still travelled")
      (is (= 0 (amity s')) "no matching good → sell no-ops, no flat amity"))))

(deftest board-19-slot-2-sell-two-pottery-cities-test
  ;; "Sell to two cities that demand Pottery (you don't have to be there)." Old arm
  ;; granted +1 pottery and a flat +3 amity. Fix: two real sells (no move) at two
  ;; cities whose live demands include :pottery.
  (testing "two pottery-demanding cities → two real sells, no move, no pottery gain"
    (let [s  (state-with 19 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             :resources {:pottery 2 :tools 0 :gold 0 :gems 0}
                             :amity 0 :caravan :kish}
                         {:city-demands {:lagash [:pottery] :uruk [:pottery]
                                         :babylon [:gold]}})
          s' (game/apply-bonus-effect s :alice 19 2)]
      (is (= 0 (res s' :pottery)) "two pottery spent (NOT a +1 pottery gain)")
      (is (= 4 (amity s')) "two merchant-level(1) sells = 2+2 = 4 amity (NOT a flat +3)")
      (is (= :kish (get-in s' [:players :alice :caravan])) "no caravan move (sell at a distance)")
      (is (= [] (get-in s' [:city-demands :lagash])) "pottery demand consumed at lagash")
      (is (= [] (get-in s' [:city-demands :uruk])) "pottery demand consumed at uruk")
      (is (= [:gold] (get-in s' [:city-demands :babylon])) "non-pottery city untouched")))
  (testing "only one pottery city → only one sell happens"
    (let [s  (state-with 19 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             :resources {:pottery 2 :tools 0 :gold 0 :gems 0}
                             :amity 0}
                         {:city-demands {:lagash [:pottery] :babylon [:gold]}})
          s' (game/apply-bonus-effect s :alice 19 2)]
      (is (= 1 (res s' :pottery)) "only one pottery spent (one city)")
      (is (= 2 (amity s')) "one sell = +2 amity")))
  (testing "no pottery held → sells no-op (no amity, demands intact)"
    (let [s  (state-with 19 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             :resources {:pottery 0 :tools 0 :gold 0 :gems 0}
                             :amity 0}
                         {:city-demands {:lagash [:pottery] :uruk [:pottery]}})
          s' (game/apply-bonus-effect s :alice 19 2)]
      (is (= 0 (amity s')) "no good to sell → no amity")
      (is (= [:pottery] (get-in s' [:city-demands :lagash])) "demand intact"))))

(deftest board-23-slot-2-sell-twice-to-eridu-test
  ;; "Sell twice to Eridu (you don't need to be there)." Old arm teleported to
  ;; Eridu and granted a flat +4 amity. Fix: two real sells at :eridu, no move.
  (testing "two matching goods at Eridu → two real sells, no caravan move"
    (let [s  (state-with 23 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             :resources {:gold 1 :gems 1 :tools 0 :pottery 0}
                             :amity 0 :caravan :kish}
                         {:city-demands {:eridu [:gold :gems]}})
          s' (game/apply-bonus-effect s :alice 23 2)]
      (is (= :kish (get-in s' [:players :alice :caravan])) "NO teleport to Eridu (sell at a distance)")
      (is (= 0 (res s' :gold)) "first matching good sold")
      (is (= 0 (res s' :gems)) "second matching good sold")
      (is (= 4 (amity s')) "two merchant-level(1) sells = 2+2 = 4 amity (now real, not a flat +4)")
      (is (= [] (get-in s' [:city-demands :eridu])) "both demand tokens consumed")))
  (testing "only one matching good → only the first sell scores"
    (let [s  (state-with 23 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             :resources {:gold 1 :tools 0 :pottery 0 :gems 0}
                             :amity 0 :caravan :kish}
                         {:city-demands {:eridu [:gold :gems]}})
          s' (game/apply-bonus-effect s :alice 23 2)]
      (is (= 0 (res s' :gold)) "the one matching good sold")
      (is (= 2 (amity s')) "one sell = +2 amity (second sell no-ops, nothing to sell)")
      (is (= [:gems] (get-in s' [:city-demands :eridu])) "the unmatched demand remains")))
  (testing "magistrate at Eridu adds leader-bonus glory per sell"
    (let [s  (state-with 23 {:roles {:merchant 1 :priest 1 :raider 1 :leader 3}
                             :resources {:gold 1 :gems 1 :tools 0 :pottery 0}
                             :amity 0 :glory 0 :caravan :kish}
                         {:city-demands {:eridu [:gold :gems]} :magistrates {0 :eridu}})
          s' (game/apply-bonus-effect s :alice 23 2)]
      (is (= (* 2 (get game/leader-bonus 3)) (glory s'))
          "two sells at a magistrate city → 2 × leader-bonus(3)=2 glory = 4"))))

(deftest board-29-slot-2-travel-then-may-sell-test
  ;; "Take a travel action then you may take a sell action." Old arm granted a flat
  ;; +3 amity and no travel. Fix: travel to chosen adjacent city, then a real sell.
  (testing "travel to chosen adjacent city + real sell there (no flat +3)"
    (let [s  (board-state 4 29 {:caravan :uruk
                                :roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                                :resources {:gold 1 :tools 0 :pottery 0 :gems 0}
                                :amity 0})
          s  (assoc-in s [:city-demands :eridu] [:gold])
          s' (game/apply-bonus-with-choice s :alice 29 2 :eridu)]
      (is (= :eridu (get-in s' [:players :alice :caravan])) "travelled to the chosen adjacent city")
      (is (= 0 (res s' :gold)) "the matching good was sold")
      (is (= 2 (amity s')) "merchant level 1 → +2 amity for the sell (NOT a flat +3)")
      (is (= [] (get-in s' [:city-demands :eridu])) "demand token consumed")))
  (testing "no choice (auto) → stay at caravan and sell there"
    (let [s  (board-state 4 29 {:caravan :uruk
                                :roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                                :resources {:pottery 1 :tools 0 :gold 0 :gems 0}
                                :amity 0})
          s  (assoc-in s [:city-demands :uruk] [:pottery])
          s' (game/apply-bonus-effect s :alice 29 2)]
      (is (= :uruk (get-in s' [:players :alice :caravan])) "default = stay at caravan")
      (is (= 2 (amity s')) "sold at the current city")))
  (testing "'you may sell': nothing sellable → travel only, no amity"
    (let [s  (board-state 4 29 {:caravan :uruk
                                :resources {:tools 0 :pottery 0 :gold 0 :gems 0}
                                :amity 0})
          s  (assoc-in s [:city-demands :eridu] [:gold])
          s' (game/apply-bonus-with-choice s :alice 29 2 :eridu)]
      (is (= :eridu (get-in s' [:players :alice :caravan])) "still travelled")
      (is (= 0 (amity s')) "no matching good → sell no-ops, no flat amity"))))

(deftest board-6-slot-3-sell-babylon-double-points-test
  (testing "two matching goods → two sells in Babylon (amity), caravan unchanged"
    (let [s  (state-with 6 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                            :resources {:tools 0 :pottery 0 :gold 0 :gems 2}
                            :amity 0 :glory 0 :caravan :uruk}
                         {:city-demands {:babylon [:gems :gems]}})
          s' (game/apply-bonus-effect s :alice 6 3)]
      (is (= 4 (amity s')) "merchant lv1 = 2 amity each × 2 sells = 4 (double)")
      (is (= 0 (glory s')) "no magistrate in Babylon → no glory")
      (is (= 0 (res s' :gems)) "both goods spent")
      (is (= [] (get-in s' [:city-demands :babylon])) "both demands consumed")
      (is (= :uruk (get-in s' [:players :alice :caravan])) "no caravan move")))
  (testing "only one matching good → one sell, second no-ops"
    (let [s  (state-with 6 {:resources {:tools 0 :pottery 0 :gold 0 :gems 1}
                            :amity 0 :caravan :uruk}
                         {:city-demands {:babylon [:gems :gems]}})
          s' (game/apply-bonus-effect s :alice 6 3)]
      (is (= 2 (amity s')) "only one sell possible → +2 amity")
      (is (= [:gems] (get-in s' [:city-demands :babylon])) "one demand left"))))

(deftest board-9-slot-4-sell-magistrate-city-then-temple-test
  (testing "caravan elsewhere → sell at magistrate city, NO temple, NO move"
    (let [s  (state-with 9 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                            :resources {:tools 0 :pottery 0 :gold 0 :gems 1}
                            :amity 0 :glory 0 :caravan :uruk :temples {}}
                         {:magistrates {:mag-0 :babylon}
                          :city-demands {:babylon [:gems]}})
          s' (game/apply-bonus-with-choice s :alice 9 4 :babylon)]
      (is (= 2 (amity s')) "merchant lv1 sell = +2 amity")
      (is (= 1 (glory s')) "Babylon has a magistrate → leader-bonus lv1 = +1 glory")
      (is (= 0 (res s' :gems)) "good spent")
      (is (= :uruk (get-in s' [:players :alice :caravan])) "NO teleport")
      (is (empty? (temples-of s')) "NOT in the city → no temple action")))
  (testing "caravan IN the magistrate city → sell AND temple"
    (let [s  (state-with 9 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                            :resources {:tools 0 :pottery 0 :gold 0 :gems 1}
                            :amity 0 :glory 0 :caravan :babylon :temples {}}
                         {:magistrates {:mag-0 :babylon}
                          :city-demands {:babylon [:gems]}})
          s' (game/apply-bonus-with-choice s :alice 9 4 :babylon)]
      (is (= 2 (amity s')) "sell amity")
      (is (= 1 (glory s')) "magistrate glory bonus")
      (is (pos? (count (get-in s' [:players :alice :temples :babylon] [])))
          "caravan IS in the city → temple placed in Babylon"))))

(deftest board-11-slot-2-sell-lagash-double-glory-test
  ;; Designer-confirmed: ONE sell at Lagash scoring DOUBLE the merchant glory
  ;; (one good + one token), no caravan move.
  (testing "one sell at Lagash → double merchant glory, one good/token, no move"
    (let [s  (state-with 11 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             :resources {:tools 0 :pottery 2 :gold 0 :gems 0}
                             :amity 0 :glory 0 :caravan :uruk}
                          {:city-demands {:lagash [:pottery :pottery]}})
          s' (game/apply-bonus-effect s :alice 11 2)]
      (is (= 4 (glory s')) "merchant lv1 glory 2 × 2 (double) = 4")
      (is (= 0 (amity s')) "scored as glory, NOT amity")
      (is (= 1 (res s' :pottery)) "exactly ONE good spent (single sell)")
      (is (= [:pottery] (get-in s' [:city-demands :lagash])) "exactly ONE demand consumed")
      (is (= :uruk (get-in s' [:players :alice :caravan])) "no caravan move")))
  (testing "magistrate in Lagash adds leader-bonus glory once on the doubled sell"
    (let [s  (state-with 11 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             :resources {:tools 0 :pottery 1 :gold 0 :gems 0}
                             :amity 0 :glory 0 :caravan :uruk}
                          {:magistrates {:mag-0 :lagash}
                           :city-demands {:lagash [:pottery]}})
          s' (game/apply-bonus-effect s :alice 11 2)]
      (is (= 5 (glory s')) "doubled merchant glory (2×2=4) + leader-bonus lv1 (1) = 5")
      (is (= 0 (amity s')) "no amity"))))

(deftest board-12-slot-3-human-path-merchant-and-current-city-glory-test
  (testing "human path increases merchant + REAL glory sell in caravan city; chosen city ignored"
    (let [s  (state-with 12 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             :resources {:tools 0 :pottery 0 :gold 0 :gems 1}
                             :amity 0 :glory 0 :caravan :uruk}
                          {:magistrates {:mag-0 :babylon}
                           :city-demands {:uruk [:gems] :babylon [:gems]}})
          s' (game/apply-bonus-with-choice s :alice 12 3 :babylon)]
      (is (= 2 (role s' :merchant)) "merchant increased (level 1→2 is free)")
      (is (= 3 (glory s')) "merchant-level points scored as GLORY for the caravan-city sell")
      (is (= 0 (amity s')) "amity NOT scored — this is a glory sell, not an amity sell")
      (is (= 0 (res s' :gems)) "the good was spent")
      (is (= [] (get-in s' [:city-demands :uruk])) "caravan-city demand consumed")
      (is (= [:gems] (get-in s' [:city-demands :babylon]))
          "the chosen city is ignored — its demand vector is unchanged")))
  (testing "no sellable good in caravan city → merchant increase only, no glory"
    (let [s  (state-with 12 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                             :resources {:tools 0 :pottery 0 :gold 0 :gems 0}
                             :amity 0 :glory 0 :caravan :uruk}
                          {:city-demands {:uruk [:gems]}})
          s' (game/apply-bonus-effect s :alice 12 3)]
      (is (= 2 (role s' :merchant)) "merchant still increased")
      (is (= 0 (glory s')) "no sellable good → no glory")
      (is (= 0 (amity s')) "no amity"))))

(deftest board-17-slot-4-sell-caravan-city-for-glory-test
  (testing "sellable good in caravan city → glory sell"
    (let [s  (state-with 17 {:roles {:merchant 2 :priest 1 :raider 1 :leader 1}
                             :resources {:tools 0 :pottery 0 :gold 0 :gems 1}
                             :amity 0 :glory 0 :caravan :uruk}
                          {:city-demands {:uruk [:gems]}})
          s' (game/apply-bonus-effect s :alice 17 4)]
      (is (= 3 (glory s')) "merchant lv2 = 3 points scored as glory")
      (is (= 0 (amity s')) "scored as glory, NOT amity")
      (is (= 0 (res s' :gems)) "good spent")
      (is (= [] (get-in s' [:city-demands :uruk])) "demand consumed")))
  (testing "nothing sellable in caravan city → no-op"
    (let [s  (state-with 17 {:resources {:tools 0 :pottery 0 :gold 0 :gems 0}
                             :amity 0 :glory 0 :caravan :uruk}
                          {:city-demands {:uruk [:gems]}})
          s' (game/apply-bonus-effect s :alice 17 4)]
      (is (= 0 (glory s')) "no good → no glory")
      (is (= 0 (amity s')) "no amity")
      (is (= [:gems] (get-in s' [:city-demands :uruk])) "demand untouched"))))

(deftest board-8-slot-2-places-nippur-babylon-then-sells-in-city-test
  ;; "Place one random Demand Token in Nippur and Babylon each. Then you may sell
  ;; once in your city." (slot index 2)
  (testing "one token to Nippur, one to Babylon, and a real sell in the caravan city"
    (let [s  (state-with 8 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                            :resources {:tools 0 :pottery 0 :gold 0 :gems 1}
                            :amity 0 :glory 0 :caravan :uruk}
                         {:demand-bag (game/full-demand-bag)
                          :city-demands {:uruk [:gems]}})
          s' (game/apply-bonus-effect s :alice 8 2)]
      (is (= 1 (count (get-in s' [:city-demands :nippur]))) "one demand token placed in Nippur")
      (is (= 1 (count (get-in s' [:city-demands :babylon]))) "one demand token placed in Babylon")
      (is (= 26 (game/bag-total (:demand-bag s'))) "two tokens drawn from the 28-token bag")
      (is (= 0 (res s' :gems)) "the gem was spent on the sell")
      (is (= [] (get-in s' [:city-demands :uruk])) "the caravan-city demand was consumed by the sell")
      (is (= 2 (amity s')) "merchant L1 sell scores +2 amity (no flat-3 proxy)")))
  (testing "no-op sell when nothing in the caravan city is sellable (still places tokens)"
    (let [s  (state-with 8 {:resources {:tools 0 :pottery 0 :gold 0 :gems 0}
                            :amity 0 :caravan :uruk}
                         {:demand-bag (game/full-demand-bag)
                          :city-demands {:uruk [:gems]}})
          s' (game/apply-bonus-effect s :alice 8 2)]
      (is (= 1 (count (get-in s' [:city-demands :nippur]))) "Nippur still got a token")
      (is (= 1 (count (get-in s' [:city-demands :babylon]))) "Babylon still got a token")
      (is (= [:gems] (get-in s' [:city-demands :uruk])) "no sellable good → demand untouched")
      (is (= 0 (amity s')) "no amity when nothing sells"))))

(deftest board-16-slot-4-places-two-on-current-city-then-sells-test
  ;; "Put two random demand tokens on the city you are in. You may take Sell
  ;; action." (slot index 4)
  (testing "two tokens land on the caravan city and a real sell fires there"
    (let [s  (state-with 16 {:roles {:merchant 2 :priest 1 :raider 1 :leader 1}
                             :resources {:tools 0 :pottery 0 :gold 0 :gems 1}
                             :amity 0 :caravan :nippur}
                          {:demand-bag (game/full-demand-bag)
                           :city-demands {:nippur [:gems]}})
          s' (game/apply-bonus-effect s :alice 16 4)]
      (is (= 2 (count (get-in s' [:city-demands :nippur])))
          "3 demands (1 seeded + 2 placed) minus 1 consumed by the sell")
      (is (= 26 (game/bag-total (:demand-bag s'))) "two tokens drawn from the bag")
      (is (= 0 (res s' :gems)) "the gem was spent on the sell")
      (is (= 3 (amity s')) "merchant L2 sell scores +3 amity (no flat tools/+3 proxy)")
      (is (= 0 (res s' :tools)) "no spurious free tools granted")))
  (testing "tokens still placed even when nothing is sellable"
    (let [s  (state-with 16 {:resources {:tools 0 :pottery 0 :gold 0 :gems 0}
                             :amity 0 :caravan :nippur}
                          {:demand-bag (game/full-demand-bag) :city-demands {}})
          s' (game/apply-bonus-effect s :alice 16 4)]
      (is (= 2 (count (get-in s' [:city-demands :nippur]))) "two tokens placed on the current city")
      (is (= 0 (amity s')) "no sell, no amity"))))

(deftest board-8-slot-3-gain-three-then-sell-test
  (testing "grants gold/gems/pottery, then sells a freshly-gained good"
    (let [s  (state-with 8 {:resources {:tools 0 :pottery 0 :gold 0 :gems 0}
                            :amity 0 :caravan :uruk}
                         {:city-demands {:uruk [:gems]}})
          s' (game/apply-bonus-effect s :alice 8 3)]
      (is (= 1 (res s' :gold))    "gold gain kept")
      (is (= 1 (res s' :pottery)) "pottery gain kept")
      (is (= 0 (res s' :gems))    "the gained gem was spent on the sell")
      (is (= 2 (amity s'))        "merchant-lv1 sell scored +2 amity")
      (is (= [] (get-in s' [:city-demands :uruk])) "the gems demand was consumed")
      (is (= [:gems] (get-in s' [:players :alice :demand-tokens]))
          "the fulfilled demand token moved to the player")))
  (testing "no matching demand → goods kept, no sell"
    (let [s  (state-with 8 {:amity 0 :caravan :uruk}
                         {:city-demands {:uruk [:tools]}})
          s' (game/apply-bonus-effect s :alice 8 3)]
      (is (= 1 (res s' :gold)) "gold kept")
      (is (= 1 (res s' :gems)) "gems kept (no tools to sell)")
      (is (= 0 (amity s'))     "no sell happened"))))

(deftest board-26-slot-3-sell-then-temple-iff-tools-or-pottery-test
  (testing "sold POTTERY → temple placed in caravan city"
    (let [s  (state-with 26 {:resources {:pottery 1} :amity 0 :caravan :uruk}
                         {:city-demands {:uruk [:pottery]}})
          s' (game/apply-bonus-effect s :alice 26 3)]
      (is (= 2 (amity s')) "merchant-lv1 sell +2 amity")
      (is (= 0 (res s' :pottery)) "pottery spent")
      (is (game/has-temple? (get-in s' [:players :alice]) :uruk)
          "temple placed because tools/pottery was sold")))
  (testing "sold GEMS (not tools/pottery) → NO temple"
    (let [s  (state-with 26 {:resources {:gems 1} :amity 0 :caravan :uruk}
                         {:city-demands {:uruk [:gems]}})
          s' (game/apply-bonus-effect s :alice 26 3)]
      (is (= 2 (amity s')) "sell still happened")
      (is (not (game/has-temple? (get-in s' [:players :alice]) :uruk))
          "no temple — gems is neither tools nor pottery")))
  (testing "nothing sellable → no sell, no temple"
    (let [s  (state-with 26 {:resources {:pottery 0} :amity 0 :caravan :uruk}
                         {:city-demands {:uruk [:pottery]}})
          s' (game/apply-bonus-effect s :alice 26 3)]
      (is (= 0 (amity s')) "no sell")
      (is (not (game/has-temple? (get-in s' [:players :alice]) :uruk))
          "no temple when nothing sold"))))

(deftest board-28-slot-3-sell-gold-empty-then-place-demand-test
  (testing "empty city + gold → spend gold, +merchant amity, place 1 random demand"
    (let [s  (state-with 28 {:resources {:gold 1} :amity 0 :caravan :uruk}
                         {:city-demands {:uruk []}
                          :demand-bag (game/full-demand-bag)})
          s' (game/apply-bonus-effect s :alice 28 3)]
      (is (= 0 (res s' :gold)) "gold spent")
      (is (= 2 (amity s'))     "merchant-lv1 amity +2")
      (is (= 1 (count (get-in s' [:city-demands :uruk])))
          "exactly one random demand placed on the city")))
  (testing "city HAS demands → no-op (gold kept, no amity)"
    (let [s  (state-with 28 {:resources {:gold 1} :amity 0 :caravan :uruk}
                         {:city-demands {:uruk [:tools]}
                          :demand-bag (game/full-demand-bag)})
          s' (game/apply-bonus-effect s :alice 28 3)]
      (is (= 1 (res s' :gold)) "gold NOT spent")
      (is (= 0 (amity s'))     "no amity")
      (is (= [:tools] (get-in s' [:city-demands :uruk])) "demands unchanged")))
  (testing "no gold → no-op even on an empty city"
    (let [s  (state-with 28 {:resources {:gold 0} :amity 0 :caravan :uruk}
                         {:city-demands {:uruk []}
                          :demand-bag (game/full-demand-bag)})
          s' (game/apply-bonus-effect s :alice 28 3)]
      (is (= 0 (amity s')) "no amity")
      (is (empty? (get-in s' [:city-demands :uruk])) "no demand placed"))))

(deftest board-32-slot-1-sell-then-glory-per-fulfilled-demand-test
  (testing "sell adds a token → glory = total fulfilled demands AFTER the sell"
    (let [s  (state-with 32 {:resources {:tools 1} :glory 0 :amity 0 :caravan :uruk
                             :demand-tokens [:gold :gems]}
                         {:city-demands {:uruk [:tools]}})
          s' (game/apply-bonus-effect s :alice 32 1)]
      (is (= 0 (res s' :tools)) "tools sold")
      (is (= 2 (amity s'))      "merchant-lv1 sell +2 amity")
      (is (= 3 (count (get-in s' [:players :alice :demand-tokens])))
          "demand-tokens now 3 (2 prior + 1 from this sell)")
      (is (= 3 (glory s')) "glory = 3 fulfilled demands counted after the sell")))
  (testing "no sale → glory = pre-existing fulfilled demands only"
    (let [s  (state-with 32 {:resources {:gold 0} :glory 0 :caravan :uruk
                             :demand-tokens [:pottery]}
                         {:city-demands {:uruk [:gold]}})
          s' (game/apply-bonus-effect s :alice 32 1)]
      (is (= 1 (glory s')) "no new token; glory = the 1 prior fulfilled demand"))))

(deftest board-6-slot-4-raider-near-lagash-plus-two-tools-test
  ;; "Place a Raider adjacent to Lagash. Gain Tools, Tools."
  ;; Bug: old arm gave +2 tools but never placed the raider.
  (testing "deploys a raider on a free route touching Lagash AND grants +2 tools"
    (let [s  (board-state 4 6 {:raiders {} :raiders-supply 6
                               :resources {:tools 0 :pottery 0 :gold 0 :gems 0}})
          s' (game/apply-bonus-effect s :alice 6 4)
          rks (set (keys (raiders s')))]
      (is (= 2 (res s' :tools)) "gained Tools, Tools")
      (is (= 1 (count rks)) "exactly one raider placed")
      (is (some (fn [[a b]] (or (= a :lagash) (= b :lagash))) rks)
          "the placed raider is on a route adjacent to Lagash")
      (is (= 5 (get-in s' [:players :alice :raiders-supply]))
          "one raider drawn from supply"))))

(deftest board-26-slot-4-raider-adjacent-caravan-surround-temple-test
  ;; "Place a Raider adjacent to your city. If you surround it, you may place a
  ;; temple in it (even if you already have a temple there)."
  (testing "completing the surround of the caravan city places a temple there"
    (let [s  (board-state 4 26 {:caravan :eridu
                                :raiders {[:eridu :lagash] [:raiding]}
                                :raiders-supply 5 :temples {} :temples-supply 7})
          s' (game/apply-bonus-effect s :alice 26 4)]
      (is (= #{[:eridu :lagash] [:eridu :uruk]} (set (keys (raiders s'))))
          "second raider completed the surround of Eridu")
      (is (game/has-temple? (get-in s' [:players :alice]) :eridu)
          "a temple was placed in the surrounded caravan city")))
  (testing "no temple when the deploy does NOT complete a surround"
    (let [s  (board-state 4 26 {:caravan :uruk :raiders {}
                                :raiders-supply 6 :temples {} :temples-supply 7})
          s' (game/apply-bonus-effect s :alice 26 4)]
      (is (= 1 (count (raiders s'))) "one raider placed adjacent to Uruk")
      (is (not (game/has-temple? (get-in s' [:players :alice]) :uruk))
          "Uruk is not surrounded → no temple"))))

(deftest board-29-slot-4-temple-in-each-surrounded-city-test
  ;; "Place a Temple in each city surrounded by your Raiders (even if you have a
  ;; Temple there)." Bug: old arm only templed the caravan city.
  (testing "templed EVERY surrounded city, not just one"
    (let [s  (board-state 4 29 {:raiders {[:eridu :lagash]   [:raiding]
                                          [:eridu :uruk]     [:raiding]
                                          [:nineveh :samarra] [:raiding]
                                          [:babylon :nineveh] [:raiding]}
                                :temples {} :temples-supply 7})
          s' (game/apply-bonus-effect s :alice 29 4)
          pd (get-in s' [:players :alice])]
      (is (game/has-temple? pd :eridu) "temple in surrounded Eridu")
      (is (game/has-temple? pd :nineveh) "temple in surrounded Nineveh")
      (is (= 2 (count (game/all-temple-states pd)))
          "exactly two temples placed (one per surrounded city)")))
  (testing "duplicate-allowed: temples even where one already exists"
    (let [s  (board-state 4 29 {:raiders {[:eridu :lagash] [:raiding]
                                          [:eridu :uruk]   [:raiding]}
                                :temples {:eridu [:face-up]} :temples-supply 7})
          s' (game/apply-bonus-effect s :alice 29 4)]
      (is (= 2 (count (get-in s' [:players :alice :temples :eridu])))
          "a SECOND temple was added to the already-templed surrounded city")))
  (testing "no surrounded city → no temple placed"
    (let [s  (board-state 4 29 {:raiders {[:eridu :lagash] [:raiding]}
                                :temples {} :temples-supply 7})
          s' (game/apply-bonus-effect s :alice 29 4)]
      (is (zero? (count (game/all-temple-states (get-in s' [:players :alice]))))
          "Eridu only half-surrounded → no temple"))))

(deftest board-32-slot-3-raider-on-each-double-temple-route-test
  ;; "Place a raider in each route that has one of your Temples in both cities."
  ;; Bug: old arm placed on only the FIRST eligible route (and passed player-count
  ;; where a state was expected, so it saw zero routes).
  (testing "places on EVERY route whose both endpoints hold your temple"
    (let [s  (board-state 4 32 {:temples {:eridu  [:face-up]
                                          :lagash [:face-up]
                                          :uruk   [:face-up]}
                                :raiders {} :raiders-supply 6 :roles {:raider 5 :merchant 1 :priest 1 :leader 1}})
          s' (game/apply-bonus-effect s :alice 32 3)
          rks (set (keys (raiders s')))]
      (is (= #{[:eridu :lagash] [:eridu :uruk] [:lagash :uruk]} rks)
          "a raider on each of the three double-temple routes")))
  (testing "no eligible route → no raiders"
    (let [s  (board-state 4 32 {:temples {:eridu [:face-up]}
                                :raiders {} :raiders-supply 6})
          s' (game/apply-bonus-effect s :alice 32 3)]
      (is (empty? (raiders s')) "only one templed city → no double-temple route"))))

(deftest board-20-slot-4-takes-goods-from-astronomer-spaces-test
  (testing "two astronomers (spaces 1 and 2) -> their four :take goods"
    (let [s  (state-with 20 {:astronomers [1 2]
                             :resources {:tools 0 :pottery 0 :gold 0 :gems 0}})
          s' (game/apply-bonus-effect s :alice 20 4)]
      (is (= 1 (res s' :gems))) (is (= 1 (res s' :tools)))
      (is (= 1 (res s' :gold))) (is (= 1 (res s' :pottery)))))
  (testing "capped at four goods even with more astronomer spaces"
    (let [s  (state-with 20 {:astronomers [1 2 3]
                             :resources {:tools 0 :pottery 0 :gold 0 :gems 0}})
          s' (game/apply-bonus-effect s :alice 20 4)]
      (is (= 4 (reduce + (vals (get-in s' [:players :alice :resources])))))))
  (testing "astronomer on space 7 (no :take) contributes nothing"
    (let [s  (state-with 20 {:astronomers [7]
                             :resources {:tools 0 :pottery 0 :gold 0 :gems 0}})
          s' (game/apply-bonus-effect s :alice 20 4)]
      (is (= 0 (reduce + (vals (get-in s' [:players :alice :resources]))))))))

(deftest board-14-slot-2-magistrate-to-uruk-then-uruk-demands-test
  (testing "magistrate moves to uruk and player gains one good per uruk demand"
    (let [s  (-> (board-state 4 14 {:roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                                    :resources {:tools 0 :pottery 0 :gold 0 :gems 0}})
                 (assoc :magistrates {:m1 :eridu}
                        :city-demands {:uruk [:gold :gems]}))
          s' (game/apply-bonus-effect s :alice 14 2)]
      (is (= :uruk (get-in s' [:magistrates :m1])))
      (is (= 1 (res s' :gold))) (is (= 1 (res s' :gems)))
      (is (= 0 (res s' :tools))))))

(deftest board-24-slot-4-good-per-demand-in-magistrate-cities-test
  (testing "one good per demand token across all magistrate cities (no cap)"
    (let [s  (-> (state-with 24 {:resources {:tools 0 :pottery 0 :gold 0 :gems 0}
                                 :demand-tokens [:tools :tools :tools]})
                 (assoc :magistrates {:m1 :kish :m2 :uruk}
                        :city-demands {:kish [:gold]
                                       :uruk [:gems :pottery]
                                       :lagash [:tools]}))
          s' (game/apply-bonus-effect s :alice 24 4)]
      (is (= 1 (res s' :gold))) (is (= 1 (res s' :gems)))
      (is (= 1 (res s' :pottery))) (is (= 0 (res s' :tools)))
      (is (= 3 (reduce + (vals (get-in s' [:players :alice :resources]))))))))

(deftest board-21-slot-1-gated-on-eridu-test
  (testing "caravan NOT in eridu -> no-op (no travel)"
    (let [s  (board-state 4 21 {:caravan :babylon})
          s' (game/apply-bonus-with-choice s :alice 21 1 :uruk)]
      (is (= :babylon (get-in s' [:players :alice :caravan])))))
  (testing "caravan IN eridu, explicit choice -> travels there"
    (let [s  (board-state 4 21 {:caravan :eridu})
          s' (game/apply-bonus-with-choice s :alice 21 1 :uruk)]
      (is (= :uruk (get-in s' [:players :alice :caravan])))))
  (testing "caravan IN eridu, bot default -> a meaningful city other than eridu"
    (let [s  (board-state 4 21 {:caravan :eridu})
          s' (game/apply-bonus-effect s :alice 21 1)]
      (is (not= :eridu (get-in s' [:players :alice :caravan]))))))

(deftest board-25-slot-1-scores-only-raiders-influence-moved-through-test
  (testing "score = number of raiders flipped :raiding->:point by the move (no +2)"
    (let [s  (-> (board-state 4 25 {:roles {:merchant 1 :priest 1 :raider 1 :leader 2}
                                    :raiders {[:lagash :nippur] [:raiding]
                                              [:babylon :uruk] [:point]}
                                    :glory 0})
                 (assoc :magistrates {:m1 :kish}))
          s' (game/apply-bonus-with-choice s :alice 25 1 :lagash)]
      (is (= :lagash (get-in s' [:magistrates :m1])))
      (is (= [:point] (get-in s' [:players :alice :raiders [:lagash :nippur]])))
      (is (= 1 (glory s')))))
  (testing "no raiders on the path -> zero glory (not +2)"
    (let [s  (-> (board-state 4 25 {:roles {:merchant 1 :priest 1 :raider 1 :leader 2}
                                    :raiders {} :glory 0})
                 (assoc :magistrates {:m1 :kish}))
          s' (game/apply-bonus-with-choice s :alice 25 1 :lagash)]
      (is (= 0 (glory s'))))))

(deftest board-35-slot-4-scores-only-raiders-influence-moved-through-test
  (testing "score = number of raiders flipped :raiding->:point by the move (no +2)"
    (let [s  (-> (board-state 4 35 {:roles {:merchant 1 :priest 1 :raider 1 :leader 2}
                                    :raiders {[:lagash :nippur] [:raiding]
                                              [:babylon :uruk] [:point]}
                                    :glory 0})
                 (assoc :magistrates {:m1 :kish}))
          s' (game/apply-bonus-with-choice s :alice 35 4 :lagash)]
      (is (= 1 (glory s')))))
  (testing "no raiders on the path -> zero glory"
    (let [s  (-> (board-state 4 35 {:roles {:merchant 1 :priest 1 :raider 1 :leader 2}
                                    :raiders {} :glory 0})
                 (assoc :magistrates {:m1 :kish}))
          s' (game/apply-bonus-with-choice s :alice 35 4 :lagash)]
      (is (= 0 (glory s'))))))

(deftest board-3-passive-end-game-gems-worth-amity-test
  ;; [3 0] :end-game: "Your Gems are worth Amity each at end of game" — mirrors
  ;; the [18 0] tools→glory arm but gems→amity. Authored arm; must fire from
  ;; apply-end-game-scoring's :end-game pass (not dead).
  (testing "apply-passive :end-game → +1 amity per gem held"
    (let [s  (-> (state-with 3 {:resources {:gems 4 :tools 0 :gold 0 :pottery 0}
                                :amity 1})
                 uncover-passive)
          s' (game/apply-passive s :alice :end-game {})]
      (is (= 5 (amity s')) "4 gems → +4 amity (1 + 4)")))
  (testing "zero gems → no amity change"
    (let [s  (-> (state-with 3 {:resources {:gems 0} :amity 2}) uncover-passive)
          s' (game/apply-passive s :alice :end-game {})]
      (is (= 2 (amity s')) "no gems → amity unchanged")))
  (testing "wrong board → arm does not fire even on :end-game"
    (let [s  (-> (state-with 19 {:resources {:gems 4} :amity 0}) uncover-passive)
          s' (game/apply-passive s :alice :end-game {})]
      (is (= 0 (amity s')) "board 19 has no gems→amity end-game arm")))
  (testing "fires end-to-end through apply-end-game-scoring (arm not dead)"
    (let [s  (-> (state-with 3 {:resources {:gems 3 :tools 0 :gold 0 :pottery 0}
                                :amity 0 :glory 0
                                :roles {:merchant 1 :priest 1 :raider 1 :leader 1}
                                :wild-points 0})
                 uncover-passive)
          s' (game/apply-end-game-scoring s)]
      (is (= 3 (amity s')) "3 gems → +3 amity after full end-game scoring"))))

(deftest board-20-slot-3-unconditional-rider-fires-without-target-test
  ;; Merge-regression guard: [20 3] "Influence a Magistrate. Then score Amity
  ;; based on your Leader level." The score-amity rider is choice-INDEPENDENT —
  ;; with NO magistrate the influence half no-ops but the amity must still fire.
  ;; The C1+C2 merge briefly dropped it for bots (empty pick list → arm never
  ;; invoked); both the bot path and the auto arm must fire the rider.
  (testing "no magistrate → leader-level amity still scored, bot == auto arm"
    (let [s    (state-with 20 {:roles {:merchant 1 :priest 1 :raider 1 :leader 3}}
                           {:magistrates {}})
          bot  (game/bot-resolve-bonus s :alice 20 3)
          auto (game/apply-bonus-effect s :alice 20 3)]
      (is (pos? (amity bot)) "the unconditional amity rider fired for the bot (was 0 pre-fix)")
      (is (= (amity auto) (amity bot)) "bot resolution matches the auto arm"))))

;; =============================================================================
;; Demand-token ownership model ([22 2] [24 2]) — "only you may fulfill"
;; =============================================================================

(deftest board-22-slot-2-owned-demands-on-facedown-temples-test
  (testing "one owner-restricted demand per FACE-DOWN temple city (not face-up)"
    (let [s  (state-with 22 {:temples {:kish [:face-down] :uruk [:face-down]
                                       :babylon [:face-up]}}
                         {:demand-bag {:gold 5 :tools 5}})
          s' (game/apply-bonus-effect s :alice 22 2)]
      (is (= 1 (count (get-in s' [:players :alice :owned-demands :kish]))) "kish got a demand")
      (is (= 1 (count (get-in s' [:players :alice :owned-demands :uruk]))) "uruk got a demand")
      (is (nil? (get-in s' [:players :alice :owned-demands :babylon])) "face-up temple got none"))))

(deftest board-24-slot-2-owned-demands-on-magistrates-test
  (testing "one owner-restricted demand per magistrate city"
    (let [s  (state-with 24 {} {:magistrates {0 :uruk 1 :lagash}
                                :demand-bag {:gold 5 :tools 5}})
          s' (game/apply-bonus-effect s :alice 24 2)]
      (is (= 1 (count (get-in s' [:players :alice :owned-demands :uruk]))))
      (is (= 1 (count (get-in s' [:players :alice :owned-demands :lagash])))))))

(deftest owned-demands-only-owner-may-fulfill-test
  (let [sell-in #'eridu.game/bonus-sell-in
        s (-> (state-with 24 {:resources {:gold 1 :tools 0 :pottery 0 :gems 0} :amity 0}
                          {:city-demands {:uruk []}})
              (assoc-in [:players :alice :owned-demands :uruk] [:gold])
              (assoc-in [:players :bob] {:resources {:gold 1} :amity 0 :owned-demands {}}))]
    (testing "owner sees and fulfills their owned demand (consumes it, scores amity)"
      (is (game/city-has-sellable-demand? s :alice :uruk))
      (let [s' (sell-in s :alice :uruk)]
        (is (= 0 (get-in s' [:players :alice :resources :gold])) "good spent")
        (is (empty? (get-in s' [:players :alice :owned-demands :uruk])) "owned demand consumed")
        (is (pos? (get-in s' [:players :alice :amity])) "amity scored")))
    (testing "another player cannot see or fulfill it"
      (is (not (game/city-has-sellable-demand? s :bob :uruk))
          "bob does not see alice's owner-restricted demand")
      (is (= s (sell-in s :bob :uruk)) "bob's sell at uruk is a no-op (nothing he may fulfill)"))))

;; =============================================================================
;; Optional-flip ([17 3]/[18 3]) + queued free-travel ([15 3]/[30 1]/[32 2])
;; =============================================================================

(defn- point-count [s]
  (->> (vals (get-in s [:players :alice :raiders])) (apply concat) (filter #{:point}) count))

(deftest board-17-slot-3-surround-amity-and-flip-test
  ;; In 4p, Uruk has 4 adjacent routes — all must hold a raider to surround it.
  (testing "Uruk surrounded by raiders → +8 amity AND one surrounding raider flipped to point"
    (let [s  (board-state 4 17 {:raiders {[:babylon :uruk] [:raiding]
                                          [:eridu :uruk]   [:raiding]
                                          [:nippur :uruk]  [:raiding]
                                          [:lagash :uruk]  [:raiding]}
                                :amity 0})
          s' (game/apply-bonus-effect s :alice 17 3)]
      (is (= 8 (amity s')) "scored 8 amity for the surround")
      (is (= 1 (point-count s')) "exactly one surrounding raider flipped :raiding→:point")))
  (testing "not surrounded → no amity, no flip"
    (let [s  (board-state 4 17 {:raiders {[:babylon :uruk] [:raiding]} :amity 0})
          s' (game/apply-bonus-effect s :alice 17 3)]
      (is (= 0 (amity s')))
      (is (= 0 (point-count s'))))))

(deftest queued-free-travel-slots-test
  (testing "[30 1] influence then queues a same-turn free travel"
    (let [s  (-> (board-state 4 30 {}) (assoc :magistrates {0 :uruk}))
          s' (game/apply-bonus-effect s :alice 30 1)]
      (is (true? (get-in s' [:players :alice :pending-free-travel])) "free travel queued")))
  (testing "[15 3] increases lowest role then queues a free travel"
    (let [s  (board-state 4 15 {:roles {:merchant 1 :priest 3 :raider 3 :leader 3}})
          s' (game/apply-bonus-effect s :alice 15 3)]
      (is (= 2 (get-in s' [:players :alice :roles :merchant])) "lowest role (merchant) increased")
      (is (true? (get-in s' [:players :alice :pending-free-travel])) "free travel queued")))
  (testing "[32 2] grants a gem and queues a (second) free travel"
    (let [s  (board-state 4 32 {:caravan :uruk :resources {:gems 0 :tools 0 :gold 0 :pottery 0}})
          s' (game/apply-bonus-effect s :alice 32 2)]
      (is (= 1 (res s' :gems)) "gem granted")
      (is (true? (get-in s' [:players :alice :pending-free-travel])) "second travel queued"))))
