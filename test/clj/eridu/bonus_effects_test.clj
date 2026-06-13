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
      (is (= 1 (count rs)) "raider-count +1 (a placement, not a flip)")
      (let [[rk side] (first rs)]
        (is (= :point side) "placed on its POINT side")
        (is (or (contains? (set rk) :eridu)) "route touches Eridu"))))
  (testing "all Eridu routes already occupied → no-op (nothing to place)"
    ;; Eridu's routes in a 2p game: eridu-uruk (road), eridu-lagash (road).
    (let [s  (state-with 17 {:roles {:raider 4 :merchant 1 :priest 1 :leader 1}
                             :raiders {[:eridu :uruk] :raiding
                                       [:eridu :lagash] :raiding}
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
    (let [s  (state-with 22 {:raiders {[:eridu :uruk] :raiding
                                       [:kish :nippur] :point
                                       [:lagash :nippur] :raiding}
                             :amity 0 :caravan :uruk})
          s' (game/apply-bonus-with-choice s :alice 22 4 :eridu)]
      (is (= 6 (amity s')) "3 raiders × 2 amity = 6 (the amity is NOT dropped)")
      (is (= :eridu (get-in s' [:players :alice :caravan])) "travelled to the chosen city")))
  (testing "auto path (no choice) also scores the amity"
    (let [s  (state-with 22 {:raiders {[:eridu :uruk] :raiding} :amity 0 :caravan :uruk})
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
  ;; [21 1] "travel anywhere from Eridu" (choice = uruk). The caravan starts in
  ;; babylon with a face-up (point) raider on the traversed road [:babylon :uruk].
  ;; A teleport would silently skip it; real travel resolution scores it: +4
  ;; glory and the raider returns to supply.
  (testing "bonus travel over a route with own point raider → +4 glory, raider returned"
    (let [s  (board-state 4 21 {:caravan :babylon
                                :raiders {[:babylon :uruk] :point}
                                :raiders-supply 5 :glory 0})
          s' (game/apply-bonus-with-choice s :alice 21 1 :uruk)]
      (is (= :uruk (get-in s' [:players :alice :caravan])) "caravan reached the destination")
      (is (= 4 (glory s')) "scored +4 glory for the own point raider on the traversed route")
      (is (nil? (get-in s' [:players :alice :raiders [:babylon :uruk]]))
          "the scored raider was removed (no longer on the route)")
      (is (= 6 (get-in s' [:players :alice :raiders-supply]))
          "the scored raider returned to supply (5 → 6)")))
  (testing "enemy raider on the traversed route is flipped to point (real resolution)"
    (let [s  (-> (board-state 4 21 {:caravan :babylon :raiders {}})
                 (assoc-in [:players :bob] {:raiders {[:babylon :uruk] :raiding}}))
          s' (game/apply-bonus-with-choice s :alice 21 1 :uruk)]
      (is (= :point (get-in s' [:players :bob :raiders [:babylon :uruk]]))
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
                           {:raiders {[:nippur :uruk] :raiding    ;; river edge
                                      [:babylon :uruk] :raiding}}));; road edge
          s' (game/perform-river-influence s :alice :nippur)]
      (is (= :nippur (get-in s' [:magistrates :m1])) "magistrate moved across the river")
      (is (= :point (get-in s' [:players :bob :raiders [:nippur :uruk]]))
          "the RIVER-edge raider was flipped (the bug: it used to flip a road raider)")
      (is (= :raiding (get-in s' [:players :bob :raiders [:babylon :uruk]]))
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
                 (assoc-in [:players :bob] {:raiders {[:nippur :uruk] :raiding}}))
          s' (game/apply-bonus-with-choice s :alice 18 1 :nippur)]
      (is (= :nippur (get-in s' [:magistrates :m1])) "magistrate moved across the river")
      (is (= :point (get-in s' [:players :bob :raiders [:nippur :uruk]]))
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
                                :raiders {[:babylon :uruk] :raiding
                                          [:nippur :uruk] :point}})
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
