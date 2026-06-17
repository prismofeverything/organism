(ns eridu.game-test
  "Tests for the pure functional core of Eridu (src/cljc/eridu/game.cljc).

   Lesson 0: prove the test harness can find this file, require eridu.game,
   and run an assertion. We pin two foundational constants — if anyone ever
   accidentally reshapes them, half the game breaks silently, so it is worth
   one cheap test to catch it loudly."
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
   [eridu.choice :as choice]
   [eridu.game :as game]
   [organism.persist-eridu :as persist-e]))

(deftest canonical-constants-test
  (testing "resource-types is the canonical 4-resource vector"
    (is (= 4 (count game/resource-types))
        "exactly 4 resource types are baked into every player board")
    (is (= #{:tools :pottery :gold :gems}
           (set game/resource-types))
        "the four resources must be tools/pottery/gold/gems"))
  (testing "roles is the canonical 4-role vector"
    (is (= 4 (count game/roles))
        "exactly 4 roles exist")
    (is (= #{:merchant :priest :raider :leader}
           (set game/roles))
        "the four roles must be merchant/priest/raider/leader")))

;; =============================================================================
;; Astronomer movement on the 7-space astrology wheel
;; =============================================================================

(deftest move-astronomer-clockwise-test
  (testing "moving forward by one lands on the next space"
    (is (= 2 (game/move-astronomer-clockwise 1 1)))
    (is (= 3 (game/move-astronomer-clockwise 2 1)))
    (is (= 7 (game/move-astronomer-clockwise 6 1))))

  (testing "moving from space 7 wraps to space 1"
    (is (= 1 (game/move-astronomer-clockwise 7 1))
        "the astrology wheel is circular — past 7 returns to 1"))

  (testing "moving zero steps is the identity"
    (is (= 5 (game/move-astronomer-clockwise 5 0))
        "a 0-step move must not change position"))

  (testing "a full loop of 7 returns you to the same space, from anywhere"
    (doseq [space (range 1 8)]
      (is (= space (game/move-astronomer-clockwise space 7))
          (str "from space " space " a full loop of 7 must return home"))))

  (testing "moving more than 7 wraps modularly"
    (is (= 2 (game/move-astronomer-clockwise 1 8))
        "1 + 8 = 9 → wraps to space 2")
    (is (= 1 (game/move-astronomer-clockwise 1 14))
        "two full loops returns home")))

;; =============================================================================
;; Pairs of dice summing to 7 (basis for double-action strategy)
;; =============================================================================

(deftest pairs-summing-to-seven-test
  (testing "empty hand → no pairs"
    (is (= [] (game/pairs-summing-to-seven []))))

  (testing "a single die cannot pair with itself"
    (is (= [] (game/pairs-summing-to-seven [3]))
        "you need two physical dice to make a pair"))

  (testing "two dice summing to 7 → one pair"
    (is (= [[3 4]] (game/pairs-summing-to-seven [3 4]))))

  (testing "two dice not summing to 7 → no pairs"
    (is (= [] (game/pairs-summing-to-seven [3 3]))))

  (testing "a 4-die hand with no summing pairs returns empty (~38% of rolls per game.cljc:508)"
    (is (= [] (game/pairs-summing-to-seven [4 4 4 4]))
        "four 4s: every pair sums to 8, no pair sums to 7")
    (is (= [] (game/pairs-summing-to-seven [1 1 2 2]))
        "1+1=2, 1+2=3, 2+2=4, no pair sums to 7"))

  (testing "four dice with multiple summing pairs returns each pair"
    (let [pairs (game/pairs-summing-to-seven [1 6 2 5])]
      (is (= 2 (count pairs)) "expect pairs (1,6) and (2,5)")
      (is (= #{[1 6] [2 5]} (set pairs))
          "set comparison is order-independent")))

  (testing "duplicate dice values produce multiple distinct pairs (different physical dice)"
    ;; [3 4 3 4] → indices (0,1), (0,3), (1,2), (2,3) all sum to 7
    (is (= 4 (count (game/pairs-summing-to-seven [3 4 3 4])))
        "four physical dice, four index-pairs that sum to 7")))

;; =============================================================================
;; Index combinations (C(n,k))
;; =============================================================================

(deftest combo-indices-test
  (testing "C(n, 0) — choosing zero items is always one (empty) combination"
    (is (= [[]] (game/combo-indices 0 0)))
    (is (= [[]] (game/combo-indices 5 0))))

  (testing "C(n, n) — choosing everything is one combination: all indices"
    (is (= [[0 1 2]] (game/combo-indices 3 3))))

  (testing "C(n, k) when k > n is impossible — no combinations"
    (is (= [] (game/combo-indices 3 5))))

  (testing "C(4, 2) — the canonical small case"
    (let [combos (game/combo-indices 4 2)]
      (is (= 6 (count combos)) "4 choose 2 = 6")
      (is (= #{[0 1] [0 2] [0 3] [1 2] [1 3] [2 3]}
             (set combos))
          "every 2-subset of {0,1,2,3} in ascending order")))

  (testing "INVARIANT: every returned combo is sorted and has no duplicates"
    ;; Property-style test: iterate over many (n,k) and assert the shape
    ;; of EVERY combo returned. One block, many assertions.
    (doseq [n (range 0 6)
            k (range 0 (inc n))
            combo (game/combo-indices n k)]
      (is (= combo (sort combo))
          (str "combo " combo " from C(" n "," k ") must be sorted ascending"))
      (is (= (count combo) (count (distinct combo)))
          (str "combo " combo " from C(" n "," k ") must have no repeats")))))

;; =============================================================================
;; Dice subsets that loop back to the starting space
;; =============================================================================

(deftest dice-combos-for-same-space-test
  (testing "no dice → no combos"
    (is (empty? (game/dice-combos-for-same-space []))))

  (testing "a hand with no summing subset returns nothing"
    (is (empty? (game/dice-combos-for-same-space [1 1 1 1]))
        "four 1s sum to 4, no subset sums to a multiple of 7"))

  (testing "a hand with one summing pair returns that pair"
    (let [combos (game/dice-combos-for-same-space [3 4 1 1])]
      (is (= [[0 1]] (vec combos))
          "only [3 4] sums to 7")))

  (testing "INVARIANT: every returned subset's dice values must sum to a multiple of 7"
    ;; Property test over a few real-shaped hands. If this ever fails, the
    ;; filter logic in dice-combos-for-same-space is wrong.
    (doseq [hand [[1 6 2 5] [3 4 5 2] [6 6 6 6] [1 2 3 4]]
            subset (game/dice-combos-for-same-space hand)
            :let [sum (reduce + (map #(nth hand %) subset))]]
      (is (zero? (mod sum 7))
          (str "subset " subset " of hand " hand " sums to " sum
               " which is not a multiple of 7")))))

;; =============================================================================
;; City board — canonical route keys
;; =============================================================================

(deftest route-key-test
  (testing "route-key returns the same value regardless of argument order"
    (is (= (game/route-key :babylon :uruk)
           (game/route-key :uruk :babylon))
        "an edge between two cities is one object regardless of direction"))

  (testing "route-key sorts alphabetically by city name"
    (is (= [:babylon :uruk] (game/route-key :babylon :uruk)))
    (is (= [:babylon :uruk] (game/route-key :uruk :babylon))
        "even when called with the larger name first, output is alphabetical"))

  (testing "INVARIANT: symmetric for every pair of distinct cities"
    ;; This is the whole point of a canonical key — symmetry. If it ever
    ;; breaks, every downstream consumer (raider tracking, route lookups,
    ;; demand checks) silently splits one edge into two.
    (doseq [a game/all-cities
            b game/all-cities
            :when (not= a b)]
      (is (= (game/route-key a b) (game/route-key b a))
          (str "asymmetric route-key for " a " ↔ " b)))))

(deftest segment-route-key-test
  (testing "extracts canonical key from a route segment map"
    (is (= [:babylon :uruk]
           (game/segment-route-key {:from :babylon :to :uruk :type :road})))
    (is (= [:babylon :uruk]
           (game/segment-route-key {:from :uruk :to :babylon :type :road}))
        "still canonical when :from/:to are in non-alphabetical order")))

;; =============================================================================
;; City graph — parameterized by player count (Samarra-removal rules)
;; =============================================================================

(deftest city-graph-by-player-count-test
  ;; The rule: 4+ players play with all 8 cities; 2-3 players remove Samarra.
  ;; That rule appears once in game.cljc:669 — but every consumer assumes it
  ;; works. We test each parameter value separately.

  (testing "4-player game includes all 8 cities"
    (let [g (game/city-graph 4)]
      (is (= 8 (count g)))
      (is (contains? g :samarra) "Samarra is present in 4-player games")))

  (testing "3-player game removes Samarra (7 cities)"
    (let [g (game/city-graph 3)]
      (is (= 7 (count g)))
      (is (not (contains? g :samarra)) "Samarra is absent in 3-player games")))

  (testing "2-player game also removes Samarra"
    (let [g (game/city-graph 2)]
      (is (= 7 (count g)))
      (is (not (contains? g :samarra))
          "the rule is <= 3, not exactly 3 — 2-player must also be tested")))

  (testing "INVARIANT: no city is isolated in any supported player count"
    ;; Every city must reach at least one other city, in any player count.
    ;; If a future edit accidentally orphans a city, this test catches it.
    (doseq [pc [2 3 4]
            [city neighbors] (game/city-graph pc)]
      (is (pos? (count neighbors))
          (str city " has no neighbors in a " pc "-player game")))))

;; =============================================================================
;; The :only-without conditional edge (Nineveh ↔ Kish)
;; =============================================================================

(deftest only-without-conditional-edge-test
  ;; The Nineveh-Kish road exists ONLY when Samarra is absent (2-3p).
  ;; This is a one-line rule in the route data — easy to miss in a refactor.

  (testing "Nineveh-Kish road is ACTIVE in 2-3 player games"
    (let [edge-keys (set (map game/segment-route-key (game/active-routes 3)))]
      (is (contains? edge-keys [:kish :nineveh])
          "the Nineveh-Kish bypass appears when Samarra is removed")))

  (testing "Nineveh-Kish road is INACTIVE in 4-player games"
    (let [edge-keys (set (map game/segment-route-key (game/active-routes 4)))]
      (is (not (contains? edge-keys [:kish :nineveh]))
          "Nineveh and Kish are NOT directly connected when Samarra is present")))

  (testing "this means Kish gains Nineveh as a neighbor in 2-3p"
    (let [kish-3p (game/city-neighbors :kish (game/active-routes 3))
          kish-4p (game/city-neighbors :kish (game/active-routes 4))]
      (is (contains? kish-3p :nineveh)
          "in 3-player: yes, Kish neighbors Nineveh directly")
      (is (not (contains? kish-4p :nineveh))
          "in 4-player: no, you'd have to go through Samarra or Babylon"))))

;; =============================================================================
;; Magistrate clockwise movement
;; =============================================================================

(deftest road-clockwise-next-test
  (let [cities-4p (set (keys (game/city-graph 4)))
        cities-3p (set (keys (game/city-graph 3)))]

    (testing "moves to the next city in the clockwise order"
      (is (= :kish (game/road-clockwise-next :samarra cities-4p)))
      (is (= :nippur (game/road-clockwise-next :kish cities-4p))))

    (testing "wraps from the last city back to the first"
      (is (= :samarra (game/road-clockwise-next :nineveh cities-4p))
          "after :nineveh comes :samarra in the 4-player clockwise order"))

    (testing "skips inactive cities (Samarra removed in 3-player)"
      (is (= :kish (game/road-clockwise-next :nineveh cities-3p))
          "without Samarra, Nineveh wraps directly to Kish"))

    (testing "returns nil for a city not in the active set"
      (is (nil? (game/road-clockwise-next :samarra cities-3p))
          "Samarra isn't in a 3-player game; pin the 'silent nil' contract"))))

(deftest road-clockwise-path-test
  (let [cities-4p (set (keys (game/city-graph 4)))
        cities-3p (set (keys (game/city-graph 3)))]

    (testing "zero steps returns an empty path"
      (is (= [] (game/road-clockwise-path :kish 0 cities-4p))))

    (testing "1 step returns one [from to] edge"
      (is (= [[:kish :nippur]] (game/road-clockwise-path :kish 1 cities-4p))))

    (testing "N steps returns N consecutive edges in order"
      (is (= [[:samarra :kish] [:kish :nippur] [:nippur :lagash]]
             (game/road-clockwise-path :samarra 3 cities-4p))))

    (testing "stepping past the end wraps around"
      (is (= [[:nineveh :samarra] [:samarra :kish]]
             (game/road-clockwise-path :nineveh 2 cities-4p))
          "Nineveh → Samarra → Kish across the wraparound"))

    (testing "path from an invalid start city silently returns []"
      ;; PINNING the behavior — this is a 'silent no-op' on bad input.
      ;; If anyone ever changes it to throw, this test forces them to update
      ;; this assertion (i.e., consciously acknowledge the contract change).
      (is (= [] (game/road-clockwise-path :samarra 5 cities-3p))
          "Samarra isn't in a 3p game — path is empty, no exception"))))

;; =============================================================================
;; River routes — implicit invariants worth pinning
;; =============================================================================

(deftest rivers-as-neighbors-test
  ;; city-neighbors returns BOTH road and river neighbors. Caravans can use
  ;; either, so this is the correct behavior. Pin it so future "let's filter
  ;; to roads here" changes break loudly.
  (testing "rivers contribute neighbors just like roads"
    (let [routes (game/active-routes 4)
          uruk-neighbors (game/city-neighbors :uruk routes)]
      ;; Uruk's routes per city-routes (game.cljc:625-635):
      ;;   road:  uruk↔babylon, uruk↔eridu
      ;;   river: uruk↔nippur, uruk↔lagash
      (is (contains? uruk-neighbors :babylon) "road neighbor")
      (is (contains? uruk-neighbors :eridu)   "road neighbor")
      (is (contains? uruk-neighbors :nippur)  "RIVER neighbor")
      (is (contains? uruk-neighbors :lagash)  "RIVER neighbor")
      (is (= 4 (count uruk-neighbors))
          "Uruk has exactly 4 neighbors across both route types"))))

(deftest road-clockwise-path-traces-only-roads-test
  ;; Implicit invariant: road-clockwise-order is hardcoded such that every
  ;; consecutive pair (under any active-cities subset we use in real play)
  ;; corresponds to a :road in city-routes — never a :river. The function
  ;; doesn't enforce this; the data is just happens to be set up that way.
  ;; This test catches accidental reorderings of road-clockwise-order that
  ;; would let a magistrate silently 'ride' a river.
  (let [routes-4p (game/active-routes 4)
        routes-3p (game/active-routes 3)
        road-edge? (fn [routes [from to]]
                     ;; Find a route between `from` and `to` whose type is :road.
                     ;; Check both orderings since route data isn't canonicalized.
                     (some #(and (= :road (:type %))
                                 (or (and (= from (:from %)) (= to (:to %)))
                                     (and (= to   (:from %)) (= from (:to %)))))
                           routes))]

    (testing "every step of the 4-player clockwise loop is a road"
      (let [cities (set (keys (game/city-graph 4)))
            full-loop (game/road-clockwise-path :samarra 8 cities)]
        (is (= 8 (count full-loop)) "full 8-city loop")
        (doseq [edge full-loop]
          (is (road-edge? routes-4p edge)
              (str edge " must be a :road, not a :river")))))

    (testing "every step of the 3-player clockwise loop is a road (incl. the Nineveh-Kish conditional)"
      (let [cities (set (keys (game/city-graph 3)))
            full-loop (game/road-clockwise-path :kish 7 cities)]
        (is (= 7 (count full-loop)) "7-city loop without Samarra")
        (doseq [edge full-loop]
          (is (road-edge? routes-3p edge)
              (str edge " must be a :road")))))))

;; =============================================================================
;; Test fixtures — minimal state builders for predicate tests
;; =============================================================================
;; State predicates need real state to test. Hand-rolling full states for
;; every test is tedious and noisy; build small, well-named helpers.
;; KEEP THE HELPERS DUMB. If you find yourself adding logic to them, you're
;; either testing the wrong thing or your production code wants restructuring.

(defn- make-player
  "Build a minimal player record. Pass overrides as a map."
  [overrides]
  (merge {:temples {} :raiders {} :astronomers []
          :resources {} :amity 0 :glory 0}
         overrides))

(defn- make-state
  "Build a minimal game state. Pass overrides as a map."
  [overrides]
  (merge {:players {} :magistrates {}}
         overrides))

;; =============================================================================
;; Simple count predicates
;; =============================================================================

(deftest count-face-down-temples-test
  (testing "counts only :face-down temples, ignoring :face-up"
    (let [p (make-player {:temples {:eridu   [:face-down]
                                    :nineveh [:face-up]
                                    :uruk    [:face-down]}})]
      (is (= 2 (game/count-face-down-temples p)))))
  (testing "no temples → zero (not nil)"
    (is (= 0 (game/count-face-down-temples (make-player {:temples {}})))))
  (testing "all face-up → zero"
    (let [p (make-player {:temples {:eridu [:face-up] :nineveh [:face-up]}})]
      (is (= 0 (game/count-face-down-temples p)))))
  (testing "multiple temples in one city are all counted"
    (let [p (make-player {:temples {:eridu [:face-down :face-down] :uruk [:face-up :face-down]}})]
      (is (= 3 (game/count-face-down-temples p))))))

(deftest count-temples-placed-test
  (testing "counts all temples regardless of face-up/face-down"
    (let [p (make-player {:temples {:eridu [:face-down] :nineveh [:face-up]}})]
      (is (= 2 (game/count-temples-placed p)))))
  (testing "a city holding two temples counts both"
    (let [p (make-player {:temples {:eridu [:face-up :face-down]}})]
      (is (= 2 (game/count-temples-placed p)))))
  (testing "empty → 0"
    (is (= 0 (game/count-temples-placed (make-player {}))))))

(deftest count-raiders-deployed-test
  (testing "counts all raiders the player has on the board"
    (let [p (make-player {:raiders {[:babylon :uruk] [:raiding]
                                    [:eridu :uruk]   [:point]}})]
      (is (= 2 (game/count-raiders-deployed p)))))
  (testing "a route holding two raiders counts both (multi-raider model)"
    (let [p (make-player {:raiders {[:babylon :uruk] [:raiding :point]
                                    [:eridu :uruk]   [:point]}})]
      (is (= 3 (game/count-raiders-deployed p)))))
  (testing "no raiders → 0"
    (is (= 0 (game/count-raiders-deployed (make-player {}))))))

(deftest astronomers-on-space-test
  (testing "lists every astronomer on the given space"
    (let [s (make-state {:players {:alice (make-player {:astronomers [1 3 1]})
                                   :bob   (make-player {:astronomers [3 1 5]})}})]
      (is (= #{[:alice 0] [:alice 2] [:bob 1]}
             (set (game/astronomers-on-space s 1)))
          "alice astronomers 0&2 are on space 1; bob astronomer 1 is on space 1")))
  (testing "empty result when no astronomers are on the space"
    (let [s (make-state {:players {:alice (make-player {:astronomers [1 1 1]})}})]
      (is (= [] (game/astronomers-on-space s 5))))))

;; =============================================================================
;; Magistrate predicates — historical bug country
;; =============================================================================

(deftest magistrate-cities-test
  (testing "returns the set of all cities currently hosting a magistrate"
    (let [s (make-state {:magistrates {0 :eridu, 1 :babylon}})]
      (is (= #{:eridu :babylon} (game/magistrate-cities s)))))

  (testing "two magistrates in the same city collapse to one entry (set semantics)"
    (let [s (make-state {:magistrates {0 :eridu, 1 :eridu}})]
      (is (= #{:eridu} (game/magistrate-cities s)))))

  (testing "no magistrates → empty set, not nil"
    (is (= #{} (game/magistrate-cities (make-state {:magistrates {}}))))))

(deftest magistrate-in-city?-test
  (let [s (make-state {:magistrates {0 :eridu, 1 :babylon}})]

    (testing "returns truthy when a magistrate is in the given city"
      (is (game/magistrate-in-city? s :eridu)   "eridu has a magistrate")
      (is (game/magistrate-in-city? s :babylon) "babylon has a magistrate"))

    (testing "returns falsy when no magistrate is in the given city"
      (is (not (game/magistrate-in-city? s :uruk))
          "uruk has no magistrate"))

    (testing "STRUCTURE GUARD: :magistrates maps {mag-id → city}, NOT {city → owner}"
      ;; The current implementation uses (vals (:magistrates state)). That
      ;; only gives city keywords if the values are cities. If anyone ever
      ;; restructures :magistrates to {city → owner-data}, vals will return
      ;; owner-data and every predicate silently returns garbage.
      ;;
      ;; This assertion pins the *shape* alongside the behavior, so a refactor
      ;; that changes one without the other gets caught.
      (is (not-any? game/all-cities (keys (:magistrates s)))
          "magistrate keys must NOT be city keywords")
      (is (every? game/all-cities (vals (:magistrates s)))
          "magistrate values MUST be city keywords"))))

(deftest g1-g2-magistrate-contests-claimable-test
  ;; Regression: G1 ("move a magistrate 4 cities in one turn") and G2 ("move a
  ;; magistrate through 3 raiders, any owner") are instrumented in the influence
  ;; choice path. G2 previously counted only :raiding-side raiders, undercounting
  ;; the point-side ones the magistrate also crosses. Pin both contests live.
  (let [base   (game/initial-state [:p1 :p2 :p3 :p4])
        player (game/current-player base)
        opp    (first (filter #(not= % player) (keys (:players base))))
        routes (game/board-routes base)
        ;; opponent holds a POINT-side raider on EVERY route, so whatever
        ;; clockwise path the magistrate sweeps, it crosses point raiders.
        all-point (into {} (map (fn [r] [(game/route-key (:from r) (:to r)) [:point]])
                                routes))
        s (-> base
              (assoc-in [:players player :roles :leader] 5)   ;; leader-movement 5
              (assoc-in [:players opp :raiders] all-point)
              (assoc :magistrates {0 (first (keys (:city-graph base)))})
              (assoc-in [:turn-stats :player] player))
        choices (choice/resolve-influence-choices s)
        next-states (vals choices)]
    (testing "G2 counts point-side raiders the magistrate crosses (was 0 before fix)"
      (let [best (apply max 0 (keep #(get-in % [:turn-stats :magistrate-raiders-flipped])
                                    next-states))]
        (is (>= best 3)
            "a multi-step sweep crosses >=3 point-side raiders")))
    (testing "a qualifying influence move makes G1 and G2 evaluate true"
      (let [g2-ns (some (fn [ns] (when (>= (get-in ns [:turn-stats :magistrate-raiders-flipped] 0) 3) ns))
                        next-states)
            g1-ns (some (fn [ns] (when (>= (get-in ns [:turn-stats :magistrate-max-move] 0) 4) ns))
                        next-states)]
        (is (and g1-ns (game/evaluate-contest g1-ns player {:id :G1}))
            "G1 claimable after a >=4-city magistrate move")
        (is (and g2-ns (game/evaluate-contest g2-ns player {:id :G2}))
            "G2 claimable after crossing >=3 raiders")))))

(deftest magistrate-and-my-temple-cities-test
  (testing "returns intersection of magistrate cities and player's temples"
    (let [alice (make-player {:temples {:eridu   [:face-up]
                                        :babylon [:face-down]
                                        :uruk    [:face-up]}})
          s (make-state {:players {:alice alice}
                         :magistrates {0 :babylon, 1 :uruk, 2 :samarra}})]
      ;; mag cities = #{:babylon :uruk :samarra}
      ;; alice temples = #{:eridu :babylon :uruk}
      ;; intersection = #{:babylon :uruk}
      ;; Order in the returned vector is unstable (depends on set iteration),
      ;; so we compare via (set ...) to assert membership, not order.
      (is (= #{:babylon :uruk}
             (set (game/magistrate-and-my-temple-cities s :alice)))
          "only cities with BOTH a magistrate AND my temple")))

  (testing "no overlap → empty vector"
    (let [alice (make-player {:temples {:eridu [:face-up]}})
          s (make-state {:players {:alice alice}
                         :magistrates {0 :babylon}})]
      (is (= [] (game/magistrate-and-my-temple-cities s :alice)))))

  (testing "CONTRACT: returns a vector, NOT a set"
    ;; Board 34's multi-pick UI flow iterates this in order, so the vector
    ;; shape is load-bearing. Pin it explicitly — if anyone changes it back
    ;; to a set or seq, the UI consumer drift would be hard to notice.
    (let [alice (make-player {:temples {:babylon [:face-up]}})
          s (make-state {:players {:alice alice}
                         :magistrates {0 :babylon}})]
      (is (vector? (game/magistrate-and-my-temple-cities s :alice))
          "consumers depend on vector shape (Board 34 UI city-picker)"))))

;; =============================================================================
;; State-query helpers extracted from personality.cljc + eridu_ws.clj in lesson 4
;; =============================================================================

(deftest space-action-types-test
  (testing "returns the set of action types on space 7 (the strategic space)"
    ;; Space 7 has {sell, deploy, influence, temple} per action-spaces.
    (is (= #{:deploy :sell :influence :temple}
           (game/space-action-types 7))))
  (testing "returns the set of action types on space 1 (duplicates collapsed)"
    ;; Space 1 has {take, sell, sell, travel} — set collapses dupes.
    (is (= #{:take :sell :travel} (game/space-action-types 1)))))

(deftest space-gives-resources-test
  (testing "returns the resource pair of the take-action on the space"
    (is (= [:gems :tools] (game/space-gives-resources 1)))
    (is (= [:gold :pottery] (game/space-gives-resources 2))))
  (testing "returns nil on space 7 (no take action there)"
    (is (nil? (game/space-gives-resources 7)))))

(deftest has-resource-excess?-test
  (testing "true when any listed resource is > 2"
    (let [p (make-player {:resources {:tools 3 :pottery 0 :gold 1 :gems 0}})]
      (is (game/has-resource-excess? p [:tools :pottery]) "tools = 3 > 2")))
  (testing "false when none exceeds the threshold"
    (let [p (make-player {:resources {:tools 2 :pottery 0}})]
      (is (not (game/has-resource-excess? p [:tools :pottery]))
          "tools = 2 is NOT > 2 (boundary)")))
  (testing "false on missing resources (defaults to 0)"
    (is (not (game/has-resource-excess? (make-player {}) [:tools])))))

(deftest city-has-sellable-demand?-test
  (testing "truthy when the player has a resource matching a city demand"
    (let [alice (make-player {:resources {:pottery 1}})
          s (make-state {:players {:alice alice}
                         :city-demands {:eridu [:pottery :gold]}})]
      (is (game/city-has-sellable-demand? s :alice :eridu)
          "alice has pottery, eridu demands pottery")))
  (testing "falsy when no overlap between resources and demands"
    (let [alice (make-player {:resources {:gems 5}})
          s (make-state {:players {:alice alice}
                         :city-demands {:eridu [:pottery :gold]}})]
      (is (not (game/city-has-sellable-demand? s :alice :eridu))
          "alice has gems, eridu doesn't demand gems")))
  (testing "falsy when city has no demands"
    (let [alice (make-player {:resources {:pottery 5}})
          s (make-state {:players {:alice alice}
                         :city-demands {}})]
      (is (not (game/city-has-sellable-demand? s :alice :eridu))))))

(deftest city-has-own-face-up-temple?-test
  (testing "true for a face-up temple"
    (let [p (make-player {:temples {:eridu [:face-up]}})]
      (is (game/city-has-own-face-up-temple? p :eridu))))
  (testing "falsy for a face-down temple"
    (let [p (make-player {:temples {:eridu [:face-down]}})]
      (is (not (game/city-has-own-face-up-temple? p :eridu)))))
  (testing "true if ANY temple in the city is face-up"
    (let [p (make-player {:temples {:eridu [:face-down :face-up]}})]
      (is (game/city-has-own-face-up-temple? p :eridu))))
  (testing "falsy when no temple in that city"
    (is (not (game/city-has-own-face-up-temple? (make-player {}) :eridu)))))

;; =============================================================================
;; Contest evaluator — specification tests for evaluate-contest
;; =============================================================================
;; The function under test is documented as "Returns true/false". These tests
;; specify what it SHOULD do per the contest cards. Some go red — that's
;; deliberate.

(defn- contest-state
  "Build a minimal state for testing evaluate-contest. Specify only the
   slice the contest under test cares about; everything else defaults to
   sensible empties."
  [{:keys [player magistrates turn-order city-demands turn-stats]
    :or {magistrates {} turn-order [:alice] city-demands {} turn-stats {}}}]
  {:players {:alice (make-player (or player {}))}
   :magistrates magistrates
   :turn-order turn-order
   :city-demands city-demands
   :turn-stats turn-stats})

(defn- check
  "Shorthand: evaluate contest `id` for :alice in `state`."
  [state id]
  (game/evaluate-contest state :alice {:id id}))

(deftest evaluate-contest-A1-test
  (testing "A1: 3+ gems/gold demands fulfilled → true"
    (is (= true (check (contest-state {:player {:demand-tokens [:gems :gold :gems]}}) :A1))))
  (testing "A1: only 2 fulfilled → false"
    (is (= false (check (contest-state {:player {:demand-tokens [:gems :gold]}}) :A1))))
  (testing "A1: tools/pottery don't count for A1"
    (is (= false (check (contest-state {:player {:demand-tokens [:tools :pottery :tools]}}) :A1)))))

(deftest evaluate-contest-C1-test
  (testing "C1: 4 face-up temples → true"
    (is (= true (check (contest-state
                        {:player {:temples {:eridu [:face-up] :uruk [:face-up]
                                            :babylon [:face-up] :nineveh [:face-up]}}})
                       :C1))))
  (testing "C1: 3 face-up temples is not enough"
    (is (= false (check (contest-state
                         {:player {:temples {:eridu [:face-up] :uruk [:face-up]
                                             :babylon [:face-up]}}})
                        :C1))))
  (testing "C1: two face-up temples in one city count separately"
    (is (= true (check (contest-state
                        {:player {:temples {:eridu [:face-up :face-up] :uruk [:face-up :face-up]}}})
                       :C1))))
  (testing "C1: face-down temples don't count"
    (is (= false (check (contest-state
                         {:player {:temples {:eridu [:face-up] :uruk [:face-down]
                                             :babylon [:face-down] :nineveh [:face-down]}}})
                        :C1)))))

(deftest evaluate-contest-D1-test
  (testing "D1: temples in BOTH eridu and nineveh"
    (is (= true (check (contest-state
                        {:player {:temples {:eridu [:face-up] :nineveh [:face-down]}}})
                       :D1)))
    (is (= false (check (contest-state {:player {:temples {:eridu [:face-up]}}}) :D1))
        "missing nineveh"))
  (testing "D1: face-down counts (D1 just checks presence)"
    (is (= true (check (contest-state
                        {:player {:temples {:eridu [:face-down] :nineveh [:face-down]}}})
                       :D1)))))

(deftest evaluate-contest-H1-test
  (testing "H1: 2+ roles at level 3+ → true"
    (is (= true (check (contest-state
                        {:player {:roles {:merchant 3 :priest 4 :raider 1 :leader 1}}})
                       :H1))))
  (testing "H1: only 1 role at 3 → false"
    (is (= false (check (contest-state
                         {:player {:roles {:merchant 3 :priest 2 :raider 1 :leader 1}}})
                        :H1)))))

(deftest evaluate-contest-H2-current-behavior-test
  ;; H2 uses (some #(= 5 (val %)) roles). When matched, that returns `true`;
  ;; when not, `nil`. Per docstring it should return true/false strictly —
  ;; but currently returns truthy/falsy. Pin current behavior; file as smell.
  (testing "H2: any role at level 5"
    (is (check (contest-state {:player {:roles {:merchant 5}}}) :H2)
        "truthy when a role is at 5")
    (is (not (check (contest-state {:player {:roles {:merchant 4}}}) :H2))
        "falsy when no role is at 5"))
  (testing "H2 SMELL: doesn't return strict false (returns nil)"
    ;; If H2 ever needs to be a strict boolean, this is the diagnostic.
    (is (nil? (check (contest-state {:player {:roles {:merchant 4}}}) :H2))
        "currently nil — should be false per docstring (filed)")))

(deftest evaluate-contest-L1-test
  (testing "L1: 5+ gems → true"
    (is (= true (check (contest-state {:player {:resources {:gems 5}}}) :L1))))
  (testing "L1: 4 gems is not enough (boundary)"
    (is (= false (check (contest-state {:player {:resources {:gems 4}}}) :L1)))))

(deftest evaluate-contest-J2-test
  (testing "J2: exactly 2 tools, 0 of everything else → true"
    (is (= true (check (contest-state
                        {:player {:resources {:tools 2 :pottery 0 :gold 0 :gems 0}}})
                       :J2))))
  (testing "J2: 2 tools + 1 pottery → false (J2 demands ONLY tools)"
    (is (= false (check (contest-state
                         {:player {:resources {:tools 2 :pottery 1}}})
                        :J2))))
  (testing "J2: 3 tools (over the exact count) → false"
    (is (= false (check (contest-state {:player {:resources {:tools 3}}}) :J2)))))

;; --- The bugs ---------------------------------------------------------------

(deftest evaluate-contest-K2-bug-test
  ;; K2 uses (when ...) inside the case arm. When preconditions fail, `when`
  ;; returns nil — but the docstring says "Returns true/false". This goes red.
  (testing "K2 BUG: no turn-stats → should be false, currently returns nil"
    (let [s (contest-state {})]
      (is (= false (check s :K2))
          "evaluate-contest contract is true/false; K2 leaks nil through `when`"))))

(deftest evaluate-contest-M1-smell-test
  ;; M1: "every magistrate must be in a city with a facedown temple."
  ;; Uses (every? ...) which returns true vacuously on an empty collection.
  ;; If there are zero magistrates, M1 returns true — which is technically
  ;; correct ("for all 0 magistrates, the condition holds") but probably not
  ;; what the rules intend. In real play there are always magistrates, so
  ;; this is theoretical. Pin the behavior and file the smell.
  (testing "M1 vacuous-true: no magistrates → currently true"
    (is (= true (check (contest-state {:magistrates {}}) :M1))
        "vacuous-true: no magistrates means 'all of them' satisfy any rule")))

;; =============================================================================
;; effect-implementation-status — honesty about which boards are faithful
;; =============================================================================

(deftest effect-implementation-status-shape-test
  ;; This map is consumed by board-effect-diagnostic (game.cljc) and
  ;; coverage-status (bonus_oracle.clj). Both report counts grouped by
  ;; value. If a fourth status keyword sneaks in via typo (:implemnted,
  ;; :imploemented, :Partial, etc.), it silently invents a new category
  ;; that nobody reports — pin the closed set of allowed values.
  (testing "every value is one of the documented status keywords"
    (let [allowed #{:implemented :partial :persistent
                    :needs-compound :needs-placement :needs-demand :conditional}
          actual-values (set (vals game/effect-implementation-status))]
      (is (every? allowed actual-values)
          (str "rogue status values: "
               (set/difference actual-values allowed)))))

  (testing "every key is [board-id slot-idx], slot 0..4, board 1..35"
    (doseq [[k _] game/effect-implementation-status]
      (is (and (vector? k) (= 2 (count k))) (str "bad key shape: " k))
      (let [[board slot] k]
        (is (<= 1 board 35) (str "board out of range: " k))
        (is (<= 0 slot  4) (str "slot out of range: " k))))))

;; =============================================================================
;; Board 32 passive choice — TDD red→green on a real bug
;; =============================================================================
;; Board 32: "Sell → discard a gem to score at Priest level instead of Merchant."
;; The code in apply-passive-choice computes net amity as
;;     (priest-LEVEL - merchant-amity-already-scored)
;; but it should be
;;     (priest-AMITY-from-lookup - merchant-amity-already-scored)
;; — silently under-scores by 1 in 6 of 10 valid (priest>merchant) combinations.

(deftest board-32-passive-choice-bug-test
  ;; Setup helper: build the state right before apply-passive-choice fires,
  ;; representing a player who just sold and is being prompted by Board 32.
  (let [make-pending (fn [{:keys [priest-lv merchant-lv amity-already-scored
                                  gems amity]
                           :or {gems 1 amity 0}}]
                       (let [p (make-player
                                {:roles {:priest priest-lv :merchant merchant-lv}
                                 :resources {:gems gems}
                                 :amity amity
                                 :bonus-board {0 :uncovered}
                                 :passive-choice-needed
                                 {:type :yes-no
                                  :board-id 32
                                  :context {:amity-scored amity-already-scored}}})]
                         {:players {:alice p}
                          :bonus-boards {:alice 32}}))]

    (testing "merchant=2 (scored 3), priest=4: correct net is +2 amity"
      ;; priest-amity (lookup) = 5, merchant-amity-scored = 3, net = +2.
      ;; Buggy code: (4 - 3) = +1.
      (let [s  (make-pending {:priest-lv 4 :merchant-lv 2
                              :amity-already-scored 3 :amity 3})
            s' (game/apply-passive-choice s :alice :yes)]
        (is (= 5 (get-in s' [:players :alice :amity]))
            "amity: 3 (existing) + 2 (priest-amity 5 minus merchant 3) = 5")
        (is (= 0 (get-in s' [:players :alice :resources :gems]))
            "gem discarded as the cost of the swap")))

    (testing "merchant=1 (scored 2), priest=3: correct net is +2 amity"
      ;; priest-amity = 4, scored = 2, net = +2.
      ;; Buggy code: (3 - 2) = +1.
      (let [s  (make-pending {:priest-lv 3 :merchant-lv 1
                              :amity-already-scored 2 :amity 2})
            s' (game/apply-passive-choice s :alice :yes)]
        (is (= 4 (get-in s' [:players :alice :amity]))
            "amity: 2 + (priest-amity 4 - merchant 2) = 4")))

    (testing "merchant=1 (scored 2), priest=2: correct net is +1 amity (the smallest swap)"
      ;; priest-amity = 3, scored = 2, net = +1.
      ;; Buggy code: (2 - 2) = 0 — player gains NOTHING from a swap they paid a gem for.
      (let [s  (make-pending {:priest-lv 2 :merchant-lv 1
                              :amity-already-scored 2 :amity 2})
            s' (game/apply-passive-choice s :alice :yes)]
        (is (= 3 (get-in s' [:players :alice :amity]))
            "amity: 2 + (priest-amity 3 - merchant 2) = 3 (NOT 2 — they paid a gem)")))

    (testing "choosing :no should change nothing — no gem cost, no amity"
      (let [s  (make-pending {:priest-lv 4 :merchant-lv 2
                              :amity-already-scored 3 :amity 3 :gems 1})
            s' (game/apply-passive-choice s :alice :no)]
        (is (= 3 (get-in s' [:players :alice :amity])) "amity unchanged")
        (is (= 1 (get-in s' [:players :alice :resources :gems])) "gem preserved")))))

;; =============================================================================
;; Board 18 slot 4 — faithful "remove the raiders" implementation
;; =============================================================================
;; Card text (from Dropbox MSE):
;;   "Score 4 A for each of your Raiders on their point side.
;;    Then remove those raiders."
;;
;; Pre-fix: scored amity correctly, but left the point raiders deployed →
;; stockpile exploit. Post-fix: scores AND removes, supply restored.

(deftest board-18-slot-4-faithful-test
  (let [make-state-with-raiders
        (fn [raiders supply amity]
          (let [p (make-player
                   {:raiders raiders
                    :raiders-supply supply
                    :amity amity
                    :roles {:raider 1 :merchant 1 :priest 1 :leader 1}})]
            {:players {:alice p}
             :turn-order [:alice]}))]

    (testing "regression: still scores +4 amity per point raider"
      ;; This already worked pre-fix — pinning it so the fix doesn't
      ;; accidentally break the amity math.
      (let [s  (make-state-with-raiders
                {[:babylon :uruk] [:point]
                 [:eridu :uruk]   [:point]
                 [:kish :nippur]  [:point]}
                3 0)
            s' (game/apply-bonus-effect s :alice 18 4)]
        (is (= 12 (get-in s' [:players :alice :amity]))
            "3 point raiders × 4 amity each = 12")))

    (testing "removes point raiders from the :raiders map"
      (let [s  (make-state-with-raiders
                {[:babylon :uruk] [:point]
                 [:eridu :uruk]   [:point]
                 [:kish :nippur]  [:raiding]}
                3 0)
            s' (game/apply-bonus-effect s :alice 18 4)]
        (is (= {[:kish :nippur] [:raiding]}
               (get-in s' [:players :alice :raiders]))
            "point raiders gone; raiding raider untouched")))

    (testing "a route with a point AND a raiding raider keeps only the raiding one"
      (let [s  (make-state-with-raiders
                {[:babylon :uruk] [:point :raiding]
                 [:eridu :uruk]   [:point]}
                3 0)
            s' (game/apply-bonus-effect s :alice 18 4)]
        (is (= {[:babylon :uruk] [:raiding]}
               (get-in s' [:players :alice :raiders]))
            "both point raiders removed, the raiding one preserved")
        (is (= 8 (get-in s' [:players :alice :amity])) "2 point raiders × 4 = 8")
        (is (= 5 (get-in s' [:players :alice :raiders-supply])) "supply 3 → 5")))

    (testing "returns removed point raiders to :raiders-supply"
      (let [s  (make-state-with-raiders
                {[:babylon :uruk] [:point]
                 [:eridu :uruk]   [:point]}
                3 0)
            s' (game/apply-bonus-effect s :alice 18 4)]
        (is (= 5 (get-in s' [:players :alice :raiders-supply]))
            "supply 3 → 5 (the two removed point raiders returned)")))

    (testing "no point raiders → no-op (no amity change, no removal, no supply)"
      (let [s  (make-state-with-raiders
                {[:babylon :uruk] [:raiding]}
                3 5)
            s' (game/apply-bonus-effect s :alice 18 4)]
        (is (= 5 (get-in s' [:players :alice :amity])) "amity unchanged")
        (is (= {[:babylon :uruk] [:raiding]}
               (get-in s' [:players :alice :raiders]))
            "raiding raider stays")
        (is (= 3 (get-in s' [:players :alice :raiders-supply]))
            "supply unchanged")))))

;; =============================================================================
;; Board 13 — "Pillars of Etana" slot 4 + slot-0 passive
;; =============================================================================
;; Bug report (eridu-bug-reports.jsonl): using slot 5 (slot-idx 4) didn't let
;; the player pick which raider to place the temple next to, and the slot-0
;; passive ("place a Temple → place a Raider adjacent") seemed not to fire.
;;
;; Two root causes:
;;   1. [13 4] was missing from bonus-needs-choice? → the WS layer auto-resolved
;;      it (first eligible city) instead of prompting → no raider-selection.
;;   2. place-temple-in never fired the :temple-placed passive, so EVERY
;;      bonus-board temple placement (incl. [13 4]) skipped board-13's slot-0
;;      "raider adjacent" passive → delta-raiders 0 in the report's log.

(deftest board-13-slot-4-temple-adjacent-raider-test
  (let [make-state
        (fn [{:keys [raiders raiders-supply temples-supply passive?]
              :or {raiders-supply 4 temples-supply 5 passive? true}}]
          (let [p (make-player
                   {:raiders raiders
                    :raiders-supply raiders-supply
                    :temples-supply temples-supply
                    :roles {:raider 1 :priest 1 :merchant 1 :leader 1}
                    :bonus-board (assoc (vec (repeat 5 :covered))
                                        0 (if passive? :uncovered :covered))})]
            {:players {:alice p}
             :bonus-boards {:alice 13}
             :turn-order [:alice :bob :carol :dave]
             :magistrates {}}))]

    (testing "BUG 1: [13 4] surfaces a pick-city choice (was nil → auto-resolved)"
      (let [need (game/bonus-needs-choice? 13 4)]
        (is (some? need) "slot 5 must require a player choice")
        (is (= :pick-city (:type need)))
        (is (= :adjacent-to-raider (:filter need))
            "eligible cities are the raider-adjacent ones")))

    (testing "eligible cities = endpoints of routes the player has a raider on"
      (let [s (make-state {:raiders {[:eridu :uruk] [:raiding]
                                     [:kish :nippur] [:point]}})]
        (is (= #{:eridu :uruk :kish :nippur}
               (set (game/cities-adjacent-to-my-raiders s :alice))))))

    (testing "applying the choice places the temple in the chosen city"
      (let [s  (make-state {:raiders {[:eridu :uruk] [:raiding]}})
            s' (game/apply-bonus-with-choice s :alice 13 4 :uruk)]
        (is (= [:face-up] (get-in s' [:players :alice :temples :uruk]))
            "temple placed where the player chose (multi-temple vector)")
        (is (= 4 (get-in s' [:players :alice :temples-supply]))
            "temple supply 5 → 4")))

    (testing "BUG 2: placing that temple fires the slot-0 passive → raider adjacent"
      (let [s  (make-state {:raiders {[:eridu :uruk] [:raiding]}})
            s' (game/apply-bonus-with-choice s :alice 13 4 :uruk)
            raiders (get-in s' [:players :alice :raiders])]
        (is (= 2 (count raiders))
            "passive placed a second raider adjacent to the new temple")
        (is (contains? raiders [:babylon :uruk])
            "the new raider sits on another route touching Uruk")
        (is (= 3 (get-in s' [:players :alice :raiders-supply]))
            "raider supply 4 → 3 (one consumed by the passive)")))

    (testing "no passive (slot 0 covered) → temple placed but NO extra raider"
      (let [s  (make-state {:raiders {[:eridu :uruk] [:raiding]} :passive? false})
            s' (game/apply-bonus-with-choice s :alice 13 4 :uruk)]
        (is (= [:face-up] (get-in s' [:players :alice :temples :uruk])))
        (is (= 1 (count (get-in s' [:players :alice :raiders])))
            "passive gated off → raider count unchanged")
        (is (= 4 (get-in s' [:players :alice :raiders-supply]))
            "raider supply untouched")))))

(deftest board-effect-diagnostic-test
  ;; Pins both the COUNTS and the report SHAPE. If anyone adds a partial
  ;; without registering it, or marks a partial as :implemented, this fails.
  (let [d (game/board-effect-diagnostic)]

    (testing "the report has every expected field"
      (is (every? d [:total :implemented :partial :persistent
                     :needs-compound :needs-placement :needs-demand :conditional])
          "all fields populated (none nil)"))

    (testing "counts sum to total (the partition is a real partition)"
      (let [bucket-sum (reduce + (map d [:implemented :partial :persistent
                                         :needs-compound :needs-placement
                                         :needs-demand :conditional]))]
        (is (= (:total d) bucket-sum)
            "every entry must land in exactly one bucket")))

    (testing "concrete counts (legacy hand-map; eridu.effect-spec is now authoritative)"
      ;; These track the legacy effect-implementation-status map. The structured
      ;; eridu.effect-spec scaffold is now the source of truth; this map is kept
      ;; for board-effect-diagnostic / apply-bonus-effect logging. 23→22: [17 1]
      ;; moved partial→implemented when its raider-placement was fixed. 22→25:
      ;; [22 2]/[24 2]/[34 2] corrected implemented→partial (they are proxies that
      ;; need a data-model change; honest status after the bug-watcher pass).
      ;; 23→22: [34 2] moved partial→implemented when the multi-raider-per-route
      ;; model landed ("place a raider on each route you have a raider").
      (is (= 22 (:partial d))
          "22 entries still admit being partial in the legacy map")
      (is (pos? (:implemented d)) "at least some are faithfully implemented")
      (is (pos? (:persistent d))  "at least some passives are wired up"))))

;; =============================================================================
;; Persistence round-trip — replay reconstructs live play (within scope)
;; =============================================================================

(deftest state-serialization-round-trip-test
  ;; The persistence layer stores game state via pr-str and reads via read-string.
  ;; If any value in state isn't EDN-serializable (e.g. a function or Java object),
  ;; the round-trip silently corrupts the state. Pin the contract.
  (testing "initial-state EDN round-trips losslessly"
    (let [s (game/initial-state [:alice :bob])]
      (is (= s (read-string (pr-str s)))
          "every value in state must serialize and read back identical")))

  (testing "round-tripped state is still usable for stepping"
    (let [s  (game/initial-state [:alice :bob])
          s' (read-string (pr-str s))
          [_phase choices] (choice/find-state-raw s')]
      (is (seq choices)
          "round-tripped state still produces valid choices — not just byte-equal")))

  (testing "smaller, hand-built states also round-trip"
    ;; A safety against the case where initial-state happens to avoid an
    ;; unsafe value but other states might use it.
    (let [s {:players {:alice (make-player {:temples {:eridu [:face-down]}})}
             :magistrates {0 :eridu}
             :turn-order [:alice]}]
      (is (= s (read-string (pr-str s)))))))

(defn- step-and-record
  "Apply one deterministic forward step from `state`. Returns [next-state key]
   or nil if no choices remain. Mimics what live play does: pick a key, apply,
   advance through trivial phases (matching persist-e/replay's auto-advance)."
  [state]
  (let [[_phase choices] (choice/find-state-raw state)]
    (when (seq choices)
      (let [k         (first (sort-by str (keys choices)))
            next-state (get choices k)
            ;; Auto-advance through trivial phases, same as persist-e/replay
            advanced   (choice/advance-through-trivial
                        next-state game/human-protected-phases)]
        [advanced k]))))

(defn- play-and-record
  "Step forward up to `n` times, returning [final-state action-keys]."
  [s0 n]
  (loop [state s0
         acts  []
         i     0]
    (if (>= i n)
      [state acts]
      (if-let [[next-state k] (step-and-record state)]
        (recur next-state (conj acts k) (inc i))
        [state acts]))))

(deftest replay-reproducibility-test
  ;; THE round-trip property: same starting state + same actions → same result.
  ;; Bounded to within-turn so we don't trip advance-turn's roll-dice.
  (testing "live play and replay of the same 3 actions yield identical state"
    (let [s0 (game/initial-state [:alice :bob])
          [s-live actions] (play-and-record s0 3)
          s-replay (persist-e/replay s0 actions)]
      (is (seq actions) "sanity: we actually recorded some actions")
      (is (= s-live s-replay)
          "replay must reproduce live play step-for-step within a turn"))))

(deftest replay-halts-on-unknown-key-test
  ;; replay uses (reduced state) when a choice key isn't found. It must NOT
  ;; throw — bad data in the DB shouldn't crash the load path.
  (testing "an unknown first key halts at initial-state"
    (let [s0 (game/initial-state [:alice :bob])
          s' (persist-e/replay s0 [:definitely-not-a-real-choice])]
      (is (= s0 s')
          "no valid choice ever applied → replay returns initial state")))

  (testing "valid keys followed by an unknown one halt mid-stream"
    (let [s0 (game/initial-state [:alice :bob])
          [_ good-actions] (play-and-record s0 2)
          mixed-actions (conj (vec good-actions) :bogus-key)
          s' (persist-e/replay s0 mixed-actions)
          [s-after-good _] (play-and-record s0 2)]
      (is (= s-after-good s')
          "replay halts at the bogus key, returning state after the 2 good ones"))))

;; =============================================================================
;; Multi-raider-per-route model — accessors, conservation, [34 2], [25 :deployed]
;; =============================================================================
;; The data model is :raiders {route-key [status ...]} (a VECTOR of 0..N raiders
;; per route, mirroring the multi-temple model). These tests pin the invariants
;; that make raider logic correct across the whole engine.

(defn- raider-supply-of [s pk] (get-in s [:players pk :raiders-supply] 0))
(defn- raiders-on-board  [s pk] (game/total-raiders (get-in s [:players pk :raiders])))
(defn- raider-total      [s pk] (+ (raider-supply-of s pk) (raiders-on-board s pk)))

(deftest raider-accessor-helpers-test
  (let [raiders {[:eridu :uruk]   [:raiding :point]
                 [:kish :nippur]  [:point]}]
    (testing "raiders-on returns the status vector (empty for absent route)"
      (is (= [:raiding :point] (game/raiders-on raiders [:eridu :uruk])))
      (is (= [:point] (game/raiders-on raiders [:kish :nippur])))
      (is (= [] (game/raiders-on raiders [:babylon :uruk]))))
    (testing "raider-on-route? is true only when >= 1 raider present"
      (is (game/raider-on-route? raiders [:eridu :uruk]))
      (is (not (game/raider-on-route? raiders [:babylon :uruk]))))
    (testing "total-raiders sums vector lengths; count-with-status filters"
      (is (= 3 (game/total-raiders raiders)))
      (is (= 1 (game/count-raiders-with-status raiders :raiding)))
      (is (= 2 (game/count-raiders-with-status raiders :point))))))

(deftest raider-conservation-invariant-test
  ;; INVARIANT: for any player, (raiders on the board) + (raiders-supply) is
  ;; CONSTANT across place / flip / score / influence. Flips never change the
  ;; count; scoring moves a raider from board → supply (total preserved).
  (testing "PLACE: board count up by N, supply down by N (total constant)"
    (let [s0 (game/initial-state [:alice :bob])
          pk :alice
          total0 (raider-total s0 pk)
          ;; place a raider via the generic helper on two distinct routes
          s1 (-> s0
                 (game/place-one-raider pk [:eridu :uruk] :raiding)
                 (update-in [:players pk :raiders-supply] dec)
                 (game/place-one-raider pk [:kish :nippur] :raiding)
                 (update-in [:players pk :raiders-supply] dec))]
      (is (= 2 (raiders-on-board s1 pk)) "two raiders now on the board")
      (is (= total0 (raider-total s1 pk)) "place conserves the total")))

  (testing "FLIP: flipping :raiding->:point does NOT change any count"
    (let [s0 (-> (game/initial-state [:alice :bob])
                 (assoc-in [:players :alice :raiders]
                           {[:eridu :uruk] [:raiding :raiding] [:kish :nippur] [:raiding]}))
          pk :alice
          before-total (raider-total s0 pk)
          before-board (raiders-on-board s0 pk)
          [s1 n] (game/flip-raiders-on-route-to-point s0 pk [:eridu :uruk])]
      (is (= 2 n) "both raiders on the route were flipped")
      (is (= [:point :point] (game/raiders-on (get-in s1 [:players pk :raiders]) [:eridu :uruk])))
      (is (= before-board (raiders-on-board s1 pk)) "flip leaves board count unchanged")
      (is (= before-total (raider-total s1 pk)) "flip conserves the total")))

  (testing "SCORE: removes exactly the scored point raider, returns it to supply"
    (let [s0 (-> (game/initial-state [:alice :bob])
                 (assoc-in [:players :alice :raiders]
                           {[:eridu :uruk] [:point :raiding]})
                 (assoc-in [:players :alice :raiders-supply] 4))
          pk :alice
          total0 (raider-total s0 pk)
          s1 (game/score-one-point-raider s0 pk [:eridu :uruk])]
      (is (= [:raiding] (game/raiders-on (get-in s1 [:players pk :raiders]) [:eridu :uruk]))
          "the point raider was removed; the raiding one preserved")
      (is (= 5 (raider-supply-of s1 pk)) "supply 4 → 5 (scored raider returned)")
      (is (= total0 (raider-total s1 pk)) "score conserves the total")))

  (testing "INFLUENCE: magistrate sweep flips raiders without changing counts"
    (let [base (game/initial-state [:alice :bob :carol :dave])
          routes (game/board-routes base)
          all-raiding (into {} (map (fn [r] [(game/route-key (:from r) (:to r)) [:raiding]]) routes))
          s0 (-> base
                 (assoc-in [:players :alice :roles :leader] 5)
                 (assoc-in [:players :bob :raiders] all-raiding)
                 (assoc :magistrates {0 (first (keys (:city-graph base)))})
                 (assoc-in [:turn-stats :player] :alice))
          before-bob (raider-total s0 :bob)
          choices (choice/resolve-influence-choices s0)
          after-states (vals choices)]
      (is (every? #(= before-bob (raider-total % :bob)) after-states)
          "every resulting influence state conserves bob's raider total (flips only)"))))

(deftest board-34-slot-2-raider-on-each-occupied-route-test
  ;; "Place a Raider on each route you already have a Raider." Faithful multi-
  ;; raider model: add ONE raider to every route currently holding >= 1.
  (testing "adds a second raider to each already-occupied route (conj, supply --)"
    (let [s0 (-> (game/initial-state [:alice :bob])
                 (assoc-in [:players :alice :roles :raider] 5) ;; high cap so all fit
                 (assoc-in [:players :alice :raiders]
                           {[:eridu :uruk] [:raiding] [:kish :nippur] [:point]})
                 (assoc-in [:players :alice :raiders-supply] 6)
                 (assoc-in [:bonus-boards :alice] 34))
          s' (game/apply-bonus-effect s0 :alice 34 2)
          rs (get-in s' [:players :alice :raiders])]
      (is (= 2 (count (game/raiders-on rs [:eridu :uruk])))
          "the raiding route now holds two raiders")
      (is (= 2 (count (game/raiders-on rs [:kish :nippur])))
          "the point route now holds two raiders")
      (is (= [:point :raiding] (game/raiders-on rs [:kish :nippur]))
          "the original :point raider is NOT clobbered; the new one is :raiding")
      (is (= 4 (raiders-on-board s' :alice)) "4 raiders total on board (2 added)")
      (is (= 4 (raider-supply-of s' :alice)) "supply 6 → 4 (two consumed)")))

  (testing "no raiders on the board → no-op"
    (let [s0 (-> (game/initial-state [:alice :bob])
                 (assoc-in [:players :alice :raiders] {})
                 (assoc-in [:bonus-boards :alice] 34))
          s' (game/apply-bonus-effect s0 :alice 34 2)]
      (is (zero? (raiders-on-board s' :alice)) "nothing placed when no route is occupied"))))

(deftest board-25-deployed-two-raiders-per-path-test
  ;; [25 0] passive "you may have two raiders on each path" sets
  ;; :allow-double-raiders so resolve-deploy-choices offers an already-occupied
  ;; adjacent route. A 2nd raider is conj'd (supply --), the first untouched; a
  ;; 3rd is never allowed.
  (let [setup
        (fn [allow?]
          (-> (game/initial-state [:alice :bob])
              (assoc-in [:players :alice :roles :raider] 5)
              ;; put alice's caravan at uruk with a raider already on an adjacent route
              (assoc-in [:players :alice :caravan] :uruk)
              (assoc-in [:players :alice :raiders] {[:eridu :uruk] [:point]})
              (assoc-in [:players :alice :raiders-supply] 5)
              (assoc-in [:players :alice :allow-double-raiders] allow?)
              (assoc :current-player-idx 0)
              (assoc :turn-order [:alice :bob])
              (assoc :player-turn {:phase :resolve-deploy :space 7
                                   :deploys-left 2 :actions-remaining 1})))]

    (testing "WITHOUT the passive: the already-occupied route is NOT placeable"
      (let [choices (choice/resolve-deploy-choices (setup false))]
        (is (not (contains? choices [:eridu :uruk]))
            "one-raider-per-route rule still holds when passive is off")))

    (testing "WITH the passive: a 2nd raider conj's onto the occupied route"
      (let [s0 (setup true)
            choices (choice/resolve-deploy-choices s0)]
        (is (contains? choices [:eridu :uruk])
            "board 25 makes the occupied adjacent route placeable")
        (let [s1 (get choices [:eridu :uruk])
              rs (get-in s1 [:players :alice :raiders])]
          (is (= [:point :raiding] (game/raiders-on rs [:eridu :uruk]))
              "2nd raider conj'd as :raiding; the original :point NOT clobbered")
          (is (= 4 (raider-supply-of s1 :alice)) "supply 5 → 4 (decremented exactly once)")
          (is (= 2 (raiders-on-board s1 :alice)) "two raiders now on the board"))))

    (testing "a 3rd raider on the same path is never offered (cap 2 per route)"
      (let [s0 (-> (setup true)
                   (assoc-in [:players :alice :raiders] {[:eridu :uruk] [:point :raiding]}))
            choices (choice/resolve-deploy-choices s0)]
        (is (not (contains? choices [:eridu :uruk]))
            "route already at 2 raiders → not placeable even with the passive")))))
