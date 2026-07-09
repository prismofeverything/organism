(ns journeymen-e.game-test
  "Day-1 invariants for the Journeymen E sim (spec: journeymen-e.md round 3)."
  (:require [clojure.test :refer [deftest testing is]]
            [journeymen-e.game :as g]
            [journeymen-e.bots :as bots]))

(defn- run-game [seed picks]
  (bots/play-game seed picks (vec (take (count picks) (cycle bots/archetypes)))))

(deftest board-shape
  (doseq [seed [11 23 77 104 555 8912]]
    (let [st (g/new-game seed [:blacksmith :goldsmith :jeweler])
          ws (:wedge-order st)
          inner-rep (fn [w] (if (= :switch (:inner w)) (:outer w) (:inner w)))
          outer-idx (vec (filter #(some? (:outer (nth ws %))) (range 6)))
          inner-idx (vec (filter #(some? (inner-rep (nth ws %))) (range 6)))
          ai (first (filter #(nil? (:outer (nth ws %))) (range 6)))
          prev-o (:outer (nth ws (mod (+ ai 5) 6)))
          next-o (:outer (nth ws (mod (inc ai) 6)))]
      (testing "the designer's wedge table: 6 wedges (order-shuffled), 8 nodes"
        (is (= 8 (count g/spaces)))
        (is (= 6 (count ws)))
        (is (= (set (range 8))
               (set (concat (keep :outer ws) (remove #{:switch} (keep :inner ws)))))
            "every space sits on the outer or inner track exactly once")
        (is (= 1 (count (filter :cross ws))) "exactly one cross wedge (Tavern/Castle)")
        (is (= 1 (count (remove :outer ws))) "exactly ONE no-outer wedge")
        (is (= 0 (:inner (first (remove :outer ws)))) "…and its inner is the Academy (switch)")
        (is (= [4] (mapv :outer (filter #(= :switch (:inner %)) ws)))
            "the Manor is the one :switch-inner wedge")
        (is (= #{1 5} (set (keep #(when (and (:outer %) (nil? (:inner %))) (:outer %)) ws)))
            "Docks + Barracks are OUTER-ONLY wedges (the inner ring skips them)"))
      (testing "outer fast lane: consecutive OUTERS connect (the Academy wedge is skipped)"
        (doseq [k (range (count outer-idx))]
          (let [a (:outer (nth ws (nth outer-idx k)))
                b (:outer (nth ws (nth outer-idx (mod (inc k) (count outer-idx)))))]
            (is (contains? (g/fwd st a) b) "outer(i) → next outer, 1 road"))))
      (testing "the Academy detour pocket: prev outer reaches BOTH next outer and Academy in 1"
        (is (contains? (g/fwd st prev-o) next-o) "the fast lane skips the Academy wedge")
        (is (contains? (g/fwd st prev-o) 0) "…but branches INTO the Academy (1 road)")
        (is (contains? (g/fwd st 0) next-o) "the Academy rejoins the next outer in 1"))
      (testing "inner ring: consecutive inner-reps (Academy/Parks/Manor/Castle only)"
        (is (= #{0 2 4 6} (set (map #(inner-rep (nth ws %)) inner-idx))))
        (doseq [k (range (count inner-idx))]
          (let [a (inner-rep (nth ws (nth inner-idx k)))
                b (inner-rep (nth ws (nth inner-idx (mod (inc k) (count inner-idx)))))]
            (is (contains? (g/fwd st a) b) "inner-rep(i) → next inner-rep"))))
      (testing "the cross wedge adds inner → outer (Castle → Tavern)"
        (is (contains? (g/fwd st 6) 7)))
      (testing "movement = 1-2 ROADS forward, directed only"
        (doseq [n (range (count g/spaces))]
          (is (seq (g/fwd st n)) "every node has a forward road")
          (is (= (g/reachable st n)
                 (disj (into (g/fwd st n) (mapcat #(g/fwd st %) (g/fwd st n))) n)))
          (doseq [t (g/reachable st n)]
            (is (<= (g/road-distance st n t) 2) "reachable ⊆ 2 directed roads"))))
      (testing "the board is a strongly-connected circuit (every node reaches every node)"
        (doseq [a (range (count g/spaces)) b (range (count g/spaces))]
          (is (some? (g/road-distance st a b)))))
      (testing "pickup adjacency = UNDIRECTED road neighbours"
        (doseq [n (range (count g/spaces)) m (g/undirected-neighbors st n)]
          (is (or (contains? (g/fwd st n) m) (contains? (g/fwd st m) n)))))))
  (testing "THE DESIGNER'S PAIRING TABLE (2026-07-03) — table-exact"
    (is (= ["Academy" "Docks" "Parks" "Temple" "Manor" "Barracks" "Castle" "Tavern"]
           (mapv :name g/spaces)))
    (is (= [{:req :innovation :opt :precision}    ; Academy
            {:req :precision  :opt :innovation}   ; Docks
            {:req :innovation :opt :durability}   ; Parks
            {:req :precision  :opt :durability}   ; Temple
            {:req :luxury}                        ; Manor
            {:req :durability :opt :precision}    ; Barracks
            {:req :luxury}                        ; Castle
            {:req :durability :opt :innovation}]  ; Tavern (D→I; both-required retired 2026-07-03)
           (mapv :demand g/spaces)))
    (is (= [:recipe-worker :shop :shop :atelier-pts :recipe-coin :shop :atelier-coin :skilled]
           (mapv :action g/spaces)))
    (is (= [nil #{:black :blue} #{:white :grey} nil nil #{:red :yellow} nil nil]
           (mapv :take g/spaces)))
    ;; the 6 D/P/I demands are the COMPLETE directed-K3 pair set (all 6 ordered pairs)
    (is (= #{[:innovation :precision] [:precision :innovation] [:innovation :durability]
             [:precision :durability] [:durability :precision] [:durability :innovation]}
           (set (for [s g/spaces :let [d (:demand s)]
                      :when (not= :luxury (:req d))]
                  [(:req d) (:opt d)]))))))

(deftest starting-coins-setup-and-blocking
  (testing "starting coins = green's 2 + seat index"
    (is (= [2 3 4] (mapv :coins (:players (g/new-game 9 [:blacksmith :alchemist :goldsmith]))))))
  (testing "BOTH setup cart placements build a free basic shop (green's :place-cart)"
    (let [st (g/new-game 9 [:blacksmith :goldsmith])
          ;; seat 0 places both carts at distinct spaces with room
          [a b] (take 2 (filter #(pos? (g/node-capacity st %)) (range 8)))
          st (g/apply-move st {:type :place-cart :node a})   ; P0 cart 1
          st (g/apply-move st {:type :place-cart :node a})   ; P1 cart 1
          st (g/apply-move st {:type :place-cart :node b})   ; P0 cart 2
          st (g/apply-move st {:type :place-cart :node b})]  ; P1 cart 2
      (is (= 4 (get-in st [:players 0 :shops-left])) "P0 placed 2 free shops")
      (is (g/player-shop-at? st 0 a))
      (is (g/player-shop-at? st 0 b) "the SECOND cart also brought a shop")))
  (testing "shop blocking (green CHANGE 4): 2p→12, 3p→4, 4p→0, ≤2 per space"
    (doseq [[n total] [[2 12] [3 4] [4 0]]]
      (let [st (g/new-game 31 (vec (take n [:blacksmith :alchemist :goldsmith :jeweler])))]
        (is (= total (reduce + 0 (vals (:blocked st)))) (str n "p blocks " total))
        (is (every? #(<= % 2) (vals (:blocked st))) "at most 2 blocks per space")
        (doseq [t (range 8)]
          (is (= (- 4 (get (:blocked st) t 0)) (g/node-capacity st t))
              "capacity = 4 minus blocked")))))
  (testing "a fully-blocked-to-capacity space refuses more basics"
    (let [st (-> (g/new-game 31 [:blacksmith :goldsmith])
                 (assoc :blocked {0 2})
                 (assoc-in [:board-shops 0 1] [{:level :basic} {:level :basic}])
                 (assoc :phase :play :step :main :active-cart 0 :current 0)
                 (assoc-in [:players 0 :carts 0] 1)   ; Docks — a :shop-action space
                 (assoc-in [:players 0 :coins] 9))
          places (set (map :node (filter :place? (filter #(= :action-shop (:type %))
                                                         (g/enumerate-moves st)))))]
      (is (not (contains? places 0)) "space 0 is at its reduced capacity (4-2=2)"))))

(deftest conservation-and-legality
  (doseq [seed [3 17 42 88 271]]
    (let [picks [:blacksmith :alchemist :goldsmith]
          ;; drive the game move-by-move, checking invariants throughout
          final
          (loop [st (g/new-game seed picks) guard 0 skilled-claimed 0]
            (if (or (g/game-over? st) (> guard 100000))
              (do (is (g/game-over? st) "game completes") st)
              (let [mv (bots/decide st (nth bots/archetypes (mod (:current st) 3)))
                    _ (is (some? mv) "a legal move always exists")
                    skilled-claimed (if (= :action-skilled (:type mv)) (inc skilled-claimed) skilled-claimed)
                    st' (g/apply-move st mv)]
                ;; goods conservation: pool + player tokens + tracks == initial 42
                (let [pool (reduce + (vals (:goods st')))
                      tokens (reduce + (for [p (:players st') [_ n] (:tokens p)] n))
                      tracks (reduce + (vals (:tracks st')))]
                  (is (= 42 (+ pool tokens tracks))
                      (str "goods conserved (pool " pool " + tokens " tokens " + tracks " tracks ")")))
                ;; worker conservation: board + hands == 24 + skilled claims
                (let [board (reduce + (map count (vals (:board-workers st'))))
                      hands (reduce + (map (comp count :workers) (:players st')))]
                  (is (= (+ 24 skilled-claimed) (+ board hands)) "workers conserved"))
                ;; tracks never exceed threshold + overfill
                (doseq [[gd n] (:tracks st')]
                  (is (<= n (+ (g/track-threshold gd) g/track-overfill))
                      (str (name gd) " track within cap")))
                ;; coins/score never negative
                (doseq [p (:players st')]
                  (is (>= (:coins p) 0) "coins non-negative")
                  (is (>= (:score p) 0) "score non-negative"))
                (recur st' (inc guard) skilled-claimed))))]
      (is (pos? (reduce + (vals (:tracks final)))) "the clock actually advanced"))))

(deftest sell-rules
  (let [st (-> (g/new-game 5 [:blacksmith :goldsmith])
               (assoc :phase :play :step :main :active-cart 0 :current 0))
        node 5   ; Barracks: demand D-req P-opt, per the designer's table
        st (-> st (assoc-in [:players 0 :carts 0] node)
               (assoc-in [:players 0 :tokens] {:precision 2}))]
    (testing "required must be sold before optional (no D tokens -> no sell at D-req)"
      (let [st (assoc-in st [:board-shops node 0] [{:level :basic}])]
        (is (empty? (filter #(= :sell (:type %)) (g/enumerate-moves st))))))
    (testing "no shop here -> no sell even with required tokens"
      (let [st (assoc-in st [:players 0 :tokens] {:durability 2})]
        (is (empty? (filter #(= :sell (:type %)) (g/enumerate-moves st))))))
    (testing "with shop + required: sells offered with a track choice per sold good"
      (let [st (-> st (assoc-in [:board-shops node 0] [{:level :basic}])
                   (assoc-in [:players 0 :tokens] {:durability 2 :precision 1}))
            sells (filter #(= :sell (:type %)) (g/enumerate-moves st))]
        (is (= #{:durability :precision} (set (map :track sells))) "seller CHOOSES the tracked good")
        (let [mv (first (filter #(= :durability (:track %)) sells))
              st' (g/apply-move st mv)]
          (is (= 1 (get-in st' [:tracks :durability])) "one token fed the track")
          (is (= (+ (get-in st [:goods :durability]) 1)
                 (get-in st' [:goods :durability])) "the OTHER durability returned to the pool")
          (is (= (+ (get-in st [:goods :precision]) 1)
                 (get-in st' [:goods :precision])) "optional returned to the pool")
          (is (= 2 (get-in st' [:players 0 :skills :durability])) "required leveled once")
          (is (= 2 (get-in st' [:players 0 :skills :precision])) "optional leveled once"))))))

(deftest dividends-stack-and-pay-owner
  (let [st (-> (g/new-game 5 [:blacksmith :goldsmith])
               (assoc :phase :play :step :main :active-cart 0 :current 1)
               (assoc-in [:players 1 :carts 0] 5)   ; Barracks: D-req
               (assoc-in [:board-shops 5 1] [{:level :basic}])
               (assoc-in [:players 1 :tokens] {:durability 1})
               ;; player 0 has TWO mastercrafts displayed on the durability track
               (assoc-in [:displays :durability] [{:pid 0} {:pid 0}]))
        sc0 (get-in st [:players 0 :score]) c0 (get-in st [:players 0 :coins])
        mv (first (filter #(and (= :sell (:type %)) (= :durability (:track %)))
                          (g/enumerate-moves st)))
        st' (g/apply-move st mv)]
    (is (= (+ sc0 2) (get-in st' [:players 0 :score])) "stacked displays pay +2 points")
    (is (= (+ c0 2) (get-in st' [:players 0 :coins])) "and +2 coins — off an OPPONENT'S sale")))

(deftest clock-end-trigger
  (let [st (-> (g/new-game 5 [:blacksmith :goldsmith])
               (assoc :phase :play :step :main :active-cart 0 :current 0 :round 4)
               (assoc-in [:players 0 :carts 0] 5)   ; Barracks: D-req
               (assoc-in [:board-shops 5 0] [{:level :basic}])
               (assoc-in [:players 0 :tokens] {:durability 1})
               (assoc-in [:tracks :precision] 5)      ; one track already filled
               (assoc-in [:tracks :durability] 4))    ; this sale fills the second
        mv (first (filter #(and (= :sell (:type %)) (= :durability (:track %)))
                          (g/enumerate-moves st)))
        st' (g/apply-move st mv)]
    (is (:ending st') "second filled track flags the ending")
    (testing "overfill: a filled track accepts +2 then stops tracking"
      (let [st2 (-> st' (assoc :step :main :current 0)
                    (assoc-in [:players 0 :tokens] {:durability 3}))
            sells (filter #(= :sell (:type %)) (g/enumerate-moves st2))]
        (is (some #(= :durability (:track %)) sells) "still trackable inside overfill")
        (let [st3 (g/apply-move st2 (first (filter #(= :durability (:track %)) sells)))]
          (is (= 6 (get-in st3 [:tracks :durability])))
          (let [st4 (-> st3 (assoc :step :main :current 0)
                        (assoc-in [:players 0 :tokens] {:durability 2}))
                s4 (filter #(= :sell (:type %)) (g/enumerate-moves st4))
                st5 (g/apply-move st4 (first (filter #(= :durability (:track %)) s4)))
                st6 (-> st5 (assoc :step :main :current 0)
                        (assoc-in [:players 0 :tokens] {:durability 1}))
                s6 (filter #(= :sell (:type %)) (g/enumerate-moves st6))]
            (is (= 7 (get-in st5 [:tracks :durability])) "overfill cap 5+2")
            (is (= [nil] (map :track s6)) "beyond overfill: sale legal but untracked")))))))

(deftest medallions-are-the-only-master-source
  ;; blacksmith medallions are now #{[:precision 3] [:innovation 4] [:innovation 6]}
  (let [st (-> (g/new-game 5 [:blacksmith :goldsmith])
               (assoc :phase :play :step :main :active-cart 0 :current 0)
               (assoc-in [:players 0 :carts 0] 1)   ; Docks: precision-req
               (assoc-in [:board-shops 1 0] [{:level :basic}])
               (assoc-in [:players 0 :skills :precision] 2)   ; next level = 3 = medallion
               (assoc-in [:players 0 :tokens] {:precision 1}))
        mv (first (filter #(= :sell (:type %)) (g/enumerate-moves st)))
        st' (g/apply-move st mv)]
    (is (= 3 (get-in st' [:players 0 :skills :precision])))
    (is (= 1 (get-in st' [:players 0 :master-recipes])) "precision r3 medallion granted a master recipe")
    (let [st'' (g/apply-move (-> st' (assoc :step :main :current 0)
                                 (assoc-in [:players 0 :skills :precision] 2)
                                 (assoc-in [:players 0 :tokens] {:precision 1}))
                             mv)]
      (is (= 1 (get-in st'' [:players 0 :master-recipes])) "each medallion fires once"))))

(deftest full-games-sane
  (doseq [seed [101 202 303 404]]
    (let [st (run-game seed [:blacksmith :alchemist :goldsmith])]
      (is (g/game-over? st))
      (is (<= 2 (count (filter (fn [[gd n]] (>= n (g/track-threshold gd))) (:tracks st))))
          "ended via the track clock (2 filled)")
      (is (every? #(>= (:score %) 0) (:players st))))))

;; ── 2026-07-02 playtest-feedback features ─────────────────────────────────────

(defn- mid-play
  "A hand-built :play state for player 0 (guilds as given), cart 0 at `node`."
  [guilds node & {:keys [step] :or {step :main}}]
  (-> (g/new-game 5 guilds)
      (assoc :phase :play :step step :active-cart 0 :current 0)
      (assoc-in [:players 0 :carts 0] node)))
(defn- moves-of [st t] (filter #(= t (:type %)) (g/enumerate-moves st)))

(deftest tavern-ordered-pair
  ;; Tavern is the D→I ordered pair (designer corrected "D+I" both-required to
  ;; D→I 2026-07-03 — completing the directed-K3 pair set). Required Durability,
  ;; optional Innovation rides along; precision/luxury never do.
  (let [base (-> (mid-play [:blacksmith :goldsmith] 7)   ; Tavern
                 (assoc-in [:board-shops 7 0] [{:level :basic}]))]
    (testing "no Durability -> NO sale even holding Innovation"
      (is (empty? (moves-of (assoc-in base [:players 0 :tokens] {:innovation 2}) :sell))))
    (testing "Durability required first; Innovation optional rides along"
      (let [st (assoc-in base [:players 0 :tokens]
                         {:durability 2 :innovation 1 :precision 3 :luxury 1})
            sells (moves-of st :sell)]
        (is (seq sells) "required D held -> sale offered")
        (is (every? #(= {:durability 2 :innovation 1} (:goods %)) sells)
            "D (required) + I (optional) sell; precision/luxury do NOT ride along")
        (is (= #{:durability :innovation} (set (map :track sells)))
            "tracked-copy CHOICE spans the sold goods")
        (let [st' (g/apply-move st (first (filter #(= :innovation (:track %)) sells)))]
          (is (= 2 (get-in st' [:players 0 :skills :durability])) "D levels once")
          (is (= 2 (get-in st' [:players 0 :skills :innovation])) "I levels once")
          (is (= 1 (get-in st' [:tracks :innovation])) "the chosen copy fed its track")
          (is (= 3 (get-in st' [:players 0 :tokens :precision])) "unsold goods stay in hand"))))
    (testing "Durability alone (no Innovation) still sells — optional not required"
      (let [st (assoc-in base [:players 0 :tokens] {:durability 1})]
        (is (seq (moves-of st :sell)) "D-only sale is legal at Tavern")))))

(deftest shop-worker-take-colors
  ;; the shop-action follow-up may only take the ACTION space's :take colours
  ;; (designer table: Docks black/blue, Parks white/grey, Barracks red/yellow),
  ;; from ANY of your shop locations
  (let [st (-> (mid-play [:blacksmith :goldsmith] 5)   ; Barracks: :take #{:red :yellow}
               (assoc-in [:board-shops 5 0] [{:level :basic}])
               (assoc-in [:board-shops 1 0] [{:level :basic}])
               (assoc-in [:board-workers 5] [:red :black :white])
               (assoc-in [:board-workers 1] [:yellow :blue :skilled-red]))
        st' (g/apply-move st (first (filter #(not (:place? %)) (moves-of st :action-shop))))]
    (is (= :shop-worker (:step st')))
    (is (= 5 (:shop-action-node st')) "the acting node is remembered")
    (is (= #{[5 :red] [1 :yellow] [1 :skilled-red]}
           (set (map (juxt :from :color) (moves-of st' :shop-worker))))
        "only :take colours offered — skilled tokens count as their BASE colour")
    (let [st'' (g/apply-move st' (first (moves-of st' :shop-worker)))]
      (is (nil? (:shop-action-node st'')) "acting node cleared after the step"))
    (testing "the designer's colour pairing on the three shop spaces"
      (is (= [#{:black :blue} #{:white :grey} #{:red :yellow}]
             (mapv #(:take (nth g/spaces %)) [1 2 5]))))))

(deftest master-pool-is-a-race
  (let [st (mid-play [:blacksmith :goldsmith] 0 :step :craft)]
    (is (= 14 (count (:master-pool st))) "the full pool is visible from the start")
    (testing "no medallion claim -> no build, even with workers"
      (let [st (assoc-in st [:players 0 :workers] [:white :white :grey :grey])]
        (is (empty? (moves-of st :craft-master)))))
    (testing "a claim + a payable token cost -> one targeted move per token"
      (let [st (-> st (assoc-in [:players 0 :master-recipes] 1)
                   (assoc-in [:players 0 :workers] [:white :white]))
            ms (moves-of st :craft-master)]
        (is (= #{:mw1 :mw10} (set (map :id ms)))
            "[:white :white] pays exactly the two [:white :white] tokens")
        (let [st' (g/apply-move st (first (filter #(= :mw1 (:id %)) ms)))]
          (is (= 13 (count (:master-pool st'))) "claimed token REMOVED for everyone")
          (is (not-any? #(= :mw1 (:id %)) (:master-pool st')))
          (is (zero? (get-in st' [:players 0 :master-recipes])) "claim spent")
          (is (= 1 (get-in st' [:players 0 :mastercrafts-built])))
          (is (empty? (get-in st' [:players 0 :workers])) "the token's cost was paid"))))))

(deftest recipe-deck-is-greens
  (let [st (g/new-game 7 [:blacksmith :goldsmith])
        full (into (vec (:recipe-market st)) (:recipe-deck st))]
    (is (= 29 (count full)) "29 cards across 4 decay stacks")
    (is (= (concat (repeat 7 :a) (repeat 9 :b) (repeat 8 :c) (repeat 5 :d)) (map :era full))
        "era order A→B→C→D, shuffled only WITHIN each stack")
    (is (= 10 (count (filter #(contains? (:flags % #{}) :chain) full)))
        "10 chains across the 4 stacks")))

(deftest recipe-decay
  ;; Mohammad 2026-07-07: first C picked clears leftover A; first D picked clears leftover B.
  (let [mk-take (fn [st era]  ;; force an `era` card into market slot 0, take it
                  (let [r (assoc {:inputs [:grey] :outputs []} :era era :id (keyword (str (name era) "x")))
                        st (-> st (assoc :phase :play :step :main :current 0 :active-cart 0)
                               (assoc-in [:players 0 :carts 0] 4)  ; Manor = recipe-coin
                               (assoc-in [:players 0 :coins] 4)
                               (assoc-in [:recipe-market 0] r))]
                    (g/apply-move st (first (filter #(and (= 0 (:idx %)) (= :coins (:pay %)))
                                                    (g/enumerate-moves st))))))
        base (-> (g/new-game 5 [:blacksmith :goldsmith])
                 ;; a market with one card of each stack (indices 1-4), plus slot 0 to take from
                 (assoc :recipe-market [nil
                                        (assoc {:inputs [:grey] :outputs []} :era :a :id :aA)
                                        (assoc {:inputs [:grey] :outputs []} :era :b :id :bB)
                                        (assoc {:inputs [:grey] :outputs []} :era :c :id :cC)
                                        (assoc {:inputs [:grey] :outputs []} :era :d :id :dD)]))]
    (testing "taking a C recipe clears the A card, keeps B/C/D"
      (let [st' (mk-take base :c)
            eras (set (keep :era (:recipe-market st')))]
        (is (:a-cleared st'))
        (is (not (contains? eras :a)) "A cleared")
        (is (every? eras [:b :c :d]) "B, C, D remain")))
    (testing "taking a D recipe clears the B card"
      (let [st' (mk-take base :d)
            eras (set (keep :era (:recipe-market st')))]
        (is (:b-cleared st'))
        (is (not (contains? eras :b)) "B cleared")
        (is (contains? eras :a) "A untouched (its trigger is C, not D)")))
    (testing "taking an A or B recipe triggers no decay"
      (is (not (:a-cleared (mk-take base :a))))
      (is (not (:b-cleared (mk-take base :b)))))))

(deftest recipe-slot-board
  (testing "green's slot board: 8 slots, base recipes seeded at the even slots"
    (let [slots (get-in (g/new-game 5 [:blacksmith :goldsmith]) [:players 0 :recipe-slots])]
      (is (= 8 (count slots)))
      (is (= [:base-d nil :base-p nil :base-i nil :base-l nil] (mapv #(some-> % :id) slots)))))
  (testing "take-recipe carries the CHOSEN target slot and covers it (base recipes included)"
    (let [st (-> (mid-play [:blacksmith :goldsmith] 4)   ; Manor = recipe-coin
                 (assoc-in [:players 0 :coins] 4))
          takes (moves-of st :action-recipe)]
      (is (= (set (range 8)) (set (map :slot takes))) "every slot is a legal target")
      (let [mv (first (filter #(and (= 0 (:idx %)) (= 3 (:slot %))) takes))
            card (nth (:recipe-market st) 0)
            st' (g/apply-move st mv)]
        (is (= (:id card) (get-in st' [:players 0 :recipe-slots 3 :id])) "the card sits in slot 3")
        (is (= (:id (nth (:recipe-deck st) 0)) (:id (nth (:recipe-market st') 0)))
            "the taken slot refills from the deck")
        ;; MARKET GROWTH (2026-07-05): a take refills the slot AND adds a 2nd card
        (is (= (inc (count (:recipe-market st))) (count (:recipe-market st')))
            "the market grew by one")
        (is (= (:id (nth (:recipe-deck st) 1)) (:id (last (:recipe-market st'))))
            "the 2nd deck card is appended to the market")
        (is (= (- (count (:recipe-deck st)) 2) (count (:recipe-deck st'))) "two drawn from the deck"))
      (let [mv (first (filter #(= 0 (:slot %)) takes))
            st' (g/apply-move st mv)]
        (is (not= :base-d (get-in st' [:players 0 :recipe-slots 0 :id]))
            "covering a BASE recipe is legal (a real trade-off)")))))

(deftest recipe-flag-effects
  (let [base (mid-play [:blacksmith :goldsmith] 0 :step :craft)
        craft-at (fn [st j] (first (filter #(= j (:slot %)) (moves-of st :craft))))]
    (testing ":bonus-skill levels the OUTPUT good's skill on craft (and :once? covers the slot)"
      (let [st (-> base
                   (assoc-in [:players 0 :workers] [:black :white])
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :bs :inputs [:black :white] :outputs [:durability]
                              :flags #{:bonus-skill} :once? true}))
            st' (g/apply-move st (craft-at st 1))]
        (is (= 2 (get-in st' [:players 0 :skills :durability])))
        (is (nil? (get-in st' [:players 0 :recipe-slots 1])) "the :once? slot is EMPTY again")))
    (testing ":lowest — a UNIQUE lowest raises immediately (no prompt)"
      (let [st (-> base
                   (assoc-in [:players 0 :skills]
                             {:durability 3 :precision 1 :innovation 2 :luxury 2})
                   (assoc-in [:players 0 :workers] [:white :grey])
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :lo :inputs [:white :grey] :outputs [] :flags #{:lowest}}))
            st' (g/apply-move st (craft-at st 1))]
        (is (= 2 (get-in st' [:players 0 :skills :precision])) "the unique lowest raised")
        (is (not= :choose-lowest (:step st')))))
    (testing ":lowest — a TIE lets the PLAYER choose which tied-lowest skill (Mohammad 2026-07-05)"
      (let [st (-> base
                   (assoc-in [:players 0 :skills]
                             {:durability 3 :precision 1 :innovation 1 :luxury 1})
                   (assoc-in [:players 0 :workers] [:white :grey])
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :lo :inputs [:white :grey] :outputs [] :flags #{:lowest}}))
            st' (g/apply-move st (craft-at st 1))]
        (is (= :choose-lowest (:step st')) "tie → the choose-lowest step")
        (is (= #{:precision :innovation :luxury} (set (map :skill (moves-of st' :choose-lowest))))
            "each tied-lowest skill offered")
        (let [st2 (g/apply-move st' {:type :choose-lowest :skill :luxury})]
          (is (= 2 (get-in st2 [:players 0 :skills :luxury])) "the chosen skill raised")
          (is (= 1 (:current st2)) "then the turn ends"))))
    (testing ":coin-per-good — +1 coin per DISTINCT NON-LUXURY good (D/P/I); LUXURY excluded"
      (let [st (-> base
                   (assoc-in [:players 0 :workers] [:grey])
                   ;; holds all 4 goods incl. luxury — only the 3 D/P/I should count
                   (assoc-in [:players 0 :tokens] {:durability 2 :precision 1 :innovation 3 :luxury 9})
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :cpg :inputs [:grey] :outputs [] :flags #{:coin-per-good} :once? true}))
            c (get-in st [:players 0 :coins])
            st' (g/apply-move st (craft-at st 1))]
        (is (= (+ c 3) (get-in st' [:players 0 :coins])) "3 D/P/I → +3, luxury ignored"))
      (let [st (-> base (assoc-in [:players 0 :workers] [:grey])
                   (assoc-in [:players 0 :tokens] {:durability 1 :luxury 5})  ; only 1 D/P/I + luxury
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :cpg :inputs [:grey] :outputs [] :flags #{:coin-per-good} :once? true}))
            c (get-in st [:players 0 :coins])]
        (is (= (+ c 1) (get-in (g/apply-move st (craft-at st 1)) [:players 0 :coins]))
            "1 D/P/I → +1 (luxury doesn't pad it)")))
    (testing ":grey-swap — spent workers leave the game; 2 grey drop on the space"
      (let [st (-> base
                   (assoc-in [:players 0 :workers] [:grey :red])   ; pays the two grey slots
                   (assoc-in [:board-workers 0] [])
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :gsw :inputs [:grey :grey] :outputs [] :flags #{:grey-swap} :once? true}))
            st' (g/apply-move st (craft-at st 1))]
        (is (= 2 (count (filter #{:grey} (get-in st' [:board-workers 0])))) "2 fresh grey dropped here")
        (is (empty? (filter #{:red} (get-in st' [:board-workers 0]))) "the spent red left the game (no recycle)")))
    (testing ":free-atelier — upgrade a chosen basic to an atelier free, then unlock an ability"
      (let [st (-> base
                   (assoc-in [:players 0 :workers] [:grey :red])
                   (assoc-in [:board-shops 0 0] [{:level :basic}])
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :fa :inputs [:grey :grey] :outputs [] :flags #{:free-atelier} :once? true}))
            st' (g/apply-move st (craft-at st 1))]
        (is (= :free-atelier (:step st')))
        (let [st2 (g/apply-move st' (first (filter #(= 0 (:basic-node %)) (moves-of st' :free-atelier))))]
          (is (= :ability (:step st2)) "upgrading opens the ability choice")
          (is (some #(= :atelier (:level %)) (get-in st2 [:board-shops 0 0])) "the basic is now an atelier"))))
    (testing ":free-shop queues a FREE placement step, then the turn ends"
      (let [st (-> base
                   (assoc-in [:players 0 :workers] [:yellow :grey])
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :fs :inputs [:yellow :grey] :outputs [:luxury]
                              :flags #{:free-shop} :once? true}))
            st' (g/apply-move st (craft-at st 1))]
        (is (= :free-shop (:step st')))
        (is (seq (moves-of st' :place-free-shop)))
        (is (= 1 (count (moves-of st' :skip-free-shop))) "always skippable")
        (let [coins (get-in st' [:players 0 :coins])
              st2 (g/apply-move st' (first (moves-of st' :place-free-shop)))]
          (is (= coins (get-in st2 [:players 0 :coins])) "placement costs nothing")
          (is (= 5 (get-in st2 [:players 0 :shops-left])) "one shop off the supply (no setup ran here)")
          (is (= 1 (:current st2)) "then the turn ends"))))
    (testing "chains fire by SLOT ADJACENCY (green): crafting slot k offers the :chain at k+1"
      (let [st (-> base
                   (assoc-in [:players 0 :workers] [:white :black :yellow])
                   ;; slot 0 = :base-d (white+black); slot 1 = a chain — its LEFT neighbour
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :ch :inputs [:yellow] :outputs [:luxury] :flags #{:chain}})
                   ;; a payable chain elsewhere must NOT be offered (not adjacent)
                   (assoc-in [:players 0 :recipe-slots 5]
                             {:id :far :inputs [:yellow] :outputs [:luxury] :flags #{:chain}}))]
        (is (not-any? #(#{1 5} (:slot %)) (moves-of st :craft))
            "chain cards are NEVER standalone crafts")
        (testing "crafting slot 2 (left of NON-chain slot 3) offers nothing"
          (let [st2 (-> st (assoc-in [:players 0 :workers] [:white :red]))
                st' (g/apply-move st2 (craft-at st2 2))]
            (is (= 1 (:current st')) "no chain adjacency -> turn just ends")))
        (let [st' (g/apply-move st (craft-at st 0))]
          (is (= :chain (:step st')) "the cascade opens on the RIGHT neighbour")
          (is (= {:next 1 :node 0} (:pending-chain st')))
          (is (= [1] (map :slot (moves-of st' :fire-chain))) "only slot 1 is offered — NOT slot 5")
          (is (= 1 (count (moves-of st' :decline-chain))))
          (let [st2 (g/apply-move st' (first (moves-of st' :fire-chain)))]
            (is (= 1 (get-in st2 [:players 0 :tokens :luxury])) "chain output granted")
            (is (empty? (get-in st2 [:players 0 :workers])) "chain cost paid")
            (is (= 1 (:current st2)) "slot 2 isn't a chain — cascade over, turn over")
            (is (nil? (:pending-chain st2)) "cascade bookkeeping cleared"))
          (testing "declining ends the cascade and the turn"
            (let [st2 (g/apply-move st' {:type :decline-chain})]
              (is (= 1 (:current st2)))
              (is (= [:yellow] (get-in st2 [:players 0 :workers])) "chain cost NOT paid"))))))
    (testing "a chain CASCADES rightward: slot 1 chain then slot 2 chain"
      (let [st (-> base
                   (assoc-in [:players 0 :workers] [:white :black :yellow :yellow])
                   (assoc-in [:players 0 :recipe-slots 1]
                             {:id :c1 :inputs [:yellow] :outputs [:luxury] :flags #{:chain}})
                   (assoc-in [:players 0 :recipe-slots 2]
                             {:id :c2 :inputs [:yellow] :outputs [:luxury] :flags #{:chain}}))
            st1 (g/apply-move st (craft-at st 0))
            st2 (g/apply-move st1 (first (moves-of st1 :fire-chain)))]
        (is (= :chain (:step st2)) "firing slot 1 offers slot 2")
        (is (= [2] (map :slot (moves-of st2 :fire-chain))))
        (let [st3 (g/apply-move st2 (first (moves-of st2 :fire-chain)))]
          (is (= 2 (get-in st3 [:players 0 :tokens :luxury])) "both chains fired")
          (is (= 1 (:current st3))))))))

(deftest payment-choice
  (testing "green 2026-06-29: each distinct payment MULTISET is its own move"
    (let [st (-> (mid-play [:blacksmith :goldsmith] 0 :step :craft)
                 ;; :base-d needs [:white :black]; white slot payable by white OR
                 ;; home(black); black slot by black — and a SKILLED black doubles
                 ;; both black options
                 (assoc-in [:players 0 :workers] [:white :black :skilled-black]))
          crafts (filter #(= 0 (:slot %)) (moves-of st :craft))
          pays (set (map :pay crafts))]
      (is (contains? pays [:black :white]) "regular pair")
      (is (contains? pays (vec (sort-by str [:white :skilled-black]))) "skilled-black pays the black slot")
      (is (contains? pays (vec (sort-by str [:black :skilled-black]))) "black can also pay the WHITE slot (home↔white)")
      (is (= 3 (count pays)) "exactly the 3 distinct multisets, no dupes")
      (testing "the chosen payment is spent EXACTLY (skilled kept or spent by choice)"
        (let [mv (first (filter #(= [:black :white] (:pay %)) crafts))
              st' (g/apply-move st mv)]
          (is (= [:skilled-black] (get-in st' [:players 0 :workers]))
              "paying with regulars keeps the skilled token"))
        (let [mv (first (filter #(some g/skilled? (:pay %)) crafts))
              st' (g/apply-move st mv)]
          (is (some g/skilled? (get-in st' [:board-workers 0]))
              "the spent skilled token dropped on the node, still marked")))))
  (testing "mastercraft builds get the same payment choice"
    (let [st (-> (mid-play [:blacksmith :goldsmith] 0 :step :craft)
                 (assoc-in [:players 0 :master-recipes] 1)
                 (assoc-in [:players 0 :workers] [:white :white :skilled-black]))
          ms (filter #(= :mw1 (:id %)) (moves-of st :craft-master))]
      ;; mw1 costs [:white :white]; black (home) can pay a white slot, so the
      ;; skilled-black opens more multisets
      (is (> (count ms) 1) "more than one way to pay -> more than one move"))))

(deftest display-triggers-prior-dividends
  (let [st (-> (mid-play [:goldsmith :jeweler] 0)
               (assoc-in [:board-shops 0 0] [{:level :atelier}])
               (assoc-in [:players 0 :mastercrafts-built] 1)
               ;; P1 already has a mastercraft displayed on durability
               (assoc-in [:displays :durability] [{:pid 1}]))
        sc1 (get-in st [:players 1 :score]) c1 (get-in st [:players 1 :coins])
        sc0 (get-in st [:players 0 :score])
        st' (g/apply-move st (first (filter #(= :durability (:track %)) (moves-of st :display))))]
    (is (= (inc sc1) (get-in st' [:players 1 :score])) "the PRIOR display earned +1pt")
    (is (= (inc c1) (get-in st' [:players 1 :coins])) "and +1 coin")
    (is (= (+ sc0 (g/grid-value :goldsmith :durability 1)) (get-in st' [:players 0 :score]))
        "the NEW display scores its grid value only — no dividend to itself")
    (testing "no prior displays -> no dividend fires"
      (let [st (assoc st :displays {})
            st' (g/apply-move st (first (filter #(= :durability (:track %)) (moves-of st :display))))]
        (is (= sc1 (get-in st' [:players 1 :score])))))))

(deftest atelier-build-unlocks-a-chosen-ability
  (let [st (-> (mid-play [:blacksmith :goldsmith] 3)   ; space 3 = atelier-pts action
               (assoc-in [:players 0 :coins] 9)
               (assoc-in [:board-shops 0 0] [{:level :basic}]))
        st' (g/apply-move st (first (moves-of st :action-atelier)))]
    (is (= :ability (:step st')) "building an atelier opens the ability choice")
    (is (= #{:optional-sale-coin :grey-sweep :shop-refund}
           (set (map :id (moves-of st' :pick-ability))))
        "all still-locked blacksmith abilities on offer")
    (let [st2 (g/apply-move st' {:type :pick-ability :id :grey-sweep})]
      (is (contains? (get-in st2 [:players 0 :abilities]) :grey-sweep))
      (is (= :craft (:step st2)))
      (testing "a second build offers only the remaining two"
        (let [st3 (-> st2 (assoc :step :main :current 0)
                      (assoc-in [:players 0 :coins] 9)
                      (assoc-in [:board-shops 5 0] [{:level :basic}]))
              st4 (g/apply-move st3 (first (moves-of st3 :action-atelier)))]
          (is (= #{:optional-sale-coin :shop-refund}
                 (set (map :id (moves-of st4 :pick-ability))))))))))

(deftest blacksmith-abilities
  (testing "passive :display-skill-first — skill rises BEFORE the display scores"
    (let [st (-> (mid-play [:blacksmith :goldsmith] 0)
                 (assoc-in [:board-shops 0 0] [{:level :atelier}])
                 (assoc-in [:players 0 :mastercrafts-built] 1))
          sc (get-in st [:players 0 :score])
          st' (g/apply-move st (first (filter #(= :durability (:track %)) (moves-of st :display))))]
      (is (= 2 (get-in st' [:players 0 :skills :durability])) "+1 skill first")
      (is (= (+ sc (g/grid-value :blacksmith :durability 2)) (get-in st' [:players 0 :score]))
          "scored at the RAISED rank (3, not rank-1's 2)")))
  (testing ":optional-sale-coin — +1 coin when the sale includes an OPTIONAL good"
    (let [base (-> (mid-play [:blacksmith :goldsmith] 5)   ; Barracks: req D, opt P
                   (assoc-in [:players 0 :abilities] #{:optional-sale-coin})
                   (assoc-in [:board-shops 5 0] [{:level :basic}]))]
      (testing "required only (durability) → no coin"
        (let [st (assoc-in base [:players 0 :tokens] {:durability 1})
              c (get-in st [:players 0 :coins])
              st' (g/apply-move st (first (moves-of st :sell)))]
          (is (= c (get-in st' [:players 0 :coins])))))
      (testing "with the optional (precision) riding along → +1 coin"
        (let [st (assoc-in base [:players 0 :tokens] {:durability 1 :precision 1})
              c (get-in st [:players 0 :coins])
              st' (g/apply-move st (first (moves-of st :sell)))]
          (is (= (inc c) (get-in st' [:players 0 :coins])))))))
  (testing ":grey-sweep — picking up a grey takes ALL greys at that space"
    (let [st (-> (mid-play [:blacksmith :goldsmith] 0 :step :pickup)
                 (assoc-in [:players 0 :abilities] #{:grey-sweep})
                 (assoc-in [:players 0 :workers] [])
                 (assoc-in [:board-workers 0] [:grey :grey :black]))
          mv (first (filter #(= :grey (:color %)) (moves-of st :pickup)))
          st' (g/apply-move st mv)]
      (is (= 2 (count (filter #{:grey} (get-in st' [:players 0 :workers])))) "both greys swept")
      (is (= [:black] (get-in st' [:board-workers 0])) "only the black remains")))
  (testing ":shop-refund — +2 coins on placing a basic shop"
    (let [st (-> (mid-play [:blacksmith :goldsmith] 1)   ; Docks: a :shop action space
                 (assoc-in [:players 0 :abilities] #{:shop-refund}))
          c (get-in st [:players 0 :coins])
          mv (first (filter #(and (:place? %) (= 4 (:node %))) (moves-of st :action-shop)))
          st' (g/apply-move st mv)]
      (is (= (+ c (- (g/build-cost st (get-in st [:players 0]))) 2)
             (get-in st' [:players 0 :coins])) "cost paid, then +2 back"))))

(deftest alchemist-abilities
  (testing "passive :innovation-boost — discard a held Innovation for a 2nd level per type"
    (let [st (-> (mid-play [:alchemist :goldsmith] 5)   ; Barracks: D-req (innovation spare)
                 (assoc-in [:board-shops 5 0] [{:level :basic}])
                 (assoc-in [:players 0 :tokens] {:durability 1 :innovation 1}))
          sells (moves-of st :sell)
          boosted (filter :boost sells)]
      (is (seq boosted) "boost variants offered (innovation held beyond the sale)")
      (let [pool-i (get-in st [:goods :innovation])
            st' (g/apply-move st (first (filter #(= :durability (:track %)) boosted)))]
        (is (= 3 (get-in st' [:players 0 :skills :durability])) "levels TWICE")
        (is (nil? (get-in st' [:players 0 :tokens :innovation])) "the boost token discarded")
        (is (= (inc pool-i) (get-in st' [:goods :innovation])) "…back to the pool"))
      (testing "no spare innovation -> no boost variant"
        (let [st (assoc-in st [:players 0 :tokens] {:durability 1})]
          (is (empty? (filter :boost (moves-of st :sell))))))))
  (testing ":recipe-coinback — +1 coin back after taking a recipe (net cost 1)"
    (let [st (-> (mid-play [:alchemist :goldsmith] 4)   ; Manor = recipe-coin
                 (assoc-in [:players 0 :abilities] #{:recipe-coinback})
                 (assoc-in [:players 0 :coins] 5))
          ms (filter #(= :coins (:pay %)) (moves-of st :action-recipe))]
      (is (seq ms))
      (is (= 4 (get-in (g/apply-move st (first ms)) [:players 0 :coins])) "−2 then +1 = net −1")))
  (testing ":display-up-innovation — displaying also raises Innovation"
    (let [st (-> (mid-play [:alchemist :goldsmith] 0)
                 (assoc-in [:players 0 :abilities] #{:display-up-innovation})
                 (assoc-in [:board-shops 0 0] [{:level :atelier}])
                 (assoc-in [:players 0 :mastercrafts-built] 1))
          st' (g/apply-move st (first (moves-of st :display)))]
      (is (= 2 (get-in st' [:players 0 :skills :innovation])))))
  (testing ":two-free-shops — unlocking places 2 FREE shops IMMEDIATELY, then you STILL craft"
    (let [st (-> (mid-play [:alchemist :goldsmith] 3 :step :ability))
          unlocked (g/apply-move st {:type :pick-ability :id :two-free-shops})]
      (is (= :free-shop (:step unlocked)) "diverts to place the shops right now")
      (is (= 2 (:pending-free-shops unlocked)))
      (let [before (g/total-player-shops unlocked 0)
            s2 (g/apply-move unlocked (first (moves-of unlocked :place-free-shop)))
            s3 (g/apply-move s2 (first (moves-of s2 :place-free-shop)))]
        (is (= (+ before 2) (g/total-player-shops s3 0)) "both placed")
        (is (= :craft (:step s3)) "then you STILL take your craft this same turn")))))

(deftest goldsmith-abilities
  (testing "passive :luxury-sale-point — +1 point on any luxury sale"
    (let [st (-> (mid-play [:goldsmith :jeweler] 6)   ; space 6 demands luxury
                 (assoc-in [:board-shops 6 0] [{:level :basic}])
                 (assoc-in [:players 0 :tokens] {:luxury 1}))
          sc (get-in st [:players 0 :score])
          st' (g/apply-move st (first (moves-of st :sell)))]
      (is (= (inc sc) (get-in st' [:players 0 :score])) "luxury pays coins; the point is the passive")))
  (testing ":display-gain-luxury — a display nets a Luxury token from the pool"
    (let [st (-> (mid-play [:goldsmith :jeweler] 0)
                 (assoc-in [:players 0 :abilities] #{:display-gain-luxury})
                 (assoc-in [:board-shops 0 0] [{:level :atelier}])
                 (assoc-in [:players 0 :mastercrafts-built] 1))
          pool-l (get-in st [:goods :luxury])
          st' (g/apply-move st (first (moves-of st :display)))]
      (is (= 1 (get-in st' [:players 0 :tokens :luxury])))
      (is (= (dec pool-l) (get-in st' [:goods :luxury])))))
  (testing ":gold-kicker — at GAME END, score points = leftover coins (not on unlock)"
    (let [st (-> (mid-play [:goldsmith :jeweler] 3 :step :ability)
                 (assoc-in [:players 0 :coins] 7))
          unlocked (g/apply-move st {:type :pick-ability :id :gold-kicker})]
      (is (= 0 (- (get-in unlocked [:players 0 :score]) (get-in st [:players 0 :score])))
          "NOTHING scored on unlock")
      (let [end (-> unlocked (assoc :step :craft :current 1 :round 5 :final-round 5)
                    (assoc-in [:players 0 :coins] 7))
            sc (get-in end [:players 0 :score])
            over (g/apply-move end {:type :skip-craft})]
        (is (g/game-over? over))
        (is (= (+ sc 7) (get-in over [:players 0 :score])) "leftover coins scored at the end"))))
  (testing ":cheap-adjacent — the adjacent pickup is free (and legal at 0 coins)"
    (let [st (-> (mid-play [:goldsmith :jeweler] 0 :step :pickup)
                 (assoc-in [:players 0 :abilities] #{:cheap-adjacent})
                 (assoc-in [:players 0 :coins] 0))
          adj (first (g/undirected-neighbors st 0))
          st (assoc-in st [:board-workers adj] [:red])
          mv (first (filter #(= adj (:node %)) (moves-of st :pickup-adjacent)))]
      (is (some? mv) "offered even with an empty purse")
      (is (zero? (get-in (g/apply-move st mv) [:players 0 :coins])) "no fee"))))

(deftest jeweler-abilities
  (testing "passive :sell-three-luxury — a ≥3-token sale nets a Luxury from the pool"
    (let [st (-> (mid-play [:jeweler :goldsmith] 5)   ; Barracks: D-req
                 (assoc-in [:board-shops 5 0] [{:level :basic}])
                 (assoc-in [:players 0 :tokens] {:durability 3}))
          pool-l (get-in st [:goods :luxury])
          st' (g/apply-move st (first (moves-of st :sell)))]
      (is (= 1 (get-in st' [:players 0 :tokens :luxury])))
      (is (= (dec pool-l) (get-in st' [:goods :luxury])))))
  (testing ":display-at-shops — display works at a BASIC shop"
    (let [st (-> (mid-play [:jeweler :goldsmith] 0)
                 (assoc-in [:board-shops 0 0] [{:level :basic}])
                 (assoc-in [:players 0 :mastercrafts-built] 1))]
      (is (empty? (moves-of st :display)) "not without the ability")
      (is (seq (moves-of (assoc-in st [:players 0 :abilities] #{:display-at-shops}) :display)))))
  (testing ":skilled-affinity — claiming skilled also grabs a matching regular here"
    (let [st (-> (mid-play [:jeweler :goldsmith] 7)   ; space 7 = skilled action
                 (assoc-in [:players 0 :abilities] #{:skilled-affinity})
                 (assoc-in [:board-workers 7] [:red :blue]))
          st' (g/apply-move st (first (filter #(= :red (:color %)) (moves-of st :action-skilled))))]
      (is (= [:skilled-red :red] (get-in st' [:players 0 :workers])))
      (is (= [:blue] (get-in st' [:board-workers 7])) "the regular red left the space")))
  (testing ":shop-payout — unlocking scores +1 coin for each shop you own"
    (let [st (-> (mid-play [:jeweler :goldsmith] 3 :step :ability)
                 (assoc-in [:board-shops 0 0] [{:level :basic}])
                 (assoc-in [:board-shops 5 0] [{:level :basic} {:level :atelier}]))
          c (get-in st [:players 0 :coins])
          shops (g/total-player-shops st 0)
          st' (g/apply-move st {:type :pick-ability :id :shop-payout})]
      (is (= 3 shops) "3 shops on the board (2 basic + 1 atelier)")
      (is (= (+ c shops) (get-in st' [:players 0 :coins])) "+1 coin per shop"))))

;; ── gene-wiring: every gene must be able to move a decision (playbook §3B) ─────
;; A dead gene wastes optimization pressure and lies to the GA. For each gene we
;; perturb it ALONE to its low vs high bound on a rich battery of mid-game states
;; (snapshots from full 4-guild games + hand-built states that expose displays,
;; ateliers, the master-pool race, flagged crafts, ability picks, unlocked
;; abilities, pickups and :ending). Guild/ability genes only fire for the relevant
;; guild, so the battery includes all four guilds and pre-unlocked abilities.

(defn- snapshot-states
  "Decision-point states (≥2 legal moves) sampled from seeded full 4-guild games."
  []
  (let [picks [:blacksmith :alchemist :goldsmith :jeweler]
        gens (vec (take 4 bots/archetypes))]
    (vec (mapcat
          (fn [seed]
            (loop [st (g/new-game seed picks) acc [] n 0]
              (if (or (g/game-over? st) (> n 400))
                acc
                (let [ms (g/enumerate-moves st)
                      acc (if (and (= :play (:phase st)) (> (count ms) 1) (zero? (mod n 3)))
                            (conj acc st) acc)]
                  (recur (g/apply-move st (bots/decide st (nth gens (mod (:current st) 4))))
                         acc (inc n))))))
          [7 19 44 91 128 260]))))

(defn- targeted-states
  "Hand-built states exercising the guild/ability + horizon genes explicitly."
  []
  (let [mp (fn [guilds node step]
             (-> (g/new-game 5 guilds)
                 (assoc :phase :play :step step :active-cart 0 :current 0)
                 (assoc-in [:players 0 :carts 0] node)))]
    [;; goldsmith luxury sell (theme=luxury, passive luxury-sale-point → track choice):
     ;; a display on durability makes track-own pick durability at low passive-lean,
     ;; and high passive-lean flips the tracked copy to luxury. coins 1 = no recipe.
     (-> (mp [:goldsmith :jeweler] 4 :main)
         (assoc-in [:players 0 :coins] 1)
         (assoc-in [:board-shops 4 0] [{:level :basic}])
         (assoc-in [:displays :durability] [{:pid 0}])
         (assoc-in [:players 0 :tokens] {:luxury 2 :durability 1 :precision 1}))
     ;; track-hold: durability track fuller than precision → track-hold (seed the
     ;; emptiest track) opposes clock-control (rush the fullest); high track-hold flips
     (-> (mp [:blacksmith :goldsmith] 5 :main)
         (assoc-in [:board-shops 5 0] [{:level :basic}])
         (assoc-in [:tracks :durability] 3)
         (assoc-in [:players 0 :tokens] {:durability 2 :precision 1}))
     ;; clock-dump-bias: durability is my top skill, precision a laggard → high
     ;; clock-dump burns the tracked copy onto the unfarmed (low-skill) precision
     (-> (mp [:blacksmith :goldsmith] 5 :main)
         (assoc-in [:board-shops 5 0] [{:level :basic}])
         (assoc-in [:players 0 :skills] {:durability 5 :precision 1 :innovation 1 :luxury 1})
         (assoc-in [:players 0 :tokens] {:durability 2 :precision 1}))
     ;; foresight-track: I hold a display on durability (track-own picks it at low
     ;; foresight-track) but I trail and durability is about to FILL — high
     ;; foresight-track flips me off the game-ending track
     (-> (mp [:blacksmith :goldsmith] 5 :main)
         (assoc-in [:board-shops 5 0] [{:level :basic}])
         (assoc-in [:tracks :durability] 4)
         (assoc-in [:displays :durability] [{:pid 0}])
         (assoc-in [:players 1 :score] 30)
         (assoc-in [:players 0 :tokens] {:durability 2 :precision 1}))
     ;; two upgradeable basics of DIFFERENT demand-fit (atelier-value steers which)
     (-> (mp [:blacksmith :goldsmith] 3 :main)
         (assoc-in [:players 0 :coins] 9)
         (assoc-in [:board-shops 0 0] [{:level :basic}])
         (assoc-in [:board-shops 5 0] [{:level :basic}])
         (assoc-in [:players 0 :skills] {:durability 5 :precision 1 :innovation 1 :luxury 1}))
     ;; skilled action, two guilds (skilled-value steers toward the HOME colour)
     (-> (mp [:blacksmith :goldsmith] 7 :main))
     (-> (mp [:goldsmith :jeweler] 7 :main))
     ;; foresight-endgame: :ending, a huge dead worker-glut makes BUILDING attractive
     ;; at low foresight-endgame, but high foresight-endgame says cash the sale instead
     (-> (mp [:blacksmith :goldsmith] 5 :main)
         (assoc :ending true)
         (assoc-in [:players 0 :coins] 9)
         (assoc-in [:board-shops 5 0] [{:level :basic}])
         (assoc-in [:board-workers 2] (vec (repeat 22 :grey)))
         (assoc-in [:players 0 :tokens] {:durability 1}))
     ;; master-pool race: a cost-3 token (mw0 [:blue :red :yellow], paid with three
     ;; skilled workers that each spend-grab a pooled regular → +1.5, wins at low
     ;; urgency) vs a cost-2 [:white :white] token (+1.0 home affinity, which high
     ;; master-race-urgency flips to). Isolates the urgency lever from hwa.
     (-> (mp [:blacksmith :goldsmith] 0 :craft)
         (assoc-in [:players 0 :master-recipes] 1)
         (assoc-in [:board-workers 0] [:blue :red :yellow])
         (assoc-in [:players 0 :workers] [:skilled-blue :skilled-red :skilled-yellow :white :white]))
     ;; recipe action with EVERY slot already holding a live recipe (covering costs)
     ;; so recipe-value alone decides take-a-recipe vs skip
     (-> (mp [:blacksmith :goldsmith] 4 :main)
         (assoc-in [:players 0 :coins] 6)
         (assoc-in [:players 0 :recipe-slots]
                   (vec (repeat 8 {:id :x :inputs [:black] :outputs [:durability]}))))
     ;; goldsmith with an econ ability unlocked (followthrough/passive contexts)
     (-> (mp [:goldsmith :jeweler] 4 :main)
         (assoc-in [:board-shops 4 0] [{:level :basic}])
         (assoc-in [:players 0 :abilities] #{:shop-payout})
         (assoc-in [:players 0 :tokens] {:luxury 2 :durability 1}))
     ;; jeweler 3-token sell (passive sell-three-luxury)
     (-> (mp [:jeweler :goldsmith] 5 :main)
         (assoc-in [:board-shops 5 0] [{:level :basic}])
         (assoc-in [:players 0 :tokens] {:durability 3 :precision 1}))
     ;; blacksmith display: mastercraft + atelier + prior displays (dividend/theme/passive)
     (-> (mp [:blacksmith :goldsmith] 3 :main)
         (assoc-in [:board-shops 3 0] [{:level :atelier}])
         (assoc-in [:players 0 :mastercrafts-built] 2)
         (assoc-in [:players 0 :skills] {:durability 2 :precision 4 :innovation 1 :luxury 1})
         (assoc-in [:displays :durability] [{:pid 0}])
         (assoc-in [:displays :precision] [{:pid 1}]))
     ;; sell where an OPPONENT out-displays me on one track (opponent-denial)
     (-> (mp [:blacksmith :goldsmith] 5 :main)
         (assoc-in [:board-shops 5 0] [{:level :basic}])
         (assoc-in [:displays :durability] [{:pid 1} {:pid 1}])
         (assoc-in [:players 0 :tokens] {:durability 2 :precision 1}))
     ;; alchemist innovation-boost sell (boost vs non-boost variant)
     (-> (mp [:alchemist :goldsmith] 5 :main)
         (assoc-in [:board-shops 5 0] [{:level :basic}])
         (assoc-in [:players 0 :tokens] {:durability 1 :innovation 2}))
     ;; craft step: many workers + a medallion-approaching skill + master recipe
     (-> (mp [:blacksmith :goldsmith] 0 :craft)
         (assoc-in [:players 0 :master-recipes] 1)
         (assoc-in [:players 0 :skills] {:durability 2 :precision 3 :innovation 1 :luxury 1})
         (assoc-in [:players 0 :workers] [:white :black :grey :grey :skilled-black :red])
         (assoc-in [:board-workers 0] [:black :grey]))
     ;; craft step with FLAG cards (bonus-recipe-value / foresight medallion+spread)
     (-> (mp [:blacksmith :goldsmith] 0 :craft)
         (assoc-in [:players 0 :skills] {:durability 2 :precision 1 :innovation 1 :luxury 1})
         (assoc-in [:players 0 :workers] [:black :white :grey :grey :yellow])
         (assoc-in [:players 0 :recipe-slots 1]
                   {:id :bs :inputs [:black :white] :outputs [:durability] :flags #{:bonus-skill} :once? true})
         (assoc-in [:players 0 :recipe-slots 3]
                   {:id :lo :inputs [:white :grey] :outputs [] :flags #{:lowest}}))
     ;; craft step, grey-any unlocked (ability-followthrough on grey pay)
     (-> (mp [:blacksmith :goldsmith] 0 :craft)
         (assoc-in [:players 0 :abilities] #{:grey-sweep})
         (assoc-in [:players 0 :workers] [:grey :grey :grey :white :black]))
     ;; ability pick steps — alchemist & jeweler carry all THREE ability TYPES
     (-> (mp [:alchemist :goldsmith] 3 :ability))
     (-> (mp [:jeweler :goldsmith] 3 :ability))
     (-> (mp [:goldsmith :jeweler] 3 :ability) (assoc-in [:players 0 :coins] 7))
     (-> (mp [:blacksmith :goldsmith] 3 :ability) (assoc-in [:players 0 :coins] 4))
     ;; shop action (spread-vs-stack / glut / cost / opponent-denial / refund follow)
     (-> (mp [:blacksmith :goldsmith] 1 :main)
         (assoc-in [:players 0 :coins] 9)
         (assoc-in [:board-shops 2 0] [{:level :basic}])
         (assoc-in [:board-shops 3 1] [{:level :basic}])
         (assoc-in [:board-workers 6] [:red :red :blue :yellow :black :white]))
     (-> (mp [:blacksmith :goldsmith] 1 :main)
         (assoc-in [:players 0 :abilities] #{:shop-refund})
         (assoc-in [:players 0 :coins] 9))
     ;; move step: hold a masterwork, an atelier + a sellable shop to route between
     (-> (mp [:blacksmith :goldsmith] 0 :move)
         (assoc-in [:players 0 :carts] [0 4])
         (assoc-in [:players 0 :mastercrafts-built] 1)
         (assoc-in [:board-shops 3 0] [{:level :atelier}])
         (assoc-in [:players 0 :tokens] {:durability 2})
         (assoc-in [:board-shops 5 0] [{:level :basic}]))
     ;; pickup step near/over the worker-target boundary (worker-value/target/affinity)
     (-> (mp [:blacksmith :goldsmith] 0 :pickup)
         (assoc-in [:board-workers 0] [:black :white :grey :red])
         (assoc-in [:players 0 :workers] (vec (repeat 8 :blue))))
     ;; pickup with cheap-adjacent unlocked (followthrough on adjacents)
     (-> (mp [:goldsmith :jeweler] 0 :pickup)
         (assoc-in [:players 0 :abilities] #{:cheap-adjacent})
         (assoc-in [:players 0 :coins] 0)
         (assoc-in [:board-workers 0] [:yellow :white]))
     ;; :ending sells (foresight-endgame)
     (-> (mp [:blacksmith :goldsmith] 5 :main)
         (assoc :ending true)
         (assoc-in [:tracks :precision] 5)
         (assoc-in [:board-shops 5 0] [{:level :basic}])
         (assoc-in [:players 0 :tokens] {:durability 3 :precision 2}))
     ;; jeweler display-at-shops unlocked (followthrough on displays)
     (-> (mp [:jeweler :goldsmith] 0 :main)
         (assoc-in [:players 0 :abilities] #{:display-at-shops})
         (assoc-in [:board-shops 0 0] [{:level :basic}])
         (assoc-in [:players 0 :mastercrafts-built] 1)
         (assoc-in [:players 0 :tokens] {:durability 1}))
     ;; recipe action (recipe-value / slot-discipline)
     (-> (mp [:blacksmith :goldsmith] 4 :main) (assoc-in [:players 0 :coins] 6))
     ;; skilled action (skilled-value)
     (-> (mp [:blacksmith :goldsmith] 7 :main))]))

(deftest gene-wiring
  (let [states (vec (concat (snapshot-states) (targeted-states)))
        base bots/default-genome
        results (vec (for [k bots/genome-keys]
                       (let [[lo hi] (get bots/genome-bounds k)
                             glo (assoc base k lo)
                             ghi (assoc base k hi)
                             moved? (boolean (some #(not= (bots/decide % glo) (bots/decide % ghi)) states))]
                         [k moved?])))
        dead (mapv first (remove second results))
        live (count (filter second results))
        frac (/ live (double (count results)))]
    (when (seq dead)
      (println "\n[gene-wiring] DEAD genes (never moved a decision):" dead))
    (println (format "[gene-wiring] %d/%d genes wired (%.0f%%) over %d states"
                     live (count results) (* 100.0 frac) (count states)))
    (is (>= frac 0.8)
        (str "≥80% of genes must be able to change a decision; dead: " dead))))

(deftest parks-action-grey-anywhere
  ;; Mohammad bug-fix 2026-07-04: the PARKS ACTION costs 2 GREY and places a shop
  ;; ANYWHERE (any space with room) — NOT '2 grey to place on Parks'. The 2 grey
  ;; drop on Parks (the action node). The coin ramp is unaffected from other actions.
  (let [parks 2 docks 1 castle 6
        base (-> (g/new-game 5 [:blacksmith :goldsmith])
                 (assoc :phase :play :step :main :active-cart 0 :current 0 :board-shops {})
                 (assoc-in [:board-workers parks] [])
                 (assoc-in [:players 0 :coins] 0))          ; DEAD BROKE
        offered (fn [st] (set (map :node (filter #(and (= :action-shop (:type %)) (:place? %))
                                                 (g/enumerate-moves st)))))]
    (testing "cart at PARKS + 2 grey + 0 coins -> can build ANYWHERE with room"
      (let [st (-> base (assoc-in [:players 0 :carts 0] parks)
                   (assoc-in [:players 0 :workers] [:grey :grey :black]))
            tgts (offered st)]
        (is (contains? tgts docks) "can place at Docks via the Parks action (anywhere)")
        (is (contains? tgts castle) "…and at Castle")
        (is (contains? tgts parks) "…and at Parks itself")))
    (testing "building at Docks via Parks action: 2 grey spent, DROP ON PARKS, no coins"
      (let [st (-> base (assoc-in [:players 0 :carts 0] parks)
                   (assoc-in [:players 0 :workers] [:grey :grey :black]))
            mv (first (filter #(and (= :action-shop (:type %)) (:place? %) (= docks (:node %)))
                              (g/enumerate-moves st)))
            st' (g/apply-move st mv)]
        (is (= 0 (get-in st' [:players 0 :coins])) "coin-free")
        (is (g/player-shop-at? st' 0 docks) "shop landed at the chosen target (Docks)")
        (is (= [:black] (get-in st' [:players 0 :workers])) "2 grey left the hand")
        (is (= 2 (count (filter #{:grey} (get-in st' [:board-workers parks]))))
            "the 2 grey dropped on PARKS (the action node), not on the target")))
    (testing "Parks action needs 2 grey (1 grey -> nothing offered)"
      (is (empty? (offered (-> base (assoc-in [:players 0 :carts 0] parks)
                               (assoc-in [:players 0 :workers] [:grey :black]))))))
    (testing "from DOCKS the cost is COINS (grey doesn't pay) — 0 coins -> nothing"
      (is (empty? (offered (-> base (assoc-in [:players 0 :carts 0] docks)
                               (assoc-in [:players 0 :workers] [:grey :grey :grey :grey]))))))
    (testing "blocker helper: Parks action w/o grey -> :need-2-grey; Docks w/o coin -> :need-coins"
      (let [at-parks (-> base (assoc-in [:players 0 :carts 0] parks)
                         (assoc-in [:players 0 :workers] [:black]))
            at-docks (-> base (assoc-in [:players 0 :carts 0] docks))]
        (is (= :need-2-grey (g/shop-build-blocker at-parks (get-in at-parks [:players 0]) parks castle)))
        (is (= :need-coins (g/shop-build-blocker at-docks (get-in at-docks [:players 0]) docks castle)))))))

