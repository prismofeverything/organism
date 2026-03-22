(ns journey.game-test
  (:require
   [clojure.test :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [journey.game :as game]
   [journey.choice :as choice]))

(deftest initial-state-test
  (testing "initial state structure"
    (let [state (game/initial-state ["alice" "bob"])]
      (pprint state)
      (is (= ["alice" "bob"] (:turn-order state)))
      (is (= 2 (count (:players state))))
      (is (map? (:board state)))
      (is (map? (:bag state)))
      (is (vector? (:deck state)))
      (is (= (* game/card-suits game/cards-per-suit) (count (:deck state))))
      (is (map? (:cipher state))))))

(deftest bag-test
  (testing "full bag has correct counts"
    (let [bag (game/full-bag)]
      (pprint bag)
      (is (= (set game/tile-colors) (set (keys bag))))
      (is (every? #(= game/num-worlds-per-color %) (vals bag))))))

(deftest cipher-test
  (testing "cipher has center and 6 color-associated positions"
    (let [cipher (game/initial-cipher)]
      (pprint cipher)
      (is (= 7 (count cipher)))
      (is (contains? cipher [0 0]))
      (is (empty? (get-in cipher [[0 0] :colors])))
      (is (= (set game/tile-colors)
             (set (mapcat (comp keys :colors) (vals cipher))))))))

(deftest choose-action-type-test
  (testing "find-state only offers action types with valid sub-choices"
    (let [state (game/initial-state ["alice" "bob"])
          [phase choices] (choice/find-state state)]
      (println "phase:" phase)
      (println "choice keys:" (keys choices))
      (is (= :choose-action-type phase))
      ;; At game start: no sundivers on board → no conversions, no stations → no activate.
      ;; Only :move is available (it always has :done at minimum).
      (is (contains? choices :move))
      (is (not (contains? choices :convert)))
      (is (not (contains? choices :activate)))
      ;; Each present choice leads to the correct next phase.
      (doseq [[action-type next-state] choices]
        (is (= action-type (get-in next-state [:player-turn :action-type])))
        (is (= (keyword (str "choose-" (name action-type)))
               (game/current-phase next-state)))))))

;; All test positions use [2,0] as target to avoid the NEUTRAL tower at [0,0].
;; Sundivers around [2,0]:
;;   dir 0 → [3,0]   dir 2 → [2,-1]   dir 3 → [1,0]   dir 4 → [1,1]
;; Foundry (dir-diff 2): dirs {0,2} → sundivers [3,0] and [2,-1],
;;   two valid targets: [2,0] and [3,-1] (their other common neighbor)
;; Matrix (dir-diff 3): dirs {0,3} → sundivers [3,0] and [1,0], target [2,0]
;; Tower  (dirs 0,2,4): sundivers [3,0],[2,-1],[1,1], target [2,0]

(defn place-tile [state pos color]
  (assoc-in state [:board pos] (game/make-tile color)))

(defn place-sundiver [state player pos]
  (-> state
      (update-in [:board pos] #(or % (game/make-tile :blue)))
      (assoc-in [:board pos :sundivers player] 1)))

(deftest action-type-filtering-test
  (testing "convert appears when conversion patterns exist"
    ;; Matrix pattern: sundivers at [3,0] and [1,0] (dirs 0 and 3 from [2,0])
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [1 0]))
          [_ choices] (choice/find-state state)]
      (is (contains? choices :convert))))

  (testing "activate appears when player has a sundiver on their station"
    ;; Place a matrix station with alice's sundiver on it directly.
    (let [state (-> (game/initial-state ["alice"])
                    (assoc-in [:board [2 0]]
                              (-> (game/make-tile :blue)
                                  (game/add-station :matrix "alice" 0)
                                  (assoc-in [:sundivers "alice"] 1)))
                    (assoc-in [:player-turn :phase] :choose-action-type))
          [_ choices] (choice/find-state state)]
      (is (contains? choices :activate)))))

(deftest convert-patterns-test
  (testing "foundry: two sundivers at 120° → two target choices"
    ;; sundivers at [3,0] and [2,-1] are at dirs 0 and 2 from [2,0]
    ;; they share two common neighbors: [2,0] and [3,-1]
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-tile [3 -1] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [2 -1]))
          foundries (filter #(= :foundry (:type %)) (game/find-conversions state "alice"))]
      (is (= 2 (count foundries)))
      (is (every? #(= #{[3 0] [2 -1]} (set (:sundivers %))) foundries))
      (is (= #{[2 0] [3 -1]} (set (map :target foundries))))))

  (testing "matrix: two sundivers directly across → one target"
    ;; sundivers at [3,0] and [1,0] are at dirs 0 and 3 from [2,0]
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [1 0]))
          matrices (filter #(= :matrix (:type %)) (game/find-conversions state "alice"))]
      (is (= 1 (count matrices)))
      (is (= [2 0] (:target (first matrices))))))

  (testing "tower: three equally spaced sundivers → one target"
    ;; sundivers at dirs 0,2,4 from [2,0]: [3,0],[2,-1],[1,1]
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [2 -1])
                    (place-sundiver "alice" [1 1]))
          towers (filter #(= :tower (:type %)) (game/find-conversions state "alice"))]
      (is (= 1 (count towers)))
      (is (= [2 0] (:target (first towers))))))

  (testing "convert places station and returns sundivers to reserve"
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [1 0]))
          after (game/convert state "alice" :matrix [2 0] [[3 0] [1 0]])]
      (is (= :matrix (get-in after [:board [2 0] :station :type])))
      (is (= "alice" (get-in after [:board [2 0] :station :player])))
      (is (nat-int? (get-in after [:board [2 0] :station :level])))
      (is (= 0 (get-in after [:board [3 0] :sundivers "alice"] 0)))
      (is (= 0 (get-in after [:board [1 0] :sundivers "alice"] 0))))))

(deftest draw-from-bag-test
  (testing "drawing from the bag reduces count"
    (let [bag   (game/full-bag)
          color (first game/tile-colors)
          [bag2 drawn] (game/draw-from-bag bag)]
      (println "drew:" drawn)
      (is (some? drawn))
      (is (= (dec game/num-worlds-per-color) (get bag2 drawn))))))

;; ─── extended simulation ──────────────────────────────────────────────────────

(defn- try-if-choices
  "Return s if find-state yields non-empty choices for it, else nil."
  [s]
  (when (seq (second (choice/find-state s))) s))

(defn- has-own-stations?
  "True if the player has at least one station they converted themselves."
  [state]
  (seq (get-in state [:players (game/current-player state) :stations])))

(defn- has-real-moves?
  "True if the current player can actually launch or fly (not just take :done)."
  [state]
  (let [move-state (get (second (choice/find-state state)) :move)
        [_ mc]     (when move-state (choice/find-state move-state))]
    (or (contains? mc :launch) (contains? mc :fly))))

(defn- simulate-step
  "Pick one smart choice and return the next state. Throws on dead-end."
  [state]
  (let [[phase choices] (choice/find-state state)]
    (when (empty? choices)
      (throw (ex-info "Dead end" {:phase phase})))
    (case phase
      ;; Action type: prefer activate (own stations) → convert → move with real
      ;; sub-choices. Skip move if its only sub-choice would be :done.
      :choose-action-type
      (or (when (has-own-stations? state) (try-if-choices (:activate choices)))
          (try-if-choices (:convert choices))
          (when (has-real-moves? state) (:move choices))
          (first (vals choices)))

      ;; Move: launch first (explores new tiles), then fly, then done
      :choose-move
      (or (:launch choices) (:fly choices) (:done choices))

      ;; Activate: always decline bonus to keep resources available for base actions.
      :choose-activate-self-bonus  (get choices 0)
      :choose-activate-owner-bonus (get choices 0)

      ;; Tower joins: join when possible (exercises that path), skip otherwise
      :choose-activate-tower-join (:join choices (:skip choices))

      ;; Tower action cost: spend first available sundiver
      :choose-activate-tower-spend (first (vals choices))

      ;; Post-action joins: always join (exercises captain/flare join paths)
      :flare-beacon-join   (:join choices (:skip choices))
      :captain-beacon-join (:join choices (:skip choices))

      ;; Ark advance wrap: always go direct
      :choose-ark-advance        (:direct choices)
      :choose-flare-advance      (:direct choices)
      :choose-drift-flare-advance (:direct choices)

      ;; Drift card: auto-draw
      :draw-drift-card (:draw choices)

      ;; Captain drift: no turn (straight ahead)
      :choose-captain-drift (:none choices)

      ;; Never land during the simulation — let all 5 rounds complete
      :choose-land (:continue choices)

      ;; Cipher: prefer centre [0 0] so landing colours accumulate there
      :cipher (or (get choices [0 0]) (first (vals choices)))

      ;; Matrix beacon: prefer tiles without stations to avoid blocking future activations
      :choose-activate-matrix-beacon
      (let [no-station (remove #(get-in state [:board % :station]) (keys choices))]
        (get choices (if (seq no-station) (first no-station) (first (keys choices)))))

      ;; Everything else: first available
      (first (vals choices)))))

(defn- play-one-player-turn
  "Step from the current player's :choose-action-type until the next player's.
   Returns [final-state phases-visited]."
  [state]
  (let [start-player (game/current-player state)]
    (loop [s state phases []]
      (cond
        (:game-over s)
        [s phases]

        (and (seq phases)
             (= :choose-action-type (game/current-phase s))
             (not= (game/current-player s) start-player))
        [s phases]

        :else
        (recur (simulate-step s)
               (conj phases (game/current-phase s)))))))

(defn- tile-color-counts [board]
  (->> (vals board)
       (map :color)
       frequencies
       (sort-by (comp name key))
       (map (fn [[c n]] (str (name c) "×" n)))
       (str/join " ")))

(defn- cipher-summary [cipher]
  (let [center (get-in cipher [[0 0] :colors])]
    (if (empty? center)
      "center=empty"
      (str "center=" (str/join "," (map name (keys center)))))))

(defn- player-line [state p]
  (let [ps  (get-in state [:players p])
        hab (get-in ps [:habitat :sundivers])
        res (get-in ps [:reserve :sundivers])
        bea (get-in ps [:reserve :beacons])
        sta (count (:stations ps))
        on-board (apply + (map #(get-in state [:board % :sundivers p] 0)
                               (keys (:board state))))]
    (format "  %-6s hab=%-2d board=%-2d res=%-2d beacons=%-3d stations=%d"
            p hab on-board res bea sta)))

(defn- print-turn
  [state turn round player phases]
  (println (format "\n─── Turn %-2d │ Round %d │ %-6s ───" turn round player))
  (println (str "  phases: " (str/join " → " (map name phases))))
  (println (format "  Ark=%-8s head=%-8s captain=%-6s flares=%d"
                   (pr-str (:ark state))
                   (pr-str (:heading-token state))
                   (:captain-flame state)
                   (:flares-drawn state 0)))
  (println (format "  board=%d tiles  [%s]"
                   (count (:board state))
                   (tile-color-counts (:board state))))
  (println (str "  " (cipher-summary (:cipher state))))
  (doseq [p (:turn-order state)]
    (println (player-line state p))))

(deftest extended-three-player-game-test
  (testing "3-player game, 5 rounds — exercises all actions and phases"
    (let [players   ["alice" "bob" "carol"]
          state0    (game/initial-state players)
          max-turns (* 5 (count players))]
      (println "\n══════════════════════════════════════════")
      (println "  3-PLAYER GAME SIMULATION — 5 rounds")
      (println "══════════════════════════════════════════")
      (loop [state state0
             turn  1]
        (cond
          (:game-over state)
          (do
            (println "\n  *** GAME OVER ***")
            (println " " (pr-str (:game-over state)))
            (is (some? (:type (:game-over state)))))

          (> turn max-turns)
          (do
            (println "\n  Completed all 15 player turns.")
            (is true))

          :else
          (let [player              (game/current-player state)
                round               (inc (quot (dec turn) (count players)))
                [s phases]          (play-one-player-turn state)
                phase-set           (set phases)]
            (print-turn s turn round player phases)

            ;; Structural invariants after every turn
            (is (= 7 (count (:cipher s)))
                "cipher always has 7 positions")
            (is (every? #(>= (get-in s [:players % :habitat :sundivers] 0) 0) players)
                "no player has negative habitat sundivers")
            (is (every? #(>= (get-in s [:players % :reserve :sundivers] 0) 0) players)
                "no player has negative reserve sundivers")
            (is (every? #(>= (get-in s [:players % :reserve :beacons] 0) 0) players)
                "no player has negative beacon reserve")

            ;; Every turn must visit at least choose-action-type and draw-cards
            (is (contains? phase-set :choose-action-type)
                "turn began with action-type choice")
            (is (contains? phase-set :draw-cards)
                "turn included card draw")

            (recur s (inc turn))))))))

