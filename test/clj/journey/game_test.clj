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

(deftest activate-bonus-routing-test
  ;; Foundry: base 1, bonus 1 at level 1. Foundry base actions always feasible,
  ;; so begin-next-station runs the activator's base action immediately.
  (testing "activating ANOTHER player's station offers the bonus to the owner, not the activator"
    (let [state (-> (game/initial-state ["alice" "bob"])
                    (assoc-in [:board [2 0]]
                              (-> (game/make-tile :blue)
                                  (game/add-station :foundry "bob" 1)
                                  (assoc-in [:sundivers "alice"] 1)))
                    (assoc-in [:player-turn :action :station-type] :foundry)
                    (assoc-in [:player-turn :action :stations-queue] [[2 0]])
                    game/begin-next-station)]
      ;; The turn still belongs to alice (the activator)…
      (is (= "alice" (game/current-player state)))
      ;; …but she is NOT offered the self bonus on bob's station.
      (is (not= :choose-activate-self-bonus (game/current-phase state))
          "activator must not be offered the bonus on another player's station")
      ;; After her base action, bob (the owner) is offered the bonus.
      (is (= :choose-activate-owner-bonus (game/current-phase state))
          "owner is offered the bonus after the activator's base actions")
      (is (= "bob" (get-in state [:player-turn :choice-player]))
          "the bonus choice is handed to the station owner")))

  (testing "activating your OWN station still offers the bonus to you (the activator)"
    (let [state (-> (game/initial-state ["alice" "bob"])
                    (assoc-in [:board [2 0]]
                              (-> (game/make-tile :blue)
                                  (game/add-station :foundry "alice" 1)
                                  (assoc-in [:sundivers "alice"] 1)))
                    (assoc-in [:player-turn :action :station-type] :foundry)
                    (assoc-in [:player-turn :action :stations-queue] [[2 0]])
                    game/begin-next-station)]
      (is (= :choose-activate-self-bonus (game/current-phase state))
          "owner activating their own station chooses the bonus themselves"))))

(deftest activate-bonus-order-and-fallback-test
  (testing "base actions run BEFORE the bonus is offered (own station)"
    (let [start (-> (game/initial-state ["alice"])
                    (assoc-in [:board [2 0]]
                              (-> (game/make-tile :blue)
                                  (game/add-station :foundry "alice" 1)
                                  (assoc-in [:sundivers "alice"] 1)))
                    (assoc-in [:player-turn :action :station-type] :foundry)
                    (assoc-in [:player-turn :action :stations-queue] [[2 0]]))
          hab0  (get-in start [:players "alice" :habitat :sundivers] 0)
          state (game/begin-next-station start)]
      ;; The base foundry action already executed (sundivers moved to habitat)…
      (is (> (get-in state [:players "alice" :habitat :sundivers] 0) hab0)
          "base action executed before any bonus prompt")
      ;; …and only then is the activator offered the bonus.
      (is (= :choose-activate-self-bonus (game/current-phase state)))))

  (testing "owner declines the bonus on their station → the activator gets the option"
    (let [state (-> (game/initial-state ["alice" "bob"])
                    (assoc-in [:board [2 0]]
                              (-> (game/make-tile :blue)
                                  (game/add-station :foundry "bob" 1)
                                  (assoc-in [:sundivers "alice"] 1)))
                    (assoc-in [:player-turn :action :station-type] :foundry)
                    (assoc-in [:player-turn :action :stations-queue] [[2 0]])
                    game/begin-next-station)]
      ;; After base, the owner (bob) decides first.
      (is (= :choose-activate-owner-bonus (game/current-phase state)))
      (is (= "bob" (get-in state [:player-turn :choice-player])))
      ;; Bob declines (choice 0) → alice (the activator) is offered the bonus.
      (let [declined (get (choice/choose-activate-owner-bonus-choices state) 0)]
        (is (= :choose-activate-self-bonus (game/current-phase declined))
            "declining hands the option to the activator")
        (is (nil? (get-in declined [:player-turn :choice-player]))
            "control returns to the activator")))))

(deftest owner-bonus-control-handoff-test
  ;; Regression: when the owner TAKES the bonus on another player's station, the
  ;; owner — not the activator — must make EVERY choice for the bonus actions
  ;; (where the beacon goes, how to pay), and control must return to the activator
  ;; once the bonus is spent. Previously the activator made the owner's choices.
  (testing "owner taking a matrix bonus controls every sub-choice, then hands back"
    (let [base (-> (game/initial-state ["alice" "bob"])
                   ;; bob's matrix station; alice's sundiver is the activating one.
                   (assoc-in [:board [2 0]]
                             (-> (game/make-tile :blue)
                                 (game/add-station :matrix "bob" 1)
                                 (assoc-in [:sundivers "alice"] 1)))
                   ;; bob can pay (habitat sundivers) and has beacons (default 21).
                   (assoc-in [:players "bob" :habitat :sundivers] 3)
                   ;; Park at the owner-bonus decision: alice's base action is done,
                   ;; one bonus action remains, and the owner (bob) decides first.
                   (assoc-in [:player-turn :action :station-type] :matrix)
                   (assoc-in [:player-turn :action :current-station] [2 0])
                   (assoc-in [:player-turn :action :current-owner] "bob")
                   (assoc-in [:player-turn :action :bonus-total] 1)
                   (assoc-in [:player-turn :action :owner-actions] 0)
                   (assoc-in [:player-turn :action :activator-actions] 0)
                   (assoc-in [:player-turn :choice-player] "bob")
                   (assoc-in [:player-turn :phase] :choose-activate-owner-bonus))
          ;; bob takes the full bonus (max-feasible = 1).
          taken (get (choice/choose-activate-owner-bonus-choices base) 1)]
      (is (some? taken) "the owner can take the bonus")
      (is (= "alice" (game/current-player taken))
          "it is still the activator's turn")
      (is (= "bob" (get-in taken [:player-turn :choice-player]))
          "but the OWNER controls the bonus actions")
      (is (= :owner (get-in taken [:player-turn :action :current-actor]))
          "the bonus actions run as the owner")
      (is (= :choose-activate-matrix-beacon (game/current-phase taken))
          "the owner is placing the bonus beacon")
      ;; Placing the beacon: it is the OWNER's beacon, and control stays with bob.
      (let [beacon-pos (first (game/matrix-beacon-positions taken "bob"))
            beacons0   (get-in taken [:players "bob" :reserve :beacons])
            placed     (get (choice/choose-activate-matrix-beacon-choices taken) beacon-pos)]
        (is (= "bob" (get-in placed [:player-turn :choice-player]))
            "owner still controls the payment step")
        (is (= "bob" (get-in placed [:board beacon-pos :beacon]))
            "the beacon placed belongs to the owner")
        (is (= (dec beacons0) (get-in placed [:players "bob" :reserve :beacons]))
            "the beacon came from the owner's reserve")
        ;; Paying from bob's habitat (nil = habitat) finishes the only bonus action.
        (let [done (get (choice/choose-activate-matrix-spend-choices placed) nil)]
          (is (some? done) "owner pays from their own pool")
          (is (= 2 (get-in done [:players "bob" :habitat :sundivers]))
              "the sundiver was spent from the OWNER's habitat")
          (is (nil? (get-in done [:player-turn :choice-player]))
              "control returns to the activator once the bonus is done")
          (is (= :choose-activate-station (game/current-phase done))
              "back to the activator's station selection"))))))

(deftest convert-not-reactivatable-test
  (testing "a station converted (and auto-activated) this turn cannot be activated again"
    ;; Foundry pattern: sundivers at [3 0] and [2 -1] convert to a station at [2 0].
    ;; Put an extra alice sundiver ON the target so the new station would otherwise
    ;; reappear in the follow-on station-selection (it has a sundiver on its tile).
    (let [state (-> (game/initial-state ["alice"])
                    (place-tile [2 0] :blue)
                    (place-sundiver "alice" [3 0])
                    (place-sundiver "alice" [2 -1])
                    (place-sundiver "alice" [2 0]))
          after (game/convert state "alice" :foundry [2 0] [[3 0] [2 -1]])]
      ;; Recorded as activated on creation, and the target still holds alice's
      ;; sundiver (so without the guard it would be offered for activation again).
      (is (contains? (get-in after [:player-turn :action :activated-stations]) [2 0])
          "converted station is marked activated on creation")
      (is (pos? (get-in after [:board [2 0] :sundivers "alice"] 0)))
      (is (= :choose-activate-self-bonus (game/current-phase after)))
      ;; Decline the automatic activation's bonus to reach station selection…
      (let [at-station (get (choice/choose-activate-self-bonus-choices after) 0)]
        (is (= :choose-activate-station (game/current-phase at-station)))
        ;; …and confirm the just-converted station is not offered again.
        (let [[_ choices] (choice/find-state-raw at-station)]
          (is (not (contains? choices [2 0]))
              "the just-converted station must not be activatable again this turn"))))))

(deftest deconvert-when-stuck-test
  (testing "starting a turn with no usable sundivers but a station forces a deconvert"
    (let [state (-> (game/initial-state ["alice" "bob"])
                    ;; Bob: no sundivers on the board or in his habitat, but owns a tower.
                    (assoc-in [:players "bob" :habitat :sundivers] 0)
                    (assoc-in [:board [2 0]]
                              (-> (game/make-tile :blue)
                                  (game/add-station :tower "bob" 1)))
                    (assoc-in [:players "bob" :stations [2 0]] {:type :tower :level 1})
                    ;; Make it alice's turn so begin-next-player-turn advances to bob.
                    (assoc-in [:player-turn :player] "alice"))
          after (game/begin-next-player-turn state)]
      (is (= "bob" (game/current-player after)))
      (is (zero? (game/total-spendable-sundivers after "bob")))
      (is (= :choose-deconvert (game/current-phase after))
          "bob is forced to deconvert")
      ;; Deconverting the tower moves 3 sundivers FROM bob's reserve to his habitat
      ;; (not minted) and returns the tower piece to reserve.
      (let [res-before    (get-in after [:players "bob" :reserve :sundivers])
            hab-before    (get-in after [:players "bob" :habitat :sundivers])
            towers-before (get-in after [:players "bob" :reserve :towers])
            done          (get (choice/choose-deconvert-choices after) [2 0])]
        (is (nil? (get-in done [:board [2 0] :station])) "station removed from the board")
        (is (not (contains? (get-in done [:players "bob" :stations]) [2 0]))
            "station removed from bob's stations")
        (is (= 3 (get-in done [:players "bob" :habitat :sundivers]))
            "3 sundivers reclaimed for a tower")
        (is (= (- res-before 3) (get-in done [:players "bob" :reserve :sundivers]))
            "the 3 sundivers came FROM the reserve, not minted from nothing")
        (is (= (+ res-before hab-before)
               (+ (get-in done [:players "bob" :reserve :sundivers])
                  (get-in done [:players "bob" :habitat :sundivers])))
            "deconvert conserves the player's total sundivers")
        (is (= (inc towers-before) (get-in done [:players "bob" :reserve :towers]))
            "the tower piece is returned to reserve")
        (is (= :choose-action-type (game/current-phase done))
            "then the player takes a normal turn with the reclaimed sundivers")))))

(deftest keep-card-held-is-an-option-test
  (testing "the held card is offered as an equal keep option alongside drawn cards"
    (let [held    (game/make-card 0 5)
          drawn   [(game/make-card 1 3) (game/make-card 2 7)]
          state   (-> (game/initial-state ["alice"])
                      (assoc-in [:players "alice" :held-card] held)
                      (assoc-in [:player-turn :action :drawn-cards] drawn)
                      (assoc-in [:player-turn :phase] :keep-card))
          choices (choice/choose-keep-card-choices state)]
      ;; All three cards (held + 2 drawn) are options, keyed by the card itself —
      ;; no separate :keep-held option.
      (is (= 3 (count choices)))
      (is (contains? choices held) "the held card is a keep option")
      (is (every? #(contains? choices %) drawn) "the drawn cards are options")
      (is (not (contains? choices :keep-held)) "no separate held/keep option")
      ;; Keeping the held card discards the two drawn cards.
      (let [done (get choices held)]
        (is (= held (get-in done [:players "alice" :held-card])))
        (is (= (set drawn) (set (:discard done))) "the unchosen cards are discarded")))))

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

(def ^:private walker-protected-phases
  "Phases the walker must always decide explicitly — mirrors choice/find-state's
   no-auto-advance set, so auto-advance stops at real decision points."
  #{:choose-action-type :choose-move :choose-convert :choose-activate
    :choose-activate-self-bonus :choose-activate-owner-bonus :choose-land})

(defn- auto-advance-state
  "Follow single-choice, non-protected phases and return the resulting STATE.
   Mirrors choice/find-state's auto-advance but yields the state, so a game-over
   reached this way (e.g. the draw that triggers the 13-flare loss) is observable
   instead of looking like a dead-end."
  [state]
  (loop [s state]
    (let [[phase choices] (choice/find-state-raw s)
          nxt             (first (vals choices))]
      (if (and (= 1 (count choices))
               (not (contains? walker-protected-phases phase))
               nxt
               (not= phase :game-over))
        (recur nxt)
        s))))

(defn- simulate-step
  "Pick one smart choice and return the next state. Throws on a genuine dead-end."
  [state]
  (let [[phase choices] (choice/find-state state)]
    (cond
      ;; The game ended during auto-advance (e.g. drawing the 13th flare → loss,
      ;; which find-state advances straight into). Surface the game-over state
      ;; instead of treating its empty choice set as a dead-end.
      (= phase :game-over)
      (auto-advance-state state)

      (empty? choices)
      (throw (ex-info "Dead end" {:phase phase}))

      :else
      (or
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

      ;; Ark advance (flare/drift-flare are now keyed by destination hex):
      ;; just take the first option (straight ahead).
      :choose-ark-advance        (first (vals choices))
      :choose-flare-advance      (first (vals choices))
      :choose-drift-flare-advance (first (vals choices))

      ;; Drift card: auto-draw
      :draw-drift-card (:draw choices)

      ;; Captain drift: choices are keyed by destination hex (like tower headings),
      ;; not :none/:left/:right — take the first available heading.
      :choose-captain-drift (first (vals choices))

      ;; Never land during the simulation — let all 5 rounds complete
      :choose-land (:continue choices)

      ;; Cipher: prefer centre [0 0] so landing colours accumulate there
      :cipher (or (get choices [0 0]) (first (vals choices)))

      ;; Matrix beacon: prefer tiles without stations to avoid blocking future activations
      :choose-activate-matrix-beacon
      (let [no-station (remove #(get-in state [:board % :station]) (keys choices))]
        (get choices (if (seq no-station) (first no-station) (first (keys choices)))))

      ;; Everything else: first available
      (first (vals choices)))
     ;; Safety net: a case returning nil (e.g. a choice key the walker no longer
     ;; matches, like a renamed phase) must never cause a recur on nil — fall
     ;; back to any valid choice so the simulation keeps progressing.
     (first (vals choices))))))

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
             (contains? #{:choose-action-type :choose-deconvert} (game/current-phase s))
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

;; ── Activation bonus: control and cost travel together ──────────────────────
;; Whoever is asked to make a choice pays for it out of their own pool, even
;; when it is someone else's turn (the owner taking the bonus on their station).

(def ^:private alice-pos [5 0])
(def ^:private bob-pos   [-5 0])

(defn- bonus-setup
  "alice (activator) is activating bob's matrix station; each has board
   sundivers nowhere near the other's, and neither has habitat sundivers, so
   the offered spend positions say unambiguously whose pool is being charged."
  []
  (-> (game/initial-state ["alice" "bob"])
      (assoc-in [:board [2 0]]
                (-> (game/make-tile :blue)
                    (game/add-station :matrix "bob" 1)
                    (assoc-in [:sundivers "alice"] 1)))
      (assoc-in [:board alice-pos] (assoc (game/make-tile :green) :sundivers {"alice" 2}))
      (assoc-in [:board bob-pos]   (assoc (game/make-tile :green) :sundivers {"bob" 2}))
      (assoc-in [:players "alice" :habitat :sundivers] 0)
      (assoc-in [:players "bob" :habitat :sundivers] 0)
      (assoc-in [:player-turn :action-type] :activate)
      (assoc-in [:player-turn :action :station-type] :matrix)
      (assoc-in [:player-turn :action :stations-queue] [[2 0]])
      (assoc-in [:player-turn :action :free-activation?] false)
      game/begin-next-station))

(deftest owner-bonus-is-paid-from-the-owners-board-sundivers-test
  (let [s0 (bonus-setup)]
    (is (= :choose-activate-matrix-beacon (game/current-phase s0)))
    ;; The activator's BASE action is the activator's: their beacon, their cost.
    (let [[_ beacon-cs] (choice/find-state-raw s0)
          s1            (get beacon-cs (first (game/matrix-beacon-positions s0 "alice")))
          [ph spend-cs] (choice/find-state-raw s1)]
      (is (= :choose-activate-matrix-spend ph))
      (is (contains? (set (keys spend-cs)) alice-pos))
      (is (not (contains? (set (keys spend-cs)) bob-pos))
          "the activator's base action must not reach into the owner's pool")

      ;; Base paid → the owner is offered the bonus.
      (let [s2             (get spend-cs alice-pos)
            [ph2 bonus-cs] (choice/find-state-raw s2)]
        (is (= :choose-activate-owner-bonus ph2))
        (is (= "bob" (get-in s2 [:player-turn :choice-player])))

        ;; The owner takes it: every sub-choice, and every cost, is theirs.
        (let [s3         (get bonus-cs 1)
              [ph3 bcs3] (choice/find-state-raw s3)]
          (is (= :owner (get-in s3 [:player-turn :action :current-actor])))
          (is (= :choose-activate-matrix-beacon ph3))
          (let [bpos       (first (game/matrix-beacon-positions s3 "bob"))
                s4         (get bcs3 bpos)
                [ph4 scs4] (choice/find-state-raw s4)]
            (is (= :choose-activate-matrix-spend ph4))
            (is (contains? (set (keys scs4)) bob-pos)
                "the owner's bonus is payable from the OWNER's sundivers")
            (is (not (contains? (set (keys scs4)) alice-pos))
                "the owner's bonus must NOT be paid from the ACTIVATOR's pool")
            ;; …and spending really does come out of bob: he drops 2 → 1, while
            ;; alice still holds the 1 she has left after paying for her base.
            (let [done (get scs4 bob-pos)]
              (is (= 1 (get-in done [:board bob-pos :sundivers "bob"])))
              (is (= 1 (get-in done [:board alice-pos :sundivers "alice"]))
                  "the activator's sundivers are untouched by the owner's bonus"))))))))

;; ── Randomized: the player asked is always the player charged ───────────────

(def ^:private activation-actor-phases
  "Phases whose choices are generated from (actor-player state)."
  #{:choose-activate-matrix-beacon
    :choose-activate-matrix-spend
    :choose-activate-tower-heading
    :choose-activate-tower-spend})

(defn- asked-player [state]
  (or (get-in state [:player-turn :choice-player])
      (game/current-player state)))

(defn- play-checking-actor
  "Walk a random game, always taking the owner bonus when offered, and record
   every activation phase where the player asked differs from the player whose
   resources the choices are built from."
  [seed max-steps counters]
  (let [rng (java.util.Random. seed)]
    (loop [state (game/initial-state ["alice" "bob" "cheryl"])
           step  0
           found []]
      (if (or (>= step max-steps) (:game-over state))
        found
        (let [[phase choices] (choice/find-state-raw state)]
          (if (empty? choices)
            found
            (let [asked (asked-player state)
                  found (if (contains? activation-actor-phases phase)
                          (let [payer (choice/actor-player state)]
                            (when (not= asked (game/current-player state))
                              (swap! counters update :owner-controlled (fnil inc 0)))
                            (if (= asked payer)
                              found
                              (conj found {:seed seed :step step :phase phase
                                           :asked asked :payer payer})))
                          found)
                  ks    (vec (keys choices))
                  k     (cond
                          (= phase :choose-activate-owner-bonus)   (apply max ks)
                          (and (= phase :choose-action-type)
                               (contains? choices :activate)
                               (< (.nextInt rng 10) 7))            :activate
                          (and (= phase :choose-action-type)
                               (contains? choices :convert)
                               (< (.nextInt rng 10) 6))            :convert
                          :else (nth ks (.nextInt rng (count ks))))]
              (if-let [next-state (get choices k)]
                (recur next-state (inc step) found)
                found))))))))

(deftest player-asked-is-the-player-charged-test
  (let [counters (atom {})
        found    (doall (mapcat #(play-checking-actor % 3000 counters) (range 40)))]
    (when (seq found)
      (println "actor/choice-player mismatches:" (take 5 found)))
    (is (pos? (:owner-controlled @counters 0))
        "the walk must actually reach owner-controlled activation phases")
    (is (empty? found)
        "the player asked to choose must be the player whose resources are used")))
