(ns universe.holdem-test
  "The engine is only as good as its chip accounting, so most of this is spent
   proving nothing is created or destroyed -- across random full tables, and
   across the specific shapes that catch poker engines out: short all-ins,
   side pots, split pots, and everybody folding before the board arrives."
  (:require
   [clojure.test :refer [deftest testing is]]
   [universe.deck :as deck]
   [universe.holdem :as holdem]))

(defn- seeded-deck [seed]
  (let [cards (java.util.ArrayList. ^java.util.Collection (vec deck/all-cards))]
    (java.util.Collections/shuffle cards (java.util.Random. seed))
    (vec cards)))

(defn- stack-of [state]
  (reduce + 0 (map :stack (:players state))))

(defn- chips [state]
  (+ (reduce + 0 (map :stack (:players state)))
     (holdem/pot state)))

(defn- pick
  "Any legal action, chosen by the seed -- a monkey at the table."
  [state ^java.util.Random rng]
  (let [{:keys [check call min-raise-to max-raise-to]} (holdem/legal-actions state)
        options (cond-> [{:action :fold}]
                  check        (conj {:action :check})
                  (pos? call)  (conj {:action :call})
                  min-raise-to (conj {:action :raise
                                      :to (+ min-raise-to
                                             (.nextInt rng (inc (- max-raise-to min-raise-to))))}))]
    (nth options (.nextInt rng (count options)))))

(defn- play-hand [state rng]
  (loop [s (holdem/start-hand state (seeded-deck (.nextLong rng)))
         guard 0]
    (cond
      (> guard 500)            (throw (ex-info "hand never ended" {:state s}))
      (holdem/hand-over? s)    s
      :else (let [before (chips s)
                  after  (holdem/act s (:to-act s) (pick s rng))]
              (is (= before (chips after))
                  (str "chips changed on an action at street " (:street s)))
              (recur after (inc guard))))))

(defn- play-game [players seed]
  (let [rng (java.util.Random. seed)]
    (loop [s (holdem/create-game players) n 0]
      (if (or (holdem/game-over? s) (> n 400) (< (count (holdem/with-chips s)) 2))
        s
        (recur (play-hand s rng) (inc n))))))

;; ── Conservation ───────────────────────────────────────────────────────────

(deftest chips-are-conserved
  (testing "no chip is created or destroyed, over whole tables"
    (doseq [players [["a" "b"] ["a" "b" "c"] ["a" "b" "c" "d" "e"]
                     ["a" "b" "c" "d" "e" "f" "g" "h" "i"]]
            seed    (range 6)]
      (let [start (holdem/create-game players)
            end   (play-game players seed)]
        (is (= (chips start) (chips end))
            (str (count players) " players, seed " seed))))))

(deftest tables-finish
  (testing "somebody ends up with everything"
    (doseq [seed (range 8)]
      (let [end (play-game ["a" "b" "c"] seed)]
        (is (holdem/game-over? end) (str "seed " seed " did not finish"))
        (is (= 1 (count (holdem/with-chips end))))
        (is (= (* 3 (:starting-stack end))
               (stack-of end)))))))

;; ── Blinds and order ───────────────────────────────────────────────────────

(deftest blinds-are-posted
  (let [s (holdem/start-hand (holdem/create-game ["a" "b" "c"]) (seeded-deck 1))]
    (testing "button on seat 0, small on 1, big on 2"
      (is (= 0 (:button s)))
      (is (= 1 (:small-blind s)))
      (is (= 2 (:big-blind s))))
    (testing "the blinds are in the pot and out of the stacks"
      (is (= 10 (get-in s [:bets 1])))
      (is (= 20 (get-in s [:bets 2])))
      (is (= 990 (holdem/stack s 1)))
      (is (= 980 (holdem/stack s 2)))
      (is (= 30 (holdem/pot s))))
    (testing "the seat after the big blind speaks first"
      (is (= 0 (:to-act s))))))

(deftest heads-up-reverses-the-blinds
  (let [s (holdem/start-hand (holdem/create-game ["a" "b"]) (seeded-deck 1))]
    (is (= (:button s) (:small-blind s)) "the button posts the small blind")
    (is (= (:button s) (:to-act s)) "and acts first before the board")))

(deftest big-blind-gets-the-option
  (testing "calling round the table does not end the round on the blind"
    (let [s  (holdem/start-hand (holdem/create-game ["a" "b" "c"]) (seeded-deck 2))
          s1 (holdem/act s 0 {:action :call})
          s2 (holdem/act s1 1 {:action :call})]
      (is (= :opening (:street s2)) "still before the board")
      (is (= 2 (:to-act s2)) "the big blind is asked")
      (let [s3 (holdem/act s2 2 {:action :check})]
        (is (= :first (:street s3)) "checking closes it")
        (is (= 1 (count (:board s3))) "and turns one card")))))

(deftest board-comes-one-card-at-a-time
  (let [s (holdem/start-hand (holdem/create-game ["a" "b"]) (seeded-deck 3))
        run (fn [st] (holdem/act st (:to-act st)
                                 (if (:check (holdem/legal-actions st))
                                   {:action :check} {:action :call})))
        s1 (-> s run run)]
    (is (= :first (:street s1)))
    (is (= 1 (count (:board s1))))
    (let [s2 (-> s1 run run)]
      (is (= :second (:street s2)))
      (is (= 2 (count (:board s2))))
      (let [s3 (-> s2 run run)]
        (is (= :third (:street s3)))
        (is (= 3 (count (:board s3))))
        (let [s4 (-> s3 run run)]
          (is (= :showdown (:street s4)))
          (is (= 3 (count (:board s4)))))))))

;; ── Folding ────────────────────────────────────────────────────────────────

(deftest everybody-folds-before-the-board
  (testing "the last player standing takes it without any cards being read"
    (let [s  (holdem/start-hand (holdem/create-game ["a" "b" "c"]) (seeded-deck 4))
          s1 (holdem/act s 0 {:action :fold})
          s2 (holdem/act s1 1 {:action :fold})]
      (is (holdem/hand-over? s2))
      (is (empty? (:board s2)) "no board was ever dealt")
      (is (false? (get-in s2 [:result :showdown?])))
      ;; the pot is 30 but 20 of it was the big blind's own, so it is up by 10
      (is (= 1010 (holdem/stack s2 2)) "the big blind collects the small blind")
      (is (= 3000 (stack-of s2)) "and the table still holds 3000"))))

(deftest an-uncalled-bet-comes-back
  (let [s  (holdem/start-hand (holdem/create-game ["a" "b" "c"]) (seeded-deck 5))
        s1 (holdem/act s 0 {:action :raise :to 200})
        s2 (holdem/act s1 1 {:action :fold})
        s3 (holdem/act s2 2 {:action :fold})]
    (is (holdem/hand-over? s3))
    (is (= 1030 (holdem/stack s3 0)) "kept the blinds, got the 180 overbet back")
    (is (= 3000 (stack-of s3)))))

;; ── Side pots ──────────────────────────────────────────────────────────────

(deftest a-short-stack-caps-what-it-plays-for
  (testing "the classic: a short all-in makes a side pot the short stack cannot win"
    (let [base   (-> (holdem/create-game ["short" "big" "bigger"])
                     (assoc-in [:players 0 :stack] 100))
          s      (holdem/start-hand base (seeded-deck 6))
          ;; seat 0 is the button here, so it acts first before the board
          s1     (holdem/act s 0 {:action :raise :to 100})   ; all-in for 100
          s2     (holdem/act s1 1 {:action :call})
          s3     (holdem/act s2 2 {:action :call})]
      (is (contains? (:all-in s3) 0))
      (is (= 300 (holdem/pot s3)))
      (let [pots (#'holdem/side-pots s3)]
        (is (= 1 (count pots)) "everyone is in for the same 100, so one pot")
        (is (= 300 (:amount (first pots)))))
      ;; now the two deep stacks bet on past the short stack.  the street has
      ;; turned, so bets start from zero again and :to is the total for THIS
      ;; street -- 100 each on top of the 100 already in
      (is (= :first (:street s3)) "the call closed the round and turned a card")
      (let [s4 (holdem/act s3 1 {:action :raise :to 100})
            s5 (holdem/act s4 2 {:action :call})
            pots (#'holdem/side-pots s5)]
        (is (= 2 (count pots)))
        (is (= 300 (:amount (first pots))) "main pot: 100 from each of three")
        (is (= #{0 1 2} (:contenders (first pots))))
        (is (= 200 (:amount (second pots))) "side pot: 100 more from each of two")
        (is (= #{1 2} (:contenders (second pots)))
            "the short stack cannot win the side pot")))))

(deftest side-pot-payouts-conserve-chips
  (testing "over many random tables with mismatched stacks"
    (doseq [seed (range 10)]
      (let [base (-> (holdem/create-game ["a" "b" "c" "d"])
                     (assoc-in [:players 0 :stack] 60)
                     (assoc-in [:players 1 :stack] 350)
                     (assoc-in [:players 2 :stack] 1200)
                     (assoc-in [:players 3 :stack] 90))
            rng  (java.util.Random. seed)
            end  (play-hand base rng)]
        (is (= 1700 (stack-of end)) (str "seed " seed))))))

;; ── Splits ─────────────────────────────────────────────────────────────────

(deftest identical-hands-split-the-pot
  (testing "same numbers, same colors, different shapes -- dead heat"
    (let [a [(deck/card :purple :eye 3)   (deck/card :green :eye 3)]
          b [(deck/card :purple :star 3)  (deck/card :green :star 3)]
          board [(deck/card :yellow :eye 1) (deck/card :purple :helix 2)
                 (deck/card :green :pyramid 4)]]
      (is (zero? (deck/compare-hands (concat a board) (concat b board)))))))

;; ── Hidden information ─────────────────────────────────────────────────────

(deftest a-view-shows-only-your-own-cards
  (let [s (holdem/start-hand (holdem/create-game ["a" "b" "c"]) (seeded-deck 7))
        v (holdem/view s "b")]
    (is (= 1 (count (:hands v))) "one player's hole cards, and they are yours")
    (is (contains? (:hands v) (holdem/seat-of s "b")))
    (is (nil? (:deck v)) "the undealt deck would give the whole hand away")
    (is (= 3 (count (:players v))) "but the table itself is public"))
  (testing "an observer sees no hole cards at all"
    (let [s (holdem/start-hand (holdem/create-game ["a" "b"]) (seeded-deck 8))]
      (is (empty? (:hands (holdem/view s nil))))))
  (testing "a showdown opens the hands that reached it"
    (let [s (holdem/start-hand (holdem/create-game ["a" "b"]) (seeded-deck 9))
          run (fn [st] (holdem/act st (:to-act st)
                                   (if (:check (holdem/legal-actions st))
                                     {:action :check} {:action :call})))
          done (nth (iterate run s) 8)]
      (is (holdem/hand-over? done))
      (when (get-in done [:result :showdown?])
        (is (= 2 (count (:hands (holdem/view done "a"))))
            "both hands are face up once they are shown")))))

;; ── Blinds outgrowing the table ────────────────────────────────────────────

(deftest blinds-that-swallow-every-stack
  (testing "heads-up, both stacks under the blinds: nobody can be asked to act"
    (let [base (-> (holdem/create-game ["a" "b"])
                   (assoc-in [:players 0 :stack] 8)
                   (assoc-in [:players 1 :stack] 6))
          s    (holdem/start-hand base (seeded-deck 11))]
      (is (holdem/hand-over? s) "so the hand resolved on the deal")
      (is (= 3 (count (:board s))) "the board ran out on its own")
      (is (get-in s [:result :showdown?]) "and the two hands were compared")
      (is (= 14 (chips s)) "the 14 chips are all still there")))
  (testing "with a third player still holding chips, the hand plays on"
    (let [base (-> (holdem/create-game ["a" "b" "c"])
                   (assoc-in [:players 0 :stack] 500)
                   (assoc-in [:players 1 :stack] 8)
                   (assoc-in [:players 2 :stack] 6))
          s    (holdem/start-hand base (seeded-deck 11))]
      (is (not (holdem/hand-over? s)) "seat 0 posted no blind and can still act")
      (is (= 0 (:to-act s)))
      (let [done (holdem/act s 0 {:action :call})]
        (is (holdem/hand-over? done) "and once it does, there is nobody left to ask")
        (is (= 3 (count (:board done))) "so the board runs out")
        (is (= 514 (chips done)))))))

(deftest a-stack-shorter-than-the-blind-is-all-in-for-what-it-has
  (let [base (-> (holdem/create-game ["a" "b" "c"])
                 (assoc-in [:players 1 :stack] 4))   ; small blind is 10
        s    (holdem/start-hand base (seeded-deck 12))]
    (is (contains? (:all-in s) 1))
    (is (= 4 (get-in s [:bets 1])) "posted what it had, not what it owed")
    (is (zero? (holdem/stack s 1)))))

;; ── What the table came to ─────────────────────────────────────────────────

(deftest a-finished-table-can-be-summarised
  (let [end (play-game ["a" "b" "c"] 3)
        s   (holdem/summary end)]
    (testing "a hand was recorded for every hand played"
      (is (pos? (:hands s)))
      (is (= (:hands s) (count (:history end)))))
    (testing "the chip lines account for everyone, from the same starting stack"
      (is (= 3 (count (:series s))))
      (doseq [line (:series s)]
        (is (= [0 (:starting-stack end)] (first (:points line)))
            "every line starts where the player sat down")
        (is (= (inc (:hands s)) (count (:points line)))
            "one point per hand, plus the start"))
      (is (= (* 3 (:starting-stack end))
             (reduce + 0 (map (comp second last :points) (:series s))))
          "the last point of every line still sums to the whole table"))
    (testing "the winner is the one still holding chips"
      (is (= (:winner end) (:winner s)))
      (is (= (:winner s)
             (:player (apply max-key :final (:series s))))))
    (testing "pots and hands won are counted, not guessed"
      (is (= (:hands s) (reduce + 0 (map :won (:series s))))
          "every hand had exactly one winner, or a split counted per winner")
      (is (pos? (:amount (:biggest-pot s)))))
    (testing "the best hand shown is really the best that was shown"
      (when-let [best (:best-hand s)]
        (let [strengths (for [h (:history end)
                              [_ {:keys [hand]}] (:shown h)
                              :when hand]
                          (:strength hand))]
          (is (= (apply max strengths) (:strength (:hand best))))
          (is (string? (deck/hand-name (:hand best)))))))))

(deftest the-history-never-carries-a-hand-that-was-not-shown
  (testing "folding to a bet keeps your cards, in the record as at the table"
    (let [s  (holdem/start-hand (holdem/create-game ["a" "b" "c"]) (seeded-deck 5))
          s1 (holdem/act s 0 {:action :raise :to 200})
          s2 (holdem/act s1 1 {:action :fold})
          s3 (holdem/act s2 2 {:action :fold})
          entry (last (:history s3))]
      (is (false? (:showdown? entry)))
      (is (nil? (:shown entry)) "nobody's cards were turned over, so none are kept")
      (is (seq (:awards entry)) "but who won it is public"))))

(deftest the-history-is-only-sent-once-it-matters
  (testing "a table in progress does not re-broadcast every hand it has played"
    (let [mid (holdem/start-hand (holdem/create-game ["a" "b"]) (seeded-deck 2))]
      (is (nil? (:history (holdem/view mid "a")))
          "no history mid-game -- it would be re-sent on every action")))
  (testing "and the finished table hands over the lot"
    (let [end (play-game ["a" "b"] 1)]
      (when (holdem/game-over? end)
        (is (seq (:history (holdem/view end "a"))))))))

(deftest a-busted-player-line-knows-where-it-ended
  (let [end (play-game ["a" "b" "c"] 3)
        s   (holdem/summary end)]
    (testing "the winner never busts"
      (let [won (first (filter #(= (:winner s) (:player %)) (:series s)))]
        (is (nil? (:out-at won)))))
    (testing "everybody else does, and at the hand their stack first hit zero"
      (doseq [line (remove #(= (:winner s) (:player %)) (:series s))]
        (is (some? (:out-at line)) (str (:player line) " never busted but did not win"))
        (is (zero? (:final line)))
        (is (zero? (second (first (filter #(= (:out-at line) (first %)) (:points line)))))
            "the hand named is one where they held nothing")
        (is (pos? (second (nth (:points line) (dec (:out-at line)))))
            "and the hand before it, they still had chips")))))
