(ns universe.player
  "Opponents that know what their cards are worth.

   ORACLE reads its own five cards and stops there: it calls what is cheap,
   raises what looks big, folds the rest. That misses the two things that
   actually decide a hand of hold'em — whether you are ahead of the hands you
   cannot see, and whether the price you are being asked is worth paying. A hand
   that looks strong against nobody is often behind three opponents, and a weak
   hand is worth calling when the pot is laying enough.

   So these players do it properly:

     equity   deal the rest of the board and everyone else's cards out a few
              thousand times and count how often you win. This is the real
              showdown rule — `holdem/hand-value`, ties included — not a proxy for it.
     price    the pot odds, `call / (pot + call)`: the share of the pot you must
              win to break even on the call.

   Call when equity beats the price. Raise when it beats it comfortably. Fold
   otherwise. On top of that sits a `profile` — how much edge the player insists
   on, how often it raises, how often it bluffs — which is the whole difference
   between the opponents, and the reason they do not all play one hand the same
   way."
  (:require
   [universe.deck :as deck]
   [universe.holdem :as holdem]))

;; ── Profiles ────────────────────────────────────────────────────────────────

(def profiles
  "Named ways to play. `edge` is how much better than the price a call has to
   look; `aggression` how readily a good hand raises; `bluff` how often a bad
   one does anyway — without some bluffing a player is transparent, and an
   opponent who only raises the nuts is free to fold against."
  {"SIBYL"  {:edge 0.06  :aggression 0.35 :bluff 0.04 :sizing 0.55
             :description "Patient. Wants a clear edge before it puts chips in, and rarely bluffs — but when it raises, believe it."}
   "AUGUR"  {:edge -0.01 :aggression 0.65 :bluff 0.16 :sizing 0.80
             :description "Aggressive. Plays thin edges, raises often and bluffs enough that you cannot simply fold to it."}
   "HARUSPEX" {:edge 0.02 :aggression 0.5 :bluff 0.09 :sizing 0.66
               :description "Balanced. Calls on the odds, raises with the best of it, and mixes in enough bluffs to stay honest."}})

(def default-profile (get profiles "HARUSPEX"))

;; ── Equity ──────────────────────────────────────────────────────────────────

(defn opponents-live
  "How many other players could still turn cards over. Equity falls fast with
   each one: a hand that beats one random holding often loses to the best of
   three."
  [state seat]
  (count (filter #(and (not= % seat) (holdem/in-hand? state %))
                 (range (holdem/seat-count state)))))

(defn equity
  "The share of the pot `hole` is worth on this board against `opponents`
   unknown hands.

   Dealt out `samples` times from the cards nobody can see. A tie counts as its
   fraction of the split, which is what the showdown actually pays.

   `table` is the game being played, for how big its board is and how its
   hands are valued; without one it is the three-card game."
  ([hole board opponents samples] (equity hole board opponents samples (java.util.Random. 20260926)))
  ([hole board opponents samples rng] (equity hole board opponents samples rng {}))
  ([hole board opponents samples ^java.util.Random rng table]
   (let [known (into #{} (concat hole board))
         unseen (vec (remove known deck/all-cards))
         needed (- (holdem/board-size table) (count board))
         value #(holdem/hand-value table %)
         draw (+ needed (* 2 opponents))]
     (if (or (zero? opponents) (> draw (count unseen)))
       ;; Nobody to beat, or a board that cannot be dealt: no information to add.
       (if (zero? opponents) 1.0 0.5)
       (loop [n 0 score 0.0]
         (if (>= n samples)
           (/ score samples)
           ;; Partial Fisher-Yates: only the cards actually dealt are shuffled.
           (let [pool (object-array unseen)
                 _ (dotimes [i draw]
                     (let [j (+ i (.nextInt rng (- (count unseen) i)))
                           a (aget pool i)]
                       (aset pool i (aget pool j))
                       (aset pool j a)))
                 rest-board (map #(aget pool %) (range needed))
                 mine (value (concat hole board rest-board))
                 others (map (fn [o]
                               (let [at (+ needed (* 2 o))]
                                 (value (concat [(aget pool at) (aget pool (inc at))]
                                                board rest-board))))
                             (range opponents))
                 best (reduce (fn [a b] (if (neg? (compare a b)) b a)) others)
                 cmp (compare mine best)]
             (recur (inc n)
                    (+ score (cond
                               (pos? cmp) 1.0
                               (neg? cmp) 0.0
                               ;; Split with everyone else holding the same value.
                               :else (/ 1.0 (inc (count (filter #(= mine %) others))))))))))))))

;; ── Deciding ────────────────────────────────────────────────────────────────

(defn price
  "The share of the final pot a call has to win to break even."
  [pot call]
  (let [total (+ (double pot) call)]
    (if (pos? total) (/ (double call) total) 0.0)))

(defn decide
  "What to do, given what the hand is worth and what it costs.

   Returns the same action maps `holdem/act` takes, so this drops in wherever
   the old heuristic sat."
  [state {:keys [edge aggression bluff sizing] :as _profile} samples rng]
  (let [{:keys [check call min-raise-to max-raise-to]} (holdem/legal-actions state)
        seat (:to-act state)
        hole (get-in state [:hands seat])
        board (:board state)
        against (opponents-live state seat)
        eq (equity hole board against samples rng state)
        pot (holdem/pot state)
        need (price pot call)
        ;; How far ahead of the asking price this hand is. Everything below
        ;; turns on this one number.
        margin (- eq need)
        roll (.nextDouble ^java.util.Random rng)
        raise-to (when min-raise-to
                   (min max-raise-to
                        (max min-raise-to (long (+ call (* sizing pot))))))]
    (cond
      ;; A hand well ahead of the price raises, sometimes. Always raising with
      ;; it would be as readable as never doing so.
      (and raise-to (> margin (+ edge 0.15)) (< roll aggression))
      {:action :raise :to raise-to}

      ;; A hand with nothing, bet as though it had something — rarely, and only
      ;; when checking is the alternative, so the bluff costs a bet and not a call.
      (and raise-to check (< eq 0.35) (< roll bluff))
      {:action :raise :to raise-to}

      (zero? call) (if check {:action :check} {:action :call})
      (> margin edge) {:action :call}
      :else {:action :fold})))

(defn actor
  "A step function for the bot registry: [state] -> action map."
  ([profile] (actor profile 1200))
  ([profile samples]
   (let [rng (java.util.Random.)]
     (fn [state] (decide state profile samples rng)))))
