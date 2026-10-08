(ns universe.network
  "UNIVERSE network: hold ten cards that link every color, shape and number.

   Two cards are linked when exactly one of the three differs -- and when it
   is the number that differs, only by one (a green 5 pyramid links to a green
   4 pyramid, a yellow 5 pyramid, a green 5 eye). A network is a set of cards
   all joined by links that between them show all three colors, all four
   shapes and all five numbers. The smallest possible is ten cards -- so with
   a hand of ten, a network is the whole hand, every card adding one value.

   Everyone is dealt ten and has their own face-up discard pile. On your turn:

     1. set aside any number of cards, k, if you like;
     2. draw k + 1 cards, one at a time, each from the deck or the top of any
        face-up pile -- anyone's, your own included;
     3. discard one card. The set-aside cards go onto your pile in the order
        you chose, then that discard on top.

   So everyone ends every turn on ten cards, and your own pile is a bank you
   can draw back from -- one anybody else can raid.

   The first network ends the game. Everyone scores the size of their pile,
   plus `penalty` for every value -- color, shape or number -- their best
   linked group is still missing. Lowest wins. (A penalty per unlinked card
   was tried first and barely ever applied: hands building toward a network
   end up all linked, and short on coverage instead.)

   When the deck runs out: if anyone has no discards, or it is the third time,
   the game ends and is scored as it stands. Otherwise whoever has the fewest
   discards gives them all back, everyone else gives back as many from the
   bottom of their pile, and that is shuffled into the new deck. Alone, the
   deck also turns a card face up between turns, to draw from.

   Pure apart from reshuffling, which uses `clojure.core/shuffle`."
  (:require
   [universe.deck :as deck]))

;; ── Links and networks ─────────────────────────────────────────────────────

(def hand-size 10)
(def max-players 4)

(def penalty
  "What each value your best linked group is missing costs at the end."
  3)

(def run-out-limit
  "The deck may run out this many times; the last time ends the game. Games
   that never make a network otherwise run for a very long time."
  3)

(defn linked?
  "Exactly one of color, shape and number differs -- a number only by one."
  [a b]
  (let [dc (not= (deck/color-index a) (deck/color-index b))
        ds (not= (deck/shape-index a) (deck/shape-index b))
        dn (- (deck/number a) (deck/number b))]
    (case (+ (if dc 1 0) (if ds 1 0) (if (zero? dn) 0 1))
      1 (or dc ds (= 1 (if (neg? dn) (- dn) dn)))
      false)))

(defn coverage
  "How many of the twelve values -- 3 colors, 4 shapes, 5 numbers -- these
   cards show between them."
  [cards]
  (+ (count (set (map deck/color-index cards)))
     (count (set (map deck/shape-index cards)))
     (count (set (map deck/number cards)))))

(defn spans? [cards] (= 12 (coverage cards)))

(defn components
  "The hand split into linked groups."
  [cards]
  (loop [left (set cards) out []]
    (if (empty? left)
      out
      (let [grp (loop [seen #{(first left)} frontier [(first left)]]
                  (if (empty? frontier)
                    seen
                    (let [nxt (for [f frontier c left
                                    :when (and (not (seen c)) (linked? f c))]
                                c)]
                      (recur (into seen nxt) (distinct nxt)))))]
        (recur (reduce disj left grp) (conj out (vec (sort grp))))))))

(defn connected? [cards] (= 1 (count (components cards))))

(defn- choose
  [k xs]
  (cond (zero? k) [[]]
        (empty? xs) []
        :else (concat (map #(into [(first xs)] %) (choose (dec k) (rest xs)))
                      (choose k (rest xs)))))

(defn network
  "The smallest network in these cards, or nil."
  [cards]
  (let [cards (vec (sort cards))]
    (when (spans? cards)
      (first (for [k (range 10 (inc (count cards)))
                   sub (choose k cards)
                   :when (and (spans? sub) (connected? sub))]
               sub)))))


(defn best-group
  "The linked group in the hand that shows the most values, the bigger on a
   tie."
  [cards]
  (reduce (fn [a b] (if (pos? (compare [(coverage b) (count b)] [(coverage a) (count a)])) b a))
          [] (components cards)))

;; ── Setup ──────────────────────────────────────────────────────────────────

(defn create-game [player-names]
  {:mode      :network
   :players   (vec (map-indexed (fn [i n] {:name n :seat i}) player-names))
   :phase     :waiting
   :hands     {}
   :piles     {}
   :aside     []
   :draws-left 0
   :step      nil
   :deck      []
   :flip      []
   :to-act    nil
   :turn      0
   :run-outs  0
   :log       []
   :winner    nil})

(defn seat-count [state] (count (:players state)))
(defn solo? [state] (= 1 (seat-count state)))
(defn player-name [state seat] (get-in state [:players seat :name]))
(defn seat-of [state name]
  (some (fn [p] (when (= name (:name p)) (:seat p))) (:players state)))

(defn missing
  "How many of the twelve values your best linked group does not show."
  [state seat]
  (- 12 (coverage (best-group (get-in state [:hands seat])))))

(defn score
  "Your pile, plus `penalty` for each missing value. Lower is better."
  [state seat]
  (+ (count (get-in state [:piles seat]))
     (* (:penalty state penalty) (missing state seat))))

(defn scores [state]
  (into {} (for [s (range (seat-count state))] [s (score state s)])))

(defn- finish [state why]
  (let [sc (scores state)]
    (-> state
        (assoc :phase :over :to-act nil :step nil :scores sc
               :winner (player-name state (apply min-key sc (range (seat-count state)))))
        (update :log conj {:event :over :why why}))))

(defn game-over? [state] (= :over (:phase state)))

(defn current-player [state]
  (when (= :playing (:phase state)) (player-name state (:to-act state))))

;; ── The deck ───────────────────────────────────────────────────────────────

(defn- run-out
  "The deck is empty. If anyone has no discards, or this is the last run-out
   allowed, that is the end. Otherwise the fewest discards anyone holds, n:
   everyone gives back n from the bottom of their pile, and those -- with any
   turned-up cards but the top one -- become the new deck."
  [state]
  (let [seats (range (seat-count state))
        sizes (map #(count (get-in state [:piles %])) seats)]
    (cond
      (some zero? sizes)                                          (finish state :no-discards)
      (>= (inc (:run-outs state)) (:run-out-limit state run-out-limit)) (finish state :run-out-limit)
      :else
      (let [n     (apply min sizes)
            back  (mapcat #(take n (get-in state [:piles %])) seats)
            flips (:flip state)]
        (-> state
            (update :piles (fn [ps] (into {} (for [[s p] ps] [s (vec (drop n p))]))))
            (assoc :deck (vec (shuffle (concat back (butlast flips))))
                   :flip (if (seq flips) [(peek flips)] []))
            (update :run-outs inc)
            (update :log conj {:event :run-out :each n}))))))

(defn- turn-up
  "Alone, nobody discards to you, so the deck turns a card face up between
   turns for you to take instead."
  [state]
  (if-not (solo? state)
    state
    (let [s (if (empty? (:deck state)) (run-out state) state)]
      (if (or (game-over? s) (empty? (:deck s)))
        s
        (-> s
            (update :flip conj (first (:deck s)))
            (update :deck (comp vec rest)))))))

(defn sources
  "Everywhere a card can be drawn from right now: :deck, :flip when alone,
   and [:pile seat] for every pile with a card on it."
  [state]
  (concat [:deck]
          (when (and (solo? state) (seq (:flip state))) [:flip])
          (for [s (range (seat-count state)) :when (seq (get-in state [:piles s]))]
            [:pile s])))

(defn top
  "The card a source would give: nil for the deck, which is face down."
  [state source]
  (cond (= :flip source)   (peek (:flip state))
        (vector? source)   (peek (get-in state [:piles (second source)]))
        :else              nil))

;; ── Turns ──────────────────────────────────────────────────────────────────

(defn start
  "Ten each; alone, the deck turns up the first card to draw from."
  [state deck]
  (let [n     (seat-count state)
        deck  (vec deck)
        hands (into {} (for [s (range n)] [s (subvec deck (* s hand-size) (* (inc s) hand-size))]))]
    (turn-up
     (assoc state
            :phase  :playing
            :hands  hands
            :piles  (into {} (for [s (range n)] [s []]))
            :deck   (subvec deck (* n hand-size))
            :to-act 0
            :step   :start
            :aside  []
            :draws-left 0
            :turn   1
            :log    [{:event :start}]))))

(defn legal-actions
  "Where you are in your turn: :start (set aside and draw), :drawing (draws
   left to take), or :discard."
  [state]
  (when (= :playing (:phase state))
    {:step       (:step state)
     :draws-left (:draws-left state)
     :sources    (vec (sources state))}))

(defn- draw-from
  "Take one card from `source` into `seat`'s hand."
  [state seat source]
  (cond
    (= :deck source)
    (let [s (if (empty? (:deck state)) (run-out state) state)]
      (if (or (game-over? s) (empty? (:deck s)))
        (if (game-over? s) s (finish s :empty))
        (-> s
            (update-in [:hands seat] conj (first (:deck s)))
            (update :deck (comp vec rest))
            (update :log conj {:event :draw :seat seat :from :deck}))))

    :else
    (let [path (if (= :flip source) [:flip] [:piles (second source)])
          c    (peek (get-in state path))]
      (-> state
          (update-in path pop)
          (update-in [:hands seat] conj c)
          (update :face-up-drawn (fnil conj #{}) c)
          (update :log conj {:event :draw :seat seat :from source :card c})))))

(defn- end-turn [state seat]
  (if (network (get-in state [:hands seat]))
    (-> state
        (update :log conj {:event :network :seat seat})
        (finish :network))
    (-> (turn-up state)
        (as-> s (if (game-over? s) s
                    (assoc s :to-act (mod (inc seat) (seat-count s))
                           :step :start :aside [] :draws-left 0 :face-up-drawn #{})))
        (update :turn inc))))

(defn act
  "Apply one action for `seat`; an illegal one returns the state unchanged.

     {:action :draw :from source}                     a draw
     {:action :draw :from source :set-aside [a b]}    the first draw of a turn,
                                                      setting a and b aside --
                                                      three draws in all
     {:action :discard :card c}                       the end of the turn

   A source is :deck, :flip, or [:pile seat]."
  [state seat {:keys [action from set-aside card]}]
  (let [hand (get-in state [:hands seat])
        step (:step state)]
    (cond
      (or (not= :playing (:phase state)) (not= seat (:to-act state)))
      state

      (= action :draw)
      (let [aside (vec (distinct set-aside))]
        (if-not (and (#{:start :drawing} step)
                     (some #{from} (sources state))
                     (or (empty? aside) (and (= step :start) (every? (set hand) aside))))
          state
          (let [s (cond-> state
                    (= step :start) (-> (assoc :aside aside :draws-left (inc (count aside)))
                                        (assoc-in [:hands seat] (vec (remove (set aside) hand)))
                                        (cond-> (seq aside)
                                          (update :log conj {:event :set-aside :seat seat
                                                             :count (count aside)}))))
                s (draw-from s seat from)]
            (if (game-over? s)
              s
              (let [left (dec (:draws-left s))]
                (assoc s :draws-left left :step (if (pos? left) :drawing :discard)))))))

      (= action :discard)
      (if-not (and (= step :discard) (some #{card} hand))
        state
        (let [s (-> state
                    (assoc-in [:hands seat] (vec (remove #{card} hand)))
                    (update-in [:piles seat] into (conj (:aside state) card))
                    (assoc :aside [])
                    (update :log conj {:event :discard :seat seat :card card}))]
          (end-turn s seat)))

      :else state)))

;; ── What each person sees ──────────────────────────────────────────────────

(defn view [state player]
  (let [you (seat-of state player)]
    (-> state
        (assoc :you you
               :deck-count (count (:deck state))
               :scores (scores state)
               :actions (when (and you (= you (:to-act state))) (legal-actions state)))
        (dissoc :deck)
        (update :hands (fn [hs] (if (game-over? state) hs (select-keys hs [you]))))
        ;; set-aside cards are face down until they land on the pile
        (update :aside (fn [a] (if (= you (:to-act state)) a (vec (repeat (count a) nil)))))
        (update :log #(vec (take-last 40 %))))))

;; ── A plain opponent ───────────────────────────────────────────────────────

(defn value
  "How close a hand is: the coverage of its best linked group, then its
   size."
  [cards]
  (let [g (best-group cards)]
    (+ (* 100 (coverage g)) (count g))))

(defn- worst
  "The card whose loss hurts least, never one of `keep`."
  ([hand] (worst hand #{}))
  ([hand keep] (apply max-key #(value (remove #{%} hand)) (remove keep hand))))

(defn- strays [hand]
  (let [g (set (best-group hand))] (remove g hand)))

(defn bot-action
  "Set aside up to `reach` cards outside its largest linked group, which buys
   that many more looks; draw the face-up card that helps most, or blind from
   the deck; throw away whatever matters least -- but never a card it drew
   face up this turn. People may; a bot doing it is only a pass, and a table
   of bots passing never uses up the deck, so never ends."
  ([state] (bot-action state 2))
  ([state reach]
   (let [seat (:to-act state)
         hand (get-in state [:hands seat])
         step (:step state)
         aside (when (= step :start) (vec (take reach (strays hand))))
         kept (vec (remove (set aside) hand))
         ;; a face-up card is worth taking only if the hand still ends the
         ;; turn better once the discard is made -- otherwise it is a pass,
         ;; and a table of passes never uses up the deck, so never ends
         gain (fn [c] (let [with (conj kept c)]
                        (value (remove #{(worst with #{c})} with))))
         best (->> (sources state)
                   (keep (fn [src] (when-let [c (top state src)] [src (gain c)])))
                   (sort-by (comp - second))
                   first)
         from (if (and best (> (second best) (value kept))) (first best) :deck)]
     (case step
       :start   (cond-> {:action :draw :from from} (seq aside) (assoc :set-aside aside))
       :drawing {:action :draw :from from}
       :discard {:action :discard :card (worst hand (:face-up-drawn state #{}))}
       nil))))

(def auto-action
  "For somebody who has stopped answering: draw one blind, throw the card
   that matters least."
  #(bot-action % 0))
