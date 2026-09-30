(ns distinctions.game
  "DISTINCTIONS: a draw-and-discard game with the sixty-four card deck.

   Every card is one answer to six two-way questions, so a card is just a
   number 0..63 and each distinction is one bit of it. See `axes`.

   Everyone is dealt eight cards and one card is turned up to start the
   discard pile. On your turn you take either the top of the discard pile or
   the top of the deck, then discard one card face up. You win when, after
   your discard, your hand is two teams of four: one team agrees on three
   distinctions, the other on the other three, so between them the hand
   covers all six.

   One player is a game too: the same rules, played against the turn count.

   You may not throw back the card you just took from the pile: that would be
   a pass dressed up as a turn.

   This namespace holds no atoms and does no I/O. The deal is passed in. The
   one exception is running out of deck mid-game: the pile under its top card
   is shuffled back in, and that shuffle is `clojure.core/shuffle`.")

;; ── The deck ───────────────────────────────────────────────────────────────

(def axes
  "The six distinctions, most significant bit first, each with its two
   values in bit order (0 then 1)."
  [{:key :background  :bit 5 :values ["black" "white"]}
   {:key :foreground  :bit 4 :values ["red" "blue"]}
   {:key :composition :bit 3 :values ["circle" "bar"]}
   {:key :eye         :bit 2 :values ["no eye" "eye"]}
   {:key :rays        :bit 1 :values ["no rays" "rays"]}
   {:key :inversion   :bit 0 :values ["plain" "inverted"]}])

(def axis-by-key (into {} (map (juxt :key identity) axes)))

(def all-cards (vec (range 64)))

(defn value
  "0 or 1: which way this card answers the distinction."
  [card axis-key]
  (bit-and 1 (bit-shift-right card (:bit (axis-by-key axis-key)))))

(defn value-name [axis-key v]
  (get-in (axis-by-key axis-key) [:values v]))

(defn attrs
  "The card spelled out: {:background \"white\" :foreground \"red\" ...}."
  [card]
  (into {} (for [{k :key} axes] [k (value-name k (value card k))])))

(defn describe [card]
  (let [a (attrs card)]
    (str (:background a) "/" (:foreground a) " " (:composition a)
         (when (= 1 (value card :eye)) " + eye")
         (when (= 1 (value card :rays)) " + rays")
         (when (= 1 (value card :inversion)) ", inverted"))))

;; ── Hands ──────────────────────────────────────────────────────────────────

(def targets
  "The twelve values: one value of one distinction."
  (vec (for [{k :key} axes v [0 1]] [k v])))

(defn target-name [[k v]] (value-name k v))

(defn target-counts
  "How many cards in the hand hold each target, as {[axis value] n}."
  [hand]
  (into {} (for [[k v :as t] targets]
             [t (count (filter #(= v (value % k)) hand))])))

;; ── Two teams of four ──────────────────────────────────────────────────────

(defn- choose
  "Every k-element subset of xs, in order."
  [k xs]
  (cond (zero? k)   [[]]
        (empty? xs) []
        :else (concat (map #(into [(first xs)] %) (choose (dec k) (rest xs)))
                      (choose k (rest xs)))))

(defn shared
  "The distinctions every card in `cards` agrees on, as [[axis value] ...]."
  [cards]
  (vec (for [{k :key} axes
             :let [vs (set (map #(value % k) cards))]
             :when (= 1 (count vs))]
         [k (first vs)])))

(def splits
  "The twenty ways to split the six distinctions three and three, each as
   [these those]."
  (let [ks (mapv :key axes)]
    (vec (for [these (choose 3 ks)]
           [(vec these) (vec (remove (set these) ks))]))))

(defn covered
  "If the hand is two teams of four covering all six distinctions, the two
   teams as [{:cards [...] :shared [[axis value] ...]} ...]; otherwise nil.
   A team may agree on more than three -- what matters is that each agrees
   on at least three and between them nothing is left out."
  [hand]
  (when (= 8 (count hand))
    (first
     (for [team (choose 4 (sort hand))
           :let [other (vec (remove (set team) hand))
                 a (shared team) b (shared other)]
           :when (and (>= (count a) 3) (>= (count b) 3)
                      (= 6 (count (set (map first (concat a b))))))]
       [{:cards (vec team) :shared a} {:cards (vec (sort other)) :shared b}]))))

(defn cover-progress
  "How close the hand is to two teams of four: the best split found, as
   {:missing n :ways w :teams [{:axes [...] :values [...] :cards [...]} x2]}.
   `missing` counts cards still to find; `ways` how many splits are that
   close. Greedy -- the first four of a group -- but good enough to steer by."
  [hand]
  (reduce
   (fn [best [these those]]
     (reduce
      (fn [best [vals group]]
        (let [team  (vec (take 4 group))
              rest* (remove (set team) hand)
              [vals2 group2] (apply max-key (comp count val)
                                    (or (seq (group-by (fn [c] (mapv #(value c %) those)) rest*))
                                        [[nil []]]))
              miss  (+ (max 0 (- 4 (count group))) (max 0 (- 4 (count group2))))
              found {:missing miss :ways 1
                     :teams [{:axes these :values vals :cards team}
                             {:axes those :values vals2 :cards (vec (take 4 group2))}]}]
          (cond (< miss (:missing best)) found
                (= miss (:missing best)) (update best :ways inc)
                :else best)))
      best
      (group-by (fn [c] (mapv #(value c %) these)) hand)))
   {:missing 99 :ways 0}
   splits))

;; ── Setup ──────────────────────────────────────────────────────────────────

(def hand-size 8)

(defn create-game
  [player-names]
  {:players   (vec (map-indexed (fn [i n] {:name n :seat i}) player-names))
   :hand-size hand-size
   :phase     :waiting
   :hands     {}
   :deck      []
   :discard   []
   :to-act    nil
   :step      nil
   :taken     nil
   :turn      0
   :reshuffles 0
   :log       []
   :winner    nil
   :made      nil})

(defn seat-count [state] (count (:players state)))

(defn player-name [state seat] (get-in state [:players seat :name]))

(defn seat-of [state name]
  (some (fn [p] (when (= name (:name p)) (:seat p))) (:players state)))

(defn max-players
  "Everyone's hand plus one card to start the pile, plus one left to draw."
  [hand-size]
  (quot (- 64 2) hand-size))

(defn start
  "Deal from `deck` (a permutation of all-cards): a hand to everyone, then
   one card face up."
  [state deck]
  (let [n     (seat-count state)
        size  (:hand-size state)
        hands (into {} (for [s (range n)]
                         [s (vec (subvec (vec deck) (* s size) (* (inc s) size)))]))
        rest* (drop (* n size) deck)]
    (assoc state
           :phase   :playing
           :hands   hands
           :discard [(first rest*)]
           :deck    (vec (rest rest*))
           :to-act  0
           :step    :draw
           :taken   nil
           :turn    1
           :log     [{:event :deal :card (first rest*)}])))

(defn current-player [state]
  (when (= :playing (:phase state))
    (player-name state (:to-act state))))

(defn game-over? [state] (= :over (:phase state)))

(defn top-discard [state] (peek (:discard state)))

;; ── Turns ──────────────────────────────────────────────────────────────────

(defn- refill
  "Out of deck: everything in the pile but its top card goes back, shuffled."
  [state]
  (if (seq (:deck state))
    state
    (let [pile (:discard state)]
      (-> state
          (assoc :deck (vec (shuffle (pop pile)))
                 :discard [(peek pile)])
          (update :reshuffles inc)
          (update :log conj {:event :reshuffle})))))

(defn legal-actions
  "What the player to act may do: {:draw true :take true} before they have
   drawn, {:discard #{cards}} after."
  [state]
  (when (= :playing (:phase state))
    (case (:step state)
      :draw    {:draw (boolean (or (seq (:deck state)) (> (count (:discard state)) 1)))
                :take (boolean (top-discard state))}
      :discard {:discard (disj (set (get-in state [:hands (:to-act state)]))
                               (:taken state))}
      nil)))

(defn- next-seat [state seat] (mod (inc seat) (seat-count state)))

(defn act
  "Apply one action for `seat`. An illegal action returns the state itself,
   unchanged and identical, which is how the server tells it was refused.

     {:action :draw}              top of the deck
     {:action :take}              top of the discard pile
     {:action :discard :card c}   put c on the pile"
  [state seat {:keys [action card]}]
  (let [legal (legal-actions state)]
    (if (not= seat (:to-act state))
      state
      (case action
        :draw
        (if-not (:draw legal)
          state
          (let [s (refill state)
                c (first (:deck s))]
            (-> s
                (update :deck (comp vec rest))
                (update-in [:hands seat] conj c)
                (assoc :step :discard :taken nil)
                (update :log conj {:event :draw :seat seat}))))

        :take
        (if-not (:take legal)
          state
          (let [c (top-discard state)]
            (-> state
                (update :discard pop)
                (update-in [:hands seat] conj c)
                (assoc :step :discard :taken c)
                (update :log conj {:event :take :seat seat :card c}))))

        :discard
        (if-not (contains? (:discard legal) card)
          state
          (let [hand (vec (remove #{card} (get-in state [:hands seat])))
                s    (-> state
                         (assoc-in [:hands seat] hand)
                         (update :discard conj card)
                         (assoc :taken nil)
                         (update :log conj {:event :discard :seat seat :card card}))]
            (if-let [m (covered hand)]
              (-> s
                  (assoc :phase :over :step nil :to-act nil
                         :winner (player-name s seat) :made m)
                  (update :log conj {:event :win :seat seat :made m}))
              (-> s
                  (assoc :to-act (next-seat s seat) :step :draw)
                  (update :turn inc)))))

        state))))

;; ── What each person gets to see ───────────────────────────────────────────

(defn view
  "The state as `player` may see it: their own hand, the size of everyone
   else's, the pile, and how many cards are left -- never the deck's order.
   Once the game is over every hand is shown."
  [state player]
  (let [you  (seat-of state player)
        open (game-over? state)]
    (cond-> (-> state
                (assoc :you you
                       :deck-count (count (:deck state))
                       :hand-counts (into {} (for [[s h] (:hands state)] [s (count h)])))
                (dissoc :deck)
                (update :hands #(if open % (select-keys % [you]))))
      (and you (= you (:to-act state))) (assoc :actions (legal-actions state)))))
