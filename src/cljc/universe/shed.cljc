(ns universe.shed
  "UNIVERSE shedding: get rid of all your cards.

   The deck is dealt out -- fifteen each at two to four players, twelve at
   five, ten at six -- and whoever holds the lowest card leads. A lead is:

     a single card
     a set     two to four cards of one number, all one color or all one shape
     a run     three to five consecutive numbers, all one color or all one shape

   Everyone after, in turn, either beats it -- the same form, the same size,
   sharing the lead's color-or-shape flavor, with a higher top card -- or
   passes. When everybody else has passed on the last play, whoever made it
   leads again, anything they like. The first to empty their hand wins.

   Cards rank by number, then by color, purple over green over yellow, the way
   hold'em breaks its ties. Shape never ranks. A pattern ranks by its top card.

   Why color-or-shape and not plain sets and runs: with plain ones nearly
   every hand empties in four plays and nearly every lead can be beaten; with
   the flavor required, hands take three to ten plays and the bigger colored
   runs often cannot be answered at all. See universe/shed_*.py.

   Pure, like holdem: the deal is passed in."
  (:require
   [clojure.string :as str]
   [universe.deck :as deck]))

;; ── Cards and patterns ─────────────────────────────────────────────────────

(defn rank
  "Number first, then color: purple 1 is above yellow 1 and below yellow 2."
  [card]
  (+ (* 3 (deck/number card)) (- 2 (deck/color-index card))))

(defn- one? [f cards] (apply = (map f cards)))

(defn- flavors
  "Which of color and shape every one of these cards agrees on."
  [cards]
  (cond-> #{}
    (one? deck/color-index cards) (conj :color)
    (one? deck/shape-index cards) (conj :shape)))

(defn classify
  "What these cards are as a play, or nil if they are not one:
   {:form :single|:set|:run :size n :flavors #{...} :top rank}."
  [cards]
  (let [cards (vec (distinct cards))
        n     (count cards)
        nums  (sort (map deck/number cards))
        fl    (when (> n 1) (flavors cards))
        top   (when (pos? n) (apply max (map rank cards)))]
    (cond
      (= n 1)
      {:form :single :size 1 :flavors #{} :top top}

      (and (<= 2 n 4) (apply = nums) (seq fl))
      {:form :set :size n :flavors fl :top top}

      (and (<= 3 n 5) (= nums (range (first nums) (+ (first nums) n))) (seq fl))
      {:form :run :size n :flavors fl :top top}

      :else nil)))

(defn beats?
  "Does play `p` answer lead `lead`?"
  [p lead]
  (and p
       (= (:form p) (:form lead))
       (= (:size p) (:size lead))
       (or (= :single (:form p))
           (seq (filter (:flavors lead) (:flavors p))))
       (> (:top p) (:top lead))))

(defn describe-play
  "\"run of 4, all purple\" -- what a play is, in words."
  [cards]
  (when-let [{:keys [form size flavors]} (classify cards)]
    (let [c (first cards)
          how (str/join " and " (for [f [:color :shape] :when (flavors f)]
                                  (if (= f :color)
                                    (str "all " (name (deck/color c)))
                                    (str "all " (name (deck/shape c)) "s"))))]
      (case form
        :single (deck/describe c)
        :set    (str "set of " size " " (deck/number c) "s, " how)
        :run    (str "run of " size ", " how)))))

;; ── Every play a hand holds ────────────────────────────────────────────────

(defn- choose
  [k xs]
  (cond (zero? k) [[]]
        (empty? xs) []
        :else (concat (map #(into [(first xs)] %) (choose (dec k) (rest xs)))
                      (choose k (rest xs)))))

(defn- one-each
  "Every way to pick one card from each group."
  [groups]
  (reduce (fn [acc g] (for [a acc c g] (conj a c))) [[]] groups))

(defn plays
  "Every legal play in `hand`, as card vectors."
  [hand]
  (let [by-flavor (concat (vals (group-by deck/color-index hand))
                          (vals (group-by deck/shape-index hand)))]
    (distinct
     (concat
      (map vector hand)
      (for [grp by-flavor
            same (vals (group-by deck/number grp))
            k    (range 2 (inc (min 4 (count same))))
            sub  (choose k same)]
        (vec (sort sub)))
      (for [grp   by-flavor
            :let  [by-num (group-by deck/number grp)]
            len   [3 4 5]
            start (range 1 (- 7 len))
            :when (every? #(by-num (+ start %)) (range len))
            pick  (one-each (map #(by-num (+ start %)) (range len)))]
        (vec (sort pick)))))))

;; ── Setup ──────────────────────────────────────────────────────────────────

(defn hand-size
  "The deck split evenly, but never more than fifteen: two or three players
   holding thirty or twenty cards can answer anything, and the rest of the
   deck is simply left out."
  [players]
  (min 15 (quot deck/card-count players)))

(defn create-game [player-names]
  {:mode      :shed
   :players   (vec (map-indexed (fn [i n] {:name n :seat i}) player-names))
   :phase     :waiting
   :hands     {}
   :lead      nil
   :passed    #{}
   :to-act    nil
   :out       []
   :turn      0
   :log       []
   :winner    nil})

(defn seat-count [state] (count (:players state)))
(defn player-name [state seat] (get-in state [:players seat :name]))
(defn seat-of [state name]
  (some (fn [p] (when (= name (:name p)) (:seat p))) (:players state)))

(defn start
  "Deal from `deck`; the holder of the lowest card leads."
  [state deck]
  (let [n     (seat-count state)
        size  (hand-size n)
        hands (into {} (for [s (range n)]
                         [s (vec (sort-by rank (subvec (vec deck) (* s size) (* (inc s) size))))]))
        first-seat (apply min-key #(rank (first (hands %))) (range n))]
    (assoc state
           :phase  :playing
           :hands  hands
           :to-act first-seat
           :lead   nil
           :passed #{}
           :turn   1
           :log    [{:event :deal :seat first-seat}])))

(defn current-player [state]
  (when (= :playing (:phase state)) (player-name state (:to-act state))))

(defn game-over? [state] (= :over (:phase state)))

(defn- in-play
  "Seats still holding cards."
  [state]
  (remove (set (:out state)) (range (seat-count state))))

(defn- next-in-play [state seat]
  (let [n (seat-count state) live (set (in-play state))]
    (first (filter live (map #(mod (+ seat %) n) (range 1 (inc n)))))))

(defn legal-actions
  "For whoever is to act: whether they are leading (and so may not pass)."
  [state]
  (when (= :playing (:phase state))
    {:leading? (nil? (:lead state))
     :pass     (some? (:lead state))}))

;; ── Turns ──────────────────────────────────────────────────────────────────

(defn- trick-over?
  "Everyone still in, other than whoever made the last play, has passed."
  [state]
  (let [owner (get-in state [:lead :seat])]
    (every? (:passed state) (remove #{owner} (in-play state)))))

(defn- after-trick
  "The last play stood. Its owner leads -- or, if that play emptied their
   hand, the next player still in."
  [state]
  (let [owner (get-in state [:lead :seat])
        leader (if (some #{owner} (in-play state)) owner (next-in-play state owner))]
    (-> state
        (assoc :lead nil :passed #{} :to-act leader)
        (update :log conj {:event :trick :seat owner}))))

(defn act
  "Apply one action for `seat`; an illegal one returns the state unchanged.

     {:action :play :cards [...]}
     {:action :pass}"
  [state seat {:keys [action cards]}]
  (let [hand (get-in state [:hands seat])
        lead (:lead state)]
    (cond
      (or (not= :playing (:phase state)) (not= seat (:to-act state)))
      state

      (= action :pass)
      (if-not lead
        state
        (let [s (-> state
                    (update :passed conj seat)
                    (update :log conj {:event :pass :seat seat}))]
          (if (trick-over? s)
            (after-trick s)
            (assoc s :to-act (next-in-play s seat)))))

      (= action :play)
      (let [cards (vec (distinct cards))
            p     (classify cards)]
        (if-not (and p (every? (set hand) cards) (or (nil? lead) (beats? p lead)))
          state
          (let [left (vec (remove (set cards) hand))
                s    (-> state
                         (assoc-in [:hands seat] left)
                         (assoc :lead (assoc p :cards cards :seat seat) :passed #{})
                         (update :turn inc)
                         (update :log conj {:event :play :seat seat :cards cards}))]
            (if (empty? left)
              (-> s
                  (update :out conj seat)
                  (assoc :phase :over :to-act nil :winner (player-name s seat))
                  (update :log conj {:event :out :seat seat}))
              (assoc s :to-act (next-in-play s seat))))))

      :else state)))

;; ── What each person sees ──────────────────────────────────────────────────

(defn view [state player]
  (let [you (seat-of state player)]
    (-> state
        (assoc :you you
               :hand-counts (into {} (for [[s h] (:hands state)] [s (count h)]))
               :actions (when (and you (= you (:to-act state))) (legal-actions state)))
        (update :hands #(if (game-over? state) % (select-keys % [you])))
        (update :log #(vec (take-last 40 %))))))

;; ── A plain opponent ───────────────────────────────────────────────────────

(defn- play-key
  "Leading: shed the most cards at once, lowest first. Following: the
   cheapest thing that beats it."
  [cards]
  [(- (count cards)) (:top (classify cards))])

(defn bot-action
  "Lead the biggest, lowest pattern it has; follow with the lowest play that
   beats the lead; pass when nothing does. It never holds back."
  [state]
  (let [seat (:to-act state)
        hand (get-in state [:hands seat])
        lead (:lead state)
        ps   (plays hand)]
    (if lead
      (if-let [best (first (sort-by (comp :top classify)
                                    (filter #(beats? (classify %) lead) ps)))]
        {:action :play :cards best}
        {:action :pass})
      {:action :play :cards (first (sort-by play-key ps))})))

(defn auto-action
  "What the table does for somebody who has stopped answering: pass if they
   may, otherwise lead their lowest card."
  [state]
  (if (:lead state)
    {:action :pass}
    {:action :play :cards [(first (sort-by rank (get-in state [:hands (:to-act state)])))]}))
