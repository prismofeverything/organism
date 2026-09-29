(ns universe.holdem
  "No-limit UNIVERSE hold'em: two cards to each player, three to the table,
   revealed one at a time.

   Your hand is your two plus all three -- exactly five cards, never a choice
   of which five.  That is the whole reason the structure is 2+3 rather than
   2+5: five numbers over sixty cards saturate fast, and any game that deals
   you more than five and lets you keep the best five stops playing the chart
   the deck actually prints.  At 2+3 every hand is a uniform five-card hand, so
   all nineteen rows occur at exactly their printed frequency.

   Four betting rounds, as hold'em has: after the hole cards, then after each
   board card.

   A table can instead be made `:seven?`: Texas hold'em's shape, two to you and
   five to the board -- a flop of three, a turn, a river -- keeping your best
   five of seven. It ranks by the seven-card chart (deck/seven-order), which is
   not the five-card one; see universe/hands7.py for why and what it costs.

   This namespace is pure.  The shuffled deck is passed in, so a game replays
   exactly from its seed and the tests can deal whatever they like."
  (:require
   [universe.deck :as deck]))

;; ── Setup ──────────────────────────────────────────────────────────────────

(def default-levels
  "Blinds climb so a table finishes.  Small/big."
  [[10 20] [20 40] [30 60] [50 100] [75 150]
   [100 200] [150 300] [250 500] [400 800] [600 1200] [1000 2000]])

(def defaults
  {:starting-stack  1000
   :hands-per-level 8
   :levels          default-levels})

(def streets
  "Betting rounds in order.  :opening is dealt hole cards; the other three each
   turn one board card face up."
  [:opening :first :second :third])

(def ^:private street-after
  (zipmap streets (rest streets)))

(defn create-game
  "A table of players, each sat down with the same stack."
  [player-names & [opts]]
  (let [{:keys [starting-stack hands-per-level levels seven?]} (merge defaults opts)]
    {:seven?          (boolean seven?)
     :players         (vec (map-indexed (fn [i n] {:name n :seat i :stack starting-stack})
                                        player-names))
     :button          (dec (count player-names))
     :hand-number     0
     :level           0
     :hands-per-level hands-per-level
     :levels          (vec levels)
     :starting-stack  starting-stack
     :street          :waiting
     :board           []
     :hands           {}
     :seated          #{}
     :bets            {}
     :committed       {}
     :folded          #{}
     :all-in          #{}
     :acted           #{}
     :locked          #{}
     :revealed        #{}
     :history         []
     :deck            []
     :min-raise       0
     :result          nil
     :log             []
     :winner          nil}))

;; ── Seats ──────────────────────────────────────────────────────────────────

(defn seat-count [state] (count (:players state)))

;; ── The board ──────────────────────────────────────────────────────────────

(defn board-size
  "How many cards the board comes to: three, or five on a seven-card table."
  [state]
  (if (:seven? state) 5 3))

(defn- street-cards
  "How many board cards a street turns: one each, except the seven-card
   table's flop, which is three at once."
  [state street]
  (if (and (:seven? state) (= :first street)) 3 1))

(defn hand-value
  "How good these cards are at this table, as something `compare` reads:
   exactly five on the three-card board, the best five of seven otherwise."
  [state cards]
  (if (:seven? state)
    (deck/best-code cards)
    (deck/value cards)))

(defn hand-row
  "The chart row these cards make at this table."
  [state cards]
  (if (:seven? state)
    (deck/seven-row cards)
    (deck/classify cards)))

(defn stack [state seat] (get-in state [:players seat :stack] 0))

(defn player-name [state seat] (get-in state [:players seat :name]))

(defn seat-of
  "The seat this player is sitting in, or nil."
  [state name]
  (some (fn [p] (when (= name (:name p)) (:seat p))) (:players state)))

(defn- order-from
  "Seats in turn order, starting at the one after `seat` and wrapping once."
  [state seat]
  (let [n (seat-count state)]
    (map #(mod (+ seat 1 %) n) (range n))))

(defn in-hand?
  "Dealt into this hand and not folded.  All-in players are still in the hand."
  [state seat]
  (and (contains? (:seated state) seat)
       (not (contains? (:folded state) seat))))

(defn live?
  "In the hand and with chips left, so still able to act."
  [state seat]
  (and (in-hand? state seat)
       (not (contains? (:all-in state) seat))))

(defn- seats-in-hand [state] (filter #(in-hand? state %) (range (seat-count state))))
(defn- seats-live    [state] (filter #(live? state %)    (range (seat-count state))))

(defn with-chips
  "Seats that still have chips, and so play the next hand."
  [state]
  (filter #(pos? (stack state %)) (range (seat-count state))))

;; ── Chips ──────────────────────────────────────────────────────────────────

(defn current-bet
  "The most anyone has put in front of them this round."
  [state]
  (apply max 0 (vals (:bets state))))

(defn- wager
  "Move up to `amount` from a seat's stack into its bet.  A player who cannot
   cover it is all-in for what they have, which is always legal."
  [state seat amount]
  (let [have (stack state seat)
        put  (min amount have)]
    (cond-> (-> state
                (update-in [:players seat :stack] - put)
                (update-in [:bets seat] (fnil + 0) put)
                (update-in [:committed seat] (fnil + 0) put))
      (zero? (- have put)) (update :all-in conj seat))))

(defn pot
  "Everything committed this hand, across every street."
  [state]
  (reduce + 0 (vals (:committed state))))

(def ^:private log-cap
  "The log is for reading back the hand in play, not the history of the table,
   and every entry is broadcast to everyone on every message."
  400)

(defn- note [state entry]
  (update state :log
          (fn [l]
            (let [l' (conj (or l []) (assoc entry :hand (:hand-number state)))]
              (if (> (count l') log-cap)
                (vec (drop (- (count l') log-cap) l'))
                l')))))

;; ── Starting a hand ────────────────────────────────────────────────────────

(declare advance)

(defn- blinds [state]
  (nth (:levels state)
       (min (:level state) (dec (count (:levels state))))))

(defn- next-button
  "One seat to the left each hand, skipping anyone who has busted.  create-game
   parks it behind seat 0 so the first hand puts it on seat 0."
  [state]
  (first (filter #(pos? (stack state %)) (order-from state (:button state)))))

(defn start-hand
  "Deal a hand from `cards` -- a shuffled sequence of card ids.  Posts the
   blinds and sets the first player to act.

   Heads-up reverses the blinds, as hold'em does: the button posts the small
   blind and acts first before the board, last after it."
  [state cards]
  (let [playing (vec (with-chips state))]
    (if (< (count playing) 2)
      state
      (let [button        (next-button state)
            hand-number   (inc (:hand-number state))
            level         (quot (dec hand-number) (:hands-per-level state))
            [small big]   (blinds (assoc state :level level))
            heads-up?     (= 2 (count playing))
            after-button  (filter (set playing) (order-from state button))
            small-seat    (if heads-up? button (first after-button))
            big-seat      (if heads-up?
                            (first after-button)
                            (second after-button))
            deal          (vec cards)
            hands         (into {} (map-indexed
                                    (fn [i seat] [seat [(nth deal (* 2 i))
                                                        (nth deal (inc (* 2 i)))]])
                                    playing))
            rest-deck     (vec (drop (* 2 (count playing)) deal))
            base          (assoc state
                                 :hand-number hand-number
                                 :level       level
                                 :button      button
                                 :street      :opening
                                 :board       []
                                 :hands       hands
                                 :seated      (set playing)
                                 :bets        {}
                                 :committed   {}
                                 :folded      #{}
                                 :all-in      #{}
                                 :acted       #{}
                                 :locked      #{}
                                 :revealed    #{}
                                 :result      nil
                                 :deck        rest-deck
                                 :min-raise   big)
            posted        (-> base
                              (wager small-seat small)
                              (wager big-seat big))
            ;; first to act is the seat after the big blind; heads-up that is
            ;; the button, who posted the small blind
            opener        (first (filter #(live? posted %)
                                         (order-from posted big-seat)))]
        (cond-> (-> posted
                    (assoc :to-act opener
                           :small-blind small-seat
                           :big-blind big-seat)
                    (note {:event :hand-start :button button :blinds [small big]}))
          ;; blinds climb, and eventually they swallow a stack whole -- when
          ;; the deal itself puts everyone all-in there is nobody to ask, so
          ;; the board runs out and the hand shows down on the spot
          (nil? opener) advance)))))

;; ── What the player to act may do ──────────────────────────────────────────

(defn legal-actions
  "What the seat to act is allowed to do.

     :call        chips needed to match (0 means checking is free)
     :min-raise-to / :max-raise-to   the window for a raise, as a total
     :all-in      the total this seat reaches by pushing everything

   A raise is stated as the total you want your bet to become, which is how
   poker says it out loud and leaves no room to disagree about the increment."
  [state]
  (let [seat (:to-act state)]
    (when (and seat (live? state seat) (not= :showdown (:street state)))
      (let [bet     (get-in state [:bets seat] 0)
            locked? (contains? (:locked state) seat)
            high    (current-bet state)
            owed    (- high bet)
            have    (stack state seat)
            all-in  (+ bet have)
            min-to  (+ high (:min-raise state))]
        {:seat         seat
         :player       (player-name state seat)
         :fold         true
         :check        (zero? owed)
         :call         (min owed have)
         :all-in       all-in
         ;; you can always push, even when it is short of a full raise -- unless
         ;; somebody else already pushed short at you, which owes you a call but
         ;; does not hand back the right to raise
         :min-raise-to (when (and (> all-in high) (not locked?)) (min min-to all-in))
         :max-raise-to (when (and (> all-in high) (not locked?)) all-in)}))))

(defn- legal?
  [state seat action]
  (let [{:keys [fold check call min-raise-to max-raise-to] :as allowed}
        (legal-actions state)]
    (and allowed
         (= seat (:seat allowed))
         (case (:action action)
           :fold  (boolean fold)
           :check (boolean check)
           :call  (pos? call)
           :raise (let [to (:to action)]
                    (and min-raise-to (integer? to)
                         (>= to min-raise-to) (<= to max-raise-to)))
           false))))

;; ── Advancing ──────────────────────────────────────────────────────────────

(defn- betting-closed?
  "A round is over once every player who can still act has acted since the last
   aggression and has matched the bet.  The big blind's option before the board
   falls out of this: posting a blind is not acting."
  [state]
  (let [live (seats-live state)
        high (current-bet state)]
    (or (empty? live)
        (and (every? #(contains? (:acted state) %) live)
             (every? #(= high (get-in state [:bets %] 0)) live)))))

(defn- collect
  "Sweep the street's bets away.  :committed keeps the running total, which is
   what the side pots are built from, so nothing is lost here."
  [state]
  (assoc state :bets {} :acted #{} :locked #{} :min-raise (second (blinds state))))

(defn- no-more-betting?
  "Nobody left who can be asked for another chip."
  [state]
  (<= (count (seats-live state)) 1))

(defn- side-pots
  "The pot in layers, smallest commitment first.  Everyone who put in at least
   a layer contributed to it; only those who did and did not fold can win it.

   This is the whole of the side-pot rule: a short all-in caps what they are
   playing for, and the excess forms a pot above them.

   `:paid-in` and `:depth` are kept because a layer can end up with no
   contenders at all -- everyone who bought into it folded, which is legal
   whenever checking was free -- and those chips have to go back to the people
   who put them in rather than evaporate."
  [state]
  (let [committed (:committed state)
        levels    (sort (distinct (remove zero? (vals committed))))]
    (->> levels
         (reduce
          (fn [{:keys [pots previous]} level]
            (let [depth   (- level previous)
                  paid-in (filter #(>= (get committed % 0) level) (keys committed))
                  amount  (* depth (count paid-in))
                  contest (set (filter #(in-hand? state %) paid-in))]
              {:previous level
               :pots     (if (pos? amount)
                           (conj pots {:amount     amount
                                       :depth      depth
                                       :paid-in    (set paid-in)
                                       :contenders contest})
                           pots)}))
          {:pots [] :previous 0})
         :pots)))

(defn- return-uncalled
  "Give back the part of a bet nobody covered.  Doing this before the pots are
   built is what guarantees every layer has somebody who can win it."
  [state]
  (let [committed (:committed state)
        amounts   (sort > (vals committed))
        top       (first amounts)
        second-   (or (second amounts) 0)]
    (if (and top (> top second-))
      (let [seat  (some (fn [[s v]] (when (= v top) s)) committed)
            extra (- top second-)]
        (-> state
            (update-in [:players seat :stack] + extra)
            (update-in [:committed seat] - extra)
            (note {:event :returned :seat seat :amount extra})))
      state)))

(defn- award
  "Split each layer among its best hands.  An odd chip goes to the first
   winner left of the button, which is where poker puts it.

   A layer with one contender is paid out without looking at any cards -- when
   everybody folds before the board is complete there are only two cards to
   look at, and no five-card hand to speak of."
  [state]
  (let [board    (:board state)
        showdown (= (board-size state) (count board))
        value-of (fn [seat] (hand-value state (concat (get-in state [:hands seat]) board)))]
    (reduce
     (fn [st {:keys [amount contenders depth paid-in]}]
       (let [contenders (vec contenders)
             best-hands (if (and showdown (> (count contenders) 1))
                          (let [top (reduce (fn [a b]
                                              (if (neg? (compare (value-of a) (value-of b))) b a))
                                            contenders)
                                best-v (value-of top)]
                            (filterv #(= best-v (value-of %)) contenders))
                          contenders)
             winners (vec (filter (set best-hands) (order-from state (:button state))))
             share   (when (seq winners) (quot amount (count winners)))
             odd     (when (seq winners) (- amount (* share (count winners))))]
         (if (empty? winners)
           ;; nobody left who can win this layer -- hand it back to whoever
           ;; bought into it, which is the only way chips are conserved
           (reduce (fn [s seat]
                     (-> s
                         (update-in [:players seat :stack] + depth)
                         (note {:event :refund :seat seat :amount depth})))
                   st
                   paid-in)
           (reduce
            (fn [s [i seat]]
              (let [got (+ share (if (zero? i) odd 0))]
                (-> s
                    (update-in [:players seat :stack] + got)
                    (update-in [:result :awards seat] (fnil + 0) got))))
            st
            (map-indexed vector winners)))))
     (assoc state :result {:awards {} :pots (side-pots state)})
     (side-pots state))))

(def ^:private history-cap
  "A long tournament is a few hundred hands; the summary at the end wants all
   of them, and each entry is a line of a table."
  300)

(defn- remember-hand
  "Record what a hand came to, for the summary when the table finishes.

   Only what was public: who won what, the board, and the hands that were
   actually turned over. A hand that everybody folded to reveals nothing, and
   the winner's cards stay theirs."
  [state]
  (let [result    (:result state)
        showdown? (:showdown? result)]
    (update state :history
            (fn [h]
              (let [h' (conj (or h [])
                             {:hand      (:hand-number state)
                              :board     (:board state)
                              :showdown? (boolean showdown?)
                              :pot       (reduce + 0 (map :amount (:pots result)))
                              :awards    (:awards result)
                              :shown     (when showdown?
                                           (into {} (filter (comp :hand val) (:hands result))))
                              :stacks    (into {} (map (juxt :seat :stack) (:players state)))})]
                (if (> (count h') history-cap)
                  (vec (drop (- (count h') history-cap) h'))
                  h'))))))

(defn- finish
  "Everyone folded but one, or the last board card has been bet.  Pay out and
   park the table until the next hand is dealt."
  [state]
  (let [state   (return-uncalled state)
        staying (seats-in-hand state)
        showdown? (> (count staying) 1)
        state   (-> state
                    (assoc :street :showdown)
                    (assoc :revealed (if showdown? (set staying) #{}))
                    award
                    ;; the chips have gone back to the stacks, so the pot is
                    ;; empty -- :result keeps what it was for the table to read
                    (assoc :bets {} :committed {})
                    (assoc-in [:result :board] (:board state))
                    (assoc-in [:result :showdown?] showdown?)
                    (assoc-in [:result :hands]
                              (into {} (map (fn [s]
                                              [s {:cards (get-in state [:hands s])
                                                  :hand  (when (and showdown?
                                                                    (= (board-size state)
                                                                       (count (:board state))))
                                                           (hand-row
                                                            state
                                                            (concat (get-in state [:hands s])
                                                                    (:board state))))}])
                                            staying))))
        state   (remember-hand state)
        left    (with-chips state)]
    (cond-> (note state {:event :hand-end})
      (= 1 (count left)) (assoc :winner (player-name state (first left))
                                :street :complete))))

(defn- reveal
  "Turn the next board card face up."
  [state]
  (let [card (first (:deck state))]
    (-> state
        (update :board conj card)
        (update :deck (comp vec rest))
        (note {:event :board :card card}))))

(defn- open-street
  "Start a betting round.  After the board comes out the first live seat left
   of the button speaks first, whatever happened before."
  [state]
  (let [opener (first (filter #(live? state %) (order-from state (:button state))))]
    (assoc state :to-act opener)))

(defn- advance
  "Move the hand on: next player, next street, or the showdown."
  [state]
  (cond
    ;; everyone but one folded -- no cards need to come out
    (<= (count (seats-in-hand state)) 1)
    (finish state)

    (not (betting-closed? state))
    (assoc state :to-act (first (filter #(live? state %)
                                        (order-from state (:to-act state)))))

    :else
    (let [state (collect state)
          done? (= :third (:street state))]
      (cond
        done? (finish state)

        ;; all-in already: run the rest of the board out and show them down
        (no-more-betting? state)
        (finish (reduce (fn [s _] (reveal s)) state
                        (range (- (board-size state) (count (:board state))))))

        :else
        (let [street (street-after (:street state))]
          (-> (reduce (fn [s _] (reveal s))
                      (assoc state :street street)
                      (range (street-cards state street)))
              open-street))))))

;; ── Acting ─────────────────────────────────────────────────────────────────

(defn act
  "Apply one action for one seat.  Returns the state unchanged if it is not
   that seat's turn or the action is not legal, so a stale click from a
   browser cannot move the game."
  [state seat action]
  (if-not (legal? state seat action)
    state
    (let [high (current-bet state)
          bet  (get-in state [:bets seat] 0)]
      (advance
       (case (:action action)
         :fold
         (-> state
             (update :folded conj seat)
             (update :acted conj seat)
             (note {:event :fold :seat seat}))

         :check
         (-> state
             (update :acted conj seat)
             (note {:event :check :seat seat}))

         :call
         (-> state
             (wager seat (- high bet))
             (update :acted conj seat)
             (note {:event :call :seat seat :amount (- high bet)}))

         :raise
         (let [to        (:to action)
               increment (- to high)
               ;; an all-in short of a full raise does not reopen the betting
               full?     (>= increment (:min-raise state))]
           (-> state
               (wager seat (- to bet))
               (assoc :acted  (if full? #{seat} (conj (:acted state) seat))
                      :locked (if full? #{} (into (:locked state) (:acted state))))
               (update :min-raise max increment)
               (note {:event :raise :seat seat :to to :short-all-in (not full?)}))))))))

;; ── Reading the table ──────────────────────────────────────────────────────

(defn current-player
  "Whose turn it is, by name -- nil between hands."
  [state]
  (when (and (:to-act state) (not (#{:showdown :complete :waiting} (:street state))))
    (player-name state (:to-act state))))

(defn hand-over? [state]
  (contains? #{:showdown :complete :waiting} (:street state)))

(defn game-over? [state] (some? (:winner state)))

(defn summary
  "What the table came to, once somebody has all the chips.

   Everything here is read back off `:history`, which only ever held public
   facts, so a summary gives nothing away that the table did not already show."
  [state]
  (let [history (:history state)
        players (:players state)
        start   (:starting-stack state)
        name-of (fn [seat] (get-in state [:players seat :name]))
        shown   (for [h history
                      [seat {:keys [cards hand]}] (:shown h)
                      :when hand]
                  {:seat seat :player (name-of seat) :cards cards :hand hand
                   :board (:board h) :at-hand (:hand h)})
        wins    (frequencies (for [h history seat (keys (:awards h))] seat))
        taken   (reduce (fn [acc h]
                          (reduce-kv (fn [m seat amount] (update m seat (fnil + 0) amount))
                                     acc (:awards h)))
                        {} history)]
    {:hands      (count history)
     :winner     (:winner state)
     :showdowns  (count (filter :showdown? history))
     :biggest-pot (when (seq history)
                    (let [h (apply max-key :pot history)]
                      {:amount (:pot h) :hand (:hand h)
                       :players (mapv name-of (keys (:awards h)))}))
     :best-hand  (when (seq shown)
                   (apply max-key (comp :strength :hand) shown))
     ;; one line per player for the chart: where their stack stood after each
     ;; hand, starting from what everybody sat down with
     :series     (vec (for [{:keys [seat name]} players]
                        (let [points (into [[0 start]]
                                           (map (fn [h] [(:hand h) (get (:stacks h) seat 0)]))
                                           history)]
                          {:seat seat
                           :player name
                           :won (get wins seat 0)
                           :taken (get taken seat 0)
                           :final (get-in state [:players seat :stack])
                           ;; the hand they busted on, if they did. Everybody but
                           ;; the winner ends a tournament at zero, so a line drawn
                           ;; to the right-hand edge would put every loser's label
                           ;; on the same pixel and say nothing for the fifty hands
                           ;; they were not in.
                           :out-at (first (for [[hand v] points
                                                :when (and (zero? v) (pos? hand))]
                                            hand))
                           :points points})))}))

(def recent-count
  "How many finished hands a view carries for the rail."
  4)

(defn view
  "What one player is allowed to see.

   Every other game on this site broadcasts one state to every watcher, which
   works because none of them hide anything.  Poker does, so the state is cut
   down per person here: you see your own two cards, and anyone else's only
   once they have been shown at a showdown.  An observer is passed nil and
   sees the table without any hole cards at all."
  [state player]
  (let [seat     (when player (seat-of state player))
        visible  (cond-> (:revealed state) seat (conj seat))]
    (-> state
        (assoc :hands (into {} (filter (fn [[s _]] (contains? visible s)) (:hands state))))
        (assoc :you seat)
        (assoc :pot (pot state))
        (assoc :actions (when (and seat (= seat (:to-act state))) (legal-actions state)))
        ;; only this hand's log: the rest is nobody's business and would be
        ;; re-sent in full on every action
        (update :log (fn [l] (vec (filter #(= (:hand %) (:hand-number state)) l))))
        ;; the last few hands, for the rail to show what was turned over --
        ;; public already, and too few to weigh anything
        (assoc :recent (vec (take-last recent-count (:history state))))
        ;; the whole history is only wanted once, for the summary at the end --
        ;; sending it on every action would be a few hundred hands each time
        (update :history (fn [h] (when (:winner state) h)))
        ;; the undealt deck is nobody's business, and it is the one thing that
        ;; would give the whole hand away
        (dissoc :deck)
        (update :result
                (fn [r] (when r
                          (update r :hands
                                  (fn [hs] (into {} (filter (fn [[s _]] (contains? visible s)) hs))))))))))
