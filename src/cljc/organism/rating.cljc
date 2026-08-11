(ns organism.rating
  "Player rating for free-for-all games that record a single winner.

   Two systems run side by side over the same stream of finished games:

   ELO      — one number that moves by a fixed step K. Legible (\"+18\") but it
              never says how sure it is, so a player with one game and a player
              with forty look alike.
   GLICKO-2 — a rating plus a deviation (RD) and a volatility. RD widens while
              a player is away and narrows as they play, so a newcomer's number
              moves fast and a regular's barely twitches.

   Both are two-player formulas and ORGANISM seats up to six, so a finished
   game is decomposed into its pairs: the winner scores 1 against every loser,
   and the losers draw with each other — the game only records a single winner,
   so their order among themselves is genuinely unknown. Each pairing carries
   weight 1/(N-1), which keeps one game worth one game's worth of movement no
   matter how crowded the table. The fifteen pairings of a six-player game
   encode one winner, not fifteen independent results.")

;; ── Shared constants ────────────────────────────────────────────────────────

(def initial-rating 1500.0)
(def initial-rd 350.0)
(def initial-volatility 0.06)

(def provisional-games
  "Below this many rated games a player is still finding their level: Elo uses
   the larger K, and the leaderboard marks them provisional."
  5)

(def fresh
  {:elo initial-rating
   :glicko initial-rating
   :rd initial-rd
   :volatility initial-volatility
   :games 0
   :wins 0})

(defn player-record
  [ratings player]
  (get ratings player fresh))

;; ── Decomposing an N-player result into pairs ───────────────────────────────

(defn pairings
  "One finished game → {player [{:opponent :score :weight} ...]}.

   Score is 1 for the winner against everyone, 0 for a loser against the
   winner, and 0.5 between two losers. Weight is 1/(N-1) so every player's
   pairings sum to a single game's worth of evidence."
  [players winner]
  (let [players (vec (distinct players))
        n (count players)
        weight (/ 1.0 (double (max 1 (dec n))))]
    (into
     {}
     (for [player players]
       [player
        (vec
         (for [opponent players
               :when (not= opponent player)]
           {:opponent opponent
            :score (cond
                     (= player winner) 1.0
                     (= opponent winner) 0.0
                     :else 0.5)
            :weight weight}))]))))

(defn rateable?
  "A game only teaches us something if it has a declared winner who was
   actually at the table, and at least two players to compare."
  [players winner]
  (let [players (distinct players)]
    (boolean
     (and winner
          (> (count players) 1)
          (some #(= winner %) players)))))

;; ── ELO ─────────────────────────────────────────────────────────────────────

(defn expected
  "Probability that a player rated `rating` beats one rated `against`.
   A 400 point gap is a 10:1 favorite."
  [rating against]
  (/ 1.0 (+ 1.0 (Math/pow 10.0 (/ (- against rating) 400.0)))))

(defn k-factor
  "Newcomers move fast, regulars settle down."
  [games]
  (if (< games provisional-games) 32.0 16.0))

(defn elo-deltas
  "What one finished game does to everyone's Elo. Returns {player delta}.
   Sums to zero when the players share a K, which is the whole point of Elo:
   points only ever move between the people at the table."
  [ratings players winner]
  (let [pairs (pairings players winner)]
    (into
     {}
     (for [[player opponents] pairs]
       (let [{:keys [elo games]} (player-record ratings player)
             k (k-factor games)]
         [player
          (reduce
           (fn [delta {:keys [opponent score weight]}]
             (let [against (:elo (player-record ratings opponent))]
               (+ delta (* k weight (- score (expected elo against))))))
           0.0
           opponents)])))))

;; ── GLICKO-2 ────────────────────────────────────────────────────────────────
;;
;; Glickman's algorithm (glicko.net/glicko/glicko2.pdf) with two adaptations:
;; per-result weights for the multiplayer decomposition above, and batching
;; into rating periods. Setting every weight to 1 recovers the paper exactly,
;; which is what the test suite checks against the published worked example.

(def glicko-scale
  "Ratings live on the familiar 1500-ish scale; the algorithm works on an
   internal one. 173.7178 = 400 / ln(10)."
  173.7178)

(def tau
  "Constrains how much volatility can move between periods. Glickman suggests
   0.3–1.2; smaller is steadier. 0.5 suits a slow asynchronous game."
  0.5)

(def convergence 0.000001)

(defn- ->mu [rating] (/ (- rating initial-rating) glicko-scale))
(defn- ->phi [rd] (/ rd glicko-scale))
(defn- <-rating [mu] (+ initial-rating (* glicko-scale mu)))
(defn- <-rd [phi] (* glicko-scale phi))

(defn- g
  "How much an opponent's own uncertainty dilutes what we learn from them."
  [phi]
  (/ 1.0 (Math/sqrt (+ 1.0 (/ (* 3.0 phi phi) (* Math/PI Math/PI))))))

(defn- expect
  [mu opponent-mu opponent-phi]
  (/ 1.0 (+ 1.0 (Math/exp (- (* (g opponent-phi) (- mu opponent-mu)))))))

(defn- volatility-objective
  "f(x) from step 5 of the paper — the function whose root is the new
   log-volatility."
  [x delta-sq phi-sq v a]
  (let [ex (Math/exp x)
        numerator (* ex (- delta-sq phi-sq v ex))
        denominator (* 2.0 (Math/pow (+ phi-sq v ex) 2.0))]
    (- (/ numerator denominator)
       (/ (- x a) (* tau tau)))))

(defn- new-volatility
  "Illinois-variant regula falsi on f, exactly as the paper prescribes."
  [volatility delta-sq phi-sq v]
  (let [a (Math/log (* volatility volatility))
        f (fn [x] (volatility-objective x delta-sq phi-sq v a))
        ;; Bracket the root: above by a, below by either a closed form (when
        ;; the result was more surprising than current uncertainty explains)
        ;; or by stepping down in units of tau until f turns positive.
        b (if (> delta-sq (+ phi-sq v))
            (Math/log (- delta-sq phi-sq v))
            (loop [k 1]
              (if (< (f (- a (* k tau))) 0.0)
                (recur (inc k))
                (- a (* k tau)))))]
    (loop [a* a
           b* b
           fa (f a)
           fb (f b)
           steps 0]
      (if (or (< (Math/abs (- b* a*)) convergence) (> steps 100))
        (Math/exp (/ a* 2.0))
        (let [c (+ a* (/ (* (- a* b*) fa) (- fb fa)))
              fc (f c)]
          (if (<= (* fc fb) 0.0)
            (recur b* c fb fc (inc steps))
            (recur a* c (/ fa 2.0) fc (inc steps))))))))

(defn glicko-update
  "One rating period's worth of results for one player.

   `results` is a seq of {:rating :rd :score :weight} — the opponents faced,
   already flattened out of however many games. An empty seq means the player
   sat the period out, in which case only RD grows: we know strictly less
   about them than we did before."
  [{:keys [glicko rd volatility] :as record} results]
  (let [mu (->mu glicko)
        phi (->phi rd)]
    (if (empty? results)
      (assoc record :rd (min initial-rd (<-rd (Math/sqrt (+ (* phi phi)
                                                            (* volatility volatility))))))
      (let [terms (map
                   (fn [{:keys [rating rd score weight]}]
                     (let [opponent-mu (->mu rating)
                           opponent-phi (->phi rd)
                           gp (g opponent-phi)
                           e (expect mu opponent-mu opponent-phi)]
                       {:g gp :e e :score score :weight (or weight 1.0)}))
                   results)
            ;; v — estimated variance of the player's skill from these results
            v (/ 1.0 (reduce
                      (fn [total {:keys [g e weight]}]
                        (+ total (* weight g g e (- 1.0 e))))
                      0.0
                      terms))
            ;; the raw direction and size of the surprise
            surprise (reduce
                      (fn [total {:keys [g e score weight]}]
                        (+ total (* weight g (- score e))))
                      0.0
                      terms)
            delta (* v surprise)
            volatility' (new-volatility volatility (* delta delta) (* phi phi) v)
            phi-star (Math/sqrt (+ (* phi phi) (* volatility' volatility')))
            phi' (/ 1.0 (Math/sqrt (+ (/ 1.0 (* phi-star phi-star)) (/ 1.0 v))))
            mu' (+ mu (* phi' phi' surprise))]
        (assoc record
               :glicko (<-rating mu')
               :rd (min initial-rd (<-rd phi'))
               :volatility volatility')))))

;; ── Replay ──────────────────────────────────────────────────────────────────
;;
;; Ratings are recomputed from the full history rather than nudged in place.
;; It costs nothing at this scale, it survives a game being un-completed by an
;; undo, it can't double-count a completion that fires twice, and retuning K
;; or tau is a re-run rather than a migration.

(defn apply-elo
  "Fold one game into the Elo half of the ratings map."
  [ratings {:keys [players winner]}]
  (let [deltas (elo-deltas ratings players winner)]
    (reduce
     (fn [ratings [player delta]]
       (-> ratings
           (update player #(or % fresh))
           (update-in [player :elo] + delta)
           (update-in [player :games] inc)
           (update-in [player :wins] + (if (= player winner) 1 0))))
     ratings
     deltas)))

(defn period-results
  "Every pairing played in one rating period, grouped by player, resolved
   against the ratings as they stood when the period opened. Glicko-2 compares
   within a period, not sequentially through it."
  [opening games]
  (apply
   merge-with into
   {}
   (for [{:keys [players winner]} games]
     (into
      {}
      (for [[player opponents] (pairings players winner)]
        [player
         (mapv
          (fn [{:keys [opponent score weight]}]
            (let [{:keys [glicko rd]} (player-record opening opponent)]
              {:rating glicko :rd rd :score score :weight weight}))
          opponents)])))))

(defn apply-glicko
  "Close out a rating period: everyone who played gets a real update, everyone
   already on the board who sat it out just gets less certain."
  [ratings opening games]
  (let [results (period-results opening games)]
    (reduce
     (fn [ratings player]
       (update ratings player glicko-update (get results player [])))
     ratings
     (keys ratings))))

(defn replay
  "Fold a chronological seq of finished games into {player record}.

   Each game is {:players [...] :winner p :period k}, sorted oldest first and
   already tagged with a rating period (any comparable value — the caller
   decides how long a period is). Elo advances game by game inside a period;
   Glicko-2 settles up when the period closes."
  [games]
  (reduce
   (fn [ratings period-games]
     (let [;; Elo first, so newcomers exist before the period is closed out —
           ;; but Glicko compares against the ratings the period opened with.
           opening ratings
           ratings (reduce apply-elo ratings period-games)]
       (apply-glicko ratings opening period-games)))
   {}
   (partition-by :period games)))

;; ── Ranking ─────────────────────────────────────────────────────────────────

(defn conservative
  "What a player's record actually establishes: Glickman's r - 2·RD.

   Ranking a leaderboard by the raw Glicko number puts whoever just won their
   first two games on top, because their number is a guess wearing a ±300 error
   bar. Subtracting two deviations asks a better question — how good are they
   at least? — and a regular with a narrow deviation answers it far better than
   a newcomer with a spectacular one."
  [{:keys [glicko rd]}]
  (- (or glicko initial-rating) (* 2.0 (or rd initial-rd))))

(defn provisional?
  [{:keys [games]}]
  (< (or games 0) provisional-games))
