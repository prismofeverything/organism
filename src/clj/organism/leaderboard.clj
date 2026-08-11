(ns organism.leaderboard
  "Turns finished games into the ratings shown on the players page.

   Ratings are a projection, not an account balance: every recompute replays
   the whole history from 1500 rather than nudging stored numbers. That costs
   about a second, and it buys immunity to the three ways an incremental
   version would drift — a completed game rewound by `walk-history`, a
   completion message arriving twice, and retuning K or tau after the fact.

   The math lives in `organism.rating`; this namespace only knows how to find
   the games and where to put the answers."
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [organism.bots :as bots]
   [organism.mongo :as db]
   [organism.persist :as persist]
   [organism.rating :as rating])
  (:import
   [java.time Instant ZoneOffset]
   [java.time.format DateTimeFormatter]
   [org.bson.types ObjectId]))

(def game-type "organism")

;; ── Which games count ───────────────────────────────────────────────────────

(defn all-human?
  "Rate only games played entirely by people.

   Bots are excluded rather than rated: a bot's strength is fixed, so it would
   make a fine anchor, but OBO has never won a recorded game and the auto
   generated showcase games would flood the board with five bot names. Bots are
   spotted the same three ways `observe-worthy?` spots them — the game's stored
   :bots set, the shared registry, and the \"generate-\" key prefix that older
   showcase games carry from before the :bots field existed."
  [game]
  (let [players (get-in game [:invocation :players])
        bot-set (set (:bots game))]
    (and (not (str/starts-with? (str (:key game)) "generate-"))
         (seq players)
         (every?
          (fn [player]
            (and player
                 (not (contains? bot-set player))
                 (not (bots/bot? game-type player))))
          players))))

(def ^:private month
  (.withZone (DateTimeFormatter/ofPattern "yyyy-MM") ZoneOffset/UTC))

(defn rating-period
  "Glicko-2 settles up once per period rather than once per game. A month is
   the right grain for a game where a single match can run for weeks: games
   played in the same month are genuinely concurrent, so they should all be
   judged against the ratings the month opened with.

   Takes epoch seconds — what an ObjectId timestamp actually is."
  [at]
  (.format month (Instant/ofEpochSecond (long at))))

(defn game-outcome
  "A finished game reduced to what rating cares about.

   The winner is recorded on the final history entry, and that entry's ObjectId
   carries the time it was written — the same trick `load-observe-games` uses to
   order the observe page. Games that were abandoned rather than won have no
   winner and drop out here."
  [db game]
  (let [final (db/find-last db (persist/history-key (:key game)) {})
        winner (:winner final)
        players (vec (get-in game [:invocation :players]))
        id (:_id final)]
    (when (and (rating/rateable? players winner)
               (instance? ObjectId id))
      {:key (:key game)
       :players players
       :winner winner
       :finished-at (.getTimestamp ^ObjectId id)})))

(defn decided-games
  "Every rateable organism game, oldest first, tagged with its rating period."
  [db]
  (let [;; Games predating :game-type are legacy organism — the same reasoning
        ;; `load-player-games` documents. Journey and eridu never wrote here.
        query {"$or" [{:game-type game-type}
                      {:game-type {"$exists" false}}]}]
    (->> (db/query db :games query)
         (filter all-human?)
         (keep (partial game-outcome db))
         (sort-by :finished-at)
         (mapv (fn [game] (assoc game :period (rating-period (:finished-at game))))))))

;; ── Storing and reading ─────────────────────────────────────────────────────
;;
;; Ratings live in their own collection rather than on the player document.
;; The player document is written by `update-player-preferences!`, which merges
;; whatever params the account form posted — putting a rating there would let a
;; player POST their way to the top of the board.

(defn store-ratings!
  [db ratings]
  (db/index! db :ratings [:key] {:unique true})
  (let [now (quot (System/currentTimeMillis) 1000)]
    (doseq [[player record] ratings]
      (db/merge! db :ratings {:key player} (assoc record :rated-at now)))
    ;; Ratings are a projection of the games, so a player whose last game was
    ;; deleted should stop having one.
    (db/delete! db :ratings {:key {"$nin" (vec (keys ratings))}})))

(defn load-ratings
  "{player {:elo :glicko :rd :volatility :games :wins}}"
  [db]
  (into
   {}
   (map (juxt :key #(dissoc % :_id :key)))
   (db/query db :ratings {})))

;; ── The recompute ───────────────────────────────────────────────────────────

(defn rate-all!
  [db]
  (let [games (decided-games db)
        ratings (rating/replay games)]
    (store-ratings! db ratings)
    (log/info "rated" (count ratings) "players over" (count games) "games")
    {:games (count games)
     :players (count ratings)}))

(defn rate-later!
  "Recompute off the request thread — finishing a game shouldn't make the
   winner wait on the whole ladder."
  [db]
  (future
    (try
      (rate-all! db)
      (catch Exception e
        (log/error e "rating recompute failed")))))

;; ── The players page ────────────────────────────────────────────────────────

(defn player-stats
  "Rows for the players page: the existing per-player counts plus ratings.

   Ordered by what each record establishes rather than by the raw number, and
   players with too few games to have established anything sort below the ones
   who have — visible, but not on top of the board for winning once."
  [db]
  (let [ratings (load-ratings db)]
    (->> (persist/load-player-stats db game-type)
         (map
          (fn [{:keys [key] :as row}]
            (if-let [record (get ratings key)]
              (assoc row
                     :elo (Math/round (double (:elo record)))
                     :glicko (Math/round (double (:glicko record)))
                     :rd (Math/round (double (:rd record)))
                     :rated (:games record)
                     :provisional (rating/provisional? record)
                     :rank (Math/round (rating/conservative record)))
              ;; Everyone else has games but none that finished with a winner.
              (assoc row :rated 0 :provisional true))))
         (sort-by
          (fn [{:keys [provisional rank]}]
            [(if provisional 1 0)
             (- (or rank -10000))])))))
