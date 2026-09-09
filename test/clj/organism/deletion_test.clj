(ns organism.deletion-test
  "The delete workflow end to end against a scratch database: which games go
   immediately, which get marked, what calls a mark off, and what the reaper
   is willing to take. The rules being checked live in organism.persist."
  (:require
   [clojure.test :refer [deftest testing is use-fixtures]]
   [organism.mongo :as db]
   [organism.persist :as persist]
   [organism.reap :as reap]
   [organism.routes.shared :as shared]))

(def test-connection
  {:host "localhost" :port 27017 :database "organism-deletion-test"})

(def ^:dynamic *db* nil)

(defn- clear!
  [connection]
  (doseq [collection (remove #(.startsWith ^String % "system.")
                             (db/collections connection))]
    (db/delete! connection collection {})))

(use-fixtures
  :each
  (fn [run]
    (let [connection (db/connect! test-connection)]
      (clear! connection)
      (binding [*db* connection]
        (run))
      (clear! connection))))

(def players ["alice" "bob"])

(defn- fake-state
  [current round]
  {:elements {}
   :captures []
   :food {}
   :round round
   :player-turn {:player current :organism-turns [] :introduction {}}})

(defn- create-test-game!
  ([db game-key roster] (create-test-game! db game-key roster #{}))
  ([db game-key roster bots]
   (persist/create-game!
    db
    {:key game-key
     :created-by (first roster)
     :game-type "organism"
     :bots bots
     :invocation {:players (vec roster)
                  :player-count (count roster)
                  :ring-count 5
                  :colors (mapv (fn [ring] [ring "#886644"]) ["A" "B" "C" "D" "E"])
                  :game-type "organism"}
     :game {:state (fake-state (first roster) 0)
            :adjacencies {}
            :players {}}
     :chat []})))

(defn- delete-request
  [player game-key]
  (shared/delete-game!
   {:game-type "organism"}
   *db*
   {:session {:player player} :path-params {:play game-key}}))

(deftest delete-game-removes-every-trace
  (testing "a game is spread over five places and all of them get cleaned up"
    (create-test-game! *db* "doomed" players)
    (persist/update-chat! *db* "doomed" {:type "chat" :player "alice" :message "hi"})
    (persist/update-state! *db* "doomed" (fake-state "bob" 1))
    (is (some? (db/one *db* :games {:key "doomed"})))
    (is (= 2 (persist/game-history-count *db* "doomed")))
    (is (every? #(some? (persist/find-player-game *db* "doomed" %)) players))

    (persist/delete-game! *db* "doomed")

    (is (nil? (db/one *db* :games {:key "doomed"})))
    (is (nil? (db/one *db* :open-games {:key "doomed"})))
    (is (zero? (persist/game-history-count *db* "doomed")))
    (is (zero? (db/number *db* (persist/chat-key "doomed"))))
    (is (every? #(nil? (persist/find-player-game *db* "doomed" %)) players))))

(deftest a-mark-reaches-everyone-and-can-be-called-off
  (create-test-game! *db* "marked" players)
  (testing "the mark lands on the game and mirrors onto every player's row"
    (persist/mark-game-for-deletion! *db* "marked" "alice")
    (is (= "alice" (get-in (db/one *db* :games {:key "marked"}) [:deletion :marked-by])))
    (is (every? #(some? (:deletion (persist/find-player-game *db* "marked" %))) players)))
  (testing "an objection clears it everywhere"
    (persist/unmark-game-for-deletion! *db* "marked")
    (is (nil? (:deletion (db/one *db* :games {:key "marked"}))))
    (is (every? #(nil? (:deletion (persist/find-player-game *db* "marked" %))) players))))

(deftest playing-is-the-veto
  (testing "taking a turn cancels a pending deletion without anyone saying so"
    (create-test-game! *db* "playing" players)
    (persist/mark-game-for-deletion! *db* "playing" "alice")
    (is (some? (:deletion (persist/find-player-game *db* "playing" "bob"))))
    (persist/update-player-games! *db* "playing" players (fake-state "bob" 1))
    (is (every? #(nil? (:deletion (persist/find-player-game *db* "playing" %))) players))))

(deftest delete-chooses-between-removing-and-marking
  (testing "a game nobody has played yet has nothing at stake, so it goes now"
    (create-test-game! *db* "fresh" players)
    (let [response (delete-request "alice" "fresh")]
      (is (= "fresh" (get-in response [:body :deleted])))
      (is (nil? (db/one *db* :games {:key "fresh"})))))

  (testing "a game in progress is marked instead, and survives the request"
    (create-test-game! *db* "underway" players)
    (persist/update-state! *db* "underway" (fake-state "bob" 1))
    (let [response (delete-request "alice" "underway")
          record (db/one *db* :games {:key "underway"})]
      (is (= "underway" (get-in response [:body :marked])))
      (is (some? record))
      (is (= "alice" (get-in record [:deletion :marked-by])))))

  (testing "with no other human on the roster there is nobody to object"
    (create-test-game! *db* "solo" ["alice" "oroboros"] #{"oroboros"})
    (persist/update-state! *db* "solo" (fake-state "oroboros" 1))
    (let [response (delete-request "alice" "solo")]
      (is (= "solo" (get-in response [:body :deleted])))
      (is (nil? (db/one *db* :games {:key "solo"})))))

  (testing "somebody with no stake in the game is refused"
    (create-test-game! *db* "theirs" players)
    (persist/update-state! *db* "theirs" (fake-state "bob" 1))
    (let [response (delete-request "mallory" "theirs")]
      (is (= 400 (:status response)))
      (is (some? (db/one *db* :games {:key "theirs"})))))

  (testing "a key with no game behind it is a 404, not a crash"
    (is (= 404 (:status (delete-request "alice" "no-such-game"))))))

(deftest reaper-takes-only-the-silent
  (create-test-game! *db* "silent" players)
  (create-test-game! *db* "revived" players)
  ;; Time is pinned to the games' own history timestamps rather than the wall
  ;; clock: "silent" is marked AFTER its last activity, "revived" BEFORE — which
  ;; is exactly the difference the reaper is supposed to notice. An all-bot game
  ;; moves this way for real, writing history without touching player-games.
  (let [silent-at (persist/last-activity-at *db* "silent")
        revived-at (persist/last-activity-at *db* "revived")
        now (+ (max silent-at revived-at) 1000)]
    (db/merge! *db* :games {:key "silent"}
               {:deletion {:marked-by "alice"
                           :marked-at (+ silent-at 10)
                           :deadline (+ silent-at 20)}})
    (db/merge! *db* :games {:key "revived"}
               {:deletion {:marked-by "alice"
                           :marked-at (- revived-at 10)
                           :deadline (+ revived-at 20)}})

    (testing "a dry run reports the same list without touching anything"
      (let [result (reap/sweep! *db* {:dry-run? true :now now})]
        (is (true? (:dry-run? result)))
        (is (= ["silent"] (:deleted result)))
        (is (some? (db/one *db* :games {:key "silent"})))))

    (testing "the sweep deletes the silent game and clears the stale mark"
      (let [result (reap/sweep! *db* {:now now})]
        (is (= ["silent"] (:deleted result)))
        (is (= ["revived"] (:kept result))))
      (is (nil? (db/one *db* :games {:key "silent"})))
      (is (every? #(nil? (persist/find-player-game *db* "silent" %)) players))
      (is (some? (db/one *db* :games {:key "revived"})))
      (is (nil? (:deletion (db/one *db* :games {:key "revived"})))))

    (testing "an unmarked game is never a candidate"
      (is (zero? (:deleted-count (reap/sweep! *db* {:now now})))))))
