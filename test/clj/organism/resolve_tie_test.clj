(ns organism.resolve-tie-test
  "Repairing a game that played on past a tie, against a scratch database."
  (:require
   [clojure.test :refer [deftest testing is use-fixtures]]
   [organism.mongo :as db]
   [organism.persist :as persist]
   [organism.resolve-tie :as resolve-tie]
   [organism.scripts.resolve-tie :as script]))

(def test-connection
  {:host "localhost" :port 27017 :database "organism-resolve-tie-test"})

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
      (binding [*db* connection] (run))
      (clear! connection))))

(def roster ["alice" "bob"])

(defn- living-organisms
  "Elements making up `n` living organisms for `player` — three element types
   each, which is what alive-elements? asks for."
  [player n]
  (into
   {}
   (for [organism (range n)
         [index type] (map-indexed vector [:eat :move :grow])]
     [[player organism index]
      {:player player :organism organism :type type}])))

(defn- state-with
  [counts acting round advance]
  {:round round
   :elements (into {} (mapcat (fn [[player n]] (living-organisms player n)) counts))
   :captures {"alice" [] "bob" []}
   :food {}
   :player-turn {:player acting :organism-turns [] :introduction {} :advance advance}})

(defn- create-game!
  [game-key opening]
  (persist/create-game!
   *db*
   {:key game-key
    :created-by "alice"
    :game-type "organism"
    :invocation {:players roster
                 :player-count 2
                 :ring-count 5
                 :colors [["A" "#886644"] ["B" "#446688"]]
                 :game-type "organism"}
    :game {:state opening
           :adjacencies {}
           :players {"alice" {:capture-limit 5} "bob" {:capture-limit 5}}
           :turn-order roster
           :organism-victory 3
           :mutations {}
           :capture-limit 5}
    :chat []}))

(defn- overrun-game!
  "A game that reached a tie on alice's turn and then kept being played."
  [game-key]
  (create-game! game-key (state-with {"alice" 2 "bob" 2} "alice" 0 nil))
  (persist/update-state! *db* game-key
                         (state-with {"alice" 3 "bob" 3} "alice" 1 :check-integrity))
  (persist/update-state! *db* game-key
                         (state-with {"alice" 3 "bob" 3} "bob" 1 nil))
  (persist/update-state! *db* game-key
                         (state-with {"alice" 3 "bob" 3} "alice" 2 nil)))

(deftest examine-finds-where-the-game-actually-ended
  (testing "the tie is spotted at the entry it happened, not the latest one"
    (overrun-game! "overran")
    (let [report (resolve-tie/examine *db* "overran")]
      (is (= :overran (:status report)))
      (is (= 1 (:index report)))
      (is (= 4 (:history-count report)))
      (is (= 2 (:discard-count report)))
      (is (= "alice" (:acting report)))
      (is (= "bob" (:winner report)))
      (is (= {"alice" 3 "bob" 3} (:organisms (:standing report))))))

  (testing "a state still waiting on conflict resolution is not judged"
    (create-game! "conflicting" (state-with {"alice" 2 "bob" 2} "alice" 0 nil))
    (persist/update-state! *db* "conflicting"
                           (state-with {"alice" 3 "bob" 3} "alice" 1 :resolve-conflicts))
    (is (= :no-winner (:status (resolve-tie/examine *db* "conflicting")))))

  (testing "a game that ended on its last state needs no repair"
    (create-game! "clean" (state-with {"alice" 2 "bob" 2} "alice" 0 nil))
    (persist/update-state! *db* "clean"
                           (state-with {"alice" 3 "bob" 2} "alice" 1 :check-integrity))
    (let [report (resolve-tie/examine *db* "clean")]
      (is (= :already-ended (:status report)))
      (is (= "alice" (:winner report)))))

  (testing "a game still in progress is left alone"
    (create-game! "ongoing" (state-with {"alice" 2 "bob" 2} "alice" 0 nil))
    (is (= :no-winner (:status (resolve-tie/examine *db* "ongoing")))))

  (testing "an unknown key reports rather than throws"
    (is (= :not-found (:status (resolve-tie/examine *db* "no-such-game"))))))

(deftest resolve-ends-the-game-and-keeps-the-overrun
  (overrun-game! "overran")
  (let [report (resolve-tie/resolve-tie! *db* "overran")]
    (testing "it reports what it did"
      (is (= :resolved (:status report)))
      (is (= "bob" (:winner report))))

    (testing "the live history stops at the ending, carrying the winner"
      (is (= 2 (persist/game-history-count *db* "overran")))
      (is (= "bob" (:winner (persist/load-game-state *db* "overran")))))

    (testing "nothing is destroyed - the overrun is kept aside"
      (is (= 2 (db/number *db* (resolve-tie/voided-history-key "overran")))))

    (testing "the game and every player's row are marked complete"
      (doseq [player roster]
        (let [player-game (persist/find-player-game *db* "overran" player)]
          (is (= "complete" (:status player-game)))
          (is (= "bob" (:winner player-game))))))

    (testing "running it again is a no-op, not a second truncation"
      (let [again (resolve-tie/resolve-tie! *db* "overran")]
        (is (= :already-ended (:status again)))
        (is (= 2 (persist/game-history-count *db* "overran")))))))

(deftest resolve-writes-nothing-to-a-healthy-game
  (create-game! "ongoing" (state-with {"alice" 2 "bob" 2} "alice" 0 nil))
  (persist/update-state! *db* "ongoing" (state-with {"alice" 2 "bob" 2} "bob" 0 nil))
  (let [report (resolve-tie/resolve-tie! *db* "ongoing")]
    (is (= :no-winner (:status report)))
    (is (= 2 (persist/game-history-count *db* "ongoing")))
    (is (zero? (db/number *db* (resolve-tie/voided-history-key "ongoing"))))
    (is (= "active" (:status (persist/find-player-game *db* "ongoing" "alice"))))))

(deftest the-script-can-print-every-report-it-produces
  (testing "each status renders without blowing up on a missing key"
    (overrun-game! "overran")
    (let [describe #'script/describe]
      (doseq [report [(resolve-tie/examine *db* "overran")
                      (resolve-tie/examine *db* "no-such-game")
                      (resolve-tie/resolve-tie! *db* "overran")
                      (resolve-tie/examine *db* "overran")]]
        (is (nil? (describe report)))))))
