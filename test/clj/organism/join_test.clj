(ns organism.join-test
  "Taking a seat in an open lobby: that the claim is actually written down,
   that the guards hold, and that a lobby starts itself the moment the last
   seat is filled by the person filling it."
  (:require
   [clojure.test :refer [deftest testing is use-fixtures]]
   [organism.board :as board]
   [organism.mongo :as db]
   [organism.persist :as persist]
   [organism.routes.websockets :as ws]))

(def test-connection
  {:host "localhost" :port 27017 :database "organism-join-test"})

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
      (reset! ws/games {:games {}})
      (binding [*db* connection] (run))
      (reset! ws/games {:games {}})
      (clear! connection))))

(defn- invocation-for
  [players]
  {:player-count (count players)
   :ring-count 4
   :organism-victory 3
   :players (vec players)
   :player-captures (vec (repeat (count players) board/default-player-captures))
   :description ""
   :mutations {}
   :game-type "organism"
   :colors (board/generate-colors (take 4 board/total-rings))})

(defn- open-lobby!
  "A lobby opened by alice, with whatever seats are given."
  [game-key players]
  (persist/create-open-game! *db* game-key (invocation-for players) "alice"))

(defn- open-players
  [game-key]
  (get-in (db/one *db* :open-games {:key game-key}) [:invocation :players]))

;; ── the reported bug ──────────────────────────────────────────────────────

(deftest a-claimed-seat-is-written-down
  (testing "joining persists to open-games rather than only the live registry"
    (open-lobby! "three" ["alice" "" ""])
    (let [result (ws/join-open-game! *db* "three" 1 "bob")]
      (is (nil? (:error result)))
      (is (false? (:begun? result))))
    (is (= ["alice" "bob" ""] (vec (open-players "three"))))

    (testing "and survives the registry being emptied, as on a restart"
      (reset! ws/games {:games {}})
      (is (= ["alice" "bob" ""] (vec (open-players "three"))))))

  (testing "a joiner does not become the owner of someone else's lobby"
    (is (= "alice" (:created-by (db/one *db* :open-games {:key "three"}))))))

(deftest joining-is-guarded
  (open-lobby! "three" ["alice" "" ""])
  (testing "a seat somebody is already in"
    (is (= "that seat is taken by alice"
           (:error (ws/join-open-game! *db* "three" 0 "bob")))))
  (testing "a player who is already at the table"
    (is (= "you are already in this game"
           (:error (ws/join-open-game! *db* "three" 1 "alice")))))
  (testing "a seat that does not exist"
    (is (some? (:error (ws/join-open-game! *db* "three" 9 "bob"))))
    (is (some? (:error (ws/join-open-game! *db* "three" nil "bob")))))
  (testing "a lobby that does not exist"
    (is (= "no such open game"
           (:error (ws/join-open-game! *db* "nowhere" 0 "bob")))))
  (testing "none of that changed the roster"
    (is (= ["alice" "" ""] (vec (open-players "three"))))))

;; ── starting on its own ───────────────────────────────────────────────────

(deftest the-last-seat-starts-the-game
  (testing "filling the final seat begins play without anyone pressing create"
    (open-lobby! "two" ["alice" ""])
    (let [result (ws/join-open-game! *db* "two" 1 "bob")]
      (is (nil? (:error result)))
      (is (true? (:begun? result))))
    (is (nil? (db/one *db* :open-games {:key "two"})) "lobby is gone")
    (let [record (db/one *db* :games {:key "two"})]
      (is (some? record) "and the game exists")
      (is (= "alice" (:created-by record))
          "credited to whoever opened it, not whoever filled the last seat"))
    (testing "with a row for every player"
      (doseq [player ["alice" "bob"]]
        (is (= "active" (:status (persist/find-player-game *db* "two" player)))))))

  (testing "the creator typing another player's name is still just editing"
    (open-lobby! "typed" ["alice" ""])
    (let [result (ws/set-slot! *db* "alice" "typed" 1 "carol")]
      (is (false? (:begun? result))))
    (is (some? (db/one *db* :open-games {:key "typed"})) "still an open lobby")
    (is (nil? (db/one *db* :games {:key "typed"})))
    (is (= ["alice" "carol"] (vec (open-players "typed")))))

  (testing "a partly typed name never trips the start"
    (open-lobby! "partial" ["alice" ""])
    (is (false? (:begun? (ws/set-slot! *db* "alice" "partial" 1 "bo"))))
    (is (some? (db/one *db* :open-games {:key "partial"})))))

;; ── the pieces underneath ─────────────────────────────────────────────────

(deftest full-invocation-means-every-seat-taken
  (is (true? (board/full-invocation? (invocation-for ["alice" "bob"]))))
  (is (false? (board/full-invocation? (invocation-for ["alice" ""]))))
  (testing "duplicates are not a full table"
    (is (false? (board/full-invocation? (invocation-for ["alice" "alice"])))))
  (testing "a roster longer than the seats sold is not full either"
    (is (false? (board/full-invocation?
                 (assoc (invocation-for ["alice" "bob"]) :player-count 3))))))

(deftest game-names-keep-their-punctuation
  (testing "the names people actually use stay legal"
    (doseq [name ["Woogachaka's Game" "let's see where this goes!"
                  "2p Testing!" "Poppolopin Jr." "ゲーム"]]
      (is (nil? (board/game-key-problem name)) name)))
  (testing "only what breaks a URL or a collection name is refused"
    (doseq [name ["a/b" "a?b" "a#b" "a%b" "a\\b" "" "  " " padded" "$where"]]
      (is (some? (board/game-key-problem name)) (pr-str name)))))

(deftest a-finished-game-leaves-the-registry
  (testing "the last channel out drops the game, so a reconnect rereads the db"
    (is (= {:games {}}
           (ws/disconnect-game "k" :channel {:games {"k" {:channels [:channel]}}})))
    (is (= {:games {"k" {:channels [:other]}}}
           (ws/disconnect-game "k" :channel
                               {:games {"k" {:channels [:channel :other]}}})))))
