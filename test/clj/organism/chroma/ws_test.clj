(ns organism.chroma.ws-test
  "Drive the server-side Chroma phase machine directly (no socket / no Mongo) to
   prove turn resolution, bot play, the place/swap phases, and chit conservation."
  (:require [clojure.test :refer [deftest is testing]]
            [organism.chroma.engine :as e]
            [organism.persist-chroma :as persist-c]
            [organism.routes.chroma-ws :as ws]))

(def new-server-game @#'ws/new-server-game)
(def maybe-resolve @#'ws/maybe-resolve)
(def human-seats @#'ws/human-seats)
(def seat-of-player @#'ws/seat-of-player)
(def connect! @#'ws/connect!)
(def save! @#'ws/save!)

(def ^:const TOTAL (* e/PER_COLOR 6))

(defn- ledger [G]
  (+ (reduce + (vals (:bag G)))
     (reduce + (map #(count (:hand %)) (:players G)))
     (->> (vals (:stacks G)) (apply concat) (remove #{"K"}) count)
     (reduce + (map #(:discarded % 0) (:players G)))))

(deftest all-bot-game-runs-to-completion
  (testing "a 3-bot game resolves every turn server-side, conserves chits, and ends"
    (let [server (new-server-game "t-allbot"
                                  {:players ["B0" "B1" "B2"] :bots [0 1 2]
                                   :palette "CMY" :depth 3 :trim true :seed 12345})
          final (maybe-resolve server)
          G (:state final)]
      (is (empty? (human-seats final)) "no human seats")
      (is (:over G) "game reached game-over")
      (is (= "over" (name (:phase final))))
      (is (= TOTAL (ledger G)) "chits conserved end-to-end")
      (is (every? #(<= (count (:hand %)) e/START_HAND) (:players G))))))

(deftest human-game-waits-for-human
  (testing "a 1-human + 2-bot game stops at :place waiting for the human"
    (let [server (new-server-game "t-human"
                                  {:players ["H" "Bot 2" "Bot 3"] :bots [1 2]
                                   :palette "CMY" :depth 3 :trim true :seed 999})
          server (maybe-resolve server)]
      (is (= [0] (vec (human-seats server))) "seat 0 is the only human")
      (is (= :place (:phase server)) "phase is :place")
      (is (= 0 (:turn (:state server))) "turn has not advanced without the human")
      (is (= 0 (seat-of-player server "H")))
      (is (= TOTAL (ledger (:state server)))))))

(deftest human-placements-drive-turns
  (testing "feeding legal human placements advances turns and conserves chits"
    (let [seed 4242]
      (loop [server (maybe-resolve
                     (new-server-game "t-drive"
                                      {:players ["H" "Bot 2" "Bot 3"] :bots [1 2]
                                       :palette "CMY" :depth 3 :trim true :seed seed}))
             steps 0]
        (let [G (:state server)]
          (is (= TOTAL (ledger G)) (str "ledger holds at turn " (:turn G)))
          (cond
            (:over G) (is true "completed")
            (>= steps 120) (is true "ran many turns without error")
            :else
            (recur
             (e/with-config G
               (case (:phase server)
                 :place
                 (let [moves (e/enumerate-moves G 0)
                       entry (if (seq moves)
                               (let [m (first moves)] {:c (:c m) :chit (:chit m) :k (:k m)})
                               :pass)]
                   (maybe-resolve (assoc-in server [:pending-placements 0] entry)))
                 :swap
                 (maybe-resolve (assoc-in server [:pending-swaps 0] :skip))
                 server))
             (inc steps))))))))

;; ── "Loading…" hang regression: persistence failures must degrade gracefully ──
;;
;; The client shows a perpetual "Loading…" only when the socket is open but the
;; server sent neither "game-state" nor "no-game". On a fresh connect, connect!
;; -> find-or-load! -> persist-c/load-game touches Mongo; if that throw escaped,
;; no frame was ever sent and Play hung forever. These tests pin the fix: a DB
;; read failure falls back to a fresh game (=> "no-game" => create UI), and a DB
;; write failure leaves the live game running in memory.

(deftest fresh-connect-sends-no-game-when-db-read-fails
  (testing "a brand-new player whose load-game throws still gets {:type no-game}, not a Loading hang"
    (reset! ws/games {:games {}})
    (let [sent (atom [])]
      (with-redefs [persist-c/load-game (fn [_ _] (throw (RuntimeException. "mongo down")))
                    ws/send! (fn [_ch msg] (swap! sent conj msg))]
        (connect! {:db :fake-db :play-key "down-key" :player "p1"} :chan-1))
      (is (= 1 (count @sent)) "exactly one frame is sent")
      (is (= "no-game" (:type (first @sent)))
          "create UI is surfaced instead of hanging on Loading…"))))

(deftest fresh-connect-sends-no-game-when-no-saved-game
  (testing "the healthy fresh path (load-game returns nil) also sends no-game"
    (reset! ws/games {:games {}})
    (let [sent (atom [])]
      (with-redefs [persist-c/load-game (fn [_ _] nil)
                    ws/send! (fn [_ch msg] (swap! sent conj msg))]
        (connect! {:db :fake-db :play-key "new-key" :player "p1"} :chan-1))
      (is (= "no-game" (:type (first @sent)))))))

(deftest connect-to-live-game-still-sends-game-state
  (testing "an in-memory game is still served as game-state (no regression)"
    (reset! ws/games {:games {}})
    (let [server (maybe-resolve
                  (new-server-game "live-key"
                                   {:players ["H" "Bot 2" "Bot 3"] :bots [1 2] :seed 31}))
          sent (atom [])]
      (swap! ws/games assoc-in [:games "live-key"] server)
      (with-redefs [ws/send! (fn [_ch msg] (swap! sent conj msg))]
        (connect! {:db :fake-db :play-key "live-key" :player "H"} :chan-1))
      (is (= "game-state" (:type (first @sent)))))))

(deftest save-failure-keeps-game-in-memory
  (testing "save! swallows a Mongo write error so live play is never interrupted"
    (reset! ws/games {:games {}})
    (let [server (new-server-game "save-key"
                                  {:players ["H" "Bot 2"] :bots [1] :seed 7})]
      (swap! ws/games assoc-in [:games "save-key"] server)
      (with-redefs [persist-c/save-game! (fn [& _] (throw (RuntimeException. "mongo down")))]
        (is (nil? (save! :fake-db "save-key")) "save! returns without throwing")))))
