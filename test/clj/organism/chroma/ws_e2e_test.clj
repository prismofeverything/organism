(ns organism.chroma.ws-e2e-test
  "Full-path websocket e2e WITHOUT a TCP socket: drives the real connect!/
   notify-clients! handlers (transit decode → dispatch → broadcast → Mongo) with a
   captured channel, against live MongoDB. Proves: create over the wire, a human
   placement, server broadcast of the rendered view, and DB persistence."
  (:require [clojure.test :refer [deftest is testing]]
            [organism.mongo :as db]
            [organism.routes.chroma-ws :as ws]
            [organism.persist-chroma :as pc]))

(defn- mongo []
  (try (db/connect! {:host "localhost" :port 27017 :database "organism-test"})
       (catch Exception _ nil)))

(deftest websocket-create-place-broadcast-persist
  (when-let [db (mongo)]
    (let [key "e2e-guest"
          player "e2e-guest"
          ch (Object.)                          ; opaque fake channel
          cfg {:db db :player player :play-key key}
          sent (atom [])]
      ;; clean slate
      (swap! ws/games update :games dissoc key)
      (pc/delete-game! db key)
      (with-redefs [ws/send! (fn [_ch msg] (swap! sent conj msg))]
        (testing "connect with no game yields a no-game message"
          (ws/connect! cfg ch)
          (is (= "no-game" (:type (last @sent)))))

        (testing "create over the wire broadcasts a place-phase game-state"
          (reset! sent [])
          (ws/notify-clients! cfg ch
            (ws/write-json {:type "create"
                            :players [player "Bot 2" "Bot 3"] :bots [1 2]
                            :palette "CMY" :depth 3 :trim true}))
          (let [gs (last (filter #(= "game-state" (:type %)) @sent))]
            (is (some? gs) "a game-state was broadcast")
            (is (= "place" (:phase gs)))
            (is (= 0 (:turn gs)))
            (is (= 82 (count (remove :removed (:board gs)))) "82 playable cells (trimmed board)")
            (is (some? (:you gs)) "the human gets a 'you' view")
            (is (seq (get-in gs [:you :hand])) "human has a hand")
            (is (seq (get-in gs [:you :moves])) "human has legal moves")
            ;; enriched view fields the polished UI renders
            (is (= 7 (count (:reference gs))) "reference table has blank + 6 color rows")
            (is (every? #(= 6 (count (:cells %))) (:reference gs)) "6 columns per reference row")
            (is (map? (:regions gs)) "largest-regions map present")
            (is (number? (:maxDry gs)) "max-dry stat present")
            (is (string? (get-in gs [:palette :chip "C"])) "palette chip colors present")))

        (testing "a human placement advances the turn machine and re-broadcasts"
          (let [gs (last (filter #(= "game-state" (:type %)) @sent))
                move (first (get-in gs [:you :moves]))
                turn-before (:turn gs)]
            (reset! sent [])
            (ws/notify-clients! cfg ch
              (ws/write-json {:type "place" :c (:c move) :chit (:chit move)}))
            (let [gs2 (last (filter #(= "game-state" (:type %)) @sent))]
              (is (some? gs2) "re-broadcast after placement")
              ;; either we advanced to the swap phase, or (if auto-skipped) the turn ticked
              (is (or (= "swap" (:phase gs2)) (> (:turn gs2) turn-before))
                  "placement moved the game forward"))))

        (testing "the game was persisted to MongoDB"
          (let [loaded (pc/load-game db key)]
            (is (some? loaded) "game is in Mongo")
            (is (= #{1 2} (set (:bots loaded))) "bots persisted")
            (is (= player (first (:players loaded))) "human seat persisted"))))
      ;; cleanup
      (swap! ws/games update :games dissoc key)
      (pc/delete-game! db key))))
