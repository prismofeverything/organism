(ns organism.game-ws-test
  "The two ways a game can lose its last watcher, which are not the same thing."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.game-ws :as gws]))

(defn- fresh []
  (atom {:games {"t" {:key "t" :state {:hand 1} :channels #{} :watchers {}}}}))

(deftest watch-records-who-is-on-the-other-end
  (let [games (fresh)]
    (gws/watch! games "t" :ch-a "ryan")
    (gws/watch! games "t" :ch-b nil)
    (is (= #{:ch-a :ch-b} (:channels (gws/game-record games "t"))))
    (is (= {:ch-a "ryan" :ch-b nil} (:watchers (gws/game-record games "t")))
        "an observer is a watcher with no name, not an absent one")))

(deftest remove-channel-drops-the-game-with-the-last-watcher
  (testing "right for a board nobody is looking at: there is nothing to run"
    (let [games (fresh)]
      (gws/watch! games "t" :ch-a "ryan")
      (gws/remove-channel! games "t" :ch-a)
      (is (nil? (gws/game-record games "t"))))))

(deftest unwatch-keeps-the-game-alive
  (testing "a game that runs on its own must outlive the tab watching it"
    (let [games (fresh)]
      (gws/watch! games "t" :ch-a "ryan")
      (gws/watch! games "t" :ch-b "sam")
      (gws/unwatch! games "t" :ch-a)
      (is (= #{:ch-b} (:channels (gws/game-record games "t"))))
      (is (= {:ch-b "sam"} (:watchers (gws/game-record games "t"))))
      (gws/unwatch! games "t" :ch-b)
      (is (some? (gws/game-record games "t"))
          "the last watcher leaving must not throw the hand away")
      (is (= {:hand 1} (:state (gws/game-record games "t")))
          "and the state is untouched")
      (gws/forget-game! games "t")
      (is (nil? (gws/game-record games "t"))))))

(deftest send-views-builds-a-message-per-watcher
  (testing "which is what lets one game hide things from its own players"
    (let [sent (atom [])
          watchers {:ch-a "ryan" :ch-b "sam" :ch-c nil}]
      (with-redefs [gws/send! (fn [ch msg] (swap! sent conj [ch msg]))]
        (gws/send-views! watchers (fn [player] {:for (or player "observer")})))
      (is (= 3 (count @sent)))
      (is (= #{{:for "ryan"} {:for "sam"} {:for "observer"}}
             (set (map second @sent)))))))
