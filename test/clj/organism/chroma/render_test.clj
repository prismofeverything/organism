(ns organism.chroma.render-test
  "Renders the new Chroma selmer templates with sample data so template syntax,
   the {% for %}/{% if %} blocks, and the number-format filters are exercised
   without booting the whole app (the uberjar can't run in this sandbox)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [selmer.parser :as parser]))

(defn- render [template params]
  (parser/set-resource-path! (io/resource "html"))
  (parser/render-file template params))

(deftest leaderboard-template-renders
  (testing "leaderboard.html renders with rows + hall of fame"
    (let [html (render "chroma/leaderboard.html"
                       {:session-player "Ada"
                        :aggregate [{:player "Ada" :bot false :games 3 :wins 2
                                     :win-rate 66.6 :avg-points 12.5 :best 18}
                                    {:player "Bot 2" :bot true :games 3 :wins 1
                                     :win-rate 33.3 :avg-points 9.0 :best 14}]
                        :hall-of-fame [{:player "Ada" :bot false :points 18 :win true
                                        :game-key-short "ada-12345"}]})]
      (is (re-find #"CHROMA" html))
      (is (re-find #"(?i)all-time leaders" html))
      (is (re-find #"Ada" html) "player row rendered")
      (is (re-find #"67%" html) "win-rate number-format applied (66.6 -> 67%)")
      (is (re-find #"12.5" html) "avg-points number-format applied")
      (is (re-find #"(?i)hall of fame" html))))
  (testing "leaderboard.html renders the empty state without error"
    ;; the handler passes nil (not []) when there are no games
    (let [html (render "chroma/leaderboard.html" {:aggregate nil :hall-of-fame nil})]
      (is (re-find #"No finished games" html)))))

(deftest home-template-renders
  (testing "home.html shows the explore section with a leaderboard link (logged in)"
    (let [html (render "chroma/home.html" {:session-player "Ada"})]
      (is (re-find #"/chroma/leaderboard" html) "leaderboard link present")
      (is (re-find #"explore" html) "explore section present")
      (is (re-find #"/chroma/play" html))))
  (testing "home.html logged-out also has the leaderboard link"
    (let [html (render "chroma/home.html" {})]
      (is (re-find #"/chroma/leaderboard" html)))))
