(ns organism.routes.home
  (:require
   [clojure.pprint :refer (pprint)]
   [organism.board :as board]
   [organism.layout :as layout]
   [organism.game :as game]
   [organism.choice :as choice]
   [clojure.java.io :as io]
   [organism.middleware :as middleware]
   [ring.util.http-response :refer [content-type ok] :as response]
   [ring.util.response]
   [ring.util.http-response :as response]))

(defn home-page [request]
  (layout/render request "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))

(defn about-page [request]
  (layout/render request "about.html"))

(defn test-game
  []
  (game/create-game
   6
   [:A :B :C :D :E :F :G]
   [["orb" [[:G 1] [:G 2] [:G 3]]]
    ["mass" [[:G 7] [:G 8] [:G 9]]]
    ["brone" [[:G 13] [:G 14] [:G 15]]]
    ["laam" [[:G 19] [:G 20] [:G 21]]]
    ["stuk" [[:G 25] [:G 26] [:G 27]]]
    ["faast" [[:G 31] [:G 32] [:G 33]]]]
   true))

(def home-game
  (atom {}))

(defn empty-game
  [starting-game]
  {:colors
   (board/generate-colors
    [:A :B :C :D :E :F :G])
   :games
   (choice/random-walk
    starting-game)})

(defn game-page [request]
  (content-type
   (ok
    (do
      (if (empty? (deref home-game))
        (reset! home-game (empty-game (test-game))))
      (let [home (deref home-game)
            {:keys [games colors]} home
            game (first games)
            _ (println "EMPTY GAME")
            _ (println (test-game))
            _ (println "GAME STATE")
            _ (pprint game)
            _ (println "COLOR" colors)
            board (board/build-board 6 50 2.1 colors (:turn-order game) true)]
        (swap! home-game update :games rest)
        (board/render-game board game))))
   "text/html; charset=utf-8"))

(defn home-routes []
  [""
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get home-page}]
   ["/about" {:get about-page}]
   ["/game" {:get game-page}]])

