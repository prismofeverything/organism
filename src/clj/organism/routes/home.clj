(ns organism.routes.home
  (:require
   [organism.board :as board]
   [organism.layout :as layout]
   [organism.game :as game]
   [organism.choice :as choice]
   [organism.layout :as layout]
   [organism.examples :as examples]
   [hiccup.core :as up]
   [clojure.java.io :as io]
   [organism.middleware :as middleware]
   [ring.util.response]
   [ring.util.http-response :as response]))

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
  (response/content-type
   (response/ok
    (do
      (if (empty? (deref home-game))
        (reset! home-game (empty-game (examples/six-player-game))))
      (let [home (deref home-game)
            {:keys [games colors]} home
            game (first games)
            board (board/build-board 6 50 2.1 colors (:turn-order game) true)]
        (swap! home-game update :games rest)
        (up/html (board/render-game board game)))))
   "text/html; charset=utf-8"))

(defn home-page [request]
  (layout/render request "home.html"))

(defn home-routes []
  [""
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get home-page}]
   ["/eternal" {:get game-page}]
   ["/docs" {:get (fn [_]
                    (-> (response/ok (-> "docs/docs.md" io/resource slurp))
                        (response/header "Content-Type" "text/plain; charset=utf-8")))}]])

