(ns organism.routes.home
  (:require
   [organism.board :as board]
   [organism.layout :as layout]
   [organism.game :as game]
   [organism.choice :as choice]
   [organism.persist :as persist]
   [organism.layout :as layout]
   [organism.examples :as examples]
   [hiccup.core :as up]
   [clojure.java.io :as io]
   [organism.middleware :as middleware]
   [ring.util.response]
   [ring.util.http-response :as response]))

(def home-game
  (atom {}))

(def all-rings
  ["A" "B" "C" "D" "E" "F" "G"])

(defn empty-game
  [starting-game]
  {:colors (board/generate-colors all-rings)
   :games (choice/random-walk starting-game)})

(defn home-page
  [request]
  (layout/render request "home.html"))

(defn player-page
  [db request]
  (let [player (-> request :path-params :player)
        player-games (persist/load-player-games db player)]
    (layout/render
     request
     "player.html"
     (merge
      {:player player
       :player-games (pr-str player-games)}))))

(defn eternal-page [request]
  (response/content-type
   (response/ok
    (do
      (if (empty? (deref home-game))
        (reset! home-game (empty-game (examples/six-player-game))))
      (let [home (deref home-game)
            {:keys [games colors]} home
            game (first games)
            board (board/build-board 6 50 2.1 colors all-rings (:turn-order game) true)]
        (swap! home-game update :games rest)
        (up/html (board/render-game board game)))))
   "text/html; charset=utf-8"))

(defn game-page
  [request]
  (layout/render
   request
   "game.html"
   (select-keys
    (:path-params request)
    [:player :game])))

(defn home-routes
  [db]
  [""
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get home-page}]
   ["/eternal" {:get eternal-page}]
   ["/player/:player" {:get (partial player-page db)}]
   ["/player/:player/game/:game" {:get game-page}]])
