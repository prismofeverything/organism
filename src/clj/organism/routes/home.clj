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
   [ring.util.response :as response]
   [ring.util.http-response :as http-response]))

(def home-game
  (atom {}))

(def all-rings
  ["A" "B" "C" "D" "E" "F" "G"])

(defn empty-game
  [starting-game]
  {:colors (board/generate-colors all-rings)
   :games (choice/random-walk starting-game)})

(defn home-page
  [db request]
  (let [players (persist/load-players db)]
    (layout/render
     request
     "home.html"
     {:players (pr-str players)})))

(defn player-page
  [db request]
  (let [player-key (-> request :path-params :player)
        preferences (persist/find-player-preferences db player-key)
        player-games (persist/load-player-games db player-key)]
    (layout/render
     request
     "player.html"
     {:player player-key
      :preferences preferences
      :player-games (pr-str player-games)})))

(defn eternal-page [request]
  (http-response/content-type
   (http-response/ok
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
  [db request]
  (let [params (select-keys (:path-params request) [:player :game])
        player-key (:player params)
        preferences (persist/find-player-preferences db player-key)]
    (layout/render
     request
     "game.html"
     (merge
      params
      {:preferences preferences}))))

(defn apply-player-preferences
  [db request]
  (let [player (-> request :path-params :player)]
    (println "applying player preferences: " player (:params request))
    (persist/update-player-preferences! db player (:params request))
    (response/response {:ok true :status :success})))

(defn home-routes
  [db]
  [""
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get (partial home-page db)}]
   ["/eternal" {:get eternal-page}]
   ["/game/:game" {:get (partial game-page db)}]
   ["/game/:game/" {:get (partial game-page db)}]
   ["/player/:player" {:get (partial player-page db)}]
   ["/player/:player/" {:get (partial player-page db)}]
   ["/player/:player/preferences" {:post (partial apply-player-preferences db)}]
   ["/player/:player/game/:game" {:get (partial game-page db)}]
   ["/player/:player/game/:game/" {:get (partial game-page db)}]])
