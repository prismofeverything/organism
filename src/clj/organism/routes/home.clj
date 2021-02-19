(ns organism.routes.home
  (:require
   [organism.board :as board]
   [organism.layout :as layout]
   [organism.game :as game]
   [clojure.java.io :as io]
   [organism.middleware :as middleware]
   [ring.util.http-response :refer [content-type ok] :as response]
   [ring.util.response]
   [ring.util.http-response :as response]))

(defn home-page [request]
  (layout/render request "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))

(defn about-page [request]
  (layout/render request "about.html"))

(def test-game
  (let [game (game/create-game
               6
               [:yellow :red :blue :orange]
               [["orb" [[:orange 0] [:orange 1] [:orange 2]]]
                ["mass" [[:orange 9] [:orange 10] [:orange 11]]]]
               false)]
    (-> game

        (game/introduce
         "orb"
         {:eat [:orange 0]
          :grow [:orange 2]
          :move [:orange 1]})

        (game/move
         "orb"
         [:orange 2]
         [:blue 1])

        (game/introduce
         "mass"
         {:eat [:orange 9]
          :grow [:orange 10]
          :move [:orange 11]})

        (game/grow
         "mass"
         {[:orange 10] 1}
         [:blue 6]
         :move)

        (game/grow
         "orb"
         {[:blue 1] 1}
         [:red 1]
         :grow)

        (game/move
         "mass"
         [:orange 10]
         [:blue 7])

        (game/circulate
         "orb"
         [:orange 0]
         [:blue 1])

        (game/circulate
         "orb"
         [:orange 1]
         [:red 1])

        (game/eat
         "mass"
         [:orange 9]))))

(defn game-page [request]
  (content-type
   (ok
    (let [board (board/build-board 6 50 2.1 (take 4 board/organism-colors) ["orb" "mass"])]
      (println "board" board)
      (board/render-game board test-game)))
   "text/html; charset=utf-8"))

(defn home-routes []
  [""
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get home-page}]
   ["/about" {:get about-page}]
   ["/game" {:get game-page}]])

