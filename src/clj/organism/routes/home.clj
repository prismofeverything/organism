(ns organism.routes.home
  (:require
   [clojure.pprint :as pprint]
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
               [:A :B :C :D]
               [["orb" [[:D 0] [:D 1] [:D 2]]]
                ["mass" [[:D 9] [:D 10] [:D 11]]]]
               false)]
    (-> game

        (game/introduce
         "orb"
         {:eat [:D 0]
          :grow [:D 2]
          :move [:D 1]})

        (game/move
         [:D 2]
         [:C 1])

        (game/introduce
         "mass"
         {:eat [:D 9]
          :grow [:D 10]
          :move [:D 11]})

        (game/grow
         {[:D 10] 1}
         [:C 6]
         :move)

        (game/grow
         {[:C 1] 1}
         [:B 1]
         :grow)

        (game/move
         [:D 10]
         [:C 7])

        (game/circulate
         [:D 0]
         [:C 1])

        (game/circulate
         [:D 1]
         [:B 1])

        (game/eat
         [:D 9]))))

(defn game-page [request]
  (content-type
   (ok
    (let [colors (board/generate-colors [:A :B :C :D :E :F :G])
          _ (println "colors" colors)
          board (board/build-board 6 50 2.1 colors ["orb" "mass"] true)]
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

