(ns organism.routes.home
  (:require
   [organism.board :as board]
   [organism.layout :as layout]
   [clojure.java.io :as io]
   [organism.middleware :as middleware]
   [ring.util.http-response :refer [content-type ok] :as response]
   [ring.util.response]
   [ring.util.http-response :as response]))

(defn home-page [request]
  (layout/render request "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))

(defn about-page [request]
  (layout/render request "about.html"))

(defn game-page [request]
  (content-type
   (ok
    (let [locations (board/board-locations 6 35 2.15 board/organism-colors)]
      (println "locations" locations)
      (board/render 6 35 2.15 (map last board/organism-colors))))
   "text/html; charset=utf-8"))

(defn home-routes []
  [""
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get home-page}]
   ["/about" {:get about-page}]
   ["/game" {:get game-page}]])

