(ns organism.routes.universal
  "HTTP routes for universal game player."
  (:require
   [organism.layout :as layout]
   [organism.middleware :as middleware]
   [organism.universal.ruleset :as rs]))

(defn play-page
  [request]
  (let [play-key (-> request :path-params :play)
        player-key (get-in request [:session :player] "--observer--")]
    (layout/render
     request
     "universal/play.html"
     {:player player-key
      :play play-key})))

(defn create-page
  [request]
  (layout/render
   request
   "universal/create.html"
   {:presets {:organism-like (pr-str rs/organism-like)
              :heterarchy-minimal (pr-str rs/heterarchy-minimal)}}))

(defn universal-routes
  [db]
  ["/universal"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get create-page}]
   ["/play/:play" {:get play-page}]
   ["/play/:play/" {:get play-page}]])
