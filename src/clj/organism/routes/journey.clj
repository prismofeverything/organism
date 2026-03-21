(ns organism.routes.journey
  (:require
   [organism.layout :as layout]
   [organism.persist :as persist]
   [organism.middleware :as middleware]
   [ring.util.response :as response]))

(defn home-page
  [request]
  (layout/render
   request
   "journey/home.html"
   {:session-player (get-in request [:session :player])}))

(defn require-auth
  [handler]
  (fn [request]
    (if (get-in request [:session :player])
      (handler request)
      (response/redirect (str "/login?redirect=" (:uri request))))))

(defn create-page
  [db request]
  (let [player (get-in request [:session :player])
        preferences (persist/find-player-preferences db player)]
    (layout/render
     request
     "journey/create.html"
     {:session-player player
      :preferences preferences})))

(defn play-page
  [db request]
  (let [play-key (-> request :path-params :play)
        player-key (get-in request [:session :player])
        preferences (persist/find-player-preferences db player-key)]
    (layout/render
     request
     "journey/play.html"
     {:player player-key
      :play play-key
      :preferences preferences})))

(defn observe-page
  [db request]
  (let [player (get-in request [:session :player])
        games (persist/load-observe-games db)]
    (layout/render
     request
     "journey/observe.html"
     {:session-player player
      :observe-games (pr-str games)})))

(defn journey-routes
  [db]
  ["/journey"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get home-page}]
   ["/create" {:get (partial create-page db)
               :middleware [require-auth]}]
   ["/play/:play" {:get (partial play-page db)}]
   ["/play/:play/" {:get (partial play-page db)}]
   ["/observe" {:get (partial observe-page db)}]])
