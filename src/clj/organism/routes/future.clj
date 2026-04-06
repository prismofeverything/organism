(ns organism.routes.future
  "HTTP routes for the Future game."
  (:require
   [organism.layout :as layout]
   [organism.persist :as persist]
   [organism.middleware :as middleware]
   [ring.util.response :as response]))

(defn require-auth
  [handler]
  (fn [request]
    (if (get-in request [:session :player])
      (handler request)
      (response/redirect (str "/login?redirect=" (:uri request))))))

(defn home-page
  [request]
  (layout/render
   request
   "future/home.html"
   {:session-player (get-in request [:session :player])}))

(defn play-list-page
  [db request]
  (let [player (get-in request [:session :player])
        player-games (persist/load-player-games db player "future")]
    (layout/render
     request
     "future/games.html"
     {:session-player player
      :player-games (pr-str player-games)})))

(defn play-page
  [request]
  (let [play-key (-> request :path-params :play)
        player-key (get-in request [:session :player] "--observer--")]
    (layout/render
     request
     "future/play.html"
     {:player player-key
      :play play-key})))

(defn create-page
  [request]
  (layout/render
   request
   "future/create.html"
   {:session-player (get-in request [:session :player])}))

(defn observe-page
  [db request]
  (let [player (get-in request [:session :player])
        games (persist/load-observe-games db)]
    (layout/render
     request
     "future/observe.html"
     {:session-player player
      :observe-games (pr-str games)})))

(defn player-page
  [db request]
  (let [player-key (-> request :path-params :player)
        preferences (persist/find-player-preferences db player-key)
        player-games (persist/load-player-games db player-key "future")]
    (layout/render
     request
     "future/player.html"
     {:player player-key
      :preferences preferences
      :player-games (pr-str player-games)})))

(defn rules-page
  [request]
  (layout/render
   request
   "future/rules.html"
   {:session-player (get-in request [:session :player])}))

(defn generate-page
  [request]
  (layout/render
   request
   "future/generate.html"
   {:session-player (get-in request [:session :player])}))

(defn future-routes
  [db]
  ["/future"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get home-page}]
   ["/create" {:get create-page
               :middleware [require-auth]}]
   ["/rules" {:get rules-page}]
   ["/generate" {:get generate-page}]
   ["/play" {:get (partial play-list-page db)
             :middleware [require-auth]}]
   ["/play/:play" {:get play-page}]
   ["/play/:play/" {:get play-page}]
   ["/observe" {:get (partial observe-page db)}]
   ["/player/:player" {:get (partial player-page db)}]
   ["/player/:player/" {:get (partial player-page db)}]])
