(ns organism.routes.universal
  "HTTP routes for universal game player."
  (:require
   [organism.layout :as layout]
   [organism.persist :as persist]
   [organism.middleware :as middleware]
   [organism.universal.ruleset :as rs]
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
   "universal/home.html"
   {:session-player (get-in request [:session :player])}))

(defn play-list-page
  "Show the logged-in player's games for universal."
  [db request]
  (let [player (get-in request [:session :player])
        player-games (persist/load-player-games db player "universal")]
    (layout/render
     request
     "universal/games.html"
     {:session-player player
      :player-games (pr-str player-games)})))

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
   {:session-player (get-in request [:session :player])
    :presets {:organism-like (pr-str rs/organism-like)
              :heterarchy-minimal (pr-str rs/heterarchy-minimal)}}))

(defn observe-page
  [db request]
  (let [player (get-in request [:session :player])
        games (persist/load-observe-games db)]
    (layout/render
     request
     "universal/observe.html"
     {:session-player player
      :observe-games (pr-str games)})))

(defn player-page
  [db request]
  (let [player-key (-> request :path-params :player)
        preferences (persist/find-player-preferences db player-key)
        player-games (persist/load-player-games db player-key "universal")]
    (layout/render
     request
     "universal/player.html"
     {:player player-key
      :preferences preferences
      :player-games (pr-str player-games)})))

(defn universal-routes
  [db]
  ["/universal"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get home-page}]
   ["/create" {:get create-page
               :middleware [require-auth]}]
   ["/play" {:get (partial play-list-page db)
             :middleware [require-auth]}]
   ["/play/:play" {:get play-page}]
   ["/play/:play/" {:get play-page}]
   ["/observe" {:get (partial observe-page db)}]
   ["/player/:player" {:get (partial player-page db)}]
   ["/player/:player/" {:get (partial player-page db)}]])
