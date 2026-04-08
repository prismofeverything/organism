(ns organism.routes.organism
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

(defn play-list-page
  "Show the logged-in player's games — reuses the player page."
  [db request]
  (let [player (get-in request [:session :player])
        preferences (persist/find-player-preferences db player)
        player-games (persist/load-player-games db player "organism")]
    (layout/render
     request
     "organism/player.html"
     {:player player
      :preferences preferences
      :player-games (pr-str player-games)})))

(defn create-page
  [db request]
  (let [player (get-in request [:session :player])
        play-key (-> request :path-params :play)
        preferences (persist/find-player-preferences db player)
        ;; If we're loading an existing open game, preload its invocation
        open-game (when play-key (persist/find-open-game db play-key))]
    (layout/render
     request
     "organism/create.html"
     {:session-player player
      :preferences preferences
      :play-key (or play-key "")
      :open-invocation (if open-game (pr-str (:invocation open-game)) "")})))

(defn play-page
  [db request]
  (let [play-key (-> request :path-params :play)
        player-key (get-in request [:session :player])
        preferences (persist/find-player-preferences db player-key)]
    (layout/render
     request
     "organism/play.html"
     {:player player-key
      :play play-key
      :preferences preferences})))

(defn observe-page
  [db request]
  (let [player (get-in request [:session :player])
        games (persist/load-observe-games db)]
    (layout/render
     request
     "organism/observe.html"
     {:session-player player
      :observe-games (pr-str games)})))

(defn player-page
  [db request]
  (let [player-key (-> request :path-params :player)
        preferences (persist/find-player-preferences db player-key)
        player-games (persist/load-player-games db player-key "organism")]
    (layout/render
     request
     "organism/player.html"
     {:player player-key
      :preferences preferences
      :player-games (pr-str player-games)})))

(defn players-page
  [db request]
  (let [player-stats (persist/load-player-stats db)]
    (layout/render
     request
     "organism/players.html"
     {:session-player (get-in request [:session :player])
      :player-stats (pr-str player-stats)})))

(defn organism-routes
  [db]
  ["/organism"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/create" {:get (partial create-page db)
               :middleware [require-auth]}]
   ["/create/:play" {:get (partial create-page db)
                     :middleware [require-auth]}]
   ["/create/:play/" {:get (partial create-page db)
                      :middleware [require-auth]}]
   ["/play" {:get (partial play-list-page db)
             :middleware [require-auth]}]
   ["/play/:play" {:get (partial play-page db)}]
   ["/play/:play/" {:get (partial play-page db)}]
   ["/observe" {:get (partial observe-page db)}]
   ["/players" {:get (partial players-page db)}]
   ["/player/:player" {:get (partial player-page db)}]
   ["/player/:player/" {:get (partial player-page db)}]])
