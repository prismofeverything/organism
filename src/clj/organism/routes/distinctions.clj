(ns organism.routes.distinctions
  "HTTP routes for DISTINCTIONS. Everything routine is delegated to
   `shared/create-game!` and `shared/require-auth`, as UNIVERSE does."
  (:require
   [organism.layout :as layout]
   [organism.middleware :as middleware]
   [organism.persist :as persist]
   [organism.persist-distinctions :as store]
   [organism.routes.distinctions-ws :as distinctions-ws]
   [organism.routes.shared :as shared]
   [distinctions.game :as game]))

(defn create-game! [db request]
  (shared/create-game!
   {:games-atom  distinctions-ws/games
    :max-players (game/max-players game/hand-size)
    :make-state  (fn [players _] (game/create-game players))
    :persist!    (fn [db play-name players bots state]
                   (store/create-game! db play-name players bots state))}
   db request))

(defn home-page [request]
  (layout/render request "distinctions/home.html"
                 {:session-player (get-in request [:session :player])}))

(defn create-page [request]
  (layout/render request "distinctions/create.html"
                 {:session-player (get-in request [:session :player])}))

(defn play-page [request]
  (layout/render request "distinctions/play.html"
                 {:player (get-in request [:session :player] "--observer--")
                  :play   (-> request :path-params :play)}))

(defn play-list-page [db request]
  (let [player (get-in request [:session :player])]
    (layout/render request "distinctions/games.html"
                   {:session-player player
                    :player-games (pr-str (persist/load-player-games db player "distinctions"))})))

(defn observe-page [db request]
  (layout/render request "distinctions/observe.html"
                 {:session-player (get-in request [:session :player])
                  :observe-games (pr-str (store/load-open-games db))}))

(defn distinctions-routes [db]
  ["/distinctions"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get home-page}]
   ["/create" {:get  create-page
               :post (partial create-game! db)
               :middleware [shared/require-auth]}]
   ["/play" {:get (partial play-list-page db)
             :middleware [shared/require-auth]}]
   ["/play/:play" {:get play-page}]
   ["/play/:play/" {:get play-page}]
   ["/observe" {:get (partial observe-page db)}]])
