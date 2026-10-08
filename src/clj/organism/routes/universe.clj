(ns organism.routes.universe
  "HTTP routes for UNIVERSE hold'em.

   Everything routine here is delegated: `shared/create-game!` builds the
   table and stores it, `shared/require-auth` guards the pages that need a
   name, and the games/observe lists come from the same per-player collections
   every other game writes.  What is left is the handful of pages."
  (:require
   [organism.layout :as layout]
   [organism.middleware :as middleware]
   [organism.persist :as persist]
   [organism.persist-universe :as store]
   [organism.routes.shared :as shared]
   [organism.routes.universe-ws :as universe-ws]
   [universe.deck :as deck]
   [universe.holdem :as holdem]
   [universe.network :as network]
   [universe.shed :as shed]))

(def max-players
  "A full ring.  The deck would stretch to twenty-eight players -- two cards
   each plus a board of three -- but a poker table is nine."
  9)

(defn- option? [params k]
  ;; the lobby's options arrive as booleans or, from a form, as strings
  (contains? #{true "true"} (get-in params [:options k] (get-in params ["options" (name k)]))))

(defn create-game!
  [db request]
  (let [params   (or (:body-params request) (:params request))
        shed?    (option? params :shed)
        network? (and (not shed?) (option? params :network))]
    (shared/create-game!
     {:games-atom  universe-ws/games
      ;; shedding splits the deck, which runs out of cards worth having past
      ;; six; network needs cards moving, which four hands of ten still allow
      :max-players (cond shed? 6 network? network/max-players :else max-players)
      :make-state  (fn [players params]
                     (cond
                       shed?    (shed/create-game players)
                       network? (network/create-game players)
                       :else
                       (holdem/create-game
                        players
                        (merge {}
                               (when-let [s (:starting-stack params)]
                                 {:starting-stack (if (string? s) (parse-long s) s)})
                               (when (option? params :seven) {:seven? true})))))
      :persist!    (fn [db play-name players bots state]
                     (store/create-game! db play-name players bots state))}
     db request)))

(defn home-page [request]
  (layout/render request "universe/home.html"
                 {:session-player (get-in request [:session :player])}))

(defn create-page [request]
  (layout/render request "universe/create.html"
                 {:session-player (get-in request [:session :player])}))

(defn play-page [request]
  (layout/render request "universe/play.html"
                 {:player (get-in request [:session :player] "--observer--")
                  :play   (-> request :path-params :play)}))

(defn play-list-page [db request]
  (let [player (get-in request [:session :player])]
    (layout/render request "universe/games.html"
                   {:session-player player
                    :player-games (pr-str (persist/load-player-games db player "universe"))})))

(defn observe-page [db request]
  (layout/render request "universe/observe.html"
                 {:session-player (get-in request [:session :player])
                  :observe-games (pr-str (store/load-open-games db))}))

(defn rules-page [request]
  (layout/render request "universe/rules.html"
                 {:session-player (get-in request [:session :player])
                  ;; the chart is the rules, so hand it to the page rather
                  ;; than writing the nineteen rows out twice
                  :chart (pr-str (mapv (fn [row]
                                         (assoc row
                                                :label (deck/hand-name row)
                                                :odds  (long (/ deck/total-hands (:count row)))))
                                       deck/chart))}))

(defn player-page [db request]
  (let [player-key (-> request :path-params :player)]
    (layout/render request "universe/player.html"
                   {:player player-key
                    :session-player (get-in request [:session :player])
                    :player-games (pr-str (persist/load-player-games db player-key "universe"))})))

(defn universe-routes
  [db]
  ["/universe"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get home-page}]
   ["/create" {:get  create-page
               :post (partial create-game! db)
               :middleware [shared/require-auth]}]
   ["/rules" {:get rules-page}]
   ["/play" {:get (partial play-list-page db)
             :middleware [shared/require-auth]}]
   ["/play/:play" {:get play-page}]
   ["/play/:play/" {:get play-page}]
   ["/observe" {:get (partial observe-page db)}]
   ["/player/:player" {:get (partial player-page db)}]
   ["/player/:player/" {:get (partial player-page db)}]])
