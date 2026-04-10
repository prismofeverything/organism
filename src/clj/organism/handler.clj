(ns organism.handler
  (:require
   [organism.mongo :as db]
   [organism.middleware :as middleware]
   [organism.layout :refer [error-page]]
   [organism.routes.home :refer [home-routes]]
   ;; [organism.routes.api :refer [api-routes]]
   [organism.routes.organism :refer [organism-routes]]
   [organism.routes.journey :refer [journey-routes]]
   [organism.routes.journey-ws :refer [journey-ws-routes]]
   [organism.routes.journey-bots :refer [journey-bot-routes organism-bot-routes]]
   [organism.routes.oroboros :refer [oroboros-routes]]
   [organism.routes.oroboros-ws :refer [oroboros-ws-routes]]
   [organism.routes.future :refer [future-routes]]
   [organism.routes.future-ws :refer [future-ws-routes]]
   [organism.routes.websockets :refer [websocket-routes]]
   [reitit.ring :as ring]
   [ring.middleware.content-type :refer [wrap-content-type]]
   [ring.middleware.webjars :refer [wrap-webjars]]
   [organism.env :refer [defaults]]
   [mount.core :as mount]))

(mount/defstate init-app
  :start ((or (:init defaults) (fn [])))
  :stop  ((or (:stop defaults) (fn []))))

(def mongo-connection
  {:host "localhost"
   :port 27017
   :database "organism"})

(mount/defstate app-routes
  :start
  (ring/ring-handler
   (ring/router
    (let [db (db/connect! mongo-connection)]
      [(home-routes db)
       ;; (api-routes)
       (organism-routes db)
       (journey-routes db)
       (journey-bot-routes db)
       (organism-bot-routes db)
       (oroboros-routes db)
       (future-routes db)
       (websocket-routes db)
       (journey-ws-routes db)
       (oroboros-ws-routes)
       (future-ws-routes)]))
   (ring/routes
    (ring/create-resource-handler
     {:path "/"})
    (wrap-content-type
     (wrap-webjars (constantly nil)))
    (ring/create-default-handler
     {:not-found
      (constantly (error-page {:status 404, :title "404 - Page not found"}))
      :method-not-allowed
      (constantly (error-page {:status 405, :title "405 - Not allowed"}))
      :not-acceptable
      (constantly (error-page {:status 406, :title "406 - Not acceptable"}))}))))

(defn app []
  (middleware/wrap-base #'app-routes))
