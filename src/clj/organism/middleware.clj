(ns organism.middleware
  (:require
    [organism.env :refer [defaults]]
    [clojure.tools.logging :as log]
    [organism.layout :refer [error-page]]
    [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
    [organism.middleware.formats :as formats]
    [muuntaja.middleware :refer [wrap-format wrap-params]]
    [organism.config :refer [env]]
    [ring.middleware.flash :refer [wrap-flash]]
    [ring.middleware.session :refer [wrap-session]]
    [ring.middleware.session.cookie :refer [cookie-store]]
    [ring.middleware.defaults :refer [site-defaults wrap-defaults]])
  )

(defn wrap-internal-error [handler]
  (fn [req]
    (try
      (handler req)
      (catch Throwable t
        (log/error t (.getMessage t))
        (error-page {:status 500
                     :title "Something very bad has happened!"
                     :message "We've dispatched a team of highly trained gnomes to take care of the problem."})))))

(defn wrap-csrf [handler]
  (wrap-anti-forgery
    handler
    {:error-response
     (error-page
       {:status 403
        :title "Invalid anti-forgery token"})}))


(defn wrap-formats [handler]
  (let [wrapped (-> handler wrap-params (wrap-format formats/instance))]
    (fn [request]
      ;; disable wrap-formats for websockets
      ;; since they're not compatible with this middleware
      ((if (:websocket? request)
         handler
         wrapped)
       request))))

;; 16-byte key for signing session cookies. Sessions survive server restarts.
(defonce session-store
  (cookie-store {:key (.getBytes "organism-secret!" "UTF-8")}))

(defn wrap-base [handler]
  (-> ((:middleware defaults) handler)
      wrap-flash
      (wrap-session {:store session-store
                     :cookie-attrs {:http-only true :max-age (* 60 60 24 30)}})
      (wrap-defaults
        (-> site-defaults
            (assoc-in [:security :anti-forgery] false)
            (dissoc :session)))
      wrap-internal-error))
