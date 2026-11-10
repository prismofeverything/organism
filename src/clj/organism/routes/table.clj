(ns organism.routes.table
  "HTTP routes for the playtest table: the list of your tables, a new one, the
   table itself, a solo table that lives only in the browser, uploaded files,
   and the components a player has kept."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [organism.layout :as layout]
   [organism.middleware :as middleware]
   [organism.persist-table :as store]
   [organism.routes.shared :as shared]
   [ring.util.response :as response]))

(defn- player [request] (get-in request [:session :player]))

(defn- table-key
  "A URL-safe key from the title, with a few random letters so two tables can
   share a name."
  [title]
  (let [slug (-> (str/lower-case (or title ""))
                 (str/replace #"[^a-z0-9]+" "-")
                 (str/replace #"^-+|-+$" ""))
        slug (if (str/blank? slug) "table" (subs slug 0 (min 40 (count slug))))
        tail (apply str (repeatedly 5 #(rand-nth "abcdefghjkmnpqrstuvwxyz23456789")))]
    (str slug "-" tail)))

(defn home-page [db request]
  (let [p (player request)]
    (layout/render request "table/home.html"
                   {:session-player p
                    :tables (when p (store/player-tables db p))})))

(defn create! [db request]
  (let [title (str/trim (or (get-in request [:params :title]) (get-in request [:params "title"]) ""))
        title (if (str/blank? title) "Playtest table" title)
        k (table-key title)]
    (store/create-table! db k title (player request))
    (response/redirect (str "/table/play/" k) :see-other)))

(defn leave! [db request]
  (store/leave-table! db (-> request :path-params :play) (player request))
  (response/redirect "/table" :see-other))

(defn play-page [db request]
  (let [k (-> request :path-params :play)]
    (if-let [t (store/load-table db k)]
      (layout/render request "table/play.html"
                     {:player (player request) :play k :title (:title t)})
      (layout/error-page {:status 404 :title "No such table"}))))

(defn solo-page [request]
  (layout/render request "table/play.html" {:player (player request) :play "" :title "Solo table"}))

;; ── files and components ───────────────────────────────────────────────────

(defn- param [request k]
  (or (get-in request [:body-params k]) (get-in request [:body-params (name k)])
      (get-in request [:params k]) (get-in request [:params (name k)])))

(defn upload! [db request]
  (let [data (param request :data)]
    (if (and (string? data) (str/starts-with? data "data:"))
      (response/response {:url (str "/table/asset/" (store/put-asset! db data))})
      (response/bad-request {:error "send {data: <data URL>}"}))))

(defn asset [db request]
  (if-let [{:keys [mime bytes]} (store/get-asset db (-> request :path-params :hash))]
    {:status 200
     :headers {"Content-Type" mime "Cache-Control" "public, max-age=31536000, immutable"}
     :body (java.io.ByteArrayInputStream. bytes)}
    (response/not-found "")))

(defn components [db request]
  (response/response {:items (pr-str (store/load-components db (player request)))}))

(defn save-components! [db request]
  (let [items (param request :items)]
    (store/save-components! db (player request) (if (string? items) (edn/read-string items) []))
    (response/response {:ok true})))

(defn table-routes [db]
  ["/table"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get (partial home-page db)}]
   ["/" {:get (partial home-page db)}]
   ["/create" {:post (partial create! db) :middleware [shared/require-auth]}]
   ["/solo" {:get solo-page}]
   ["/play/:play" {:get (partial play-page db) :middleware [shared/require-auth]}]
   ["/play/:play/leave" {:post (partial leave! db) :middleware [shared/require-auth]}]
   ["/upload" {:post (partial upload! db) :middleware [shared/require-auth]}]
   ["/asset/:hash" {:get (partial asset db)}]
   ["/components" {:get (partial components db)
                   :post (partial save-components! db)
                   :middleware [shared/require-auth]}]])
