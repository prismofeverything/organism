(ns organism.routes.journey-bots
  "HTTP routes for per-player bot editors. Generic — works for any game type.

   - GET  /<game>/bots                 list this player's bots
   - GET  /<game>/bots/:name           open the editor preloaded with that bot
   - POST /<game>/bots/:name           save the bot definition
   - POST /<game>/bots/:name/delete    delete the bot"
  (:require
   [clojure.string :as str]
   [organism.layout :as layout]
   [organism.middleware :as middleware]
   [organism.persist-journey-bots :as bots-db]
   [organism.routes.shared :as shared]
   [ring.util.response :as response]))

(def require-auth shared/require-auth)

(defn- list-page
  [db game-type template-prefix request]
  (let [player (get-in request [:session :player])
        my-bots  (bots-db/list-bots-for-owner db player game-type)
        all-bots (bots-db/list-bots-for-game db game-type)]
    (layout/render
     request
     (str template-prefix "/bots.html")
     {:session-player player
      :my-bots  (pr-str my-bots)
      :all-bots (pr-str all-bots)})))

(defn- editor-page
  [db game-type template-prefix request]
  (let [player   (get-in request [:session :player])
        bot-name (some-> (get-in request [:path-params :name])
                         str/trim str/upper-case)
        bot      (when (and bot-name (not= bot-name "NEW"))
                   (bots-db/find-bot db game-type bot-name))]
    (layout/render
     request
     (str template-prefix "/bot_editor.html")
     {:session-player player
      :bot-name (or (:name bot) (when (and bot-name (not= bot-name "NEW")) bot-name) "")
      :bot-owner (:owner bot)
      :bot (pr-str bot)})))

(defn- save-bot!
  [db game-type request]
  (let [player (get-in request [:session :player])
        params (or (:body-params request) (:params request))
        name   (get params :name (get params "name"))
        desc   (get params :description (get params "description"))
        defn-s (get params :definition (get params "definition"))
        defn   (cond
                 (string? defn-s) (try (read-string defn-s) (catch Exception _ nil))
                 (map?    defn-s) defn-s)]
    (if (and (seq name) defn)
      (let [norm     (-> name str/trim str/upper-case)
            existing (bots-db/find-bot db game-type norm)]
        (cond
          (and existing (:owner existing) (not= (:owner existing) player))
          (response/bad-request {:error (str "bot " norm " is owned by " (:owner existing))})
          :else
          (do (bots-db/save-bot!
               db {:name norm
                   :game-type game-type
                   :owner player
                   :description desc
                   :definition defn})
              (response/response {:saved norm}))))
      (response/bad-request {:error "name and definition required"}))))

(defn- delete-bot!
  [db game-type request]
  (let [player (get-in request [:session :player])
        bot-name (some-> (get-in request [:path-params :name])
                         str/trim str/upper-case)
        bot (when bot-name (bots-db/find-bot db game-type bot-name))]
    (cond
      (nil? bot)
      (response/not-found {:error "not found"})
      (and (:owner bot) (not= (:owner bot) player))
      (response/bad-request {:error "not your bot"})
      :else
      (do (bots-db/delete-bot! db game-type bot-name)
          (response/response {:deleted bot-name})))))

(defn game-bot-routes
  "Build bot editor routes for a game. `path-prefix` is e.g. \"/journey/bots\",
   `game-type` is e.g. \"journey\", `template-prefix` is e.g. \"journey\"."
  [db path-prefix game-type template-prefix]
  [path-prefix
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get (partial list-page db game-type template-prefix)
        :middleware [require-auth]}]
   ["/:name"        {:get (partial editor-page db game-type template-prefix)
                     :post (partial save-bot! db game-type)
                     :middleware [require-auth]}]
   ["/:name/delete" {:post (partial delete-bot! db game-type)
                     :middleware [require-auth]}]])

;; Convenience wrappers
(defn journey-bot-routes [db]
  (game-bot-routes db "/journey/bots" "journey" "journey"))

(defn organism-bot-routes [db]
  (game-bot-routes db "/organism/bots" "organism" "organism"))
