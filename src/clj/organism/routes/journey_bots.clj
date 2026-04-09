(ns organism.routes.journey-bots
  "HTTP routes for the per-player journey bot editor.

   - GET  /journey/bots                 list this player's bots + a 'new bot' link
   - GET  /journey/bots/new             open the editor with the empty default bot
   - GET  /journey/bots/:name           open the editor preloaded with that bot
   - POST /journey/bots/:name           save the bot definition
   - POST /journey/bots/:name/delete    delete the bot"
  (:require
   [clojure.string :as str]
   [organism.layout :as layout]
   [organism.middleware :as middleware]
   [organism.persist-journey-bots :as bots-db]
   [organism.routes.shared :as shared]
   [ring.util.response :as response]))

(def require-auth shared/require-auth)

(defn list-page
  [db request]
  (let [player (get-in request [:session :player])
        my-bots  (bots-db/list-bots db player)
        all-bots (bots-db/list-bots db)]
    (layout/render
     request
     "journey/bots.html"
     {:session-player player
      :my-bots  (pr-str my-bots)
      :all-bots (pr-str all-bots)})))

(defn editor-page
  "Render the editor preloaded with either an existing bot or the default."
  [db request]
  (let [player   (get-in request [:session :player])
        bot-name (some-> (get-in request [:path-params :name])
                         str/trim
                         str/upper-case)
        bot      (when (and bot-name (not= bot-name "NEW"))
                   (bots-db/find-bot db bot-name))]
    (layout/render
     request
     "journey/bot_editor.html"
     {:session-player player
      :bot-name (or (:name bot) (when (and bot-name (not= bot-name "NEW")) bot-name) "")
      :bot-owner (:owner bot)
      :bot (pr-str bot)})))

(defn save-bot!
  "POST handler — accepts {:name :description :definition} and stores it.
   Only the owner can overwrite an existing bot."
  [db request]
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
            existing (bots-db/find-bot db norm)]
        (cond
          (and existing (:owner existing) (not= (:owner existing) player))
          (response/bad-request {:error (str "bot " norm " is owned by " (:owner existing))})
          :else
          (do (bots-db/save-bot!
               db {:name norm
                   :owner player
                   :description desc
                   :definition defn})
              (response/response {:saved norm}))))
      (response/bad-request {:error "name and definition required"}))))

(defn delete-bot!
  [db request]
  (let [player (get-in request [:session :player])
        bot-name (some-> (get-in request [:path-params :name])
                         str/trim str/upper-case)
        bot (when bot-name (bots-db/find-bot db bot-name))]
    (cond
      (nil? bot)
      (response/not-found {:error "not found"})
      (and (:owner bot) (not= (:owner bot) player))
      (response/bad-request {:error "not your bot"})
      :else
      (do (bots-db/delete-bot! db bot-name)
          (response/response {:deleted bot-name})))))

(defn journey-bot-routes
  [db]
  ["/journey/bots"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get (partial list-page db)
        :middleware [require-auth]}]
   ;; /journey/bots/NEW is handled by editor-page as a fresh template;
   ;; any other :name loads that bot for editing.
   ["/:name"        {:get (partial editor-page db)
                     :post (partial save-bot! db)
                     :middleware [require-auth]}]
   ["/:name/delete" {:post (partial delete-bot! db)
                     :middleware [require-auth]}]])
