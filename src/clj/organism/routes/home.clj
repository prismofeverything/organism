(ns organism.routes.home
  (:require
   [organism.board :as board]
   [organism.layout :as layout]
   [organism.choice :as choice]
   [organism.persist :as persist]
   [organism.examples :as examples]
   [hiccup.core :as up]
   [organism.middleware :as middleware]
   [buddy.hashers :as hashers]
   [ring.util.response :as response]
   [ring.util.http-response :as http-response]))

(def home-game
  (atom {}))

(def all-rings
  ["A" "B" "C" "D" "E" "F" "G"])

(defn empty-game
  [starting-game]
  {:colors (board/generate-colors all-rings)
   :games (choice/random-walk starting-game)})

(defn require-player-auth
  [handler]
  (fn [request]
    (let [player (-> request :path-params :player)]
      (if (= (get-in request [:session :player]) player)
        (handler request)
        (response/redirect "/login")))))

(defn catalog-page
  [request]
  (layout/render request "home.html" {}))

(defn organism-home-page
  [request]
  (layout/render
   request
   "organism/home.html"
   {:session-player (get-in request [:session :player])}))

(defn learn-page
  [request]
  (layout/render request "learn.html" {:session-player (get-in request [:session :player])}))

(defn safe-redirect
  "Only allow relative paths to prevent open redirect."
  [redirect _player]
  (if (and redirect
           (string? redirect)
           (clojure.string/starts-with? redirect "/")
           (not (clojure.string/starts-with? redirect "//")))
    redirect
    "/"))

(defn- game-title
  "Derive a display title from the redirect path."
  [redirect]
  (cond
    (and redirect (clojure.string/starts-with? redirect "/journey"))  "JOURNEY"
    (and redirect (clojure.string/starts-with? redirect "/oroboros")) "UNIVERSAL"
    :else "ORGANISM"))

(defn login-page
  [request]
  (let [redirect (get-in request [:query-params "redirect"])]
    (layout/render request "login.html"
                   {:redirect redirect
                    :game-title (game-title redirect)})))

(defn login-submit
  [db request]
  (let [params (:params request)
        player (:player params)
        password (:password params)
        redirect (safe-redirect (:redirect params) player)
        stored-hash (persist/find-player-password db player)]
    (if (and stored-hash (hashers/check password stored-hash))
      (-> (response/redirect redirect)
          (assoc :session {:player player}))
      (layout/render request "login.html"
                     {:error "Invalid player name or password"
                      :redirect (:redirect params)
                      :game-title (game-title (:redirect params))}))))

(defn register-page
  [request]
  (let [redirect (get-in request [:query-params "redirect"])]
    (layout/render request "register.html"
                   {:redirect redirect
                    :game-title (game-title redirect)})))

(defn register-submit
  [db request]
  (let [params (:params request)
        player (:player params)
        password (:password params)
        password-confirm (:password-confirm params)
        redirect (safe-redirect (:redirect params) player)]
    (cond
      (or (empty? player) (empty? password))
      (layout/render request "register.html"
                     {:error "Player name and password are required"
                      :redirect (:redirect params)
                      :game-title (game-title (:redirect params))})

      (not= password password-confirm)
      (layout/render request "register.html"
                     {:error "Passwords do not match"
                      :redirect (:redirect params)
                      :game-title (game-title (:redirect params))})

      (persist/player-has-password? db player)
      (layout/render request "register.html"
                     {:error "That player name is already taken"
                      :redirect (:redirect params)
                      :game-title (game-title (:redirect params))})

      :else
      (let [hashed (hashers/derive password)]
        (persist/set-player-password! db player hashed)
        (persist/update-player-preferences! db player {:color (board/random-color 0.4 0.8)})
        (-> (response/redirect redirect)
            (assoc :session {:player player}))))))

(defn logout
  [request]
  (-> (response/redirect "/")
      (assoc :session nil)))

(defn player-page
  [db request]
  (let [player-key (-> request :path-params :player)
        preferences (persist/find-player-preferences db player-key)
        player-games (persist/load-player-games db player-key)]
    (layout/render
     request
     "player.html"
     {:player player-key
      :preferences preferences
      :player-games (pr-str player-games)})))

(defn eternal-page [request]
  (http-response/content-type
   (http-response/ok
    (do
      (if (empty? (deref home-game))
        (reset! home-game (empty-game (examples/six-player-game))))
      (let [home (deref home-game)
            {:keys [games colors]} home
            game (first games)
            board (board/build-board 6 50 2.1 colors all-rings (:turn-order game) true)]
        (swap! home-game update :games rest)
        (up/html (board/render-game board game)))))
   "text/html; charset=utf-8"))

(defn apply-player-preferences
  [db request]
  (let [player (-> request :path-params :player)]
    (println "applying player preferences: " player (:params request))
    (persist/update-player-preferences! db player (:params request))
    (response/response {:ok true :status :success})))

(defn account-page
  [db request]
  (let [player (-> request :path-params :player)
        preferences (persist/find-player-preferences db player)
        color (or (:color preferences) "#888888")]
    (layout/render
     request
     "account.html"
     {:player player
      :color color})))

(defn account-submit
  [db request]
  (let [player (-> request :path-params :player)
        color (get-in request [:params :color])]
    (persist/update-player-preferences! db player {:color color})
    (response/response {:ok true})))

(defn home-routes
  [db]
  [""
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get catalog-page}]
   ["/organism" {:get organism-home-page}]
   ["/eternal" {:get eternal-page}]
   ["/login" {:get login-page
              :post (partial login-submit db)}]
   ["/register" {:get register-page
                 :post (partial register-submit db)}]
   ["/logout" {:get logout}]
   ["/learn" {:get learn-page}]
   ["/player/:player" {:get (partial player-page db)}]
   ["/player/:player/" {:get (partial player-page db)}]
   ["/player/:player/account" {:get (partial account-page db)
                               :post (partial account-submit db)
                               :middleware [require-player-auth]}]
   ["/player/:player/preferences" {:post (partial apply-player-preferences db)
                                   :middleware [require-player-auth]}]])
