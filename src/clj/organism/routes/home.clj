(ns organism.routes.home
  (:require
   [organism.board :as board]
   [organism.layout :as layout]
   [organism.game :as game]
   [organism.choice :as choice]
   [organism.persist :as persist]
   [organism.layout :as layout]
   [organism.examples :as examples]
   [hiccup.core :as up]
   [clojure.java.io :as io]
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

(defn home-page
  [db request]
  (let [player (get-in request [:session :player])]
    (layout/render
     request
     "home.html"
     {:session-player player})))

(defn observe-page
  [db request]
  (let [player (get-in request [:session :player])
        games (persist/load-observe-games db)]
    (layout/render request "observe.html"
                   {:session-player player
                    :observe-games (pr-str games)})))

(defn stats-page
  [request]
  (layout/render request "stats.html" {:session-player (get-in request [:session :player])}))

(defn learn-page
  [request]
  (layout/render request "learn.html" {:session-player (get-in request [:session :player])}))

(defn create-game-page
  [db request]
  (let [player (get-in request [:session :player])
        preferences (persist/find-player-preferences db player)]
    (layout/render request "create.html"
                   {:session-player player
                    :preferences preferences})))

(defn login-page
  [request]
  (layout/render request "login.html" {}))

(defn login-submit
  [db request]
  (let [params (:params request)
        player (:player params)
        password (:password params)
        stored-hash (persist/find-player-password db player)]
    (if (and stored-hash (hashers/check password stored-hash))
      (-> (response/redirect (str "/player/" player))
          (assoc :session {:player player}))
      (layout/render request "login.html" {:error "Invalid player name or password"}))))

(defn register-page
  [request]
  (layout/render request "register.html" {}))

(defn register-submit
  [db request]
  (let [params (:params request)
        player (:player params)
        password (:password params)
        password-confirm (:password-confirm params)]
    (cond
      (or (empty? player) (empty? password))
      (layout/render request "register.html" {:error "Player name and password are required"})

      (not= password password-confirm)
      (layout/render request "register.html" {:error "Passwords do not match"})

      (persist/player-has-password? db player)
      (layout/render request "register.html" {:error "That player name is already taken"})

      :else
      (let [hashed (hashers/derive password)]
        (persist/set-player-password! db player hashed)
        (-> (response/redirect (str "/player/" player))
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

(defn game-page
  [db request]
  (let [game-key (-> request :path-params :game)
        player-key (get-in request [:session :player])
        preferences (persist/find-player-preferences db player-key)]
    (layout/render
     request
     "game.html"
     {:player player-key
      :game game-key
      :preferences preferences})))

(defn apply-player-preferences
  [db request]
  (let [player (-> request :path-params :player)]
    (println "applying player preferences: " player (:params request))
    (persist/update-player-preferences! db player (:params request))
    (response/response {:ok true :status :success})))

(defn require-auth
  [handler]
  (fn [request]
    (if (get-in request [:session :player])
      (handler request)
      (response/redirect "/login"))))

(defn home-routes
  [db]
  [""
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get (partial home-page db)}]
   ["/eternal" {:get eternal-page}]
   ["/login" {:get login-page
              :post (partial login-submit db)}]
   ["/register" {:get register-page
                 :post (partial register-submit db)}]
   ["/logout" {:get logout}]
   ["/observe" {:get (partial observe-page db)}]
   ["/stats" {:get stats-page}]
   ["/learn" {:get learn-page}]
   ["/create" {:get (partial create-game-page db)
               :middleware [require-auth]}]
   ["/game/:game" {:get (partial game-page db)}]
   ["/game/:game/" {:get (partial game-page db)}]
   ["/player/:player" {:get (partial player-page db)
                       :middleware [require-player-auth]}]
   ["/player/:player/" {:get (partial player-page db)
                        :middleware [require-player-auth]}]
   ["/player/:player/preferences" {:post (partial apply-player-preferences db)
                                   :middleware [require-player-auth]}]])
