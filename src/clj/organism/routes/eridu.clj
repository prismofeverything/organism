(ns organism.routes.eridu
  (:require
   [clojure.string :as str]
   [organism.layout :as layout]
   [organism.persist :as persist]
   [organism.persist-eridu :as persist-e]
   [organism.middleware :as middleware]
   [organism.routes.eridu-ws :as eridu-ws]
   [ring.util.response :as response]
   [eridu.game :as game]))

(defn home-page
  [request]
  (layout/render
   request
   "eridu/home.html"
   {:session-player (get-in request [:session :player])}))

(defn require-auth
  [handler]
  (fn [request]
    (if (get-in request [:session :player])
      (handler request)
      (response/redirect (str "/login?redirect=" (:uri request))))))

(defn create-page
  [db request]
  (let [player (get-in request [:session :player])
        preferences (persist/find-player-preferences db player)]
    (layout/render
     request
     "eridu/create.html"
     {:session-player player
      :preferences preferences})))

(defn play-list-page
  "Show the logged-in player's eridu games."
  [db request]
  (let [player (get-in request [:session :player])
        player-games (persist/load-player-games db player "eridu")]
    (layout/render
     request
     "eridu/games.html"
     {:session-player player
      :player-games (pr-str player-games)})))

(defn play-page
  [db request]
  (let [play-key (-> request :path-params :play)
        player-key (get-in request [:session :player])
        preferences (persist/find-player-preferences db player-key)]
    (layout/render
     request
     "eridu/play.html"
     {:player player-key
      :play play-key
      :preferences preferences})))

(defn observe-page
  [db request]
  (let [player (get-in request [:session :player])
        games (persist-e/load-observe-games db)]
    (layout/render
     request
     "eridu/observe.html"
     {:session-player player
      :observe-games (pr-str games)})))

;; ── Generate page — creates an all-bot game ─────────────────��────────────────

(def generate-bot-names
  ["enki" "inanna" "marduk" "ninhursag" "utu"])

(def ^:private generate-words
  ["ziggurat" "cuneiform" "euphrates" "tigris" "sumer" "akkad"
   "temple" "tablet" "reed" "clay" "bronze" "lapis"
   "crescent" "delta" "oasis" "steppe" "marsh" "dune"])

(defn- generate-game-name []
  (let [words (repeatedly 3 #(rand-nth generate-words))]
    (str "generate-" (str/join "-" words))))

(defn generate-page
  "Create an all-bot game and render the play page directly."
  [db request]
  (let [players  generate-bot-names
        bot-set  (set players)
        game-key (generate-game-name)
        state    (game/initial-state players)
        player-key (get-in request [:session :player])]
    (swap! eridu-ws/games
           assoc-in [:games game-key]
           {:key           game-key
            :state         state
            :initial-state state
            :history       []
            :bots          bot-set
            :players       (vec players)
            :chat          []
            :channels      #{}})
    (persist-e/save-game! db game-key state bot-set (vec players) state)
    (layout/render
     request
     "eridu/play.html"
     {:player player-key
      :play game-key
      :preferences "{}"})))

(defn create-game!
  "POST handler: create a new eridu game."
  [db request]
  (let [params    (or (:body-params request) (:params request))
        play-name (get params :play-name (get params "play-name"))
        players   (get params :players (get params "players"))
        bots      (get params :bots (get params "bots" []))]
    (if (and (seq play-name) (seq players))
      (let [state   (game/initial-state (vec players))
            bot-set (set bots)]
        (swap! eridu-ws/games
               assoc-in [:games play-name]
               {:key           play-name
                :state         state
                :initial-state state
                :history       []
                :bots          bot-set
                :players       (vec players)
                :chat          []
                :channels      #{}})
        (persist-e/save-game! db play-name state bot-set (vec players) state)
        (response/response {:play-key play-name}))
      (response/bad-request {:error "play-name and players required"}))))

(defn eridu-routes
  [db]
  ["/eridu"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get home-page}]
   ["/create" {:get  (partial create-page db)
               :post (partial create-game! db)
               :middleware [require-auth]}]
   ["/play" {:get (partial play-list-page db)
             :middleware [require-auth]}]
   ["/play/:play" {:get (partial play-page db)}]
   ["/play/:play/" {:get (partial play-page db)}]
   ["/observe" {:get (partial observe-page db)}]
   ["/generate" {:get (partial generate-page db)}]])
