(ns organism.routes.eridu
  (:require
   [clojure.string :as str]
   [organism.layout :as layout]
   [organism.persist :as persist]
   [organism.persist-eridu :as persist-e]
   [organism.middleware :as middleware]
   [organism.routes.eridu-ws :as eridu-ws]
   [ring.util.response :as response]
   [eridu.game :as game]
   [eridu.personality :as pers]
   [eridu.simulate :as sim]
   [eridu.evolve :as evolve]))

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
      :preferences preferences
      :timestamp (System/currentTimeMillis)})))

(defn offline-page
  "Render the offline-vs-AI page, gated by login so games can be tagged
   with the session player and synced back later."
  [db request]
  (let [player-key (get-in request [:session :player])
        preferences (persist/find-player-preferences db player-key)]
    (layout/render
     request
     "eridu/offline.html"
     {:player player-key
      :preferences preferences
      :timestamp (System/currentTimeMillis)})))

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
  "Create a random 3-player game and render the play page directly.
   Each reload starts a fresh game."
  [db request]
  (let [players  (vec (take 3 (shuffle generate-bot-names)))
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
            :bot-delay     150
            :chat          []
            :channels      #{}})
    (persist-e/save-game! db game-key state bot-set (vec players) state)
    (layout/render
     request
     "eridu/play.html"
     {:player player-key
      :play game-key
      :preferences "{}"
      :timestamp (System/currentTimeMillis)})))

(defn create-game!
  "POST handler: create a new eridu game."
  [db request]
  (let [params    (or (:body-params request) (:params request))
        play-name (get params :play-name (get params "play-name"))
        players   (get params :players (get params "players"))
        bots      (get params :bots (get params "bots" []))
        mode      (keyword (get params :mode (get params "mode" "normal")))]
    (if (and (seq play-name) (seq players)
             (<= (count players) (if (= mode :solo) 1 4)))
      (let [players (if (= mode :solo) [(first players)] (vec (take 4 players)))
            state   (if (= mode :solo)
                      (game/initial-solo-state (first players))
                      (game/initial-state players))
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

;; ── Simulation state ─────────────────────────────────────────────────────────

(defonce simulation-results (atom nil))
(defonce simulation-running? (atom false))

;; ── Stats page ──────────────────────────────────────────────────────────────

(defn stats-page
  [_db request]
  (let [player (get-in request [:session :player])
        sim-data @simulation-results
        evo-status (evolve/evolution-status)]
    (layout/render
     request
     "eridu/stats.html"
     {:session-player player
      :sim-running (boolean @simulation-running?)
      :sim-data (when sim-data
                  (pr-str {:meta (:meta sim-data)
                           :by-personality (sim/aggregate-by-personality (:summaries sim-data))
                           :by-player-count (sim/aggregate-by-player-count (:summaries sim-data))
                           :weighted (sim/weighted-aggregate-by-personality (:summaries sim-data))}))
      :evo-status (pr-str evo-status)})))

;; ── Run simulation endpoint ──────────────────────────────────────────────────

(defn run-simulation!
  [_db request]
  (if @simulation-running?
    (response/response {:status "already-running"})
    (do
      (reset! simulation-running? true)
      (future
        (try
          (let [personalities (concat pers/archetypes
                                      (repeatedly 4 pers/random-personality))
                ;; Run 1000 games of each player count
                results-2p (sim/run-batch 1000 2 personalities)
                results-3p (sim/run-batch 1000 3 personalities)
                results-4p (sim/run-batch 1000 4 personalities)
                combined {:summaries (vec (concat (:summaries results-2p)
                                                   (:summaries results-3p)
                                                   (:summaries results-4p)))
                          :all-snapshots (vec (concat (:all-snapshots results-2p)
                                                      (:all-snapshots results-3p)
                                                      (:all-snapshots results-4p)))
                          :meta {:total-games 3000
                                 :player-counts [2 3 4]
                                 :games-per-count 1000}}]
            (reset! simulation-results combined))
          (catch Exception e
            (println "Simulation error:" (.getMessage e)))
          (finally
            (reset! simulation-running? false))))
      (response/response {:status "started"}))))

;; ── CSV download endpoints ───────────────────────────────────────────────────

(defn download-summaries-csv [_db _request]
  (if-let [data @simulation-results]
    (-> (response/response (sim/export-summaries-csv data))
        (response/content-type "text/csv")
        (response/header "Content-Disposition" "attachment; filename=eridu-summaries.csv"))
    (response/not-found "No simulation data")))

(defn download-snapshots-csv [_db _request]
  (if-let [data @simulation-results]
    (-> (response/response (sim/export-snapshots-csv data))
        (response/content-type "text/csv")
        (response/header "Content-Disposition" "attachment; filename=eridu-snapshots.csv"))
    (response/not-found "No simulation data")))

;; ── Evolution endpoints ──────────────────────────────────────────────────────

(defn start-evolution-endpoint! [_db request]
  (let [params (or (:body-params request) (:params request))
        config {:pop-size          (or (:pop-size params) 30)
                :generations       (or (:generations params) 100)
                :games-per-matchup (or (:games-per-matchup params) 3)
                :mutation-rate     (or (:mutation-rate params) 0.3)
                :elite-count       (or (:elite-count params) 4)
                :player-counts     (or (:player-counts params) [2 3 4])}]
    (try
      (evolve/start-evolution! config)
      (response/response {:status "started" :config config})
      (catch Exception e
        (response/response {:status "error" :message (.getMessage e)})))))

(defn stop-evolution-endpoint! [_db _request]
  (evolve/stop-evolution!)
  (response/response {:status "stopped"}))

(defn evolution-status-endpoint [_db _request]
  (response/response (evolve/evolution-status)))

(defn top-personalities-endpoint [_db _request]
  (response/response {:personalities (vec (evolve/top-personalities 10))}))

(def ^:private bug-report-file
  "Where in-game bug reports get appended (JSON Lines)."
  (str (System/getProperty "user.home") "/Documents/eridu-bug-reports.jsonl"))

(defn dump-bug-reports
  "Stream the local bug-report JSONL back to any logged-in user. Lets
   Mohammad pull reports via HTTPS instead of needing SSH access. The
   require-auth middleware ensures the request has a session player; the
   content-disposition makes browsers save it as a file."
  [_db _request]
  (let [file (java.io.File. ^String bug-report-file)]
    (if (.exists file)
      (-> (response/response (slurp file))
          (response/content-type "application/x-ndjson")
          (response/header "Content-Disposition"
                           "attachment; filename=eridu-bug-reports.jsonl"))
      (-> (response/response "")
          (response/content-type "application/x-ndjson")
          (response/header "Content-Disposition"
                           "attachment; filename=eridu-bug-reports.jsonl")))))

(defn report-bug!
  "Append a player-submitted bug report (plus the game state snapshot) to a
   local JSONL file so a separate watcher script can pick them up and triage."
  [_db request]
  (let [session-player (get-in request [:session :player])
        body           (or (:body-params request) (:params request))
        record         (assoc body
                              :session-player session-player
                              :received-at (System/currentTimeMillis)
                              :remote-addr (:remote-addr request))]
    (try
      (let [file (java.io.File. ^String bug-report-file)]
        (-> file .getParentFile (.mkdirs)))
      (with-open [w (java.io.FileWriter. ^String bug-report-file true)]
        (.write w (pr-str record))
        (.write w "\n"))
      (response/response {:ok true})
      (catch Exception e
        (-> (response/response {:error (.getMessage e)})
            (response/status 500))))))

(defn log-offline-game!
  "Persist a completed offline-vs-AI game record sent from the client.
   Auth-gated: the server overwrites :human with the session player so
   clients can't impersonate other users. Returns the saved game-key."
  [db request]
  (let [session-player (get-in request [:session :player])
        body           (or (:body-params request) (:params request))
        record         (assoc body
                              :human session-player
                              :received-at (System/currentTimeMillis))]
    (cond
      (not session-player)
      (-> (response/response {:error "not authenticated"})
          (response/status 401))

      (or (empty? (:game-key record)) (empty? (:initial-state record)))
      (-> (response/response {:error "missing required fields :game-key, :initial-state"})
          (response/status 400))

      :else
      (do (persist-e/save-offline-game! db record)
          (response/response {:ok true :game-key (:game-key record)})))))

;; ── Routes ───────────────────────────────────────────────────────────────────

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
   ["/offline" {:get (partial offline-page db)
                :middleware [require-auth]}]
   ["/observe" {:get (partial observe-page db)}]
   ["/generate" {:get (partial generate-page db)}]
   ["/stats" {:get (partial stats-page db)}]
   ["/simulate" {:post (partial run-simulation! db)}]
   ["/simulate/summaries.csv" {:get (partial download-summaries-csv db)}]
   ["/simulate/snapshots.csv" {:get (partial download-snapshots-csv db)}]
   ["/simulate/weighted.csv" {:get (fn [_ _]
                                      (if-let [data @simulation-results]
                                        (-> (response/response (sim/export-weighted-csv (:summaries data)))
                                            (response/content-type "text/csv")
                                            (response/header "Content-Disposition"
                                                             "attachment; filename=eridu-weighted.csv"))
                                        (response/not-found "No simulation data")))}]
   ["/evolve/start" {:post (partial start-evolution-endpoint! db)}]
   ["/evolve/stop" {:post (partial stop-evolution-endpoint! db)}]
   ["/evolve/status" {:get (partial evolution-status-endpoint db)}]
   ["/evolve/top" {:get (partial top-personalities-endpoint db)}]
   ["/offline/log" {:post (partial log-offline-game! db)
                    :middleware [require-auth]}]
   ["/bug-report" {:post (partial report-bug! db)}]
   ["/bug-report/dump" {:get (partial dump-bug-reports db)
                        :middleware [require-auth]}]])
