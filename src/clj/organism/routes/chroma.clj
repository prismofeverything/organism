(ns organism.routes.chroma
  "Server shell around the hand-written Chroma JS engine. The game logic lives in
   resources/public/chroma-core.js (served as a static asset and untouched here);
   Clojure only handles serving the play page, the rules reference (rendered from
   chroma.md so it can't drift), and the bug log — mirroring the Eridu pattern."
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [organism.layout :as layout]
   [organism.middleware :as middleware]
   [organism.chroma.engine :as eng]
   [organism.persist-chroma :as persist-c]
   [markdown.core :refer [md-to-html-string]]
   [ring.util.response :as response]))

;; ── auth (mirrors eridu/require-auth) ────────────────────────────────────────

(defn require-auth
  [handler]
  (fn [request]
    (if (get-in request [:session :player])
      (handler request)
      (response/redirect (str "/login?redirect=" (:uri request))))))

(defn- short-key
  ([s] (short-key s 16))
  ([s n]
   (cond
     (nil? s) ""
     (<= (count s) n) s
     :else (str (subs s 0 n) "…"))))

;; ── pages ────────────────────────────────────────────────────────────────────

(defn home-page
  [request]
  (layout/render
   request
   "chroma/home.html"
   {:session-player (get-in request [:session :player])}))

(defn play-page
  "Serve the Chroma play page. The game is fully server-authoritative now: the
   page boots the chroma.js client, which opens a websocket to /ws/chroma/play/:play.
   Each player gets one persistent game keyed by their identity, so a refresh (or a
   different device) resumes the same game straight from MongoDB. Logged-out users
   share a single 'chroma-guest' game on this host."
  [request]
  (let [session-player (get-in request [:session :player])
        play-key (if (seq session-player) session-player "chroma-guest")
        identity (or session-player play-key)]
    (layout/render
     request
     "chroma/play.html"
     {:player identity
      :play play-key
      :timestamp (System/currentTimeMillis)})))

(defn rules-page
  "Render chroma.md as the in-app rules & design reference. chroma.md is the
   source of truth, copied onto the classpath at build time (docs/chroma.md) so
   the page can never drift from the design notes."
  [request]
  (let [res (io/resource "docs/chroma.md")
        md  (if res
              (slurp res)
              "# Chroma rules\n\n_Rules source (docs/chroma.md) not found in this build. Run `./deploy.sh build` to copy it._")]
    (layout/render
     request
     "chroma/rules.html"
     {:session-player (get-in request [:session :player])
      :rules-html (md-to-html-string md)})))

;; ── leaderboard (mirrors eridu's aggregate + hall-of-fame) ───────────────────

(defn leaderboard-data
  "Aggregate completed Chroma games into per-player standings + a hall-of-fame of
   top single-game scores. Scores come from the engine's op2b scoring on each
   finished board. Bots are included (flagged) so a solo human still sees a board."
  [db]
  (let [entries
        (for [server (persist-c/completed-games db)
              :let [G (:state server)
                    scores (eng/with-config G (eng/score-game (:players G) (:stacks G)))
                    names (:players server)
                    bots (set (:bots server))
                    n (count scores)
                    winner (when (pos? n) (apply max-key #(:mult (nth scores %)) (range n)))]
              seat (range n)]
          {:player (nth names seat (str "seat-" seat))
           :bot (contains? bots seat)
           :points (:mult (nth scores seat))
           :win (= seat winner)
           :game-key (:game-key server)
           :at (:updated server)})
        aggregate
        (->> (group-by :player entries)
             (map (fn [[player es]]
                    (let [games (count es)
                          wins (count (filter :win es))
                          pts (map :points es)]
                      {:player player
                       :bot (boolean (:bot (first es)))
                       :games games
                       :wins wins
                       :win-rate (if (pos? games) (double (* 100 (/ wins games))) 0.0)
                       :avg-points (if (pos? games) (double (/ (reduce + pts) games)) 0.0)
                       :best (if (seq pts) (reduce max pts) 0)})))
             (sort-by (juxt #(- (:wins %)) #(- (:avg-points %))))
             vec)
        hall (->> entries
                  (sort-by #(- (:points %)))
                  (take 25)
                  (mapv (fn [e] (assoc e :game-key-short (short-key (str (:game-key e)) 18)))))]
    {:aggregate aggregate :hall-of-fame hall}))

(defn leaderboard-page
  [db request]
  (let [data (leaderboard-data db)]
    (layout/render
     request
     "chroma/leaderboard.html"
     {:session-player (get-in request [:session :player])
      ;; nil (not []) when empty so the template's {% if %} shows the empty state —
      ;; selmer treats an empty vector as truthy.
      :aggregate (seq (:aggregate data))
      :hall-of-fame (seq (:hall-of-fame data))})))

;; ── bug log (mirrors eridu's JSONL bug-report) ───────────────────────────────

(def ^:private bug-report-file
  "Where in-game bug reports get appended (JSON Lines), alongside Eridu's."
  (str (System/getProperty "user.home") "/Documents/chroma-bug-reports.jsonl"))

(defn- append-record!
  "Append one EDN record as a line to the bug-report JSONL. Best-effort."
  [record]
  (let [file (io/file bug-report-file)]
    (some-> file .getParentFile (.mkdirs))
    (with-open [w (java.io.FileWriter. ^java.io.File file true)]
      (.write w (pr-str record))
      (.write w "\n"))))

(defn- read-bug-reports
  "Parse the JSONL log into a vector of maps. Any unreadable line is skipped."
  []
  (let [f (io/file bug-report-file)]
    (when (.exists f)
      (->> (slurp f)
           str/split-lines
           (keep (fn [line]
                   (when (seq line)
                     (try (read-string line) (catch Exception _ nil)))))
           (map-indexed (fn [idx r] (assoc r :idx idx)))
           vec))))

(defn bug-report-page
  "Standalone bug-report form, accessible without a game in progress."
  [request]
  (layout/render
   request
   "chroma/bug-report.html"
   {:session-player (get-in request [:session :player])
    :sent? (= "1" (get-in request [:params :sent]))}))

(defn report-bug!
  "In-game bug report (JSON POST): appends the report + game-state snapshot to
   the JSONL. No auth — the play page can submit anonymously, like Eridu."
  [request]
  (let [session-player (get-in request [:session :player])
        body   (or (:body-params request) (:params request))
        record (assoc body
                      :session-player session-player
                      :received-at (System/currentTimeMillis)
                      :remote-addr (:remote-addr request))]
    (try
      (append-record! record)
      (response/response {:ok true})
      (catch Exception e
        (-> (response/response {:error (.getMessage e)})
            (response/status 500))))))

(defn submit-bug-form!
  "Form POST from /chroma/bug-report: appends to the same JSONL, then redirects
   back with a sent flag. Auth-gated so reports carry a username."
  [request]
  (let [session-player (get-in request [:session :player])
        params (or (:form-params request) (:params request))
        text   (get params "text" (get params :text ""))
        state  (get params "state" (get params :state nil))
        record (cond-> {:text text
                        :session-player session-player
                        :player session-player
                        :ts (str (java.time.Instant/now))
                        :url (get-in request [:headers "referer"])
                        :remote-addr (:remote-addr request)
                        :received-at (System/currentTimeMillis)}
                 (and state (seq (str state))) (assoc :state state))]
    (if (str/blank? text)
      (response/redirect "/chroma/bug-report")
      (do (try (append-record! record) (catch Exception _ nil))
          (response/redirect "/chroma/bug-report?sent=1")))))

(defn bug-report-list-page
  "Admin viewer: every report in the JSONL, newest first."
  [request]
  (let [reports (or (read-bug-reports) [])
        decorated (mapv (fn [r]
                          (assoc r :player-short (short-key (str (:player r)) 16)))
                        reports)]
    (layout/render
     request
     "chroma/bugs-list.html"
     {:session-player (get-in request [:session :player])
      :reports (vec (reverse decorated))
      :report-count (count decorated)})))

(defn dump-bug-reports
  "Stream the local bug-report JSONL to any logged-in user (HTTPS pull, no SSH)."
  [_request]
  (let [file (io/file bug-report-file)]
    (-> (response/response (if (.exists file) (slurp file) ""))
        (response/content-type "application/x-ndjson")
        (response/header "Content-Disposition"
                         "attachment; filename=chroma-bug-reports.jsonl"))))

;; ── routes ───────────────────────────────────────────────────────────────────

(defn chroma-routes
  [db]
  ["/chroma"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get home-page}]
   ["/play" {:get play-page}]
   ["/rules" {:get rules-page}]
   ["/leaderboard" {:get (partial leaderboard-page db)}]
   ["/bug-report" {:get  bug-report-page
                   :post report-bug!}]
   ["/bug-report/submit" {:post submit-bug-form!
                          :middleware [require-auth]}]
   ["/bug-report/list" {:get bug-report-list-page
                        :middleware [require-auth]}]
   ["/bug-report/dump" {:get dump-bug-reports
                        :middleware [require-auth]}]])
