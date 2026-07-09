(ns organism.routes.journeymen
  "Journeymen on the organism server. The engine runs entirely client-side
   (journeymen-e.play drives the cljc engine in the browser; seats beyond 0 are
   bots) — the server serves the play + rules pages chroma-style, remembers whole-game
   snapshots so any device can resume a game by its key, and collects in-game BUG
   REPORTS + JS error beacons to MongoDB.

   Pages (CSRF-wrapped, rendered via selmer like chroma):
     GET  /journeymen            play (client deals a fresh game vs bots)
     GET  /journeymen/play       alias of the play page
     GET  /journeymen/rules      journeymen-e-rules.md rendered as the in-app reference

   Data endpoints (best-effort, unauthenticated — opaque client keys; snapshots/reports
   are just board state + free text, no auth/session/cross-user state, so no CSRF
   concern; deliberately OUTSIDE wrap-csrf, the client fetches carry no token):
     POST /journeymen/save/:id   body = pr-str'd {:schema .. :st ..}  -> {:ok true}
     GET  /journeymen/load/:id                                        -> {:state \"<edn>\"|nil}
     POST /journeymen/bug        body = pr-str'd {:notes .. :state .. :log ..} -> {:ok true}
     POST /journeymen/beacon     body = JSON from the page's window.onerror hook -> {:ok true}

   Save/load pass the EDN body string through verbatim (read-string round-trips the
   engine's sets: :gates set-of-sets, :skilled-pool set, :thugs map-of-sets)."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [markdown.core :refer [md-to-html-string]]
   [organism.layout :as layout]
   [organism.middleware :as middleware]
   [organism.mongo :as db]
   [ring.util.response :as response]))

(defn- read-body-string
  "Slurp the request body to a string (the client POSTs raw pr-str'd EDN)."
  [request]
  (let [b (:body request)]
    (cond
      (nil? b)    nil
      (string? b) b
      :else       (slurp (io/reader b)))))

;; ── pages ────────────────────────────────────────────────────────────────────

(defn play-e-page
  "Journeymen play page. Client-side game vs bots; state in localStorage; bug
   beacons share /journeymen/beacon."
  [request]
  (layout/render
   request
   "journeymen/play-e.html"
   {:session-player (get-in request [:session :player])
    :timestamp (System/currentTimeMillis)}))

(defn rules-e-page
  "Journeymen rules (game-ideas/journeymen/journeymen-e-rules.md; uberjar fallback
   docs/journeymen-e-rules.md staged by deploy.sh)."
  [request]
  (let [repo-file (io/file "game-ideas/journeymen/journeymen-e-rules.md")
        md (cond
             (.exists repo-file) (slurp repo-file)
             :else (if-let [res (io/resource "docs/journeymen-e-rules.md")]
                     (slurp res)
                     "# Journeymen rules\n\n_Rules source not found in this build._"))]
    (layout/render
     request
     "journeymen/rules.html"
     {:session-player (get-in request [:session :player])
      :rules-html (md-to-html-string md)})))

;; ── snapshot persistence ─────────────────────────────────────────────────────

(defn save-game!
  "Persist the posted snapshot string under the :id path param. Stores the body verbatim and parses
   it best-effort for turn/round/phase metadata. Degrades gracefully (the client save is fire-and-forget)."
  [db request]
  (let [game-key (-> request :path-params :id)
        raw (read-body-string request)]
    (try
      (if (and (seq game-key) (seq raw))
        (let [parsed (try (read-string raw) (catch Exception _ nil))
              st (or (:st parsed) (:state parsed))]
          (db/index! db :journeymen-games [:key] {:unique true})
          (db/merge!
           db :journeymen-games
           {:key game-key}
           {:snapshot  raw
            :game-type "journeymen"
            :turn      (:turn st)
            :round     (:round st)
            :phase     (name (or (:phase st) :setup))
            :over      (= :over (:phase st))
            :updated   (quot (System/currentTimeMillis) 1000)})
          (response/response (pr-str {:ok true})))
        (-> (response/response (pr-str {:ok false :error "missing id or body"}))
            (response/status 400)))
      (catch Exception e
        (-> (response/response (pr-str {:ok false :error (.getMessage e)}))
            (response/status 500))))))

(defn load-game
  "Return the saved snapshot STRING for the :id path param under :state (nil when absent). The client
   read-strings the {:state ..} envelope, then read-strings the inner snapshot back into its atom."
  [db request]
  (let [game-key (-> request :path-params :id)]
    (try
      (let [doc  (when (seq game-key) (db/one db :journeymen-games {:key game-key}))
            snap (:snapshot doc)]
        (-> (response/response (pr-str {:state snap}))
            (response/content-type "application/edn")))
      (catch Exception e
        (-> (response/response (pr-str {:state nil :error (.getMessage e)}))
            (response/content-type "application/edn")
            (response/status 500))))))

;; ── bug reports + error beacons ──────────────────────────────────────────────

(defn submit-bug!
  "Append an in-game bug report to the :journeymen-bugs MongoDB collection: the player's free-text
   notes plus the attached game snapshot (sans the bulky :log) and the last slice of the log, with
   turn/round/seed/phase pulled out for easy querying. Best-effort."
  [db request]
  (let [raw (read-body-string request)]
    (try
      (if (seq raw)
        (let [report (read-string raw)
              st     (:state report)]
          (db/insert!
           db :journeymen-bugs
           {:notes    (str (:notes report))
            :snapshot (pr-str st)
            :log      (pr-str (:log report))
            :turn     (:turn st)
            :round    (:round st)
            :seed     (:seed st)
            :phase    (name (or (:phase st) :setup))
            :created  (quot (System/currentTimeMillis) 1000)})
          (response/response (pr-str {:ok true})))
        (-> (response/response (pr-str {:ok false :error "empty report"}))
            (response/status 400)))
      (catch Exception e
        (-> (response/response (pr-str {:ok false :error (.getMessage e)}))
            (response/status 500))))))

(defn submit-beacon!
  "JS error beacon from the play page's window.onerror/unhandledrejection hooks.
   The raw JSON line is stored verbatim in :journeymen-bugs, flagged :beacon so
   real reports stay queryable."
  [db request]
  (let [raw (read-body-string request)]
    (try
      (if (seq raw)
        (do
          (db/insert!
           db :journeymen-bugs
           {:beacon  true
            :raw     (str/trim raw)
            :created (quot (System/currentTimeMillis) 1000)})
          (response/response (pr-str {:ok true})))
        (-> (response/response (pr-str {:ok false :error "empty beacon"}))
            (response/status 400)))
      (catch Exception e
        (-> (response/response (pr-str {:ok false :error (.getMessage e)}))
            (response/status 500))))))

;; ── routes ───────────────────────────────────────────────────────────────────

(def ^:private page-middleware
  [middleware/wrap-csrf
   middleware/wrap-formats])

(defn journeymen-routes
  [db]
  ["/journeymen"
   ["" {:middleware page-middleware
        :get play-e-page}]
   ["/play" {:middleware page-middleware
             :get play-e-page}]
   ["/rules" {:middleware page-middleware
              :get rules-e-page}]
   ;; ── persistence + telemetry (opaque client keys; outside wrap-csrf) ──
   ["/save/:id" {:post (partial save-game! db)}]
   ["/load/:id" {:get  (partial load-game db)}]
   ["/bug"      {:post (partial submit-bug! db)}]
   ["/beacon"   {:post (partial submit-beacon! db)}]])
