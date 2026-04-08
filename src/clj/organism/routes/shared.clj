(ns organism.routes.shared
  "Shared route helpers for game pages — create, observe, players, learn.
   Games implement their own page handlers but can delegate the
   routine data-loading and rendering to these helpers."
  (:require
   [clojure.string :as str]
   [organism.bots :as bots]
   [organism.layout :as layout]
   [organism.persist :as persist]
   [ring.util.response :as response]))

;; ── Auth helper ──────────────────────────────────────────────────────────

(defn require-auth
  "Redirect to login if the user isn't authenticated."
  [handler]
  (fn [request]
    (if (get-in request [:session :player])
      (handler request)
      (response/redirect (str "/login?redirect=" (:uri request))))))

;; ── Common data loaders ──────────────────────────────────────────────────

(defn load-open-games-for
  "Load open games filtered by game-type."
  [db game-type]
  (filter #(= game-type (:game-type (:invocation %)))
          (persist/load-open-games db)))

;; ── Page handlers ─────────────────────────────────────────────────────────

(defn observe-page
  "Render an observe page using the game-spec."
  [{:keys [template-prefix load-observe] :as _spec} db request]
  (let [player (get-in request [:session :player])
        games (when load-observe (load-observe db))]
    (layout/render
     request
     (str template-prefix "/observe.html")
     {:session-player player
      :observe-games (pr-str (or games []))})))

(defn players-page
  "Render a players/stats page using the game-spec."
  [{:keys [template-prefix load-player-stats] :as _spec} db request]
  (let [player (get-in request [:session :player])
        stats (when load-player-stats (load-player-stats db))]
    (layout/render
     request
     (str template-prefix "/players.html")
     {:session-player player
      :player-stats (pr-str (or stats []))})))

(defn learn-page
  "Render a learn page using the game-spec."
  [{:keys [template-prefix] :as _spec} request]
  (let [player (get-in request [:session :player])]
    (layout/render
     request
     (str template-prefix "/learn.html")
     {:session-player player})))

;; ── Shared API: player search ────────────────────────────────────────────

(defn search-players
  "Prefix-search player names + bots for the given game-type.
   Query params: q (prefix), game-type (e.g. 'organism').
   Returns {:players [{:name :bot? :description}, ...]}."
  [db request]
  (let [q (or (get-in request [:params :q]) "")
        game-type (or (get-in request [:params :game-type]) "")
        players (persist/load-players db)
        human-names (keep :key players)
        bot-list (when-not (str/blank? game-type) (bots/list-bots game-type))
        starts-with (fn [s]
                      (or (str/blank? q)
                          (str/starts-with? (str/lower-case (str s))
                                            (str/lower-case q))))
        ;; Bots sort first so they show prominently in the dropdown
        bot-results (->> bot-list
                         (filter #(starts-with (:name %)))
                         (map (fn [b] {:name (:name b)
                                       :bot? true
                                       :description (:description b)})))
        human-results (->> human-names
                           (filter starts-with)
                           (map (fn [n] {:name n :bot? false :description ""})))]
    (response/response
     {:players (vec (take 10 (concat bot-results human-results)))})))
