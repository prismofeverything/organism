(ns organism.routes.shared
  "Shared route helpers for game pages — create, observe, players, learn.
   Games implement their own page handlers but can delegate the
   routine data-loading and rendering to these helpers."
  (:require
   [clojure.string :as str]
   [organism.bots :as bots]
   [organism.layout :as layout]
   [organism.persist :as persist]
   [organism.persist-journey-bots :as bots-db]
   [ring.util.response :as response]))

;; ── Auth helper ──────────────────────────────────────────────────────────

(defn require-auth
  "Redirect to login if the user isn't authenticated."
  [handler]
  (fn [request]
    (if (get-in request [:session :player])
      (handler request)
      (response/redirect (str "/login?redirect=" (:uri request))))))

;; ── Create handler ─────────────────────────────────────────────────────────

(defn create-game!
  "Generic POST create handler shared by player-list games (future/journey/…).

   Spec keys:
     :games-atom  — the game's ws `games` atom (required)
     :make-state  — (fn [players params] → initial game-state) (required)
     :max-players — cap on player count (optional)
     :persist!    — (fn [db play-name players bot-set state]) side effect (optional)
     :after!      — (fn [db play-name state]) e.g. kick off bot turns (optional)

   Reads :play-name/:players/:bots from the request, validates, stores a
   standard game record in the atom, persists, fires the after-hook, and
   responds with {:play-key play-name}."
  [{:keys [games-atom make-state max-players persist! after!]} db request]
  (let [params    (or (:body-params request) (:params request))
        play-name (get params :play-name (get params "play-name"))
        players   (vec (get params :players (get params "players")))
        bots      (get params :bots (get params "bots" []))
        players   (if max-players (vec (take max-players players)) players)]
    (if (and (seq play-name) (seq players))
      (let [state   (make-state players params)
            bot-set (set bots)]
        (swap! games-atom assoc-in [:games play-name]
               {:key           play-name
                :state         state
                :initial-state state
                :history       []
                :bots          bot-set
                :players       players
                :chat          []
                :channels      #{}})
        (when persist! (persist! db play-name players bot-set state))
        (when after!   (after! db play-name state))
        (response/response {:play-key play-name}))
      (response/bad-request {:error "play-name and players required"}))))

;; ── Deletion ─────────────────────────────────────────────────────────────
;;
;; See the workflow described in organism.persist. These two handlers are the
;; whole of it: delete decides between removing the game now and marking it,
;; keep is the objection that cancels a mark.

(defn- participant?
  "Only somebody with a stake in the game gets to touch its deletion state —
   whoever created it, or anyone on the roster."
  [record player]
  (boolean
   (and player
        (or (= player (:created-by record))
            (contains? (set (get-in record [:invocation :players])) player)))))

(defn- humans-in
  "The human names on a roster. Bots come from the game's stored :bots set, the
   shared registry, and the \"generate-\" games that predate :bots."
  [game-type record]
  (let [bot-set (set (:bots record))]
    (->> (get-in record [:invocation :players])
         (remove str/blank?)
         (remove bot-set)
         (remove #(bots/bot? game-type %))
         distinct)))

(defn- nothing-at-stake?
  "True when removing the game takes nothing from anyone else: no other human
   on the roster, or not a single turn played yet. Those go immediately —
   there is nobody who could object, so there is nothing to wait for."
  [db game-type record player]
  (or (empty? (remove #{player} (humans-in game-type record)))
      (<= (persist/game-history-count db (:key record)) 1)))

(defn delete-game!
  "POST handler. Deletes outright when nothing is at stake, otherwise marks the
   game and lets the grace period run.

   `:on-delete` on the spec is called with the game key after a real deletion,
   so the game's ws layer can drop it and tell any open tabs."
  [{:keys [game-type on-delete] :as _spec} db request]
  (let [player (get-in request [:session :player])
        game-key (-> request :path-params :play)
        record (persist/find-game-record db game-key)]
    (cond
      (nil? record)
      (response/not-found {:error "no such game"})

      (not (participant? record player))
      (response/bad-request {:error "not your game"})

      (nothing-at-stake? db game-type record player)
      (do
        (persist/delete-game! db game-key)
        (when on-delete (on-delete game-key))
        (response/response {:deleted game-key}))

      :else
      (response/response
       {:marked game-key
        :deletion (persist/mark-game-for-deletion! db game-key player)}))))

(defn keep-game!
  "POST handler — the objection. Any participant can cancel a pending deletion,
   which is what keeps the player who marked it from stalling one through."
  [db request]
  (let [player (get-in request [:session :player])
        game-key (-> request :path-params :play)
        record (persist/find-game-record db game-key)]
    (cond
      (nil? record)
      (response/not-found {:error "no such game"})

      (not (participant? record player))
      (response/bad-request {:error "not your game"})

      :else
      (do
        (persist/unmark-game-for-deletion! db game-key)
        (response/response {:kept game-key})))))

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
  "Render a learn page using the game-spec.

   `:learn-params` on the spec is merged into the template context, so a game
   can hand its learn template whatever it needs (organism passes the action
   clips) without this handler knowing anything about it."
  [{:keys [template-prefix learn-params] :as _spec} request]
  (let [player (get-in request [:session :player])]
    (layout/render
     request
     (str template-prefix "/learn.html")
     (merge {:session-player player} learn-params))))

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
        ;; Built-in (hard-coded) bots from the registry
        registry-bots (when-not (str/blank? game-type) (bots/list-bots game-type))
        ;; Player-created flowchart bots from the database
        db-bots (when-not (str/blank? game-type)
                  (map (fn [b] {:name (:name b) :description (or (:description b) "")})
                       (bots-db/list-bots-for-game db game-type)))
        ;; Merge, dedup by name (DB bots override registry if same name)
        all-bots (vals (merge (into {} (map (juxt :name identity) registry-bots))
                              (into {} (map (juxt :name identity) db-bots))))
        starts-with (fn [s]
                      (or (str/blank? q)
                          (str/starts-with? (str/lower-case (str s))
                                            (str/lower-case q))))
        bot-results (->> all-bots
                         (filter #(starts-with (:name %)))
                         (map (fn [b] {:name (:name b)
                                       :bot? true
                                       :description (:description b)})))
        human-results (->> human-names
                           (filter starts-with)
                           (map (fn [n] {:name n :bot? false :description ""})))]
    (response/response
     {:players (vec (take 10 (concat bot-results human-results)))})))
