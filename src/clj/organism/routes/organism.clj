(ns organism.routes.organism
  (:require
   [clojure.string :as str]
   [organism.board :as board]
   [organism.game :as game]
   [organism.layout :as layout]
   [organism.persist :as persist]
   [organism.middleware :as middleware]
   [organism.routes.organism-bot :as bot]
   [organism.routes.shared :as shared]
   [organism.routes.websockets :as ws]))

;; ── Game spec ────────────────────────────────────────────────────────────

(def organism-spec
  {:game-type        "organism"
   :title            "ORGANISM"
   :template-prefix  "organism"
   :home-path        "/organism"
   :create-path      "/organism/create"
   :play-path        "/organism/play"
   :ws-prefix        "/ws/organism/play/"
   :load-observe     persist/load-observe-games
   :load-player-stats persist/load-player-stats})

;; ── Page handlers ─────────────────────────────────────────────────────────

(defn play-list-page
  "Show the logged-in player's games — reuses the player page."
  [db request]
  (let [player (get-in request [:session :player])
        preferences (persist/find-player-preferences db player)
        player-games (persist/load-player-games db player "organism")]
    (layout/render
     request
     "organism/player.html"
     {:player player
      :preferences preferences
      :player-games (pr-str player-games)})))

(defn create-page
  [db request]
  (let [player (get-in request [:session :player])
        play-key (-> request :path-params :play)
        preferences (persist/find-player-preferences db player)
        open-game (when play-key (persist/find-open-game db play-key))]
    (layout/render
     request
     "organism/create.html"
     {:session-player player
      :preferences preferences
      :play-key (or play-key "")
      :open-invocation (if open-game (pr-str (:invocation open-game)) "")})))

(defn play-page
  [db request]
  (let [play-key (-> request :path-params :play)
        player-key (get-in request [:session :player])
        preferences (persist/find-player-preferences db player-key)]
    (layout/render
     request
     "organism/play.html"
     {:player player-key
      :play play-key
      :preferences preferences})))

(defn player-page
  [db request]
  (let [player-key (-> request :path-params :player)
        preferences (persist/find-player-preferences db player-key)
        player-games (persist/load-player-games db player-key "organism")]
    (layout/render
     request
     "organism/player.html"
     {:player player-key
      :preferences preferences
      :player-games (pr-str player-games)})))

;; ── Generate page (all-bot game) ─────────────────────────────────────────

(def ^:private generate-bot-names
  ["oroboros" "helios" "selene" "atlas"])

(def ^:private generate-words
  ["solar" "lunar" "stellar" "cosmic" "astral" "void" "nebula" "nova"
   "drift" "pulse" "ember" "spark" "flame" "frost" "tide" "storm"
   "crystal" "prism" "cipher" "rune" "glyph" "sigil" "nexus" "apex"])

(defn- generate-game-name []
  (str "generate-" (str/join "-" (repeatedly 3 #(rand-nth generate-words)))))

(defn- make-generate-invocation
  "Build an invocation suitable for an all-bot generated game."
  [players]
  (let [n (count players)
        ring-count 4
        colors (board/generate-colors-buffer board/total-rings ring-count n)
        captures (vec (repeat n board/default-player-captures))]
    {:ring-count ring-count
     :player-count n
     :players (vec players)
     :player-captures captures
     :organism-victory 3
     :mutations {}
     :colors colors
     :description "auto-generated bot game"
     :game-type "organism"}))

(defn generate-page
  "Create an all-bot organism game, render the play page immediately, and
   start the bot turns in the background so observers can watch it unfold."
  [db request]
  (let [players generate-bot-names
        bot-set (set players)
        game-key (generate-game-name)
        player-key (get-in request [:session :player])
        invocation (make-generate-invocation players)
        base-state {:key game-key
                    :invocation invocation
                    :game nil
                    :chat []
                    :history []
                    :channels #{}}
        complete (ws/complete-game-state base-state)
        initial-state (assoc complete :bots bot-set)]
    ;; Persist initial game so the play page can load it
    (swap! ws/games assoc-in [:games game-key] initial-state)
    (persist/create-game! db (assoc (dissoc initial-state :channels)
                                    :created-by (or player-key "system")
                                    :game-type "organism"))
    ;; Run the bot in the background. Each turn the bot sends the list of
    ;; choice keys it picked, and the client replays them via find-state.
    (bot/run-bot-turns!
     ws/games game-key 200
     (fn [choice-keys _next-game]
       (let [channels (get-in @ws/games [:games game-key :channels])]
         (when (seq channels)
           (ws/send-channels! channels
                              {:type "bot-choices"
                               :choices choice-keys}))))
     (fn [next-state]
       (persist/update-state! db game-key next-state)))
    (layout/render
     request
     "organism/play.html"
     {:player (or player-key "")
      :play game-key
      :preferences "{}"})))

;; ── Routes ────────────────────────────────────────────────────────────────

(defn organism-routes
  [db]
  ["/organism"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/create"        {:get (partial create-page db)
                      :middleware [shared/require-auth]}]
   ["/create/:play"  {:get (partial create-page db)
                      :middleware [shared/require-auth]}]
   ["/create/:play/" {:get (partial create-page db)
                      :middleware [shared/require-auth]}]
   ["/play"          {:get (partial play-list-page db)
                      :middleware [shared/require-auth]}]
   ["/play/:play"    {:get (partial play-page db)}]
   ["/play/:play/"   {:get (partial play-page db)}]
   ["/observe"       {:get (partial shared/observe-page organism-spec db)}]
   ["/players"       {:get (partial shared/players-page organism-spec db)}]
   ["/learn"         {:get (partial shared/learn-page organism-spec)}]
   ["/generate"      {:get (partial generate-page db)}]
   ["/player/:player"  {:get (partial player-page db)}]
   ["/player/:player/" {:get (partial player-page db)}]])
