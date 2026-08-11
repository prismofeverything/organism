(ns organism.routes.organism
  (:require
   [clojure.string :as str]
   [organism.board :as board]
   [organism.game :as game]
   [organism.layout :as layout]
   [organism.leaderboard :as leaderboard]
   [organism.persist :as persist]
   [organism.middleware :as middleware]
   [organism.routes.organism-bot :as bot]
   [organism.routes.shared :as shared]
   [organism.routes.websockets :as ws]))

;; ── Learn page clips ─────────────────────────────────────────────────────

(def action-clips
  "The short silent loops on the learn page, in teaching order: the four things
   an element can do, then what happens between players, then how you win."
  [{:file "clip_eat.mp4"        :title "eat"
    :caption "take food into your organism"}
   {:file "clip_move.mp4"       :title "move"
    :caption "move a fed/mobile element to an open space"}
   {:file "clip_grow.mp4"       :title "grow"
    :caption "spend food to make a new element"}
   {:file "clip_circulate.mp4"  :title "circulate"
    :caption "send half the food from one element to another"}
   {:file "clip_conflict.mp4"   :title "conflict"
    :caption "elements of different players conflict"}
   {:file "clip_perish.mp4"     :title "perish"
    :caption "if an organism is missing any element type, it perishes"}
   {:file "clip_power.mp4"      :title "power"
    :caption "for each turn you held the center, and each element you unravel"}
   {:file "clip_two_org.mp4"    :title "two organisms"
    :caption "each organism you control gets its own turn"}
   {:file "clip_three_org.mp4"  :title "three organisms"
    :caption "if you ever have three living organisms at any time on your turn you win (!)"}])

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
   :load-player-stats leaderboard/player-stats
   :learn-params     {:clips action-clips}})

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
  ["oroboros" "helios" "selene" "atlas" "aurora"])

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
        ring-count 6
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
    ;; Run the bot in the background. Each turn the bot broadcasts the full
    ;; resulting state (like journey) so observers render it directly. We do
    ;; NOT send choice keys for client-side replay: an all-bot game streams
    ;; turns continuously, and any replay divergence sends the client's
    ;; find-next-choices into a spin that freezes the tab.
    (bot/run-bot-turns!
     ws/games game-key 200
     (fn [_choice-keys next-game]
       (let [channels (get-in @ws/games [:games game-key :channels])]
         (when (seq channels)
           (ws/send-channels! channels
                              {:type "game-state"
                               :game (:state next-game)}))))
     (fn [next-state]
       (persist/update-state! db game-key next-state))
     db)
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
