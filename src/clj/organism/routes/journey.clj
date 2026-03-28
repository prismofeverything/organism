(ns organism.routes.journey
  (:require
   [clojure.string :as str]
   [organism.layout :as layout]
   [organism.persist :as persist]
   [organism.persist-journey :as persist-j]
   [organism.middleware :as middleware]
   [organism.routes.journey-ws :as journey-ws]
   [ring.util.response :as response]
   [journey.game :as game]
   [journey.choice :as choice]))

(defn home-page
  [request]
  (layout/render
   request
   "journey/home.html"
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
     "journey/create.html"
     {:session-player player
      :preferences preferences})))

(defn play-list-page
  "Show the logged-in player's games for journey."
  [db request]
  (let [player (get-in request [:session :player])
        player-games (persist/load-player-games db player "journey")]
    (layout/render
     request
     "journey/games.html"
     {:session-player player
      :player-games (pr-str player-games)})))

(defn play-page
  [db request]
  (let [play-key (-> request :path-params :play)
        player-key (get-in request [:session :player])
        preferences (persist/find-player-preferences db player-key)]
    (layout/render
     request
     "journey/play.html"
     {:player player-key
      :play play-key
      :preferences preferences})))

(defn observe-page
  [db request]
  (let [player (get-in request [:session :player])
        games (persist-j/load-observe-games db)]
    (layout/render
     request
     "journey/observe.html"
     {:session-player player
      :observe-games (pr-str games)})))

;; ── Smart playing agent ──────────────────────────────────────────────────────

(defn- player-idx
  "Turn-order index of current player — used for deterministic per-player variety."
  [state]
  (let [order (vec (:turn-order state))]
    (.indexOf order (game/current-player state))))

(defn- on-board-count
  "How many of player's sundivers are currently on board tiles."
  [state player]
  (apply + (map #(get-in state [:board % :sundivers player] 0)
                (keys (:board state)))))

(defn- enables-conversion?
  "True if moving to this next-state opens at least one conversion for current player."
  [state next-s]
  (seq (game/find-conversions next-s (game/current-player state))))

(defn- enables-matrix-conversion?
  "True if moving to this next-state opens a matrix conversion for current player."
  [state next-s]
  (some #(= :matrix (:type %))
        (game/find-conversions next-s (game/current-player state))))

(defn- enables-tower-conversion?
  "True if moving to this next-state opens a tower conversion for current player."
  [state next-s]
  (some #(= :tower (:type %))
        (game/find-conversions next-s (game/current-player state))))

(defn- pick-varied
  "Pick from a seq of candidate values, varying by player index for spread."
  [state candidates]
  (when (seq candidates)
    (nth (vec candidates) (mod (player-idx state) (count candidates)))))

(defn- has-own-stations? [state]
  (seq (get-in state [:players (game/current-player state) :stations])))

(defn- beacon-positions
  "All board positions that currently have a beacon."
  [state]
  (filter #(get-in state [:board % :beacon]) (keys (:board state))))

(defn- effective-ark-dist
  "Minimum distance the ark achieves to the nearest beacon by moving in dir.
   When the direct next position is unexplored, also considers wrap-target
   (since :choose-ark-advance will offer :wrap as an option)."
  [state ark dir beacons]
  (let [direct-pos  (game/add-hex ark dir)
        direct-dist (apply min (map #(game/hex-distance direct-pos %) beacons))]
    (if (game/get-tile state direct-pos)
      direct-dist
      (let [wrap-pos  (game/wrap-target state ark dir)
            wrap-dist (apply min (map #(game/hex-distance wrap-pos %) beacons))]
        (min direct-dist wrap-dist)))))

(defn agent-step
  "Pick one goal-oriented choice and return [choice-key next-state].
   Uses find-state-raw to avoid stale .cljc auto-advance issues."
  [state]
  (let [[phase choices] (choice/find-state-raw state)]
    (when (seq choices)
      (let [next-s
            (case phase
              ;; Priority: activate own stations → convert → move with real choices
              ;; Move choices now have inlined hex positions and [:fly pos] keys instead of :launch/:fly
              :choose-action-type
              (let [move-next   (:move choices)
                    ;; Use find-state-raw to peek at move choices without auto-advance
                    [_ move-cs] (when move-next (choice/find-state-raw move-next))
                    ;; Real move = has any hex pos (launch) or [:fly pos] keys (not just :done)
                    real-move?  (some #(and (vector? %) (or (number? (first %)) (= :fly (first %)))) (keys move-cs))]
                (or (when (has-own-stations? state) (:activate choices))
                    (:convert choices)
                    (when real-move? move-next)
                    (first (vals choices))))

              ;; Prefer tower → matrix → foundry.
              ;; But if no beacons on the board yet, prioritize matrix to place beacons.
              :choose-convert
              (let [of-type  (fn [t] (some #(when (= t (:type (key %))) (val %)) choices))
                    has-beacons? (some #(get-in state [:board % :beacon]) (keys (:board state)))]
                (if has-beacons?
                  (or (of-type :tower) (of-type :matrix) (of-type :foundry)
                      (first (vals choices)))
                  (or (of-type :matrix) (of-type :tower) (of-type :foundry)
                      (first (vals choices)))))

              ;; choose-move now has inlined launch [q r] / [:wrap ...] / [:fly pos] / :done
              ;; Prefer fly when 3+ on board; otherwise launch (any hex pos) to build presence.
              :choose-move
              (let [player    (game/current-player state)
                    fly-keys  (filter #(and (vector? %) (= :fly (first %))) (keys choices))
                    pos-keys  (filter #(and (vector? %) (number? (first %))) (keys choices))
                    prefer-fly? (>= (on-board-count state player) 3)]
                (or (when (and prefer-fly? (seq fly-keys))
                      (let [best (apply max-key
                                   #(get-in state [:board (second %) :sundivers player] 0)
                                   fly-keys)]
                        (get choices best)))
                    (when (seq pos-keys)
                      (let [fewest (apply min-key
                                     #(get-in state [:board % :sundivers player] 0)
                                     pos-keys)]
                        (get choices fewest)))
                    (when (seq fly-keys) (get choices (first fly-keys)))
                    (:done choices)
                    (first (vals choices))))

              ;; Launch: prefer conversion-enabling tiles, then spread out
              ;; (fewest of this player's sundivers), then non-wrap, wrap as last resort.
              :choose-launch-destination
              (let [player   (game/current-player state)
                    non-wrap (into {} (remove #(= :wrap (first (key %))) choices))
                    no-beacons? (empty? (beacon-positions state))
                    ;; No beacons: prioritize matrix patterns. With beacons: prioritize tower patterns.
                    conv     (if no-beacons?
                               (or (some #(when (enables-matrix-conversion? state (val %)) (val %)) non-wrap)
                                   (some #(when (enables-conversion? state (val %)) (val %)) non-wrap))
                               (or (some #(when (enables-tower-conversion? state (val %)) (val %)) non-wrap)
                                   (some #(when (enables-conversion? state (val %)) (val %)) non-wrap)))
                    fewest   (when (and (not conv) (seq non-wrap))
                               (let [k (apply min-key
                                         #(get-in state [:board % :sundivers player] 0)
                                         (keys non-wrap))]
                                 (get non-wrap k)))]
                (or conv fewest (pick-varied state (vals non-wrap)) (pick-varied state (vals choices))))

              ;; Fly from the position with most sundivers (spread them out)
              :choose-fly-from
              (let [player (game/current-player state)
                    best   (apply max-key #(get-in state [:board % :sundivers player] 0)
                                  (keys choices))]
                (get choices best))

              ;; Fly to: prefer conversion-enabling tiles, then spread out, then non-wrap.
              ;; Avoid ALL positions visited this turn (not just immediate fly-from).
              :choose-fly-to
              (let [player   (game/current-player state)
                    visited  (get-in state [:player-turn :action :bot-fly-visited] #{})
                    non-wrap (into {} (remove #(= :wrap (first (key %))) choices))
                    ;; Exclude all positions visited this turn
                    non-visited (into {} (remove #(contains? visited (key %)) non-wrap))
                    targets  (if (seq non-visited) non-visited non-wrap)
                    no-beacons? (empty? (beacon-positions state))
                    conv     (if no-beacons?
                               (or (some #(when (enables-matrix-conversion? state (val %)) (val %)) targets)
                                   (some #(when (enables-conversion? state (val %)) (val %)) targets))
                               (or (some #(when (enables-tower-conversion? state (val %)) (val %)) targets)
                                   (some #(when (enables-conversion? state (val %)) (val %)) targets)))
                    fewest   (when (and (not conv) (seq targets))
                               (let [k (apply min-key
                                         #(get-in state [:board % :sundivers player] 0)
                                         (keys targets))]
                                 (get targets k)))]
                (or conv fewest (pick-varied state (vals targets)) (pick-varied state (vals choices))))

              ;; Take max bonus actions to fully utilise stations
              :choose-activate-self-bonus  (get choices (apply max (keys choices)))
              :choose-activate-owner-bonus (get choices (apply max (keys choices)))
              :choose-activate-tower-join  (:join choices (:skip choices))
              :choose-activate-tower-spend (first (vals choices))
              :choose-activate-tower-join-spend (first (vals choices))
              :choose-activate-matrix-spend (first (vals choices))

              :flare-beacon-join       (:join choices (:skip choices))
              :flare-beacon-join-spend (first (vals choices))
              :captain-beacon-join     (:join choices (:skip choices))
              :captain-beacon-join-spend (first (vals choices))
              :cipher-spend            (first (vals choices))

              ;; Auto-advance trivial phases
              :draw-cards       (first (vals choices))
              :keep-card        (or (:continue choices) (first (vals (dissoc choices :keep-held)))
                                    (first (vals choices)))

              ;; Landing first; then prefer wrap if it gets ark closer to a beacon.
              :choose-ark-advance
              (let [landings (game/available-landings state)]
                (if (seq landings)
                  (let [ark        (:ark state)
                        dir        (game/heading-direction state)
                        direct-pos (:heading-token state)
                        wrap-pos   (game/wrap-target state ark dir)
                        d-fn       (fn [p] (apply min (map #(game/hex-distance p %) landings)))
                        d-direct   (d-fn direct-pos)
                        d-wrap     (d-fn wrap-pos)]
                    (if (< d-wrap d-direct)
                      (or (:wrap choices) (:direct choices))
                      (:direct choices)))
                  (let [bs (beacon-positions state)]
                    (if (seq bs)
                      (let [ark        (:ark state)
                            dir        (game/heading-direction state)
                            direct-pos (:heading-token state)
                            wrap-pos   (game/wrap-target state ark dir)
                            d-direct   (apply min (map #(game/hex-distance direct-pos %) bs))
                            d-wrap     (apply min (map #(game/hex-distance wrap-pos   %) bs))]
                        (if (< d-wrap d-direct)
                          (or (:wrap choices) (:direct choices))
                          (:direct choices)))
                      (:direct choices)))))
              :choose-flare-advance        (:direct choices)
              :choose-drift-flare-advance  (:direct choices)

              :draw-drift-card (:draw choices)

              ;; Choices are now hex positions. Pick the one closest to landings, then beacons.
              :choose-captain-drift
              (let [board    (:board state)
                    landings (game/available-landings state)
                    beacons  (filter #(get-in board [% :beacon]) (keys board))
                    pos-keys (filter #(and (vector? %) (= 2 (count %)) (number? (first %))) (keys choices))
                    closest  (fn [targets]
                               (when (and (seq targets) (seq pos-keys))
                                 (let [best (apply min-key
                                              (fn [p] (apply min (map #(game/hex-distance p %) targets)))
                                              pos-keys)]
                                   (get choices best))))]
                (or (closest landings)
                    (closest beacons)
                    (first (vals choices))))

              ;; Pick the first available station to activate
              :choose-activate-station
              (or (first (vals (dissoc choices :done)))
                  (:done choices))

              ;; Tower heading: choices are now hex positions. Pick closest to beacons.
              :choose-activate-tower-heading
              (let [board    (:board state)
                    beacons  (filter #(get-in board [% :beacon]) (keys board))
                    pos-keys (filter #(and (vector? %) (= 2 (count %)) (number? (first %))) (keys choices))
                    closest  (fn [targets]
                               (when (and (seq targets) (seq pos-keys))
                                 (let [best (apply min-key
                                              (fn [p] (apply min (map #(game/hex-distance p %) targets)))
                                              pos-keys)]
                                   (get choices best))))]
                (or (closest beacons) (first (vals choices))))

              ;; Land whenever possible
              :choose-land (:land choices (:continue choices))

              ;; Cipher: center is REQUIRED for any matches to show. Place center first,
              ;; then balance between more center colors (landing) and outer (match points).
              :cipher
              (let [{:keys [color]} (first (get-in state [:player-turn :cipher-queue] []))
                    board     (:board state)
                    ;; How many active colors are already at center?
                    center-n  (count (filter (fn [[_ ps]] (seq ps))
                                            (get-in state [:cipher [0 0] :colors] {})))
                    score     (fn [pos]
                                (if (game/cipher-color-active? state pos color)
                                  0  ;; already active here, no new value
                                  (if (= pos [0 0])
                                    ;; Center: enables matches for all tiles of this color.
                                    ;; Count how many ACTUAL new matches would appear
                                    ;; (tiles of this color that have neighbors active at outer dirs)
                                    (let [tiles-of-color (filter #(= color (:color (get board %))) (keys board))
                                          new-matches    (count
                                                          (for [tile-pos tiles-of-color
                                                                dir      game/hex-directions
                                                                :let [n-color (get-in board [(game/add-hex tile-pos dir) :color])]
                                                                :when (and n-color
                                                                           (game/cipher-color-active? state dir n-color))]
                                                            1))]
                                      (cond
                                        (zero? center-n) (+ 9999 new-matches)  ;; MUST place center first
                                        (< center-n 3)   (+ (* (count tiles-of-color) 2) new-matches)
                                        :else            (max 1 (+ (quot (count tiles-of-color) 4) new-matches))))
                                    ;; Outer: count ACTUAL new matches that would appear
                                    ;; A match appears when: tile-color active at center AND neighbor-color active at dir
                                    (let [actual-matches
                                          (count
                                           (for [tile-pos (keys board)
                                                 :let [tile-color (:color (get board tile-pos))
                                                       n-color    (get-in board [(game/add-hex tile-pos pos) :color])]
                                                 :when (and (= n-color color)
                                                            tile-color
                                                            (game/cipher-color-active? state [0 0] tile-color))]
                                             1))]
                                      (if (zero? center-n)
                                        (max 1 actual-matches)
                                        (* (max 1 actual-matches) 3))))))
                    placeable (dissoc choices :skip)]
                (if (and color (seq placeable))
                  (get choices (apply max-key score (keys placeable)))
                  (or (:skip choices) (first (vals choices)))))

              ;; Matrix beacon: prefer tile on the ark's heading path, else no-station tile
              :choose-activate-matrix-beacon
              (let [ahead    (game/add-hex (:ark state) (game/heading-direction state))
                    no-sta   (remove #(get-in state [:board % :station]) (keys choices))
                    on-path  (filter #{ahead} no-sta)]
                (get choices (or (first on-path) (first no-sta) (first (keys choices)))))

              (first (vals choices)))]
        (when next-s
          (let [ck (some (fn [[k v]] (when (= v next-s) k)) choices)]
            [ck next-s]))))))

;; ── Generate page — creates an all-bot game and redirects to play page ──────

(def generate-bot-names
  "Fixed bot names for generated games. In the future, each can be a different trained model."
  ["oroboros" "helios" "selene" "atlas" "aurora"])

(def ^:private generate-words
  ["solar" "lunar" "stellar" "cosmic" "astral" "void" "nebula" "nova"
   "drift" "pulse" "ember" "spark" "flame" "frost" "tide" "storm"
   "crystal" "prism" "cipher" "rune" "glyph" "sigil" "nexus" "apex"
   "echo" "arc" "flux" "bloom" "shade" "gleam" "veil" "haze"])

(defn- generate-game-name []
  (let [words (repeatedly 3 #(rand-nth generate-words))]
    (str "generate-" (str/join "-" words))))

(defn generate-page
  "Create an all-bot game with fast turns and render the play page directly.
   Each reload creates a fresh game."
  [db request]
  (let [players  generate-bot-names
        bot-set  (set players)
        game-key (generate-game-name)
        state    (game/initial-state players)
        player-key (get-in request [:session :player])]
    (swap! journey-ws/games
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
    (persist-j/save-game! db game-key state bot-set (vec players) state)
    (journey-ws/run-bot-turns! db game-key)
    ;; Render the play page template directly with the generated key
    (layout/render
     request
     "journey/play.html"
     {:player player-key
      :play game-key
      :preferences "{}"})))

(defn create-game!
  "POST handler: create a new game in the games atom, persist to DB, and return the play key."
  [db request]
  (let [params    (or (:body-params request) (:params request))
        play-name (get params :play-name (get params "play-name"))
        players   (get params :players (get params "players"))
        bots      (get params :bots (get params "bots" []))]
    (if (and (seq play-name) (seq players))
      (let [state   (game/initial-state (vec players))
            bot-set (set bots)]
        (swap! journey-ws/games
               assoc-in [:games play-name]
               {:key           play-name
                :state         state
                :initial-state state
                :history       []
                :bots          bot-set
                :players       (vec players)
                :chat          []
                :channels      #{}})
        (persist-j/save-game! db play-name state bot-set (vec players) state)
        (when (contains? bot-set (game/current-player state))
          (journey-ws/run-bot-turns! db play-name))
        (response/response {:play-key play-name}))
      (response/bad-request {:error "play-name and players required"}))))

(defn journey-routes
  [db]
  ["/journey"
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
