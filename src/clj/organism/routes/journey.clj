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

(defn- sundivers-low?
  "True when the player's total sundivers (board + habitat + reserve) are critically low."
  [state]
  (let [player    (game/current-player state)
        on-board  (on-board-count state player)
        habitat   (get-in state [:players player :habitat :sundivers] 0)
        reserve   (get-in state [:players player :reserve :sundivers] 0)
        total     (+ on-board habitat reserve)]
    (<= total 4)))

(defn- has-foundry-conversion?
  "True if a foundry conversion is currently available."
  [state]
  (some #(= :foundry (:type %))
        (game/find-conversions state (game/current-player state))))

(defn- has-tower-conversion? [state]
  (some #(= :tower (:type %))
        (game/find-conversions state (game/current-player state))))

(defn- tower-available-in-reserve? [state]
  (pos? (get-in state [:players (game/current-player state) :reserve :towers] 0)))

(defn- move-could-enable-tower?
  "True if any move choice leads to a state where a tower conversion becomes available."
  [state move-next]
  (when (and move-next (tower-available-in-reserve? state))
    (let [[_ move-cs] (choice/find-state-raw move-next)]
      (some (fn [[_k ns]] (when ns (enables-tower-conversion? state ns)))
            move-cs))))

(defn- beacon-positions
  "All board positions that currently have a beacon."
  [state]
  (filter #(get-in state [:board % :beacon]) (keys (:board state))))

(defn- near-landable-positions
  "Board positions with 4+ cipher matches where center color is active.
   These are the tiles closest to becoming landing sites."
  [state]
  (filter (fn [pos]
            (let [tile-color (get-in state [:board pos :color])]
              (and tile-color
                   (game/cipher-color-active? state [0 0] tile-color)
                   (>= (game/count-cipher-matches state pos) 4))))
          (keys (:board state))))

(defn- effective-ark-dist
  "Minimum distance the ark achieves to the nearest target by moving in dir.
   When the direct next position is unexplored, also considers wrap-target
   (since :choose-ark-advance will offer :wrap as an option)."
  [state ark dir targets]
  (let [direct-pos  (game/add-hex ark dir)
        direct-dist (apply min (map #(game/hex-distance direct-pos %) targets))]
    (if (game/get-tile state direct-pos)
      direct-dist
      (let [wrap-pos  (game/wrap-target state ark dir)
            wrap-dist (apply min (map #(game/hex-distance wrap-pos %) targets))]
        (min direct-dist wrap-dist)))))

(defn agent-step
  "Pick one goal-oriented choice and return [choice-key next-state].
   Uses find-state-raw to avoid stale .cljc auto-advance issues."
  [state]
  (let [[phase choices] (choice/find-state-raw state)]
    (when (seq choices)
      (let [next-s
            (case phase
              ;; Priority: activate → convert (but move first if it enables a tower) → move
              ;; Exception: when sundivers are low, prioritize foundry conversion over activate
              :choose-action-type
              (let [move-next   (:move choices)
                    ;; Use find-state-raw to peek at move choices without auto-advance
                    [_ move-cs] (when move-next (choice/find-state-raw move-next))
                    ;; Real move = has any hex pos (launch) or [:fly pos] keys (not just :done)
                    real-move?  (some #(and (vector? %) (or (number? (first %)) (= :fly (first %)))) (keys move-cs))
                    ;; If convert is available but no tower yet, check if moving first sets up a tower
                    has-tower?  (has-tower-conversion? state)
                    skip-convert-for-tower? (and (:convert choices)
                                                 (not has-tower?)
                                                 real-move?
                                                 (move-could-enable-tower? state move-next))
                    ;; When sundivers low, build a foundry ASAP
                    need-foundry? (and (sundivers-low? state)
                                       (:convert choices)
                                       (has-foundry-conversion? state))]
                (or (when need-foundry? (:convert choices))
                    (when (has-own-stations? state) (:activate choices))
                    (when (and (:convert choices) (not skip-convert-for-tower?))
                      (:convert choices))
                    (when real-move? move-next)
                    (:convert choices)
                    (first (vals choices))))

              ;; Prefer tower → matrix → foundry.
              ;; But if no beacons yet, prioritize matrix. If sundivers low, prioritize foundry.
              :choose-convert
              (let [of-type  (fn [t] (some #(when (= t (:type (key %))) (val %)) choices))
                    has-beacons? (some #(get-in state [:board % :beacon]) (keys (:board state)))
                    low?     (sundivers-low? state)]
                (cond
                  low?         (or (of-type :foundry) (of-type :tower) (of-type :matrix)
                                   (first (vals choices)))
                  has-beacons? (or (of-type :tower) (of-type :matrix) (of-type :foundry)
                                   (first (vals choices)))
                  :else        (or (of-type :matrix) (of-type :tower) (of-type :foundry)
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

              ;; Landing first; then beacons; then near-landable.
              ;; Prefer wrap when it gets closer to the priority target.
              :choose-ark-advance
              (let [landings  (game/available-landings state)
                    bs        (beacon-positions state)
                    near-land (near-landable-positions state)
                    targets   (cond (seq landings)  landings
                                    (seq bs)        bs
                                    (seq near-land) near-land
                                    :else           nil)]
                (if targets
                  (let [ark        (:ark state)
                        dir        (game/heading-direction state)
                        direct-pos (:heading-token state)
                        wrap-pos   (game/wrap-target state ark dir)
                        d-fn       (fn [p] (apply min (map #(game/hex-distance p %) targets)))
                        d-direct   (d-fn direct-pos)
                        d-wrap     (d-fn wrap-pos)]
                    (if (< d-wrap d-direct)
                      (or (:wrap choices) (:direct choices))
                      (:direct choices)))
                  (:direct choices)))
              :choose-flare-advance        (:direct choices)
              :choose-drift-flare-advance  (:direct choices)

              :draw-drift-card (:draw choices)

              ;; Choices are now hex positions. Pick closest to landings > beacons > near-landable.
              :choose-captain-drift
              (let [board     (:board state)
                    landings  (game/available-landings state)
                    near-land (near-landable-positions state)
                    beacons   (filter #(get-in board [% :beacon]) (keys board))
                    pos-keys  (filter #(and (vector? %) (= 2 (count %)) (number? (first %))) (keys choices))
                    closest   (fn [targets]
                                (when (and (seq targets) (seq pos-keys))
                                  (let [best (apply min-key
                                               (fn [p] (apply min (map #(game/hex-distance p %) targets)))
                                               pos-keys)]
                                    (get choices best))))]
                (or (closest landings)
                    (closest beacons)
                    (closest near-land)
                    (first (vals choices))))

              ;; Pick the first available station to activate
              :choose-activate-station
              (or (first (vals (dissoc choices :done)))
                  (:done choices))

              ;; Tower heading: choices are now hex positions.
              ;; Priority: landings > near-landable > beacons
              :choose-activate-tower-heading
              (let [board    (:board state)
                    landings (game/available-landings state)
                    near-land (near-landable-positions state)
                    beacons  (filter #(get-in board [% :beacon]) (keys board))
                    pos-keys (filter #(and (vector? %) (= 2 (count %)) (number? (first %))) (keys choices))
                    closest  (fn [targets]
                               (when (and (seq targets) (seq pos-keys))
                                 (let [best (apply min-key
                                              (fn [p] (apply min (map #(game/hex-distance p %) targets)))
                                              pos-keys)]
                                   (get choices best))))]
                (or (closest landings) (closest beacons) (closest near-land) (first (vals choices))))

              ;; Land whenever possible
              :choose-land (:land choices (:continue choices))

              ;; Cipher: center is REQUIRED for any matches to show. Place center first,
              ;; then balance between more center colors (landing) and outer (match points).
              ;; Heavily weight matches near the Ark (these contribute to landing).
              :cipher
              (let [{:keys [color]} (first (get-in state [:player-turn :cipher-queue] []))
                    board     (:board state)
                    ark       (:ark state)
                    ;; How many active colors are already at center?
                    center-n  (count (filter (fn [[_ ps]] (seq ps))
                                            (get-in state [:cipher [0 0] :colors] {})))
                    ;; Weight matches near the Ark much higher (3x for distance ≤ 2)
                    ark-weight (fn [tile-pos]
                                 (let [d (if ark (game/hex-distance tile-pos ark) 99)]
                                   (cond (<= d 1) 5 (<= d 2) 3 (<= d 3) 2 :else 1)))
                    score     (fn [pos]
                                (if (game/cipher-color-active? state pos color)
                                  0  ;; already active here, no new value
                                  (if (= pos [0 0])
                                    ;; Center: enables matches for all tiles of this color.
                                    ;; Count how many ACTUAL new matches would appear, weighted by Ark proximity
                                    (let [tiles-of-color (filter #(= color (:color (get board %))) (keys board))
                                          new-matches    (apply +
                                                          (for [tile-pos tiles-of-color
                                                                dir      game/hex-directions
                                                                :let [n-color (get-in board [(game/add-hex tile-pos dir) :color])]
                                                                :when (and n-color
                                                                           (game/cipher-color-active? state dir n-color))]
                                                            (ark-weight tile-pos)))]
                                      (cond
                                        (zero? center-n) (+ 9999 new-matches)  ;; MUST place center first
                                        (< center-n 3)   (+ (* (count tiles-of-color) 2) new-matches)
                                        :else            (max 1 (+ (quot (count tiles-of-color) 4) new-matches))))
                                    ;; Outer: count ACTUAL new matches that would appear, weighted by Ark proximity
                                    (let [actual-matches
                                          (apply +
                                           (for [tile-pos (keys board)
                                                 :let [tile-color (:color (get board tile-pos))
                                                       n-color    (get-in board [(game/add-hex tile-pos pos) :color])]
                                                 :when (and (= n-color color)
                                                            tile-color
                                                            (game/cipher-color-active? state [0 0] tile-color))]
                                             (ark-weight tile-pos)))]
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
