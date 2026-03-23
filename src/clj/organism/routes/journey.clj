(ns organism.routes.journey
  (:require
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
        games (persist/load-observe-games db)]
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
  "Pick one goal-oriented choice and return [choice-key next-state]."
  [state]
  (let [[phase choices] (choice/find-state state)]
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

              ;; Prefer tower → matrix → foundry
              :choose-convert
              (let [of-type (fn [t] (some #(when (= t (:type (key %))) (val %)) choices))]
                (or (of-type :tower) (of-type :matrix) (of-type :foundry)
                    (first (vals choices))))

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
                    conv     (some #(when (enables-conversion? state (val %)) (val %)) non-wrap)
                    ;; Prefer tile where this player has fewest sundivers (spread out)
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
              :choose-fly-to
              (let [player   (game/current-player state)
                    non-wrap (into {} (remove #(= :wrap (first (key %))) choices))
                    conv     (some #(when (enables-conversion? state (val %)) (val %)) non-wrap)
                    fewest   (when (and (not conv) (seq non-wrap))
                               (let [k (apply min-key
                                         #(get-in state [:board % :sundivers player] 0)
                                         (keys non-wrap))]
                                 (get non-wrap k)))]
                (or conv fewest (pick-varied state (vals non-wrap)) (pick-varied state (vals choices))))

              ;; Take max bonus actions to fully utilise stations
              :choose-activate-self-bonus  (get choices (apply max (keys choices)))
              :choose-activate-owner-bonus (get choices (apply max (keys choices)))
              :choose-activate-tower-join  (:join choices (:skip choices))
              :choose-activate-tower-spend (first (vals choices))

              :flare-beacon-join   (:join choices (:skip choices))
              :captain-beacon-join (:join choices (:skip choices))

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

              ;; Priority 0: steer toward a landable tile (if any exist).
              ;; Priority 1: steer toward the nearest beacon (shortest hex path).
              ;; Priority 2: wrap back over explored space (tiles in back half).
              ;; Priority 3: vary by player index (spread the arcs apart).
              :choose-captain-drift
              (let [ark      (:ark state)
                    dir      (game/heading-direction state)
                    idx      (game/direction-index dir)
                    board    (:board state)
                    landings (game/available-landings state)
                    beacons  (filter #(get-in board [% :beacon]) (keys board))

                    ;; Turn that gets ark closest to any landable tile (highest priority)
                    landing-turn
                    (when (seq landings)
                      (let [d-none  (effective-ark-dist state ark dir                   landings)
                            d-left  (effective-ark-dist state ark (game/rotate-ccw dir) landings)
                            d-right (effective-ark-dist state ark (game/rotate-cw  dir) landings)
                            best-d  (min d-none d-left d-right)]
                        (when-not (= d-none d-left d-right)
                          (cond
                            (= best-d d-none)  (:none  choices)
                            (= best-d d-left)  (or (:left  choices) (:none choices))
                            :else              (or (:right choices) (:none choices))))))

                    ;; Turn option that gets the ark closest to the nearest beacon,
                    ;; accounting for wrap as an option when the next cell is unexplored.
                    beacon-turn
                    (when (seq beacons)
                      (let [d-none  (effective-ark-dist state ark dir                     beacons)
                            d-left  (effective-ark-dist state ark (game/rotate-ccw dir)   beacons)
                            d-right (effective-ark-dist state ark (game/rotate-cw  dir)   beacons)
                            best-d  (min d-none d-left d-right)]
                        (when-not (= d-none d-left d-right)
                          (cond
                            (= best-d d-none)  (:none  choices)
                            (= best-d d-left)  (or (:left  choices) (:none choices))
                            :else              (or (:right choices) (:none choices))))))

                    ;; Back-half tile check (fallback when no beacons)
                    at       (fn [o] (contains? board (game/add-hex ark (nth game/hex-directions (mod (+ idx o) 6)))))
                    cw-back  (at 2)
                    behind   (at 3)
                    ccw-back (at 4)]

                (or landing-turn
                    beacon-turn
                    (cond
                      (and cw-back (not ccw-back)) (or (:right choices) (:none choices))
                      (and ccw-back (not cw-back)) (or (:left  choices) (:none choices))
                      (or cw-back behind ccw-back)
                      (if (even? (player-idx state))
                        (or (:left  choices) (:none choices))
                        (or (:right choices) (:none choices)))
                      :else
                      (let [r (mod (+ (player-idx state) (:round state 0)) 3)]
                        (case r
                          0 (:none  choices)
                          1 (or (:left  choices) (:none choices))
                          2 (or (:right choices) (:none choices)))))))

              ;; Pick the first available station to activate
              :choose-activate-station
              (or (first (vals (dissoc choices :done)))
                  (:done choices))

              ;; Tower heading: aim toward a tile with a beacon, else straight
              :choose-activate-tower-heading
              (let [beaconed (filter #(get-in state [:board % :beacon]) (keys (:board state)))]
                (if (seq beaconed)
                  (or (:left choices) (:right choices) (:none choices))
                  (:none choices)))

              ;; Land whenever possible
              :choose-land (:land choices (:continue choices))

              ;; Cipher: always place if affordable. Score by new board matches;
              ;; prefer new activations, but join existing colors too.
              ;; Only skip when no placeable positions (can't pay).
              :cipher
              (let [{:keys [color]} (first (get-in state [:player-turn :cipher-queue] []))
                    board     (:board state)
                    score     (fn [pos]
                                (if (game/cipher-color-active? state pos color)
                                  0
                                  (if (= pos [0 0])
                                    (count (filter #(= color (:color %)) (vals board)))
                                    (count (filter #(= color (get-in board [(game/add-hex % pos) :color]))
                                                   (keys board))))))
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

;; ── Generate page (smart simulation with full choice history) ─────────────────────────────────────────

(defn generate-history
  "Simulate a 5-player game using the smart agent until game-over.
   Returns a vector of {:step :player :phase :choice :state} entries."
  []
  (let [players ["alice" "bob" "carol" "dave" "eve"]
        state0  (game/initial-state players)
        initial {:step 0 :player nil :phase :initial :choice "—" :state state0}]
    (loop [s state0 i 0 history [initial]]
      (if (:game-over s)
        history
        (if-let [[ck next-s] (agent-step s)]
          (let [[phase _] (choice/find-state s)]
            (recur next-s
                   (inc i)
                   (conj history {:step   (inc i)
                                  :player (game/current-player s)
                                  :phase  phase
                                  :choice (pr-str ck)
                                  :state  next-s})))
          history)))))

(defn generate-page
  [request]
  (layout/render
   request
   "journey/generate.html"
   {:generate-history (pr-str (generate-history))}))

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
   ["/generate" {:get generate-page}]])
