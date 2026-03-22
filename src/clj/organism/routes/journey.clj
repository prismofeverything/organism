(ns organism.routes.journey
  (:require
   [organism.layout :as layout]
   [organism.persist :as persist]
   [organism.middleware :as middleware]
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

(defn- agent-step
  "Pick one goal-oriented choice and return [choice-key next-state]."
  [state]
  (let [[phase choices] (choice/find-state state)]
    (when (seq choices)
      (let [next-s
            (case phase
              ;; Priority: activate own stations → convert → move with real choices
              :choose-action-type
              (let [move-next   (:move choices)
                    [_ move-cs] (when move-next (choice/find-state move-next))
                    real-move?  (or (contains? move-cs :launch) (contains? move-cs :fly))]
                (or (when (has-own-stations? state) (:activate choices))
                    (:convert choices)
                    (when real-move? move-next)
                    (first (vals choices))))

              ;; Prefer fly when 3+ on board; otherwise launch to build presence
              :choose-move
              (let [player (game/current-player state)]
                (if (>= (on-board-count state player) 3)
                  (or (:fly choices) (:launch choices) (:done choices))
                  (or (:launch choices) (:fly choices) (:done choices))))

              ;; Launch to a position that creates a conversion pattern if possible;
              ;; otherwise spread players to different flanks via player index
              :choose-launch-destination
              (let [non-wrap (into {} (remove #(vector? (key %)) choices))
                    conv     (some #(when (enables-conversion? state (val %)) (val %)) non-wrap)]
                (or conv (pick-varied state (vals non-wrap)) (first (vals choices))))

              ;; Fly from the position with most sundivers (spread them out)
              :choose-fly-from
              (let [player (game/current-player state)
                    best   (apply max-key #(get-in state [:board % :sundivers player] 0)
                                  (keys choices))]
                (get choices best))

              ;; Fly to a position that creates a conversion pattern if possible;
              ;; otherwise vary by player to avoid all moving the same direction
              :choose-fly-to
              (let [conv (some #(when (enables-conversion? state (val %)) (val %)) choices)]
                (or conv (pick-varied state (vals choices))))

              ;; Take max bonus actions to fully utilise stations
              :choose-activate-self-bonus  (get choices (apply max (keys choices)))
              :choose-activate-owner-bonus (get choices (apply max (keys choices)))
              :choose-activate-tower-join  (:join choices (:skip choices))
              :choose-activate-tower-spend (first (vals choices))

              :flare-beacon-join   (:join choices (:skip choices))
              :captain-beacon-join (:join choices (:skip choices))

              :choose-ark-advance          (:direct choices)
              :choose-flare-advance        (:direct choices)
              :choose-drift-flare-advance  (:direct choices)

              :draw-drift-card (:draw choices)

              ;; Vary ark heading by player index — each player sweeps a different arc
              :choose-captain-drift
              (let [r (mod (+ (player-idx state) (:round state 0)) 3)]
                (case r
                  0 (:none  choices)
                  1 (or (:left  choices) (:none choices))
                  2 (or (:right choices) (:none choices))))

              ;; Tower heading: aim toward a tile with a beacon, else straight
              :choose-activate-tower-heading
              (let [beaconed (filter #(get-in state [:board % :beacon]) (keys (:board state)))]
                (if (seq beaconed)
                  (or (:left choices) (:right choices) (:none choices))
                  (:none choices)))

              ;; Land whenever possible
              :choose-land (:land choices (:continue choices))

              ;; Cipher: prefer center, then spread via player index
              :cipher
              (or (get choices [0 0])
                  (pick-varied state (vals choices)))

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
  "Simulate a 5-player game using the smart agent until game-over or 1000 steps.
   Returns a vector of {:step :player :phase :choice :state} entries."
  []
  (let [players ["alice" "bob" "carol" "dave" "eve"]
        state0  (game/initial-state players)
        initial {:step 0 :player nil :phase :initial :choice "—" :state state0}]
    (loop [s state0 i 0 history [initial]]
      (if (or (:game-over s) (>= i 1000))
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

(def cached-history
  (delay (pr-str (generate-history))))

(defn generate-page
  [request]
  (layout/render
   request
   "journey/generate.html"
   {:generate-history @cached-history}))

(defn journey-routes
  [db]
  ["/journey"
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["" {:get home-page}]
   ["/create" {:get (partial create-page db)
               :middleware [require-auth]}]
   ["/play/:play" {:get (partial play-page db)}]
   ["/play/:play/" {:get (partial play-page db)}]
   ["/observe" {:get (partial observe-page db)}]
   ["/generate" {:get generate-page}]])
