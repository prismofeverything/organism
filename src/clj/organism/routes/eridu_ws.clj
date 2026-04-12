(ns organism.routes.eridu-ws
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [cognitect.transit :as transit]
   [org.httpkit.server :as hk]
   [eridu.game :as game]
   [eridu.choice :as choice]
   [organism.persist :as persist]
   [organism.persist-eridu :as persist-e])
  (:import
   [java.io ByteArrayOutputStream]))

;; ── Transit helpers ───────────────────────────────────────────────────────────

(defn- ->stream [input]
  (cond (string? input) (io/input-stream (.getBytes input))
        :else input))

(defn read-json [input]
  (with-open [ins (->stream input)]
    (-> ins (transit/reader :json) transit/read)))

(defn write-json [output]
  (let [out (ByteArrayOutputStream. 4096)
        w   (transit/writer out :json)
        _   (transit/write w output)
        ret (.toString out)]
    (.reset out)
    ret))

(defn send! [channel message]
  (hk/send! channel (write-json message)))

(defn send-channels! [channels message]
  (doseq [ch channels]
    (send! ch message)))

;; ── Games atom ────────────────────────────────────────────────────────────────

(defonce games (atom {:games {}}))

;; ── Helpers ─────────────────────────────────────────────────────────────────

(defn- choice-player [state]
  (game/current-player state))

;; ── Persistence helper ──────────────────────────────────────────────────────

(defn- save-state!
  ([db play-key] (save-state! db play-key nil))
  ([db play-key choice-key]
   (let [game (get-in @games [:games play-key])]
     (when (:state game)
       (persist-e/save-game!
        db play-key (:state game) (:bots game)
        (or (:players game) (:turn-order (:state game)))
        (:initial-state game))
       (when choice-key
         (persist-e/append-action! db play-key choice-key))))))

;; ── State broadcasting ────────────────────────────────────────────────────────

(defn broadcast-state! [play-key]
  (let [game  (get-in @games [:games play-key])
        state (:state game)]
    (when state
      (let [[phase choices] (choice/find-state-raw state)]
        (send-channels!
         (:channels game)
         {:type     "game-state"
          :state    (pr-str state)
          :phase    (str phase)
          :choices  (pr-str (keys choices))
          :bots     (vec (:bots game))
          :can-undo (boolean (seq (:history game)))})))))

;; ── Game management ───────────────────────────────────────────────────────────

(defn empty-game [play-key channel]
  {:key      play-key
   :state    nil
   :history  []
   :bots     #{}
   :players  []
   :chat     []
   :channels #{channel}})

(defn append-channel! [play-key channel]
  (swap! games update-in [:games play-key :channels] conj channel))

(defn load-game! [db play-key channel]
  (if-let [saved (persist-e/load-game db play-key)]
    (let [g {:key           play-key
             :state         (:state saved)
             :initial-state (:initial-state saved)
             :history       []
             :bots          (set (:bots saved))
             :players       (:players saved)
             :saved-history (:history saved)
             :chat          []
             :channels      #{channel}}]
      (swap! games assoc-in [:games play-key] g)
      g)
    (let [g (empty-game play-key channel)]
      (swap! games assoc-in [:games play-key] g)
      g)))

(defn find-game! [db play-key channel]
  (let [existing (get-in @games [:games play-key])]
    (if (empty? existing)
      (load-game! db play-key channel)
      (do (append-channel! play-key channel)
          (update existing :channels conj channel)))))

;; ── Bot AI ───────────────────────────────────────────────────────────────────

(def ^:private bot-protected-phases
  "Phases where we broadcast before continuing, so watchers can see the game."
  #{:choose-die :choose-action :resolve-landing :game-over})

(defn- bot-advance
  "Advance state through trivial single-choice phases."
  [state]
  (loop [s state]
    (let [[p cs] (choice/find-state-raw s)]
      (if (and (= 1 (count cs))
               (not (contains? bot-protected-phases p)))
        (let [ns (first (vals cs))]
          (if ns (recur ns) s))
        s))))

;; ── Bot AI helpers ────────────────────────────────────────────────────────────

(defn- game-progress
  "Returns a value 0.0-1.0 indicating how far through the game we are."
  [state]
  (let [round (:round state 1)
        turn  (:turn-in-round state 1)]
    (/ (+ (* (dec round) game/turns-per-round) (dec turn))
       (* game/rounds-per-game game/turns-per-round))))

(defn- space-has-empty?
  "True if the astronomer would land on a space with no other astronomers (enabling role increase)."
  [state space-id]
  (<= (count (game/astronomers-on-space state space-id)) 1))

(defn- space-action-types
  "Return set of action types available on a space."
  [space-id]
  (set (map :type (:actions (get game/action-spaces space-id)))))

(defn- space-gives-resources
  "Return the resources a take action on this space would give."
  [space-id]
  (some :resources (:actions (get game/action-spaces space-id))))

(defn- has-resource-excess?
  "True if player has >2 of any resource in the given set."
  [pdata resources]
  (some #(> (get-in pdata [:resources %] 0) 2) resources))

(defn- city-has-own-face-up-temple?
  "True if player has a face-up temple in the given city."
  [pdata city]
  (= :face-up (get-in pdata [:temples city])))

(defn- city-has-sellable-demand?
  "True if the city has a demand the player can currently fulfill."
  [state player city]
  (let [pdata (game/player-data state player)
        demands (get-in state [:city-demands city] [])
        resources (:resources pdata)]
    (some #(pos? (get resources % 0)) demands)))

(defn- cities-with-demands-for
  "Cities that have demands matching the player's resources."
  [state player]
  (let [pdata (game/player-data state player)
        resources (:resources pdata)]
    (for [[city demands] (:city-demands state)
          :when (some #(pos? (get resources % 0)) demands)]
      city)))

(defn- own-raiders-on-routes
  "Returns route-keys where player has a raider in :point state (flippable via influence)."
  [pdata]
  (for [[rk rs] (:raiders pdata) :when (= rs :point)] rk))

(defn- influence-can-flip-own-raider?
  "True if any influence choice would move a magistrate to a city adjacent to a :point raider."
  [state player choices]
  (let [pdata (game/player-data state player)
        point-routes (set (own-raiders-on-routes pdata))
        non-skip (dissoc choices :skip)]
    (some (fn [[_k next-state]]
            ;; Check if in the next state, a magistrate is near a point raider route
            (let [mag-cities (set (keys (:magistrates next-state)))]
              (some (fn [rk]
                      (let [[c1 c2] rk]
                        (or (contains? mag-cities c1)
                            (contains? mag-cities c2))))
                    point-routes)))
          non-skip)))

(defn- space-score-for-goal
  "Score a destination space for how well it serves a strategic goal.
   Considers both the immediate die and what remaining dice could chain into."
  [state player pdata dest-space remaining-dice lower-track]
  (let [types (space-action-types dest-space)
        caravan-city (:caravan pdata)
        can-sell-here (city-has-sellable-demand? state player caravan-city)

        ;; Direct value of this space for current needs
        action-score
        (cond-> 0
          (and (= lower-track :amity) (contains? types :sell) can-sell-here) (+ 8)
          (and (= lower-track :amity) (contains? types :sell))              (+ 3)
          (and (= lower-track :amity) (contains? types :temple))            (+ 4)
          (and (= lower-track :glory) (contains? types :deploy))            (+ 4)
          (and (= lower-track :glory) (contains? types :influence))         (+ 5)
          (contains? types :travel)                                          (+ 1))

        ;; Chain bonus: can a remaining die move another astronomer to complement?
        ;; e.g., if we're setting up a sell, does another die land on a travel space?
        chain-bonus
        (if (seq remaining-dice)
          (let [other-astro-positions (:astronomers pdata)
                chain-scores
                (for [other-die remaining-dice
                      other-pos other-astro-positions
                      :let [other-dest (game/move-astronomer-clockwise other-pos other-die)
                            other-types (space-action-types other-dest)
                            ;; Complementary actions: if we sell here, travel there sets up next sell
                            ;; If we deploy here, influence there chains
                            combo-score
                            (cond-> 0
                              ;; Travel + sell chain
                              (and (contains? types :travel)
                                   (contains? other-types :sell))
                              (+ 4)
                              ;; Travel on follow-up to reach temple/sell city
                              (and (contains? types :sell)
                                   (contains? other-types :travel))
                              (+ 3)
                              ;; Deploy + influence chain (place raider, then move magistrate to flip it)
                              (and (contains? types :deploy)
                                   (contains? other-types :influence))
                              (+ 5)
                              ;; Influence + deploy chain
                              (and (contains? types :influence)
                                   (contains? other-types :deploy))
                              (+ 4)
                              ;; Temple + travel chain (place temple, then travel to flip it)
                              (and (contains? types :temple)
                                   (contains? other-types :travel))
                              (+ 4))]]
                  combo-score)]
            (if (seq chain-scores) (apply max chain-scores) 0))
          0)]
    (+ action-score (* 0.5 chain-bonus))))

(defn agent-step
  "Pick a choice for the bot. Strategic heuristic AI.
   Returns [choice-key next-state] or nil."
  [state]
  (let [[phase choices] (choice/find-state-raw state)]
    (when (and (not= phase :game-over) (seq choices))
      (let [player (game/current-player state)
            pdata  (game/player-data state player)
            progress (game-progress state)
            amity (:amity pdata 0)
            glory (:glory pdata 0)
            lower-track (if (<= amity glory) :amity :glory)

            pick
            (case phase
              ;; ── Die selection ──────────────────────────────────────────
              ;; Early game: prefer dice that land on empty spaces (role increases)
              ;; Late game: prefer dice that land on spaces with more astronomers (more actions)
              ;; Also de-prioritize spaces giving resources we already have >2 of
              :choose-die
              (let [dice (get pdata :dice-available [])
                    astronomer-positions (:astronomers pdata)
                    scored
                    (for [idx (range (count dice))
                          :let [die-val (nth dice idx)
                                ;; Remaining dice after using this one
                                remaining (into (subvec dice 0 idx) (subvec dice (inc idx)))
                                ;; Score each astronomer's destination
                                astro-scores
                                (for [astro-pos astronomer-positions
                                      :let [dest (game/move-astronomer-clockwise astro-pos die-val)
                                            on-space (count (game/astronomers-on-space state dest))
                                            will-be-alone (= on-space 0)
                                            space-resources (space-gives-resources dest)
                                            resource-penalty (if (and space-resources
                                                                      (has-resource-excess? pdata space-resources))
                                                               -3 0)
                                            ;; Chain score: how well does this die + remaining dice
                                            ;; serve our strategic goals?
                                            chain-score (space-score-for-goal
                                                         state player pdata dest
                                                         remaining lower-track)]]
                                  (+ resource-penalty
                                     chain-score
                                     (if (< progress 0.4)
                                       ;; Early: strongly prefer empty spaces for role increases
                                       (if will-be-alone 10 (+ 2 on-space))
                                       ;; Late: prefer populated spaces for more actions
                                       (+ (* on-space 5) (if will-be-alone 2 0)))))
                                best-astro-score (apply max astro-scores)]]
                      [best-astro-score idx])]
                (if (seq scored)
                  (second (last (sort scored)))
                  0))

              ;; ── Astronomer selection ───────────────────────────────────
              ;; Choose astronomer that best matches our early/late strategy
              :choose-astronomer
              (let [die-val (get-in state [:player-turn :die-value])
                    astronomer-positions (:astronomers pdata)
                    scored
                    (for [idx (range (count astronomer-positions))
                          :when (contains? choices idx)
                          :let [pos (nth astronomer-positions idx)
                                dest (game/move-astronomer-clockwise pos die-val)
                                on-space (count (game/astronomers-on-space state dest))
                                will-be-alone (= on-space 0)
                                types (space-action-types dest)
                                ;; Prefer spaces with actions matching our needs
                                need-amity (= lower-track :amity)
                                action-bonus (cond
                                               (and need-amity (contains? types :sell)) 3
                                               (and need-amity (contains? types :temple)) 2
                                               (and (not need-amity) (contains? types :influence)) 3
                                               (and (not need-amity) (contains? types :deploy)) 2
                                               :else 0)
                                space-resources (space-gives-resources dest)
                                resource-penalty (if (and space-resources
                                                          (has-resource-excess? pdata space-resources))
                                                   -3 0)]]
                      [(+ resource-penalty action-bonus
                          (if (< progress 0.4)
                            (if will-be-alone 10 (+ 2 on-space))
                            (+ (* on-space 5) (if will-be-alone 1 0))))
                       idx])]
                (if (seq scored)
                  (second (last (sort scored)))
                  (first (keys choices))))

              ;; ── Landing resolution ─────────────────────────────────────
              ;; Early: prefer role increase when alone. Late: prefer actions.
              :resolve-landing
              (cond
                (and (contains? choices :begin) (> progress 0.4)) :begin
                (contains? choices :increase-role) :increase-role
                (contains? choices :begin) :begin
                :else (first (keys choices)))

              ;; ── Role increase ──────────────────────────────────────────
              ;; Prefer role that helps the lower track
              :choose-role-increase
              (if (> (count choices) 1)
                (let [role-choices (dissoc choices :skip)
                      role-levels (:roles pdata)]
                  (if (seq role-choices)
                    (let [;; Priority based on which track is lower
                          role-priority
                          (if (= lower-track :amity)
                            {:merchant 0 :priest 1 :leader 2 :raider 3}
                            {:leader 0 :raider 1 :merchant 2 :priest 3})
                          scored (for [role (keys role-choices)
                                      :when (keyword? role)
                                      :let [level (get role-levels role 1)
                                            ;; Prefer lower-level roles (more room to grow)
                                            ;; But weight by priority
                                            pri (get role-priority role 5)]]
                                  [(+ (* pri 2) level) role])]
                      (if (seq scored)
                        (second (first (sort scored)))
                        (first (keys role-choices))))
                    :skip))
                (first (keys choices)))

              ;; ── Action selection ────────────────────────────────────────
              ;; Dynamic priority based on lower track and game state
              :choose-action
              (if (contains? choices :done)
                :done
                (let [space (get-in state [:player-turn :space])
                      caravan-city (:caravan pdata)
                      can-sell-here (city-has-sellable-demand? state player caravan-city)
                      has-face-up-temple (city-has-own-face-up-temple? pdata caravan-city)
                      nearby-sellable (seq (cities-with-demands-for state player))

                      ;; Build dynamic priority based on game state
                      action-priority
                      (cond
                        ;; If amity is lower: prioritize sell and temple
                        (= lower-track :amity)
                        (merge
                         {:take 3 :travel 5 :deploy 6 :influence 4}
                         (if can-sell-here {:sell 0} {:sell 4})
                         (if has-face-up-temple {:temple 7} {:temple 1}))

                        ;; If glory is lower: prioritize deploy and influence
                        :else
                        (merge
                         {:take 3 :sell 4 :temple 5}
                         {:deploy 1 :influence 0}
                         ;; But if we can sell, still do it
                         (when can-sell-here {:sell 2})
                         ;; Travel only if we can flip a temple or sell nearby
                         {:travel (if (or has-face-up-temple nearby-sellable) 3 7)}))

                      action-choices (dissoc choices :done)
                      scored (for [[idx _] action-choices
                                   :let [action (nth (:actions (get game/action-spaces space)) idx)
                                         atype (:type action)
                                         base-pri (get action-priority atype 99)
                                         ;; Bonus: de-prioritize take if resources >2
                                         resource-penalty
                                         (if (and (= atype :take)
                                                  (has-resource-excess? pdata (:resources action)))
                                           10 0)]]
                               [(+ base-pri resource-penalty) idx])]
                  (if (seq scored)
                    (second (first (sort scored)))
                    (first (keys choices)))))

              ;; ── Sell resolution ─────────────────────────────────────────
              :resolve-sell
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  ;; Sell the resource we have the most of
                  (let [resources (:resources pdata)]
                    (apply max-key #(get resources % 0) (keys non-skip)))
                  :skip))

              ;; ── Temple placement ────────────────────────────────────────
              ;; Place temples in cities where we'll travel to fulfill demands
              :resolve-temple
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (let [;; Score cities: prefer cities with demands we can fulfill
                        scored (for [city (keys non-skip)
                                     :let [demands (get-in state [:city-demands city] [])
                                           resources (:resources pdata)
                                           can-sell (count (filter #(pos? (get resources % 0)) demands))
                                           ;; Prefer cities with more demand slots
                                           demand-slots (get game/city-demand-count city 1)
                                           has-magistrate (game/magistrate-in-city? state city)]]
                                 [(+ (* can-sell 5) (* demand-slots 2)
                                     (if has-magistrate 3 0))
                                  city])]
                    (if (seq scored)
                      (second (last (sort scored)))
                      (first (keys non-skip))))
                  :skip))

              ;; ── Deploy raiders ──────────────────────────────────────────
              :resolve-deploy
              (let [non-skip (dissoc choices :skip :done)]
                (if (seq non-skip)
                  ;; Place raiders on routes where opponents are likely to travel
                  (let [scored (for [rk (keys non-skip)
                                     :let [[c1 c2] rk
                                           ;; Prefer routes near cities with demands
                                           d1 (count (get-in state [:city-demands c1] []))
                                           d2 (count (get-in state [:city-demands c2] []))
                                           ;; Prefer routes near opponent caravans
                                           near-opponent (count
                                                          (for [[pk pd] (:players state)
                                                                :when (not= pk player)
                                                                :when (or (= (:caravan pd) c1)
                                                                          (= (:caravan pd) c2))]
                                                            pk))]]
                                 [(+ d1 d2 (* near-opponent 5)) rk])]
                    (if (seq scored)
                      (second (last (sort scored)))
                      (first (keys non-skip))))
                  (or (:done choices) (first (keys choices)))))

              ;; ── Travel ──────────────────────────────────────────────────
              ;; Only travel if moving to flip a temple or toward a city where we can sell
              :resolve-travel
              (let [non-skip (dissoc choices :skip)
                    caravan-city (:caravan pdata)]
                (if (seq non-skip)
                  (let [scored
                        (for [dest (keys non-skip)
                              :let [;; Can we flip our own temple there?
                                    has-temple (city-has-own-face-up-temple? pdata dest)
                                    ;; Can we sell there now or soon?
                                    can-sell (city-has-sellable-demand? state player dest)
                                    ;; Does it have demands we might fulfill later?
                                    has-demands (seq (get-in state [:city-demands dest] []))
                                    ;; Is there a magistrate (glory bonus)?
                                    has-magistrate (game/magistrate-in-city? state dest)
                                    ;; Do we have own point raider on this route? (score glory)
                                    rk (game/route-key caravan-city dest)
                                    own-point-raider (= :point (get-in pdata [:raiders rk]))]]
                          [(+ (if has-temple 10 0)
                              (if can-sell 8 0)
                              (if own-point-raider 7 0)
                              (if has-magistrate 3 0)
                              (if has-demands 2 0))
                           dest])]
                    (let [best (last (sort scored))]
                      (if (and best (pos? (first best)))
                        (second best)
                        ;; No good destination - skip if possible
                        (if (contains? choices :skip) :skip (second (first (sort scored)))))))
                  :skip))

              ;; ── Travel continue ─────────────────────────────────────────
              ;; Only spend a good for extra movement if destination is very valuable
              :travel-continue
              (let [non-skip (dissoc choices :done)
                    caravan-city (:caravan pdata)]
                (if (seq non-skip)
                  (let [;; Check if any destination after extra travel is worth it
                        worth-it?
                        (some (fn [resource-key]
                                (when-let [next-s (get choices resource-key)]
                                  ;; In the next state we'd be in resolve-travel
                                  ;; Check if there's a good destination
                                  (let [next-pdata (game/player-data next-s player)
                                        next-city (:caravan next-pdata)
                                        neighbors (get-in next-s [:city-graph next-city])]
                                    (some (fn [dest]
                                            (or (city-has-own-face-up-temple? pdata dest)
                                                (city-has-sellable-demand? state player dest)))
                                          neighbors))))
                              (keys non-skip))]
                    (if worth-it? :done :done))
                  :done))

              ;; ── Influence ───────────────────────────────────────────────
              ;; Prioritize moving magistrate to flip own raiders or to cities with temples
              :resolve-influence
              (let [non-skip (dissoc choices :skip)]
                (if (seq non-skip)
                  (let [point-routes (set (map first (own-raiders-on-routes pdata)))
                        scored
                        (for [[k next-s] non-skip
                              :let [;; Where does the magistrate end up?
                                    dest (when (vector? k) (second k))
                                    ;; Can we flip own raider?
                                    near-point-raider
                                    (when dest
                                      (some #(or (= dest (first %)) (= dest (second %)))
                                            (for [[rk rs] (:raiders pdata)
                                                  :when (= rs :point)]
                                              rk)))
                                    ;; Is there our temple in destination?
                                    has-own-temple (and dest
                                                        (contains? (:temples pdata) dest))
                                    ;; Are there demands we can sell?
                                    has-demands (and dest
                                                     (seq (get-in state [:city-demands dest] [])))]]
                          [(+ (if near-point-raider 10 0)
                              (if has-own-temple 5 0)
                              (if has-demands 3 0))
                           k])]
                    (if (seq scored)
                      (second (last (sort scored)))
                      (first (keys non-skip))))
                  :skip))

              ;; Take goods: auto-resolve
              :resolve-take
              :done

              ;; Default: first choice
              (first (keys choices)))]

        (when-let [next-s (get choices pick)]
          [pick next-s])))))

;; ── Bot turns ────────────────────────────────────────────────────────────────

(defn run-bot-turns!
  "Spawn a future that auto-plays bot turns with delay until game over."
  [db play-key]
  (future
    (try
      (loop []
        (let [game-data (get-in @games [:games play-key])
              state     (:state game-data)
              bots      (:bots game-data)]
          (when (and state
                     (not (:game-over state))
                     (contains? bots (choice-player state)))
            (Thread/sleep (get game-data :bot-delay 300))
            (let [current-state (:state (get-in @games [:games play-key]))]
              (when (and current-state
                         (not (:game-over current-state))
                         (contains? bots (choice-player current-state)))
                (let [step-result (or (agent-step current-state)
                                      (let [[_ cs] (choice/find-state-raw current-state)]
                                        (when (seq cs)
                                          [(first (keys cs)) (first (vals cs))])))]
                  (when-let [[ck next-state] step-result]
                    (let [effective (bot-advance next-state)]
                      (swap! games
                             (fn [gs]
                               (-> gs
                                   (assoc-in [:games play-key :state] effective)
                                   (assoc-in [:games play-key :history] []))))
                      (broadcast-state! play-key)
                      (save-state! db play-key ck)
                      (recur)))))))))
      (catch Exception e
        (log/error "Eridu bot turn error" play-key (.getMessage e))))))

;; ── Message handlers ──────────────────────────────────────────────────────────

(def ^:private protected-phases
  #{:choose-die :choose-astronomer :choose-action :choose-role-increase
    :resolve-landing :resolve-sell :resolve-temple :resolve-deploy
    :resolve-travel :travel-continue :resolve-influence :game-over})

(defn handle-create! [db play-key {:keys [players bots]}]
  (when (seq players)
    (let [state    (game/initial-state (vec players))
          bot-set  (set (or bots []))]
      (swap! games
             (fn [gs]
               (-> gs
                   (assoc-in [:games play-key :state] state)
                   (assoc-in [:games play-key :initial-state] state)
                   (assoc-in [:games play-key :history] [])
                   (assoc-in [:games play-key :bots] bot-set)
                   (assoc-in [:games play-key :players] (vec players)))))
      (log/info "Created eridu game" play-key "players:" players "bots:" bots)
      (save-state! db play-key)
      (broadcast-state! play-key)
      (when (contains? bot-set (choice-player state))
        (run-bot-turns! db play-key)))))

(defn handle-action! [db play-key player-key {:keys [choice]}]
  (let [game-data (get-in @games [:games play-key])
        state     (:state game-data)]
    (when state
      (try
        (let [choice-key          (edn/read-string choice)
              [_phase choices-map] (choice/find-state-raw state)
              next-state           (get choices-map choice-key)]
          (if next-state
            (let [effective (loop [s next-state]
                              (let [p  (game/current-phase s)
                                    cs (second (choice/find-state-raw s))]
                                (if (and (= 1 (count cs))
                                         (not (contains? protected-phases p)))
                                  (recur (first (vals cs)))
                                  s)))
                  old-player    (choice-player state)
                  new-player    (choice-player effective)
                  turn-changed? (not= old-player new-player)
                  bots          (:bots game-data)]
              (swap! games
                     (fn [gs]
                       (-> gs
                           (assoc-in [:games play-key :state] effective)
                           (assoc-in [:games play-key :history]
                                     (if turn-changed?
                                       []
                                       (conj (:history (get-in gs [:games play-key])) state))))))
              (log/info "Action" play-key player-key (pr-str choice-key))
              (broadcast-state! play-key)
              (save-state! db play-key choice-key)
              (when (contains? bots new-player)
                (run-bot-turns! db play-key)))
            (log/warn "Unknown choice key" play-key player-key (pr-str choice-key))))
        (catch Exception e
          (log/error "Failed to apply action" play-key player-key choice (.getMessage e)))))))

(defn handle-undo! [db play-key player-key]
  (let [game-data (get-in @games [:games play-key])
        history   (:history game-data)]
    (when (seq history)
      (let [prev-state (peek history)]
        (swap! games
               (fn [gs]
                 (-> gs
                     (assoc-in [:games play-key :state] prev-state)
                     (update-in [:games play-key :history] pop))))
        (log/info "Undo" play-key player-key)
        (broadcast-state! play-key)
        (save-state! db play-key)))))

(defn handle-chat! [db play-key player-key {:keys [message]}]
  (let [msg {:type    "chat"
             :player  player-key
             :time    (quot (System/currentTimeMillis) 1000)
             :message message}]
    (swap! games update-in [:games play-key :chat] conj msg)
    (send-channels! (get-in @games [:games play-key :channels]) msg)
    (persist/update-chat! db play-key msg)))

;; ── WebSocket lifecycle ───────────────────────────────────────────────────────

(defn connect! [{:keys [db play-key player]} channel]
  (let [game-data (find-game! db play-key channel)]
    (log/info "Eridu CONNECT" player play-key)
    (let [base-msg {:type    "initialize"
                    :key     play-key
                    :player  player
                    :bots    (vec (:bots game-data))
                    :chat    (:chat game-data)
                    :history (vec (map #(dissoc % :state) (or (:saved-history game-data) [])))}]
      (send! channel
             (if-let [state (:state game-data)]
               (let [[phase choices] (choice/find-state-raw state)]
                 (assoc base-msg
                        :state (pr-str state)
                        :phase (str phase)
                        :choices (pr-str (keys choices))
                        :can-undo (boolean (seq (:history game-data)))))
               base-msg))
      ;; If all players are bots and game isn't over, start bot turns
      (when-let [state (:state game-data)]
        (when (and (not (:game-over state))
                   (contains? (:bots game-data) (choice-player state)))
          (run-bot-turns! db play-key))))))

(defn disconnect! [{:keys [play-key player]} channel status]
  (log/info "Eridu DISCONNECT" player status)
  (swap! games
         (fn [gs]
           (let [remaining (remove #{channel}
                                   (get-in gs [:games play-key :channels]))]
             (if (empty? remaining)
               (update-in gs [:games] dissoc play-key)
               (assoc-in gs [:games play-key :channels] (set remaining)))))))

(defn notify-clients! [{:keys [db play-key player]} _channel raw]
  (let [{:keys [type] :as message} (read-json raw)]
    (log/info "Eridu MSG" type player)
    (case type
      "create" (handle-create! db play-key message)
      "action" (handle-action! db play-key player message)
      "undo"   (handle-undo! db play-key player)
      "chat"   (handle-chat! db play-key player message)
      (log/warn "Unknown eridu message type" type))))

;; ── Route wiring ─────────────────────────────────────────────────────────────

(defn websocket-callbacks [db player play-key]
  (let [cfg {:db db :player player :play-key play-key}]
    {:on-open    (partial connect!         cfg)
     :on-close   (partial disconnect!      cfg)
     :on-receive (partial notify-clients!  cfg)}))

(defn ws-handler [db {:keys [path-params session] :as request}]
  (let [play   (:play path-params)
        player (or (:player session) "--observer--")]
    (hk/as-channel request (websocket-callbacks db player play))))

(defn eridu-ws-routes [db]
  [["/ws/eridu/play/:play" (partial ws-handler db)]])
