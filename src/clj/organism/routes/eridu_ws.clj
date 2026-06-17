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
         (cond-> {:type     "game-state"
                  :state    (pr-str state)
                  :phase    (str phase)
                  :choices  (pr-str (keys choices))
                  :bots     (vec (:bots game))
                  :can-undo (boolean (seq (:history game)))}
           (:pending-claim game)
           (assoc :pending-claim (pr-str (:pending-claim game)))
           (:pending-bonus game)
           (assoc :pending-bonus (pr-str (:pending-bonus game)))))))))

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
  game/bot-protected-phases)

(defn- bot-advance
  "Advance state through trivial single-choice phases."
  [state]
  (choice/advance-through-trivial state bot-protected-phases))

;; ── Bot AI helpers ────────────────────────────────────────────────────────────

(defn- game-progress
  "Returns a value 0.0-1.0 indicating how far through the game we are."
  [state]
  (let [round (:round state 1)
        turn  (:turn-in-round state 1)
        tpr   (game/turns-per-round state)]
    (/ (+ (* (dec round) tpr) (dec turn))
       (* game/rounds-per-game tpr))))

(defn- space-has-empty?
  "True if the astronomer would land on a space with no other astronomers (enabling role increase)."
  [state space-id]
  (<= (count (game/astronomers-on-space state space-id)) 1))

;; (Five state-query helpers that lived here previously now live in
;;  eridu.game — see space-action-types, space-gives-resources,
;;  has-resource-excess?, city-has-sellable-demand?, city-has-own-face-up-temple?.)

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
  (for [[rk rs] (:raiders pdata) :when (some #{:point} rs)] rk))

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
  (let [types (game/space-action-types dest-space)
        caravan-city (:caravan pdata)
        can-sell-here (game/city-has-sellable-demand? state player caravan-city)

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
                            other-types (game/space-action-types other-dest)
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
                                            space-resources (game/space-gives-resources dest)
                                            resource-penalty (if (and space-resources
                                                                      (game/has-resource-excess? pdata space-resources))
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
                                types (game/space-action-types dest)
                                ;; Prefer spaces with actions matching our needs
                                need-amity (= lower-track :amity)
                                action-bonus (cond
                                               (and need-amity (contains? types :sell)) 3
                                               (and need-amity (contains? types :temple)) 2
                                               (and (not need-amity) (contains? types :influence)) 3
                                               (and (not need-amity) (contains? types :deploy)) 2
                                               :else 0)
                                space-resources (game/space-gives-resources dest)
                                resource-penalty (if (and space-resources
                                                          (game/has-resource-excess? pdata space-resources))
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
              (if (= #{:done} (set (keys choices)))
                :done
                (let [space (get-in state [:player-turn :space])
                      caravan-city (:caravan pdata)
                      can-sell-here (game/city-has-sellable-demand? state player caravan-city)
                      has-face-up-temple (game/city-has-own-face-up-temple? pdata caravan-city)
                      nearby-sellable (seq (cities-with-demands-for state player))

                      ;; Can we actually deploy? (have raiders in supply)
                      can-deploy? (pos? (:raiders-supply pdata 0))
                      ;; Can we actually place a temple? (have temples in supply)
                      can-temple? (pos? (:temples-supply pdata 0))

                      ;; Build dynamic priority based on game state
                      ;; Lower number = higher priority. 99 = never pick.
                      action-priority
                      (cond
                        ;; If amity is lower: prioritize sell and temple
                        (= lower-track :amity)
                        (merge
                         {:take 3 :travel 5 :influence 4}
                         ;; Never pick sell if we can't sell
                         {:sell (if can-sell-here 0 99)}
                         {:deploy (if can-deploy? 6 99)}
                         {:temple (cond (not can-temple?) 99
                                        has-face-up-temple 7
                                        :else 1)})

                        ;; If glory is lower: prioritize deploy and influence
                        :else
                        (merge
                         {:take 3}
                         {:sell (if can-sell-here 2 99)}
                         {:deploy (if can-deploy? 1 99)}
                         {:temple (cond (not can-temple?) 99
                                        has-face-up-temple 5
                                        :else 4)}
                         {:influence 0}
                         {:travel (if (or has-face-up-temple nearby-sellable) 3 7)}))

                      action-choices (dissoc choices :done)
                      scored (for [[idx _] action-choices
                                   ;; skip non-numeric choice keys (e.g. board-14
                                   ;; [:uruk-move dest], board-6 :free-travel) — they
                                   ;; aren't action-space indices. Matches personality.cljc.
                                   :when (number? idx)
                                   :let [action (nth (:actions (get game/action-spaces space)) idx)
                                         atype (:type action)
                                         base-pri (get action-priority atype 99)
                                         ;; Bonus: de-prioritize take if resources >2
                                         resource-penalty
                                         (if (and (= atype :take)
                                                  (game/has-resource-excess? pdata (:resources action)))
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
                                    has-temple (game/city-has-own-face-up-temple? pdata dest)
                                    ;; Can we sell there now or soon?
                                    can-sell (game/city-has-sellable-demand? state player dest)
                                    ;; Does it have demands we might fulfill later?
                                    has-demands (seq (get-in state [:city-demands dest] []))
                                    ;; Is there a magistrate (glory bonus)?
                                    has-magistrate (game/magistrate-in-city? state dest)
                                    ;; Do we have own point raider on this route? (score glory)
                                    rk (game/route-key caravan-city dest)
                                    own-point-raider (some #{:point} (game/raiders-on (:raiders pdata) rk))]]
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
                                            (or (game/city-has-own-face-up-temple? pdata dest)
                                                (game/city-has-sellable-demand? state player dest)))
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
                                                  :when (some #{:point} rs)]
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

(def ^:private protected-phases game/human-protected-phases)

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

(defn- auto-resolve-passive-for-bot
  "Pick an automatic choice for a bot's passive. Returns a keyword."
  [pending pdata]
  (case (:type pending)
    :pick-resource (let [resources (:resources pdata)
                         cheapest (apply min-key #(get resources % 0) game/resource-types)]
                     cheapest)
    :pick-role     (first (or (:options pending) game/roles))
    :yes-no        :yes
    :tools))

(defn- promote-passive-choice!
  "Check if any player in the game has :passive-choice-needed set by apply-passive.
   If so, move it to :pending-bonus on the game-data level (reusing existing UI).
   For bots, auto-resolve immediately."
  [play-key]
  (let [game-data (get-in @games [:games play-key])
        state (:state game-data)
        bots (:bots game-data)]
    (when state
      (doseq [[pk pdata] (:players state)]
        (when-let [pending (:passive-choice-needed pdata)]
          (log/info "Passive choice needed" play-key pk (:board-id pending) (:type pending))
          (if (contains? bots pk)
            ;; Bot: auto-resolve immediately
            (let [choice-val (auto-resolve-passive-for-bot pending pdata)]
              (swap! games
                     (fn [gs]
                       (let [cur-state (:state (get-in gs [:games play-key]))
                             new-state (-> cur-state
                                           (game/apply-passive-choice pk choice-val)
                                           (update :log conj
                                                   {:type :passive-effect :player pk
                                                    :round (:round cur-state 1)
                                                    :turn (:turn-in-round cur-state 1)
                                                    :message (str "Passive (board " (:board-id pending)
                                                                  "): bot chose " (name choice-val))}))]
                         (assoc-in gs [:games play-key :state] new-state))))
              (log/info "Bot auto-resolved passive" play-key pk choice-val))
            ;; Human: set pending-bonus for UI
            (swap! games
                   (fn [gs]
                     (assoc-in gs [:games play-key :pending-bonus]
                               {:player pk
                                :board-id (:board-id pending)
                                :choice-type (:type pending)
                                :prompt (:prompt pending)
                                :source :passive
                                :options (:options pending)
                                :context (:context pending)
                                :remaining 1
                                :total 1})))))))))

(defn handle-action! [db play-key player-key {:keys [choice]}]
  (let [game-data (get-in @games [:games play-key])
        state     (:state game-data)]
    (when state
      (try
        (let [choice-key          (edn/read-string choice)
              [_phase choices-map] (choice/find-state-raw state)
              next-state           (get choices-map choice-key)]
          (if next-state
            (let [effective (choice/advance-through-trivial next-state protected-phases)
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
              ;; If game just ended, ensure end-game scoring is applied
              (when (and (:game-over effective)
                         (not (:game-over state)))
                ;; Re-apply scoring to guarantee wild points + role bonuses are distributed
                ;; (advance-turn should have done this, but as a safety net we apply again)
                (let [;; Strip game-over, re-score, re-set game-over
                      base (dissoc effective :game-over)
                      scored (game/apply-end-game-scoring base)
                      final (assoc scored :game-over (:game-over effective))]
                  (swap! games assoc-in [:games play-key :state] final)))
              ;; Check if any passive triggered a choice prompt
              (promote-passive-choice! play-key)
              (broadcast-state! play-key)
              (save-state! db play-key choice-key)
              (when (contains? bots new-player)
                (run-bot-turns! db play-key)))
            (log/warn "Unknown choice key" play-key player-key (pr-str choice-key))))
        (catch Exception e
          (log/error "Failed to apply action" play-key player-key choice (.getMessage e)))))))

(defn handle-claim-feat! [db play-key player-key {:keys [feat-id slot-idx]}]
  (when-let [game-data (get-in @games [:games play-key])]
  (when-let [state (:state game-data)]
  (let [contest-id (when feat-id (keyword feat-id))
        contest   (when contest-id
                    (first (filter #(= (:id %) contest-id) (:contests state []))))
        already-claimed? (some #{player-key} (get-in state [:contest-claims contest-id] []))
        condition-met? (when contest
                         (game/evaluate-contest state player-key contest))
        bonus-board (get-in state [:players player-key :bonus-board]
                            (vec (repeat 5 :covered)))
        ;; slot-idx provided = player chose which slot. nil = just highlight (step 1).
        chosen-slot (when slot-idx (Integer/parseInt (str slot-idx)))]
    (cond
      (not contest)
      (log/warn "Unknown feat" play-key feat-id)

      ;; Must be this player's turn
      (not= player-key (game/current-player state))
      (log/warn "Not your turn to claim" play-key feat-id player-key)

      already-claimed?
      (log/warn "Already claimed" play-key feat-id)

      (not condition-met?)
      (log/warn "Feat condition not met" play-key feat-id)

      ;; No slot chosen yet: store pending claim so UI can show slot picker
      (nil? chosen-slot)
      (do
        (swap! games assoc-in [:games play-key :pending-claim]
               {:player player-key :contest-id contest-id})
        (broadcast-state! play-key))

      ;; Slot chosen: execute the claim
      (not= :covered (nth bonus-board chosen-slot nil))
      (log/warn "Slot already uncovered" play-key feat-id chosen-slot)

      :else
      (let [current-claims (get-in state [:contest-claims contest-id] [])
            claim-count (count current-claims)
            wild-points (get game/bonus-contest-values claim-count 1)
            board-id (get-in state [:bonus-boards player-key])
            board (get game/bonus-boards-by-id board-id)
            effect-text (get (:effects board) chosen-slot "—")
            contest-name (:name contest "")
            raw-needs-choice (game/bonus-needs-choice? board-id chosen-slot)
            multi? (:multi raw-needs-choice)
            ;; FIX 3: every :pick-city filter's legal target set is computed
            ;; server-side (game/eligible-cities-for-filter) so the UI gets a
            ;; concrete city list for ALL filters — and an empty set uniformly
            ;; turns into a no-op rather than an unresolvable/absent picker.
            eligible-cities (when (= :pick-city (:type raw-needs-choice))
                              (game/eligible-cities-for-filter
                               state player-key (:filter raw-needs-choice)))
            computed-targets? (some? eligible-cities)
            needs-choice (cond
                           (not raw-needs-choice) nil
                           (and computed-targets? (empty? eligible-cities)) nil
                           :else raw-needs-choice)
            no-target? (and computed-targets? (empty? eligible-cities))
            ;; Resolve the uncovered slot's bonus through the SHARED primitive
            ;; game/apply-feat-claim! (same one the bot uses), so claim + wild +
            ;; :feat-claimed passive can never drift between paths. The human
            ;; defers an interactive pick to pending-bonus (bonus-fn = identity)
            ;; and applies a non-interactive effect immediately.
            bonus-fn (cond
                       ;; interactive WITH a real target → defer to pending-bonus
                       needs-choice identity
                       ;; no eligible target (or non-interactive) → fire the arm
                       ;; once: a choice-independent rider still happens, the
                       ;; choice-dependent part no-ops via (or choice default).
                       :else (fn [s] (game/apply-bonus-effect s player-key board-id chosen-slot)))
            claimed (game/apply-feat-claim! state player-key contest-id chosen-slot
                                            wild-points bonus-fn)
            new-state (-> claimed
                          (update :log conj
                                  {:type :feat-claim :player player-key
                                   :round (:round state 1) :turn (:turn-in-round state 1)
                                   :message (str "Claimed feat " (name contest-id)
                                                 " (" contest-name ") → +"
                                                 wild-points " wild points")})
                          (update :log conj
                                  {:type :bonus-effect :player player-key
                                   :round (:round state 1) :turn (:turn-in-round state 1)
                                   :message (str "Bonus #" chosen-slot " uncovered: " effect-text
                                                 (cond needs-choice " — choose below"
                                                       no-target?   " — no qualifying cities, no effect"
                                                       :else        ""))}))]
        (swap! games
               (fn [gs]
                 (-> gs
                     (assoc-in [:games play-key :state] new-state)
                     (update-in [:games play-key] dissoc :pending-claim)
                     ;; If bonus needs choice, store pending-bonus
                     (cond-> needs-choice
                       (assoc-in [:games play-key :pending-bonus]
                                 (let [n (if multi? (count eligible-cities)
                                             (get needs-choice :count 1))]
                                   (cond-> {:player player-key
                                            :board-id board-id
                                            :slot-idx chosen-slot
                                            :choice-type (:type needs-choice)
                                            :prompt (:prompt needs-choice)
                                            :filter (:filter needs-choice)
                                            :action (:action needs-choice)
                                            :remaining n
                                            :total n}
                                     (seq eligible-cities)
                                     (assoc :eligible-cities (vec eligible-cities)))))))))
        (log/info "Feat claimed" play-key player-key feat-id "slot" chosen-slot
                  "wild+" wild-points
                  (when needs-choice (str " (needs " (:type needs-choice) " choice)")))
        (broadcast-state! play-key)
        (save-state! db play-key)))))))

(defn handle-resolve-bonus! [db play-key player-key {:keys [choice]}]
  (let [game-data (get-in @games [:games play-key])
        state     (:state game-data)
        pending   (:pending-bonus game-data)]
    (when (and state pending (= player-key (:player pending)))
      (if (= :passive (:source pending))
        ;; ── Passive-triggered choice: delegate to apply-passive-choice ──
        (let [choice-val (edn/read-string choice)
              new-state (-> state
                            (game/apply-passive-choice player-key choice-val)
                            (update :log conj
                                    {:type :passive-effect :player player-key
                                     :round (:round state 1) :turn (:turn-in-round state 1)
                                     :message (str "Passive (board " (:board-id pending)
                                                   "): chose " (name choice-val))}))]
          (swap! games
                 (fn [gs]
                   (-> gs
                       (assoc-in [:games play-key :state] new-state)
                       (update-in [:games play-key] dissoc :pending-bonus))))
          (log/info "Passive choice" play-key player-key (:board-id pending) choice-val)
          (broadcast-state! play-key)
          (save-state! db play-key))
        ;; ── Normal bonus board choice ──
        (let [{:keys [board-id slot-idx]} pending
              remaining (get pending :remaining 1)
              choice-val (edn/read-string choice)
              eligible (get pending :eligible-cities)
              ;; If we're tracking a restricted set of targets (e.g. board 34),
              ;; the chosen value must come from it.
              eligible-set (when eligible (set eligible))
              valid? (or (nil? eligible-set) (contains? eligible-set choice-val))]
          (if-not valid?
            (log/warn "Bonus pick not in eligible set" play-key player-key
                      choice-val "eligible:" eligible)
            (let [;; Apply this single pick via the choice handler
                  new-state (-> state
                                (game/apply-bonus-with-choice player-key board-id slot-idx choice-val)
                                (update :log conj
                                        {:type :bonus-effect :player player-key
                                         :round (:round state 1) :turn (:turn-in-round state 1)
                                         :message (str "Bonus pick: " (name choice-val)
                                                       " (" (- remaining 1) " left)")}))
                  picks-left (dec remaining)
                  new-eligible (when eligible (vec (remove #(= % choice-val) eligible)))]
              (swap! games
                     (fn [gs]
                       (let [gs (assoc-in gs [:games play-key :state] new-state)]
                         (if (and (pos? picks-left) (or (nil? eligible) (seq new-eligible)))
                           (assoc-in gs [:games play-key :pending-bonus]
                                     (cond-> (assoc pending
                                                    :remaining picks-left
                                                    :prompt (str (:prompt pending)
                                                                 " (" picks-left " of "
                                                                 (:total pending) " left)"))
                                       new-eligible (assoc :eligible-cities new-eligible)))
                           (update-in gs [:games play-key] dissoc :pending-bonus)))))
              (log/info "Bonus pick" play-key player-key choice-val
                        "remaining:" picks-left)
              (broadcast-state! play-key)
              (save-state! db play-key))))))))

(defn handle-use-passive! [db play-key player-key {:keys [passive-id choice]}]
  (let [game-data (get-in @games [:games play-key])
        state     (:state game-data)]
    (when state
      (let [player player-key
            pdata  (game/player-data state player)]
        (case passive-id
          ;; Board 14: Uruk bonus travel — discard one good, move between Uruk and adjacent city
          "14"
          (let [resource (when choice (edn/read-string choice))
                caravan  (:caravan pdata)
                uruk-adj (set (get-in state [:city-graph :uruk]))
                ;; Player must be in Uruk or adjacent to Uruk
                in-uruk? (= caravan :uruk)
                adj-to-uruk? (contains? uruk-adj caravan)
                destination (if in-uruk?
                              ;; From Uruk: we need the player to pick a destination.
                              ;; For simplicity, if the choice includes a destination, use it.
                              ;; Otherwise, skip.
                              nil ;; handled by choice format below
                              :uruk)
                ;; Parse choice: should be [resource destination] when from Uruk, or resource when to Uruk
                [resource destination]
                (if (vector? resource)
                  [(first resource) (second resource)]
                  [resource destination])
                has-passive? (game/has-passive? state player)
                board-id (game/player-board-id state player)
                has-resource? (and resource (pos? (get-in pdata [:resources resource] 0)))
                used? (get-in state [:players player :used-uruk-travel])
                valid-dest? (or (and in-uruk? (contains? uruk-adj destination))
                                (and adj-to-uruk? (= destination :uruk)))]
            (if (and has-passive? (= board-id 14)
                     (or in-uruk? adj-to-uruk?)
                     has-resource? (not used?) valid-dest?)
              (let [new-state (-> state
                                  (update-in [:players player :resources resource] dec)
                                  (assoc-in [:players player :used-uruk-travel] true)
                                  (choice/travel-to-city player destination)
                                  (update :log conj
                                          {:type    :passive-effect
                                           :player  player
                                           :round   (:round state 1)
                                           :turn    (:turn-in-round state 1)
                                           :message (str "Bonus travel (board 14): discarded "
                                                         (name resource) ", moved from "
                                                         (name caravan) " to " (name destination))}))]
                (swap! games assoc-in [:games play-key :state] new-state)
                (log/info "Use-passive board 14" play-key player-key resource destination)
                (broadcast-state! play-key)
                (save-state! db play-key))
              (log/warn "Invalid use-passive board 14" play-key player-key
                        {:in-uruk? in-uruk? :adj? adj-to-uruk? :resource resource
                         :has-resource? has-resource? :used? used? :dest destination})))

          ;; Default: unknown passive
          (log/warn "Unknown use-passive" play-key player-key passive-id))))))

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

(defn handle-resign! [db play-key player-key]
  ;; Mark the game over with :resigned-by + a winner = the other live player
  ;; (or nil in solo). Player must be in the game; can't resign someone else's.
  (let [game-data (get-in @games [:games play-key])
        state     (:state game-data)]
    (when (and state
               (contains? (set (:turn-order state)) player-key)
               (not (:game-over state)))
      (let [others (remove #{player-key} (:turn-order state))
            ;; Pick a winner by current reputation among remaining players
            winner (when (seq others)
                     (apply max-key
                            (fn [pk]
                              (let [pd (get-in state [:players pk])]
                                (min (:amity pd 0) (:glory pd 0))))
                            others))
            new-state (-> state
                          (assoc :game-over {:reason :resigned
                                             :resigned-by player-key
                                             :winner winner})
                          (update :log (fnil conj [])
                                  {:type :resign
                                   :player player-key
                                   :round (:round state 1)
                                   :turn (:turn-in-round state 1)
                                   :message (str (name player-key) " resigned"
                                                 (when winner
                                                   (str " — " (name winner) " wins")))}))]
        (swap! games assoc-in [:games play-key :state] new-state)
        (log/info "Resign" play-key player-key "winner:" winner)
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
      "create"        (handle-create! db play-key message)
      "action"        (handle-action! db play-key player message)
      "undo"          (handle-undo! db play-key player)
      "resign"        (handle-resign! db play-key player)
      "claim-feat"    (handle-claim-feat! db play-key player message)
      "resolve-bonus" (handle-resolve-bonus! db play-key player message)
      "use-passive"   (handle-use-passive! db play-key player message)
      "chat"          (handle-chat! db play-key player message)
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
