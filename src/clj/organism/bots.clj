(ns organism.bots
  "Shared bot registry. Each game registers its bots with a name and an
   `agent-step` function. Bots can be hand-coded heuristics (like OBO) or
   trained models (alpha-zero style — to be added).

   Bot names are conventionally ALL CAPS to distinguish them from human
   players. Multiple instances of the same bot in one game get auto-suffixed
   names (OBO-A, OBO-B, etc.) so the game state keys stay unique.")

(defonce ^{:doc "Registry: {game-type {bot-name {:agent-step fn :description str}}}"}
  registry (atom {}))

(defn register-bot!
  "Register a bot for a game-type. agent-step is a fn [game] → next-game."
  [game-type bot-name {:keys [agent-step description] :as bot-def}]
  (swap! registry assoc-in [game-type bot-name]
         {:agent-step  agent-step
          :description (or description "")})
  bot-name)

(defn list-bots
  "Return a seq of {:name :description} for all bots registered for game-type."
  [game-type]
  (for [[name bot] (get @registry game-type {})]
    {:name name
     :description (:description bot)}))

(defn get-agent-step
  "Look up the agent-step fn for a bot. Strips numeric suffix (OBO-1 → OBO)."
  [game-type instance-name]
  (let [base-name (clojure.string/replace instance-name #"-(?:[A-Z]|\d+)$" "")]
    (get-in @registry [game-type base-name :agent-step])))

(defn instance-base-name
  "Strip the alphabetic/numeric suffix to recover the base bot name (OBO-A → OBO)."
  [instance-name]
  (when instance-name
    (clojure.string/replace instance-name #"-(?:[A-Z]|\d+)$" "")))

(defn bot?
  "True if the player name corresponds to a registered bot for this game-type
   (handles auto-suffixed instance names)."
  [game-type player-name]
  (when (and game-type player-name)
    (contains? (get @registry game-type {}) (instance-base-name player-name))))

(defn next-instance-name
  "Given a base bot name and a list of existing player names, return the
   next available alphabetical instance: OBO → OBO-A → OBO-B → ..."
  [base-name existing-names]
  (let [taken (set existing-names)
        letters (map char (range 65 91))]  ;; A-Z
    (loop [ls letters]
      (if-let [c (first ls)]
        (let [candidate (str base-name "-" c)]
          (if (taken candidate) (recur (rest ls)) candidate))
        ;; Past Z — fall back to numeric
        (loop [i 1]
          (let [candidate (str base-name "-" i)]
            (if (taken candidate) (recur (inc i)) candidate)))))))

;; ── Generic bot runtime ─────────────────────────────────────────────────────
;;
;; Any game can run its bots through this loop. The game provides:
;;   - how to read/write the game state
;;   - the bot's agent-step+key function (returns [choice-key next-game])
;;   - victory and current-player accessors
;;   - what counts as a "turn boundary"
;;   - what to do on each turn boundary (broadcast, persist)
;; The shared loop handles spawning the future, throttling, error handling,
;; tracking choice keys per turn, and stopping conditions.

(defn run-bot-loop!
  "Spawn a background future that runs bot turns until a human's turn, the
   game ends, or the step cap is reached. Tracks choice keys per turn so
   each turn boundary callback can do event-sourced replay.

   Config map keys:
   :get-game       (fn []) → current game wrapper (with :state etc.)
   :put-game!      (fn [next-game]) updates the shared atom (called every step)
   :agent-step+key (fn [game] → [choice-key next-game]) — bot decision
   :victory?       (fn [game] → winner-or-nil)
   :current-player (fn [game] → player)
   :turn-changed?  (fn [prev-game next-game] → bool) — default: player change
   :on-turn        (fn [turn-choices next-game]) called on each turn boundary
   :humans         set of human player names — loop stops when their turn comes
   :delay-ms       sleep between turn boundaries (default 200)
   :max-steps      step cap to prevent infinite loops (default 20000)
   :label          string identifier for log messages"
  [{:keys [get-game put-game! agent-step+key victory? current-player
           turn-changed? on-turn humans delay-ms max-steps label]
    :or {humans #{}
         delay-ms 200
         max-steps 20000
         label "BOT"}}]
  (let [turn-changed? (or turn-changed?
                          (fn [a b] (not= (current-player a) (current-player b))))]
    (future
      (try
        (println label "starting")
        (loop [game (get-game)
               turn-choices []
               n 0]
        (let [winner (when game (victory? game))
              cur    (when game (current-player game))]
          (cond
            winner
            (do
              (when on-turn (on-turn turn-choices game))
              (println label "finished after" n "steps — winner:" winner))

            (or (> n max-steps) (nil? game))
            (println label "finished after" n "steps")

            (contains? humans cur)
            (println label "stopping at human" cur)

            :else
            (if-let [[ck next-game] (agent-step+key game)]
              (let [changed?  (turn-changed? game next-game)
                    choices'  (conj turn-choices ck)]
                (when put-game! (put-game! next-game))
                (if changed?
                  (do
                    (when on-turn (on-turn choices' next-game))
                    (Thread/sleep delay-ms)
                    (recur next-game [] (inc n)))
                  (recur next-game choices' (inc n))))
              (println label "no progress at step" n)))))
        (catch Exception e
          (println label "error:" (.getMessage e))
          (.printStackTrace e))))))
