(ns organism.bots
  "Shared bot registry. Each game registers its bots with a name and an
   `agent-step` function. Bots can be hand-coded heuristics (like OBO) or
   trained models (alpha-zero style — to be added).

   Bot names are conventionally ALL CAPS to distinguish them from human
   players. Multiple instances of the same bot in one game get auto-suffixed
   names (OBO-1, OBO-2, etc.) so the game state keys stay unique.")

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
  (let [base-name (clojure.string/replace instance-name #"-\d+$" "")]
    (get-in @registry [game-type base-name :agent-step])))

(defn bot?
  "True if the player name corresponds to a registered bot for this game-type
   (handles auto-suffixed instance names)."
  [game-type player-name]
  (when (and game-type player-name)
    (let [base-name (clojure.string/replace player-name #"-\d+$" "")]
      (contains? (get @registry game-type {}) base-name))))

(defn next-instance-name
  "Given a base bot name and a list of existing player names, return the
   next available suffixed instance: OBO → OBO-1 → OBO-2 → ..."
  [base-name existing-names]
  (let [taken (set existing-names)]
    (loop [i 1]
      (let [candidate (str base-name "-" i)]
        (if (taken candidate) (recur (inc i)) candidate)))))
