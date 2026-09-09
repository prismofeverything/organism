(ns organism.resolve-tie
  "Repair a game that ran past its own ending.

   Before the tie rule existed, a tie returned no winner and play simply carried
   on. The states that came after were played in a game that had already ended.
   This walks a game's history, finds the first point where the victory check
   NOW names a winner, and ends the game there.

   Nothing is destroyed: the states played after the ending are copied into a
   voided-history- collection before they leave the live history."
  (:require
   [organism.game :as game]
   [organism.mongo :as db]
   [organism.persist :as persist]))

(defn voided-history-key
  [game-key]
  (str "voided-history-" game-key))

(defn load-inner-game
  "The stored game with its pr-str'd fields read back — the shape the victory
   check expects, minus the state, which gets swapped in per history entry."
  [db game-key]
  (when-let [record (db/one db :games {:key game-key})]
    (-> (:game record)
        (update :players persist/conditional-string)
        (update :adjacencies persist/conditional-string))))

(defn raw-history
  "History documents in insertion order, _ids kept so entries can be truncated."
  [db game-key]
  (db/query db (persist/history-key game-key) {}))

(defn awaiting-conflicts?
  "Whether this state is still waiting on conflict resolution.

   Mongo stores keyword VALUES as plain strings, so :advance comes back off a
   history document as \"resolve-conflicts\" where the live game holds the
   keyword. Accept either, or every stored state looks ready to be judged."
  [state]
  (let [advance (get-in state [:player-turn :advance])]
    (= "resolve-conflicts" (when advance (name advance)))))

(defn decisive-winner
  "The winner find-state would have declared at this state, if any.

   Mirrors the cond in choice/find-state: the :resolve-conflicts branch is
   tested before the victory branch, so a state still waiting on conflict
   resolution never reaches the check and must not be judged here either."
  [inner-game state]
  (when-not (awaiting-conflicts? state)
    (game/victory? (assoc inner-game :state state))))

(defn find-ending
  "The first history entry where the game actually ended.
   {:index :id :winner :state} or nil."
  [inner-game history]
  (first
   (keep-indexed
    (fn [index record]
      (let [state (persist/deserialize-state record)]
        (when-let [winner (decisive-winner inner-game state)]
          {:index index
           :id (:_id record)
           :winner winner
           :state state})))
    history)))

(defn standing
  "Living organisms per player and relative captures at a state, so a report can
   show what the tie actually looked like."
  [inner-game state]
  (let [at (assoc inner-game :state state)]
    {:organisms (into {}
                      (map (fn [[player organisms]]
                             [player (count (filter (fn [[_organism elements]]
                                                      (game/alive-elements? elements))
                                                    organisms))])
                           (game/all-organisms at)))
     :captures (game/all-relative-captures at)}))

(defn examine
  "What is wrong with this game, if anything. Pure — writes nothing."
  [db game-key]
  (let [inner-game (load-inner-game db game-key)
        history (raw-history db game-key)]
    (cond
      (nil? inner-game)
      {:status :not-found :key game-key}

      (empty? history)
      {:status :no-history :key game-key}

      :else
      (let [ending (find-ending inner-game history)
            total (count history)]
        (cond
          (nil? ending)
          {:status :no-winner :key game-key :history-count total}

          (= (:index ending) (dec total))
          (assoc ending :status :already-ended :key game-key :history-count total
                 :standing (standing inner-game (:state ending)))

          :else
          (assoc ending :status :overran :key game-key :history-count total
                 :discard-count (- total 1 (:index ending))
                 :acting (get-in (:state ending) [:player-turn :player])
                 :round (:round (:state ending))
                 :standing (standing inner-game (:state ending))))))))

(defn resolve-tie!
  "End the game at the point it actually ended. Only acts on an :overran game.

   The states after the ending are copied to voided-history-<key> and then
   dropped from the live history, because load-game hands the play page the
   LAST history entry — leave the overrun in place and the board would render a
   position from after the winner was decided."
  [db game-key]
  (let [report (examine db game-key)]
    (if (not= :overran (:status report))
      report
      (let [{:keys [id winner state index]} report
            history-collection (persist/history-key game-key)
            history (raw-history db game-key)
            discarded (drop (inc index) history)]
        ;; keep the overrun before it leaves the live history
        (doseq [record discarded]
          (db/insert! db (voided-history-key game-key) record))
        (doseq [record discarded]
          (db/delete! db history-collection {:_id (:_id record)}))
        ;; the ending itself carries the winner, for load-game and observe
        (db/merge! db history-collection {:_id id} {:winner winner})
        ;; and the game plus every player's row is marked complete
        (persist/complete-game! db game-key (assoc state :winner winner))
        (assoc report :status :resolved)))))
