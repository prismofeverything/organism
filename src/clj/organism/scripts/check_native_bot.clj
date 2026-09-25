(ns organism.scripts.check-native-bot
  "Check that the Clojure engine and the native move server describe the same
   game, then play one out with the trained network.

   The bot is only as sound as that agreement: it reads a position out of one
   engine and a move out of the other, so any disagreement about which moves
   exist, or about what a move index means, is a wrong move on the website.
   This walks real games and compares the two at every decision.

       lein run -m organism.scripts.check-native-bot [games]"
  (:require
   [organism.board :as board]
   [organism.bots :as bots]
   [organism.choice :as choice]
   [organism.game :as game]
   [organism.native-bot :as native]))

(defn create
  "A board set up the way the lobby sets one up. Three players on four rings by
   default: the board the model was trained for."
  ([] (create ["orb" "mass" "brone"] 4))
  ([players ring-count]
   (let [rings (vec (take ring-count board/total-rings))
         starting (board/starting-spaces ring-count (count players) players board/total-rings {})
         info (game/initial-players
               starting (vec (repeat (count players) board/default-player-captures)))]
     (game/create-game (board/player-symmetry (count players)) rings info 3 false {}))))

(defn check-registry
  "The bot has to be reachable the way the website reaches it: listed for
   organism, recognised as a bot, willing to play its own board and unwilling
   to play any other."
  []
  (assert (some #(= "NEURON" (:name %)) (bots/list-bots "organism"))
          "NEURON is not listed among organism's bots")
  (assert (bots/bot? "organism" "NEURON-A")
          "a seated NEURON-A is not recognised as a bot")
  (assert (bots/get-agent-step+key "organism" "NEURON-A")
          "NEURON-A resolves to no step function")
  (assert (bots/plays? "organism" "NEURON" (create))
          "NEURON will not play the board it was trained for")
  (assert (not (bots/plays? "organism" "NEURON" (create ["orb" "mass"] 3)))
          "NEURON should decline a two-player board, not answer with moves for another")
  (println "registry: NEURON is listed, resolves to the trained network, and declines other boards"))

(defn compare-choices
  "The action indices each engine offers at this position. Funding a growth is
   one choice here and a run of picks there, so it is compared by which growers
   may pay rather than by whole allocations."
  [settings game phase choices]
  (let [served (set (get (native/ask settings {"position" (native/position game)}) "legal"))
        [index-of spaces] (native/space-index game)
        ours (if (= :grow-from phase)
               (let [donors (set (map index-of (mapcat keys (keys choices))))]
                 (if (empty? donors) #{(+ (count spaces) 14)} donors))
               (set (keep (partial native/action-for game phase) (keys choices))))]
    [ours served]))

(defn walk
  "Play one game, checking the bot at every decision and then moving on at
   random.

   A trained network answers the same position the same way every time, so
   letting it play both sides would replay one game however many are asked for.
   Walking randomly visits a different game each seed while still putting the
   bot's translation through every position on the way."
  [settings seed steps-cap play?]
  (let [random (java.util.Random. seed)]
    (loop [game (create) steps 0 checks 0]
      (let [[phase choices] (choice/find-state game)]
        (cond
          (or (empty? choices) (game/victory? game) (> steps steps-cap))
          [game steps checks]

          (or (contains? choices :advance) (= 1 (count choices)))
          (recur (second (first choices)) (inc steps) checks)

          :else
          (let [[ours served] (compare-choices settings game phase choices)]
            (when-not (= ours served)
              (throw (ex-info "the two engines offer different moves"
                              {:phase phase :round (get-in game [:state :round])
                               :ours (sort ours) :served (sort served)})))
            (let [[key played] (native/agent-step+key game settings)]
              (when-not (contains? choices key)
                (throw (ex-info "the bot chose something this game is not offering"
                                {:phase phase :chose (pr-str key)})))
              (recur (if play?
                       played
                       (nth (vals choices) (.nextInt random (count choices))))
                     (inc steps) (inc checks)))))))))

(defn -main
  [& args]
  (let [games (Integer/parseInt (or (first args) "3"))
        ;; A random walk plays the loose original rules, where sitting and
        ;; eating goes on as long as it is allowed to; cap it rather than wait.
        steps-cap (Integer/parseInt (or (second args) "1200"))
        ;; "play" lets the network take the moves it picks, which is a real game
        ;; rather than a walk — one game, since it plays each position the same
        ;; way every time.
        play? (= "play" (nth args 2 nil))
        ;; A shallow search: this is checking that the two engines describe the
        ;; same game, not how well the network plays it.
        settings (assoc (native/settings) :sims 16)]
    (check-registry)
    (loop [seed 0 decisions 0 compared 0]
      (if (>= seed games)
        (println (format (str "native bot parity passed: %d games, %d decisions, "
                              "%d positions where both engines offered the same moves "
                              "and the bot's move was one of them")
                         games decisions compared))
        (let [[final steps checks] (walk settings seed steps-cap play?)]
          (println (format "game %d: %d decisions, %d checked, winner %s"
                           (inc seed) steps checks (pr-str (game/victory? final))))
          (recur (inc seed) (+ decisions steps) (+ compared checks)))))))
