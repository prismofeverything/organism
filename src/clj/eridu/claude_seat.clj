(ns eridu.claude-seat
  "Interactive harness that lets Claude drive ONE seat of an Eridu game,
   reading exactly the same presented choice set a human/bot sees from
   choice/find-state-raw. Opponents are driven by the evolved baseline
   personalities via personality-step. READ-ONLY w.r.t. game rules: every
   actor (Claude included) may pick only from the find-state-raw option set.

   Protocol (line-based, stdin -> stdout):
     start4   -> new 4-seat game, Claude = seat P1, advance to my first decision
     startsolo-> new solo game
     <int>    -> Claude picks option <int> from the last presented list
     state    -> re-print current decision
   After every command the harness prints a rendered decision block ending
   with a line '===END===' (or '===GAMEOVER===')."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [eridu.game :as game]
   [eridu.cards :as cards]
   [eridu.choice :as choice]
   [eridu.personality :as pers]))

(def protected game/bot-protected-phases)

(def ^:private state* (atom nil))           ;; current game state
(def ^:private my-seat* (atom nil))         ;; the seat Claude drives
(def ^:private pmap* (atom {}))             ;; player-key -> personality weights
(def ^:private choices* (atom []))          ;; ordered [key next-state] for current decision
(def ^:private logfile* (atom nil))         ;; path to play-log artifact

(defn- log-lines [state] (mapv :message (:log state)))

(defn- new-log-msgs
  "Log messages added by next-state beyond base-state."
  [base next-state]
  (let [b (count (:log base))]
    (mapv :message (subvec (vec (:log next-state)) (min b (count (:log next-state)))))))

(defn- pdata [state pk] (game/player-data state pk))

(def ^:private space-action-abbr
  {1 "tk/sl/sl/tv" 2 "tk/dp/dp/tv" 3 "tk/tv/tv/TEMPLE" 4 "tk/in/in/dp"
   5 "tk/sl/tv/tv" 6 "tk/TEMPLE/TEMPLE/in" 7 "dp/sl/in/TEMPLE"})

(defn- player-line [state pk]
  (let [d (pdata state pk)
        r (:roles d)]
    (format "  %-14s rep=%d (A%d/G%d) M%d P%d R%d L%d | res T%d Po%d Go%d Ge%d | wild%d | feats:%d | caravan:%s | astros@wheel%s%s"
            (str (name pk) (when (= pk @my-seat*) " *YOU*"))
            (min (:amity d 0) (:glory d 0)) (:amity d 0) (:glory d 0)
            (get r :merchant 1) (get r :priest 1) (get r :raider 1) (get r :leader 1)
            (get-in d [:resources :tools] 0) (get-in d [:resources :pottery] 0)
            (get-in d [:resources :gold] 0) (get-in d [:resources :gems] 0)
            (:wild-points d 0)
            (count (filter #(some #{pk} (val %)) (:contest-claims state {})))
            (str (:caravan d))
            (pr-str (:astronomers d))
            (let [pl (game/count-temples-placed d) fd (game/count-face-down-temples d)]
              (format " | temples placed=%d (up%d/dn%d) sup=%s" pl (- pl fd) fd (str (:temples-supply d)))))))

(defn- feat-desc [id]
  (let [c (get cards/bonus-contests-by-id id)]
    (str (:name c "?") " — " (:description c "?"))))

(defn- feats-line [state]
  (let [me @my-seat*
        tgt (:target-feats (pdata state me))]
    (str "  Your target feats:\n"
         (str/join "\n" (map (fn [t] (str "    " (name (:id t)) ": " (feat-desc (:id t)))) tgt)))))

(defn- contests-line [state]
  (str "  Contests in play:\n"
       (str/join "\n"
                 (map (fn [c]
                        (str "    " (name (:id c)) ": " (:description c (feat-desc (:id c)))
                             (when-let [cl (seq (get-in state [:contest-claims (:id c)]))]
                               (str "  [CLAIMED by " (str/join "," (map name cl)) "]"))))
                      (:contests state [])))))

(defn- render-decision []
  (let [state @state*
        [phase choices] (choice/find-state-raw state)
        ordered (vec choices)
        me @my-seat*]
    (reset! choices* ordered)
    (let [sb (StringBuilder.)]
      (.append sb (format "\n========== ROUND %s  turn-in-round %s  | phase %s | to-move: %s%s ==========\n"
                          (:round state) (:turn-in-round state) (name phase)
                          (name (game/current-player state))
                          (if (= (game/current-player state) me) "  <<< YOUR DECISION" "")))
      (doseq [pk (:turn-order state)] (.append sb (str (player-line state pk) "\n")))
      (.append sb (str (feats-line state) "\n"))
      (.append sb (str (contests-line state) "\n"))
      ;; wheel legend + die context
      (.append sb "  WHEEL (clockwise 1->..7->1; temple actions on 3,6,7):\n")
      (.append sb "    1:tk-gems/tools,sl,sl,tv | 2:tk-gold/pot,dp,dp,tv | 3:tk-pot/gems,tv,tv,TEMPLE\n")
      (.append sb "    4:tk-pot/tools,in,in,dp | 5:tk-tools/gold,sl,tv,tv | 6:tk-gems/gold,TEMPLE,TEMPLE,in | 7:dp,sl,in,TEMPLE\n")
      (when-let [dv (get-in state [:player-turn :die-value])]
        (.append sb (format "  die-value=%s  landed-space=%s (%s)\n" dv (get-in state [:player-turn :landed-space])
                            (get space-action-abbr (get-in state [:player-turn :landed-space]) "?"))))
      (.append sb (format "  OPTIONS (%d):\n" (count ordered)))
      (doseq [[i [k ns]] (map-indexed vector ordered)]
        (let [msgs (new-log-msgs state ns)
              desc (if (seq msgs) (str/join " ; " (take-last 2 msgs)) "(no log delta)")]
          (.append sb (format "   [%d] key=%s  =>  %s\n" i (pr-str k) desc))))
      (.append sb "===END===\n")
      (println (.toString sb)))))

(defn- advance-until-my-decision
  "Auto-play: take single choices and opponent moves until it is my seat's
   turn with >1 choice, or the game ends."
  []
  (loop [guard 0]
    (let [state @state*
          [phase choices] (choice/find-state-raw state)
          cp (game/current-player state)]
      (cond
        (> guard 10000) (println "WARN: guard tripped")
        (:game-over state) :over
        (or (= phase :game-over) (empty? choices)) :over
        ;; My genuine decision point
        (and (= cp @my-seat*) (> (count choices) 1)) :decide
        :else
        (let [step (if (= cp @my-seat*)
                     ;; forced single choice for me: take it (log it)
                     (let [[k ns] (first choices)]
                       (when @logfile*
                         (spit @logfile* (format "[auto/you] %s -> %s\n" (name phase)
                                                 (str/join "; " (new-log-msgs state ns)))
                               :append true))
                       [k ns])
                     ;; opponent: personality-step, fallback first choice
                     (or (pers/personality-step state (get @pmap* cp pers/default-weights))
                         (first choices)))
              [_ ns] step]
          (if ns
            (do (reset! state* (choice/advance-through-trivial ns protected))
                (recur (inc guard)))
            :stuck))))))

(defn- load-baseline-personalities [n]
  (let [data (edn/read-string (slurp (io/resource "eridu/evolved-baseline.edn")))
        orgs (->> (:organisms data)
                  (sort-by :elo >)
                  (take n))]
    (mapv :personality orgs)))

(defn- start! [mode]
  (let [solo? (= mode :solo)
        ;; opponents from top evolved baseline
        opp-pers (load-baseline-personalities 3)
        my-key "P1-YOU"
        opp-keys (mapv #(str "P" (+ 2 %) "-" (subs (:name %2) 0 (min 8 (count (:name %2)))))
                       (range) opp-pers)
        keys (if solo? [my-key] (into [my-key] opp-keys))
        pm (if solo? {} (zipmap opp-keys opp-pers))
        init (if solo? (game/initial-solo-state my-key) (game/initial-state keys))
        ;; cache personality weights for opponents (feat-claiming logic reads it)
        init (reduce (fn [s [pk w]]
                       (assoc-in s [:players pk :personality-cache]
                                 (select-keys w [:tempo :feat-awareness :prefer-onetime-bonus
                                                 :feat-sequence :feat-closure-urgency])))
                     init pm)
        ;; HARNESS NOTE: feat-claiming is auto-resolved in game/advance-turn ONLY
        ;; for seats that carry a :personality-cache (is-bot?). The headless
        ;; find-state-raw state machine exposes NO claim choice — the human claim
        ;; path (handle-claim-feat!) lives only in the cljs UI. So to let Claude's
        ;; seat actually SCORE the feats it meets, we give it an eager claim cache
        ;; (low tempo = claim early, high awareness). This changes NO game rule;
        ;; it only routes claim-resolution through the same shared primitive bots
        ;; use. Logged as a Phase-0 finding.
        init (assoc-in init [:players my-key :personality-cache]
                       {:tempo 0.05 :feat-awareness 0.95 :prefer-onetime-bonus 0.5
                        :feat-sequence 0.5 :feat-closure-urgency 0.9})]
    (reset! my-seat* my-key)
    (reset! pmap* pm)
    (reset! logfile* (str "/tmp/eridu-playlog-" (if solo? "solo" "4p") ".txt"))
    (spit @logfile* (format "=== Eridu play log (%s) — Claude drives %s ===\n"
                            (if solo? "SOLO" "4-SEAT") my-key))
    (reset! state* (choice/advance-through-trivial init protected))
    (println (format "Started %s game. Your seat: %s. Opponents: %s"
                     (if solo? "SOLO" "4-SEAT") my-key (str/join ", " opp-keys)))
    (when-not solo?
      (doseq [pk opp-keys] (println (format "  %s = %s (elo-ranked baseline)" pk pk))))
    (let [r (advance-until-my-decision)]
      (if (= r :over) (do (println "===GAMEOVER===")) (render-decision)))))

(defn- pick! [idx]
  (let [ordered @choices*]
    (if-not (and (>= idx 0) (< idx (count ordered)))
      (println (format "Invalid index %d (have %d options). ===END===" idx (count ordered)))
      (let [[k ns] (nth ordered idx)
            state @state*
            msgs (new-log-msgs state ns)]
        (when @logfile*
          (spit @logfile* (format "[YOU pick %d] key=%s -> %s\n" idx (pr-str k)
                                  (str/join "; " msgs)) :append true))
        (reset! state* (choice/advance-through-trivial ns protected))
        (let [r (advance-until-my-decision)]
          (if (= r :over)
            (do (println "Game ended.")
                (doseq [pk (:turn-order @state*)] (println (player-line @state* pk)))
                (println "===GAMEOVER==="))
            (render-decision)))))))

(defn -main [& _]
  (println "eridu.claude-seat ready. Commands: start4 | startsolo | <int> | state | quit")
  (println "===READY===")
  (loop []
    (let [line (read-line)]
      (when line
        (let [cmd (str/trim line)]
          (try
            (cond
              (= cmd "start4") (start! :multi)
              (= cmd "startsolo") (start! :solo)
              (= cmd "state") (render-decision)
              (= cmd "quit") (do (println "bye") (System/exit 0))
              (re-matches #"-?\d+" cmd) (pick! (Integer/parseInt cmd))
              (str/blank? cmd) nil
              :else (println (str "Unknown cmd: " cmd " ===END===")))
            (catch Exception e
              (println (str "ERROR: " (.getMessage e)))
              (.printStackTrace e)
              (println "===END==="))))
        (flush)
        (recur)))))
