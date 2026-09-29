(ns organism.history-test
  "A game is its log, and only the server writes it. Driven here through the
   real websocket handlers by browsers that behave as the page does.

   A round-10 game was rewound to its opening move. The server used to store
   whatever whole state a browser sent, keep a second copy of the history in
   memory beside the database, and let undo change one without the other. Now
   a browser sends only the path of choice keys it took, the server replays it
   through the rules and appends the result to the one log, and every browser
   mirrors that log. This walks long random sequences -- moves, undos, clears,
   reloads, browsers that are behind, players out of turn, pages from before
   the change -- and after every single step holds the whole system to:

     the server keeps no positions of its own; the log is the only copy
     the present is the log's newest entry
     a move appends exactly what the rules make of the path, and nothing else
     an undo removes exactly one entry, and never the opening position
     a refused message changes nothing at all
     every browser's copy of the history is the log, entry for entry

   The database is an atom, so the test sees exactly what was written."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.choice :as choice]
   [organism.game :as game]
   [organism.game-log :as game-log]
   [organism.history :as history]
   [organism.persist :as persist]
   [organism.routes.websockets :as ws]))

;; ── A database and a network that are atoms ──────────────────────────────

(defn- browsers [] (atom {:ch-a {:history nil} :ch-b {:history nil}}))

(defn- deliver!
  "What a page does with a message: mirror the log, or ask for all of it."
  [pages db-log ch msg]
  (case (:type msg)
    "initialize" (swap! pages assoc-in [ch :history] (vec (:history msg)))
    "sync" (swap! pages assoc-in [ch :history] (vec (:history msg)))
    "game-state" (let [h (get-in @pages [ch :history])]
                   (if-let [next (history/follow-log h (:length msg) (:game msg))]
                     (swap! pages assoc-in [ch :history] next)
                     ;; the page asks for the log, and the server sends it
                     (swap! pages assoc-in [ch :history] (vec @db-log))))
    nil))

(defmacro ^:private on-a-fake-server
  [& body]
  `(let [~'db-log (atom [])
         ~'record (atom nil)
         ~'pages (browsers)]
     (with-redefs [ws/games (atom {:games {}})
                   ws/send! (fn [ch# msg#] (deliver! ~'pages ~'db-log ch# msg#))
                   ws/send-channels! (fn [chs# msg#]
                                       (doseq [ch# chs#] (deliver! ~'pages ~'db-log ch# msg#)))
                   organism.leaderboard/rate-later! (fn [& _#])
                   game-log/entries (fn [_# _#] @~'db-log)
                   game-log/present (fn [_# _#] (last @~'db-log))
                   game-log/recent (fn [_# _# n#] (vec (take-last n# @~'db-log)))
                   game-log/length (fn [_# _#] (count @~'db-log))
                   game-log/append! (fn [_# _# s#] (count (swap! ~'db-log conj (game-log/position s#))))
                   game-log/undo! (fn [_# _#] (when (>= (count @~'db-log) 2)
                                                (last (swap! ~'db-log pop))))
                   persist/create-game! (fn [_# gs#]
                                          (reset! ~'db-log [(get-in gs# [:game :state])])
                                          (reset! ~'record gs#))
                   persist/load-game (fn [_# _#]
                                       (when-let [r# @~'record]
                                         (-> r# (assoc :history @~'db-log)
                                             (assoc-in [:game :state] (last @~'db-log)))))
                   persist/find-player-game (fn [& _#] {:witness (count @~'db-log)})
                   persist/game-exists? (fn [_# _#] (some? @~'record))
                   persist/find-open-game (fn [& _#] nil)
                   persist/create-open-game! (fn [& _#])
                   persist/remove-open-game! (fn [& _#])
                   persist/update-player-games! (fn [& _#])
                   persist/complete-game! (fn [& _#])
                   persist/store-witness! (fn [& _#])]
       ~@body)))

(defn- registry [] (get-in @ws/games [:games "g"]))
(defn- rules [] (:game (registry)))
(defn- present-game [log] (assoc (rules) :state (last log)))
(defn- current-rules [] (assoc (rules) :state nil))

(defn- start!
  "Open a lobby for two, with both browsers watching, and begin it."
  [mutations]
  (swap! ws/games assoc-in [:games "g"]
         {:key "g" :channels #{:ch-a :ch-b} :history [] :chat []
          :invocation {:ring-count 5 :player-count 2 :players ["a" "b"]
                       :colors [] :organism-victory 3 :player-captures [5 5]
                       :mutations mutations}})
  (ws/begin-game! nil "g" "a"))

;; ── Choosing, as the page does ───────────────────────────────────────────

(defn- a-path
  "A choice a page could make from the log's present: the steps that happen
   on their own, then one of the choices on offer. Its path, as sent."
  [log ^java.util.Random rng]
  (let [[g _ choices] (binding [*out* (java.io.StringWriter.)]
                        (choice/find-next-choices (present-game log)))
        keys (sort-by pr-str (remove #(and (vector? %) (= :cancel (first %))) (keys choices)))]
    (when (seq keys)
      (let [chosen (get choices (nth keys (.nextInt rng (count keys))))]
        (choice/state-path (:state (or chosen g)))))))

(defn- on-the-clock [log] (game/current-player {:state (last log)}))
(defn- other [player] (if (= player "a") "b" "a"))

;; ── The walk ─────────────────────────────────────────────────────────────

(defn- step!
  [db-log ^java.util.Random rng]
  (let [log @db-log
        roll (.nextInt rng 100)]
    (cond
      (< roll 60)
      (if-let [path (a-path log rng)]
        (do (ws/choose! nil (on-the-clock log) "g" :ch-a {:path path}) [:move path])
        [:nothing])

      (< roll 72) (do (ws/walk-history nil (on-the-clock log) "g" :ch-a {}) [:undo])
      (< roll 77) (do (ws/clear-player-turn nil (on-the-clock log) "g" :ch-a {}) [:clear])

      (< roll 84)
      ;; a page that is behind: a path worked out from an earlier position
      (let [earlier (vec (butlast log))]
        (if (seq earlier)
          (let [old (subvec earlier 0 (inc (.nextInt rng (count earlier))))
                path (a-path old rng)]
            (if path
              (do (ws/choose! nil (on-the-clock log) "g" :ch-a {:path path}) [:behind path])
              [:nothing]))
          [:nothing]))

      (< roll 89)
      (if-let [path (a-path log rng)]
        (do (ws/choose! nil (other (on-the-clock log)) "g" :ch-b {:path path}) [:out-of-turn])
        [:nothing])

      (< roll 93)
      ;; a page from before the change sends a whole state -- here an old one
      (do (ws/stale-state! nil (on-the-clock log) "g" :ch-a {:game (first log)})
          [:old-page])

      :else
      ;; every tab closes and one reopens
      (do (swap! ws/games update :games dissoc "g")
          (ws/connect! {:db nil :game-key "g" :player "a"} :ch-a)
          (swap! ws/games update-in [:games "g" :channels] conj :ch-b)
          [:reload]))))

(defn- walk [mutations seed steps]
  (let [rng (java.util.Random. seed)]
    (on-a-fake-server
      (start! mutations)
      (is (= 1 (count @db-log)) "a game begins with its opening position in the log")
      (dotimes [n steps]
        (let [before @db-log
              [what path] (step! db-log rng)
              after @db-log
              where (str what " at step " n ", seed " seed)]
          (is (nil? (get-in (registry) [:game :state])) (str "no position kept in memory, " where))
          (is (nil? (:history (registry))) (str "no history kept in memory, " where))
          (doseq [ch [:ch-a :ch-b]]
            (is (= after (get-in @pages [ch :history]))
                (str (name ch) "'s copy is the log after " where)))
          (case what
            :move (let [expected (some-> (game-log/apply-path (present-game before) path) :state)]
                    (is (= after (if expected (conj before expected) before))
                        (str "a move appends what the rules make of it " where)))
            :behind (let [expected (some-> (game-log/apply-path (present-game before) path) :state)]
                      (is (= after (if expected (conj before expected) before))
                          (str "an old path is only ever applied to the present " where)))
            :undo (do (is (or (= after before) (= after (pop before)))
                          (str "an undo removes exactly one entry " where))
                      (is (seq after) (str "never the opening position " where)))
            :clear (is (or (= after before) (= (pop after) before))
                       (str "a clear adds exactly one entry " where))
            (:out-of-turn :old-page :reload) (is (= after before) (str "changes nothing, " where))
            nil)))
      (is (< 20 (count @db-log)) (str "the walk really plays the game, seed " seed))
      (is (< 0 (count (distinct (map :round @db-log)))) "across rounds"))))

(deftest the-log-holds-under-random-play
  (testing "an ordinary game"
    (doseq [seed (range 4)] (walk {} seed 220)))
  (testing "a FLOW game"
    (doseq [seed (range 4)] (walk {:FLOW {}} seed 220))))

;; ── The cases that destroyed a game, one by one ──────────────────────────

(deftest undo-on-a-fresh-game-keeps-the-opening-position
  (on-a-fake-server
    (start! {:FLOW {}})
    (let [opening @db-log]
      (dotimes [_ 3] (ws/walk-history nil "a" "g" :ch-a {}))
      (is (= opening @db-log))
      (is (= opening (get-in @pages [:ch-a :history]))))))

(deftest an-old-whole-state-is-never-stored
  (testing "the round-10 game: a page sending its opening position as the
            present no longer rewinds anything"
    (on-a-fake-server
      (start! {:FLOW {}})
      (let [rng (java.util.Random. 5)]
        (dotimes [_ 40]
          (when-let [path (a-path @db-log rng)]
            (ws/choose! nil (on-the-clock @db-log) "g" :ch-a {:path path}))))
      (let [log @db-log]
        (ws/stale-state! nil (on-the-clock log) "g" :ch-a {:game (first log)})
        (is (= log @db-log))
        (is (= log (get-in @pages [:ch-a :history])) "and the page is put back on the log")))))

(deftest a-path-the-rules-do-not-offer-is-refused
  (on-a-fake-server
    (start! {})
    (let [log @db-log]
      (ws/choose! nil "a" "g" :ch-a {:path [:no-such-choice]})
      (is (= log @db-log)))))

(deftest a-move-is-what-the-server-makes-of-it
  (testing "the browser sends only keys; the stored position is the server's own"
    (on-a-fake-server
      (start! {})
      (let [path (a-path @db-log (java.util.Random. 1))
            expected (:state (game-log/apply-path (present-game @db-log) path))]
        (ws/choose! nil "a" "g" :ch-a {:path path})
        (is (= expected (last @db-log)))))))

;; ── flowflow: a live game created a second time ──────────────────────────

(deftest a-late-create-page-cannot-change-or-recreate-a-live-game
  (testing "the lobby began on its own; the create page, still open, then sent
            its settings with FLOW and pressed CREATE. That used to rewrite the
            live game's rules in memory and build it again with no name."
    (on-a-fake-server
      (let [created (atom 0)]
        (with-redefs [persist/create-game! (let [real persist/create-game!]
                                             (fn [db gs] (swap! created inc) (real db gs)))]
          (start! {})
          (let [log @db-log
                rules-before (rules)
                invocation (:invocation (registry))
                late {:ring-count 5 :player-count 2 :players ["a" "b"] :colors []
                      :organism-victory 3 :player-captures [5 5] :mutations {:FLOW {}}}]
            (ws/update-create-game nil "a" "g" :ch-a {:invocation late})
            (ws/update-open-game nil "a" "g" :ch-a {:invocation late})
            (ws/trigger-creation nil "a" "g" :ch-a {})
            (is (= 1 @created) "the game is not created a second time")
            (is (= rules-before (rules)) "its rules are untouched")
            (is (= invocation (:invocation (registry))) "and so are its settings")
            (is (not (game/flow? (current-rules)))
                "it stays the game it began as")
            (is (= log @db-log) "and its log is untouched")))))))

(deftest a-game-with-no-name-is-never-stored
  (is (thrown? clojure.lang.ExceptionInfo
               (persist/create-game! nil {:key nil :invocation {} :game {:state {}}})))
  (is (thrown? clojure.lang.ExceptionInfo
               (persist/create-game! nil {:key "" :invocation {} :game {:state {}}}))))
