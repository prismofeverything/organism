(ns organism.passing-test
  "When a player may pass.

   A turn is declared for an action type, and must begin with something done:
   the type is offered only if its first action could be taken -- that action
   or a circulate -- and that first action is owed. Every action after it may
   be passed. The base game and FLOW follow the same rule, and an organism
   that can begin nothing at all still has a turn to play: it passes."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.board :as board]
   [organism.choice :as choice]
   [organism.examples :as examples]
   [organism.game :as game]))

(defn- turn-for
  [game player]
  (-> game
      game/find-organisms
      (assoc-in [:state :player-turn :player] player)
      (assoc-in [:state :player-turn :organism-turns] [])))

(defn- offered [game] (second (choice/find-state game)))

(defn- actions-done
  "Actions the current organism has finished, in the base game."
  [game]
  (count (:actions (game/get-organism-turn game))))

(defn- step
  "Take the first choice on offer that is not a pass -- doing something."
  [game]
  (let [[_ choices] (choice/find-state game)
        key (first (sort-by pr-str (remove #(or (= :pass %) (and (vector? %) (= :pass (first %))))
                                           (keys choices))))]
    (assert key (str "nothing to do but pass at " (first (choice/find-state game))))
    (:state (get choices key) (get choices key))))

(defn- advance-to
  "Step until `done?`, at most 40 times."
  [game done?]
  (loop [game game n 0]
    (cond (done? game) game
          (> n 40) (throw (ex-info "never got there" {:phase (first (choice/find-state game))}))
          :else (recur (assoc game :state (:state (step game) (step game))) (inc n)))))

;; An organism with two eaters, so an EAT turn is two actions; nothing fed.
(def two-eaters
  (-> examples/two-player-close
      (game/add-element "orb" 0 :eat [:orange 0] 0)
      (game/add-element "orb" 0 :grow [:orange 1] 0)
      (game/add-element "orb" 0 :move [:orange 2] 0)
      (game/add-element "orb" 0 :eat [:orange 3] 0)
      (turn-for "orb")))

(deftest a-type-is-offered-only-if-its-first-action-can-be-taken
  (testing "no food anywhere: moving, growing and circulating are all out, so
            only EAT can begin a turn"
    (is (= #{:eat} (set (keys (offered two-eaters))))))

  (testing "with food, GROW stands even short of a growth: it can circulate first"
    (let [fed (-> examples/two-player-close
                  (game/add-element "orb" 0 :eat [:orange 0] 1)
                  (game/add-element "orb" 0 :grow [:orange 1] 0)
                  (game/add-element "orb" 0 :move [:orange 2] 0)
                  (turn-for "orb"))]
      (is (contains? (set (keys (offered fed))) :grow))))

  (testing "the original game offers every type"
    (game/with-original-rules
      (is (= #{:eat :grow :move} (set (keys (offered two-eaters))))))))

(deftest the-first-action-is-owed-and-the-rest-may-be-passed
  (let [declared (assoc two-eaters :state (:state (get (offered two-eaters) :eat)))]
    (testing "the first action: no pass"
      (is (= :choose-action (first (choice/find-state declared))))
      (is (not (contains? (offered declared) :pass))))

    (testing "after it: the second action may be passed"
      (let [one-done (advance-to declared #(and (= 1 (actions-done %))
                                                (= :choose-action (first (choice/find-state %)))))]
        (is (contains? (offered one-done) :pass))
        (is (contains? (offered one-done) :eat) "and eating is still on offer")))))

;; ── FLOW ────────────────────────────────────────────────────────────────────

(defn- element [player organism type space food]
  {:player player :organism organism :type type :space space :food food :captures []})

(def flow-two-eaters
  (let [players ["a" "b"]
        starting (board/starting-spaces 4 2 players board/total-rings {})
        info (game/initial-players starting [5 5])]
    (-> (game/create-game (board/player-symmetry 2) (vec (take 4 board/total-rings)) info 3 false {:FLOW {}})
        (assoc-in [:state :elements]
                  {["D" 0] (element "a" 1 :eat ["D" 0] 0)
                   ["D" 1] (element "a" 1 :grow ["D" 1] 0)
                   ["D" 2] (element "a" 1 :move ["D" 2] 0)
                   ["D" 3] (element "a" 1 :eat ["D" 3] 0)
                   ["D" 9] (element "b" 9 :eat ["D" 9] 1)
                   ["D" 10] (element "b" 9 :move ["D" 10] 1)
                   ["D" 11] (element "b" 9 :grow ["D" 11] 1)})
        (assoc-in [:state :food] {})
        (assoc-in [:state :player-turn] {:player "a" :introduction {} :organism-turns [] :advance nil})
        game/find-organisms)))

(defn- flow-passes [game]
  (filter #(and (vector? %) (= :pass (first %))) (keys (offered game))))

(deftest flow-follows-the-same-rule
  (let [declare-key (first (filter #(and (vector? %) (= :eat (second %))) (keys (offered flow-two-eaters))))
        declared (assoc flow-two-eaters :state (:state (get (offered flow-two-eaters) declare-key)))]
    (testing "an organism with no food declares only EAT"
      (is (= #{:eat} (set (map second (keys (offered flow-two-eaters)))))))

    (testing "the first action: no pass"
      (is (= :flow-choose (first (choice/find-state declared))))
      (is (empty? (flow-passes declared))))

    (testing "after the first action commits: the next may be passed"
      (let [turns #(get-in % [:state :player-turn :organism-turns])
            one-done (advance-to declared #(and (= 1 (game/flow-action-index (turns %)))
                                                (= :flow-choose (first (choice/find-state %)))))]
        (is (seq (flow-passes one-done)))))))
