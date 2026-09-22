(ns universe.bot-test
  "The bot is driven through the shared registry, the same way the websocket
   layer drives it, so this covers the lookup as well as the policy.  It plays
   whole tables out: a hand-written heuristic is exactly the kind of thing that
   divides by an empty pot or asks for the strength of a hand that is not five
   cards yet, and neither shows up until it is left to run."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.bots :as bots]
   [organism.routes.universe-ws]
   [universe.deck :as deck]
   [universe.holdem :as holdem]))

(defn- seeded-deck [seed]
  (let [cards (java.util.ArrayList. ^java.util.Collection (vec deck/all-cards))]
    (java.util.Collections/shuffle cards (java.util.Random. seed))
    (vec cards)))

(defn- chips [state]
  (+ (reduce + 0 (map :stack (:players state))) (holdem/pot state)))

(deftest the-bot-is-registered
  (testing "so the create lobby can offer it"
    (is (bots/bot? "universe" "ORACLE"))
    (is (bots/bot? "universe" "ORACLE-B") "auto-suffixed instances too")
    (is (some #(= "ORACLE" (:name %)) (bots/list-bots "universe")))
    (is (fn? (bots/get-agent-step "universe" "ORACLE-C")))))

(deftest bots-play-whole-tables-without-falling-over
  (let [step (bots/get-agent-step "universe" "ORACLE")]
    (doseq [players [["ORACLE-A" "ORACLE-B"]
                     ["ORACLE-A" "ORACLE-B" "ORACLE-C"]
                     ["ORACLE-A" "ORACLE-B" "ORACLE-C" "ORACLE-D" "ORACLE-E"]]
            seed    (range 4)]
      (let [rng   (java.util.Random. seed)
            start (holdem/create-game players)]
        (loop [s start hands 0]
          (cond
            (or (holdem/game-over? s) (> hands 300)
                (< (count (holdem/with-chips s)) 2))
            (is (= (chips start) (chips s))
                (str (count players) " players, seed " seed))

            :else
            (recur
             (loop [st (holdem/start-hand s (seeded-deck (.nextLong rng))) guard 0]
               (if (or (holdem/hand-over? st) (> guard 400))
                 st
                 (let [next-state (step st)]
                   (is (not (identical? st next-state))
                       "the bot chose something illegal and the state stood still")
                   (recur next-state (inc guard)))))
             (inc hands))))))))

(deftest a-bot-table-reaches-a-winner
  (let [step (bots/get-agent-step "universe" "ORACLE")
        rng  (java.util.Random. 99)]
    (loop [s (holdem/create-game ["ORACLE-A" "ORACLE-B"]) hands 0]
      (cond
        (holdem/game-over? s)
        (do (is (some? (:winner s)))
            (is (= 2000 (chips s)) "and the winner holds every chip"))

        (> hands 300) (is false "two bots could not finish a table in 300 hands")

        :else
        (recur (loop [st (holdem/start-hand s (seeded-deck (.nextLong rng))) g 0]
                 (if (or (holdem/hand-over? st) (> g 400)) st (recur (step st) (inc g))))
               (inc hands))))))
