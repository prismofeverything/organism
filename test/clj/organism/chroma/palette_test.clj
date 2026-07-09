(ns organism.chroma.palette-test
  "Part 1 — prove RYB+OPG is genuinely testable end-to-end. The :RYB palette already
   lives in the engine; this guards the whole path so a future change (or a stale
   build) can't silently drop you back onto CMY. No Mongo needed — engine + the
   pr-str/read-string snapshot round-trip the server uses for persistence."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [organism.chroma.engine :as e]))

(def ^:private ryb-only #{"O" "P"})   ; orange + purple exist ONLY in the RYB order
(def ^:private cmy-only #{"C" "M"})   ; cyan + magenta exist ONLY in the CMY order

(defn- seeded-colors
  "All chit colors painted onto the board at game start."
  [G]
  (set (mapcat identity (vals (:stacks G)))))

(deftest ryb-palette-applies-end-to-end
  (let [specs [{:isBot false :name "You"} {:isBot true :name "Bot 2"}]
        Gryb (e/new-game specs {:palette :RYB :depth 3 :seed 7 :removed e/trim-cells})
        Gcmy (e/new-game specs {:palette :CMY :depth 3 :seed 7 :removed e/trim-cells})]

    (testing "the game records the chosen palette"
      (is (= :RYB (:palette Gryb)))
      (is (= :CMY (:palette Gcmy))))

    (testing "the palette order is the RYB wheel, not CMY"
      (is (= ["R" "O" "Y" "G" "B" "P"] (:order (e/palettes (:palette Gryb)))))
      (is (= ["C" "B" "M" "R" "Y" "G"] (:order (e/palettes (:palette Gcmy))))))

    (testing "seeded board colors come from the RYB palette"
      (let [seeded (seeded-colors Gryb)]
        ;; RYB-exclusive colors are present...
        (is (seq (set/intersection seeded ryb-only))
            "RYB board seeds in Orange/Purple")
        ;; ...and the CMY-exclusive colors never leak in
        (is (empty? (set/intersection seeded cmy-only))
            "no Cyan/Magenta on an RYB board")))

    (testing "classification operates in RYB color space, not CMY"
      ;; classify(mix-stack [...]) is the exact path the view's reference table uses.
      ;; Orange ('O') is a classification target ONLY under RYB — if it round-trips,
      ;; the engine is genuinely mixing on the RYB wheel for this game.
      (e/with-config Gryb
        (is (= "O" (e/classify (e/mix-stack ["O"])))
            "Orange classifies to Orange under RYB")
        (is (= "P" (e/classify (e/mix-stack ["P"])))
            "Purple classifies to Purple under RYB")
        ;; every classification stays inside the RYB result space (+ mud/white)
        (let [legal #{"R" "O" "Y" "G" "B" "P" "mud" "white"}]
          (is (every? #(contains? legal (e/classify (e/mix-stack [%])))
                      ["R" "O" "Y" "G" "B" "P"])
              "single-chit classifications are all RYB colors"))))

    (testing "the bag is denominated in the RYB colors"
      (is (= #{"R" "O" "Y" "G" "B" "P"} (set (keys (:bag Gryb))))))))

(deftest ryb-survives-the-persistence-round-trip
  ;; Chroma snapshots the whole server map with pr-str and reloads it with
  ;; read-string (see organism.persist-chroma). A keyword palette must survive that
  ;; round-trip as a keyword, or the view falls back to CMY on resume — exactly the
  ;; "RYB didn't carry over" symptom.
  (let [G (e/new-game [{:isBot false :name "You"}]
                      {:palette :RYB :depth 3 :seed 1 :removed e/trim-cells})
        server {:key "k" :state G :phase :place :bots #{} :players ["You"]}
        round-tripped (read-string (pr-str server))]
    (is (= :RYB (get-in round-tripped [:state :palette]))
        "palette is still the :RYB keyword after pr-str/read-string")
    (is (keyword? (get-in round-tripped [:state :palette])))))
