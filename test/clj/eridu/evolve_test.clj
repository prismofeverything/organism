(ns eridu.evolve-test
  "Tests for the GA harness fixes that break self-play monoculture collapse:
   the per-region diversity cap and the frozen reference panel that supplies an
   external fitness gradient."
  (:require
   [clojure.set]
   [clojure.test :refer [deftest is testing]]
   [eridu.evolve :as evolve]
   [eridu.personality :as pers]))

(defn- org [name region rep]
  {:name name :region region :avg-reputation rep})

(deftest cap-by-region-prevents-monoculture-test
  (testing "the dominant region is held to its cap when enough diversity exists"
    ;; merchant owns the top 8 scores; raider/priest trail — but there are
    ;; enough of them to fill the 5 survivor slots, so the hard cap holds.
    (let [dom (for [i (range 8)] (org (str "dom" i) "merchant-balanced" (- 100 i)))
          raid (for [i (range 4)] (org (str "raid" i) "raider-deploy" (- 60 i)))
          prie (for [i (range 4)] (org (str "prie" i) "priest-temple" (- 50 i)))
          pop (concat dom raid prie)
          ;; keep 5, cap any region at 40% -> max 2 per region
          kept (evolve/cap-by-region pop 5 #(:avg-reputation %) 0.4)
          by-region (frequencies (map :region kept))]
      (is (= 5 (count kept)) "fills all survivor slots")
      (is (<= (get by-region "merchant-balanced" 0) 2)
          "dominant region is capped at 40% even though it owns the top scores")
      (is (contains? (set (map :region kept)) "raider-deploy")
          "a lonely region survives the cull it would lose under greedy selection")
      (is (contains? (set (map :region kept)) "priest-temple")
          "a second trailing region also survives")))
  (testing "cap softens to backfill rather than shrink the population"
    ;; only 3 regions present, n-keep 5, cap 2 -> can't fill 5 without exceeding
    ;; one region's cap, so it backfills (4 empty slots would be worse).
    (let [pop (concat (for [i (range 8)] (org (str "m" i) "merchant-balanced" (- 100 i)))
                      [(org "r1" "raider-deploy" 40) (org "p1" "priest-temple" 39)])
          kept (evolve/cap-by-region pop 5 #(:avg-reputation %) 0.4)
          by-region (frequencies (map :region kept))]
      (is (= 5 (count kept)) "never leaves survivor slots empty")
      (is (<= (get by-region "merchant-balanced" 0) 3)
          "backfill stays minimal — only the slots diversity can't fill")
      (is (= #{"merchant-balanced" "raider-deploy" "priest-temple"}
             (set (map :region kept)))
          "both trailing regions still rescued")))
  (testing "overflow backfills when there aren't enough distinct regions"
    (let [pop (for [i (range 6)] (org (str "m" i) "merchant-balanced" (- 100 i)))
          kept (evolve/cap-by-region pop 4 #(:avg-reputation %) 0.4)]
      (is (= 4 (count kept))
          "still fills every slot from the only region rather than leaving gaps"))))

(deftest blended-rep-falls-back-without-panel-test
  (testing "blended-rep == avg-reputation when no panel score is attached"
    (is (= 7.0 (evolve/blended-rep {:avg-reputation 7.0}))))
  (testing "blended-rep averages in-pop and panel when both present"
    (is (= 6.0 (evolve/blended-rep {:avg-reputation 8.0 :panel-rep 4.0})))))

(deftest reference-panel-is-frozen-and-well-formed-test
  (testing "panel is the archetypes plus the three adversaries, all valid genomes"
    (is (= (+ (count pers/archetypes) 3) (count evolve/reference-panel)))
    (is (every? :name evolve/reference-panel))
    (is (= #{"Ref-FeatRacer" "Ref-Denial" "Ref-TempleEngine"}
           (clojure.set/difference (set (map :name evolve/reference-panel))
                                   (set (map :name pers/archetypes)))))
    ;; adversaries are built from existing genome dimensions (no novel keys)
    (is (every? #(contains? evolve/adversary-temple-engine %)
                (keys pers/default-weights)))))
