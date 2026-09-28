(ns organism.attribution-test
  "The attribution check, run as part of the suite.

   `organism.scripts.rules-attribution` walks the original game and asks, at every
   position, whether each difference the tightened rules make is one a named rule
   meant to make. The script takes a large sample; this takes a small one, so the
   check runs on every `lein test` rather than only when somebody remembers.

   It earns its place by failing on the real bug: with the broken filter restored
   — asking whether an action can be taken this instant rather than at any point
   in the turn — this reports unjustified removals of both GROW and MOVE. Nothing
   else in the suite did, because everything else compared the implementation to
   itself."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.scripts.rules-attribution :as attribution]))

(deftest every-difference-the-rules-make-is-one-a-rule-meant-to-make
  (let [reports (mapcat #(attribution/walk % 90) (range 2))
        removed (mapcat :removed reports)
        unattributed (filter (comp nil? :rule) removed)
        unjustified (filter :unjustified removed)
        invented (mapcat :added reports)]

    (testing "the sample actually exercised the rules"
      (is (pos? (count reports))
          "no position where the tightened rules differ — the walk found nothing to check")
      (is (pos? (count removed))
          "no move removed anywhere, so nothing was attributed"))

    (testing "every removed move is owned by a named rule"
      (is (empty? unattributed)
          (str "a move the original game allows that no rule removes: "
               (pr-str (take 3 unattributed)))))

    (testing "and every owner can justify the removal"
      ;; This is the assertion the six-day bug would have tripped. Owning a
      ;; removal is not the same as being entitled to it: require-useful-action
      ;; claims only to remove types the organism cannot use, and a search of the
      ;; base game is what holds it to that.
      (is (empty? unjustified)
          (str "a rule removed a move it cannot justify: "
               (pr-str (take 3 unjustified)))))

    (testing "and the tightened rules never invent a move"
      (is (empty? invented)
          (str "the tightened rules offered something the original game does not: "
               (pr-str (take 3 invented)))))))
