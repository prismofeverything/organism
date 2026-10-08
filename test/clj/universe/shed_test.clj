(ns universe.shed-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [universe.deck :as deck]
   [universe.shed :as shed]))

(defn- c [color shape number] (deck/card color shape number))

(defn- seeded-deck [seed]
  (let [cards (java.util.ArrayList. ^java.util.Collection deck/all-cards)]
    (java.util.Collections/shuffle cards (java.util.Random. seed))
    (vec cards)))

(deftest ranking
  (testing "number first, then purple over green over yellow"
    (is (< (shed/rank (c :purple :eye 1)) (shed/rank (c :yellow :eye 2))))
    (is (> (shed/rank (c :purple :eye 3)) (shed/rank (c :green :star 3))))
    (is (= (shed/rank (c :green :eye 3)) (shed/rank (c :green :star 3))) "shape never ranks")))

(deftest patterns
  (testing "a set must share a color or a shape"
    (is (= :set (:form (shed/classify [(c :purple :eye 3) (c :purple :star 3)]))))
    (is (= #{:shape} (:flavors (shed/classify [(c :purple :eye 3) (c :green :eye 3)]))))
    (is (nil? (shed/classify [(c :purple :eye 3) (c :green :star 3)]))))
  (testing "runs are three to five consecutive, one color or one shape"
    (is (= :run (:form (shed/classify [(c :green :eye 1) (c :green :star 2) (c :green :helix 3)]))))
    (is (nil? (shed/classify [(c :green :eye 1) (c :green :star 2)])))
    (is (nil? (shed/classify [(c :green :eye 1) (c :green :star 2) (c :green :helix 4)])))
    (is (nil? (shed/classify [(c :green :eye 1) (c :purple :star 2) (c :yellow :helix 3)])))
    (is (= #{:color :shape} (:flavors (shed/classify (map #(c :purple :eye %) [1 2 3 4 5]))))))
  (testing "five of a number is no set"
    (is (nil? (shed/classify [(c :purple :eye 2) (c :purple :star 2) (c :purple :helix 2)
                               (c :purple :pyramid 2) (c :green :eye 2)])))))

(deftest beating
  (let [lead (shed/classify [(c :green :eye 1) (c :green :star 2) (c :green :helix 3)])]
    (is (shed/beats? (shed/classify [(c :yellow :eye 2) (c :yellow :star 3) (c :yellow :helix 4)]) lead))
    (testing "a shape run does not answer a color run"
      (is (not (shed/beats? (shed/classify [(c :yellow :eye 2) (c :green :eye 3) (c :purple :eye 4)]) lead))))
    (testing "the same top number in a lighter color is lower"
      (is (not (shed/beats? (shed/classify [(c :yellow :eye 1) (c :yellow :star 2) (c :yellow :helix 3)]) lead))))
    (testing "a longer run is a different play"
      (is (not (shed/beats? (shed/classify (map #(c :purple :eye %) [2 3 4 5])) lead))))))

(deftest every-play-is-a-play
  (let [hand (take 15 (seeded-deck 4))]
    (is (every? shed/classify (shed/plays hand)))
    (is (= 15 (count (filter #(= 1 (count %)) (shed/plays hand)))))))

(defn- cards-in [s]
  (+ (reduce + (map count (vals (:hands s))))
     (count (mapcat :cards (filter #(= :play (:event %)) (:log s))))))

(deftest bots-play-to-a-winner
  (doseq [seed (range 60) n [2 3 4 5 6]]
    (let [dealt (* n (shed/hand-size n))
          s     (loop [s (shed/start (shed/create-game (map str (range n))) (seeded-deck seed)) i 0]
                  (assert (= dealt (cards-in s)) "no card created or lost")
                  (if (or (shed/game-over? s) (> i 2000))
                    s
                    (let [t (shed/act s (:to-act s) (shed/bot-action s))]
                      (assert (not (identical? s t)) "the bot only plays legal moves")
                      (recur t (inc i)))))]
      (is (shed/game-over? s) (str "seed " seed " at " n))
      (is (empty? (get-in s [:hands (shed/seat-of s (:winner s))]))))))

(deftest refusals
  (let [s (shed/start (shed/create-game ["a" "b"]) (seeded-deck 9))
        who (:to-act s)
        other (- 1 who)]
    (is (identical? s (shed/act s who {:action :pass})) "the leader cannot pass")
    (is (identical? s (shed/act s other {:action :pass})) "out of turn")
    (is (identical? s (shed/act s who {:action :play :cards [(first (get-in s [:hands other]))]}))
        "not your card")))

(deftest the-view-hides-hands
  (let [s (shed/start (shed/create-game ["a" "b" "c"]) (seeded-deck 5))
        v (shed/view s "b")]
    (is (= #{1} (set (keys (:hands v)))))
    (is (= {0 15 1 15 2 15} (:hand-counts v)))))
