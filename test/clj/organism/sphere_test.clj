(ns organism.sphere-test
  "The SPHERE mutation's boards, and the ordinary game played on them."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.board :as board]
   [organism.choice :as choice]
   [organism.game :as game]
   [organism.sphere :as sphere]
   [organism.sphere-boards :as boards]))

(defn- distances
  "Steps from `sources` to every space."
  [adjacencies sources]
  (loop [seen (zipmap sources (repeat 0)) frontier (vec sources)]
    (if (empty? frontier)
      seen
      (let [next (for [s frontier n (get adjacencies s) :when (not (contains? seen n))] [n (inc (seen s))])
            seen' (into seen (reverse next))]
        (recur seen' (vec (distinct (map first next))))))))

(deftest boards-are-sound
  (doseq [[players {:keys [adjacencies centers homes positions ring-count player-gap]}] boards/boards]
    (testing (str players " players")
      (testing "neighbours are mutual, and the board is one piece"
        (doseq [[s ns] adjacencies n ns]
          (is (some #{s} (get adjacencies n)) [s n]))
        (is (= (count adjacencies) (count (distances adjacencies [(first centers)])))))
      (testing "the sphere's few short-handed cells total twelve, as Euler says"
        (is (= 12 (reduce + (map #(- 6 (count %)) (vals adjacencies))))))
      (testing "ring A is the cores, each later ring a step further from them"
        (let [d (distances adjacencies centers)]
          (doseq [[[ring _ :as s] steps] d]
            (is (= ring (nth board/total-rings steps)) s))
          (is (= ring-count (inc (apply max (vals d)))))))
      (testing "every player has three home spaces, together, apart from the rest"
        (is (= players (count homes)))
        (is (= (* 3 players) (count (set (apply concat homes)))))
        (doseq [home homes
                a home b home :when (not= a b)]
          (is (some #{b} (get adjacencies a))))
        (doseq [home homes]
          (let [d (distances adjacencies home)
                gap (apply min (for [other homes :when (not= other home) s other] (dec (d s))))]
            (is (= player-gap gap)))))
      (testing "every space is on the globe"
        (is (= (set (keys adjacencies)) (set (keys positions))))))))

(defn- start
  [players]
  (let [names (vec (take players board/default-player-order))
        info (game/initial-players (sphere/starting-spaces players names) (repeat players 5))]
    (sphere/create-game info 3 {:SPHERE {}})))

(deftest random-games-play-out
  (doseq [players sphere/player-counts]
    (testing (str players " players")
      (let [start (start players)]
        (is (= (:centers start) (:centers (sphere/sphere-board players))))
        (dotimes [_ 3]
          (loop [[game turn choices] (choice/find-next-choices start)
                 n 0]
            (is (every? #(contains? (:adjacencies start) %) (keys (get-in game [:state :elements]))))
            (when (and (seq choices) (< n 600) (not (get-in game [:state :winner])))
              (recur (choice/find-next-choices (rand-nth (vals choices))) (inc n)))))))))

(deftest both-cores-pay
  (let [game (start 5)
        [a b] (game/centers game)
        held (-> game
                 (game/add-element "orb" 0 :eat a 0)
                 (game/add-element "orb" 1 :eat b 0)
                 (game/start-turn "orb"))]
    (is (= 2 (count (get-in held [:state :captures "orb"]))))
    (is (= #{a b} (set (map :space (get-in held [:state :captures "orb"])))))))

(deftest the-flattened-globe
  (doseq [players sphere/player-counts]
    (let [colors (sphere/ring-colors players 10)
          names (take players board/default-player-order)
          b (sphere/generate-board colors names)]
      (testing (str players " players: every space has a place, and they do not overlap")
        (is (= (set (keys (:locations b))) (set (keys (:adjacencies (sphere/sphere-board players))))))
        (let [pts (vals (:locations b))]
          (is (every? (fn [[x1 y1]]
                        (every? (fn [[x2 y2]]
                                  (or (and (= x1 x2) (= y1 y2))
                                      (> (Math/hypot (- x2 x1) (- y2 y1)) (* 2 (:radius b)))))
                                pts))
                      pts)))
        (is (= (set names) (set (keys (:player-colors b)))))))))

(deftest the-2d-globe-keeps-neighbours-together
  (doseq [players sphere/player-counts]
    (let [{:keys [adjacencies]} (sphere/sphere-board players)
          names (take players board/default-player-order)
          views (take 12 (iterate #(sphere/turn % [137 61]) sphere/identity-view))
          away? (fn [[x _]] (neg? x))]
      (testing (str players " players: from every view, neighbours on the page sit close")
        (doseq [view views
                :let [b (sphere/generate-board (sphere/ring-colors players 10) names view)
                      locs (:locations b)]
                [a ns] adjacencies n ns
                :let [pa (get locs a) pb (get locs n)]
                :when (not (or (away? pa) (away? pb)))]
          ;; the map is true to distance from its centre and stretches only
          ;; round it, by reach/sin(reach) at most (1.8 at 105 degrees); a
          ;; neighbour torn away would be many times its own length
          (let [{:keys [positions]} (sphere/sphere-board players)
                angle (Math/acos (min 1.0 (reduce + (map * (get positions a) (get positions n)))))
                page-per-radian (/ 400 (Math/toRadians 105))]
            (is (< (Math/hypot (- (first pb) (first pa)) (- (second pb) (second pa)))
                   (* 2.0 angle page-per-radian))
                [view a n]))))
      (testing (str players " players: turning the globe brings every space into view")
        (is (= (set (keys adjacencies))
               (set (for [view views
                          [space at] (sphere/locations players view)
                          :when (not (away? at))]
                      space))))))))
