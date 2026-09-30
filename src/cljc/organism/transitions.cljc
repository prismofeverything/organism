(ns organism.transitions
  "What changed between two positions, as the things a player would see
   happen: moves, growth, losses, food going round. Both boards animate from
   this -- the 2D board and the 3D view -- so they show the same move the same
   way."
  (:require
   [clojure.set :as cset]))

(defn diff
  "Detect what changed between two game states. Returns a vector of transition
   maps. Types:
   {:type :move            :from space :to space :element element}
   {:type :grow            :to space :element element}
   {:type :lose            :space space :element element}  ;; conflict/integrity
   {:type :circulate       :from space :to space :amount n :element-color c}
   {:type :food-up         :space space :amount n}          ;; e.g. eating
   {:type :food-down       :space space :amount n}
   {:type :free-food-appear :space space :amount n}
   {:type :free-food-vanish :space space :amount n}"
  [from-state to-state]
  (let [from-els   (:elements from-state)
        to-els     (:elements to-state)
        from-food  (:food from-state)
        to-food    (:food to-state)
        from-spaces (set (keys from-els))
        to-spaces   (set (keys to-els))
        new-spaces  (cset/difference to-spaces from-spaces)
        gone-spaces (cset/difference from-spaces to-spaces)
        common-spaces (cset/intersection from-spaces to-spaces)
        ;; Match up moves: a gone-space element matched with a new-space element
        ;; of the same player/organism/type. Each match consumes both.
        [move-pairs unmoved-gone unmoved-new]
        (reduce
         (fn [[pairs gs ns] s]
           (let [el (get from-els s)
                 match (first
                        (filter
                         (fn [ns-space]
                           (let [new-el (get to-els ns-space)]
                             (and new-el
                                  (= (:player el) (:player new-el))
                                  (= (:organism el) (:organism new-el))
                                  (= (:type el) (:type new-el)))))
                         ns))]
             (if match
               [(conj pairs {:from s :to match :element (get to-els match)})
                (disj gs s)
                (disj ns match)]
               [pairs gs ns])))
         [[] gone-spaces new-spaces]
         gone-spaces)
        ;; Food deltas on elements present in both states
        food-changes
        (for [s common-spaces
              :let [old-f (or (:food (get from-els s)) 0)
                    new-f (or (:food (get to-els s)) 0)
                    delta (- new-f old-f)
                    el (get from-els s)]
              :when (not (zero? delta))]
          {:space s :delta delta
           :player (:player el) :organism (:organism el)})
        ups   (vec (filter #(pos? (:delta %)) food-changes))
        downs (vec (filter #(neg? (:delta %)) food-changes))
        ;; Greedy matching of +N/−N pairs within same player+organism => circulate
        [circ-pairs remaining-ups remaining-downs]
        (reduce
         (fn [[circs us ds] u]
           (let [match (first
                        (filter
                         (fn [d]
                           (and (= (:player u)   (:player d))
                                (= (:organism u) (:organism d))
                                (= (:delta u)    (- (:delta d)))))
                         ds))]
             (if match
               [(conj circs {:from (:space match) :to (:space u)
                             :amount (:delta u)})
                (remove #{u} us)
                (remove #{match} ds)]
               [circs us ds])))
         [[] ups downs]
         ups)]
    (vec
     (concat
      ;; Moves
      (for [{:keys [from to element]} move-pairs]
        {:type :move :from from :to to :element element})
      ;; Lost (gone without a move match)
      (for [s unmoved-gone]
        {:type :lose :space s :element (get from-els s)})
      ;; Grown (new without a move match)
      (for [s unmoved-new]
        {:type :grow :to s :element (get to-els s)})
      ;; Circulate pairs (enriched with food counts so the animation can
      ;; compute the exact coin slot the food leaves from and arrives at)
      (for [c circ-pairs]
        (assoc c
               :type :circulate
               :from-food-before (or (:food (get from-els (:from c))) 0)
               :to-food-after    (or (:food (get to-els (:to c))) 0)))
      ;; Remaining food ups
      (for [u remaining-ups]
        {:type :food-up :space (:space u) :amount (:delta u)})
      ;; Remaining food downs
      (for [d remaining-downs]
        {:type :food-down :space (:space d) :amount (- (:delta d))})
      ;; Free food appearing
      (for [s (cset/difference (set (keys to-food)) (set (keys from-food)))
            :let [amt (get to-food s)]]
        {:type :free-food-appear :space s :amount amt})
      ;; Free food vanishing
      (for [s (cset/difference (set (keys from-food)) (set (keys to-food)))
            :let [amt (get from-food s)]]
        {:type :free-food-vanish :space s :amount amt})))))

