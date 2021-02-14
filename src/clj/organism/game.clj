(ns organism.game)

(defn map-cat
  [f s]
  (reduce into [] (mapv f s)))

(defn build-ring
  [symmetry color level]
  (mapv
   (fn [step]
     [color step])
   (range (* level symmetry))))

(defn build-rings
  [symmetry colors]
  (let [core-color (first colors)
        core (list [core-color 0])]
    (concat
     [[core-color core]]
     (mapv
      (fn [color level]
        [color
         (build-ring
          symmetry
          color
          level)])
      (rest colors)
      (map inc (range))))))

(defn rings->spaces
  [rings]
  (apply
   concat
   (mapv second rings)))

(defn mod-space
  [color spaces step]
  [color (mod step spaces)])

(defn space-adjacencies
  [rings space]
  (let [[color step] space
        level (.indexOf (map first rings) color)
        same-ring (nth rings level)
        same-spaces (count (last same-ring))
        same (map (partial + step) [-1 1])
        same-neighbors [[color same-spaces] same]        

        along (mod step level)
        axis? (zero? along)
        cycle (quot step level)

        inner-ring (nth rings (dec level))
        inner-color (first inner-ring)
        inner-spaces (count (last inner-ring))
        inner-ratio (* (dec level) cycle)
        inner-along (dec along)
        inner-space (+ inner-ratio inner-along)
        inner (if axis?
                [inner-ratio]
                [inner-space (inc inner-space)])
        inner-neighbors [[inner-color inner-spaces] inner]

        outer? (< level (dec (count rings)))
        outer (if outer?
                (let [outer-ring (nth rings (inc level))
                      outer-color (first outer-ring)
                      outer-spaces (count (last outer-ring))
                      outer-ratio (* (inc level) cycle)
                      outer-along (+ along outer-ratio)]
                  [[outer-color outer-spaces]
                   (if axis?
                     (map (partial + outer-ratio) [-1 0 1])
                     (map (partial + outer-along) [0 1]))]))

        neighbors [same-neighbors inner-neighbors]
        neighbors (if outer
                    (conj neighbors outer)
                    neighbors)

        adjacent-spaces (map-cat
                         (fn [[[color spaces] adjacent]]
                           (map
                            (partial mod-space color spaces)
                            adjacent))
                         neighbors)]

    adjacent-spaces))

(defn ring-adjacencies
  [rings color]
  (let [spaces (get (into {} rings) color)]
    (mapv
     (juxt
      identity
      (partial
       space-adjacencies
       rings))
     spaces)))

(defn find-adjacencies
  [rings]
  (let [colors (map first rings)
        [core-color core-spaces] (first rings)
        core (first core-spaces)
        adjacent {core (second (second rings))}
        others (map-cat
                (partial ring-adjacencies rings)
                (rest colors))]
    (into adjacent others)))
