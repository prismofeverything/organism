(ns organism.base)

(defn map-cat
  "non-lazy mapcat"
  [f s]
  (reduce into [] (mapv f s)))


