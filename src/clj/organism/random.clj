(ns organism.random
  (:require
   [mersenne-twister.core :as twist]))

(defn make-rand
  [seed]
  (let [generator (twist/new-generator seed)]
    (fn [n]
      (if (= n :generator)
        generator
        (mod (twist/next-int generator) n)))))
