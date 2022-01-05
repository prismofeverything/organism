(ns organism.random
  #?(:clj (:require [mersenne-twister.core :as twist])))

(defn make-rand
  ([seed] (make-rand seed 0))
  ([seed initial-step]
   #?(:clj
      (let [generator (twist/new-generator seed)
            step (atom initial-step)]
        (doseq [n (range initial-step)]
          (twist/next-int generator))
        (fn [n]
          (cond
            (= n :generator) generator
            (= n :step) @step
            :else
            (do
              (swap! step inc)
              (mod (twist/next-int generator) n)))))
      :cljs
      rand-int)))

(defn seed-seq
  [seed seq]
  (let [path (make-rand seed)]
    (map path seq)))

(defn phrase->rand
  [phrase]
  (let [seed (hash phrase)]
    (make-rand seed)))

(defn phrase-seq
  [phrase seq]
  (let [seed (hash phrase)]
    (seed-seq seed)))

(defn choose
  [generator seq]
  (let [len (count seq)
        choice (generator len)]
    (nth seq choice)))
