(ns organism.ogf
  "Portable OGF view coordinates. Ring identity is independent of its palette."
  (:require [clojure.string :as string]))

(def coordinate-system "rings-clockwise-30deg-v1")

(defn ring-label [index]
  (loop [n (inc index) result ""]
    (if (zero? n) result
        (let [n (dec n)
              letter #?(:clj (str (char (+ 65 (mod n 26))))
                        :cljs (js/String.fromCharCode (+ 65 (mod n 26))))]
          (recur (quot n 26) (str letter result))))))

(defn ring-index [label]
  (dec (reduce (fn [n c]
                 (+ (* n 26) (- #?(:clj (int c) :cljs (.charCodeAt c 0)) 64)))
               0 label)))

(defn parse-space [id]
  (if-let [i (string/last-index-of id ":")]
    [(subs id 0 i) #?(:clj (Integer/parseInt (subs id (inc i)))
                     :cljs (js/parseInt (subs id (inc i)) 10))]
    (if-let [[_ ring index] (re-matches #"([A-Z]+)(0|[1-9][0-9]*)" id)]
      [ring #?(:clj (Integer/parseInt index) :cljs (js/parseInt index 10))]
      (throw (ex-info "Invalid OGF space" {:id id})))))

(defn space-id [ring index] (str (ring-label ring) index))
