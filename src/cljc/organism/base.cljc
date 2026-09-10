(ns organism.base
  (:require
   [clojure.string :as string]))

(defn map-cat
  "non-lazy mapcat"
  [f s]
  (reduce into [] (mapv f s)))

(defn join-path
  "Join a URL prefix and a segment with exactly one slash between them.

   Pages hand these prefixes over inconsistently — js/playerPath is
   \"/organism/player\" with no trailing slash, while the games list passes
   \"/organism/player/\" with one. Concatenating blindly produced
   /organism/playeralice from the observe page."
  [prefix segment]
  (str (string/replace (str prefix) #"/+$" "") "/" segment))
