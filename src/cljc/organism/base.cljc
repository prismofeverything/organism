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

(defn- byte->hex
  [n]
  (let [n (max 0 (min 255 (int n)))
        digits "0123456789abcdef"]
    (str (nth digits (quot n 16)) (nth digits (mod n 16)))))

(defn- parse-number
  [s]
  #?(:clj (Double/parseDouble s)
     :cljs (js/parseFloat s)))

(defn hsl->hex
  "h in degrees, s and l in [0,1] → \"#rrggbb\"."
  [h s l]
  ;; every literal is a double on purpose: in Clojure (/ 210 60) is the ratio
  ;; 7/2, and Math/abs has no overload for one.
  (let [h (* 1.0 h)
        s (* 1.0 s)
        l (* 1.0 l)
        c (* (- 1.0 (Math/abs (- (* 2.0 l) 1.0))) s)
        h6 (/ (mod h 360.0) 60.0)
        x (* c (- 1.0 (Math/abs (- (mod h6 2.0) 1.0))))
        [r g b] (cond
                  (< h6 1.0) [c x 0.0]
                  (< h6 2.0) [x c 0.0]
                  (< h6 3.0) [0.0 c x]
                  (< h6 4.0) [0.0 x c]
                  (< h6 5.0) [x 0.0 c]
                  :else      [c 0.0 x])
        m (- l (/ c 2.0))]
    (str "#" (apply str (map #(byte->hex (Math/round (* 255.0 (+ % m))))
                             [r g b])))))

(defn color->hex
  "A CSS colour as #rrggbb.

   An <input type=\"color\"> accepts nothing else, and player colours have been
   stored as hsl(...) strings ever since random-color started producing them.
   Anything unrecognised falls back so the picker still opens."
  ([css] (color->hex css "#445566"))
  ([css fallback]
   (let [text (string/trim (str css))
         hsl (re-matches
              #"(?i)hsla?\(\s*([-\d.]+)\s*,\s*([\d.]+)%\s*,\s*([\d.]+)%\s*(?:,[^)]*)?\)"
              text)]
     (cond
       (re-matches #"#[0-9a-fA-F]{6}" text)
       (string/lower-case text)

       (re-matches #"#[0-9a-fA-F]{3}" text)
       (string/lower-case (apply str "#" (mapcat #(list % %) (subs text 1))))

       hsl
       (let [[_ h sat l] hsl]
         (hsl->hex (parse-number h) (/ (parse-number sat) 100.0) (/ (parse-number l) 100.0)))

       :else fallback))))
