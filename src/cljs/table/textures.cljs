(ns table.textures
  "Textures by key, made once. A key is a URL (library art, an imported image
   as a data URL) or a procedural name: \"std:QS\" a standard playing card,
   \"std:back\", \"std:blank\". Loading is asynchronous; `on-load` is told when
   a texture arrives so the scene can redraw."
  (:require ["three" :as THREE]
            [clojure.string :as str]))

(defonce ^:private cache (atom {}))
(defonce ^:private canvases (atom {}))   ; procedural key -> canvas, for previews
(defonce on-load (atom (fn [])))
(defonce anisotropy (atom 4))

(def suits {"S" ["♠" "#1b1b1b"] "H" ["♥" "#c8202f"] "D" ["♦" "#c8202f"] "C" ["♣" "#1b1b1b"]})
(def ranks ["A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"])

(defn- rounded! [^js g x y w h r]
  (.beginPath g)
  (.moveTo g (+ x r) y)
  (.arcTo g (+ x w) y (+ x w) (+ y h) r)
  (.arcTo g (+ x w) (+ y h) x (+ y h) r)
  (.arcTo g x (+ y h) x y r)
  (.arcTo g x y (+ x w) y r)
  (.closePath g))

;; where the pips go on a 1..10 card, in a 0..1 box
(def ^:private pip-layout
  {1 [[0.5 0.5]]
   2 [[0.5 0.12] [0.5 0.88]]
   3 [[0.5 0.12] [0.5 0.5] [0.5 0.88]]
   4 [[0.25 0.12] [0.75 0.12] [0.25 0.88] [0.75 0.88]]
   5 [[0.25 0.12] [0.75 0.12] [0.5 0.5] [0.25 0.88] [0.75 0.88]]
   6 [[0.25 0.12] [0.75 0.12] [0.25 0.5] [0.75 0.5] [0.25 0.88] [0.75 0.88]]
   7 [[0.25 0.12] [0.75 0.12] [0.5 0.31] [0.25 0.5] [0.75 0.5] [0.25 0.88] [0.75 0.88]]
   8 [[0.25 0.12] [0.75 0.12] [0.5 0.31] [0.25 0.5] [0.75 0.5] [0.5 0.69] [0.25 0.88] [0.75 0.88]]
   9 [[0.25 0.12] [0.75 0.12] [0.25 0.37] [0.75 0.37] [0.5 0.5] [0.25 0.63] [0.75 0.63] [0.25 0.88] [0.75 0.88]]
   10 [[0.25 0.12] [0.75 0.12] [0.5 0.24] [0.25 0.37] [0.75 0.37] [0.25 0.63] [0.75 0.63] [0.5 0.76] [0.25 0.88] [0.75 0.88]]})

(defn- card-canvas [code]
  (let [w 320 h 448
        c (doto (js/document.createElement "canvas") (aset "width" w) (aset "height" h))
        ^js g (.getContext c "2d")]
    (cond
      (= code "back")
      (do (set! (.-fillStyle g) "#f4f1ea") (.fillRect g 0 0 w h)
          (set! (.-fillStyle g) "#8d1f2a") (rounded! g 14 14 (- w 28) (- h 28) 16) (.fill g)
          (set! (.-strokeStyle g) "rgba(255,240,220,0.35)") (set! (.-lineWidth g) 2)
          (doseq [i (range -30 30)]
            (.beginPath g) (.moveTo g (* i 22) 14) (.lineTo g (+ (* i 22) h) (+ h 14)) (.stroke g)
            (.beginPath g) (.moveTo g (+ (* i 22) h) 14) (.lineTo g (* i 22) (+ h 14)) (.stroke g))
          (set! (.-strokeStyle g) "#f4f1ea") (set! (.-lineWidth g) 8)
          (rounded! g 14 14 (- w 28) (- h 28) 16) (.stroke g))

      (= code "blank")
      (do (set! (.-fillStyle g) "#fbfaf6") (.fillRect g 0 0 w h))

      (str/starts-with? code "joker")
      (do (set! (.-fillStyle g) "#fbfaf6") (.fillRect g 0 0 w h)
          (set! (.-fillStyle g) (if (= code "joker2") "#c8202f" "#1b1b1b"))
          (set! (.-font g) "bold 34px Georgia, serif") (set! (.-textAlign g) "center")
          (doseq [[i ch] (map-indexed vector "JOKER")]
            (.fillText g ch 30 (+ 50 (* i 36))))
          (set! (.-font g) "150px serif") (.fillText g "★" (/ w 2) (+ (/ h 2) 50)))

      :else
      (let [suit (subs code (dec (count code)))
            rank (subs code 0 (dec (count code)))
            [sym col] (suits suit)
            n (js/parseInt rank)]
        (set! (.-fillStyle g) "#fbfaf6") (.fillRect g 0 0 w h)
        (set! (.-fillStyle g) col) (set! (.-textAlign g) "center")
        ;; corner indices, one turned
        (doseq [flip [false true]]
          (.save g)
          (when flip (.translate g w h) (.rotate g js/Math.PI))
          (set! (.-font g) "bold 44px Georgia, serif") (.fillText g rank 34 54)
          (set! (.-font g) "38px serif") (.fillText g sym 34 92)
          (.restore g))
        (cond
          (js/isNaN n)               ; J Q K: a framed letter
          (do (set! (.-strokeStyle g) col) (set! (.-lineWidth g) 3)
              (.strokeRect g 70 70 (- w 140) (- h 140))
              (set! (.-font g) "bold 120px Georgia, serif") (.fillText g rank (/ w 2) (+ (/ h 2) 10))
              (set! (.-font g) "64px serif") (.fillText g sym (/ w 2) (+ (/ h 2) 90)))
          (= n 1)
          (do (set! (.-font g) "190px serif") (.fillText g sym (/ w 2) (+ (/ h 2) 66)))
          :else
          (do (set! (.-font g) "58px serif")
              (doseq [[px py] (pip-layout n)]
                (.save g)
                (.translate g (+ 80 (* px 160)) (+ 74 (* py 300)))
                (when (> py 0.55) (.rotate g js/Math.PI))
                (.fillText g sym 0 20)
                (.restore g))))))
    c))

(defn die-canvas [value color]
  (let [s 128
        c (doto (js/document.createElement "canvas") (aset "width" s) (aset "height" s))
        ^js g (.getContext c "2d")
        light? (let [col (THREE/Color. color)] (> (+ (.-r col) (.-g col) (.-b col)) 1.6))
        spots {1 [[2 2]] 2 [[1 1] [3 3]] 3 [[1 1] [2 2] [3 3]] 4 [[1 1] [3 1] [1 3] [3 3]]
               5 [[1 1] [3 1] [2 2] [1 3] [3 3]] 6 [[1 1] [3 1] [1 2] [3 2] [1 3] [3 3]]}]
    (set! (.-fillStyle g) color) (.fillRect g 0 0 s s)
    (set! (.-fillStyle g) (if light? "#161616" "#f6f3ea"))
    (doseq [[x y] (spots value)]
      (.beginPath g) (.arc g (* x 32) (* y 32) 12 0 (* 2 js/Math.PI)) (.fill g))
    c))

(defn- finish! [^js tex]
  (set! (.-colorSpace tex) THREE/SRGBColorSpace)
  (set! (.-anisotropy tex) @anisotropy)
  tex)

(defn texture
  "The texture for `key`, nil for nil."
  [key]
  (when key
    (or (@cache key)
        (let [tex (if (str/starts-with? key "std:")
                    (let [c (card-canvas (subs key 4))]
                      (swap! canvases assoc key c)
                      (THREE/CanvasTexture. c))
                    (.load (THREE/TextureLoader.) key (fn [_] (@on-load))))]
          (finish! tex)
          (swap! cache assoc key tex)
          tex))))

(defn die-texture [value color]
  (let [key (str "die:" value ":" color)]
    (or (@cache key)
        (let [t (finish! (THREE/CanvasTexture. (die-canvas value color)))]
          (swap! cache assoc key t)
          t))))

(defn preview-url
  "Something an <img> can show for `key`."
  [key]
  (cond (nil? key) nil
        (str/starts-with? key "std:") (do (texture key) (.toDataURL (@canvases key)))
        :else key))

(defn standard-deck [jokers?]
  (vec (concat (for [s ["S" "H" "C" "D"] r ranks]
                 {:name (str r (first (suits s))) :face (str "std:" r s) :back "std:back"})
               (when jokers?
                 [{:name "Joker" :face "std:joker" :back "std:back"}
                  {:name "Joker" :face "std:joker2" :back "std:back"}]))))
