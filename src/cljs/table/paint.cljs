(ns table.paint
  "Drawing on the top face of a board or tile.

   A surface is three canvases: the base (background, printed image, grid),
   the ink (every stroke, so the eraser takes ink and never the print), and
   the one the texture shows, which is base + ink + whatever stroke is being
   drawn right now. Strokes are data in the doc -- {:tool :color :size (mm)
   :alpha :fill :pts [[u v] ...] :text} with u, v the face's texture
   coordinates -- so they redraw at any resolution."
  (:require ["three" :as THREE]))

(def max-px 2048)

(defn- canvas [w h]
  (doto (js/document.createElement "canvas") (aset "width" w) (aset "height" h)))

(defn px-per-mm [[w _ d]]
  (min 8 (/ max-px (max w d))))

(defn new-surface [[w _ d :as size]]
  (let [ppm (px-per-mm size)
        cw (max 16 (js/Math.round (* w ppm)))
        ch (max 16 (js/Math.round (* d ppm)))
        out (canvas cw ch)
        tex (THREE/CanvasTexture. out)]
    (set! (.-colorSpace tex) THREE/SRGBColorSpace)
    (set! (.-anisotropy tex) 8)
    {:ppm ppm :w cw :h ch
     :base (canvas cw ch) :ink (canvas cw ch) :out out :texture tex
     :drawn [] :base-key nil :image nil}))

(defn- ctx [c] (.getContext c "2d"))

(defn- xy [{:keys [w h]} [u v]] [(* u w) (* (- 1 v) h)])

(defn- hexagon! [^js g cx cy r]
  (.beginPath g)
  (dotimes [i 6]
    (let [a (+ (/ js/Math.PI 6) (* i (/ js/Math.PI 3)))
          x (+ cx (* r (js/Math.cos a))) y (+ cy (* r (js/Math.sin a)))]
      (if (zero? i) (.moveTo g x y) (.lineTo g x y))))
  (.closePath g))

(defn draw-stroke!
  "Draw one stroke onto canvas context g of surface s."
  [s ^js g {:keys [tool color size alpha fill pts text]}]
  (let [ppm (:ppm s)
        lw (max 1 (* size ppm))
        p (mapv #(xy s %) pts)
        [x0 y0] (first p)
        [x1 y1] (peek p)
        r (js/Math.hypot (- x1 x0) (- y1 y0))
        shape! (fn [] (if fill (.fill g) (.stroke g)))]
    (.save g)
    (set! (.-globalAlpha g) (or alpha 1))
    (set! (.-strokeStyle g) color)
    (set! (.-fillStyle g) color)
    (set! (.-lineWidth g) lw)
    (set! (.-lineCap g) "round")
    (set! (.-lineJoin g) "round")
    (when (= tool :erase)
      (set! (.-globalCompositeOperation g) "destination-out")
      (set! (.-globalAlpha g) 1))
    (when (seq p)
      (case tool
        (:pen :erase)
        (if (= 1 (count p))
          (do (.beginPath g) (.arc g x0 y0 (/ lw 2) 0 (* 2 js/Math.PI)) (.fill g))
          (do (.beginPath g) (.moveTo g x0 y0)
              (doseq [[x y] (rest p)] (.lineTo g x y))
              (.stroke g)))
        :line (do (.beginPath g) (.moveTo g x0 y0) (.lineTo g x1 y1) (.stroke g))
        :rect (do (.beginPath g) (.rect g (min x0 x1) (min y0 y1) (js/Math.abs (- x1 x0)) (js/Math.abs (- y1 y0)))
                  (shape!))
        :circle (do (.beginPath g) (.arc g x0 y0 r 0 (* 2 js/Math.PI)) (shape!))
        :hex (do (hexagon! g x0 y0 r) (shape!))
        :text (do (set! (.-font g) (str "600 " (js/Math.round (* 3 lw)) "px system-ui, sans-serif"))
                  (set! (.-textAlign g) "center")
                  (set! (.-textBaseline g) "middle")
                  (.fillText g (or text "") x0 y0))
        nil))
    (.restore g)))

(defn- draw-grid! [s ^js g {:keys [type cell color]}]
  (let [{:keys [w h ppm]} s
        c (* (or cell 40) ppm)]
    (when (and (> c 4) (not= type "none"))
      (.save g)
      (set! (.-strokeStyle g) (or color "#6d7680"))
      (set! (.-lineWidth g) (max 1 (* 0.5 ppm)))
      (case type
        "square"
        (do (.beginPath g)
            (doseq [x (range (mod (/ w 2) c) w c)] (.moveTo g x 0) (.lineTo g x h))
            (doseq [y (range (mod (/ h 2) c) h c)] (.moveTo g 0 y) (.lineTo g w y))
            (.stroke g))
        "hex"
        ;; pointy-top hexes, `cell` across the flats, one centred
        (let [r (/ c (js/Math.sqrt 3))
              dy (* 1.5 r)
              rows (js/Math.ceil (/ h dy))
              cols (js/Math.ceil (/ w c))]
          (doseq [row (range (- (inc rows)) (inc rows))
                  col (range (- (inc cols)) (inc cols))]
            (let [cy (+ (/ h 2) (* row dy))
                  cx (+ (/ w 2) (* col c) (if (odd? row) (/ c 2) 0))]
              (when (and (< (- r) cx (+ w r)) (< (- r) cy (+ h r)))
                (hexagon! g cx cy r) (.stroke g)))))
        "dots"
        (do (set! (.-fillStyle g) (or color "#6d7680"))
            (doseq [x (range (mod (/ w 2) c) w c) y (range (mod (/ h 2) c) h c)]
              (.beginPath g) (.arc g x y (max 1.5 (* 0.8 ppm)) 0 (* 2 js/Math.PI)) (.fill g)))
        nil)
      (.restore g))))

(defn base-key [o] [(:size o) (:bg o) (:grid o) (or (:image o) (:face o)) (:shape o)])

(defn- draw-base! [s o ^js img]
  (let [{:keys [w h]} s
        g (ctx (:base s))]
    (.clearRect g 0 0 w h)
    ;; under a printed tile's art, its own edge colour, so transparent corners stay dark
    (set! (.-fillStyle g) (or (:bg o) (:edge o) "#f3efe4"))
    (.fillRect g 0 0 w h)
    (when (and img (.-complete img) (pos? (.-naturalWidth img)))
      (.drawImage g img 0 0 w h))
    (when-let [grid (:grid o)] (draw-grid! s g grid))))

(defn composite!
  "out = base + ink (+ a live stroke drawn straight on top)."
  [s live]
  (let [{:keys [w h]} s
        g (ctx (:out s))]
    (.clearRect g 0 0 w h)
    (.drawImage g (:base s) 0 0)
    (if (and live (= (:tool live) :erase))
      (let [tmp (canvas w h) tg (ctx tmp)]
        (.drawImage tg (:ink s) 0 0)
        (draw-stroke! s tg live)
        (.drawImage g tmp 0 0))
      (do (.drawImage g (:ink s) 0 0)
          (when live (draw-stroke! s g live))))
    (set! (.-needsUpdate (:texture s)) true)))

(defn sync!
  "Bring surface s up to object o, drawing only what changed. Returns s, which
   may be a new surface if o was resized. `redraw!` is called when an image the
   base needs arrives later."
  [s o redraw!]
  (let [s (if (and s (= (:size o) (:size-of s))) s
              (do (when s (.dispose (:texture s)))
                  (assoc (new-surface (:size o)) :size-of (:size o))))
        src (or (:image o) (:face o))
        s (if (and src (not= src (:image-src s)))
                (let [img (js/Image.)]
                  (set! (.-crossOrigin img) "anonymous")
                  (set! (.-onload img) redraw!)
                  (set! (.-src img) src)
                  (assoc s :img img :image-src src :base-key nil))
                s)
        bk (conj (base-key o) (and (:img s) (.-complete (:img s))))
        strokes (or (:strokes o) [])
        drawn (:drawn s)
        base-changed? (not= bk (:base-key s))
        appended? (and (not base-changed?)
                       (>= (count strokes) (count drawn))
                       (= drawn (subvec strokes 0 (count drawn))))]
    (when base-changed? (draw-base! s o (:img s)))
    (let [ig (ctx (:ink s))]
      (if appended?
        (doseq [st (subvec strokes (count drawn))] (draw-stroke! s ig st))
        (do (.clearRect ig 0 0 (:w s) (:h s))
            (doseq [st strokes] (draw-stroke! s ig st)))))
    (when (or base-changed? (not= strokes drawn)) (composite! s nil))
    (assoc s :base-key bk :drawn strokes)))
