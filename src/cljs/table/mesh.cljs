(ns table.mesh
  "Object spec -> three.js. Each object is a Group (its :pos and :rot) holding
   a body centred on the group's origin. Shapes, tiles and boards are built at
   unit size and scaled to :size, so resizing never rebuilds anything."
  (:require ["three" :as THREE]
            ["three/examples/jsm/geometries/RoundedBoxGeometry.js" :refer [RoundedBoxGeometry]]
            ["three/examples/jsm/loaders/GLTFLoader.js" :refer [GLTFLoader]]
            ["three/examples/jsm/loaders/STLLoader.js" :refer [STLLoader]]
            ["three/examples/jsm/loaders/OBJLoader.js" :refer [OBJLoader]]
            [clojure.string :as str]
            [table.textures :as tx]))

(def card-mm 0.3)   ; one card's thickness

;; ── unit geometry ───────────────────────────────────────────────────────────

(defn- unit!
  "Centre g on the origin and scale it to fit a 1 x 1 x 1 box."
  [^js g]
  (.computeBoundingBox g)
  (let [b (.-boundingBox g)
        c (.getCenter b (THREE/Vector3.))
        s (.getSize b (THREE/Vector3.))]
    (.translate g (- (.-x c)) (- (.-y c)) (- (.-z c)))
    (.scale g (/ 1 (.-x s)) (/ 1 (.-y s)) (/ 1 (.-z s)))
    (.computeVertexNormals g)
    g))

(defn- meeple-geometry []
  (let [s (THREE/Shape.)]
    (doto s
      (.moveTo -0.5 0)
      (.lineTo -0.13 0)
      (.quadraticCurveTo 0 0.3 0.13 0)
      (.lineTo 0.5 0)
      (.quadraticCurveTo 0.42 0.25 0.27 0.46)
      (.quadraticCurveTo 0.5 0.5 0.55 0.58)
      (.quadraticCurveTo 0.56 0.68 0.42 0.68)
      (.lineTo 0.2 0.66)
      (.bezierCurveTo 0.3 0.75 0.27 0.97 0 0.98)
      (.bezierCurveTo -0.27 0.97 -0.3 0.75 -0.2 0.66)
      (.lineTo -0.42 0.68)
      (.quadraticCurveTo -0.56 0.68 -0.55 0.58)
      (.quadraticCurveTo -0.5 0.5 -0.27 0.46)
      (.quadraticCurveTo -0.42 0.25 -0.5 0))
    (unit! (THREE/ExtrudeGeometry. s #js {:depth 0.3 :bevelEnabled true :bevelThickness 0.04
                                          :bevelSize 0.03 :bevelSegments 3 :curveSegments 16}))))

(defn- pawn-geometry []
  (let [pts (concat [[0 0] [0.5 0] [0.5 0.06] [0.42 0.1] [0.36 0.16] [0.22 0.22] [0.17 0.5]
                     [0.3 0.56] [0.3 0.6] [0.15 0.64]]
                    (for [i (range 0 13)]
                      (let [a (- (* i (/ js/Math.PI 12)) (/ js/Math.PI 2) 0.35)
                            a (min a (/ js/Math.PI 2))]
                        [(* 0.2 (js/Math.cos a)) (+ 0.8 (* 0.2 (js/Math.sin a)))]))
                    [[0 1.0]])]
    (unit! (THREE/LatheGeometry. (clj->js (map (fn [[x y]] (THREE/Vector2. (max 0 x) y)) pts)) 40))))

(defonce ^:private geometries (atom {}))

(defn unit-geometry [shape]
  (or (@geometries shape)
      (let [g (case shape
                "cube" (RoundedBoxGeometry. 1 1 1 3 0.07)
                ("cylinder" "disk") (THREE/CylinderGeometry. 0.5 0.5 1 48)
                "sphere" (THREE/SphereGeometry. 0.5 32 18)
                "cone" (THREE/ConeGeometry. 0.5 1 40)
                "hex" (THREE/CylinderGeometry. 0.5 0.5 1 6)
                "tri" (THREE/CylinderGeometry. 0.5 0.5 1 3)
                "meeple" (meeple-geometry)
                "pawn" (pawn-geometry)
                "d6" (RoundedBoxGeometry. 1 1 1 3 0.12)
                (THREE/BoxGeometry. 1 1 1))
            g (if (#{"meeple" "pawn"} shape) g (unit! g))]
        (swap! geometries assoc shape g)
        g)))

(defn- slab-geometry
  "A tile or card: a unit box, or a unit disc whose caps show the image upright
   (three's cylinder caps are turned a quarter, so turn the disc back)."
  [round?]
  (let [k (if round? :slab-round :slab-rect)]
    (or (@geometries k)
        (let [g (if round?
                  (doto (THREE/CylinderGeometry. 0.5 0.5 1 72) (.rotateY (/ js/Math.PI 2)))
                  (THREE/BoxGeometry. 1 1 1))]
          (swap! geometries assoc k g)
          g))))

;; which material index is the top face, for painting
(defn top-index [round?] (if round? 1 2))

;; ── materials ───────────────────────────────────────────────────────────────

(defonce ^:private materials (atom {}))

(defn color-material [color & [rough]]
  (let [k [:color color rough]]
    (or (@materials k)
        (let [m (THREE/MeshStandardMaterial. #js {:color color :roughness (or rough 0.55) :metalness 0.02})]
          (swap! materials assoc k m)
          m))))

(defn image-material [key & [fallback]]
  (if key
    (let [k [:image key]]
      (or (@materials k)
          (let [m (THREE/MeshStandardMaterial. #js {:map (tx/texture key) :roughness 0.75})]
            (swap! materials assoc k m)
            m)))
    (color-material (or fallback "#eeeae0") 0.8)))

(defn- slab-materials
  "Materials in the geometry's group order, given top, bottom and edge."
  [round? top bottom edge]
  (if round?
    #js [edge top bottom]
    #js [edge edge top bottom edge edge]))

(defn- shadowed! [^js o]
  (.traverse o (fn [^js m] (when (.-isMesh m) (set! (.-castShadow m) true) (set! (.-receiveShadow m) true))))
  o)

;; ── models ──────────────────────────────────────────────────────────────────

(defonce ^:private models (atom {}))   ; url -> promise of {:template :size :textured?}

(defn- ext [url]
  (cond (str/starts-with? url "data:")
        (let [mime (second (re-find #"^data:([^;,]+)" url))]
          (cond (str/includes? mime "gltf") "glb"
                (str/includes? mime "stl") "stl"
                (str/includes? mime "obj") "obj"
                :else (last (str/split mime #"/"))))
        :else (str/lower-case (last (str/split (first (str/split url #"\?")) #"\.")))))

(defn load-model
  "A promise of {:template Object3D centred on its origin, :size Vector3 (mm,
   before any scale), :textured? bool}. The format comes from the URL, or an
   imported file's own type, given as a data URL."
  [url z-up & [kind]]
  (let [k [url z-up]]
    (or (@models k)
        (let [p (js/Promise.
                 (fn [ok fail]
                   (let [done (fn [^js obj]
                                (let [inner (THREE/Group.)
                                      holder (THREE/Group.)
                                      textured? (atom false)]
                                  (.add inner obj)
                                  (.traverse obj (fn [^js m]
                                                   (when (.-isMesh m)
                                                     (when (some-> m .-material .-map) (reset! textured? true))
                                                     (when-not (.. m -geometry -attributes -normal)
                                                       (.computeVertexNormals (.-geometry m))))))
                                  (when z-up (set! (.. inner -rotation -x) (- (/ js/Math.PI 2))))
                                  (.add holder inner)
                                  (.updateMatrixWorld holder true)
                                  (let [b (.setFromObject (THREE/Box3.) holder)
                                        c (.getCenter b (THREE/Vector3.))]
                                    (.sub (.-position inner) c)
                                    (ok {:template holder :size (.getSize b (THREE/Vector3.))
                                         :textured? @textured?}))))]
                     (case (or kind (ext url))
                       ("glb" "gltf") (.load (GLTFLoader.) url (fn [^js g] (done (.-scene g))) nil fail)
                       "stl" (.load (STLLoader.) url
                                    (fn [g] (done (THREE/Mesh. g (color-material "#b8b2a6"))))
                                    nil fail)
                       "obj" (.load (OBJLoader.) url done nil fail)
                       (fail (js/Error. (str "can't read a model from " (subs url 0 40))))))))]
          (swap! models assoc k p)
          p))))

(defonce loaded-models (atom {}))   ; [url z-up] -> resolved model, for synchronous building

(defn model-ready [{:keys [model z-up model-kind]}]
  (-> (load-model model z-up model-kind)
      (.then (fn [m] (swap! loaded-models assoc [model z-up] m) m))))

(defn- model-body [{:keys [model z-up color scale base-scale]}]
  (when-let [{:keys [template]} (@loaded-models [model z-up])]
    (let [body (.clone ^js template true)
          s (or base-scale 1)
          [sx sy sz] (or scale [1 1 1])]
      ;; a colour paints over the model; none keeps an imported model's own look
      (when color
        (.traverse body (fn [^js m] (when (.-isMesh m) (set! (.-material m) (color-material color 0.5))))))
      (.set (.-scale body) (* s sx) (* s sy) (* s sz))
      body)))

;; ── cards ───────────────────────────────────────────────────────────────────

(defn card-thickness [n] (max 0.5 (* card-mm n)))

(defn visible-faces
  "[top bottom] image keys of a stack of cards, in its own frame."
  [cards]
  (let [top (peek cards) bot (first cards)]
    [(if (:down top) (:back top) (:face top))
     (if (:down bot) (:face bot) (:back bot))]))

;; ── build ───────────────────────────────────────────────────────────────────

(defn visual-key
  "What, if it changed, means the body must be rebuilt."
  [o]
  (case (:kind o)
    "cards" (select-keys o [:kind :cards :size :shape])
    "model" (select-keys o [:kind :model :z-up :color :scale :base-scale])
    "board" (select-keys o [:kind :shape :back :edge :bg])
    "tile" (assoc (select-keys o [:kind :shape :face :back :edge]) :painted (boolean (seq (:strokes o))))
    (select-keys o [:kind :shape :color])))

(defn paintable? [o] (or (= "board" (:kind o)) (and (= "tile" (:kind o)) (seq (:strokes o)))))
(defn can-paint? [o] (#{"board" "tile"} (:kind o)))

(defn build
  "A fresh body for o. `surface` is the paint surface's texture when o is drawn on."
  [o ^js surface]
  (let [{:keys [kind shape size color]} o
        round? (= shape "round")
        body
        (case kind
          "shape"
          (if (= shape "d6")
            (THREE/Mesh. (unit-geometry "d6")
                         (clj->js (for [v [2 5 1 6 3 4]]
                                    (THREE/MeshStandardMaterial. #js {:map (tx/die-texture v (or color "#f2efe6"))
                                                                      :roughness 0.4}))))
            (THREE/Mesh. (unit-geometry shape) (color-material (or color "#c9c2b4"))))

          ("tile" "board")
          (let [edge (color-material (or (:edge o) (if (= kind "board") "#3b3b3b" "#e8e3d6")) 0.7)
                top (if surface
                      (THREE/MeshStandardMaterial. #js {:map surface :roughness 0.8})
                      (image-material (:face o) (:bg o)))
                bottom (image-material (:back o) (or (:bg o) "#d9d4c7"))]
            (THREE/Mesh. (slab-geometry round?) (slab-materials round? top bottom edge)))

          "cards"
          (let [[w l] size
                t (card-thickness (count (:cards o)))
                [top bot] (visible-faces (:cards o))
                m (THREE/Mesh. (slab-geometry round?)
                               (slab-materials round? (image-material top) (image-material bot)
                                               (color-material "#efebe1" 0.8)))]
            (.set (.-scale m) w t l)
            m)

          "model"
          (or (model-body o) (THREE/Group.)))]
    (shadowed! body)))

(defn scale-body!
  "Set a body's scale from :size, for the kinds built at unit size."
  [^js body o]
  (when (#{"shape" "tile" "board"} (:kind o))
    (let [[w h d] (:size o)]
      (.set (.-scale body) w h d))))
