(ns table.world
  "The three.js side: renderer, camera, the table, and one Group per object,
   kept in step with the doc by `sync!` -- objects that changed are moved,
   rebuilt only when how they look changed. The scene draws only when
   something moved."
  (:require ["three" :as THREE]
            ["three/examples/jsm/controls/OrbitControls.js" :refer [OrbitControls]]
            [table.mesh :as mesh]
            [table.paint :as paint]
            [table.state :as st]
            [table.textures :as tx]))

(def table-w 2400)
(def table-d 1600)

(defonce world (atom nil))       ; {:renderer :scene :camera :controls}
(defonce registry (atom {}))     ; id -> {:root :body :vkey :obj :surface :helper}
(defonce dirty (atom true))
(defn redraw! [] (reset! dirty true))

;; ── setup ───────────────────────────────────────────────────────────────────

(defn- felt-texture []
  (let [s 512
        c (doto (js/document.createElement "canvas") (aset "width" s) (aset "height" s))
        g (.getContext c "2d")
        img (.createImageData g s s)
        d (.-data img)]
    (dotimes [i (* s s)]
      (let [n (+ (* 9 (js/Math.random)) -4.5)]
        (aset d (* 4 i) (+ 0x2b n))
        (aset d (+ 1 (* 4 i)) (+ 0x3d n))
        (aset d (+ 2 (* 4 i)) (+ 0x4a n))
        (aset d (+ 3 (* 4 i)) 255)))
    (.putImageData g img 0 0)
    (let [t (THREE/CanvasTexture. c)]
      (set! (.-wrapS t) THREE/RepeatWrapping)
      (set! (.-wrapT t) THREE/RepeatWrapping)
      (.set (.-repeat t) 6 4)
      (set! (.-colorSpace t) THREE/SRGBColorSpace)
      t)))

(defn init! [^js host]
  (let [renderer (THREE/WebGLRenderer. #js {:antialias true})
        scene (THREE/Scene.)
        camera (THREE/PerspectiveCamera. 40 1 5 30000)
        controls (OrbitControls. camera (.-domElement renderer))
        sun (THREE/DirectionalLight. "#ffffff" 2.2)
        top (THREE/Mesh. (THREE/PlaneGeometry. table-w table-d)
                         (THREE/MeshStandardMaterial. #js {:map (felt-texture) :roughness 0.95}))
        rim (THREE/Mesh. (THREE/BoxGeometry. (+ table-w 60) 50 (+ table-d 60))
                         (THREE/MeshStandardMaterial. #js {:color "#4a3527" :roughness 0.6}))
        grid (THREE/GridHelper. table-w (/ table-w 50) "#5b6f7c" "#3d505c")]
    (.setPixelRatio renderer (min 2 js/devicePixelRatio))
    (set! (.. renderer -shadowMap -enabled) true)
    (set! (.. renderer -shadowMap -type) THREE/PCFSoftShadowMap)
    (reset! tx/anisotropy (min 8 (.. renderer -capabilities getMaxAnisotropy)))
    (.appendChild host (.-domElement renderer))
    (set! (.-background scene) (THREE/Color. "#14171b"))
    (.add scene (THREE/HemisphereLight. "#f4f1ea" "#3a3f46" 1.5))
    (.set (.-position sun) 500 1400 400)
    (set! (.-castShadow sun) true)
    (let [sc (.. sun -shadow -camera)]
      (set! (.-left sc) -1300) (set! (.-right sc) 1300)
      (set! (.-top sc) 900) (set! (.-bottom sc) -900)
      (set! (.-near sc) 100) (set! (.-far sc) 4000))
    (.set (.. sun -shadow -mapSize) 4096 4096)
    (set! (.. sun -shadow -bias) -0.0004)
    (set! (.. sun -shadow -normalBias) 0.6)
    (.add scene sun)
    (.rotateX top (- (/ js/Math.PI 2)))
    (set! (.-receiveShadow top) true)
    (set! (.. rim -position -y) -25.2)
    (set! (.. grid -position -y) 0.05)
    (set! (.. grid -material -transparent) true)
    (set! (.. grid -material -opacity) 0.35)
    (doto scene (.add top) (.add rim) (.add grid))
    (.set (.-position camera) 0 780 720)
    (set! (.-zoomToCursor controls) true)
    (set! (.-maxPolarAngle controls) (* 0.48 js/Math.PI))
    (set! (.-minDistance controls) 25)
    (set! (.-maxDistance controls) 6000)
    (set! (.-screenSpacePanning controls) false)
    (set! (.-mouseButtons controls) #js {:LEFT -1 :MIDDLE THREE/MOUSE.PAN :RIGHT THREE/MOUSE.ROTATE})
    (set! (.-touches controls) #js {:ONE -1 :TWO THREE/TOUCH.DOLLY_PAN})
    (.update controls)
    (.addEventListener controls "change" redraw!)
    (reset! world {:renderer renderer :scene scene :camera camera :controls controls
                   :table top :grid grid})
    (reset! tx/on-load redraw!)))

(defn resize! []
  (when-let [{:keys [^js renderer ^js camera]} @world]
    (let [p (.. renderer -domElement -parentElement)
          w (.-clientWidth p) h (.-clientHeight p)]
      (.setSize renderer w h)
      (set! (.-aspect camera) (/ w h))
      (.updateProjectionMatrix camera)
      (redraw!))))

;; ── sync ────────────────────────────────────────────────────────────────────

(defn- dispose-entry! [{:keys [root surface helper]}]
  (let [{:keys [scene]} @world]
    (.remove scene root)
    (when helper (.remove scene helper) (.dispose helper))
    (when surface (.dispose (:texture surface)))))

(declare repaint! sync!)

(defn- place! [^js root o]
  (let [[x y z] (:pos o) [qx qy qz qw] (:rot o)]
    (.set (.-position root) x y z)
    (.set (.-quaternion root) qx qy qz qw)
    (.updateMatrixWorld root true)))

(defn- build-entry [o old]
  (let [{:keys [scene]} @world
        surface (when (mesh/paintable? o)
                  (paint/sync! (:surface old) o #(repaint! (:id o))))
        root (or (:root old) (THREE/Group.))
        body (mesh/build o (:texture surface))
        id (:id o)]
    ;; a model still loading: build again once it's here
    (when (and (= "model" (:kind o)) (not (@mesh/loaded-models [(:model o) (:z-up o)])))
      (.then (mesh/model-ready o)
             (fn [_] (when (@registry id) (swap! registry update id assoc :vkey nil :obj nil) (sync!)))))
    (when-let [b (:body old)] (.remove root b))
    (when (and (:surface old) (not (identical? (:surface old) surface)) (nil? surface))
      (.dispose (:texture (:surface old))))
    (set! (.. root -userData -id) (:id o))
    (.traverse body (fn [^js m] (set! (.. m -userData -id) (:id o))))
    (.add root body)
    (when-not (:root old) (.add scene root))
    (assoc old :root root :body body :vkey (mesh/visual-key o) :surface surface)))

(defn- update-entry [e o]
  (let [e (if (= (:vkey e) (mesh/visual-key o))
            (if (mesh/paintable? o)
              (let [s (paint/sync! (:surface e) o #(repaint! (:id o)))]
                (if (identical? (:texture s) (:texture (:surface e)))
                  (assoc e :surface s)
                  (build-entry o (assoc e :surface s))))   ; resized: a new canvas
              e)
            (build-entry o e))]
    (mesh/scale-body! (:body e) o)
    (place! (:root e) o)
    ;; kept for the resting and spacing questions, which ask it of everything
    (assoc e :obj o :box (.setFromObject (THREE/Box3.) (:root e)))))

(defn sync!
  "Make the scene match the doc and the selection."
  []
  (when @world
    (let [objs (st/objects)
          reg @registry
          sel (:selected @st/ui)
          hover (:hover @st/ui)
          reg (reduce-kv (fn [r id e]
                           (if (contains? objs id) r (do (dispose-entry! e) (dissoc r id))))
                         reg reg)
          reg (reduce-kv (fn [r id o]
                           (let [e (get r id)]
                             (if (and e (identical? (:obj e) o))
                               r
                               (assoc r id (update-entry (or e {}) o)))))
                         reg objs)
          ;; selection boxes
          reg (reduce-kv (fn [r id e]
                           (let [want (cond (contains? sel id) "#4fc3f7"
                                            (= hover id) "#9aa7b0")]
                             (cond
                               (and want (:helper e))
                               (do (.setFromObject ^js (:helper e) (:root e))
                                   (.set (.. (:helper e) -material -color) want)
                                   r)
                               want
                               (let [h (THREE/BoxHelper. (:root e) want)]
                                 (.add (:scene @world) h)
                                 (assoc-in r [id :helper] h))
                               (:helper e)
                               (do (.remove (:scene @world) (:helper e))
                                   (.dispose (:helper e))
                                   (update r id dissoc :helper))
                               :else r)))
                         reg reg)]
      (reset! registry reg)
      (redraw!))))

(defn repaint!
  "An image a surface waits for has arrived: redraw it."
  [id]
  (when-let [e (@registry id)]
    (when-let [s (:surface e)]
      (swap! registry assoc-in [id :surface] (paint/sync! s (:obj e) #(repaint! id)))
      (redraw!))))

(defn live-stroke!
  "Show a stroke being drawn on object id's surface, without committing it."
  [id stroke]
  (when-let [s (get-in @registry [id :surface])]
    (paint/composite! s stroke)
    (redraw!)))

;; ── picking and resting ─────────────────────────────────────────────────────

(defonce ^:private raycaster (THREE/Raycaster.))

(defn ndc [x y]
  (let [r (.getBoundingClientRect (.-domElement (:renderer @world)))]
    (THREE/Vector2. (- (* 2 (/ (- x (.-left r)) (.-width r))) 1)
                    (+ (* -2 (/ (- y (.-top r)) (.-height r))) 1))))

(defn- roots [& [exclude]]
  (into-array (keep (fn [[id e]] (when-not (contains? exclude id) (:root e))) @registry)))

(defn pick
  "The nearest hit under screen point x y: {:id :point :uv :face-index} or nil."
  [x y]
  (.setFromCamera raycaster (ndc x y) (:camera @world))
  (when-let [^js hit (first (.intersectObjects raycaster (roots) true))]
    {:id (.. hit -object -userData -id)
     :point (.-point hit)
     :uv (.-uv hit)
     :material (some-> hit .-face .-materialIndex)}))

(defn table-point
  "Where screen point x y meets the horizontal plane at height h."
  [x y h]
  (.setFromCamera raycaster (ndc x y) (:camera @world))
  (let [plane (THREE/Plane. (THREE/Vector3. 0 1 0) (- h))
        p (THREE/Vector3.)]
    (when (.intersectPlane (.-ray raycaster) plane p) p)))

(defn bounds
  "World bounding box of object id as it stands."
  [id]
  (when-let [e (@registry id)]
    (.setFromObject (THREE/Box3.) (:root e))))

(defn half-height
  "How far below its centre object id reaches, as it is turned."
  [id]
  (if-let [b (bounds id)]
    (- (.. (:root (@registry id)) -position -y) (.. b -min -y))
    5))

(defn- footprint [^js b] [(- (.. b -max -x) (.. b -min -x)) (- (.. b -max -z) (.. b -min -z))])

(defn small-beside?
  "Is the thing with bounds b too small to hold up something w x d across? A
   board put down over a meeple goes under it, not on top of it."
  [^js b [w d]]
  (let [[bw bd] (footprint b)]
    (and (< bw (/ w 3)) (< bd (/ d 3)))))

(defn support
  "The highest surface under a footprint w x d centred at x z, ignoring
   `exclude` and anything too small to hold it up."
  [x z [w d] exclude]
  (let [ignore (into (set exclude)
                     (keep (fn [[id e]] (when (small-beside? (:box e) [w d]) id)))
                     @registry)
        rs (roots ignore)
        down (THREE/Vector3. 0 -1 0)]
    (reduce (fn [best [dx dz]]
              (.set raycaster (THREE/Vector3. (+ x dx) 20000 (+ z dz)) down)
              (if-let [^js hit (first (.intersectObjects raycaster rs true))]
                (max best (.. hit -point -y))
                best))
            0
            (for [fx [0 -0.2 0.2] fz [0 -0.2 0.2]] [(* fx w) (* fz d)]))))

(defn rest-y
  "The :pos y at which object id, moved to x z, rests on whatever is beneath it."
  [id x z exclude]
  (let [b (bounds id)
        fp (if b (footprint b) [10 10])]
    (+ (support x z fp (conj (set exclude) id)) (half-height id) 0.05)))

(defn riders
  "Small things that object id, as it now lies, has been put down over: they
   belong on top of it."
  [id exclude]
  (when-let [b (bounds id)]
    (let [fp (footprint b)
          top (.. b -max -y)]
      (for [[oid e] @registry
            :when (not (contains? exclude oid))
            :when (not= oid id)
            :let [^js ob (:box e)
                  p (.-position (:root e))]
            :when (and (small-beside? ob fp)
                       (< (.. ob -min -y) (- top 0.2))
                       (> (.. ob -max -y) (.. b -min -y))
                       (<= (.. b -min -x) (.-x p) (.. b -max -x))
                       (<= (.. b -min -z) (.-z p) (.. b -max -z)))]
        oid))))

(defn free-spot
  "The nearest place to x z where object id, laid down, would not land on or
   in anything but a board or other surface bigger than itself."
  [id x z]
  (if-let [b (bounds id)]
    (let [[w d] (footprint b)
          others (keep (fn [[oid e]]
                         (when (not= oid id)
                           (let [ob (:box e)
                                 [ow od] (footprint ob)]
                             (when-not (and (>= ow w) (>= od d) (> (* ow od) (* 4 w d)))
                               ob))))
                       @registry)
          clear? (fn [cx cz]
                   (not-any? (fn [^js ob]
                               (and (< (- cx (/ w 2) 4) (.. ob -max -x)) (> (+ cx (/ w 2) 4) (.. ob -min -x))
                                    (< (- cz (/ d 2) 4) (.. ob -max -z)) (> (+ cz (/ d 2) 4) (.. ob -min -z))))
                             others))
          step (max 12 (/ (min w d) 2))]
      (or (first (for [ring (range 0 60)
                       k (range (max 1 (* 6 ring)))
                       :let [a (* 2 js/Math.PI (/ k (max 1 (* 6 ring))))
                             cx (+ x (* ring step (js/Math.cos a)))
                             cz (+ z (* ring step (js/Math.sin a)))]
                       :when (clear? cx cz)]
                   [cx cz]))
          [x z]))
    [x z]))

(defn view-centre []
  (let [t (.-target (:controls @world))] [(.-x t) (.-z t)]))

(defn focus! [^js p]
  (let [{:keys [^js camera ^js controls]} @world
        ^js d (.clone (.sub (.clone (.-position camera)) (.-target controls)))]
    (.copy (.-target controls) p)
    (.copy (.-position camera) (.add (.clone p) (.multiplyScalar d 0.6)))
    (.update controls)))

(defn pan!
  "Slide the view along the table: dx right, dz forward, scaled to the distance."
  [dx dz]
  (let [{:keys [^js camera ^js controls]} @world
        dist (.distanceTo (.-position camera) (.-target controls))
        fwd (.getWorldDirection camera (THREE/Vector3.))
        _ (set! (.-y fwd) 0)
        fwd (.normalize fwd)
        right (.crossVectors (THREE/Vector3.) fwd (THREE/Vector3. 0 1 0))
        mv (.add (.multiplyScalar right (* dx dist 0.012)) (.multiplyScalar fwd (* dz dist 0.012)))]
    (.add (.-position camera) mv)
    (.add (.-target controls) mv)
    (.update controls)))

(defn screen-pos
  "Object id's centre on screen, in client pixels."
  [id]
  (when-let [e (@registry id)]
    (let [{:keys [^js renderer camera]} @world
          ^js root (:root e)
          r (.getBoundingClientRect (.-domElement renderer))
          v (.project (.clone (.-position root)) camera)]
      [(+ (.-left r) (* (.-width r) (/ (inc (.-x v)) 2)))
       (+ (.-top r) (* (.-height r) (/ (- 1 (.-y v)) 2)))])))

(defn start-loop! [tick]
  (letfn [(frame []
            (tick)
            (when-let [{:keys [renderer scene camera controls]} @world]
              (.update controls)
              (when @dirty
                (reset! dirty false)
                (.render renderer scene camera)))
            (js/requestAnimationFrame frame))]
    (js/requestAnimationFrame frame)))

(defn beneath
  "The id of whatever is directly under object id's centre."
  [id]
  (when-let [e (@registry id)]
    (let [p (.. (:root e) -position)]
      (.set raycaster (THREE/Vector3. (.-x p) 20000 (.-z p)) (THREE/Vector3. 0 -1 0))
      (when-let [^js hit (first (.intersectObjects raycaster (roots #{id}) true))]
        (.. hit -object -userData -id)))))

;; ── other people ────────────────────────────────────────────────────────────

(defonce ^:private peer-marks (atom {}))   ; player -> Group

(defn player-colour
  "A steady colour for a player's name."
  [player]
  (let [h (reduce (fn [h c] (mod (+ (* h 31) (.charCodeAt c 0)) 360)) 7 (str player))]
    (str "hsl(" h ", 70%, 60%)")))

(defn- name-sprite [player colour]
  (let [c (doto (js/document.createElement "canvas") (aset "width" 256) (aset "height" 64))
        ^js g (.getContext c "2d")]
    (set! (.-font g) "600 30px system-ui, sans-serif")
    (let [w (+ 24 (.-width (.measureText g player)))]
      (set! (.-fillStyle g) "rgba(0,0,0,0.65)")
      (.fillRect g (- 128 (/ w 2)) 8 w 46)
      (set! (.-fillStyle g) colour)
      (set! (.-textAlign g) "center")
      (.fillText g player 128 42))
    (let [tex (THREE/CanvasTexture. c)
          s (THREE/Sprite. (THREE/SpriteMaterial. #js {:map tex :depthTest false :transparent true}))]
      (set! (.-colorSpace tex) THREE/SRGBColorSpace)
      (.set (.-scale s) 80 20 1)
      (.set (.-position s) 0 40 0)
      (set! (.-renderOrder s) 10)
      s)))

(defn- peer-mark [player]
  (let [colour (player-colour player)
        g (THREE/Group.)
        cone (THREE/Mesh. (THREE/ConeGeometry. 6 16 20)
                          (THREE/MeshStandardMaterial. #js {:color colour :emissive colour :emissiveIntensity 0.35}))]
    (.rotateX cone js/Math.PI)
    (set! (.. cone -position -y) 12)
    (.add g cone)
    (.add g (name-sprite player colour))
    g))

(defn show-peers!
  "Put a marker where each other player's cursor is: {player [x y z]}."
  [peers]
  (when-let [{:keys [scene]} @world]
    (doseq [[p ^js m] @peer-marks :when (not (contains? peers p))]
      (.remove scene m)
      (swap! peer-marks dissoc p))
    (doseq [[p [x y z]] peers]
      (let [^js m (or (@peer-marks p)
                      (let [m (peer-mark p)] (.add scene m) (swap! peer-marks assoc p m) m))]
        (.set (.-position m) x y z)))
    (redraw!)))
