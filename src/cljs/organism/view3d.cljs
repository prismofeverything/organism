(ns organism.view3d
  "The game in 3D: the printed board and the print pieces, to walk around.

   Loaded as its own module (see shadow-cljs.edn) the first time a viewer turns
   it on, so the 2D page never carries three.js. Everything is in millimetres,
   as the Blender scenes are (pieces/build_play_real.py): a space is about 43 mm
   across, a piece stands at 0.9 on it, food sits on the peg 4.3 mm below the
   piece's top, 6.4 mm per food, the whole stack shown (up to a dozen). The assets and those
   numbers come from pieces/make_web3d.py, which writes
   resources/public/organism/3d/.

   The board is the printed art on one disc. make_web3d.py redraws each printed
   circle exactly where the game puts that space, so a space's world position
   and its place on the texture follow from one transform. Printed circles this
   game has no space for -- a notched corner -- show as printed; the art is
   never covered. Past six players, or seven
   rings, there is no printed board, and the rings are drawn in the game's
   colours instead.

   The scene only draws when something changes -- the camera, the window, the
   position -- rather than every frame."
  (:require
   ["three" :as THREE]
   ["three/examples/jsm/controls/OrbitControls.js" :refer [OrbitControls]]
   ["three/examples/jsm/loaders/GLTFLoader.js" :refer [GLTFLoader]]
   [organism.board :as board]
   [organism.transitions :as transitions]))

(def ^:private asset-root "/organism/3d/")
(def ^:private piece-names ["EAT" "GROW" "MOVE" "FOOD"])
(def ^:private default-mm-per-space 43.2)

;; ── Assets, loaded once per page ──────────────────────────────────────────

(defonce ^:private assets (atom nil))

(defn- load-glb [loader name]
  (js/Promise.
   (fn [ok fail]
     (.load loader (str asset-root name ".glb")
            (fn [^js gltf]
              (let [mesh (atom nil)]
                (.traverse (.-scene gltf) (fn [^js o] (when (and (nil? @mesh) (.-isMesh o)) (reset! mesh o))))
                (let [^js geometry (.-geometry ^js @mesh)]
                  ;; the print meshes are Z-up; three is Y-up
                  (.rotateX geometry (- (/ js/Math.PI 2)))
                  (.computeVertexNormals geometry)
                  (ok [name geometry]))))
            nil fail))))

(defn- load-assets!
  "The pieces, the board metadata, and whichever board textures get used."
  []
  (or @assets
      (let [loader (GLTFLoader.)
            p (js/Promise.all
               (clj->js
                (concat
                 [(-> (js/fetch (str asset-root "board.json"))
                      (.then #(.json %))
                      (.then #(vector :meta (js->clj % :keywordize-keys true))))]
                 (map (partial load-glb loader) piece-names))))]
        (reset! assets
                (.then p (fn [results]
                           (let [pairs (js->clj results)]
                             {:meta (second (first pairs))
                              :geometry (into {} (map (fn [[n g]] [n g]) (rest pairs)))})))))))

(defonce ^:private textures (atom {}))

(defn- texture
  "A board texture. It arrives after the scene is built, and the scene only
   draws on a change, so its arrival is one: `loaded` redraws."
  [name loaded]
  (or (get @textures name)
      (let [t (.load (THREE/TextureLoader.) (str asset-root name) (fn [_] (loaded)))]
        (set! (.-colorSpace t) THREE/SRGBColorSpace)
        (set! (.-anisotropy t) 8)
        (swap! textures assoc name t)
        t)))

;; ── Where things go ───────────────────────────────────────────────────────

(defn- layout
  "Every space of this game, as [x z] millimetres from the centre, and what the
   board is drawn from."
  [game invocation meta]
  (let [rings (:rings game)
        ring-count (count rings)
        player-count (count (:turn-order game))
        symmetry (board/player-symmetry player-count)
        printed (when (and (<= player-count 6) (<= ring-count 7))
                  (get-in meta [:boards (if (= 5 symmetry) :penta :hex)]))
        mm (or (:mm_per_space printed) default-mm-per-space)
        rotation (* (/ js/Math.PI 180) (or (:rotation printed) 30))
        colors (or (seq (:colors invocation))
                   (map vector rings (repeat "#556")))
        place (fn [locations]
                (let [[cx cy] (some (fn [[[ring _] [x y]]] (when (= ring (first rings)) [x y])) locations)]
                  (into {} (for [[space [x y]] locations
                                 :let [dx (- x cx) dy (- y cy)]]
                             [space [(* mm (- (* dx (js/Math.cos rotation)) (* dy (js/Math.sin rotation))))
                                     (* mm (+ (* dx (js/Math.sin rotation)) (* dy (js/Math.cos rotation))))]]))))
        spaces (place (board/board-locations symmetry 1.0 1.0 (take ring-count colors)))]
    {:spaces (select-keys spaces (keys (:adjacencies game)))
     :rings rings
     :printed printed
     :mm mm
     :ring-count ring-count
     :ring-colors (into {} colors)}))

;; ── The scene ─────────────────────────────────────────────────────────────

(defn- request-render! [view]
  (when-not (:pending @view)
    (swap! view assoc :pending true)
    (js/requestAnimationFrame
     (fn []
       (swap! view assoc :pending false)
       (when-let [{:keys [renderer scene camera on-frame]} @view]
         (when renderer
           (.render renderer scene camera)
           ;; the page's highlights are placed by projecting board points, so
           ;; they follow every frame the camera moves
           (when on-frame (on-frame))))))))

(defn- disc [radius color y & [segments]]
  (let [m (THREE/Mesh. (THREE/CircleGeometry. radius (or segments 48))
                       (THREE/MeshStandardMaterial. #js {:color color :roughness 0.9}))]
    (.rotateX m (- (/ js/Math.PI 2)))
    (set! (.. m -position -y) y)
    (set! (.-receiveShadow m) true)
    m))

(defn- outline
  "The board's edge, always round: the printed board's own rim for a full
   board, and for a smaller game a circle a little way past its outer ring --
   so a sliver of the next printed ring shows, as on the real board."
  [{:keys [mm ring-count printed]}]
  (let [shape (THREE/Shape.)
        radius (if (and printed (= ring-count (:rings printed)))
                 (/ (:board_mm printed) 2)
                 (* mm (+ (dec ring-count) 0.7)))]
    (.absarc shape 0 0 radius 0 (* 2 js/Math.PI) false)
    shape))

(defn- flat-shape-mesh
  "A shape laid on the table, textured from world position when it carries a
   texture, the way the printed art is mapped."
  [shape material board-mm y]
  (let [geometry (THREE/ShapeGeometry. shape 24)
        position (.getAttribute geometry "position")
        uv (.getAttribute geometry "uv")]
    (when board-mm
      (dotimes [i (.-count position)]
        ;; shape y is world z; the art's rows run down the page, which is +z
        (.setXY uv i (+ 0.5 (/ (.getX position i) board-mm)) (- 0.5 (/ (.getY position i) board-mm)))))
    ;; +90 degrees about x takes the shape's y to the world's z, which is how
    ;; the outline was drawn -- and turns its face to the table, so it is drawn
    ;; from both sides
    (.rotateX geometry (/ js/Math.PI 2))
    (set! (.-side material) THREE/DoubleSide)
    (let [m (THREE/Mesh. geometry material)]
      (set! (.. m -position -y) y)
      (set! (.-receiveShadow m) true)
      m)))

(defn- rim [^js shape]
  (let [points (.getPoints shape 240)
        curve (THREE/CatmullRomCurve3.
               (clj->js (map (fn [^js p] (THREE/Vector3. (.-x p) 0.4 (.-y p))) points)) true)]
    (THREE/Mesh. (THREE/TubeGeometry. curve 480 2.6 10 true)
                 (THREE/MeshStandardMaterial. #js {:color "#1b2230" :roughness 0.6}))))

(defn- printed-board
  "The printed art on a board the size of this game's, texture placed so each
   space falls on its own printed circle."
  [{:keys [printed] :as geometry} _rings redraw]
  (let [group (THREE/Group.)
        shape (outline geometry)]
    (.add group (flat-shape-mesh shape
                                 (THREE/MeshStandardMaterial. #js {:map (texture (:texture printed) redraw)
                                                                   :roughness 0.85})
                                 (:board_mm printed) 0))
    (.add group (rim shape))
    group))

(defn- coloured-board
  "No printed board for this game: the rings in the game's own colours."
  [{:keys [mm spaces ring-colors] :as geometry} _rings]
  (let [group (THREE/Group.)
        shape (outline geometry)]
    (.add group (flat-shape-mesh shape (THREE/MeshStandardMaterial. #js {:color "#1d2430" :roughness 0.9}) nil 0))
    (doseq [[[ring _] [x z]] spaces]
      (let [c (disc (* 0.46 mm) (get ring-colors ring "#556") 0.2 32)]
        (set! (.. c -position -x) x)
        (set! (.. c -position -z) z)
        (.add group c)))
    (.add group (rim shape))
    group))

(defn- material [view color]
  (or (get-in @view [:materials color])
      (let [m (THREE/MeshStandardMaterial. #js {:color color :roughness 0.45 :metalness 0.0})]
        (swap! view assoc-in [:materials color] m)
        m)))

(def ^:private food-color "#f2e6a0")

;; ── The real pieces' colours ──────────────────────────────────────────────

(defn- hsl
  "#rrggbb or hsl(h,s%,l%) as [hue 0-1, saturation, lightness]."
  [color]
  (let [c (THREE/Color.) out #js {}]
    (.setStyle c (str color))
    (.getHSL c out)
    [(.-h out) (.-s out) (.-l out)]))

(defn- hue-distance
  "How far apart two colours are as a player would name them: by hue, round
   the colour wheel. A washed-out colour has no hue to speak of -- a grey is
   nearest the real Dark, which is the only muted piece colour -- so
   saturation counts as well."
  [a b]
  (let [[ha sa] (hsl a) [hb sb] (hsl b)
        around (js/Math.abs (- ha hb))
        around (min around (- 1 around))
        muted (fn [s] (if (< s 0.22) 1 0))]
    (+ (* 2 (js/Math.abs (- (muted sa) (muted sb)))) around)))

(defn physical-colors
  "Each player's colour on the real pieces: the one nearest their colour
   online by hue, each used once, nearest pairs first. Past six players there
   are no more real colours, and the online ones stay."
  [player-colors piece-colors]
  (if (or (empty? piece-colors) (> (count player-colors) (count piece-colors)))
    player-colors
    (let [pairs (sort-by first (for [[player online] player-colors
                                     real piece-colors]
                                 [(hue-distance online real) player real]))]
      (loop [[[_ player real] & more] pairs
             assigned {}
             used #{}]
        (cond
          (nil? player) assigned
          (or (contains? assigned player) (contains? used real)) (recur more assigned used)
          :else (recur more (assoc assigned player real) (conj used real)))))))

(defn- piece-mesh [geometry mat x y z scale turn]
  (let [m (THREE/Mesh. geometry mat)]
    (.set (.-position m) x y z)
    (.setScalar (.-scale m) scale)
    (set! (.. m -rotation -y) turn)
    (set! (.-castShadow m) true)
    (set! (.-receiveShadow m) true)
    m))

(defn- space-turn
  "A fixed, space-dependent twist, so a board of pieces does not look stamped."
  [[ring step]]
  (* 0.37 (+ (* 7 (.charCodeAt (str ring) 0)) (* 13 step))))

(defn- space-contents
  "What stands on a space, as the value its meshes are rebuilt from."
  [state space]
  (let [element (get-in state [:elements space])]
    [(select-keys element [:type :player :food]) (get-in state [:food space] 0)]))

(defn- food-height
  "How high the kth food on a space sits: on a piece's peg, or on the board."
  [meta type k]
  (let [piece-scale (:piece_scale meta 0.9)
        step (:food_step meta 6.4)]
    (if type
      (let [height (get-in meta [:pieces (keyword (.toUpperCase (name type))) :height] 40)]
        (+ 1 (* piece-scale (- height (:peg_below_top meta 4.3))) (* k step)))
      (+ 1 (* k step)))))

(defn- build-space
  "What stands on one space, as a group placed on it, its meshes local to it
   -- so a move slides the group, and growing or being lost scales it where
   it stands."
  [view {:keys [geometry meta]} player-colors [x z] [element free-food] space]
  (let [group (THREE/Group.)
        piece-scale (:piece_scale meta 0.9)
        food-scale (:food_scale meta 0.94)
        shown (:food_shown meta 12)
        food-mat (material view food-color)
        turn (space-turn space)
        type (:type element)]
    (.set (.-position group) x 0 z)
    ;; tagged, so a target can find the piece, or the kth food, on a space
    (when type
      (let [^js m (piece-mesh (get geometry (.toUpperCase (name type)))
                              (material view (get player-colors (:player element) "#888"))
                              0 1 0 piece-scale turn)]
        (set! (.-userData m) #js {:role "piece"})
        (.add group m)))
    (dotimes [k (min shown (if type (:food element 0) free-food))]
      (let [^js m (piece-mesh (get geometry "FOOD") food-mat 0 (food-height meta type k) 0 food-scale turn)]
        (set! (.-userData m) #js {:role "food" :index k})
        (.add group m)))
    group))

(defn- piece-at
  "The piece mesh standing on a space, or the kth food there."
  [view space & [food-index]]
  (when-let [^js g (get-in @view [:shown space :group])]
    (some (fn [^js m]
            (let [d (.-userData m)]
              (if food-index
                (when (and (= "food" (.-role d)) (= food-index (.-index d))) m)
                (when (= "piece" (.-role d)) m))))
          (array-seq (.-children g)))))

;; ── Animation ─────────────────────────────────────────────────────────────
;;
;; Frames are drawn continuously only while something is moving; otherwise the
;; view draws on change. Each animation is a duration and a function of eased
;; progress, with something to do when it lands.

(defn- ease [t] (if (< t 0.5) (* 2 t t) (- 1 (/ (js/Math.pow (+ (* -2 t) 2) 2) 2))))

(defn- tick! [view]
  (let [now (js/performance.now)
        {:keys [anims renderer scene camera on-frame]} @view]
    (if-not renderer
      (swap! view assoc :animating false)
      (let [running (reduce (fn [running {:keys [start ms step done linear] :as a}]
                              (let [t (min 1 (/ (- now start) ms))]
                                (step (if linear t (ease t)))
                                (if (< t 1)
                                  (conj running a)
                                  (do (when done (done)) running))))
                            [] anims)]
        ;; anything started by a landing animation is kept
        (swap! view update :anims (fn [all] (vec (concat running (drop (count anims) all)))))
        (.render renderer scene camera)
        (when on-frame (on-frame))
        (if (seq (:anims @view))
          (js/requestAnimationFrame #(tick! view))
          (swap! view assoc :animating false))))))

(defn- animate!
  "Run `step` on progress from 0 to 1 over `ms`, eased unless `linear` -- a
   thing thrown keeps its speed; `done` when it lands."
  [view ms step & [done linear]]
  (swap! view update :anims (fnil conj [])
         {:start (js/performance.now) :ms ms :step step :done done :linear linear})
  (when-not (:animating @view)
    (swap! view assoc :animating true)
    (js/requestAnimationFrame #(tick! view))))

(def ^:private move-ms 520)
(def ^:private grow-ms 420)
(def ^:private flight-ms 560)

(defn- food-meshes
  "The food shown on a space's group, bottom to top."
  [^js g]
  (sort-by #(.. ^js % -userData -index)
           (filter #(= "food" (.. ^js % -userData -role)) (array-seq (.-children g)))))

(defn- sync-pieces!
  "Bring the pieces to `state`. Spaces the change touches get animated the way
   the transitions say -- a move slides along the board, a new piece rises in, a lost one sinks
   away, circulated food is thrown across in a parabola -- and show their new contents once it
   lands. With no transitions, or too many to follow, everything just appears."
  [view {:keys [meta geometry] :as loaded} {:keys [spaces]} state player-colors changes before]
  (let [{:keys [pieces-group shown]} @view
        wanted (into {} (for [space (distinct (concat (keys (:elements state)) (keys (:food state))))
                              :when (contains? spaces space)
                              :let [contents (space-contents state space)]
                              :when (or (:type (first contents)) (pos? (second contents)))]
                          [space contents]))
        changed (set (filter #(not= (get-in shown [% :contents]) (get wanted %))
                             (distinct (concat (keys shown) (keys wanted)))))
        fresh (into {} (for [space changed
                             :when (contains? wanted space)]
                         (let [g (build-space view loaded player-colors (get spaces space) (get wanted space) space)]
                           (.add pieces-group g)
                           [space g])))
        old (fn [space] (get-in shown [space :group]))
        remove! (fn [g] (when g (.remove pieces-group g)))
        reveal! (fn [g] (when g (set! (.-visible g) true)))
        hide! (fn [g] (when g (set! (.-visible g) false)))
        animate? (and before (seq changes) (<= (count changes) 24))
        receiving (set (keep #(when (= :circulate (:type %)) (:to %)) changes))
        claimed (atom #{})]
    (when animate?
      (doseq [{:keys [type from to space] :as change} changes]
        (case type
          :move
          (when-let [^js g (old from)]
            (swap! claimed conj from to)
            (hide! (get fresh to))
            (let [[ax az] (get spaces from) [bx bz] (get spaces to)
                  ;; each space twists its piece its own way: turn to the new
                  ;; space's twist on the way, the short way round, rather than
                  ;; snapping to it on landing
                  d (- (space-turn to) (space-turn from))
                  twist (js/Math.atan2 (js/Math.sin d) (js/Math.cos d))]
              (animate! view move-ms
                        (fn [e] (.set (.-position g) (+ ax (* e (- bx ax))) 0 (+ az (* e (- bz az))))
                          (set! (.. g -rotation -y) (* e twist)))
                        (fn [] (remove! g) (reveal! (get fresh to))))))

          :grow
          (when-let [^js g (get fresh to)]
            (swap! claimed conj to)
            (remove! (old to))
            (.setScalar (.-scale g) 0.001)
            (animate! view grow-ms (fn [e] (.setScalar (.-scale g) (max 0.001 e)))))

;; food made from nothing grows in from nothing, as in the gameplay video
          (:food-up :free-food-appear)
          (when-let [^js g (and (not (contains? @claimed space)) (get fresh space))]
            (let [foods (food-meshes g)
                  new (drop (max 0 (- (count foods) (:amount change 1))) foods)
                  full (:food_scale meta 0.94)]
              (doseq [^js m new] (.setScalar (.-scale m) 0.001))
              (animate! view grow-ms (fn [e] (doseq [^js m new] (.setScalar (.-scale m) (max 0.001 (* full e))))))))

          ;; and food used up shrinks away where it was
          (:food-down :free-food-vanish)
          (when-let [^js g (and (not (contains? @claimed space)) (old space))]
            (swap! claimed conj space)
            (hide! (get fresh space))
            (let [foods (food-meshes g)
                  gone (drop (max 0 (- (count foods) (:amount change 1))) foods)
                  full (:food_scale meta 0.94)]
              (animate! view grow-ms
                        (fn [e] (doseq [^js m gone] (.setScalar (.-scale m) (max 0.001 (* full (- 1 e))))))
                        (fn [] (remove! g) (reveal! (get fresh space))))))

          :lose
          (when-let [^js g (old space)]
            (swap! claimed conj space)
            (animate! view grow-ms
                      (fn [e] (.setScalar (.-scale g) (max 0.001 (- 1 e)))
                        (set! (.. g -position -y) (* -10 e)))
                      (fn [] (remove! g))))

          :circulate
          ;; the food itself is taken: the top of the giving stack lifts off
          ;; as one, the stack left behind shows at once, and the receiving
          ;; stack takes it only when it lands
          (let [[ax az] (get spaces from) [bx bz] (get spaces to)
                keep-b (old to)
                shown-max (:food_shown meta 12)
                had (:from-food-before change 1)
                has (:to-food-after change 1)
                amount (:amount change 1)
                lifted (max 1 (min amount shown-max had))
                from-type (get-in before [:elements from :type])
                to-type (get-in state [:elements to :type])
                top-from (dec (min shown-max had))
                top-to (dec (min shown-max has))]
            (when (and ax bx)
              (swap! claimed conj from to)
              ;; a stack that is also receiving is left to that animation
              (when-not (contains? receiving from) (remove! (old from)))
              (hide! (get fresh to))
              (let [tokens (vec (for [i (range lifted)]
                                  (let [^js token (piece-mesh (get geometry "FOOD") (material view food-color)
                                                              ax 0 az (:food_scale meta 0.94) 0)]
                                    (.add pieces-group token)
                                    ;; i counts down from the top of each stack
                                    {:token token
                                     :y0 (food-height meta from-type (max 0 (- top-from i)))
                                     :y1 (food-height meta to-type (max 0 (- top-to i)))})))]
                ;; thrown: steady across the table, height a parabola in time
                (animate! view flight-ms
                          (fn [t]
                            (doseq [{:keys [^js token y0 y1]} tokens]
                              (.set (.-position token) (+ ax (* t (- bx ax)))
                                    (+ y0 (* t (- y1 y0)) (* 4 45 t (- 1 t)))
                                    (+ az (* t (- bz az))))))
                          (fn []
                            (doseq [{:keys [token]} tokens] (remove! token))
                            (remove! keep-b)
                            (reveal! (get fresh to)))
                          true))))
          nil)))
    ;; everything no animation took charge of changes at once
    (doseq [space changed
            :when (not (contains? @claimed space))]
      (remove! (old space)))
    (swap! view assoc :shown
           (merge (into {} (filter (fn [[space _]] (not (contains? changed space))) shown))
                  (into {} (for [[space g] fresh] [space {:contents (get wanted space) :group g}]))))))

;; ── Targets ───────────────────────────────────────────────────────────────
;;
;; What can be chosen, from organism.targets, shown in the scene: pieces and
;; food light up and glow outward, spaces burn with plasma, types to pick
;; from hover as ghost pieces over the space, and a FLOW choice not yet
;; committed stands as a ghost where it will land. All of it lives in the scene,
;; so it holds still as the camera moves and never covers a piece.
;;
;; Choosing goes a level at a time: a piece, then where it goes, then which
;; type. Clicking the empty board steps back out.

(def ^:private tone-colors
  {:act "#ffe39a" :dest "#9ff6ff" :pending "#cdb8ff" :pass "#ff9d9d" :chosen "#ffc23c"})

;; A piece that can be chosen stands in a spotlight from high above, in the
;; tone's colour: lit, not washed out. Pointed at -- or once chosen -- it
;; glows as well: light from within, a halo round it, and light cast on the
;; board about it.
;;
;; The lights are fixed pools, dark when unused -- adding and removing lights
;; would make three.js rebuild every material's shader each time the choices
;; change.
(def ^:private glow-lights 8)
(def ^:private spot-height 230.0)
(def ^:private spot-light 3.75)
(def ^:private halo-reach 1.5)
(def ^:private halo-opacity 0.4)
(def ^:private glow-light 30.0)
(def ^:private inner-glow 0.4)

(def ^:private halo-texture
  "Light falling off from a centre: bright where the piece stands in front of
   it, gone by the edge."
  (delay
    (let [n 128
          canvas (doto (js/document.createElement "canvas") (set! -width n) (set! -height n))
          ^js ctx (.getContext canvas "2d")
          ^js g (.createRadialGradient ctx (/ n 2) (/ n 2) 0 (/ n 2) (/ n 2) (/ n 2))]
      (doseq [[at alpha] [[0 1] [0.3 0.7] [0.55 0.28] [0.8 0.07] [1 0]]]
        (.addColorStop g at (str "rgba(255,255,255," alpha ")")))
      (set! (.-fillStyle ctx) g)
      (.fillRect ctx 0 0 n n)
      (THREE/CanvasTexture. canvas))))

(defn- light-up!
  "Make a piece glow from within in `tone`: its own material, copied, with
   light of that colour in it. Returns [mesh original-material lit-material]
   so it can be put back."
  [^js mesh tone]
  (let [original (.-material mesh)
        ^js lit (.clone original)]
    (.set (.-emissive lit) (get tone-colors tone "#ffffff"))
    (set! (.-emissiveIntensity lit) 0)
    (set! (.-material mesh) lit)
    [mesh original lit]))

(defn- shell!
  "A glow round `mesh`: soft light behind it, always facing the eye and a
   little wider than the piece, so the piece seems to shine outward."
  [^js mesh tone]
  (let [^js geometry (.-geometry mesh)
        _ (when-not (.-boundingSphere geometry) (.computeBoundingSphere geometry))
        ^js sphere (.-boundingSphere geometry)
        size (* 2 halo-reach (.-radius sphere))
        glow (THREE/Sprite.
              (THREE/SpriteMaterial. #js {:map @halo-texture :color (get tone-colors tone "#ffffff")
                                          :transparent true :opacity 0
                                          :depthWrite false
                                          :blending THREE/AdditiveBlending}))]
    (.copy (.-position glow) (.-center sphere))
    (.set (.-scale glow) size size 1)
    (set! (.-raycast glow) (fn [])) ; the piece is what is picked, not its light
    (set! (.-userData glow) #js {:role "shell" :size size})
    (.add mesh glow)
    glow))

;; ── Plasma ────────────────────────────────────────────────────────────────
;;
;; The golden fire that marks a space in the gameplay clips, ported from its
;; Blender shader (pieces/build_clip.py, plasma_mat): an open cylinder, 4D noise
;; scrolling upward and morphing, masked into wisps, a dense pool at the base
;; thinning to vapour and gone by 85% of the height, coloured by height from a
;; hot gold base to a deep tip. The same numbers, bar a little more body -- a
;; taller column (23 mm, the clips' is 18) with a denser base: radius 21 mm,
;; noise scale 0.18 with height squashed by half, rising 0.16 and morphing 0.15
;; per frame, wisps from 0.30 (0.20 at the base) to 0.68 of the noise -- but at a fifth of the
;; clips' 24 fps: on a board you sit and study, full speed is a flicker.
;;
;; Blender blooms it in the compositor, which makes the column's edges burn.
;; Here the edges are brightened by the angle they are seen at, and the fire
;; is blended over the board rather than added to it -- added gold turns green
;; over a blue ring.

(def ^:private plasma-radius 21.0)
(def ^:private plasma-gain 1.6)
(def ^:private plasma-gain-hover 2.6)
(def ^:private plasma-height 23.0)

(def ^:private plasma-vertex
  "varying vec3 vObj; varying float vH; varying vec3 vNormal; varying vec3 vView;
   uniform float uHeight;
   void main() {
     vObj = position;
     vH = clamp(position.y / uHeight, 0.0, 1.0);
     vec4 mv = modelViewMatrix * vec4(position, 1.0);
     vNormal = normalize(normalMatrix * normal);
     vView = normalize(-mv.xyz);
     gl_Position = projectionMatrix * mv;
   }")

(def ^:private plasma-fragment
  "varying vec3 vObj; varying float vH; varying vec3 vNormal; varying vec3 vView;
   uniform float uTime; uniform float uGain; uniform vec3 uBase; uniform vec3 uMid; uniform vec3 uTip;
   // 4D simplex noise (Ashima Arts / Stefan Gustavson, MIT)
   vec4 mod289(vec4 x){return x-floor(x*(1.0/289.0))*289.0;}
   float mod289(float x){return x-floor(x*(1.0/289.0))*289.0;}
   vec4 permute(vec4 x){return mod289(((x*34.0)+1.0)*x);}
   float permute(float x){return mod289(((x*34.0)+1.0)*x);}
   vec4 taylorInvSqrt(vec4 r){return 1.79284291400159-0.85373472095314*r;}
   float taylorInvSqrt(float r){return 1.79284291400159-0.85373472095314*r;}
   vec4 grad4(float j, vec4 ip){
     const vec4 ones=vec4(1.0,1.0,1.0,-1.0); vec4 p,s;
     p.xyz=floor(fract(vec3(j)*ip.xyz)*7.0)*ip.z-1.0;
     p.w=1.5-dot(abs(p.xyz),ones.xyz);
     s=vec4(lessThan(p,vec4(0.0)));
     p.xyz=p.xyz+(s.xyz*2.0-1.0)*s.www; return p;}
   float snoise(vec4 v){
     const vec4 C=vec4(0.138196601125011,0.276393202250021,0.414589803375032,-0.447213595499958);
     vec4 i=floor(v+dot(v,vec4(0.309016994374947451)));
     vec4 x0=v-i+dot(i,C.xxxx);
     vec4 i0; vec3 isX=step(x0.yzw,x0.xxx); vec3 isYZ=step(x0.zww,x0.yyz);
     i0.x=isX.x+isX.y+isX.z; i0.yzw=1.0-isX;
     i0.y+=isYZ.x+isYZ.y; i0.zw+=1.0-isYZ.xy; i0.z+=isYZ.z; i0.w+=1.0-isYZ.z;
     vec4 i3=clamp(i0,0.0,1.0); vec4 i2=clamp(i0-1.0,0.0,1.0); vec4 i1=clamp(i0-2.0,0.0,1.0);
     vec4 x1=x0-i1+C.xxxx; vec4 x2=x0-i2+C.yyyy; vec4 x3=x0-i3+C.zzzz; vec4 x4=x0+C.wwww;
     i=mod289(i);
     float j0=permute(permute(permute(permute(i.w)+i.z)+i.y)+i.x);
     vec4 j1=permute(permute(permute(permute(i.w+vec4(i1.w,i2.w,i3.w,1.0))+i.z+vec4(i1.z,i2.z,i3.z,1.0))+i.y+vec4(i1.y,i2.y,i3.y,1.0))+i.x+vec4(i1.x,i2.x,i3.x,1.0));
     vec4 ip=vec4(1.0/294.0,1.0/49.0,1.0/7.0,0.0);
     vec4 p0=grad4(j0,ip); vec4 p1=grad4(j1.x,ip); vec4 p2=grad4(j1.y,ip); vec4 p3=grad4(j1.z,ip); vec4 p4=grad4(j1.w,ip);
     vec4 norm=taylorInvSqrt(vec4(dot(p0,p0),dot(p1,p1),dot(p2,p2),dot(p3,p3)));
     p0*=norm.x; p1*=norm.y; p2*=norm.z; p3*=norm.w; p4*=taylorInvSqrt(dot(p4,p4));
     vec3 m0=max(0.6-vec3(dot(x0,x0),dot(x1,x1),dot(x2,x2)),0.0);
     vec2 m1=max(0.6-vec2(dot(x3,x3),dot(x4,x4)),0.0);
     m0=m0*m0; m1=m1*m1;
     return 49.0*(dot(m0*m0,vec3(dot(p0,x0),dot(p1,x1),dot(p2,x2)))+dot(m1*m1,vec2(dot(p3,x3),dot(p4,x4))));}
   // Blender's noise Fac: 0..1, detail 1.5, roughness 0.4
   float fac(vec4 p){
     float n=snoise(p)+0.4*0.5*snoise(p*2.0+17.3);
     return clamp(0.5+0.5*n/1.2,0.0,1.0);}
   float ramp(float h){
     if(h<0.28) return mix(1.0,0.8,h/0.28);
     if(h<0.85) return mix(0.8,0.0,(h-0.28)/0.57);
     return 0.0;}
   void main(){
     float frame=uTime*24.0*0.2;
     vec3 q=vec3(vObj.x, vObj.z, vObj.y*0.5 + frame*0.16);
     float f=fac(vec4(q*0.18, frame*0.15));
     // the wisps close up toward the base, so the pool there is solid fire
     float lo=mix(0.20,0.30,smoothstep(0.0,0.35,vH));
     float flame=clamp((f-lo)/0.38,0.0,1.0);
     float d=ramp(vH)*flame;
     // seen edge-on the wall is a long look through the fire: the column's
     // silhouette burns brightest, as the bloomed clips show it
     float edge=1.0-abs(dot(normalize(vNormal),normalize(vView)));
     float rim=0.45+2.4*pow(edge,2.2);
     vec3 col=vH<0.5 ? mix(uBase,uMid,vH/0.5) : mix(uMid,uTip,(vH-0.5)/0.5);
     // the fire covers what is behind it rather than adding to it, so it
     // stays gold over a blue ring as over a red one
     float a=clamp(d*rim*0.55*uGain,0.0,0.92);
     gl_FragColor=vec4(min(col*(0.85+0.25*uGain),vec3(1.0)), a);
   }")

(defn- plasma-colors
  "Gold, as in the clips, or a player's colour worked into the same gradient:
   a hot core, the colour, a deep tip."
  [color]
  (if-not color
    [[1.0 0.72 0.22] [1.0 0.50 0.12] [1.0 0.34 0.06]]
    (let [c (THREE/Color. color)
          [r g b] [(.-r c) (.-g c) (.-b c)]
          mix (fn [t [r2 g2 b2]] [(+ r (* t (- r2 r))) (+ g (* t (- g2 g))) (+ b (* t (- b2 b)))])]
      [(mix 0.30 [1 1 1]) [r g b] (mix 0.35 [0 0 0])])))

(defn- plasma!
  "A plasma column on a space, and an invisible disc to click it by."
  [layer [x z] mm color]
  (let [[base mid tip] (plasma-colors color)
        v3 (fn [[r g b]] (THREE/Vector3. r g b))
        uniforms #js {:uTime #js {:value (/ (js/performance.now) 1000)}
                      :uGain #js {:value plasma-gain}
                      :uHeight #js {:value plasma-height}
                      :uBase #js {:value (v3 base)} :uMid #js {:value (v3 mid)} :uTip #js {:value (v3 tip)}}
        mat (THREE/ShaderMaterial. #js {:uniforms uniforms :vertexShader plasma-vertex
                                        :fragmentShader plasma-fragment :transparent true
                                        :depthWrite false :side THREE/DoubleSide})
        geometry (THREE/CylinderGeometry. plasma-radius plasma-radius plasma-height 48 1 true)
        _ (.translate geometry 0 (/ plasma-height 2) 0)
        column (THREE/Mesh. geometry mat)
        pick (THREE/Mesh. (THREE/CircleGeometry. (* 0.5 mm) 32)
                          (THREE/MeshBasicMaterial. #js {:transparent true :opacity 0 :depthWrite false}))
        g (THREE/Group.)]
    (.rotateX pick (- (/ js/Math.PI 2)))
    (.add g column) (.add g pick)
    (.set (.-position g) x 0.2 z)
    (.add layer g)
    {:pick pick :looks [] :base [] :plasma uniforms :column column}))

(defn- plasma-loop!
  "Plasma moves, so while any is showing the view draws continuously -- at
   most 30 frames a second, and not at all once none is left."
  [view]
  (when-not (:plasma-running @view)
    (swap! view assoc :plasma-running true)
    (let [last (atom 0)]
      (letfn [(frame [now]
                (let [{:keys [visuals renderer scene camera on-frame]} @view
                      live (keep (comp :plasma second) visuals)]
                  (if (or (not renderer) (empty? live))
                    (swap! view assoc :plasma-running false)
                    (do
                      (when (> (- now @last) 33)
                        (reset! last now)
                        (doseq [^js u live] (set! (.. u -uTime -value) (/ now 1000)))
                        (when-not (:animating @view)
                          (.render renderer scene camera)
                          (when on-frame (on-frame))))
                      (js/requestAnimationFrame frame)))))]
        (js/requestAnimationFrame frame)))))

(defn- ring! [layer [x z] mm tone]
  (let [g (THREE/Group.)
        ring (THREE/Mesh. (THREE/RingGeometry. (* 0.4 mm) (* 0.5 mm) 48)
                          (THREE/MeshBasicMaterial. #js {:color (get tone-colors tone) :transparent true
                                                         :opacity 0.8 :depthWrite false}))
        disc (THREE/Mesh. (THREE/CircleGeometry. (* 0.5 mm) 48)
                          (THREE/MeshBasicMaterial. #js {:color (get tone-colors tone) :transparent true
                                                         :opacity 0.16 :depthWrite false}))]
    (doseq [^js m [ring disc]]
      (.rotateX m (- (/ js/Math.PI 2))))
    (.add g ring) (.add g disc)
    (.set (.-position g) x 0.8 z)
    (.add layer g)
    {:pick disc :looks [(.-material ring) (.-material disc)] :base [0.8 0.16]}))

(defn- ghost! [view layer {:keys [geometry meta]} type color [x y z] scale tone opacity]
  (let [mat (THREE/MeshStandardMaterial. #js {:color color :roughness 0.4 :transparent true
                                              :opacity opacity :emissive (get tone-colors tone)
                                              :emissiveIntensity 0.25})
        ^js m (THREE/Mesh. (get geometry (.toUpperCase (name type))) mat)]
    (.set (.-position m) x y z)
    (.setScalar (.-scale m) scale)
    (.add layer m)
    (let [^js sh (shell! m tone)]
      {:pick m :shell sh})))

(def ^:private option-disc
  "The backing for a type to pick: a dark disc with a rim in the tone's colour,
   one texture per tone."
  (memoize
   (fn [tone]
     (let [n 128
           canvas (doto (js/document.createElement "canvas") (set! -width n) (set! -height n))
           ^js ctx (.getContext canvas "2d")]
       (.beginPath ctx)
       (.arc ctx (/ n 2) (/ n 2) (- (/ n 2) 6) 0 (* 2 js/Math.PI))
       (set! (.-fillStyle ctx) "rgba(14,18,32,0.88)")
       (.fill ctx)
       (set! (.-lineWidth ctx) 7)
       (set! (.-strokeStyle ctx) (get tone-colors tone "#ffffff"))
       (.stroke ctx)
       (THREE/CanvasTexture. canvas)))))

(defn- option!
  "A type to pick: the piece itself, solid, in front of a disc that faces the
   eye -- so it reads as a button, not as part of the board. The disc is what
   is clicked, so the whole of it counts."
  [layer {:keys [geometry]} type color [x y z] scale tone mm]
  (let [^js g (get geometry (.toUpperCase (name type)))
        _ (when-not (.-boundingSphere g) (.computeBoundingSphere g))
        centre (* scale (.. g -boundingSphere -center -y))
        ;; transparent only so it is drawn after its disc; it is fully solid
        mat (THREE/MeshStandardMaterial. #js {:color color :roughness 0.4 :transparent true :opacity 1
                                              :emissive (get tone-colors tone) :emissiveIntensity 0.1})
        m (THREE/Mesh. g mat)
        size (* 0.86 mm)
        disc (THREE/Sprite. (THREE/SpriteMaterial. #js {:map (option-disc tone) :transparent true
                                                        :opacity 0.9 :depthWrite false}))]
    (.set (.-position m) x y z)
    (.setScalar (.-scale m) scale)
    (set! (.-renderOrder m) 3)
    (.set (.-position disc) x (+ y centre) z)
    (.set (.-scale disc) size size 1)
    (set! (.-renderOrder disc) 2)
    (.add layer disc)
    (.add layer m)
    {:pick disc :looks [(.-material disc)] :base [0.9] :swell [disc size]}))

(def ^:private heat-ms 180)

(defn- heat!
  "Set how far a piece's glow is on, 0 to 1: its light from within, its halo,
   and the light it casts."
  [^js shell ^js lit h]
  (when lit (set! (.-emissiveIntensity lit) (* h inner-glow)))
  (when shell
    (let [size (* (.. shell -userData -size) (+ 1 (* 0.12 h)))]
      (set! (.. shell -userData -heat) h)
      (set! (.. shell -material -opacity) (* h halo-opacity))
      (.set (.-scale shell) size size 1)
      (when-let [^js light (.. shell -userData -light)]
        (set! (.-intensity light) (* h glow-light))))))

(defn- hot!
  "Turn a piece's glow on or off: at once without a `view`, or eased from
   wherever it stands -- so a pointer passing over fades it in and out, and
   leaving mid-fade turns it back without a jump."
  ([^js shell ^js lit on?] (heat! shell lit (if on? 1 0)))
  ([view ^js shell ^js lit on?]
   (let [from (or (some-> shell .-userData .-heat) 0)
         to (if on? 1 0)
         token (js-obj)]
     (when (not= from to)
       (when shell (set! (.. shell -userData -fade) token))
       (animate! view (* heat-ms (js/Math.abs (- to from)))
                 (fn [t]
                   ;; a newer fade on this piece has taken over
                   (when (or (nil? shell) (identical? token (.. shell -userData -fade)))
                     (heat! shell lit (+ from (* t (- to from))))))
                 nil true)))))

(defn- light-glows!
  "Put a spotlight of the pool over each glow, and a glow light at it, as far
   as the pools go; the rest go dark."
  [view glows]
  (doseq [[i ^js light ^js spot] (map vector (range) (:lights @view) (:spots @view))]
    (if-let [^js glow (get glows i)]
      (let [at (.getWorldPosition glow (THREE/Vector3.))]
        (.copy (.-position light) at)
        (.copy (.-color light) (.. glow -material -color))
        (set! (.-intensity light) (if (.. glow -userData -hot) glow-light 0))
        (set! (.. glow -userData -light) light)
        (.set (.-position spot) (.-x at) (+ (.-y at) spot-height) (.-z at))
        (.set (.. spot -target -position) (.-x at) 0 (.-z at))
        (.copy (.-color spot) (.. glow -material -color))
        (set! (.-intensity spot) spot-light))
      (do (set! (.-intensity light) 0)
          (set! (.-intensity spot) 0)))))

(defn- clear-targets! [view]
  (let [{:keys [target-layer shells lit]} @view]
    (doseq [^js sh shells] (when-let [p (.-parent sh)] (.remove p sh)))
    (doseq [[^js mesh original ^js copy] lit]
      (set! (.-material mesh) original)
      (.dispose copy))
    (when target-layer (.clear target-layer))
    (swap! view assoc :shells [] :lit [] :pickables [] :hover nil)))

(defn- pay-level
  "Paying for a growth: the food on each grower that can still give, and what
   has been given so far."
  [view]
  (let [{:keys [spent variants]} (:pay @view)
        state (:last-state @view)]
    (for [grower (distinct (mapcat (comp keys :spent) variants))
          :let [have (get-in state [:elements grower :food] 0)
                given (get spent grower 0)]
          index (range (min have (get-in @view [:meta :food_shown] 12)))]
      {:kind :food :space grower :index index :tone (if (< index given) :chosen :act)
       :label (str "pay with this food (" (reduce + 0 (vals spent)) " of " (:cost (:pay @view)) ")")
       :pay-grower grower})))

(defn- level
  "The targets on show: those revealed by the last thing chosen, or the top."
  [view]
  (let [{:keys [targets stack pay]} @view]
    (cond
      pay (pay-level view)
      (seq stack) (:next (peek stack))
      :else targets)))

(defn- render-targets!
  "Show the current level of targets in the scene."
  [view]
  (clear-targets! view)
  (let [{:keys [target-layer spaces mm stack assets me-color camera]} @view
        pickables (atom [])
        shells (atom [])
        visuals (atom [])
        mark! (fn [target look] (when look (swap! pickables conj [(:pick look) target]) (swap! visuals conj [target look])))
        lit (atom [])
        outline (fn [^js mesh tone & [hot]]
                  (when mesh
                    (let [^js sh (shell! mesh tone)
                          [_ _ glow :as l] (light-up! mesh tone)]
                      (swap! shells conj sh)
                      (swap! lit conj l)
                      (when hot
                        (set! (.. sh -userData -hot) true)
                        (hot! sh glow true))
                      {:pick mesh :shell sh :glow glow})))
        ;; the camera's right, along the table: types to pick line up across
        ;; the view rather than into it
        right (let [e (.-elements (.-matrixWorld camera))
                    x (aget e 0) z (aget e 2) l (max 1e-6 (js/Math.hypot x z))]
                [(/ x l) (/ z l)])
        option-slot (fn [targets t]
                      (let [same (filterv #(and (= :option (:kind %)) (= (:space %) (:space t))) targets)
                            i (.indexOf same t) n (count same)
                            [x z] (get spaces (:space t))
                            off (* 0.95 mm (- i (/ (dec n) 2)))]
                        [(+ x (* off (first right))) (* 1.7 mm) (+ z (* off (second right)))]))
        current (level view)]
    ;; what has been chosen so far stays lit
    ;; a type chosen for a space shows there, full size and see-through, as
    ;; the element it will be once the choice is made: the elements of an
    ;; introduction so far, a growth waiting to be paid for
    (let [placed (merge (:placed (last (filter :placed stack)))
                        (into {} (for [{:keys [kind space type]} stack
                                       :when (and (= kind :option) type)]
                                   [space type])))]
      (doseq [[space type] placed
              :let [[x z] (get spaces space)]
              :when x]
        (ghost! view target-layer assets type me-color [x 1 z]
                (get-in assets [:meta :piece_scale] 0.9) :chosen 0.5))
      (doseq [{:keys [kind space index]} stack]
        (case kind
          :piece (some-> (piece-at view space) (outline :chosen true))
          :food (some-> (piece-at view space index) (outline :chosen true))
          :space (when-not (contains? placed space)
                   (let [{:keys [^js plasma]} (plasma! target-layer (get spaces space) mm nil)]
                     (set! (.. plasma -uGain -value) plasma-gain-hover)))
          nil)))
    (doseq [{:keys [kind space index type tone] :as t} current
            :when (contains? spaces space)]
      (mark! t
             (case kind
               :piece (outline (piece-at view space) tone)
               :food (outline (piece-at view space index) tone)
               :space (plasma! target-layer (get spaces space) mm (when (= tone :act) me-color))
               :option (option! target-layer assets type me-color (option-slot current t) 0.55 tone mm)
               :ghost (if type
                        (let [[x z] (get spaces space)]
                          (ghost! view target-layer assets type me-color [x 1 z]
                                  (get-in assets [:meta :piece_scale] 0.9) tone 0.45))
                        (plasma! target-layer (get spaces space) mm (get tone-colors :pending)))
               nil)))
    (let [glows (vec (distinct (into @shells (keep :shell (map second @visuals)))))]
      (swap! view assoc :pickables @pickables :visuals @visuals :lit @lit :shells glows)
      (light-glows! view glows))
    (plasma-loop! view)
    (request-render! view)))

(defn- set-hover!
  "Light what is under the pointer, and everything that goes with it."
  [view target]
  (when (not= target (:hover @view))
    (swap! view assoc :hover target)
    (let [group (:group target)]
      (doseq [[t {:keys [looks base shell ^js glow ^js plasma swell]}] (:visuals @view)
              :let [on? (and target (or (= t target) (and group (= group (:group t)))))]]
        (doseq [[^js m b] (map vector looks base)]
          (set! (.-opacity m) (if on? (min 1 (+ b 0.4)) b)))
        (when-let [[^js o size] swell]
          (let [k (* size (if on? 1.12 1))] (.set (.-scale o) k k 1)))
        (when plasma (set! (.. plasma -uGain -value) (if on? plasma-gain-hover plasma-gain)))
        (when (or shell glow) (hot! view shell glow on?))))
    (let [{:keys [label el]} @view]
      (set! (.. el -style -cursor) (if target "pointer" ""))
      (set! (.-textContent label) (or (:label target) ""))
      (set! (.. label -style -display) (if target "block" "none")))
    (request-render! view)))

(defn- choose!
  "Choosing a target: make its choice, start paying, or go a level in."
  [view t]
  (cond
    (:pay-grower t)
    (let [{:keys [cost spent variants]} (:pay @view)
          spent' (update spent (:pay-grower t) (fnil inc 0))
          have (get-in @view [:last-state :elements (:pay-grower t) :food] 0)]
      (when (<= (get spent' (:pay-grower t)) have)
        (if (>= (reduce + 0 (vals spent')) cost)
          (when-let [variant (first (filter #(= (:spent %) spent') variants))]
            (swap! view assoc :pay nil :stack [])
            ((:send! @view) (:state variant)))
          (swap! view assoc-in [:pay :spent] spent'))))

    (:send t) (do (swap! view assoc :stack [] :pay nil)
                  ((:send! @view) (:send t)))

    (:pay t) (swap! view assoc :pay (assoc (:pay t) :spent {}))

    (seq (:next t)) (swap! view update :stack conj t))
  (set-hover! view nil)
  (render-targets! view))

(defn- back-out! [view]
  (let [{:keys [pay stack]} @view]
    (cond
      pay (swap! view assoc :pay nil)
      (seq stack) (swap! view update :stack pop)))
  (render-targets! view))

(defn- pick
  "The target under a pointer event, if any."
  [view ^js event]
  (let [{:keys [renderer camera pickables ^js raycaster]} @view
        rect (.getBoundingClientRect (.-domElement renderer))
        ndc (THREE/Vector2. (- (* 2 (/ (- (.-clientX event) (.-left rect)) (.-width rect))) 1)
                            (+ (* -2 (/ (- (.-clientY event) (.-top rect)) (.-height rect))) 1))]
    (.setFromCamera raycaster ndc camera)
    (let [by-object (into {} (map (fn [[o t]] [o t]) pickables))
          hits (.intersectObjects raycaster (clj->js (map first pickables)) false)]
      (when (pos? (.-length hits))
        (get by-object (.-object (aget hits 0)))))))

(defn- listen! [view]
  (let [canvas (.-domElement (:renderer @view))
        down (atom nil)]
    (.addEventListener canvas "pointerdown"
                       (fn [^js e] (reset! down [(.-clientX e) (.-clientY e)])))
    (.addEventListener canvas "pointerup"
                       (fn [^js e]
                         ;; a click, not the end of a drag to turn the board
                         (when-let [[x y] @down]
                           (reset! down nil)
                           (when (< (js/Math.hypot (- (.-clientX e) x) (- (.-clientY e) y)) 6)
                             (if-let [t (pick view e)]
                               (choose! view t)
                               (back-out! view))))))
    (.addEventListener canvas "pointermove"
                       (fn [^js e]
                         (let [{:keys [label el]} @view
                               rect (.getBoundingClientRect el)]
                           (set! (.. label -style -left) (str (+ 14 (- (.-clientX e) (.-left rect))) "px"))
                           (set! (.. label -style -top) (str (+ 14 (- (.-clientY e) (.-top rect))) "px")))
                         (when-not @down (set-hover! view (pick view e)))))
    ;; leaving the board straight off a piece lets it go
    (.addEventListener canvas "pointerleave" (fn [_] (set-hover! view nil)))))

(defn- set-board! [view assets geometry]
  (let [{:keys [scene board-group]} @view]
    (when board-group (.remove scene board-group))
    (let [rings (:rings geometry)
          g (if (:printed geometry)
              (printed-board geometry rings #(request-render! view))
              (coloured-board geometry rings))]
      (.add scene g)
      (swap! view assoc :board-group g :board-key (select-keys geometry [:printed :ring-count :mm])))))

(defn- frame-camera! [view {:keys [mm ring-count]}]
  (let [{:keys [camera controls]} @view
        r (* mm (+ ring-count 0.5))]
    ;; far enough back that the whole board sits in the view, tilted as if
    ;; seated at the table
    (.set (.-position camera) 0 (* 2.1 r) (* 1.9 r))
    (.set (.-target controls) 0 0 0)
    (set! (.-maxDistance controls) (* 5 r))
    (.update controls)))

;; ── Board coordinates ─────────────────────────────────────────────────────
;;
;; The page's highlights -- every halo, popup and ghost the 2D board draws --
;; are placed in the 2D board's own coordinates. Both boards come from the
;; same layout (board-locations), so a board point maps onto the table by one
;; similarity transform, fitted from two spaces: the centre and the space
;; farthest from it.

(defn- board-map [locations {:keys [spaces rings]}]
  (let [shared (filter #(contains? spaces %) (keys locations))
        centre (some #(when (= (first %) (first rings)) %) shared)
        far (apply max-key (fn [s] (let [[x z] (get spaces s)] (js/Math.hypot x z))) shared)]
    (when (and centre far (not= centre far))
      (let [[bx0 by0] (get locations centre)
            [bx1 by1] (get locations far)
            [wx0 wz0] (get spaces centre)
            [wx1 wz1] (get spaces far)
            bdx (- bx1 bx0) bdy (- by1 by0)
            wdx (- wx1 wx0) wdz (- wz1 wz0)
            scale (/ (js/Math.hypot wdx wdz) (js/Math.hypot bdx bdy))
            turn (- (js/Math.atan2 wdz wdx) (js/Math.atan2 bdy bdx))]
        {:origin [bx0 by0] :world [wx0 wz0] :scale scale
         :cos (js/Math.cos turn) :sin (js/Math.sin turn)}))))

(defn- board->world [{:keys [origin world scale cos sin]} [bx by]]
  (let [dx (- bx (first origin)) dy (- by (second origin))]
    [(+ (first world) (* scale (- (* dx cos) (* dy sin))))
     (+ (second world) (* scale (+ (* dx sin) (* dy cos))))]))

(defn project
  "Where a 2D board point is on screen: [x y k] in the view's pixels, k being
   how many pixels one board unit is there -- so a highlight drawn in board
   units can be scaled to its place in the picture. Nil before the board is
   known, or behind the camera."
  [view [bx by] height]
  (when-let [{:keys [camera el board-map]} @view]
    (when board-map
      (let [[x z] (board->world board-map [bx by])
            [x2 z2] (board->world board-map [(+ bx 1) by])
            w (.-clientWidth el) h (.-clientHeight el)
            screen (fn [x y z]
                     (let [v (.project (THREE/Vector3. x y z) camera)]
                       [(* w (/ (+ 1 (.-x v)) 2)) (* h (/ (- 1 (.-y v)) 2)) (.-z v)]))
            [sx sy sz] (screen x height z)
            [tx ty] (screen x2 height z2)]
        (when (< sz 1)
          [sx sy (js/Math.hypot (- tx sx) (- ty sy))])))))

;; ── The API play.cljs calls, through the lazy module loader ───────────────

(defn mount!
  "Build a view in `el`. Returns the view, which show! and unmount! take.
   `on-frame` is called after each frame is drawn."
  [el & [{:keys [on-frame]}]]
  (let [view (atom {})
        renderer (THREE/WebGLRenderer. #js {:antialias true})
        scene (THREE/Scene.)
        camera (THREE/PerspectiveCamera. 38 1 5 20000)
        controls (OrbitControls. camera (.-domElement renderer))
        pieces-group (THREE/Group.)
        target-layer (THREE/Group.)
        label (js/document.createElement "div")
        sun (THREE/DirectionalLight. "#ffffff" 2.2)]
    (set! (.. label -style -cssText)
          (str "position:absolute;display:none;pointer-events:none;padding:4px 10px;"
               "background:rgba(10,14,28,0.9);color:#fff;border-radius:6px;"
               "font:13px monospace;letter-spacing:1px;white-space:nowrap;z-index:3"))
    (.setPixelRatio renderer (min 2 (or js/window.devicePixelRatio 1)))
    (set! (.-outputColorSpace renderer) THREE/SRGBColorSpace)
    (set! (.. renderer -shadowMap -enabled) true)
    (set! (.. renderer -shadowMap -type) THREE/PCFSoftShadowMap)
    (set! (.-background scene) (THREE/Color. "#222222"))
    (.add scene (THREE/HemisphereLight. "#dfe6ff" "#3a3228" 1.1))
    (.set (.-position sun) 300 700 250)
    (set! (.-castShadow sun) true)
    (.setScalar (.. sun -shadow -mapSize) 2048)
    (let [c (.. sun -shadow -camera)]
      (set! (.-left c) -400) (set! (.-right c) 400) (set! (.-top c) 400) (set! (.-bottom c) -400)
      (set! (.-far c) 2000))
    (.add scene sun)
    (.add scene pieces-group)
    (.add scene target-layer)
    (let [lights (vec (repeatedly glow-lights #(THREE/PointLight. "#ffffff" 0 130 1)))
          spots (vec (repeatedly glow-lights #(THREE/SpotLight. "#ffffff" 0 0 0.2 0.55 0)))]
      (doseq [l lights] (.add scene l))
      (doseq [^js l spots] (.add scene l) (.add scene (.-target l)))
      (swap! view assoc :lights lights :spots spots))
    (set! (.-maxPolarAngle controls) (* 0.47 js/Math.PI))
    (set! (.-minDistance controls) 60)
    (set! (.-enableDamping controls) false)
    (.addEventListener controls "change" #(request-render! view))
    (.appendChild el (.-domElement renderer))
    (.appendChild el label)
    (let [resize (fn []
                   (let [w (max 1 (.-clientWidth el)) h (max 1 (.-clientHeight el))]
                     (.setSize renderer w h)
                     (set! (.-aspect camera) (/ w h))
                     (.updateProjectionMatrix camera)
                     (request-render! view)))
          observer (js/ResizeObserver. resize)]
      (.observe observer el)
      (reset! view {:el el :renderer renderer :scene scene :camera camera :controls controls
                    :pieces-group pieces-group :observer observer :shown {} :materials {}
                    :target-layer target-layer :label label :raycaster (THREE/Raycaster.)
                    :stack [] :on-frame on-frame :lights (:lights @view) :spots (:spots @view)})
      (listen! view)
      ;; development builds only: where each target is on screen, so a test
      ;; can click the canvas exactly where a person would
      (when ^boolean goog.DEBUG
        (set! (.-__organism3d js/window)
              #js {:foods
                   (fn []
                     (clj->js
                      (for [^js g (array-seq (.-children (:pieces-group @view)))
                            ^js m (array-seq (.-children g))
                            :when (= "food" (.. m -userData -role))]
                        (js/Math.round (* 100 (.. m -scale -x))))))
                   :shells
                   (fn []
                     (clj->js
                      (for [^js sh (:shells @view)
                            :let [p (THREE/Vector3.) sc (THREE/Vector3.)]]
                        (do (.getWorldPosition sh p) (.getWorldScale sh sc)
                            {:type (.-type sh) :parent (some-> sh .-parent .-userData .-role)
                             :pos [(.-x p) (.-y p) (.-z p)] :scale [(.-x sc) (.-y sc)]
                             :opacity (.. sh -material -opacity) :visible (.-visible sh)}))))
                   :targets
                   (fn []
                     (let [{:keys [pickables camera el]} @view]
                       (clj->js
                        (for [[^js o t] pickables
                              :let [p (THREE/Vector3.)
                                    _ (.getWorldPosition o p)
                                    v (.project p camera)
                                    r (.getBoundingClientRect el)]]
                          {:kind (name (:kind t)) :label (:label t)
                           :x (+ (.-left r) (* (.-clientWidth el) (/ (+ 1 (.-x v)) 2)))
                           :y (+ (.-top r) (* (.-clientHeight el) (/ (- 1 (.-y v)) 2)))}))))}))
      (resize))
    view))

(defn show!
  "Show this position: `game` (rules and :state), the invocation it was made
   from, each player's colour, and the 2D board (for its coordinates). A new
   position on the same board is animated from the last one shown."
  [view {:keys [game invocation player-colors board targets turn send!]}]
  (-> (load-assets!)
      (.then
       (fn [loaded]
         (when (:renderer @view)
           (let [geometry (layout game invocation (:meta loaded))
                 board-key (select-keys geometry [:printed :ring-count :mm])
                 new-board? (not= board-key (:board-key @view))
                 before (when-not new-board? (:last-state @view))
                 state (:state game)]
             (swap! view assoc :board-map (board-map (:locations board) geometry))
             (when new-board?
               (set-board! view loaded geometry)
               (frame-camera! view geometry))
             (let [colors (physical-colors player-colors (vals (get-in loaded [:meta :piece_colors])))]
               (when (not= before state)
                 (sync-pieces! view loaded geometry state colors
                               (when before (transitions/diff before state))
                               before))
               (swap! view assoc :last-state state :spaces (:spaces geometry) :mm (:mm geometry)
                      :assets loaded :meta (:meta loaded) :send! send!
                      :me-color (get colors (get-in state [:player-turn :player]) "#cccccc"))
               ;; a new position or phase is a new set of targets; anything
               ;; else re-rendering the page keeps what has been chosen so far
               (let [key [state turn]]
                 (if (not= key (:targets-key @view))
                   (do (swap! view assoc :targets targets :targets-key key :stack [] :pay nil)
                       (render-targets! view))
                   (swap! view assoc :targets targets))))
             (request-render! view)))))))

(defn reset-view! [view]
  (when-let [key (:board-key @view)]
    (frame-camera! view key)
    (request-render! view)))

(defn unmount! [view]
  (when-let [{:keys [renderer controls observer el]} @view]
    (.disconnect observer)
    (.dispose controls)
    (.dispose renderer)
    (when-let [canvas (.-domElement renderer)]
      (when (.-parentNode canvas) (.removeChild el canvas)))
    (reset! view {})))

(def api
  "What the lazy loader hands back."
  {:mount! mount! :show! show! :reset-view! reset-view! :unmount! unmount!
   :project project})
