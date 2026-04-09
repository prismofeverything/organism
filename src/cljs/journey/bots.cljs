(ns journey.bots
  "Per-player journey bot list and flowchart editor.

   Two pages share this single cljs build:
   - bot list (js/isBotList): list saved bots, link to create/edit
   - bot editor (js/isBotEditor): canvas with diagrams, palette, properties panel"
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [ajax.core :refer [POST]]
   [journey.bot-flow :as bf]
   [organism.ajax :as ajax]))

;; ── State ────────────────────────────────────────────────────────────────────

(defonce bot
  (r/atom bf/default-bot))

(defonce bot-name
  (r/atom ""))

(defonce bot-owner
  (r/atom nil))

(defonce selected
  (r/atom nil))  ;; {:diagram diagram-name :tile tile-id} or nil

(defonce palette-selection
  (r/atom nil))  ;; {:kind :type :label}

(defonce link-drag
  (r/atom nil))  ;; {:from {:diagram :tile :port} :mx :my} during a port-drag

(defonce tile-drag
  (r/atom nil))  ;; {:diagram :tile :ox :oy} while dragging a tile

(defonce save-status
  (r/atom nil))

(defonce next-id-counter
  (r/atom 1000))

(defn fresh-id [prefix]
  (let [n (swap! next-id-counter inc)]
    (keyword (str prefix "-" n))))

;; ── Region geometry ──────────────────────────────────────────────────────────

(def tile-w 132)
(def tile-h 60)
(def region-pad 30)
(def region-title-h 22)
(def diagram-gap 24)

(defn tile-bounds
  "Compute the [x y w h] of a region from its tiles."
  [diagram]
  (let [tiles (vals (:tiles diagram))
        [base-x base-y] (or (:origin diagram) [0 0])]
    (if (seq tiles)
      (let [xs (map (fn [t] (first (:pos t))) tiles)
            ys (map (fn [t] (second (:pos t))) tiles)
            min-x (apply min xs)
            min-y (apply min ys)
            max-x (apply max xs)
            max-y (apply max ys)
            w (+ (- max-x min-x) tile-w (* 2 region-pad))
            h (+ (- max-y min-y) tile-h (* 2 region-pad) region-title-h)]
        [(- (+ base-x min-x) region-pad)
         (- (+ base-y min-y) region-pad region-title-h)
         w h])
      [base-x base-y 240 (+ region-title-h (* 2 region-pad))])))

(defn- index-of
  "CLJS-friendly indexOf for any seqable. Returns -1 if not found."
  [coll x]
  (loop [i 0 c (seq coll)]
    (cond
      (nil? c)        -1
      (= (first c) x) i
      :else           (recur (inc i) (next c)))))

(defn rects-collide?
  [[ax ay aw ah] [bx by bw bh]]
  (not (or (<= (+ ax aw) bx)
           (<= (+ bx bw) ax)
           (<= (+ ay ah) by)
           (<= (+ by bh) ay))))

(defn relayout
  "Push diagrams down to avoid overlaps. Iterates until stable or max passes."
  [b]
  (let [order (vec (keys (:diagrams b)))
        rects (atom (into {} (map (fn [k]
                                    [k (tile-bounds (get-in b [:diagrams k]))])
                                  order)))]
    (loop [i 0]
      (if (> i 30)
        b
        (let [conflicts? (atom false)]
          (doseq [a order
                  bk order
                  :when (not= a bk)]
            (let [[ax ay aw ah] (get @rects a)
                  [bx by bw bh] (get @rects bk)]
              (when (rects-collide? [ax ay aw ah] [bx by bw bh])
                (reset! conflicts? true)
                ;; push the lower one further down
                (if (> ay by)
                  (let [push (- (+ by bh diagram-gap) ay)]
                    (swap! rects assoc-in [a 1] (+ ay push)))
                  (let [push (- (+ ay ah diagram-gap) by)]
                    (swap! rects assoc-in [bk 1] (+ by push)))))))
          (if @conflicts?
            (recur (inc i))
            ;; apply the new rect ys back to diagram :origin
            (reduce (fn [acc k]
                      (let [[nx ny _ _] (get @rects k)
                            old (get-in acc [:diagrams k])
                            [ox oy _ _] (tile-bounds old)
                            dy (- ny oy)
                            dx (- nx ox)]
                        (-> acc
                            (update-in [:diagrams k]
                                       (fn [d]
                                         (let [[bx by] (or (:origin d) [0 0])]
                                           (assoc d :origin [(+ bx dx) (+ by dy)])))))))
                    b
                    order)))))))

;; ── Tile/diagram operations ─────────────────────────────────────────────────

(defn diagram-key
  "Convert a user-supplied diagram name to a stable keyword (UPPER snake)."
  [s]
  (-> s str/trim str/lower-case (str/replace #"\s+" "-") keyword))

(defn add-diagram!
  [name]
  (let [k     (diagram-key name)
        next? (contains? (:diagrams @bot) k)]
    (when (and (not next?) (seq (str name)))
      (let [start-id :start
            color    (rand-nth ["#1e3a5a" "#3a1e5a" "#5a1e3a" "#1e5a3a" "#5a3a1e" "#3a5a1e"])
            n-existing (count (:diagrams @bot))
            base-y   (* n-existing 220)]
        (swap! bot
               (fn [b]
                 (relayout
                  (assoc-in b [:diagrams k]
                            {:name name
                             :color color
                             :collapsed? false
                             :origin [40 (+ 40 base-y)]
                             :start-tile start-id
                             :tiles {start-id {:id start-id :kind :start :type :start
                                               :pos [40 80] :params {}}}
                             :links []}))))
        k))))

(defn delete-diagram!
  [diagram-key]
  (when (not= diagram-key (or (:start-diagram @bot) :main))
    (swap! bot
           (fn [b]
             (-> b
                 (update :diagrams dissoc diagram-key)
                 relayout)))))

(defn add-tile-to-diagram!
  "Add a palette tile (:kind :type :label) to a diagram at a position."
  [diagram-name palette-tile [x y]]
  (let [id (fresh-id (name (:type palette-tile)))
        spec (case (:kind palette-tile)
               :condition (get bf/conditions (:type palette-tile))
               :logic     (get bf/logic-tiles (:type palette-tile))
               :effect    (get bf/effects (:type palette-tile))
               :jump      (get bf/effects :jump)
               nil)
        defaults (into {} (map (juxt :key :default) (:params spec)))]
    (swap! bot
           (fn [b]
             (relayout
              (assoc-in b [:diagrams diagram-name :tiles id]
                        {:id id
                         :kind (:kind palette-tile)
                         :type (:type palette-tile)
                         :pos [x y]
                         :params defaults}))))
    (reset! selected {:diagram diagram-name :tile id})
    (reset! palette-selection nil)))

(defn delete-tile!
  [diagram-name tile-id]
  (when-not (= tile-id :start)
    (swap! bot
           (fn [b]
             (-> b
                 (update-in [:diagrams diagram-name :tiles] dissoc tile-id)
                 (update-in [:diagrams diagram-name :links]
                            (fn [ls]
                              (vec (remove
                                    (fn [l]
                                      (or (= tile-id (get-in l [:from :tile]))
                                          (= tile-id (get-in l [:to :tile]))))
                                    ls))))
                 relayout)))
    (reset! selected nil)))

(defn move-tile!
  [diagram-name tile-id [x y]]
  (swap! bot
         (fn [b]
           (-> b
               (assoc-in [:diagrams diagram-name :tiles tile-id :pos] [x y])
               relayout))))

(defn add-link!
  [diagram-name from-id from-port to-id to-port]
  (when (not= from-id to-id)
    (swap! bot
           (fn [b]
             (update-in b [:diagrams diagram-name :links]
                        (fn [ls]
                          (let [stripped (vec (remove
                                               (fn [l]
                                                 (and (= (get-in l [:from :tile]) from-id)
                                                      (= (get-in l [:from :port]) from-port)))
                                               ls))]
                            (conj stripped {:from {:tile from-id :port from-port}
                                            :to   {:tile to-id   :port to-port}}))))))))

(defn delete-link!
  [diagram-name from-id from-port]
  (swap! bot
         (fn [b]
           (update-in b [:diagrams diagram-name :links]
                      (fn [ls]
                        (vec (remove
                              (fn [l]
                                (and (= (get-in l [:from :tile]) from-id)
                                     (= (get-in l [:from :port]) from-port)))
                              ls)))))))

(defn update-tile-param!
  [diagram-name tile-id k v]
  (swap! bot assoc-in [:diagrams diagram-name :tiles tile-id :params k] v))

(defn toggle-collapsed!
  [diagram-name]
  (swap! bot update-in [:diagrams diagram-name :collapsed?] not))

;; ── Geometry helpers for SVG drawing ────────────────────────────────────────

(defn tile-abs-pos
  "Get the absolute SVG coordinate for a tile (origin + relative pos)."
  [diagram tile]
  (let [[ox oy] (or (:origin diagram) [0 0])
        [px py] (:pos tile)]
    [(+ ox px) (+ oy py)]))

(defn output-ports-of [tile]
  (let [spec (bf/tile-spec tile)
        out  (or (:outputs spec)
                 (case (:kind tile)
                   :condition [:true :false]
                   :logic     (case (:type tile)
                                :branch [:a :b]
                                [:out])
                   :start     [:out]
                   []))]
    out))

(defn output-port-pos [diagram tile port]
  (let [[x y] (tile-abs-pos diagram tile)
        outs  (output-ports-of tile)
        idx   (index-of outs port)
        n     (count outs)
        py    (+ y (/ tile-h 2)
                 (- (* (/ tile-h (max 1 (inc n)))
                       (- (inc idx) (/ (inc n) 2.0)))))]
    [(+ x tile-w) py]))

(defn input-port-pos [diagram tile]
  (let [[x y] (tile-abs-pos diagram tile)]
    [x (+ y (/ tile-h 2))]))

;; ── Save / load ─────────────────────────────────────────────────────────────

(defn save-bot! []
  (let [n (str/upper-case (str/trim @bot-name))]
    (cond
      (str/blank? n)
      (reset! save-status {:error "Bot name is required"})
      (re-find #"[^A-Z0-9_-]" n)
      (reset! save-status {:error "Bot name must be uppercase letters/digits"})
      :else
      (POST (str "/journey/bots/" n)
        {:params {:name n :description "" :definition @bot}
         :format :transit
         :response-format :transit
         :headers {"x-csrf-token" js/csrfToken
                   "X-CSRF-Token" js/csrfToken}
         :handler (fn [_]
                    (reset! save-status {:ok (str "saved " n)})
                    (reset! bot-name n))
         :error-handler (fn [err]
                          (reset! save-status {:error (str "Save failed: " (pr-str err))}))}))))

;; ── List page (saved bots) ──────────────────────────────────────────────────

(defonce my-bots-list  (r/atom []))
(defonce all-bots-list (r/atom []))

(def page-bg "#04040E")
(def panel-bg "#0A0E1C")
(def border-c "#1A2A40")
(def text-c "#AABBCC")
(def accent  "#7AAAE0")
(def muted   "#556677")

(def list-card-style
  {:background panel-bg :border (str "1px solid " border-c)
   :border-radius "6px" :padding "12px 16px"
   :font-family "monospace" :color text-c
   :display "flex" :align-items "center" :gap "12px"})

(defn bot-list-page []
  (let [my-set (set (map :name @my-bots-list))]
    [:div {:style {:padding "48px" :background page-bg
                   :min-height "100vh" :color text-c
                   :font-family "monospace"}}
     [:div {:style {:display "flex" :align-items "center" :margin-bottom "24px"
                    :gap "16px"}}
      [:h2 {:style {:color accent :margin 0 :flex 1}} "JOURNEY — Bots"]
      [:a {:href "/journey" :style {:color muted :text-decoration "none"
                                    :font-size "13px"}} "← home"]]

     [:div {:style {:margin-bottom "16px"}}
      [:a {:href "/journey/bots/new"
           :style {:display "inline-block"
                   :padding "10px 20px" :background "#10182A"
                   :border (str "1px solid " "#2A4A80") :border-radius "4px"
                   :color accent :text-decoration "none"
                   :font-family "monospace" :font-size "14px"}}
       "+ NEW BOT"]]

     [:h3 {:style {:color accent :font-size "13px" :margin "32px 0 12px"
                   :letter-spacing "2px"}} "YOUR BOTS"]
     (if (seq @my-bots-list)
       [:div {:style {:display "flex" :flex-direction "column" :gap "8px"}}
        (for [b @my-bots-list]
          ^{:key (:name b)}
          [:div {:style list-card-style}
           [:a {:href (str "/journey/bots/" (:name b))
                :style {:flex 1 :color accent :text-decoration "none"
                        :font-weight "bold"}}
            (:name b)]
           (when (:description b)
             [:span {:style {:color muted :font-size "12px"}}
              (:description b)])
           [:span {:style {:color "#445566" :font-size "11px"}}
            (str "diagrams: "
                 (count (:diagrams (:definition b))))]])]
       [:div {:style {:color muted :font-style "italic"}}
        "no bots yet — create one to design custom behavior"])

     [:h3 {:style {:color accent :font-size "13px" :margin "32px 0 12px"
                   :letter-spacing "2px"}} "OTHER BOTS"]
     (if-let [others (seq (remove #(my-set (:name %)) @all-bots-list))]
       [:div {:style {:display "flex" :flex-direction "column" :gap "8px"}}
        (for [b others]
          ^{:key (:name b)}
          [:div {:style list-card-style}
           [:a {:href (str "/journey/bots/" (:name b))
                :style {:flex 1 :color "#88AACC" :text-decoration "none"}}
            (:name b)]
           [:span {:style {:color muted :font-size "11px"}}
            (str "by " (or (:owner b) "?"))]])]
       [:div {:style {:color muted :font-style "italic"}}
        "no shared bots from other players"])]))

;; ── Editor: palette ─────────────────────────────────────────────────────────

(defn category-color [cat]
  (case cat
    :conditions "#5a3a1e"
    :logic      "#1e5a3a"
    :effects    "#3a1e5a"
    :flow       "#5a1e3a"
    "#333"))

(defn palette []
  [:div {:style {:width "200px" :background panel-bg
                 :border-right (str "1px solid " border-c)
                 :overflow-y "auto"
                 :padding "16px 12px"
                 :font-family "monospace"}}
   [:h3 {:style {:color accent :font-size "12px" :margin "0 0 8px"
                 :letter-spacing "2px"}} "TILES"]
   [:div {:style {:font-size "10px" :color muted :margin-bottom "12px"
                  :line-height "1.4"}}
    "click a tile then click on a diagram to add it"]
   (for [{:keys [category tiles]} (bf/all-categories)]
     ^{:key category}
     [:div {:style {:margin-bottom "16px"}}
      [:div {:style {:color "#778899" :font-size "10px" :letter-spacing "1.5px"
                     :text-transform "uppercase" :margin-bottom "6px"}}
       (name category)]
      (for [t tiles]
        (let [selected? (and (= (:kind @palette-selection) (:kind t))
                             (= (:type @palette-selection) (:type t)))]
          ^{:key (str (:kind t) "-" (:type t))}
          [:div {:on-click #(reset! palette-selection
                                    (if selected? nil
                                        {:kind (:kind t) :type (:type t) :label (:label t)}))
                 :style {:padding "6px 10px" :margin-bottom "4px"
                         :background (if selected? "#2a4a80"
                                         (category-color category))
                         :border-radius "4px"
                         :cursor "pointer"
                         :color text-c :font-size "12px"
                         :transition "background 0.1s"}
                 :title (or (:description t) "")}
           (:label t)]))])])

;; ── Editor: SVG canvas ──────────────────────────────────────────────────────

(defn port-color [port]
  (case port
    :true  "#88CC66"
    :false "#CC6666"
    :a     "#88BBCC"
    :b     "#BB88CC"
    :out   "#AABBCC"
    "#888"))

(defn render-tile [diagram-name diagram tile]
  (let [[x y] (tile-abs-pos diagram tile)
        spec (bf/tile-spec tile)
        label (or (:label spec) (name (or (:type tile) :tile)))
        sel? (= @selected {:diagram diagram-name :tile (:id tile)})
        kind-color (case (:kind tile)
                     :condition "#5a3a1e"
                     :logic     "#1e5a3a"
                     :effect    "#3a1e5a"
                     :jump      "#5a1e3a"
                     :start     "#444444"
                     "#222")
        outs (output-ports-of tile)]
    [:g {:key (str diagram-name "-" (name (:id tile)))}
     ;; tile body
     [:rect
      {:x x :y y :width tile-w :height tile-h
       :rx 6 :ry 6
       :fill kind-color
       :stroke (if sel? "#FFD030" "#444")
       :stroke-width (if sel? 2 1)
       :on-mouse-down
       (fn [e]
         (.stopPropagation e)
         (let [svg  (.. e -currentTarget -ownerSVGElement)
               pt   (.createSVGPoint svg)
               _    (set! (.-x pt) (.-clientX e))
               _    (set! (.-y pt) (.-clientY e))
               ctm  (.getScreenCTM svg)
               loc  (.matrixTransform pt (.inverse ctm))]
           (reset! selected {:diagram diagram-name :tile (:id tile)})
           (reset! tile-drag {:diagram diagram-name :tile (:id tile)
                              :ox (- (.-x loc) x) :oy (- (.-y loc) y)})))}]
     ;; label
     [:text
      {:x (+ x (/ tile-w 2)) :y (+ y 22)
       :text-anchor "middle"
       :font-family "monospace" :font-size "12"
       :fill "#FFF" :pointer-events "none"}
      label]
     ;; param summary
     (when (seq (:params tile))
       [:text
        {:x (+ x (/ tile-w 2)) :y (+ y 40)
         :text-anchor "middle"
         :font-family "monospace" :font-size "9"
         :fill "#bbb" :pointer-events "none"}
        (str/join " "
                  (map (fn [[k v]]
                         (str (name k) "=" (cond
                                             (keyword? v) (name v)
                                             :else        (str v))))
                       (:params tile)))])
     ;; input port
     (when (or (= :start (:kind tile))
               (#{:condition :logic :effect :jump} (:kind tile)))
       (when (not= :start (:kind tile))
         [:circle
          {:cx x :cy (+ y (/ tile-h 2)) :r 5
           :fill "#222" :stroke "#888" :stroke-width 1
           :on-mouse-up
           (fn [e]
             (.stopPropagation e)
             (when-let [{:keys [from]} @link-drag]
               (when (= (:diagram from) diagram-name)
                 (add-link! diagram-name (:tile from) (:port from)
                            (:id tile) :in))
               (reset! link-drag nil)))}]))
     ;; output ports
     (for [port outs]
       (let [[px py] (output-port-pos diagram tile port)]
         ^{:key (str (:id tile) "-" (name port))}
         [:g
          [:circle
           {:cx px :cy py :r 5
            :fill (port-color port) :stroke "#000" :stroke-width 1
            :on-mouse-down
            (fn [e]
              (.stopPropagation e)
              (let [svg  (.. e -currentTarget -ownerSVGElement)
                    pt   (.createSVGPoint svg)
                    _    (set! (.-x pt) (.-clientX e))
                    _    (set! (.-y pt) (.-clientY e))
                    ctm  (.getScreenCTM svg)
                    loc  (.matrixTransform pt (.inverse ctm))]
                (reset! link-drag
                        {:from {:diagram diagram-name :tile (:id tile) :port port}
                         :mx (.-x loc) :my (.-y loc)})))
            :on-click
            (fn [e]
              (when (.-shiftKey e)
                (.stopPropagation e)
                (delete-link! diagram-name (:id tile) port)))}]
          [:text
           {:x (+ px 8) :y (+ py 4)
            :font-family "monospace" :font-size "9"
            :fill (port-color port) :pointer-events "none"}
           (name port)]]))]))

(defn render-link [diagram-name diagram link]
  (let [from-tile (get-in diagram [:tiles (get-in link [:from :tile])])
        to-tile   (get-in diagram [:tiles (get-in link [:to :tile])])]
    (when (and from-tile to-tile)
      (let [[x1 y1] (output-port-pos diagram from-tile (get-in link [:from :port]))
            [x2 y2] (input-port-pos  diagram to-tile)
            mx      (/ (+ x1 x2) 2)
            d       (str "M " x1 " " y1
                         " C " mx " " y1 " " mx " " y2 " " x2 " " y2)
            color   (port-color (get-in link [:from :port]))]
        [:path
         {:key (str diagram-name "-link-"
                    (name (get-in link [:from :tile])) "-"
                    (name (get-in link [:from :port])) "-"
                    (name (get-in link [:to :tile])))
          :d d :stroke color :stroke-width 2 :fill "none"
          :on-click (fn [e]
                      (when (.-shiftKey e)
                        (.stopPropagation e)
                        (delete-link! diagram-name
                                      (get-in link [:from :tile])
                                      (get-in link [:from :port]))))}]))))

(defn render-diagram [diagram-name diagram]
  (let [[rx ry rw rh] (tile-bounds diagram)
        title (or (:name diagram) (name diagram-name))
        collapsed? (:collapsed? diagram)]
    [:g {:key (str "dg-" (name diagram-name))}
     ;; region background
     [:rect
      {:x rx :y ry :width rw :height (if collapsed? region-title-h rh)
       :rx 8 :ry 8
       :fill (or (:color diagram) "#1e3a5a")
       :fill-opacity 0.18
       :stroke (or (:color diagram) "#1e3a5a")
       :stroke-width 1.5
       :on-mouse-down
       (fn [e]
         (when @palette-selection
           (.stopPropagation e)
           (let [svg  (.. e -currentTarget -ownerSVGElement)
                 pt   (.createSVGPoint svg)
                 _    (set! (.-x pt) (.-clientX e))
                 _    (set! (.-y pt) (.-clientY e))
                 ctm  (.getScreenCTM svg)
                 loc  (.matrixTransform pt (.inverse ctm))
                 [ox oy] (or (:origin diagram) [0 0])
                 lx   (- (.-x loc) ox)
                 ly   (- (.-y loc) oy)]
             (add-tile-to-diagram! diagram-name @palette-selection [lx ly]))))}]
     ;; title bar
     [:rect
      {:x rx :y ry :width rw :height region-title-h
       :rx 8 :ry 8
       :fill (or (:color diagram) "#1e3a5a")
       :fill-opacity 0.55
       :on-click (fn [_] (toggle-collapsed! diagram-name))
       :style {:cursor "pointer"}}]
     [:text
      {:x (+ rx 12) :y (+ ry 15)
       :font-family "monospace" :font-size "12"
       :fill "#FFF" :pointer-events "none"}
      (str (if collapsed? "▶ " "▼ ") title)]
     ;; tiles + links
     (when-not collapsed?
       [:g
        (for [link (:links diagram)]
          (render-link diagram-name diagram link))
        (for [[_ tile] (:tiles diagram)]
          (render-tile diagram-name diagram tile))])]))

(defonce canvas-pan-x (r/atom 0))
(defonce canvas-pan-y (r/atom 0))
(defonce canvas-drag  (r/atom nil))

(defn canvas []
  (let [b @bot]
    [:div {:style {:flex 1 :background "#04040E"
                   :overflow "hidden"
                   :position "relative"}}
     [:svg
      {:width "100%" :height "100%"
       :on-mouse-down
       (fn [e]
         (when-not @palette-selection
           (let [svg (.-currentTarget e)
                 pt  (.createSVGPoint svg)
                 _   (set! (.-x pt) (.-clientX e))
                 _   (set! (.-y pt) (.-clientY e))
                 ctm (.getScreenCTM svg)
                 loc (.matrixTransform pt (.inverse ctm))]
             (reset! canvas-drag {:mx (.-x loc) :my (.-y loc)
                                  :px @canvas-pan-x :py @canvas-pan-y}))))
       :on-mouse-move
       (fn [e]
         (let [svg (.-currentTarget e)
               pt  (.createSVGPoint svg)
               _   (set! (.-x pt) (.-clientX e))
               _   (set! (.-y pt) (.-clientY e))
               ctm (.getScreenCTM svg)
               loc (.matrixTransform pt (.inverse ctm))]
           (cond
             @tile-drag
             (let [{:keys [diagram tile ox oy]} @tile-drag
                   d (get-in @bot [:diagrams diagram])
                   [orx ory] (or (:origin d) [0 0])
                   nx (- (.-x loc) ox orx)
                   ny (- (.-y loc) oy ory)]
               (move-tile! diagram tile [nx ny]))

             @link-drag
             (swap! link-drag assoc :mx (.-x loc) :my (.-y loc))

             @canvas-drag
             (let [{:keys [mx my px py]} @canvas-drag]
               (reset! canvas-pan-x (+ px (- (.-x loc) mx)))
               (reset! canvas-pan-y (+ py (- (.-y loc) my)))))))
       :on-mouse-up
       (fn [_]
         (reset! tile-drag nil)
         (reset! link-drag nil)
         (reset! canvas-drag nil))
       :style {:cursor (cond @canvas-drag "grabbing"
                             @palette-selection "crosshair"
                             :else "grab")}}
      [:g {:transform (str "translate(" @canvas-pan-x " " @canvas-pan-y ")")}
       (for [[k d] (:diagrams b)]
         ^{:key (name k)} (render-diagram k d))
       ;; in-progress link drag
       (when-let [{:keys [from mx my]} @link-drag]
         (let [d  (get-in b [:diagrams (:diagram from)])
               t  (get-in d [:tiles (:tile from)])
               [px py] (output-port-pos d t (:port from))]
           [:line {:x1 px :y1 py :x2 mx :y2 my
                   :stroke (port-color (:port from))
                   :stroke-width 2 :stroke-dasharray "4,3"}]))]]
     ;; status overlay
     [:div {:style {:position "absolute" :bottom "12px" :left "12px"
                    :color muted :font-family "monospace" :font-size "11px"}}
      (cond
        @palette-selection
        (str "click in a diagram to place: " (:label @palette-selection))
        @link-drag
        "drop on an input port to connect (shift-click an output port to delete a link)"
        :else
        "drag tiles to move · drag from output port to input port to link · shift-click to delete a link")]]))

;; ── Editor: properties panel ────────────────────────────────────────────────

(def input-style
  {:background "#0A0E1C" :color "#AACCEE"
   :border "1px solid #2A4A80" :border-radius "4px"
   :padding "6px 10px" :font-family "monospace" :font-size "12px"
   :width "100%"})

(defn render-param [diagram-name tile param-spec]
  (let [k (:key param-spec)
        v (get-in tile [:params k] (:default param-spec))
        on-change (fn [new-v]
                    (update-tile-param! diagram-name (:id tile) k new-v))]
    [:div {:key (name k) :style {:margin-bottom "8px"}}
     [:label {:style {:color muted :font-size "10px" :display "block"
                      :margin-bottom "3px"}}
      (name k)]
     (case (:type param-spec)
       :enum
       [:select {:value (str v)
                 :on-change (fn [e]
                              (let [s (.. e -target -value)]
                                (on-change (keyword (subs s 1)))))
                 :style input-style}
        (for [opt (:options param-spec)]
          ^{:key (str opt)} [:option {:value (str opt)} (name opt)])]

       :int
       [:input {:type "number" :value (or v 0)
                :on-change #(on-change (js/parseInt (.. % -target -value)))
                :style input-style}]

       :float
       [:input {:type "number" :step "0.05" :value (or v 0)
                :on-change #(on-change (js/parseFloat (.. % -target -value)))
                :style input-style}]

       :diagram-ref
       [:select {:value (str v)
                 :on-change (fn [e]
                              (let [s (.. e -target -value)]
                                (on-change (when (seq s) (keyword (subs s 1))))))
                 :style input-style}
        [:option {:value ""} "(none)"]
        (for [k2 (keys (:diagrams @bot))]
          ^{:key (name k2)}
          [:option {:value (str k2)} (name k2)])]

       [:input {:type "text" :value (str (or v ""))
                :on-change #(on-change (.. % -target -value))
                :style input-style}])]))

(defn properties-panel []
  [:div {:style {:width "280px" :background panel-bg
                 :border-left (str "1px solid " border-c)
                 :overflow-y "auto"
                 :padding "16px 14px"
                 :font-family "monospace" :color text-c}}
   ;; bot meta
   [:h3 {:style {:color accent :font-size "12px" :margin "0 0 12px"
                 :letter-spacing "2px"}} "BOT"]
   [:div {:style {:margin-bottom "8px"}}
    [:label {:style {:color muted :font-size "10px" :display "block"
                     :margin-bottom "3px"}} "name (ALL CAPS)"]
    [:input {:type "text" :value @bot-name
             :on-change #(reset! bot-name (str/upper-case (.. % -target -value)))
             :placeholder "ASTRA"
             :style input-style}]]
   [:button {:on-click save-bot!
             :style {:width "100%" :background "#10182A"
                     :border "1px solid #2A4A80" :border-radius "4px"
                     :padding "8px" :color accent :cursor "pointer"
                     :font-family "monospace" :font-size "13px"
                     :margin-bottom "6px"}}
    "SAVE BOT"]
   (when-let [s @save-status]
     [:div {:style {:font-size "11px"
                    :color (if (:error s) "#CC6666" "#88CC66")
                    :margin-bottom "8px"}}
      (or (:error s) (:ok s))])
   [:a {:href "/journey/bots"
        :style {:font-size "11px" :color muted :text-decoration "none"
                :display "block" :margin-bottom "16px"}}
    "← all bots"]

   ;; diagrams
   [:h3 {:style {:color accent :font-size "12px" :margin "16px 0 8px"
                 :letter-spacing "2px"}} "DIAGRAMS"]
   (for [[k d] (:diagrams @bot)]
     ^{:key (name k)}
     [:div {:style {:padding "6px 10px" :margin-bottom "4px"
                    :background (or (:color d) "#1e3a5a")
                    :background-color "transparent"
                    :border (str "1px solid " (or (:color d) "#1e3a5a"))
                    :border-radius "4px" :font-size "12px"
                    :display "flex" :align-items "center" :gap "8px"}}
      [:span {:style {:flex 1 :color text-c}} (or (:name d) (name k))]
      (when (and (not= k (or (:start-diagram @bot) :main))
                 (not= k :main))
        [:button {:on-click #(delete-diagram! k)
                  :style {:background "none" :border "none" :cursor "pointer"
                          :color "#886666" :font-size "11px"}}
         "✕"])])
   [:button {:on-click (fn []
                         (let [n (js/prompt "diagram name:")]
                           (when (and n (seq (str/trim n)))
                             (add-diagram! n))))
             :style {:width "100%" :background "transparent"
                     :border (str "1px dashed " muted) :border-radius "4px"
                     :padding "6px" :color muted :cursor "pointer"
                     :font-family "monospace" :font-size "11px"
                     :margin-top "8px"}}
    "+ NEW DIAGRAM"]

   ;; selected tile
   (when-let [{:keys [diagram tile]} @selected]
     (when-let [t (get-in @bot [:diagrams diagram :tiles tile])]
       [:div {:style {:margin-top "20px" :padding-top "16px"
                      :border-top (str "1px solid " border-c)}}
        [:h3 {:style {:color accent :font-size "12px" :margin "0 0 8px"
                      :letter-spacing "2px"}}
         (str "TILE: " (name (or (:type t) :?)))]
        [:div {:style {:color muted :font-size "10px" :margin-bottom "12px"}}
         (or (:description (bf/tile-spec t)) "")]
        (let [spec (bf/tile-spec t)]
          (for [p (:params spec)]
            ^{:key (name (:key p))}
            [render-param diagram t p]))
        (when (not= :start (:kind t))
          [:button {:on-click #(delete-tile! diagram tile)
                    :style {:width "100%" :background "transparent"
                            :border "1px solid #4A2A2A" :border-radius "4px"
                            :padding "6px" :color "#886666" :cursor "pointer"
                            :font-family "monospace" :font-size "11px"
                            :margin-top "12px"}}
           "DELETE TILE"])]))])

(defn bot-editor-page []
  [:div {:style {:display "flex" :width "100vw" :height "100vh"
                 :background page-bg :overflow "hidden"}}
   [palette]
   [canvas]
   [properties-panel]])

;; ── Page container ──────────────────────────────────────────────────────────

(defn page-container []
  (cond
    (and (exists? js/isBotEditor) js/isBotEditor) [bot-editor-page]
    (and (exists? js/isBotList)   js/isBotList)   [bot-list-page]
    :else [:div]))

;; ── Init ────────────────────────────────────────────────────────────────────

(defn mount-components []
  (rdom/render [#'page-container] (.getElementById js/document "journey")))

(defn- safe-read [s]
  (when (and (string? s) (not (str/blank? s)))
    (try (reader/read-string s) (catch :default _ nil))))

(defn init! []
  (ajax/load-interceptors!)
  (when (and (exists? js/preloadedBot) js/preloadedBot)
    (let [b (safe-read js/preloadedBot)]
      (when (and b (:definition b))
        (reset! bot (:definition b))
        (reset! bot-name (or (:name b) ""))
        (reset! bot-owner (:owner b)))
      (when-not b
        (reset! bot bf/default-bot))))
  (when (and (exists? js/preloadedBotName) js/preloadedBotName
             (str/blank? @bot-name))
    (reset! bot-name js/preloadedBotName))
  (when (and (exists? js/myBots) js/myBots)
    (when-let [v (safe-read js/myBots)]
      (reset! my-bots-list v)))
  (when (and (exists? js/allBots) js/allBots)
    (when-let [v (safe-read js/allBots)]
      (reset! all-bots-list v)))
  (mount-components))
