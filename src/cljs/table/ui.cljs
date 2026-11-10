(ns table.ui
  "The panels over the table: the library on the left, the selected thing's
   properties on the right, tools along the top, and the menus and dialogs."
  (:require [clojure.string :as str]
            [cljs.reader :as reader]
            [table.actions :as act]
            [table.input :as input]
            [table.library :as lib]
            [table.mesh :as mesh]
            [table.net :as net]
            [table.state :as st]
            [table.store :as store]
            [table.textures :as tx]
            [table.world :as world]
            [reagent.core :as r]))

;; ── spawning and importing ──────────────────────────────────────────────────

(defn spawn-item!
  "Put a library item down at x z, or near the middle of the view."
  [item & [x z]]
  (let [[cx cz] (world/view-centre)]
    (if (= "objects" (:kind item))
      (act/add-objects! (:objects item) (or x cx) (or z cz))
      (if x (act/spawn! item x z) (act/spawn! item)))))

(defn- file-ext [^js f] (str/lower-case (last (str/split (.-name f) #"\."))))
(defn- base-name [^js f] (str/replace (.-name f) #"\.[^.]+$" ""))
(def image-exts #{"png" "jpg" "jpeg" "webp" "gif" "svg" "bmp"})
(def model-exts #{"glb" "gltf" "stl" "obj"})

(defn- import-model! [^js f x z]
  (let [kind (let [e (file-ext f)] (if (= e "gltf") "glb" e))
        z-up (= kind "stl")]
    (-> (store/read-file f false)
        (.then net/upload!)
        (.then (fn [url]
                 (-> (mesh/load-model url z-up kind)
                     (.then (fn [{:keys [size]}]
                              (let [m (max (.-x size) (.-y size) (.-z size))
                                    ;; metres, or a model far too big for a table: guess millimetres
                                    s (cond (< m 2) 1000 (> m 1200) (/ 100 m) :else 1)]
                                (spawn-item! {:kind "model" :model url :zUp z-up :model-kind kind
                                              :scale s :label (base-name f) :color nil}
                                             x z)))))))
        (.catch (fn [e] (st/status! (str "Couldn't read " (.-name f) ": " e)))))))

(defn- import-scene! [^js f x z]
  (-> (store/read-file f true)
      (.then (fn [text]
               (let [data (reader/read-string text)
                     objs (vals (or (:objects data) (into {} (map (juxt :id identity) data))))]
                 (if (js/confirm (str (.-name f) ": replace the table with it?\n(Cancel adds its "
                                      (count objs) " things to this table instead.)"))
                   (do (st/checkpoint!) (swap! st/doc assoc :objects (:objects data)))
                   (let [[cx cz] (world/view-centre)]
                     (act/add-objects! objs (or x cx) (or z cz)))))))
      (.catch (fn [e] (st/status! (str "Couldn't read " (.-name f) ": " e))))))

(defn import-files!
  "Files dropped on the table or chosen: models and saved tables go straight
   on; images wait for the import dialog to say what they are."
  [files x z]
  (let [files (vec files)
        images (filterv #(image-exts (file-ext %)) files)]
    (doseq [f files]
      (cond (model-exts (file-ext f)) (import-model! f x z)
            (#{"edn" "table"} (file-ext f)) (import-scene! f x z)))
    (when (seq images)
      (swap! st/ui assoc :import {:files images :x x :z z :back? false}))))

(defn- finish-image-import! [as]
  (let [{:keys [files x z back?]} (:import @st/ui)
        max-px (if (= as :boards) 2048 1024)]
    (swap! st/ui assoc :import nil)
    (-> (js/Promise.all (clj->js (map #(-> (store/read-file % false)
                                                (.then (fn [u] (store/shrink-image u max-px)))
                                                (.then (fn [[u w h]] (.then (net/upload! u) (fn [url] [url w h])))))
                                      files)))
        (.then
         (fn [results]
           (let [imgs (map (fn [[url w h] f] {:url url :w w :h h :name (base-name f)})
                           (js->clj results) files)
                 aspect (fn [{:keys [w h]}] (/ h w))]
             (case as
               :deck
               (let [[back faces] (if (and back? (> (count imgs) 1)) [(first imgs) (rest imgs)] [nil imgs])
                     w 63.5]
                 (spawn-item! {:kind "cards" :label "Imported deck"
                               :size [w (* w (aspect (first faces)))]
                               :cards (mapv (fn [i] {:name (:name i) :face (:url i)
                                                     :back (or (:url back) "std:back")})
                                            faces)}
                              x z))
               (:tiles :boards)
               (let [width (if (= as :boards) 500 80)]
                 (st/checkpoint!)
                 (doseq [[n i] (map-indexed vector imgs)]
                   (let [item (if (= as :boards)
                                {:kind "board" :shape "rect" :label (:name i) :image (:url i) :bg "#ffffff"
                                 :size [width 3 (* width (aspect i))]}
                                {:kind "tile" :shape "rect" :label (:name i) :face (:url i)
                                 :size [width 2 (* width (aspect i))]})]
                     (if x
                       (spawn-item! item (+ x (* n (+ width 10))) z)
                       (spawn-item! item)))))))))
        (.catch (fn [e] (st/status! (str "Import failed: " e)))))))

;; ── saving ──────────────────────────────────────────────────────────────────

(defonce saves (r/atom []))

(defn refresh-saves! []
  (.then (store/keys!)
         (fn [ks] (reset! saves (sort (keep #(when (str/starts-with? % "table:") (subs % 6)) ks))))))

(defn save-as! [name]
  (when (seq name)
    (.then (store/put! (str "table:" name) {:objects (st/objects)})
           (fn [] (refresh-saves!) (st/status! (str "Saved \"" name "\""))))))

(defn load-save! [name]
  (.then (store/get! (str "table:" name))
         (fn [data]
           (st/checkpoint!)
           (swap! st/doc assoc :objects (or (:objects data) {}))
           (swap! st/ui assoc :selected #{} :saves? false)
           (st/status! (str "Loaded \"" name "\"")))))

(defn export! []
  (store/download! (str "table-" (.slice (.toISOString (js/Date.)) 0 10) ".edn")
                   (pr-str {:objects (st/objects)}) "application/edn"))

;; ── pieces ──────────────────────────────────────────────────────────────────

(defn- btn [attrs label]
  [:button.tb (merge {:type "button"} attrs) label])

(defn- file-picker [label accept on-files]
  [:label.tb label
   [:input {:type "file" :multiple true :accept accept :style {:display "none"}
            :on-change (fn [e] (on-files (array-seq (.. e -target -files))) (set! (.. e -target -value) ""))}]])

(def tools [[:pen "Pen" "✎"] [:line "Line" "╱"] [:rect "Rectangle" "▭"] [:circle "Circle" "◯"]
            [:hex "Hexagon" "⬡"] [:text "Text" "T"] [:erase "Eraser" "⌫"]])

(defn- presence []
  (let [st @net/status
        here (set @net/who)
        me (net/me)]
    [:div.seg.presence
     [:span.conn {:class (name st)
                  :title (case st :open "Connected" :connecting "Connecting…"
                               :missing "This table doesn't exist" "Disconnected — reconnecting")}]
     [:b.title (when (exists? js/tableTitle) js/tableTitle)]
     (for [p (sort here)]
       ^{:key p} [:span.who {:style {:color (world/player-colour p)}} p (when (= p me) " (you)")])
     [btn {:title "Copy this table's link to send to the others"
           :on-click #(-> (.writeText js/navigator.clipboard (.-href js/location))
                          (.then (fn [] (st/status! "Link copied"))))} "🔗 Invite"]
     [:a.tb {:href "/table" :title "Your tables"} "⌂"]]))

(defn toolbar []
  (let [{:keys [mode tool brush grid?]} @st/ui
        paint? (= mode :paint)]
    [:div.toolbar
     (when (net/online?) [presence])
     [:div.seg
      [btn {:class (when-not paint? "on") :on-click #(swap! st/ui assoc :mode :select)
            :title "Move things (Esc)"} "✋ Move"]
      [btn {:class (when paint? "on") :on-click #(swap! st/ui assoc :mode :paint)
            :title "Draw on boards, paper and tiles (P)"} "🖌 Draw"]]
     (when paint?
       [:div.seg
        (for [[t title icon] tools]
          ^{:key t} [btn {:class (when (= tool t) "on") :title title
                          :on-click #(swap! st/ui assoc :tool t)} icon])
        [:input {:type "color" :value (:color brush) :title "Ink"
                 :on-change #(swap! st/ui assoc-in [:brush :color] (.. % -target -value))}]
        [:label.slider {:title "Line width (mm)"}
         [:input {:type "range" :min -1.2 :max 3.4 :step 0.05
                  :value (js/Math.log (:size brush))
                  :on-change #(swap! st/ui assoc-in [:brush :size]
                                     (/ (js/Math.round (* 10 (js/Math.exp (js/parseFloat (.. % -target -value))))) 10))}]
         [:span (str (:size brush) " mm")]]
        [:label.slider {:title "Opacity"}
         [:input {:type "range" :min 0.1 :max 1 :step 0.05 :value (:alpha brush)
                  :on-change #(swap! st/ui assoc-in [:brush :alpha] (js/parseFloat (.. % -target -value)))}]]
        (when (#{:rect :circle :hex} tool)
          [btn {:class (when (:fill brush) "on") :on-click #(swap! st/ui update-in [:brush :fill] not)}
           "Fill"])])
     [:div.seg
      [btn {:on-click st/undo! :title "Undo (Ctrl+Z)"} "↶"]
      [btn {:on-click st/redo! :title "Redo (Ctrl+Shift+Z)"} "↷"]]
     [:div.seg
      [btn {:class (when grid? "on") :title "Show the table grid (5 cm)"
            :on-click (fn [] (swap! st/ui update :grid? not)
                        (set! (.-visible (:grid @world/world)) (:grid? @st/ui)) (world/redraw!))} "#"]
      [btn {:on-click #(do (refresh-saves!) (swap! st/ui update :saves? not))} "Tables…"]
      [file-picker "Import…" ".png,.jpg,.jpeg,.webp,.gif,.svg,.glb,.gltf,.stl,.obj,.edn"
       #(import-files! % nil nil)]
      [btn {:on-click #(swap! st/ui update :help? not) :title "Controls (H)"} "?"]]]))

(defn- item-swatch [item]
  (let [img (case (:kind item)
              "cards" (some-> (first (:cards item)) :face tx/preview-url)
              "tile" (:face item)
              "board" (:image item)
              nil)]
    (cond
      img [:img.thumb {:src img :loading "lazy"}]
      (:color item) [:span.dot {:style {:background (:color item)}}]
      (= "board" (:kind item)) [:span.dot.square {:style {:background (:bg item)}}]
      (= "set" (:kind item)) [:img.thumb {:src (:face (first (:items item))) :loading "lazy"}]
      :else [:span.dot {:style {:background "#888"}}])))

(defn library-panel []
  (let [search (r/atom "")
        closed (r/atom #{})]
    (fn []
      (let [q (str/lower-case @search)
            shut @closed]
        (if-not (:library-open? @st/ui)
          [:button.tb.lib-open {:on-click #(swap! st/ui assoc :library-open? true)} "☰ Library"]
          [:div.panel.library
           [:div.panel-head
            [:b "Library"]
            [:button.x {:on-click #(swap! st/ui assoc :library-open? false)} "×"]]
           [:input.search {:placeholder "Search…" :value @search
                           :on-change #(reset! search (.. % -target -value))}]
           [:div.hint "Click to add, or drag onto the table. Drop images, STL/GLB/OBJ files or a saved table anywhere."]
           (for [{:keys [group game items mine?]} (doall (lib/all-groups))
                 :let [items (filter #(str/includes? (str/lower-case (str (:label %) " " group)) q) items)]
                 :when (seq items)
                 :let [k (str game "/" group)
                       open? (or (seq q) (not (shut k)))]]
             ^{:key k}
             [:div.group
              [:div.group-head {:on-click #(swap! closed (fn [c] (if (c k) (disj c k) (conj c k))))}
               (if open? "▾ " "▸ ") (when game [:span.game game]) group
               [:span.count (count items)]]
              (when open?
                (for [item items]
                  ^{:key (:id item)}
                  [:div.item {:draggable true
                              :on-drag-start #(.setData (.-dataTransfer %) "text/x-table-item" (:id item))
                              :on-click #(spawn-item! item)
                              :title (:label item)}
                   [item-swatch item]
                   [:span.label (:label item)]
                   (when mine?
                     [:button.x {:title "Remove from My components"
                                 :on-click (fn [e] (.stopPropagation e) (lib/forget-component! (:id item)))}
                      "×"])]))])])))))

(defn- num-input [value on-set & [attrs]]
  [:input.num (merge {:type "number" :step "any" :value (if (number? value) (/ (js/Math.round (* 10 value)) 10) value)
                      :on-change #(let [v (js/parseFloat (.. % -target -value))]
                                    (when (and (not (js/isNaN v)) (pos? v)) (on-set v)))}
                     attrs)])

(defn- colour-row [label value on-set]
  [:div.row [:span.k label]
   [:input {:type "color" :value (or value "#888888") :on-change #(on-set (.. % -target -value))}]
   [:div.swatches
    (for [c lib/palette]
      ^{:key c} [:span.sw {:style {:background c} :on-click #(on-set c)}])]])

(defn props-panel []
  (let [objs (doall (st/selected-objs))
        ids (mapv :id objs)
        o (first objs)
        one? (= 1 (count objs))
        kinds (set (map :kind objs))]
    (when (seq objs)
      [:div.panel.props
       [:div.panel-head
        (if one?
          [:input.title {:value (or (:label o) "") :placeholder (str/capitalize (or (:shape o) (:kind o)))
                         :on-change #(act/set-prop! ids :label (.. % -target -value))}]
          [:b (count objs) " selected"])
        [:button.x {:on-click #(do (swap! st/ui assoc :selected #{}) (world/sync!))} "×"]]

       (when (and one? (= "cards" (:kind o)))
         [:div.box
          [:div.row [:span.k "Cards"] [:b (count (:cards o))]
           (when (> (count (:cards o)) 1) [:span.muted " — Alt-drag takes the top card"])]
          (when (> (count (:cards o)) 1)
            [:div.btns
             [btn {:on-click #(act/roll! ids)} "Shuffle"]
             [btn {:on-click #(act/deal! (:id o) 1)} "Deal 1"]
             [btn {:on-click #(act/deal! (:id o) 5)} "Deal 5"]
             [btn {:on-click #(act/spread! (:id o))} "Spread"]])])
       (when (and (not one?) (contains? kinds "cards"))
         [:div.btns [btn {:on-click #(act/gather! ids)} "Gather into a deck (G)"]])

       ;; colour
       (cond
         (every? #{"shape" "model"} kinds)
         [colour-row "Colour" (:color o) #(act/set-prop! ids :color %)]
         (every? #{"board"} kinds)
         [:<>
          [colour-row "Surface" (:bg o) #(act/set-prop! ids :bg %)]
          [colour-row "Edge" (:edge o) #(act/set-prop! ids :edge %)]]
         (every? #{"tile"} kinds)
         [colour-row "Edge" (:edge o) #(act/set-prop! ids :edge %)])

       ;; size
       (when (every? #{"shape" "tile" "board"} kinds)
         [:div.row [:span.k "Size mm"]
          (for [[i n] [[0 "W"] [1 "H"] [2 "D"]]]
            ^{:key i} [:label.dim n [num-input (get-in o [:size i]) #(act/set-size! ids i %)]])])
       (when (every? #{"model"} kinds)
         [:div.row [:span.k "Scale %"]
          (for [[i n] [[0 "X"] [1 "Y"] [2 "Z"]]]
            ^{:key i} [:label.dim n [num-input (* 100 (get-in o [:scale i] 1)) #(act/set-size! ids i (/ % 100))]])
          [btn {:title "Scale evenly to the X value"
                :on-click #(let [s (get-in o [:scale 0] 1)] (doseq [i [1 2]] (act/set-size! ids i s)))} "="]])

       ;; drawing
       (when (and one? (mesh/can-paint? o))
         [:div.box
          (when (= "board" (:kind o))
            (let [g (:grid o)]
              [:div.row [:span.k "Grid"]
               [:select {:value (or (:type g) "none")
                         :on-change #(act/set-prop! ids :grid (assoc (or g {:cell 40 :color "#9aa3ab"})
                                                                     :type (.. % -target -value)))}
                (for [t ["none" "square" "hex" "dots"]] ^{:key t} [:option {:value t} t])]
               (when (and g (not= "none" (:type g)))
                 [:<>
                  [num-input (:cell g) #(act/set-prop! ids :grid (assoc g :cell %)) {:title "Cell size (mm)"}]
                  [:input {:type "color" :value (or (:color g) "#9aa3ab")
                           :on-change #(act/set-prop! ids :grid (assoc g :color (.. % -target -value)))}]])]))
          [:div.btns
           [btn {:on-click #(swap! st/ui assoc :mode :paint)} "🖌 Draw on it"]
           (when (seq (:strokes o))
             [btn {:on-click #(act/clear-drawing! ids)} (str "Clear drawing (" (count (:strokes o)) ")")])]])

       [:div.btns
        [btn {:on-click #(act/flip! ids) :title "F"} "Flip"]
        [btn {:on-click #(act/rotate! ids 15) :title "Q"} "⟲ 15°"]
        [btn {:on-click #(act/rotate! ids -15) :title "E"} "⟳ 15°"]
        [btn {:on-click #(act/tilt! ids) :title "T: tip over a quarter turn"} "Tip"]
        [btn {:on-click #(act/upright! ids) :title "U"} "Upright"]
        (when (some #(= "d6" (:shape %)) objs) [btn {:on-click #(act/roll! ids) :title "R"} "Roll"])]
       [:div.btns
        [btn {:on-click #(act/toggle-lock! ids) :class (when (every? :locked objs) "on") :title "L"}
         (if (every? :locked objs) "🔒 Locked" "Lock")]
        [btn {:on-click #(act/duplicate! ids) :title "Ctrl+D"} "Duplicate"]
        [btn {:on-click #(when-let [n (not-empty (js/prompt "Name for this component:" (or (:label o) "")))]
                           (lib/save-component! n objs)
                           (st/status! (str "Added \"" n "\" to My components")))
              :title "Keep it in the library for any table"} "Save to library"]
        [btn {:class "danger" :on-click #(act/delete! ids) :title "Delete"} "Delete"]]])))

(defn context-menu []
  (when-let [{:keys [x y id]} (:menu @st/ui)]
    (let [ts (input/targets)
          objs (keep st/obj ts)
          o (st/obj id)
          item (fn [label f] [:div.mi {:on-click #(do (swap! st/ui assoc :menu nil) (f))} label])
          [cx cz] (let [p (world/table-point x y 0)] (if p [(.-x p) (.-z p)] (world/view-centre)))]
      [:div.menu {:style {:left x :top y}}
       (if (empty? objs)
         [:<>
          (when (:clipboard @st/ui) [item "Paste here" #(act/paste! cx cz)])
          [item "Add a cube here" #(spawn-item! (lib/by-id "cube") cx cz)]
          [item "Add a blank board here" #(spawn-item! (lib/by-id "board-blank") cx cz)]
          [item "Look here" #(world/focus! (world/table-point x y 0))]]
         [:<>
          (when (and o (act/cards? o) (> (count (:cards o)) 1))
            [:<>
             [item "Shuffle  R" #(act/roll! [id])]
             [item "Deal 1" #(act/deal! id 1)]
             [item "Deal 5" #(act/deal! id 5)]
             [item "Spread out" #(act/spread! id)]])
          (when (> (count (filter act/cards? objs)) 1) [item "Gather into a deck  G" #(act/gather! ts)])
          [item "Flip  F" #(act/flip! ts)]
          [item "Rotate  Q / E" #(act/rotate! ts 90)]
          (when (some #(= "d6" (:shape %)) objs) [item "Roll  R" #(act/roll! ts)])
          (when (some mesh/can-paint? objs) [item "Draw on it  P" #(swap! st/ui assoc :mode :paint)])
          [item "Duplicate  Ctrl+D" #(act/duplicate! ts)]
          [item "Copy  Ctrl+C" #(act/copy! ts)]
          [item (if (every? :locked objs) "Unlock  L" "Lock  L") #(act/toggle-lock! ts)]
          [item "Delete  Del" #(act/delete! ts)]])])))

(defn import-dialog []
  (when-let [{:keys [files back?]} (:import @st/ui)]
    [:div.modal-bg {:on-click #(swap! st/ui assoc :import nil)}
     [:div.modal {:on-click #(.stopPropagation %)}
      [:h3 (count files) (if (= 1 (count files)) " image" " images") " — put them on the table as:"]
      [:div.choices
       [btn {:on-click #(finish-image-import! :deck)} "A deck of cards"]
       [btn {:on-click #(finish-image-import! :tiles)} "Tiles"]
       [btn {:on-click #(finish-image-import! :boards)} "Boards to draw on"]]
      (when (> (count files) 1)
        [:label.check [:input {:type "checkbox" :checked back?
                               :on-change #(swap! st/ui update-in [:import :back?] not)}]
         " The first image (" (.-name (first files)) ") is the card back"])
      [:div.muted "Cards are poker size, 63.5 mm wide; resize anything afterwards."]]]))

(defn saves-dialog []
  (let [name (r/atom "")]
    (fn []
      (when (:saves? @st/ui)
        [:div.modal-bg {:on-click #(swap! st/ui assoc :saves? false)}
         [:div.modal {:on-click #(.stopPropagation %)}
          [:h3 "Tables"]
          [:div.muted (if (net/online?)
                        "This table is kept on the server and everyone at it sees each change. Keep a copy of it here under a name, or load one onto it."
                        "The table saves itself as you go. Keep a copy under a name to come back to it.")]
          [:div.row
           [:input {:placeholder "Name" :value @name :on-change #(reset! name (.. % -target -value))
                    :on-key-down #(when (= "Enter" (.-key %)) (save-as! @name))}]
           [btn {:on-click #(save-as! @name)} "Save"]]
          [:div.saves
           (for [s @saves]
             ^{:key s}
             [:div.save-row
              [:span s]
              [btn {:on-click #(load-save! s)} "Load"]
              [btn {:on-click #(save-as! s) :title "Overwrite with this table"} "Save over"]
              [btn {:class "danger" :on-click #(when (js/confirm (str "Delete \"" s "\"?"))
                                                 (.then (store/del! (str "table:" s)) refresh-saves!))} "×"]])]
          [:div.btns
           [btn {:on-click export!} "Export to a file"]
           [file-picker "Open a file…" ".edn" #(import-files! % nil nil)]
           [btn {:class "danger"
                 :on-click #(when (js/confirm "Clear everything off the table? (Undo brings it back.)")
                              (st/checkpoint!) (swap! st/doc assoc :objects {})
                              (swap! st/ui assoc :saves? false))} "Clear the table"]]]]))))

(def help-rows
  [["Left-drag" "pick up and move (shift-click adds to the selection; drag empty space to box-select)"]
   ["Right-drag / middle-drag" "orbit / pan the view   — Shift+right-drag also pans"]
   ["Wheel" "zoom toward the cursor   — WASD or arrows slide the view; double-click looks at a spot"]
   ["Right-click" "menu for the thing under the cursor"]
   ["F / Q / E" "flip / rotate 15° (Shift: 90°)"]
   ["T / U" "tip over a quarter turn / stand upright"]
   ["R" "roll dice, shuffle a deck"]
   ["Alt-drag a deck" "take its top card   — 1-9 deal that many; G gathers selected cards"]
   ["Alt (hold)" "magnify the card or tile under the cursor"]
   ["Drop a card on a deck" "it joins the deck"]
   ["L" "lock in place (locked things don't drag; boards are best locked)"]
   ["Ctrl+C / V / D / X" "copy / paste at the cursor / duplicate / cut"]
   ["Ctrl+Z / Ctrl+Shift+Z" "undo / redo — everything, drawing included"]
   ["P" "draw mode: draw on boards, paper, tiles — any printed board too"]
   ["Del" "delete"]])

(defn help []
  (when (:help? @st/ui)
    [:div.modal-bg {:on-click #(swap! st/ui assoc :help? false)}
     [:div.modal.help
      [:h3 "Playtest table"]
      [:table [:tbody (for [[k v] help-rows] ^{:key k} [:tr [:td.k k] [:td v]])]]
      [:div.muted "Drop images (cards, tiles, boards), 3D models (STL, GLB, OBJ) or a saved .edn table onto the window to bring them in."]]]))

(defn overlays []
  (let [{:keys [box hover peek status]} @st/ui
        [mx my] @input/mouse
        ho (st/obj hover)]
    [:<>
     (when box
       (let [{:keys [x0 y0 x1 y1]} box]
         [:div.boxsel {:style {:left (min x0 x1) :top (min y0 y1)
                               :width (js/Math.abs (- x1 x0)) :height (js/Math.abs (- y1 y0))}}]))
     (when (and ho (nil? (:menu @st/ui)) (not= :paint (:mode @st/ui)))
       [:div.tip {:style {:left (+ mx 14) :top (+ my 14)}}
        (or (not-empty (:label ho)) (str/capitalize (or (:shape ho) (:kind ho))))
        (when (act/cards? ho) (str " · " (count (:cards ho)) (if (= 1 (count (:cards ho))) " card" " cards")))
        (when (:locked ho) " · locked")])
     (when-let [po (st/obj peek)]
       (let [key (case (:kind po)
                   "cards" (let [[top bot] (mesh/visible-faces (:cards po))] (if (act/flipped? po) bot top))
                   "tile" (if (act/flipped? po) (:back po) (:face po))
                   "board" (:image po)
                   nil)]
         (when-let [url (tx/preview-url key)]
           [:div.peek [:img {:src url :class (when (= "round" (:shape po)) "round")}]])))
     (when status [:div.status status])
     (when (= :paint (:mode @st/ui))
       [:div.mode-banner "Drawing — left-drag on a board, paper or tile · Esc or P to stop"])]))

(defn root []
  [:<>
   [toolbar]
   [library-panel]
   [props-panel]
   [context-menu]
   [overlays]
   [import-dialog]
   [saves-dialog]
   [help]])
