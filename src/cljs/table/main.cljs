(ns table.main
  "The playtest table: an open 3D table to put any game's components on --
   organism's pieces and boards, the UNIVERSE and DISTINCTIONS decks, playing
   cards, cubes, meeples, dice, boards to draw on -- and play with them.
   Served at /table/ (resources/public/table/index.html)."
  (:require [table.actions :as act]
            [table.input :as input]
            [table.net :as net]
            [table.library :as lib]
            [table.state :as st]
            [table.store :as store]
            [table.ui :as ui]
            [table.world :as world]
            [reagent.dom :as rdom]))

(defn- starter-table!
  "What a first visit sees: a board to draw on, a deck, some bits."
  []
  (let [by lib/by-id]
    (-> (act/spawn! (by "board-hex-grid") 0 0)
        (.then (fn [[id]] (st/update-obj! id assoc :locked true)))
        (.then #(act/spawn! (by "deck-52") 420 -120))
        (.then #(act/spawn! (by "d6") 380 80))
        (.then #(act/spawn! (by "d6-red") 410 95))
        (.then (fn [_]
                 (doseq [[i c] (map-indexed vector ["#c94c4c" "#3f8fbf" "#5aa456" "#e3c34a"])]
                   (act/spawn! (assoc (by "meeple") :color c) (+ -90 (* 60 i)) 270)
                   (doseq [j (range 3)]
                     (act/spawn! (assoc (by "cube-small") :color c) (+ -100 (* 60 i) (* 13 j)) 320)))))
        (.then (fn [_]
                 (swap! st/ui assoc :selected #{})
                 (reset! st/history {:past [] :future [] :tag nil :at 0})
                 (world/sync!))))))

(defonce ^:private save-timer (atom nil))

(defn- autosave! []
  (js/clearTimeout @save-timer)
  (reset! save-timer (js/setTimeout #(store/put! "autosave" {:objects (st/objects)}) 800)))

(defn- table-drop! [^js e]
  (.preventDefault e)
  (let [x (.-clientX e) y (.-clientY e)
        p (world/table-point x y 0)
        [px pz] (if p [(.-x p) (.-z p)] (world/view-centre))
        dt (.-dataTransfer e)
        item-id (.getData dt "text/x-table-item")]
    (cond
      (seq item-id) (when-let [item (lib/by-id item-id)] (ui/spawn-item! item px pz))
      (pos? (.. dt -files -length)) (ui/import-files! (array-seq (.-files dt)) px pz))))

(defn mount-components []
  (rdom/render [ui/root] (js/document.getElementById "table-ui")))

(defn init! []
  (let [host (js/document.getElementById "table-view")]
    (world/init! host)
    (world/resize!)
    (let [canvas (.-domElement (:renderer @world/world))]
      (.addEventListener canvas "pointerdown"
                         (fn [e] (.setPointerCapture canvas (.-pointerId e)) (input/on-pointer-down e)))
      (.addEventListener canvas "pointermove" input/on-pointer-move)
      (.addEventListener canvas "pointermove"
                         #(net/cursor! (world/table-point (.-clientX %) (.-clientY %) 0)))
      (.addEventListener canvas "pointerleave" #(net/cursor! nil))
      ;; coming back onto the table hands the keys back to it
      (.addEventListener canvas "pointerenter" #(some-> js/document.activeElement .blur))
      (.addEventListener canvas "pointerup" input/on-pointer-up)
      (.addEventListener canvas "dblclick" input/on-double-click)
      (.addEventListener canvas "contextmenu" #(.preventDefault %)))
    (.addEventListener js/window "keydown" input/on-key-down)
    (.addEventListener js/window "keyup" input/on-key-up)
    (.addEventListener js/window "blur" #(do (reset! input/held-keys #{}) (swap! st/ui assoc :peek nil)))
    (.addEventListener js/window "resize" world/resize!)
    (.addEventListener js/document "dragover" #(.preventDefault %))
    (.addEventListener js/document "drop" table-drop!)
    (add-watch st/doc ::sync (fn [_ _ old new]
                               (world/sync!)
                               (when-not (identical? (:objects old) (:objects new))
                                 (if (net/online?) (net/changed!) (autosave!)))))
    (add-watch net/peers ::peers (fn [_ _ _ ps] (world/show-peers! ps)))
    ;; for poking at from the console (and the browser tests)
    (set! (.-tableObjects js/window) (fn [] (clj->js (vals (st/objects)))))
    (set! (.-tableScreenPos js/window) (fn [id] (clj->js (world/screen-pos id))))
    (set! (.-tableUi js/window) (fn [] (clj->js (dissoc @st/ui :clipboard))))
    (world/start-loop! input/tick)
    (mount-components)
    (lib/load!)
    (if (net/online?)
      ;; a shared table: what's on it comes from the server
      (net/connect!)
      (-> (store/get! "autosave")
          (.then (fn [saved]
                   (if (seq (:objects saved))
                     (reset! st/doc saved)
                     (starter-table!))))
          (.catch (fn [e] (js/console.warn "no saved table" e) (starter-table!)))))))
