(ns table.store
  "Keeping tables and components between visits: IndexedDB in this browser
   (localStorage is too small once an imported image is in the scene), plus
   files to carry a table elsewhere. Everything is stored as EDN."
  (:require [cljs.reader :as reader]))

(def ^:private db-name "organism-table")
(def ^:private store "kv")
(defonce ^:private db (atom nil))

(defn- open []
  (or @db
      (reset! db (js/Promise.
                  (fn [ok fail]
                    (let [req (.open js/indexedDB db-name 1)]
                      (set! (.-onupgradeneeded req) #(.createObjectStore (.. % -target -result) store))
                      (set! (.-onsuccess req) #(ok (.. % -target -result)))
                      (set! (.-onerror req) #(fail (.. % -target -error)))))))))

(defn- request [mode f]
  (.then (open)
         (fn [^js d]
           (js/Promise.
            (fn [ok fail]
              (let [req (f (.objectStore (.transaction d store mode) store))]
                (set! (.-onsuccess req) #(ok (.. % -target -result)))
                (set! (.-onerror req) #(fail (.. % -target -error)))))))))

(defn put! [k v] (request "readwrite" #(.put % (pr-str v) k)))
(defn del! [k] (request "readwrite" #(.delete % k)))
(defn get! [k] (.then (request "readonly" #(.get % k)) #(when % (reader/read-string %))))
(defn keys! [] (.then (request "readonly" #(.getAllKeys %)) js->clj))

;; ── files ───────────────────────────────────────────────────────────────────

(defn download! [filename text mime]
  (let [url (js/URL.createObjectURL (js/Blob. #js [text] #js {:type mime}))
        a (js/document.createElement "a")]
    (set! (.-href a) url)
    (set! (.-download a) filename)
    (.click a)
    (js/setTimeout #(js/URL.revokeObjectURL url) 1000)))

(defn read-file
  "A promise of the file's contents: text, or a data URL."
  [^js file as-text?]
  (js/Promise.
   (fn [ok fail]
     (let [r (js/FileReader.)]
       (set! (.-onload r) #(ok (.-result r)))
       (set! (.-onerror r) #(fail (.-error r)))
       (if as-text? (.readAsText r file) (.readAsDataURL r file))))))

(defn shrink-image
  "A promise of [data-url width height], no more than `max-px` on a side, so a
   phone photo doesn't make every save tens of megabytes."
  [data-url max-px]
  (js/Promise.
   (fn [ok fail]
     (let [img (js/Image.)]
       (set! (.-onerror img) fail)
       (set! (.-onload img)
             (fn []
               (let [w (.-naturalWidth img) h (.-naturalHeight img)
                     s (min 1 (/ max-px (max w h)))]
                 (if (and (= s 1) (< (count data-url) 1500000))
                   (ok [data-url w h])
                   (let [c (doto (js/document.createElement "canvas")
                             (aset "width" (js/Math.round (* w s)))
                             (aset "height" (js/Math.round (* h s))))]
                     (.drawImage (.getContext c "2d") img 0 0 (.-width c) (.-height c))
                     (ok [(.toDataURL c "image/webp" 0.88) w h]))))))
       (set! (.-src img) data-url)))))
