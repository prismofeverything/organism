(ns organism.dom)

(defn document-head
  []
  (or
   (.-head js/document)
   (first
    (.getElementsByTagName
     js/document
     "head"))))

(defn change-favicon
  [favicon-path]
  (let [favicon-id "dynamic-favicon"
        link (.createElement js/document "link")
        old-link (.getElementById js/document favicon-id)]
    (set! (.-id link) favicon-id)
    (set! (.-rel link) "shortcut icon")
    (set! (.-href link) favicon-path)
    (when old-link
      (.removeChild (.-head js/document) old-link))
    (.appendChild (.-head js/document) link)))

(defn redirect!
  [path]
  (set!
   (-> js/window .-location .-pathname)
   path))
