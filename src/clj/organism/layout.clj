(ns organism.layout
  (:require
    [clojure.java.io]
    [clojure.string :as str]
    [jsonista.core :as json]
    [selmer.parser :as parser]
    [selmer.filters :as filters]
    [markdown.core :refer [md-to-html-string]]
    [ring.util.http-response :refer [content-type ok]]
    [ring.util.anti-forgery :refer [anti-forgery-field]]
    [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
    [ring.util.response]))

(parser/set-resource-path!  (clojure.java.io/resource "html"))
(parser/add-tag! :csrf-field (fn [_ _] (anti-forgery-field)))
(filters/add-filter! :markdown (fn [content] [:safe (md-to-html-string content)]))

(defn js-literal
  "A value as a JavaScript string literal, quotes included, safe to drop into a
   <script> block.

   Selmer escapes for HTML by default, which is right for markup and wrong
   inside a string literal: a game called \"Woogachaka's Game\" arrived in the
   browser as \"Woogachaka&#39;s Game\" and pointed at a game that does not
   exist. Game names and player names are free text, so encode them as JSON and
   then hide the three characters that could close the script tag early."
  [value]
  (-> (json/write-value-as-string (str value))
      (str/replace "<" "\\u003c")
      (str/replace ">" "\\u003e")
      (str/replace "&" "\\u0026")))

;; Use as {{play|js}} — WITHOUT surrounding quotes, the literal brings its own.
(filters/add-filter! :js (fn [value] [:safe (js-literal value)]))

(defn render
  "renders the HTML template located relative to resources/html"
  [request template & [params]]
  (content-type
    (ok
      (parser/render-file
        template
        (assoc params
          :page template
          :csrf-token *anti-forgery-token*)))
    "text/html; charset=utf-8"))

(defn error-page
  "error-details should be a map containing the following keys:
   :status - error status
   :title - error title (optional)
   :message - detailed error message (optional)

   returns a response map with the error page as the body
   and the status specified by the status key"
  [error-details]
  {:status  (:status error-details)
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    (parser/render-file "error.html" error-details)})
