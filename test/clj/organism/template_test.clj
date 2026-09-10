(ns organism.template-test
  "Game names and player names are free text, and they reach the browser as
   JavaScript string literals inside a template. Selmer escapes for HTML by
   default, which silently rewrote every apostrophe — a game called
   \"Woogachaka's Game\" arrived as \"Woogachaka&#39;s Game\" and pointed the
   page at a game that does not exist."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest testing is]]
   [jsonista.core :as json]
   [organism.layout :as layout]
   [selmer.parser :as parser]))

(def awkward-names
  ["Woogachaka's Game"
   "let's see where this goes!"
   "2p Testing!"
   "Poppolopin Jr."
   "a & b"
   "\"quoted\""
   "ゲーム"])

(defn- script-line
  [html variable]
  (->> (str/split-lines html)
       (filter #(str/includes? % (str "var " variable " =")))
       first
       str/trim))

(deftest js-literal-round-trips
  (testing "the filter produces a literal that reads back as the same string"
    (doseq [name awkward-names]
      (is (= name (-> (layout/js-literal name)
                      (str/replace "\\u003c" "<")
                      (str/replace "\\u003e" ">")
                      (str/replace "\\u0026" "&")
                      json/read-value))
          name))))

(deftest a-game-name-reaches-the-page-intact
  (doseq [game-key awkward-names]
    (testing (pr-str game-key)
      (let [html (parser/render-file "organism/play.html"
                                     {:play game-key :player game-key})]
        (is (= (str "var playKey = " (layout/js-literal game-key) ";")
               (script-line html "playKey"))
            (script-line html "playKey"))
        (is (not (str/includes? (script-line html "playKey") "&#39;"))
            "html-escaped into a JavaScript string literal")))))

(defn- render-player-page
  [name]
  (parser/render-file "organism/player.html"
                      {:player name :session-player name
                       :player-games "{}" :preferences "{}"}))

(deftest names-cannot-close-the-script-tag
  (testing "a name carrying markup stays inside the string it belongs to"
    ;; The page legitimately has more than one </script> — an inline block and
    ;; the bundle tag — so the invariant is that a hostile name adds none.
    (let [ordinary (count (re-seq #"(?i)</script>" (render-player-page "alice")))
          nasty (render-player-page "</script><script>alert(1)</script>")]
      (is (= ordinary (count (re-seq #"(?i)</script>" nasty))))
      (is (not (str/includes? nasty "<script>alert"))))))

(deftest every-game-carries-its-keys-the-same-way
  (doseq [template ["organism/play.html" "organism/player.html"
                    "organism/create.html" "journey/play.html"
                    "future/play.html" "oroboros/play.html"]]
    (testing template
      (let [html (parser/render-file template
                                     {:play "it's here" :play-key "it's here"
                                      :player "o'brien" :session-player "o'brien"
                                      :player-games "{}" :preferences "{}"
                                      :open-invocation "{}"})]
        (is (not (str/includes? html "&#39;"))
            (str "html-escaped a name somewhere in " template))))))
