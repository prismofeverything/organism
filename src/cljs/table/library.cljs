(ns table.library
  "What can be put on the table: the basic bits every prototype needs, every
   game's own components (resources/public/table/library.json, from
   table/build_library.py), and components saved here from the table."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [table.store :as store]
            [table.textures :as tx]
            [reagent.core :as r]))

(def palette ["#c94c4c" "#e08a3c" "#e3c34a" "#5aa456" "#3f8fbf" "#7656b0" "#d6d1c4" "#2b2b2b"
              ;; organism's players
              "#BA6A75" "#DCAD3F" "#4BA166" "#6DA2B2" "#9C6E90" "#4F6573"])

(defn- shape [id label shape size color]
  {:id id :label label :kind "shape" :shape shape :size size :color color})

(def basics
  [{:group "Basics"
    :items [(shape "cube" "Cube" "cube" [16 16 16] "#c94c4c")
            (shape "cube-small" "Small cube" "cube" [10 10 10] "#3f8fbf")
            (shape "cylinder" "Cylinder" "cylinder" [16 18 16] "#5aa456")
            (shape "disk" "Disk" "disk" [22 4 22] "#e3c34a")
            (shape "meeple" "Meeple" "meeple" [16 18 8] "#7656b0")
            (shape "pawn" "Pawn" "pawn" [14 26 14] "#e08a3c")
            (shape "sphere" "Ball" "sphere" [14 14 14] "#d6d1c4")
            (shape "cone" "Cone" "cone" [14 18 14] "#3f8fbf")
            (shape "hex" "Hex prism" "hex" [24 8 24] "#5aa456")
            (shape "tri" "Triangle prism" "tri" [24 8 24] "#c94c4c")
            (shape "bar" "Bar" "cube" [40 6 10] "#d6d1c4")
            (shape "d6" "Die (d6)" "d6" [16 16 16] "#f2efe6")
            (shape "d6-red" "Die (d6, red)" "d6" [16 16 16] "#b32b2b")]}
   {:group "Boards & paper"
    :items [{:id "board-blank" :label "Blank board" :kind "board" :shape "rect" :size [500 3 500]
             :bg "#f3efe4" :edge "#3b3b3b"}
            {:id "board-round" :label "Round board" :kind "board" :shape "round" :size [500 3 500]
             :bg "#f3efe4" :edge "#3b3b3b"}
            {:id "board-square-grid" :label "Square-grid board" :kind "board" :shape "rect" :size [500 3 500]
             :bg "#f3efe4" :grid {:type "square" :cell 50 :color "#9aa3ab"}}
            {:id "board-hex-grid" :label "Hex-grid board" :kind "board" :shape "rect" :size [600 3 450]
             :bg "#f3efe4" :grid {:type "hex" :cell 40 :color "#9aa3ab"}}
            {:id "board-dots" :label "Dot-grid board" :kind "board" :shape "rect" :size [500 3 500]
             :bg "#f3efe4" :grid {:type "dots" :cell 25 :color "#8a939b"}}
            {:id "paper" :label "Sheet of paper (A4)" :kind "board" :shape "rect" :size [210 0.3 297]
             :bg "#fbfaf6" :edge "#e9e6dd"}
            {:id "tile-blank" :label "Blank tile" :kind "board" :shape "rect" :size [60 2 60]
             :bg "#e8dcc0" :edge "#cbbd9c"}
            {:id "token-blank" :label "Blank token" :kind "board" :shape "round" :size [30 2 30]
             :bg "#e8dcc0" :edge "#cbbd9c"}]}
   {:group "Playing cards"
    :items [{:id "deck-52" :label "Standard deck" :kind "cards" :size [63.5 88.9]
             :cards (tx/standard-deck false)}
            {:id "deck-54" :label "Standard deck + jokers" :kind "cards" :size [63.5 88.9]
             :cards (tx/standard-deck true)}
            {:id "deck-blank" :label "Blank cards (20)" :kind "cards" :size [63.5 88.9]
             :cards (vec (repeat 20 {:name "Blank" :face "std:blank" :back "std:back"}))}]}])

(defonce games (r/atom []))     ; [{:group :items}] from library.json
(defonce mine (r/atom []))      ; saved components

(defn all-groups []
  (concat basics @games
          (when (seq @mine) [{:group "My components" :mine? true :items @mine}])))

(defn by-id [id]
  (some #(when (= id (:id %)) %) (mapcat :items (all-groups))))

(defn- signed-in? []
  (and (exists? js/playerKey) (not (str/blank? js/playerKey)) (exists? js/csrfToken)))

(defn load! []
  (-> (js/fetch "/table/library.json")
      (.then #(if (.-ok %) (.json %) #js []))
      (.then (fn [js-lib]
               (reset! games
                       (vec (for [g (js->clj js-lib :keywordize-keys true)
                                  [sub items] (group-by #(or (:group %) (str (:game g))) (:items g))]
                              {:group sub :game (:game g) :items (vec items)})))))
      (.catch (fn [e] (js/console.warn "no game library" e))))
  (-> (if (signed-in?)
        (-> (js/fetch "/table/components" #js {:headers #js {"Accept" "application/json"}})
            (.then (fn [^js res] (.json res)))
            (.then (fn [^js body] (reader/read-string (.-items body)))))
        (store/get! "components"))
      (.then #(reset! mine (vec %)))
      (.catch #(js/console.warn "no saved components" %))))

(defn- keep-mine!
  "Write My components where they live: with the account when signed in, so
   they follow you to any table and browser; in this browser otherwise."
  []
  (if (signed-in?)
    (js/fetch "/table/components"
              #js {:method "POST"
                   :headers #js {"Content-Type" "application/json" "Accept" "application/json"
                                 "x-csrf-token" js/csrfToken}
                   :body (js/JSON.stringify #js {:items (pr-str @mine)})})
    (store/put! "components" @mine)))

(defn save-component!
  "Keep objs (table objects) in My components under `label`."
  [label objs]
  (let [item (if (= 1 (count objs))
               {:id (str "mine-" (random-uuid)) :label label :kind "object" :object (first objs)}
               {:id (str "mine-" (random-uuid)) :label label :kind "objects" :objects (vec objs)})]
    (swap! mine conj item)
    (keep-mine!)))

(defn forget-component! [id]
  (swap! mine (fn [m] (vec (remove #(= id (:id %)) m))))
  (keep-mine!))
