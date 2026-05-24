(ns organism.format
  "Organism Game Format (OGF) — a universal, readable, minimal JSON encoding of an
   organism game sequence: board topology + a per-turn snapshot of the board.

   Designed to be language-neutral (plain JSON), human-readable, and compact.

   {
     \"format\": \"organism\", \"version\": 1,
     \"name\": <game name>,
     \"symmetry\": <int>,                    ; board rotational symmetry (hex=6, pentagon=5)
     \"players\": [<player> ...],            ; turn order
     \"colors\": {<player>: <color> ...},    ; player -> piece color
     \"board\": {
       \"center\": <space>,
       \"ring-colors\": [<color> ...],       ; ring (board-zone) colors present
       \"spaces\": [<space> ...],
       \"adjacencies\": {<space>: [<space> ...] ...}
     },
     \"frames\": [                            ; one per recorded state, in order
       {\"turn\": <i>, \"round\": <r>, \"player\": <player>,
        \"elements\": [[<player>, <type>, <space>, <food>] ...],
        \"food\": {<space>: <amount> ...},    ; free food on the board
        \"captures\": {<player>: <count> ...}}
     ]
   }

   <space> is the string \"<ring-color>:<index>\" e.g. \"red:3\" — an opaque key into
   adjacencies / a layout. <type> is eat|move|grow. The board graph (adjacencies +
   center) plus symmetry fully define the board geometry; a 2D layout can be derived
   (ring = graph distance from center) or matched to organism.board/board-locations.

   jsonista is on the classpath via metosin/muuntaja."
  (:require
   [clojure.string :as string]
   [jsonista.core :as json]))

(def ^:private pretty (json/object-mapper {:pretty true}))

(defn space->str
  "Encode a space id [ring-label index] as \"label:index\". Works whether the ring
   label is a keyword (:red) or a string (\"A\")."
  [[color n]]
  (str (name color) ":" n))

(defn str->space
  [s]
  (let [i (string/last-index-of s ":")]
    [(keyword (subs s 0 i)) (Integer/parseInt (subs s (inc i)))]))

(defn board-symmetry
  "Board rotational symmetry = degree of the center space, matched by string id
   (robust to keyword-vs-string ring labels)."
  [str-adjacencies center-str]
  (count (get str-adjacencies center-str)))

(defn element->tuple
  [el]
  [(name (:player el)) (name (:type el)) (space->str (:space el)) (:food el)])

(defn tuple->element
  [[player type space food]]
  {:player (keyword player) :type (keyword type)
   :space (str->space space) :food food :captures []})

(defn state->frame
  [i state]
  {:turn i
   :round (:round state)
   :player (some-> (get-in state [:player-turn :player]) name)
   :elements (mapv (fn [[_ el]] (element->tuple el)) (:elements state))
   :food (into {} (map (fn [[sp amt]] [(space->str sp) amt])) (:food state))
   :captures (into {} (map (fn [[pl caps]] [(name pl) (count caps)])) (:captures state))})

(defn frame->state
  [frame]
  {:round (get frame "round")
   :player-turn {:player (some-> (get frame "player") keyword)}
   :elements (into {} (map (fn [t] (let [el (tuple->element t)] [(:space el) el]))) (get frame "elements"))
   :food (into {} (map (fn [[s a]] [(str->space s) a])) (get frame "food"))
   :captures (into {} (map (fn [[p c]] [(keyword p) c])) (get frame "captures"))})

(defn game->ogf
  "Build OGF data (plain data) from a loaded game (organism.persist/load-game)."
  [{:keys [key invocation game history]}]
  (let [players (mapv name (:players invocation))
        colors-list (map (comp name last) (:colors invocation))
        adjacencies (:adjacencies game)
        str-adj (into {} (map (fn [[sp adjs]] [(space->str sp) (mapv space->str adjs)])) adjacencies)
        center-str (space->str (:center game))
        spaces (vec (sort (keys str-adj)))
        ring-colors (vec (distinct (map #(first (string/split % #":")) spaces)))]
    {:format "organism"
     :version 1
     :name key
     :symmetry (board-symmetry str-adj center-str)
     :players players
     :colors (zipmap players colors-list)
     :board {:center center-str
             :ring-colors ring-colors
             :spaces spaces
             :adjacencies str-adj}
     :frames (vec (map-indexed state->frame history))}))

(defn ogf->game
  "Reconstruct board + state sequence from parsed OGF data (string keys)."
  [ogf]
  {:name (get ogf "name")
   :symmetry (get ogf "symmetry")
   :players (mapv keyword (get ogf "players"))
   :colors (into {} (map (fn [[p c]] [(keyword p) (keyword c)])) (get ogf "colors"))
   :board {:center (str->space (get-in ogf ["board" "center"]))
           :adjacencies (into {} (map (fn [[s adjs]] [(str->space s) (mapv str->space adjs)]))
                              (get-in ogf ["board" "adjacencies"]))}
   :history (mapv frame->state (get ogf "frames"))})

(defn write-ogf!
  "Write OGF data to a JSON file (pretty-printed)."
  [ogf path]
  (spit path (json/write-value-as-string ogf pretty)))

(defn read-ogf
  "Read a JSON OGF file -> parsed data (string keys)."
  [path]
  (json/read-value (slurp path)))
