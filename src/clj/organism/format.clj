(ns organism.format
  "Organism Game Format (OGF) — a universal, readable, minimal JSON encoding of an
   organism game sequence: board topology + a per-turn snapshot of the board.

   Exports v2 view records. Reads legacy v1 snapshots. See docs/ogf-view-v2.md.
   The example below documents the legacy v1 layout.

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
   [organism.ogf :as coordinates]
   [organism.board :as board]
   [jsonista.core :as json]))

(def ^:private pretty (json/object-mapper {:pretty true}))

(defn space->str
  "Encode a space id [ring-label index] as \"label:index\". Works whether the ring
   label is a keyword (:red) or a string (\"A\")."
  [[color n]]
  (str (name color) ":" n))

(defn str->space [s]
  (let [[ring index] (coordinates/parse-space s)] [(keyword ring) index]))

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
  "Export OGF v2 view records with color-independent ring coordinates and saved palette."
  [{:keys [key invocation game history]}]
  (let [players (mapv name (:players invocation))
        adjacencies (:adjacencies game)
        used (set (map (comp name first) (keys adjacencies)))
        palette (vec (:colors invocation))
        rings (vec (filter #(contains? used (name (first %))) palette))
        _ (when-not (= (count used) (count rings))
            (throw (ex-info "Board ring palette is incomplete" {:rings used})))
        labels (zipmap (map (comp name first) rings) (range))
        encode (fn [[ring index]] (coordinates/space-id (get labels (name ring)) index))
        ids (into {} (map (fn [sp] [(space->str sp) (encode sp)])) (keys adjacencies))
        convert (fn [i state]
                  (let [f (state->frame i state)]
                    (-> f
                        (update :elements #(mapv (fn [[p t s food]] [p t (get ids s) food]) %))
                        (update :food #(into {} (map (fn [[s food]] [(get ids s) food])) %)))))
        str-adj (into {} (map (fn [[sp adjs]] [(encode sp) (mapv encode adjs)])) adjacencies)
        center (encode (:center game))
        colors (mapv (comp name last) rings)
        tail (mapv (comp name last) (take (max 0 (- (count players) (count rings))) (drop (count rings) palette)))]
    {:format "organism" :version 2 :profile "view" :name key
     :symmetry (board-symmetry str-adj center) :players players
     :board (cond-> {:center center :coordinates coordinates/coordinate-system
                     :ring-colors colors
                     :spaces (mapv encode (sort-by (fn [[r i]] [(get labels (name r)) i]) (keys adjacencies)))
                     :adjacencies str-adj}
              (seq tail) (assoc :palette-tail tail))
     :frames (vec (map-indexed convert history))}))

(defn ogf->game
  "Reconstruct board + state sequence from parsed OGF data (string keys)."
  [ogf]
  {:name (get ogf "name")
   :symmetry (get ogf "symmetry")
   :players (mapv keyword (get ogf "players"))
   :colors (if (= 2 (get ogf "version"))
             (board/find-player-colors (mapv keyword (get ogf "players"))
                                      (concat (get-in ogf ["board" "ring-colors"])
                                              (get-in ogf ["board" "palette-tail"])))
             (into {} (map (fn [[p c]] [(keyword p) c])) (get ogf "colors")))
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
