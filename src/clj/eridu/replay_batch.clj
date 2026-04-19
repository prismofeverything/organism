(ns eridu.replay-batch
  "Investigatory replay: given a JSON file of target games (each with a fixed
   roster of player-configs), run N replay games per target with the same
   personality composition and player-count, writing per-target summaries +
   snapshots to CSV. Used by the deep-dive pipeline's Stage 3.

   Invocation:
     lein run -m eridu.replay-batch <targets.edn> <output-dir> [games-per-target]

   Targets EDN shape:
     {:games-per-target 500
      :targets [{:bucket \"1p-bottom\"
                 :game-id \"g5-1p\"
                 :original-run 1
                 :original-generation 0
                 :original-collective-rep 3
                 :original-seed 12345
                 :player-configs [{:key \"...\" :personality {:name \"...\" ...weights...}}]}]}"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [eridu.simulate :as sim]))

(def summary-columns
  [:bucket :target-game-id :original-run :original-generation
   :original-collective-rep :original-seed :replay-idx
   :game-id :player-count :player :personality
   :amity :glory :reputation
   :merchant-lv :priest-lv :raider-lv :leader-lv
   :temples-placed :temples-flipped :raiders-deployed :demands-fulfilled
   :starting-card-num :starting-city :starting-role :starting-resource
   :space-visits :avg-astros-on-visit :solo-landings :top-space
   :sp1 :sp2 :sp3 :sp4 :sp5 :sp6 :sp7 :top-pair
   :bonus-board-id :wild-points :feats-claimed
   :target-feat-1 :target-feat-2 :total-travels
   :dice-quads :dice-triples :dice-doubles :dice-unique
   :board-effects-fired :board-effects-noop :board-effect-amity :board-effect-glory
   :contests-in-play :seed])

(def snapshot-columns
  [:bucket :target-game-id :replay-idx
   :game-id :player-count :round :turn :player :phase
   :amity :glory :reputation
   :merchant-lv :priest-lv :raider-lv :leader-lv
   :tools :pottery :gold :gems
   :temples-placed :temples-flipped :raiders-deployed :raiders-supply
   :temples-supply :demands-fulfilled :caravan-city :bonus-board-id])

(defn- csv-cell [v]
  (cond
    (nil? v) ""
    (keyword? v) (name v)
    (string? v) (if (re-find #"[,\"\n]" v)
                  (str "\"" (str/replace v "\"" "\"\"") "\"")
                  v)
    :else (str v)))

(defn- write-header! [writer columns]
  (.write writer (str (str/join "," (map name columns)) "\n")))

(defn- write-row! [writer columns row]
  (.write writer (str (str/join "," (map #(csv-cell (get row %)) columns)) "\n")))

(defn- read-targets [path]
  (edn/read-string (slurp path)))

(defn- decode-configs [player-configs]
  (mapv (fn [pc]
          {:key (:key pc)
           :personality (:personality pc)})
        player-configs))

(defn- summary-row-base [tgt replay-idx summary-row]
  (merge summary-row
         {:bucket (:bucket tgt)
          :target-game-id (:game-id tgt)
          :original-run (:original-run tgt)
          :original-generation (:original-generation tgt)
          :original-collective-rep (:original-collective-rep tgt)
          :original-seed (:original-seed tgt)
          :replay-idx replay-idx
          :player-count (count (:player-configs tgt))}))

(defn- snapshot-row-base [tgt replay-idx replay-game-id snap]
  (merge snap
         {:bucket (:bucket tgt)
          :target-game-id (:game-id tgt)
          :replay-idx replay-idx
          :game-id replay-game-id
          :player-count (count (:player-configs tgt))}))

(defn run-target!
  "Run n-games replays for one target, writing rows to the supplied writers."
  [tgt n-games summary-writer snapshot-writer]
  (let [configs (decode-configs (:player-configs tgt))]
    (dotimes [i n-games]
      (let [result (sim/run-game configs)
            summaries (sim/game-result-summary result configs)
            replay-game-id (str (:game-id tgt) "-r" i)]
        (doseq [s summaries]
          (write-row! summary-writer summary-columns
                      (summary-row-base tgt i s)))
        (doseq [snap (:snapshots result)]
          (write-row! snapshot-writer snapshot-columns
                      (snapshot-row-base tgt i replay-game-id snap)))))))

(defn run-batch!
  "Run all targets. Writes summaries.csv + snapshots.csv in output-dir."
  [targets-path output-dir default-games]
  (let [payload (read-targets targets-path)
        n-games (or (:games-per-target payload) default-games 500)
        targets (:targets payload)
        summaries-path (str output-dir "/replay-summaries.csv")
        snapshots-path (str output-dir "/replay-snapshots.csv")
        _ (.mkdirs (io/file output-dir))]
    (with-open [sw (io/writer summaries-path)
                snw (io/writer snapshots-path)]
      (write-header! sw summary-columns)
      (write-header! snw snapshot-columns)
      (let [n-targets (count targets)]
        (doseq [[idx tgt] (map-indexed vector targets)]
          (println (format "[%d/%d] bucket=%s target=%s — %d replays"
                           (inc idx) n-targets (:bucket tgt) (:game-id tgt) n-games))
          (flush)
          (run-target! tgt n-games sw snw))))
    (println (format "Wrote %s and %s" summaries-path snapshots-path))))

(defn -main [& args]
  (let [[targets-path output-dir games-arg] args
        n-games (when games-arg (Integer/parseInt games-arg))]
    (when (or (nil? targets-path) (nil? output-dir))
      (println "Usage: lein run -m eridu.replay-batch <targets.edn> <output-dir> [games-per-target]")
      (System/exit 2))
    (run-batch! targets-path output-dir n-games)
    (shutdown-agents)))
