(ns organism.scripts.extract-game
  "Extract an organism game sequence by name and export it to OGF (JSON).

   Run on the server (where MongoDB lives):
     lein run -m organism.scripts.extract-game <game-name> [out.json]

   <game-name> is the game's key (its generated name, e.g. \"happy-tiger\").
   Default output is <game-name>.ogf.json. The OGF file format is documented in
   organism.format."
  (:require
   [organism.mongo :as db]
   [organism.persist :as persist]
   [organism.format :as ogf]))

(def mongo-connection
  {:host "localhost" :port 27017 :database "organism"})

(defn -main
  [& args]
  (let [game-name (first args)
        out (or (second args) (str game-name ".ogf.json"))]
    (if-not game-name
      (do
        (println "usage: lein run -m organism.scripts.extract-game <game-name> [out.json]")
        (System/exit 1))
      (let [db   (db/connect! mongo-connection)
            game (persist/load-game db game-name)]
        (if-not game
          (do
            (println "no game found named:" game-name)
            (System/exit 2))
          (let [data (ogf/game->ogf game)]
            (ogf/write-ogf! data out)
            (println "wrote" out)
            (println "  symmetry:" (:symmetry data))
            (println "  players :" (:players data))
            (println "  frames  :" (count (:frames data)))
            (println "  spaces  :" (count (get-in data [:board :spaces])))
            (System/exit 0)))))))
