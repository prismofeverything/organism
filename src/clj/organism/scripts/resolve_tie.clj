(ns organism.scripts.resolve-tie
  "End a game that carried on past a tie.

     java -cp organism.jar clojure.main -m organism.scripts.resolve-tie <game-key>
     java -cp organism.jar clojure.main -m organism.scripts.resolve-tie <game-key> --apply

   Without --apply it only reports. Run it that way first and check the winner
   is the one you expect. After applying, re-run the ratings:

     java -cp organism.jar clojure.main -m organism.scripts.rate-players"
  (:require
   [organism.handler :as handler]
   [organism.mongo :as db]
   [organism.resolve-tie :as resolve-tie]))

(defn- describe
  [{:keys [status key winner acting round history-count discard-count standing index]}]
  (case status
    :not-found     (println "no game called" key)
    :no-history    (println key "has no history")
    :no-winner     (println key "- no victory anywhere in" history-count "states; nothing to fix")
    :already-ended (println key "- already ended properly, winner" winner)
    (do
      (println (str key " - ended at history entry " index " of " history-count
                    " (round " round ")"))
      (println "  winner:        " winner)
      (println "  turn taken by: " acting (str "(" acting " caused the tie, so " acting " loses)"))
      (println "  organisms:     " (:organisms standing))
      (println "  rel. captures: " (:captures standing))
      (println "  states played after the game ended:" discard-count)
      (when (= status :resolved)
        (println "  -> resolved. the overrun is kept in"
                 (resolve-tie/voided-history-key key)))
      (when (= status :overran)
        (println "  -> dry run, nothing written. re-run with --apply to end it here.")))))

(defn -main
  [& args]
  (let [game-key (first (remove #(.startsWith ^String % "--") args))
        apply? (boolean (some #{"--apply"} args))]
    (if-not game-key
      (do (println "usage: resolve-tie <game-key> [--apply]")
          (System/exit 1))
      (let [connection (db/connect! handler/mongo-connection)
            report (if apply?
                     (resolve-tie/resolve-tie! connection game-key)
                     (resolve-tie/examine connection game-key))]
        (describe report)
        (System/exit 0)))))
