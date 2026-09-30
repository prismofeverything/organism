(ns distinctions.bot
  "Opponents for DISTINCTIONS.

   A hand is scored by how few cards it is missing from its closest split
   into two teams of four (see `game/cover-progress`), and among equals by
   how many splits are that close. A bot takes the pile's top card only if it
   can then discard its way to a better hand than it holds now, and otherwise
   draws blind. It always discards the card whose loss leaves the best hand
   behind.

   SORTER plays exactly that. DRIFTER does the same but, one time in four,
   draws blind when it would have taken -- it telegraphs less and plays a
   little worse."
  (:require
   [distinctions.game :as game]))

(defn score
  "How good `hand` is: fewer cards missing first, then more ways to finish."
  [hand]
  (let [{:keys [missing ways]} (game/cover-progress hand)]
    (- ways (* 1000 missing))))

(defn best-discard
  "The card to throw from `hand` (which holds one extra), never `keep`."
  [hand keep]
  (apply max-key
         (fn [c] (score (remove #{c} hand)))
         (remove #{keep} hand)))

(defn- would-take?
  [state seat]
  (let [hand (get-in state [:hands seat])
        top  (game/top-discard state)]
    (when top
      (let [with (conj hand top)]
        (> (score (remove #{(best-discard with top)} with))
           (score hand))))))

(defn choose
  "The action the bot at the seat to act takes. `caution` is the chance it
   draws blind even when the pile would help it."
  ([state] (choose state 0.0))
  ([state caution]
   (let [seat  (:to-act state)
         legal (game/legal-actions state)]
     (case (:step state)
       :draw    (if (and (:take legal)
                         (or (not (:draw legal))
                             (and (would-take? state seat) (>= (rand) caution))))
                  {:action :take}
                  {:action :draw})
       :discard {:action :discard
                 :card   (best-discard (get-in state [:hands seat]) (:taken state))}
       nil))))

(def profiles
  [{:name "SORTER"  :caution 0.0
    :description "Takes the pile's card only when it helps, and always throws the card that matters least."}
   {:name "DRIFTER" :caution 0.25
    :description "Plays like SORTER, but a quarter of the time draws blind rather than show you what it wants."}])

(defn step
  "A whole move for whoever is to act: returns the state after it."
  [state caution]
  (if-let [a (choose state caution)]
    (game/act state (:to-act state) a)
    state))
