(ns organism.history
  "How a browser keeps its copy of a game's history in step with the server's
   log (organism.game-log). Shared, so the server's tests run the very function
   the page does.")

(defn follow-log
  "This page's history is a copy of the server's log, and `length` is how
   long the log now is with `state` as its newest entry. One longer: a move,
   add it. Shorter: an undo, cut back to it -- provided the entry there is
   `state`. Anything else and the copy is wrong: nil, and the page asks for
   the whole log."
  [history length state]
  (let [history (vec history)
        n (count history)]
    (cond
      (nil? length) nil
      (= length (inc n)) (conj history state)
      (and (pos? length) (<= length n) (= state (nth history (dec length))))
      (subvec history 0 length)
      :else nil)))
