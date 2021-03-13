(ns organism.organism
  (:require
    [cljs.spec.alpha :as s]
    [expound.alpha :as expound]
    [organism.play :as play]))

(extend-protocol IPrintWithWriter
  js/Symbol
  (-pr-writer [sym writer _]
    (-write writer (str "\"" (.toString sym) "\""))))

(set! s/*explain-out* expound/printer)

(enable-console-print!)

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(play/init!)

