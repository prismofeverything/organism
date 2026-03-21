(ns ^:figwheel-no-load journey.journey
  (:require
   [journey.play :as play]
   [cljs.spec.alpha :as s]
   [expound.alpha :as expound]
   [devtools.core :as devtools]))

(extend-protocol IPrintWithWriter
  js/Symbol
  (-pr-writer [sym writer _]
    (-write writer (str "\"" (.toString sym) "\""))))

(set! s/*explain-out* expound/printer)

(enable-console-print!)

(devtools/install!)

(play/init!)
