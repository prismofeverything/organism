(ns organism.organism
  (:require
    [organism.play :as play]
    [cljs.spec.alpha :as s]
    [expound.alpha :as expound]))

(extend-protocol IPrintWithWriter
  js/Symbol
  (-pr-writer [sym writer _]
    (-write writer (str "\"" (.toString sym) "\""))))

(set! s/*explain-out* expound/printer)

(enable-console-print!)





;; ;;ignore println statements in prod
;; (set! *print-fn* (fn [& _]))

(play/init!)

;; (defn initiate!
;;   [tries]
;;   (cond
;;     (.getElementById js/document "organism")
;;     (play/init!)

;;     (> tries 0)
;;     (.setTimeout js/window (partial initiate! (dec tries)))

;;     :else
;;     (println "FAILED TO MOUNT organism")))

;; (initiate! 13)
