(ns organism.app
  (:require [organism.play :as play]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(play/init!)
