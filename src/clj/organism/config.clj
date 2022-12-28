(ns organism.config
  (:require
    [cprop.core :refer [load-config]]
    [cprop.source :as source]
    [mount.core :refer [args defstate]]))

(defstate env
  :start
  (let [merge-list
        [(args)
         (source/from-system-props)
         (source/from-env)]]
    (load-config
     :merge
     merge-list)))
