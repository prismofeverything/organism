;; (ns organism.db
;;   (:require
;;    [mongo-driver-3.client :as client]
;;    [mongo-driver-3.collection :as collection]))

;; ;; unfinished attempt to port to new mongo driver (monger is deprecated)

;; (defn connect!
;;   [config]
;;   (let [client (client/create (str (:host config) ":" (:port config)))
;;         db (client/get-db client (name (:database config)))]
;;     {:client client
;;      :db db}))

;; (defn query
;;   ([connection collection] (query connection collection {}))
;;   ([connection collection where] (query connection collection where {}))
;;   ([{:keys [db]} collection where options]
;;    (collection/find db collection where options)))

;; (defn number
;;   ([connection collection] (number connection collection {}))
;;   ([{:keys [db]} collection where]
;;    (collection/count-documents connection collection where)))

;; (defn one
;;   ([connection collection] (one connection collection {}))
;;   ([{:keys [db]} collection where]
;;    ))

;; (defn find-all
;;   [connection collection]
;;   (query connection collection {}))

;; (defn find-last
;;   [{:keys [db]} collection data])

;; (defn merge!
;;   [{:keys [db]} collection data])

;; (defn insert!
;;   [{:keys [db]} collection data])

;; (defn delete!
;;   [{:keys [db]} collection data])

;; (defn purge!
;;   [{:keys [db]} collection])

;; (defn drop!
;;   [{:keys [db]} collection])

;; (defn index!
;;   [{:keys [db]} collection key data])
