(ns organism.routes.table-ws
  "WebSocket handler for the playtest table.

   There are no rules to run, so the server's whole job is ordering: every
   change arrives as a patch (table.patch), is applied to the table here, and
   is passed on to everyone else at the table in the order it was applied. The
   sender gets an ack in that same order, which is how a browser knows whether
   a change it hears about came before or after its own (see table.net).

   Cursors are passed on and forgotten. The table is saved a couple of seconds
   after it stops changing, and when the last person leaves."
  (:require
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [org.httpkit.server :as hk]
   [organism.game-ws :as gws :refer [read-json send!]]
   [organism.persist-table :as persist]
   [table.patch :as patch]))

;; {:games {key {:key :title :objects :players :channels :watchers :save-at}}}
(defonce games (atom {:games {}}))

(def save-delay-ms 2000)

(defn- others [game channel] (remove #{channel} (:channels game)))

(defn- presence-message [game]
  {"type" "presence" "who" (vec (distinct (remove nil? (vals (:watchers game)))))})

(defn- broadcast-presence! [key]
  (when-let [game (gws/game-record games key)]
    (gws/send-channels! (:channels game) (presence-message game))))

;; ── saving ─────────────────────────────────────────────────────────────────

(defn- save-now! [db key]
  (when-let [game (and db (gws/game-record games key))]
    (try (persist/save-objects! db key (:objects game))
         (catch Exception e (log/error "Table save failed" key (.getMessage e))))))

(defn- save-soon!
  "Save once the table has been still for a moment. Each change pushes the
   moment back; the one waiting thread notices and waits again."
  [db key]
  (let [at (+ (System/currentTimeMillis) save-delay-ms)
        [old _] (swap-vals! games update-in [:games key] assoc :save-at at)]
    (when-not (get-in old [:games key :save-at])
      (future
        (loop []
          (let [due (get-in @games [:games key :save-at])]
            (when due
              (let [wait (- due (System/currentTimeMillis))]
                (if (pos? wait)
                  (do (Thread/sleep wait) (recur))
                  (do (swap! games update-in [:games key] dissoc :save-at)
                      (save-now! db key)))))))))))

;; ── messages ───────────────────────────────────────────────────────────────

(def ^:private patch-keys #{:remove :put :move :stroke :trim})

(defn- handle-patch! [db key player channel message]
  (let [raw (get message "patch")
        p (try (edn/read-string raw) (catch Exception _ nil))]
    (if-not (and (map? p) (every? patch-keys (keys p)))
      (log/warn "Table: bad patch from" player key)
      (do
        (swap! games update-in [:games key :objects] patch/apply-patch p)
        (let [game (gws/game-record games key)]
          (gws/send-channels! (others game channel) {"type" "patch" "patch" raw "from" player}))
        (send! channel {"type" "ack"})
        (save-soon! db key)))))

(defn- handle-cursor! [key player channel message]
  (let [game (gws/game-record games key)]
    (gws/send-channels! (others game channel)
                        {"type" "cursor" "player" player "at" (get message "at")})))

;; ── lifecycle ──────────────────────────────────────────────────────────────

(defn- load-table! [db key]
  (or (gws/game-record games key)
      (when-let [stored (and db (persist/load-table db key))]
        (gws/put-game! games key (assoc stored :channels #{} :watchers {})))))

(defn connect! [{:keys [key player db]} channel]
  (if-let [game (load-table! db key)]
    (do
      (gws/watch! games key channel player)
      (when (and player (not (some #{player} (:players game))))
        (let [players (conj (vec (:players game)) player)]
          (swap! games assoc-in [:games key :players] players)
          (when db (persist/save-players! db key players))))
      (log/info "Table CONNECT" player key)
      (let [game (gws/game-record games key)]
        (send! channel {"type" "initialize" "player" (str player) "title" (:title game)
                        "objects" (pr-str (:objects game))
                        "players" (vec (:players game))}))
      (broadcast-presence! key))
    (do (send! channel {"type" "missing"})
        (hk/close channel))))

(defn disconnect! [{:keys [key player db]} channel status]
  (log/info "Table DISCONNECT" player key status)
  (gws/unwatch! games key channel)
  (let [game (gws/game-record games key)]
    (if (empty? (:channels game))
      (do (save-now! db key)
          (gws/forget-game! games key))
      (do (gws/send-channels! (:channels game) {"type" "cursor" "player" player "at" nil})
          (broadcast-presence! key)))))

(defn notify-clients! [{:keys [key player db]} channel raw]
  (let [message (read-json raw)]
    (case (get message "type")
      "patch" (handle-patch! db key player channel message)
      "cursor" (handle-cursor! key player channel message)
      (log/warn "Unknown table message" (get message "type")))))

(defn ws-handler [db {:keys [path-params session] :as request}]
  (if-let [player (:player session)]
    (hk/as-channel request
                   (gws/make-callbacks {:player player :key (:play path-params) :db db}
                                       {:on-open #'connect!
                                        :on-close #'disconnect!
                                        :on-receive #'notify-clients!}))
    {:status 403 :body "sign in to join a table"}))

(defn table-ws-routes [db]
  [["/ws/table/play/:play" (partial ws-handler db)]])
