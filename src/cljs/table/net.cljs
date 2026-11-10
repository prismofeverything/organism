(ns table.net
  "Playing at one table with other people, over the site's WebSocket.

   Nothing here knows what a card or a board is. `sent` is the table as the
   server has it, or will once it has what we sent; the doc is the table as
   this browser shows it. Every so often the difference between the two goes
   to the server as a patch (table.patch) and `sent` catches up -- so a drag
   of a hundred mouse moves goes as a couple of dozen small moves, and a
   change made while the connection is down goes when it comes back.

   Patches from other people are applied to both, so they never come back out
   as ours, and to every undo snapshot, so undo takes back only what you did.

   Order: the server applies patches one at a time and tells everyone else in
   that order; it acks each of ours in the same order. So a patch from someone
   else touching a thing we have changed and not yet had acked was applied
   BEFORE ours -- ours lands on top of it at the server -- and we skip it, or
   this browser would show their version while everyone else shows ours."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [cognitect.transit :as t]
            [reagent.core :as r]
            [table.patch :as patch]
            [table.state :as st]))

(defn online? []
  (and (exists? js/playKey) (not (str/blank? js/playKey))))

(defn me [] (when (and (exists? js/playerKey) (not (str/blank? js/playerKey))) js/playerKey))

(defonce status (r/atom :offline))   ; :connecting :open :closed :missing
(defonce who (r/atom []))            ; players here now
(defonce peers (r/atom {}))          ; player -> [x y z] of their cursor

(defonce ^:private sent (atom nil))
(defonce ^:private unacked (atom #queue []))   ; id sets, oldest first
(defonce ^:private pending (atom {}))           ; id -> patches of ours not yet acked
(defonce ^:private conn (atom nil))
(defonce ^:private backoff (atom 1000))
(defonce ^:private flush-timer (atom nil))

(def ^:private reader (t/reader :json))
(def ^:private writer (t/writer :json))
(def frame-limit 48000)   ; the server takes frames up to 64 KB

(defn- send! [msg]
  (when-let [^js ws @conn]
    (when (= 1 (.-readyState ws))
      (.send ws (t/write writer msg))
      true)))

;; ── outgoing ───────────────────────────────────────────────────────────────

(defn flush! []
  (reset! flush-timer nil)
  (when (and (= :open @status) @sent)
    (let [now (st/objects)
          p (patch/diff @sent now)]
      (when-not (patch/empty-patch? p)
        (doseq [part (patch/split p frame-limit)]
          (let [ids (patch/ids part)]
            (when (send! {"type" "patch" "patch" (pr-str part)})
              (swap! unacked conj ids)
              (swap! pending #(reduce (fn [m id] (update m id (fnil inc 0))) % ids)))))
        (reset! sent now)))))

(defn changed!
  "The doc changed: send the difference soon."
  []
  (when (and (online?) (nil? @flush-timer))
    (reset! flush-timer (js/setTimeout flush! 40))))

(defonce ^:private last-cursor (atom 0))

(defn cursor! [^js p]
  (let [t (js/Date.now)]
    (when (and (online?) (> (- t @last-cursor) 80))
      (reset! last-cursor t)
      (send! {"type" "cursor" "at" (when p [(js/Math.round (.-x p)) (js/Math.round (.-y p)) (js/Math.round (.-z p))])}))))

;; ── incoming ───────────────────────────────────────────────────────────────

(defn- acked! []
  (when-let [ids (peek @unacked)]
    (swap! unacked pop)
    (swap! pending #(reduce (fn [m id] (let [n (dec (get m id 0))] (if (pos? n) (assoc m id n) (dissoc m id))))
                            % ids))))

(defn- theirs! [raw]
  (let [p (patch/only (reader/read-string raw) #(not (contains? @pending %)))]
    (when-not (patch/empty-patch? p)
      (swap! sent patch/apply-patch p)
      (st/rebase-history! #(patch/apply-patch % p))
      (swap! st/doc update :objects patch/apply-patch p)
      (swap! st/ui update :selected #(set (filter (st/objects) %))))))

(defn- initialize! [objects]
  ;; whatever we did while disconnected goes on top of the table as it is now
  (let [ours (when @sent (patch/diff @sent (st/objects)))]
    (reset! sent objects)
    (reset! unacked #queue [])
    (reset! pending {})
    (reset! st/history {:past [] :future [] :tag nil :at 0})
    (swap! st/doc assoc :objects (cond-> objects ours (patch/apply-patch ours)))
    (swap! st/ui update :selected #(set (filter (st/objects) %)))
    (changed!)))

(defn- receive! [^js e]
  (let [m (t/read reader (.-data e))]
    (case (get m "type")
      "initialize" (do (reset! status :open)
                       (reset! backoff 1000)
                       (initialize! (reader/read-string (get m "objects"))))
      "patch" (theirs! (get m "patch"))
      "ack" (acked!)
      "presence" (do (reset! who (vec (get m "who")))
                     (swap! peers select-keys (get m "who")))
      "cursor" (let [p (get m "player")]
                 (when (not= p (me))
                   (if-let [at (get m "at")] (swap! peers assoc p at) (swap! peers dissoc p))))
      "missing" (reset! status :missing)
      (js/console.log "table: unknown message" (get m "type")))))

;; ── connection ─────────────────────────────────────────────────────────────

(defn connect! []
  (when (online?)
    (let [proto (if (= "https:" (.-protocol js/location)) "wss:" "ws:")
          ws (js/WebSocket. (str proto "//" (.-host js/location) "/ws/table/play/"
                                 (js/encodeURIComponent js/playKey)))]
      (reset! status :connecting)
      (reset! conn ws)
      (set! (.-onmessage ws) receive!)
      (set! (.-onclose ws)
            (fn [_]
              (when (identical? ws @conn)
                (when-not (= :missing @status) (reset! status :closed))
                (reset! who [])
                (reset! peers {})
                ;; back off, never give up: a server restart is minutes, not forever
                (let [wait @backoff]
                  (swap! backoff #(min 30000 (* 2 %)))
                  (when-not (= :missing @status)
                    (js/setTimeout connect! wait)))))))))

;; ── files ──────────────────────────────────────────────────────────────────

(defn upload!
  "A promise of a URL for a data URL: at a shared table the file is kept on
   the server (the table holds only its address); on your own it stays as is."
  [data-url]
  (if-not (me)
    (js/Promise.resolve data-url)
    (-> (js/fetch "/table/upload"
                  #js {:method "POST"
                       :headers #js {"Content-Type" "application/json"
                                     "Accept" "application/json"
                                     "x-csrf-token" js/csrfToken}
                       :body (js/JSON.stringify #js {:data data-url})})
        (.then (fn [^js res] (if (.-ok res) (.json res) (throw (js/Error. (str "upload: " (.-status res)))))))
        (.then (fn [^js body] (.-url body))))))
