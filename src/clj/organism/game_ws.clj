(ns organism.game-ws
  "Shared WebSocket plumbing for every game.

   Two things are identical across all per-game `*_ws.clj` handlers and live
   here so they are written once:

   1. Transit (de)serialization — `read-json` / `write-json` / `send!` /
      `send-channels!`.
   2. The channel registry over a per-game `games` atom of shape
        {:games {play-key {... :channels #{channel ...}}}}
      — `find-game!` / `append-channel!` / `remove-channel!`.

   Each game keeps its OWN `games` atom (the per-game game record differs —
   bots/history/ruleset/etc.) and supplies the game-specific lifecycle
   (`connect!` / `disconnect!` / `notify-clients!`) plus `broadcast-state!`.
   The registry helpers take the atom explicitly so they stay game-agnostic."
  (:require
   [org.httpkit.server :as hk]
   [clojure.java.io :as io]
   [cognitect.transit :as transit])
  (:import
   [java.io ByteArrayOutputStream]))

;; ── Transit helpers ─────────────────────────────────────────────────────────

(defn- ->stream [input]
  (cond (string? input) (io/input-stream (.getBytes ^String input))
        :else input))

(defn read-json
  "Read a transit-json message (string or stream) into Clojure data."
  [input]
  (with-open [ins (->stream input)]
    (-> ins (transit/reader :json) transit/read)))

(defn write-json
  "Serialize Clojure data to a transit-json string."
  [output]
  (let [out (ByteArrayOutputStream. 4096)
        w   (transit/writer out :json)]
    (transit/write w output)
    (.toString out)))

(defn send!
  "Send a single message to one channel."
  [channel message]
  (hk/send! channel (write-json message)))

(defn send-channels!
  "Send a message to every channel in a collection."
  [channels message]
  (doseq [ch channels]
    (send! ch message)))

;; ── Channel registry over a per-game games atom ─────────────────────────────

(defn game-record
  "The game record stored under play-key, or nil."
  [games-atom play-key]
  (get-in @games-atom [:games play-key]))

(defn put-game!
  "Store game under play-key, returning the game."
  [games-atom play-key game]
  (swap! games-atom assoc-in [:games play-key] game)
  game)

(defn append-channel!
  "Register channel as a watcher of play-key."
  [games-atom play-key channel]
  (swap! games-atom update-in [:games play-key :channels] conj channel))

(defn find-game!
  "Find-or-create the game record for play-key and register `channel`.

   When the game is absent, `(make-game play-key channel)` is called to build
   the initial record (it MUST include `channel` in its `:channels` set and
   should NOT itself write to the atom — storage happens here)."
  [games-atom play-key channel make-game]
  (let [existing (get-in @games-atom [:games play-key])]
    (if (empty? existing)
      (put-game! games-atom play-key (make-game play-key channel))
      (do (append-channel! games-atom play-key channel)
          (update existing :channels conj channel)))))

(defn remove-channel!
  "Drop channel from play-key; remove the game entirely when no channels remain."
  [games-atom play-key channel]
  (swap! games-atom
         (fn [gs]
           (let [remaining (remove #{channel}
                                   (get-in gs [:games play-key :channels]))]
             (if (empty? remaining)
               (update gs :games dissoc play-key)
               (assoc-in gs [:games play-key :channels] (set remaining)))))))

;; ── Route wiring ────────────────────────────────────────────────────────────

(defn make-callbacks
  "Build httpkit channel callbacks from a `cfg` map and the three lifecycle
   handlers, each invoked as (handler cfg ...)."
  [cfg {:keys [on-open on-close on-receive]}]
  {:on-open    (partial on-open    cfg)
   :on-close   (partial on-close   cfg)
   :on-receive (partial on-receive cfg)})
