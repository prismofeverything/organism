(ns organism.middleware.range
  "HTTP range requests for static assets.

   Ring 1.9 has no range middleware (it arrived upstream in 1.12), and without
   one every request for a resource returns the whole thing. A browser given a
   plain 200 for a video cannot seek: scrubbing the play-through on the learn
   page means re-downloading it. The same 200 makes a PDF viewer pull all 24
   pages of the rulebook before showing the first.

   So: advertise `Accept-Ranges` on static responses, and when a request asks
   for a byte range, serve exactly that range as a 206.

   Only single `bytes=` ranges are handled. Multi-range responses need a
   multipart/byteranges body, no browser needs one to play a video, and
   answering with the full 200 is a legal response to a range we decline."
  (:import
   [java.io File InputStream]))

;; ── Parsing ─────────────────────────────────────────────────────────────────

(defn parse-range
  "`Range: bytes=…` → [first-byte last-byte] inclusive, clamped to `total`.

   Returns :unsatisfiable when the range names nothing inside the resource, and
   nil when the header is absent, malformed, multi-range, or in units we don't
   speak — all of which mean \"just send the whole thing\"."
  [header total]
  (when (and header (pos? (long total)))
    (let [[_ spec] (re-matches #"(?i)\s*bytes\s*=\s*(.*)" header)]
      (when (and spec (not (re-find #"," spec)))
        (let [[_ from to] (re-matches #"\s*(\d*)\s*-\s*(\d*)\s*" spec)
              total (long total)]
          (cond
            (nil? from) nil

            ;; "bytes=-500" — the final 500 bytes. Browsers use this to grab an
            ;; MP4's trailing metadata before deciding what else to ask for.
            (empty? from)
            (if (empty? to)
              nil
              (let [want (Long/parseLong to)]
                (if (zero? want)
                  :unsatisfiable
                  [(max 0 (- total want)) (dec total)])))

            :else
            (let [start (Long/parseLong from)]
              (if (>= start total)
                :unsatisfiable
                [start (if (empty? to)
                         (dec total)
                         (min (dec total) (Long/parseLong to)))]))))))))

;; ── Slicing a body ──────────────────────────────────────────────────────────

(defn- skip-fully
  "InputStream/skip is allowed to skip less than asked; keep going until we
   have actually consumed `n` bytes."
  [^InputStream in ^long n]
  (loop [remaining n]
    (when (pos? remaining)
      (let [skipped (.skip in remaining)]
        (if (pos? skipped)
          (recur (- remaining skipped))
          ;; skip/0 means nothing buffered right now, not necessarily EOF
          (when (not= -1 (.read in))
            (recur (dec remaining))))))))

(defn bounded-stream
  "A stream that stops after `limit` bytes and closes the one underneath.

   All three `read` arities are spelled out: callers reach for whichever they
   like — `clojure.java.io/copy` uses `read(byte[])`, servers tend to use the
   three argument form — and a proxy only answers the arities it declares."
  [^InputStream in ^long limit]
  (let [remaining (volatile! limit)
        read-into (fn [^bytes buf off len]
                    (if (pos? @remaining)
                      (let [n (.read in buf (int off) (min (int len) (int @remaining)))]
                        (when (pos? n) (vswap! remaining - n))
                        n)
                      -1))]
    (proxy [InputStream] []
      (read
        ([]
         (if (pos? @remaining)
           (let [b (.read in)]
             (when-not (neg? b) (vswap! remaining dec))
             b)
           -1))
        ([buf]
         (read-into buf 0 (alength ^bytes buf)))
        ([buf off len]
         (read-into buf off len)))
      (available [] (min (.available in) @remaining))
      (close [] (.close in)))))

(defn- slice
  "Body limited to bytes [start end], as a stream. Returns nil if the body is
   not something we know how to seek into."
  [body ^long start ^long end]
  (let [length (inc (- end start))]
    (cond
      (instance? File body)
      (let [in (java.io.FileInputStream. ^File body)]
        (skip-fully in start)
        (bounded-stream in length))

      (instance? InputStream body)
      (do (skip-fully body start)
          (bounded-stream body length))

      :else nil)))

;; ── Middleware ──────────────────────────────────────────────────────────────

(defn- content-length
  [response]
  (some-> (get-in response [:headers "Content-Length"]) str Long/parseLong))

(defn sliceable?
  "Only static resources get range treatment. They arrive as a File or an
   InputStream; a rendered page arrives as a string and is left alone, so
   ordinary responses gain neither the header nor the overhead."
  [body]
  (or (instance? File body) (instance? InputStream body)))

(defn- apply-range
  [request response]
  (let [total (content-length response)]
    (if (or (not= 200 (:status response))
            (nil? total)
            (not (sliceable? (:body response))))
      response
      (let [ranged (assoc-in response [:headers "Accept-Ranges"] "bytes")
            wanted (parse-range (get-in request [:headers "range"]) total)]
        (cond
          (nil? wanted) ranged

          (= :unsatisfiable wanted)
          (do (when (instance? InputStream (:body response))
                (.close ^InputStream (:body response)))
              {:status 416
               :headers {"Content-Range" (str "bytes */" total)
                         "Accept-Ranges" "bytes"}
               :body ""})

          :else
          (let [[start end] wanted]
            (if-let [body (slice (:body response) start end)]
              (-> ranged
                  (assoc :status 206 :body body)
                  (assoc-in [:headers "Content-Length"] (str (inc (- end start))))
                  (assoc-in [:headers "Content-Range"]
                            (str "bytes " start "-" end "/" total)))
              ranged)))))))

(defn wrap-range
  "Give a static-resource handler range support. Requests that carry no usable
   Range header pass straight through, only gaining an Accept-Ranges header."
  [handler]
  (fn
    ([request]
     (let [response (handler request)]
       (if (and response (= :get (:request-method request)))
         (apply-range request response)
         response)))
    ([request respond raise]
     (handler request
              (fn [response]
                (respond
                 (if (and response (= :get (:request-method request)))
                   (apply-range request response)
                   response)))
              raise))))
