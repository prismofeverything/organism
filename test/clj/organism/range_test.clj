(ns organism.range-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [organism.middleware.range :as range])
  (:import
   [java.io ByteArrayInputStream File]))

(def total 1000)

(defn- parse [header] (range/parse-range header total))

(deftest parse-range-test
  (testing "the ordinary forms a browser sends"
    (is (= [0 99]    (parse "bytes=0-99")))
    (is (= [100 999] (parse "bytes=100-"))   "open ended — the rest of the file")
    (is (= [500 999] (parse "bytes=-500"))   "suffix — the trailing bytes")
    (is (= [0 999]   (parse "bytes=0-"))     "chrome opens with the whole file"))

  (testing "clamped to what actually exists"
    (is (= [500 999] (parse "bytes=500-99999")))
    (is (= [0 999]   (parse "bytes=-2000")) "suffix longer than the file"))

  (testing "past the end is unsatisfiable, not empty"
    (is (= :unsatisfiable (parse "bytes=1000-")))
    (is (= :unsatisfiable (parse "bytes=1500-1600")))
    (is (= :unsatisfiable (parse "bytes=-0"))))

  (testing "anything we don't speak means send the whole thing"
    (is (nil? (parse nil)))
    (is (nil? (parse "")))
    (is (nil? (parse "garbage")))
    (is (nil? (parse "items=0-99"))        "units other than bytes")
    (is (nil? (parse "bytes=0-99,200-299")) "multi-range needs a multipart body")
    (is (nil? (parse "bytes=-")))
    (is (nil? (range/parse-range "bytes=0-99" 0)) "empty resource"))

  (testing "case and whitespace"
    (is (= [0 9] (parse "BYTES=0-9")))
    (is (= [0 9] (parse "bytes = 0 - 9")))))

;; ── Slicing ─────────────────────────────────────────────────────────────────

(def payload (byte-array (map byte (range 0 100))))

(defn- drain [stream]
  (with-open [in stream out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (vec (.toByteArray out))))

(deftest bounded-stream-test
  (testing "stops at the limit"
    (is (= (vec (take 10 payload))
           (drain (range/bounded-stream (ByteArrayInputStream. payload) 10)))))

  (testing "a limit past the end just yields what is there"
    (is (= (vec payload)
           (drain (range/bounded-stream (ByteArrayInputStream. payload) 500)))))

  (testing "byte at a time reads honour the limit too"
    (let [in (range/bounded-stream (ByteArrayInputStream. payload) 3)]
      (is (= [0 1 2 -1] [(.read in) (.read in) (.read in) (.read in)])))))

;; ── Middleware ──────────────────────────────────────────────────────────────

(defn- static-handler
  "Stands in for the resource handler: 200 with a body and a Content-Length."
  [body]
  (fn [_request]
    {:status 200
     :headers {"Content-Length" (str (count payload)) "Content-Type" "video/mp4"}
     :body body}))

(defn- fetch [range-header body]
  (let [handler (range/wrap-range (static-handler body))]
    (handler (cond-> {:request-method :get :uri "/video/x.mp4" :headers {}}
               range-header (assoc :headers {"range" range-header})))))

(deftest wrap-range-test
  (testing "a plain request is untouched apart from advertising range support"
    (let [response (fetch nil (ByteArrayInputStream. payload))]
      (is (= 200 (:status response)))
      (is (= "bytes" (get-in response [:headers "Accept-Ranges"])))
      (is (= (vec payload) (drain (:body response))))))

  (testing "a range request gets exactly those bytes"
    (let [response (fetch "bytes=10-19" (ByteArrayInputStream. payload))]
      (is (= 206 (:status response)))
      (is (= "bytes 10-19/100" (get-in response [:headers "Content-Range"])))
      (is (= "10" (get-in response [:headers "Content-Length"])))
      (is (= (vec (subvec (vec payload) 10 20)) (drain (:body response))))))

  (testing "seeking to the tail works — this is what scrubbing a video does"
    (let [response (fetch "bytes=90-" (ByteArrayInputStream. payload))]
      (is (= 206 (:status response)))
      (is (= "bytes 90-99/100" (get-in response [:headers "Content-Range"])))
      (is (= (vec (subvec (vec payload) 90 100)) (drain (:body response))))))

  (testing "a File body seeks the same way"
    (let [file (File/createTempFile "range" ".bin")]
      (io/copy payload file)
      (.deleteOnExit file)
      (let [response (fetch "bytes=40-49" file)]
        (is (= 206 (:status response)))
        (is (= (vec (subvec (vec payload) 40 50)) (drain (:body response)))))))

  (testing "past the end is 416, and says how big the resource really is"
    (let [response (fetch "bytes=500-600" (ByteArrayInputStream. payload))]
      (is (= 416 (:status response)))
      (is (= "bytes */100" (get-in response [:headers "Content-Range"])))))

  (testing "a range we decline falls back to the whole resource"
    (let [response (fetch "bytes=0-9,20-29" (ByteArrayInputStream. payload))]
      (is (= 200 (:status response)))
      (is (= (vec payload) (drain (:body response))))))

  (testing "only GET is ranged"
    (let [handler (range/wrap-range (static-handler (ByteArrayInputStream. payload)))
          response (handler {:request-method :post :uri "/video/x.mp4"
                             :headers {"range" "bytes=0-9"}})]
      (is (= 200 (:status response)))))

  (testing "a 404 from the handler underneath passes through"
    (let [handler (range/wrap-range (constantly {:status 404 :headers {} :body "nope"}))
          response (handler {:request-method :get :uri "/missing" :headers {"range" "bytes=0-9"}})]
      (is (= 404 (:status response))))))
