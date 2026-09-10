(ns organism.handler-test
  (:require
    [clojure.test :refer :all]
    [clojure.string]
    [ring.mock.request :refer :all]
    [organism.handler :refer :all]
    [organism.middleware.formats :as formats]
    [muuntaja.core :as m]
    [mount.core :as mount]))

(defn parse-json [body]
  (m/decode formats/instance "application/json" body))

(use-fixtures
  :once
  (fn [f]
    (mount/start #'organism.config/env
                 #'organism.handler/app-routes)
    (f)))

(deftest test-app
  (testing "root redirects to the organism landing page"
    ;; the multi-game catalog is no longer surfaced — see routes.home/root-redirect
    (let [response ((app) (request :get "/"))]
      (is (= 302 (:status response)))
      ;; ring absolutizes the Location against the request host
      (is (clojure.string/ends-with? (get-in response [:headers "Location"])
                                     "/organism"))))

  (testing "and that landing page renders"
    (let [response ((app) (request :get "/organism"))]
      (is (= 200 (:status response)))))

  (testing "not-found route"
    (let [response ((app) (request :get "/invalid"))]
      (is (= 404 (:status response))))))
