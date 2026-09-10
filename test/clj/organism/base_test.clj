(ns organism.base-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.base :as base]))

(deftest join-path-is-indifferent-to-the-trailing-slash
  (testing "the observe page passed a prefix with no slash and got /playeralice"
    (is (= "/organism/player/alice" (base/join-path "/organism/player" "alice"))))
  (testing "and the games list passes one that already has it"
    (is (= "/organism/player/alice" (base/join-path "/organism/player/" "alice"))))
  (testing "however many it has"
    (is (= "/organism/player/alice" (base/join-path "/organism/player///" "alice"))))
  (testing "the segment is left exactly as given, already encoded by the caller"
    (is (= "/organism/play/a%20b" (base/join-path "/organism/play/" "a%20b")))))
