(ns organism.color-test
  "Player colours are stored as hsl(...) strings, and an <input type=\"color\">
   accepts only #rrggbb."
  (:require
   [clojure.test :refer [deftest testing is]]
   [organism.base :as base]))

(deftest hsl-becomes-hex
  (testing "the corners of the wheel"
    (is (= "#ff0000" (base/hsl->hex 0 1.0 0.5)))
    (is (= "#00ff00" (base/hsl->hex 120 1.0 0.5)))
    (is (= "#0000ff" (base/hsl->hex 240 1.0 0.5))))
  (testing "greys have no hue"
    (is (= "#000000" (base/hsl->hex 210 0.5 0.0)))
    (is (= "#ffffff" (base/hsl->hex 210 0.5 1.0)))
    (is (= "#808080" (base/hsl->hex 210 0.0 0.5)))))

(deftest css-colours-become-hex
  (testing "the shapes actually found in the players collection"
    (is (= "#6dd0d0" (base/color->hex "hsl(180,51%,62%)")))
    (is (= "#2d690c" (base/color->hex "hsl(99,79%,23%)"))))
  (testing "hex passes through, normalised"
    (is (= "#aabbcc" (base/color->hex "#AABBCC")))
    (is (= "#aabbcc" (base/color->hex "#abc"))))
  (testing "hsla and loose spacing"
    (is (= "#6dd0d0" (base/color->hex "hsl( 180 , 51% , 62% )")))
    (is (= "#6dd0d0" (base/color->hex "hsla(180,51%,62%,1.0)"))))
  (testing "anything else falls back so the picker still opens"
    (is (= "#445566" (base/color->hex nil)))
    (is (= "#445566" (base/color->hex "rebeccapurple")))
    (is (= "#123456" (base/color->hex "" "#123456")))))
