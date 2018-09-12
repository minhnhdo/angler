(ns angler.passes.desugar-test
  (:require [clojure.test :refer :all]
            [angler.passes.desugar :refer :all]))

(deftest desugared-vector
  (testing "Desugared vector"
    (is (= (desugar [[1 2 3]])
           '[(vector 1 2 3)]))))

(deftest desugared-map
  (testing "Desugared map"
    (is (= (desugar '[{a 1, b 2, c 3}])
           '[(hash-map [a 1] [b 2] [c 3])]))))
