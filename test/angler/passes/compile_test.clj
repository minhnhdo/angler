(ns angler.passes.compile-test
  (:require [clojure.test :refer :all]
            [angler.passes.compile :refer [free-vars]]
            [angler.passes.desugar :refer [desugar]]))

(deftest no-free-vars-literals
  (doseq [l [1 1.0 #{1 2 3} ["a"] {:a "a"}]]
    (testing (str "No free vars in " l)
      (is (= #{} (free-vars {} (desugar [l])))))))
