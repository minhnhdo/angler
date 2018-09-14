(ns angler.passes.compile-test
  (:require [clojure.test :refer :all]
            [angler.passes.compile :refer [free-vars]]
            [angler.passes.desugar :refer [desugar]]))

(deftest no-free-vars-literals
  (doseq [l [1 1.0 #{1 2 3} ["a"] {:a "a"}]]
    (testing (str "No free vars in " l)
      (is (= #{} (free-vars {} (nth (desugar [l]) 0)))))))

(deftest no-free-vars-let
  (doseq [e ['(let [v 1] v) '(let [a :a b :b] (+ a b))]]
    (testing (str "No free vars in " e)
      (is (= #{} (free-vars {} (nth (desugar [e]) 0)))))))

(deftest some-free-vars
  (doseq [[e expected] {'a #{'a}
                        {:a 'a, :b 'b} #{'a 'b}
                        '(let [v 1] (+ a b c v)) #{'a 'b 'c}
                        '(let [a 1] (let [b 2] (+ a b c d))) #{'c 'd}}]
    (testing (str "No free vars in " e)
      (is (= expected (free-vars {} (nth (desugar [e]) 0)))))))
