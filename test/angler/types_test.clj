(ns angler.types-test
  (:require [clojure.test :refer :all]
            [angler.passes.desugar :refer [desugar]]
            [angler.types :refer [free-vars peval]]))

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

(deftest peval-literals
  (doseq [e [1 1.0 "a" :b]]
    (testing (str "peval " e " should be " e)
      (is (= e (peval e))))))

(deftest peval-simple-expressions
  (doseq [[e expected] {'(+ 1 1) 2
                        '(= 1 2) false
                        '(str "a" "b" "c" "d") "abcd"
                        '(if true 1 2) 1
                        '(conj (list 1 2 3) a) '(list a 1 2 3)
                        '(conj (vector 1 2 3) a) [1 2 3 'a]
                        '(conj (hash-set 1 2 3) a) #{1 2 3 'a}
                        '(conj (hash-map 1 2 3 4) [b c]) {1 2 3 4 'b 'c}
                        '(peek (list 1 2 3)) 1
                        '(peek (vector 1 2 3)) 3
                        '(nth (list 1 2 3) 0) 1
                        '(nth (vector 1 2 3) 0) 1
                        '(get (hash-map 1 (+ 4 5) 3 a) 1) 9
                        '(get (hash-map :a b :c d) :a) 'b}]
    (testing (str e " should evaluate to " expected)
      (is (= expected (peval e))))))

(deftest peval-nested-expressions
  (doseq [[e expected] {'(if true (+ 1 2) (- 3 4)) 3
                        '(if false
                           (conj (vector 3 4) 5)
                           (first (rest (list 1 2)))) 2
                        '(conj (if true (vector 1 2) (list 3 4))
                               (if false (+ 1 2) (- 3 4))) [1 2 -1]}]
    (testing (str e " should evaluate to " expected)
      (is (= expected (peval e))))))
