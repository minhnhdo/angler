(ns angler.inference-test
  (:require [clojure.test :refer :all]
            [angler.test-utils :refer [abs-no-branching d=]]
            [anglican.core :refer [doquery]]
            [anglican.emit :refer [defm defquery]]
            [anglican.runtime :refer [normal sqrt]]
            [anglican.stat :refer [collect-by empirical-mean]]
            [angler.inference :refer [p1 p2 p3 p4 p5 query]]))

(defn angler-query
  [algorithm program nsamples]
  (->> (query algorithm program)
       (take nsamples)
       (collect-by :result)
       empirical-mean))

(defn anglican-query
  [algorithm program nsamples]
  (->> (doquery algorithm program []
                :number-of-particles 10000)
       (take nsamples)
       (collect-by :result)
       empirical-mean))

(defquery anglican-p1 []
  (let [mu (sample (normal 1 (sqrt 5)))
        sigma (sqrt 2)
        lik (normal mu sigma)]
    (observe lik 8)
    (observe lik 9)
    mu))

(defm observe-data [_ data slope bias]
  (let [xn (first data)
        yn (second data)
        zn (+ (* slope xn) bias)]
    (observe (normal zn 1.0) yn)
    (rest (rest data))))

(defquery anglican-p2 []
  (let [slope (sample (normal 0.0 10.0))
        bias  (sample (normal 0.0 10.0))
        data (vector 1.0 2.1 2.0 3.9 3.0 5.3
                     4.0 7.7 5.0 10.2 6.0 12.9)]
    (loop [d data]
      (when (seq d)
        (recur (observe-data nil d slope bias))))
    (vector slope bias)))

(deftest program-1
  (testing "program 1 with Gibbs sampling"
    (let [reference (anglican-query :rmh anglican-p1 1000)]
      (is (d= reference
              (angler-query :gibbs p1 1000)
              (abs-no-branching (* 0.05 reference)))))))

(deftest program-2
  (testing "program 2 with Gibbs sampling"
    (let [[ref-slope ref-bias] (anglican-query :rmh anglican-p2 1000)
          [slope bias] (angler-query :gibbs p2 1000)]
      (is (d= ref-slope slope (abs-no-branching (* 0.05 ref-slope))))
      (is (d= ref-bias bias (abs-no-branching (* 0.05 ref-bias)))))))
