(ns angler.inference-test
  (:require [clojure.test :refer :all]
            [angler.test-utils :refer [d=]]
            [anglican.core :refer [doquery]]
            [anglican.emit :refer [defquery]]
            [anglican.runtime :refer [normal sqrt]]
            [anglican.stat :refer [collect-by empirical-mean]]
            [angler.inference :refer [p1 p2 p3 p4 p5 query]]))

(defquery anglican-p1 []
  (let [mu (sample (normal 1 (sqrt 5)))
        sigma (sqrt 2)
        lik (normal mu sigma)]
    (observe lik 8)
    (observe lik 9)
    mu))

(deftest program-1
  (testing "program-1"
    (let [reference (->> (doquery :rmh anglican-p1 []
                                  :number-of-particles 10000)
                         (take 1000)
                         (collect-by :result)
                         empirical-mean)]
      (is (d= reference
              (->> (query :gibbs p1)
                   (take 1000)
                   (collect-by :result)
                   empirical-mean)
              (* 0.05 reference))))))
