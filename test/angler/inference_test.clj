(ns angler.inference-test
  (:require [clojure.test :refer :all]
            [angler.test-utils :refer [abs-no-branching d=]]
            [anglican.core :refer [doquery]]
            [anglican.emit :refer [defm defquery with-primitive-procedures]]
            [anglican.runtime :refer [dirichlet discrete flip gamma normal
                                      sqrt]]
            [anglican.stat :refer [collect-by empirical-mean]]
            [angler.primitives :refer [dirac]]
            [angler.inference :refer [p1 p2 p3 p4 p5 query]]))

(defn angler-query
  [algorithm program nsamples]
  (->> (query algorithm program)
       (take nsamples)
       (collect-by :result)))

(defn anglican-query
  [algorithm program nsamples]
  (->> (doquery algorithm program []
                :number-of-particles 10000)
       (take nsamples)
       (collect-by :result)))

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

(defquery anglican-p3 []
  (let [data [1.1 2.1 2.0 1.9 0.0 -0.1 -0.05]
        likes (repeatedly 3 #(let [mu (sample (normal 0.0 10.0))
                                   sigma (sample (gamma 1.0 1.0))]
                               (normal mu sigma)))
        pi (sample (dirichlet [1.0 1.0 1.0]))
        z-prior (discrete pi)
        z (map (fn [y]
                 (let [z (sample z-prior)]
                   (observe (nth likes z) y)))
               data)]
    (= (first z) (second z))))

(defquery anglican-p4 []
  (let [sprinkler true
        wet-grass true
        is-cloudy (sample (flip 0.5))
        is-raining (if (= is-cloudy true)
                     (sample (flip 0.8))
                     (sample (flip 0.2)))
        sprinkler-dist (if (= is-cloudy true)
                         (flip 0.1)
                         (flip 0.5))
        wet-grass-dist (if (and (= sprinkler true)
                                (= is-raining true))
                         (flip 0.99)
                         (if (and (= sprinkler false)
                                  (= is-raining false))
                           (flip 0.0)
                           (if (or (= sprinkler true)
                                   (= is-raining true))
                             (flip 0.9))))]
    (observe sprinkler-dist sprinkler)
    (observe wet-grass-dist wet-grass)
    is-raining))

(with-primitive-procedures [dirac]
  (defquery anglican-p5 []
    (let [x (sample (normal 0 10))
          y (sample (normal 0 10))]
      (observe (dirac (+ x y)) 7)
      [x y])))

(deftest program-1
  (testing "program 1 with Gibbs sampling"
    (let [reference (empirical-mean (anglican-query :rmh anglican-p1 1000))
          result (empirical-mean (angler-query :gibbs p1 1000))]
      (is (d= reference result (abs-no-branching (* 0.05 reference)))))))

(deftest program-2
  (testing "program 2 with Gibbs sampling"
    (let [[ref-slope ref-bias] (empirical-mean
                                 (anglican-query :rmh anglican-p2 1000))
          [slope bias] (empirical-mean (angler-query :gibbs p2 1000))]
      (is (d= ref-slope slope (abs-no-branching (* 0.05 ref-slope))))
      (is (d= ref-bias bias (abs-no-branching (* 0.05 ref-bias)))))))

#_(deftest program-4
    (testing "program 4 with Gibbs sampling"
      (let [reference ((anglican-query :rmh anglican-p2 1000) true)
            result ((angler-query :gibbs p2 1000) true)]
        (is (d= reference result (abs-no-branching (* 0.05 reference)))))))

#_(deftest program-5
    (testing "program 5 with Gibbs sampling"
      (let [reference ((anglican-query :rmh anglican-p5 1000) true)
            result ((angler-query :gibbs p5 1000) true)]
        (is (d= reference result (abs-no-branching (* 0.05 reference)))))))
