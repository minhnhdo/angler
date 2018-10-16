(ns angler.inference-test
  (:require [clojure.test :refer :all]
            [angler.test-utils :refer [abs-no-branching d=]]
            [anglican.core :refer [doquery]]
            [anglican.emit :refer [defm defquery with-primitive-procedures]]
            [anglican.runtime :refer [dirichlet discrete flip gamma mean normal
                                      sqrt std]]
            [anglican.stat :refer [collect-by empirical-mean]]
            [angler.errors :refer [debug]]
            [angler.primitives :refer [dirac]]
            [angler.inference :refer [p1 p2 p3 p4 p5 query]]))

(defn angler-query
  [algorithm program nsamples & options]
  (->> (apply query algorithm program options)
       (take nsamples)))

(defn anglican-query
  [algorithm program nsamples & options]
  (let [burn-in (or ((apply hash-map options) :burn-in) 10000)]
    (->> (apply doquery algorithm program [] options)
         (map :result)
         (drop burn-in)
         (take nsamples))))

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

(defn d=5%
  [^double reference ^double result]
  (d= reference result (abs-no-branching (* 0.05 reference))))

(deftest program-1
  (testing "program 1 with Gibbs sampling"
    (let [reference (anglican-query :smc anglican-p1 200000
                                    :burn-in 10000
                                    :number-of-particles 10000)
          result (angler-query :gibbs p1 200000 :burn-in 10000)
          r-mean (mean reference)
          r-std (std reference)
          m (mean result)
          s (std result)]
      (is (d=5% r-mean m))
      (is (d=5% r-std s)))))

(deftest program-2
  (testing "program 2 with Gibbs sampling"
    (let [reference (anglican-query :smc anglican-p2 200000
                                    :burn-in 10000
                                    :number-of-particles 10000)
          result (angler-query :gibbs p2 200000 :burn-in 10000)
          r-slope (map first reference)
          r-bias (map second reference)
          r-slope-mean (mean r-slope)
          r-slope-std (std r-slope)
          r-bias-mean (mean r-bias)
          r-bias-std (std r-bias)
          slope (map first result)
          bias (map second result)
          slope-mean (mean slope)
          slope-std (std slope)
          bias-mean (mean bias)
          bias-std (std bias)]
      (is (d=5% r-slope-mean slope-mean))
      (is (d=5% r-slope-std slope-std))
      (is (d=5% r-bias-mean bias-mean))
      (is (d=5% r-bias-std bias-std)))))

#_(deftest program-3
  (testing "program 3 with Gibbs sampling"
    (let [reference (anglican-query :smc anglican-p3 1000)
          result (angler-query :gibbs p3 1000)]
      (println reference)
      (println result)
      #_(is (d= reference result (abs-no-branching (* 0.05 reference)))))))

#_(deftest program-4
    (testing "program 4 with Gibbs sampling"
      (let [reference ((anglican-query :smc anglican-p2 1000) true)
            result ((angler-query :gibbs p2 1000) true)]
        (is (d= reference result (abs-no-branching (* 0.05 reference)))))))

#_(deftest program-5
    (testing "program 5 with Gibbs sampling"
      (let [reference ((anglican-query :smc anglican-p5 1000) true)
            result ((angler-query :gibbs p5 1000) true)]
        (is (d= reference result (abs-no-branching (* 0.05 reference)))))))
