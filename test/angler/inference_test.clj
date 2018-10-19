(ns angler.inference-test
  (:require [clojure.test :refer :all]
            [angler.test-utils :refer [d=5%]]
            [anglican.core :refer [doquery]]
            [anglican.emit :refer [defm defquery with-primitive-procedures]]
            [anglican.runtime :refer [dirichlet discrete flip gamma mean normal
                                      sqrt std variance]]
            [angler.primitives :refer [dirac]]
            [angler.inference :refer [p1 p2 p3 p4 p5 query]]))

(def number-of-samples 100000)

(def burn-in 10000)

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
                   (observe (nth likes z) y)
                   z))
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

(deftest gibbs-program-1
  (testing "program 1 with Gibbs sampling"
    (println "running program 1 with Gibbs sampling")
    (let [reference (anglican-query :lmh anglican-p1 number-of-samples
                                    :burn-in burn-in)
          result (angler-query :gibbs p1 number-of-samples :burn-in burn-in)
          r-mean (mean reference)
          r-std (std reference)
          m (mean result)
          s (std result)]
      (println "program 1")
      (println "reference" r-mean r-std)
      (println "result" m s)
      (is (d=5% r-mean m))
      (is (d=5% r-std s)))))

(deftest gibbs-program-2
  (testing "program 2 with Gibbs sampling"
    (println "running program 2 with Gibbs sampling")
    (let [reference (anglican-query :lmh anglican-p2 number-of-samples
                                    :burn-in burn-in)
          result (angler-query :gibbs p2 number-of-samples :burn-in burn-in)
          r-slope-mean (mean (map first reference))
          r-slope-std (std (map first reference))
          r-bias-mean (mean (map second reference))
          r-bias-std (std (map second reference))
          slope-mean (mean (map first result))
          slope-std (std (map first result))
          bias-mean (mean (map second result))
          bias-std (std (map second result))]
      (println "program 2")
      (println "reference slope" r-slope-mean r-slope-std)
      (println "reference bias" r-bias-mean r-bias-std)
      (println "result slope" slope-mean slope-std)
      (println "result bias" bias-mean bias-std)
      (is (d=5% r-slope-mean slope-mean))
      (is (d=5% r-slope-std slope-std))
      (is (d=5% r-bias-mean bias-mean))
      (is (d=5% r-bias-std bias-std)))))

(deftest gibbs-program-3
  (testing "program 3 with Gibbs sampling"
    (println "running program 3 with Gibbs sampling")
    (let [reference (map #(if % 1.0 0.0)
                         (anglican-query :lmh anglican-p3 number-of-samples
                                         :burn-in burn-in))
          result (map #(if % 1.0 0.0)
                      (angler-query :gibbs p3 number-of-samples
                                    :burn-in burn-in))
          r-mean (mean reference)
          r-std (std reference)
          m (mean result)
          s (std result)]
      (println "program 3")
      (println "reference" r-mean r-std)
      (println "result" m s)
      (is (d=5% r-mean m))
      (is (d=5% r-std s)))))

(deftest gibbs-program-4
  (testing "program 4 with Gibbs sampling"
    (println "running program 4 with Gibbs sampling")
    (let [reference (map #(if % 1.0 0.0)
                         (anglican-query :lmh anglican-p4 number-of-samples
                                         :burn-in burn-in))
          result (map #(if % 1.0 0.0)
                      (angler-query :gibbs p4 number-of-samples
                                    :burn-in burn-in))
          r-mean (mean reference)
          r-std (std reference)
          m (mean result)
          s (std result)]
      (println "program 4")
      (println "reference" r-mean r-std)
      (println "result" m s)
      (is (d=5% r-mean m))
      (is (d=5% r-std s)))))

(deftest gibbs-program-5
  (testing "program 5 with Gibbs sampling"
    (println "running program 5 with Gibbs sampling")
    (let [;reference (anglican-query :lmh anglican-p5 number-of-samples
          ;                          :burn-in burn-in)
          result (angler-query :gibbs p5 number-of-samples :burn-in burn-in)
          ;r-x-mean (mean (map first reference))
          ;r-x-variance (variance (map first reference))
          ;r-y-mean (mean (map second reference))
          ;r-y-variance (variance (map second reference))
          x-mean (mean (map first result))
          x-variance (variance (map first result))
          y-mean (mean (map second result))
          y-variance (variance (map second result))]
      (println "program 5")
      ;(println "reference x ~ (" r-x-mean "," r-x-variance ") y ~("
      ;         r-y-mean "," r-y-variance ")")
      (println "result x ~ (" x-mean "," x-variance ") y ~("
               y-mean "," y-variance ")")
      ;(is (d=5% r-x-mean x-mean))
      ;(is (d=5% r-x-variance x-variance))
      ;(is (d=5% r-y-mean y-mean))
      ;(is (d=5% r-y-variance y-variance))
      )))

(deftest hmc-program-1
  (testing "program 1 with HMC"
    (println "running program 1 with HMC")
    (let [reference (anglican-query :lmh anglican-p1 number-of-samples
                                        :burn-in burn-in)
          result (angler-query :hmc p1 number-of-samples :burn-in burn-in)
          r-mean (mean reference)
          r-std (std reference)
          m (mean result)
          s (std result)]
      (println "program 1")
      (println "reference" r-mean r-std)
      (println "result" m s)
      (is (d=5% r-mean m))
      (is (d=5% r-std s)))))
