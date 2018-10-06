(ns angler.autodiff-test
  (:require [clojure.string :as s]
            [clojure.test :refer :all]
            [angler.autodiff :refer [autodiff]]
            [angler.types :refer [built-ins]]))

(defn abs
  [x]
  (if (< x 0)
    (- x)
    x))

(defn d=
  [^double x ^double y ^double e]
  (< (abs (- x y)) e))

(deftest normpdf
  (testing "normpdf"
    (let [[result {:syms [x mu sigma] :as m}]
          (autodiff '(fn [x mu sigma]
                       (* (/ 1 (sqrt (* 2 (* 3.141592653589793
                                             (* sigma sigma)))))
                          (exp (- 0
                                  (/ (* (- x mu) (- x mu))
                                     (* 2 (* sigma sigma)))))))
                    [0.5 0 1])]
      (is (d= result 0.352 0.0005))
      (is (d= x -0.176 0.0005))
      (is (d= mu 0.176 0.0005))
      (is (d= sigma -0.264 0.0005))
      (is (= 3 (count m))))))

(deftest marked-fns
  (testing "(fn [x] (exp (sin x)))"
    (letfn [(func [x] ((get built-ins 'exp) ((get built-ins 'sin) x)))
            (deriv [x] (* ((get built-ins 'cos) x)
                          ((get built-ins 'exp) ((get built-ins 'sin) x))))]
      (doseq [input (range (- Math/PI) Math/PI 0.314)]
        (let [[result {:syms [x] :as m}]
              (autodiff '(fn [x] (exp (sin x))) [input])]
          (is (= 1 (count m)))
          (is (d= (func input) result 0.000001))
          (is (d= (deriv input) x 0.000001))))))
  (testing "(fn [x] (+ (* x x) (sin x)))"
    (letfn [(func [x] (+ (* x x) ((get built-ins 'sin) x)))
            (deriv [x] (+ (* 2 x) ((get built-ins 'cos) x)))]
      (doseq [input (range (- Math/PI) Math/PI 0.314)]
        (let [[result {:syms [x] :as m}]
              (autodiff '(fn [x] (+ (* x x) (sin x))) [input])]
          (is (= 1 (count m)))
          (is (d= (func input) result 0.000001))
          (is (d= (deriv input) x 0.000001))))))
  (testing "(fn [x] (if (> x 5) (* x x) (+ x 18)))"
    (letfn [(func [x] (if (> x 5) (* x x) (+ x 18)))
            (deriv [x] (if (> x 5) (* 2 x) 1))]
      (doseq [input (range (- Math/PI) Math/PI 0.314)]
        (let [[result {:syms [x] :as m}]
              (autodiff '(fn [x] (if (> x 5) (* x x) (+ x 18))) [input])]
          (is (= 1 (count m)))
          (is (d= (func input) result 0.000001))
          (is (d= (deriv input) x 0.000001))))))
  (testing "(fn [x] (log x))"
    (letfn [(func [x] ((get built-ins 'log) x))
            (deriv [x] (/ 1 x))]
      (doseq [input (range 0.001 Math/PI 0.314)]
        (let [[result {:syms [x] :as m}]
              (autodiff '(fn [x] (log x)) [input])]
          (is (= 1 (count m)))
          (is (d= (func input) result 0.000001))
          (is (d= (deriv input) x 0.000001))))))
  #_(testing "(fn [x mu sigma] (+ (- 0 (/ (* (- x mu) (- x mu)) (* 2 (* sigma sigma)))) (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma)))))))"
      (letfn [(func [x mu sigma]
                (+ (- 0 (/ (* (- x mu) (- x mu))
                           (* 2 (* sigma sigma))))
                   (* (- 0 (/ 1 2))
                      ((get built-ins 'log) (* 2 (* 3.141592653589793
                                                    (* sigma sigma)))))))
              (dx [x mu sigma] (/ (mu - x) (* sigma sigma)))
              (dmu [x mu sigma] (/ (x - mu) (* sigma sigma)))
              (dsigma [x mu sigma]
                (let [x-mu (- x mu)
                      variance (* sigma sigma)]
                  (/ (- (* x-mu x-mu) variance) (* variance sigma))))]
        (doseq [ix (range (- Math/PI) Math/PI 0.314)
                imu [0 1]
                isigma [1 2]]
          (let [[result {:syms [x mu sigma] :as m}]
                (autodiff '(fn [x mu sigma]
                             (+ (- 0 (/ (* (- x mu) (- x mu))
                                        (* 2 (* sigma sigma))))
                                (* (- 0 (/ 1 2))
                                   (log (* 2 (* 3.141592653589793
                                                (* sigma sigma)))))))
                          [ix imu isigma])]
            (is (= 3 (count m)))
            (is (d= (func ix imu isigma) result 0.000001))
            (is (d= (dx ix imu isigma) x 0.000001))
            (is (d= (dmu ix imu isigma) mu 0.000001))
            (is (d= (dsigma ix imu isigma) sigma 0.000001))))))
  (testing "built-in normpdf"
    (let [[result {:syms [x mu sigma] :as m}]
          (autodiff '(fn [x mu sigma] (normpdf x mu sigma))
                    [0.5 0 1])]
      (is (d= 0.352 result 0.0005))
      (is (d= -0.176 x 0.0005))
      (is (d= 0.176 mu 0.0005))
      (is (d= -0.264 sigma 0.0005))
      (is (= 3 (count m))))))
