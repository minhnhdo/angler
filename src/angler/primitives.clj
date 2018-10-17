(ns angler.primitives
  (:require [clojure.core.matrix :as m]
            [anglican.core :refer :all]
            [anglican.runtime :refer :all])
  (:refer-clojure :exclude [loop map reduce filter keep keep-indexed remove
                            repeatedly every? not-any? some every-pred some-fn
                            comp juxt partial]))

(defn append
  [& args]
  (apply conj args))

(defn mat-mul
  [& args]
  (apply m/mmul args))

(defn mat-add
  [& args]
  (apply m/add args))

(defn mat-transpose
  [& args]
  (apply m/transpose args))

(defn mat-tanh
  [M]
  (m/emap tanh M))

(defn mat-relu
  [M]
  (m/emap (fn [x] (if (> x 0) x 0)) M))

(defn mat-repmat
  [M r c]
  (let [R (clojure.core/reduce (clojure.core/partial m/join-along 0)
                               (repeat r M))]
    (clojure.core/reduce (clojure.core/partial m/join-along 1) (repeat c R))))

(defdist dirac [x] []
  (sample* [this] x)
  (observe* [this value] (if (= value x)
                           0
                           Double/NEGATIVE_INFINITY)))
