(ns angler.types
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [clojure.string :refer [ends-with? starts-with?]]
            [anglican core runtime])
  (:import (clojure.lang IPersistentSet IPersistentMap)))

(def pmf
  (->> (ns-publics 'anglican.runtime)
       (map #(let [[k v] %]
               [(name k) v]))
       (filter #(let [[k v] %]
                  (and (starts-with? k "->")
                       (ends-with? k "-distribution"))))
       (map #(let [[k v] %
                   l (count k)]
               [(symbol (subs k 2 (- l 13))) v]))
       (into {})))

(def built-ins
  (merge {'if 'if
          'loop 'loop
          'observe 'observe
          'observe* 'observe*
          'sample 'sample}
         {'append (fn [& vectors]
                    (apply into [] vectors))} ; fix for missing append
         pmf
         (ns-publics 'anglican.core)
         (ns-publics 'anglican.runtime)
         (ns-publics 'clojure.core)))

(defrecord Graph
  [^IPersistentSet V
   ^IPersistentSet A
   ^IPersistentMap P
   ^IPersistentMap Y])

(defn graph?
  [obj]
  (instance? Graph obj))

(defn new-graph
  ^Graph
  [^IPersistentSet V
   ^IPersistentSet A
   ^IPersistentMap P
   ^IPersistentMap Y]
  (Graph. V A P Y))

(defn empty-graph
  ^Graph
  []
  (Graph. #{} #{} {} {}))

(defn join-graph
  ^Graph

  ([] (empty-graph))

  ([^Graph graph] graph)

  ([^Graph graph1 ^Graph graph2]
   (Graph. (union (:V graph1) (:V graph2))
           (union (:A graph1) (:A graph2))
           (merge (:P graph1) (:P graph2))
           (merge (:Y graph1) (:Y graph2))))

  ([^Graph graph1 ^Graph graph2 & graphs]
   (reduce join-graph (join-graph graph1 graph2) graphs)))

(defn count-vertices
  [^Graph graph]
  (count (:V graph)))

(defn count-edges
  [^Graph graph]
  (count (:A graph)))

(defn print-graph
  [^Graph graph]
  (pprint graph))
