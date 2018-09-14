(ns angler.types
  (:require [clojure.set :refer [union]]))

(def pmf
  {'bernoulli 'p_bernoulli
   'beta 'p_beta
   'exp 'p_exp
   'normal 'p_normal})

(def distributions (set (keys pmf)))

(defrecord Graph
  [^clojure.lang.IPersistentSet V
   ^clojure.lang.IPersistentSet A
   ^clojure.lang.IPersistentMap P
   ^clojure.lang.IPersistentMap Y])

(defn graph?
  [obj]
  (instance? Graph obj))

(defn new-graph
  ^Graph
  [^clojure.lang.IPersistentSet V
   ^clojure.lang.IPersistentSet A
   ^clojure.lang.IPersistentMap P
   ^clojure.lang.IPersistentMap Y]
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
