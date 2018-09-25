(ns angler.types
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [clojure.string :refer [ends-with? starts-with?]]
            [anglican core runtime]
            [angler.errors :refer [graph-error]]
            angler.primitives)
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
  (dissoc (merge (ns-publics 'clojure.core)
                 (ns-publics 'anglican.core)
                 (ns-publics 'anglican.runtime)
                 (ns-publics 'angler.primitives)
                 {'if 'if
                  'loop 'loop
                  'observe 'observe
                  'observe* 'observe*
                  'sample 'sample}
                 pmf)
          'map 'reduce
          'filter 'keep 'keep-indexed 'remove
          'repeatedly
          'every? 'not-any? 'some
          'every-pred 'some-fn
          'comp 'juxt 'partial))

(defn- value?
  [exp]
  (cond
    (symbol? exp) false
    (and (list? exp) (seq exp)) (= 'list (first exp))
    :else true))

(defn- list-literal?
  [exp]
  (and (list? exp) (= 'list (first exp))))

(defn peval
  [exp]
  (if (and (list? exp) (seq exp))
    (let [[op & params] (map peval exp)]
      (cond
        (or (not (symbol? op)) (= 'list op)) (apply list op params)
        (contains? #{'hash-map 'hash-set 'vector} op)
        (apply (built-ins op) params)
        (= 'if op)
        (let [[cond-exp then-exp else-exp] params]
          (cond
            (not (value? cond-exp)) (list 'if cond-exp then-exp else-exp)
            cond-exp (peval then-exp)
            :else (peval else-exp)))

        ; list operations
        (and (= 'count op) (list-literal? (first params)) (= 1 (count params)))
        (- (count (first params)) 1)

        (and (= 'peek op) (list-literal? (first params)) (= 1 (count params)))
        (let [[[_ f]] params] f)

        (and (= 'pop op) (list-literal? (first params)) (= 1 (count params)))
        (apply list 'list (rest (rest (first params))))

        (and (= 'list? op) (= 1 (count params)))
        (let [[f] params]
          (and (list? f) (= 'list (first f))))

        (and (= 'conj op) (list-literal? (first params)))
        (let [[[_ & elems] & added] params]
          (apply list 'list (into elems added)))

        ; TODO list*

        ;; sequence operations on lists

        (and (= 'first op) (list-literal? (first params)) (= 1 (count params)))
        (let [[[_ f]] params] f)

        (and (= 'second op) (list-literal? (first params)) (= 1 (count params)))
        (let [[[_ _ s]] params] s)

        (and (= 'rest op) (list-literal? (first params)) (= 1 (count params)))
        (let [[[_ _ & elems]] params] (apply list 'list elems))

        ; last is correctly implemented by default

        (and (= 'cons op) (list-literal? (second params)) (= 2 (count params)))
        (let [[elem [_ & elems]] params] (apply list 'list elem elems))

        (and (= 'nth op)
             (list-literal? (first params))
             (= 2 (count params))
             (int? (second params))
             (<= 0 (second params) (- (count (first params)) 2)))
        (nth (first params) (+ 1 (second params)))

        (and (= 'get op)
             (list-literal? (first params))
             (= 2 (count params))
             (int? (second params)))
        (get (first params) (+ 1 (second params)))

        ; try to perform primitive calls
        :else (let [resolved-op (built-ins op)]
                (cond
                  (and (contains? #{'conj 'cons 'first 'rest 'last} op)
                       (value? (first params)))
                  (apply resolved-op params)
                  (or (nil? resolved-op) (not (every? value? params)))
                  (apply list op params)
                  :else (apply resolved-op params)))))
    exp))

(defn edge-vector->adjacency-vector
  [edges]
  (loop [result {}
         es edges]
    (if (seq es)
      (let [[v1 v2] (peek es)]
        (recur (assoc result v1 (conj (vec (result v1)) v2))
               (pop es)))
      result)))

(defn topological-sort
  [vertices adjacency-vector]
  (loop [to-visit (mapv #(vector #{} % (adjacency-vector %)) vertices)
         visited #{}
         result (list)]
    (if (empty? to-visit)
      result
      (let [[temporary v neighbors] (peek to-visit)
            new-to-visit (pop to-visit)]
        (cond
          (contains? visited v) (recur new-to-visit
                                       visited
                                       result)
          (empty? neighbors) (recur new-to-visit
                                    (conj visited v)
                                    (conj result v))
          (contains? temporary v) (graph-error "Not a DAG "
                                               vertices " "
                                               adjacency-vector)
          :else (let [new-neighbors (pop neighbors)
                      m (peek neighbors)]
                  (recur (into new-to-visit
                               [[temporary v new-neighbors]
                                [(conj temporary v)
                                 m
                                 (vec (filter #(not (contains? visited %))
                                              (adjacency-vector m)))]])
                         visited
                         result)))))))

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

(defn ancestral-ordering
  [^Graph {:keys [V A]}]
  (topological-sort V (edge-vector->adjacency-vector (vec A))))

(defn bind-free-variables
  [^IPersistentMap sub exp]
  (cond
    (seq? exp) (apply (cond
                        (list? exp) list
                        (vector? exp) vector
                        (map? exp) hash-map
                        (set? exp) hash-set
                        :else list)
                      (map #(bind-free-variables sub %) exp))
    (list? exp) (apply list (map #(bind-free-variables sub %) exp))
    (vector? exp) (apply vector (map #(bind-free-variables sub %) exp))
    (contains? sub exp) (sub exp)
    :else exp))

(defn sample-from-joint
  [^Graph {:keys [P Y] :as graph}]
  (loop [m {}
         to-do (ancestral-ordering graph)]
    (if (seq to-do)
      (let [[v & new-to-do] to-do
            sampled-value (let [[_ dist _] (P v)]
                            (anglican.runtime/sample*
                              (if (satisfies? anglican.runtime/distribution dist)
                                dist
                                (peval (bind-free-variables m dist)))))]
        (recur (assoc m v sampled-value)
               new-to-do))
      m)))
