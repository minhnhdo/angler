(ns angler.types
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [clojure.string :refer [ends-with? starts-with?]]
            [anglican core runtime]
            [angler.errors :refer [graph-error]]
            angler.primitives)
  (:import [clojure.lang IPersistentList IPersistentMap IPersistentSet
            IPersistentVector]))

(def distributions
  (->> (ns-publics 'anglican.runtime)
       (map #(name (nth % 0)))
       (filter #(and (starts-with? % "->") (ends-with? % "-distribution")))
       (map #(let [l (count %)] (symbol (subs % 2 (- l 13)))))
       (into #{})))

(def built-ins
  (dissoc (merge (ns-publics 'clojure.core)
                 (ns-publics 'anglican.core)
                 (ns-publics 'anglican.runtime)
                 (ns-publics 'angler.primitives))
          'loop
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
    (seqable? exp) (every? value? exp)
    :else true))

(defn- list-literal?
  [exp]
  (and (list? exp) (= 'list (first exp))))

(defn peval-once
  ^IPersistentVector
  [exp]
  (cond
    (and (list? exp) (seq exp))
    (let [peval-results (map peval-once exp)
          [op & params] (map #(nth % 0) peval-results)
          changed (boolean (some identity (map #(nth % 1) peval-results)))]
      (cond
        (or (not (symbol? op)) (= 'list op)) [(apply list op params) changed]

        (contains? #{'hash-map 'hash-set 'vector} op)
        [(apply (built-ins op) params) true]

        (= 'if op)
        (let [[cond-exp then-exp else-exp] params]
          (cond
            (not (value? cond-exp)) [(list 'if cond-exp then-exp else-exp)
                                     changed]
            cond-exp (peval-once then-exp)
            :else (peval-once else-exp)))

        (= 'or op) (if (some true? params)
                     [true true]
                     [(apply list 'or params) changed])

        (= 'and op) (if (some false? params)
                      [false true]
                      [(apply list 'and params) changed])

        ; list operations
        (and (= 'count op) (list-literal? (first params)) (= 1 (count params)))
        [(- (count (first params)) 1) true]

        (and (= 'peek op) (list-literal? (first params)) (= 1 (count params)))
        [(let [[[_ f]] params] f) true]

        (and (= 'pop op) (list-literal? (first params)) (= 1 (count params)))
        [(apply list 'list (rest (rest (first params)))) true]

        (and (= 'list? op) (= 1 (count params)))
        [(let [[f] params] (and (list? f) (= 'list (first f)))) true]

        (and (= 'conj op) (list-literal? (first params)))
        [(let [[[_ & elems] & added] params]
           (apply list 'list (into elems added))) true]

        ; TODO list*

        ;; sequence operations on lists

        (and (= 'first op) (list-literal? (first params)) (= 1 (count params)))
        [(let [[[_ f]] params] f) true]

        (and (= 'second op) (list-literal? (first params)) (= 1 (count params)))
        [(let [[[_ _ s]] params] s) true]

        (and (= 'rest op) (list-literal? (first params)) (= 1 (count params)))
        [(let [[[_ _ & elems]] params] (apply list 'list elems)) true]

        ; last is correctly implemented by default

        (and (= 'cons op) (list-literal? (second params)) (= 2 (count params)))
        [(let [[elem [_ & elems]] params] (apply list 'list elem elems)) true]

        (and (= 'nth op)
             (list-literal? (first params))
             (= 2 (count params))
             (int? (second params))
             (<= 0 (second params) (- (count (first params)) 2)))
        [(nth (first params) (+ 1 (second params))) true]

        (and (= 'get op)
             (list-literal? (first params))
             (= 2 (count params))
             (int? (second params)))
        [(get (first params) (+ 1 (second params))) true]

        ; try to perform primitive calls
        :else (let [resolved-op (built-ins op)]
                (cond
                  (and (contains? #{'append 'conj 'cons 'first
                                    'second 'rest 'last}
                                  op)
                       (seqable? (first params))) [(apply resolved-op params)
                                                   true]
                  (and (contains? #{'get 'nth} op)
                       (let [[f & others] params]
                         (and (seqable? f)
                              (not (and (or (list? f) (seq? f))
                                        (contains? built-ins (first f))))
                              (every? value?
                                      others)))) [(apply resolved-op params)
                                                  true]
                  (or (nil? resolved-op)
                      (not (every? value? params))) [(apply list op params)
                                                     changed]
                  :else [(apply resolved-op params) true]))))
    (seqable? exp) (let [peval-results (map peval-once exp)
                         contents (map #(nth % 0) peval-results)
                         changed (boolean (some identity
                                                (map #(nth % 1) peval-results)))]
                     (cond
                       (not changed) [exp false]
                       (map? exp) [(apply into {} contents) true]
                       (set? exp) [(set contents) true]
                       (vector? exp) [(vec contents) true]
                       :else [(apply list contents) true]))
    :else [exp false]))

(defn peval
  [exp]
  (loop [[result changed] [exp true]]
    (if changed
      (recur (peval-once result))
      result)))

(declare free-vars)

(defn- free-vars-list
  [procs list-exp]
  (let [[op & params] list-exp]
    (if (= 'let op)
      (let [[[v e] body] params]
        (disj (union (free-vars procs e) (free-vars procs body)) v))
      (apply union (map #(free-vars procs %) params)))))

(defn free-vars
  [procs ast]
  (cond
    (and (list? ast) (seq ast)) (free-vars-list procs ast)
    (seqable? ast) (apply union (map #(free-vars procs %) ast))
    (symbol? ast) (if (or (contains? procs ast)
                          (contains? built-ins ast))
                    #{}
                    #{ast})
    :else #{}))

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
  ^IPersistentList
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
    (satisfies? anglican.runtime/distribution exp) exp
    (seqable? exp) ((cond
                      (vector? exp) vec
                      (map? exp) #(apply into {} %)
                      (set? exp) set
                      (list? exp) #(apply list %)
                      :else #(apply list %))
                    (map #(bind-free-variables sub %) exp))
    (contains? sub exp) (sub exp)
    :else exp))

(defn sample-from-joint
  ([^Graph {:keys [P]} ^IPersistentList ordering]
   (loop [m {}
          to-do ordering]
     (if (seq to-do)
       (let [[v & new-to-do] to-do
             sampled-value (let [dist (P v)]
                             (anglican.runtime/sample*
                               (if (satisfies? anglican.runtime/distribution dist)
                                 dist
                                 (peval (bind-free-variables m dist)))))]
         (recur (assoc m v sampled-value)
                new-to-do))
       m)))
  ([^Graph graph] (sample-from-joint graph (ancestral-ordering graph))))

(defn sample-from-prior
  ([^Graph {:keys [P Y]} ^IPersistentList ordering]
   (loop [m {}
          to-do ordering]
     (if (seq to-do)
       (let [[v & new-to-do] to-do]
         (if (not (contains? Y v))
           (let [sampled-value (let [dist (P v)]
                                 (anglican.runtime/sample*
                                   (if (satisfies? anglican.runtime/distribution dist)
                                     dist
                                     (peval (bind-free-variables (into m Y) dist)))))]
             (recur (assoc m v sampled-value)
                    new-to-do))
           (recur m new-to-do)))
       m)))
  ([^Graph graph] (sample-from-prior graph (ancestral-ordering graph))))
