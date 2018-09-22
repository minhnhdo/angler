(ns angler.passes.compile
  (:require [clojure.set :refer [intersection union]]
            [angler.errors :refer [checks compile-error]]
            [angler.types :refer [built-ins empty-graph join-graph new-graph
                                  pmf]])
  (:import [angler.errors CompileError]))

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
          (contains? temporary v) (compile-error "Not a DAG "
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

(defn literal?
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
        (apply (resolve op) params)
        (= 'if op)
        (let [[cond-exp then-exp else-exp] params]
          (cond
            (not (literal? cond-exp)) (list 'if cond-exp then-exp else-exp)
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
          (apply list 'list (concat (reverse added) elems)))

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
        :else (let [resolved-op (resolve op)]
                (cond
                  (and (contains? #{'conj 'cons 'first 'rest 'last} op)
                       (literal? (first params)))
                  (apply resolved-op params)
                  (or (nil? resolved-op) (not (every? literal? params)))
                  (apply list op params)
                  :else (apply resolved-op params)))))
    exp))

(declare free-vars)

(defn- free-vars-list
  [procs list-exp]
  (let [[op & params] list-exp]
    (if (= 'let op)
      (let [[[v e] body] params]
        (disj (union (free-vars procs e) (free-vars procs body)) v))
      (apply union (map #(free-vars procs %) list-exp)))))

(defn free-vars
  [procs ast]
  (cond
    (and (list? ast) (seq ast)) (free-vars-list procs ast)
    (symbol? ast) (if (or (contains? procs ast)
                          (contains? built-ins ast)
                          (resolve ast))
                    #{}
                    #{ast})
    :else #{}))

(defn- score
  [exp v]
  (checks
    [(and (list? exp) (seq exp))
     (throw (CompileError. (str "Unexpected " (class exp) " " exp)))]
    (let [[op & params] exp]
      (cond
        (= 'if op) (let [[e1 e2 e3] params]
                     (list 'if e1 (score e2 v) (score e3 v)))
        (contains? pmf op) (list 'observe* exp v)
        :else (throw (CompileError. (str "Unexpected " exp)))))))

(defn- compile-identifier
  [sub _ _ identifier]
  [(empty-graph)
   (if (contains? sub identifier)
     (sub identifier)
     identifier)])

(declare compile-expression)

(defn- compile-let
  [sub procs pred let-exp]
  (let [[_ [v e] body] let-exp
        [graph-e compiled-e] (compile-expression sub procs pred e)
        [graph-body compiled-body]
        (compile-expression (assoc sub v compiled-e) procs pred body)]
    [(join-graph graph-e graph-body) compiled-body]))

(defn- compile-if
  [sub procs pred if-exp]
  (let [[_ cond-exp then-exp else-exp] if-exp
        [graph-cond compiled-cond] (compile-expression sub procs pred cond-exp)
        [graph-then compiled-then]
        (compile-expression sub procs (peval (list 'and pred cond-exp)) then-exp)
        [graph-else compiled-else]
        (compile-expression sub procs (peval (list 'and pred (not cond-exp))) else-exp)]
    [(join-graph graph-cond graph-then graph-else)
     (peval (list 'if compiled-cond compiled-then compiled-else))]))

(defn- compile-sample
  [sub procs pred sample-exp]
  (let [[_ e] sample-exp
        [{:keys [V A P Y]} compiled-e] (compile-expression sub procs pred e)
        v (gensym)
        Z (free-vars procs compiled-e)
        F (score compiled-e v)]
    [(new-graph (conj V v)
                (into A (map #(vector % v) Z))
                (assoc P v F)
                Y)
     v]))

(defn- compile-observe
  [sub procs pred observe-exp]
  (let [[_ e1 e2] observe-exp
        [graph-e1 compiled-e1] (compile-expression sub procs pred e1)
        [graph-e2 compiled-e2] (compile-expression sub procs pred e2)
        {:keys [V A P Y]} (join-graph graph-e1 graph-e2)
        v (gensym)
        F1 (score compiled-e1 v)
        F (peval (list 'if pred F1 1))
        Z (disj (free-vars procs F1) v)
        B (set (map #(vector % v) Z))
        unexpected-free-vars (intersection (free-vars procs compiled-e2) V)]
    (if (seq unexpected-free-vars)
      (throw (CompileError.
               (str "Unexpected free variables " unexpected-free-vars
                    " in " observe-exp)))
      [(new-graph (conj V v) (union A B) (assoc P v F) (assoc Y v compiled-e2))
       compiled-e2])))

(defn- compile-procedure-call
  [sub procs pred call-exp]
  (let [[fn-name & params] call-exp
        [_ _ arguments body] (procs fn-name)
        compiled-results (map #(compile-expression sub procs pred %) params)
        graphs (map #(nth % 0) compiled-results)
        compiled-exps (map #(nth % 1) compiled-results)
        new-sub (into sub (map #(vector %1  %2) arguments compiled-exps))
        [graph-body compiled-body] (compile-expression new-sub procs pred body)]
    [(apply join-graph (concat graphs graph-body))
     compiled-body]))

(defn- compile-primitive-call
  [sub procs pred call-exp]
  (let [[op & params] call-exp
        compiled-results (map #(compile-expression sub procs pred %) params)
        graphs (map #(nth % 0) compiled-results)
        compiled-exps (map #(nth % 1) compiled-results)]
    [(apply join-graph graphs)
     (peval (apply list op compiled-exps))]))

(defn- compile-list
  [sub procs pred e]
  (let [[op] e]
    (cond
      (= 'let op) (compile-let sub procs pred e)
      (= 'if op) (compile-if sub procs pred e)
      (= 'sample op) (compile-sample sub procs pred e)
      (= 'observe op) (compile-observe sub procs pred e)
      (contains? procs op) (compile-procedure-call sub procs pred e)
      (or (contains? pmf op)
          (resolve op)) (compile-primitive-call sub procs pred e)
      :else [(empty-graph) e])))

(defn- compile-expression
  [sub procs pred e]
  (cond
    (and (list? e) (seq e)) (compile-list sub procs pred e)
    (symbol? e) (compile-identifier sub procs pred e)
    :else [(empty-graph) e]))

(defn compile-to-graph
  [program]
  (let [procs (into {} (map #(vector (second %) %)) (pop program))
        exp (peek program)]
    (try
      (compile-expression {} procs true exp)
      (catch CompileError e
        (compile-error (.getMessage e))))))
