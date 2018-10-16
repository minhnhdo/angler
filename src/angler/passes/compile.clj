(ns angler.passes.compile
  (:require [clojure.set :refer [intersection union]]
            [angler.errors :refer [checks compile-error]]
            [angler.types :refer [built-ins distributions empty-graph free-vars
                                  join-graph new-graph peval]])
  (:import [clojure.lang IPersistentMap IPersistentVector]
           [angler.errors CompileError]))

(defn- score
  [exp v]
  (cond
    (list? exp) (let [[op & params] exp]
                  (if (= 'if op)
                    (let [[e1 e2 e3] params]
                      (list 'if e1 (score e2 v) (score e3 v)))
                    exp))
    (satisfies? anglican.runtime/distribution exp) exp
    :else (throw (CompileError. (str "Unexpected " exp)))))

(defn- compile-identifier
  ^IPersistentVector
  [^IPersistentMap sub _ _ identifier]
  [(empty-graph)
   (if (contains? sub identifier)
     (sub identifier)
     identifier)])

(declare compile-expression)

(defn- compile-let
  ^IPersistentVector
  [^IPersistentMap sub ^IPersistentMap procs pred let-exp]
  (let [[_ [v e] body] let-exp
        [graph-e compiled-e] (compile-expression sub procs pred e)
        [graph-body compiled-body]
        (compile-expression (assoc sub v compiled-e) procs pred body)]
    [(join-graph graph-e graph-body) compiled-body]))

(defn- compile-if
  ^IPersistentVector
  [^IPersistentMap sub ^IPersistentMap procs pred if-exp]
  (let [[_ cond-exp then-exp else-exp] if-exp
        [graph-cond compiled-cond] (compile-expression sub procs pred cond-exp)
        [graph-then compiled-then]
        (compile-expression sub procs (peval (list 'and pred cond-exp)) then-exp)
        [graph-else compiled-else]
        (compile-expression sub procs (peval (list 'and pred (not cond-exp))) else-exp)]
    [(join-graph graph-cond graph-then graph-else)
     (peval (list 'if compiled-cond compiled-then compiled-else))]))

(defn- compile-sample
  ^IPersistentVector
  [^IPersistentMap sub ^IPersistentMap procs pred sample-exp]
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
  ^IPersistentVector
  [^IPersistentMap sub ^IPersistentMap procs pred observe-exp]
  (let [[_ e1 e2] observe-exp
        [graph-e1 compiled-e1] (compile-expression sub procs pred e1)
        [graph-e2 compiled-e2] (compile-expression sub procs pred e2)
        {:keys [V A P Y]} (join-graph graph-e1 graph-e2)
        v (gensym)
        F1 (score compiled-e1 v)
        F (peval (list 'if pred F1 1))
        Z (disj (free-vars procs F1) v)
        B (map #(vector % v) Z)
        unexpected-free-vars (intersection (free-vars procs compiled-e2) V)]
    (if (seq unexpected-free-vars)
      (throw (CompileError.
               (str "Unexpected free variables " unexpected-free-vars
                    " in " observe-exp)))
      [(new-graph (conj V v) (into A B) (assoc P v F) (assoc Y v compiled-e2))
       compiled-e2])))

(defn- compile-procedure-call
  ^IPersistentVector
  [^IPersistentMap sub ^IPersistentMap procs pred call-exp]
  (let [[fn-name & params] call-exp
        [_ _ arguments body] (procs fn-name)
        compiled-results (map #(compile-expression sub procs pred %) params)
        graphs (map #(nth % 0) compiled-results)
        compiled-exps (map #(nth % 1) compiled-results)
        new-sub (into sub (map #(vector %1  %2) arguments compiled-exps))
        [graph-body compiled-body] (compile-expression new-sub procs pred body)]
    [(join-graph (apply join-graph graphs) graph-body)
     compiled-body]))

(defn- compile-primitive-call
  ^IPersistentVector
  [^IPersistentMap sub ^IPersistentMap procs pred call-exp]
  (let [[op & params] call-exp
        compiled-results (map #(compile-expression sub procs pred %) params)
        graphs (map #(nth % 0) compiled-results)
        compiled-exps (map #(nth % 1) compiled-results)]
    [(apply join-graph graphs)
     (peval (apply list op compiled-exps))]))

(defn- compile-list
  ^IPersistentVector
  [^IPersistentMap sub ^IPersistentMap procs pred e]
  (let [[op] e]
    (cond
      (= 'let op) (compile-let sub procs pred e)
      (= 'if op) (compile-if sub procs pred e)
      (= 'sample op) (compile-sample sub procs pred e)
      (= 'observe op) (compile-observe sub procs pred e)
      (contains? procs op) (compile-procedure-call sub procs pred e)
      (contains? built-ins op) (compile-primitive-call sub procs pred e)
      :else [(empty-graph) e])))

(defn- compile-expression
  ^IPersistentVector
  [^IPersistentMap sub ^IPersistentMap procs pred e]
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
