(ns angler.passes.compile
  (:require [clojure.set :refer [intersection union]]
            [angler.errors :refer [checks]]
            [angler.passes.scope :refer [built-ins]]
            [angler.types :refer [empty-graph join-graph pmf new-graph]]))

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
     (throw (RuntimeException. (str "Unexpected " (class exp) " " exp)))]
    (let [[op & params] exp]
      (cond
        (= 'if op) (let [[e1 e2 e3] params]
                     (list 'if e1 (score e2 v) (score e3 v)))
        (contains? pmf op) (apply list (pmf op) v params)
        :else (throw (RuntimeException. (str "Unexpected " exp)))))))

(defn- compile-identifier
  [sub procs pred identifier]
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
        (compile-expression sub procs (list 'and pred cond-exp) then-exp)
        [graph-else compiled-else]
        (compile-expression sub procs (list 'and pred (not cond-exp)) else-exp)]
    [(join-graph graph-cond graph-then graph-else)
     (list 'if compiled-cond compiled-then compiled-else)]))

(defn- compile-sample
  [sub procs pred sample-exp]
  (let [[_ e] sample-exp
        [{:keys [V A P Y]} compiled-e] (compile-expression sub procs pred e)
        v (gensym)
        Z (intersection (free-vars procs compiled-e) V)
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
        F (list 'if pred F1 1)
        Z (intersection (disj (free-vars procs F1) v) V)
        B (set (map #(vector % v) Z))
        unexpected-free-vars (intersection (free-vars procs compiled-e2) V)]
    (if (seq unexpected-free-vars)
      (throw (RuntimeException.
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
        new-sub (into sub (map #(vector %1 %2) arguments compiled-exps))
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
     (apply list op compiled-exps)]))

(defn- compile-list
  [sub procs pred e]
  (let [[op & params] e]
    (cond
      (= 'let op) (compile-let sub procs pred e)
      (= 'if op) (compile-if sub procs pred e)
      (= 'sample op) (compile-sample sub procs pred e)
      (= 'observe op) (compile-observe sub procs pred e)
      (contains? procs op) (compile-procedure-call sub procs pred e)
      (or (contains? pmf op) (resolve op)) (compile-primitive-call
                                             sub procs pred e)
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
    (compile-expression {} procs true exp)))
