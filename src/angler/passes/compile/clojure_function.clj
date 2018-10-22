(ns angler.passes.compile.clojure-function
  (:import [clojure.lang IPersistentMap IPersistentVector Symbol]))

(declare compile-expression)

(defn- compile-list
  [^Symbol log-weight-atom ^IPersistentMap defns e]
  (let [op (first e)
        compiled-params (map #(compile-expression log-weight-atom defns %)
                             (rest e))]
    (cond
      (= 'let op) (apply list 'let (vec (first compiled-params))
                         (rest compiled-params))
      (= 'sample op) (apply list 'sample* compiled-params)
      (= 'observe op) (let [[e1 e2] compiled-params]
                        (list 'do
                              (list 'swap! log-weight-atom
                                    (list 'fn ['a]
                                          (list '+ 'a (list 'observe* e1 e2))))
                              e2))
      (contains? defns op) (let [[_ _ args body] (defns op)
                                 bindings (interleave args compiled-params)]
                             (list 'let (vec bindings)
                                   (compile-expression
                                     log-weight-atom defns body)))
      :else (apply list (compile-expression log-weight-atom defns op)
                   compiled-params))))

(defn- compile-expression
  [^Symbol log-weight-atom ^IPersistentMap defns e]
  (if (and (seqable? e) (seq e))
    (compile-list log-weight-atom defns e)
    e))

(defn compile-to-function
  [^IPersistentVector program]
  (let [defns (into {} (map (fn [[_ fn-name :as func]] [fn-name func])
                            (pop program)))
        expr (peek program)
        log-weight-atom (gensym "log-weight-")]
    (binding [*ns* (in-ns 'angler.primitives)]
      (eval (list 'fn [log-weight-atom]
                  (compile-expression log-weight-atom defns expr))))))
