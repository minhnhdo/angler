(ns angler.passes.desugar)

(defn- desugar-identifier
  [e]
  (if (= '_ e)
    (gensym)
    e))

(declare desugar-expression)

(defn- desugar-let
  [e]
  (let [[_ bindings & body] e
        reversed-desugared-bindings (rseq
                                      (mapv #(vector
                                               (desugar-identifier (nth % 0))
                                               (desugar-expression (nth % 1)))
                                            (partition 2 bindings)))
        reversed-desugared-exps (rseq (mapv desugar-expression body))
        desugared-body (reduce #(list 'let [(gensym) %2] %1)
                               reversed-desugared-exps)]
    (reduce #(list 'let [(nth %2 0) (nth %2 1)] %1)
            desugared-body
            reversed-desugared-bindings)))

(defn- desugar-list
  [e]
  (let [[op & params] e
        desugared-params (mapv desugar-expression params)]
    (cond
      (= 'if op) (list 'if (nth desugared-params 0)
                       (nth desugared-params 1)
                       (nth desugared-params 2))
      (= 'let op) (desugar-let e)
      :else (apply list op desugared-params))))

(defn- desugar-vector
  [e]
  (apply list 'vector e))

(defn- desugar-map
  [e]
  (apply list 'hash-map e))

(defn- desugar-expression
  [e]
  (cond
    (list? e) (desugar-list e)
    (vector? e) (desugar-vector e)
    (map? e) (desugar-map e)
    :else e))

(defn- desugar-defn
  [d]
  (let [[_ fn-name arguments body] d]
    (list
      'defn (desugar-identifier fn-name)
      (mapv desugar-identifier arguments)
      (desugar-expression body))))

(defn desugar
  [program]
  (conj (mapv desugar-defn (pop program))
        (desugar-expression (peek program))))
