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

(defn- desugar-loop
  [expr]
  (let [[_ c e f & body] expr
        as (repeatedly (count body) gensym)
        vs (repeatedly c gensym)
        calls (rseq (mapv #(vector %1 (apply list f %2 %3 as))
                          vs
                          (range c)
                          (cons e vs)))
        vlast (nth (nth calls 0) 0)]
    (desugar-let
      (list 'let (vec (interleave as body))
            (reduce #(list 'let %2 %1)
                    vlast
                    calls)))))

(defn- desugar-foreach
  [e]
  (let [[_ c bindings & body] e
        partitioned-bindings (partition 2 bindings)]
    (apply list 'vector
           (for [i (range c)]
             (desugar-let
               (apply list 'let (vec (mapcat #(let [v (nth % 0)
                                                    e (nth % 1)]
                                                [v (list 'nth e i)])
                                             partitioned-bindings))
                      body))))))

(defn- desugar-list
  [e]
  (let [[op & params] e]
    (cond
      (= 'if op) (let [[cond-exp then-exp else-exp] (map desugar-expression params)]
                   ;; the above explicit binding is to handle if expressions
                   ;; with missing else and then branches
                   (list 'if cond-exp then-exp else-exp))
      (= 'let op) (desugar-let e)
      (= 'foreach op) (desugar-foreach e)
      (= 'loop op) (desugar-loop e)
      :else (apply list op (map desugar-expression params)))))

(defn- desugar-expression
  [e]
  (cond
    (and (list? e) (seq e)) (desugar-list e)
    (list? e) (list 'list)
    (vector? e) (apply list 'vector (map desugar-expression e))
    (set? e) (apply list 'hash-set (map desugar-expression e))
    (map? e) (apply list 'hash-map (map desugar-expression
                                        (mapcat identity (seq e))))
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
