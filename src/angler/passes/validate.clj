(ns angler.passes.validate
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [angler.errors :refer [checks validate-error]]))

(defn- prettify
  [ast]
  (with-out-str (pprint ast)))

(defn- validate-identifier
  [identifier]
  (checks
    [(symbol? identifier)
     (validate-error
       "Expected identifier, found " (class identifier) "\n"
       (prettify identifier))]
    identifier))

(declare validate-expression)

(defn- validate-list
  [ast]
  (let [op (first ast)
        validated-params (map validate-expression (rest ast))
        errors (filter :angler.errors/error validated-params)]
    (cond
      (seq errors) (validate-error
                     (string/join \newline (map :angler.errors/message errors)))
      (= 'let op) (let [[bindings & body] validated-params]
                    (checks
                      [(vector? bindings)
                       (validate-error
                         "Expected bindings as a vector\n" (prettify bindings))

                       (= 0 (mod (count bindings) 2))
                       (validate-error
                         "Expected even number of elements in binding vector\n"
                         (prettify bindings))]
                      (let [validated-bindings
                            (map #(vector (validate-identifier (nth % 0))
                                          (validate-expression (nth % 1)))
                                 (partition 2 bindings))
                            validated-body (map validate-expression body)
                            errors (filter
                                     :angler.errors/error
                                     (into (vec (flatten validated-bindings))
                                           validated-body))]
                        (if (empty? errors)
                          ast
                          (validate-error
                            (string/join
                              \newline (map :angler.errors/message errors)))))))
      (= 'foreach op) (let [[c bindings & body] validated-params]
                        (checks
                          [(and (int? c) (> c 0))
                           (validate-error
                             "Expected positive integer, found "
                             (class c) "\n"
                             (prettify c))

                           (vector? bindings)
                           (validate-error "Expected bindings as a vector\n"
                                           (prettify bindings))

                           (= 0 (mod (count bindings) 2))
                           (validate-error
                             "Expected even number of elements in binding vector\n"
                             (prettify bindings))]
                          (let [validated-bindings
                                (map #(vector
                                        (validate-identifier (nth % 0))
                                        (validate-expression (nth % 1)))
                                     (partition 2 bindings))
                                validated-body
                                (map validate-expression body)
                                errors (filter
                                         :angler.errors/error
                                         (into (vec (flatten validated-bindings))
                                               validated-body))]
                            (if (empty? errors)
                              ast
                              (validate-error
                                (string/join
                                  \newline (map :angler.errors/message errors)))))))
      (= 'loop op) (let [[c _ f] validated-params]
                     (checks
                       [(and (int? c) (> c 0))
                        (validate-error
                          "Expected positive integer, found "
                          (class c) "\n"
                          (prettify c))

                        (not (:angler.errors/error
                               (validate-identifier f)))
                        (validate-error
                          "Expected function name, found " (class f) "\n"
                          (prettify f))]
                       ast))
      (= 'sample op) (checks
                       [(= 1 (count validated-params))
                        (validate-error
                          "Expected 1 argument to sample, found "
                          (count validated-params) "\n"
                          (prettify ast))]
                       ast)
      (= 'observe op) (checks
                        [(= 2 (count validated-params))
                         (validate-error
                           "Expected 2 arguments to observe, found "
                           (count validated-params)
                           (prettify ast))]
                        ast)
      :else (let [validated-op (validate-identifier op)]
              (if (:angler.errors/error validated-op)
                validated-op
                ast)))))

(defn- params-helper
  [params]
  (let [errors (filter :angler.errors/error
                       (map validate-expression params))]
    (when (seq errors)
      (validate-error
        (string/join \newline (map :angler.errors/message errors))))))

(defn- validate-expression
  [ast]
  (cond
    (and (list? ast) (seq ast)) (validate-list ast)
    (seqable? ast) (or (params-helper ast) ast)
    (symbol? ast) (validate-identifier ast)
    :else ast))

(defn- validate-defn
  [ast]
  (checks
    [(list? ast) (validate-error "Unexpected " (class ast) " for defn in\n"
                                 (prettify ast))]
    (let [[defn-kw fn-name arguments body & more] ast]
      (checks
        [(= 'defn defn-kw)
         (validate-error "Expected defn, found " defn-kw "\n" (prettify ast))

         (not (:angler.errors/error (validate-identifier fn-name)))
         (validate-error "Expected function name as a symbol, found "
                         (class fn-name) "\n" (prettify ast))

         (vector? arguments)
         (validate-error
           "Expected vector of arguments, found " (class arguments) "\n"
           (prettify ast))

         (every? #(not (:angler.errors/error (validate-identifier %)))
                 arguments)
         (validate-error "Expected arguments to be symbols\n" (prettify ast))

         (nil? more)
         (validate-error "Unexpected " more "\n" (prettify ast))]
        (let [validated-exp (validate-expression body)]
          (if (:angler.errors/error validated-exp)
            (validate-error "Found below error while parsing body of "
                            fn-name "\n"
                            (:angler.errors/message validated-exp))
            ast))))))

(defn- validate-program
  [exps]
  (checks
    [(seq exps) (validate-error "Empty program")]
    (let [validated-defns (map validate-defn (pop exps))
          validated-exp (validate-expression (peek exps))
          errors (filter :angler.errors/error
                         (conj (vec validated-defns) validated-exp))]
      (checks
        [(empty? errors)
         (validate-error
           (string/join \newline (map :angler.errors/message errors)))]
        exps))))

(defn validate
  [exps]
  (validate-program exps))
