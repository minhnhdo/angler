(ns angler.passes.validate
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [angler.built-ins :refer [built-ins]]
            [angler.errors :refer [checks validate-error]]))

(def keywords #{'defn 'foreach 'if 'let 'loop})

(defn- prettify
  [ast]
  (with-out-str (pprint ast)))

(defn- validate-identifier
  [bound-syms identifier]
  (checks
    [(symbol? identifier)
     (validate-error
       "Expected identifier, found " (class identifier) "\n"
       (prettify identifier))

     (not (contains? keywords identifier))
     (validate-error "Expected identifier, found " (prettify identifier))]

    identifier))

(declare validate-expression)

(defn- validate-list
  [bound-syms ast]
  (let [op (first ast)
        validated-params (mapv #(validate-expression bound-syms %) (rest ast))
        errors (filter :angler.errors/error validated-params)]
    (cond
      (seq errors) (validate-error
                     (string/join \newline (map :angler.errors/message errors)))
      (= 'if op) (if (= 3 (count validated-params))
                   ast
                   (validate-error "Expected 3 arguments to if\n"
                                   (prettify ast)))
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
                            (map #(vector (validate-identifier bound-syms
                                                               (nth % 0))
                                          (validate-expression bound-syms
                                                               (nth % 1)))
                                 (partition 2 bindings))
                            validated-body (map #(validate-expression bound-syms %) body)
                            errors (filter :angler.errors/error
                                           (concat (flatten validated-bindings)
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
                                        (validate-identifier bound-syms
                                                             (nth % 0))
                                        (validate-expression bound-syms
                                                             (nth % 1)))
                                     (partition 2 bindings))
                                validated-body
                                (map #(validate-expression bound-syms %) body)
                                errors (filter :angler.errors/error
                                               (concat (flatten validated-bindings)
                                                       validated-body))]
                            (if (empty? errors)
                              ast
                              (validate-error
                                (string/join
                                  \newline (map :angler.errors/message errors)))))))
      (= 'loop op) (let [[c e f & body] validated-params]
                     (checks
                       [(and (int? c) (> c 0))
                        (validate-error
                          "Expected positive integer, found "
                          (class c) "\n"
                          (prettify c))

                        (not (:angler.errors/error
                               (validate-identifier bound-syms f)))
                        (validate-error
                          "Expected function name, found " (class f) "\n"
                          (prettify f))]
                       ast))
      :else (let [validated-op (validate-identifier bound-syms op)]
              (checks
                [(not (:angler.errors/error validated-op)) validated-op

                 (or (bound-syms validated-op) (resolve validated-op))
                 (validate-error "Unknown procedure " validated-op "\n"
                                 (prettify op))]
                ast)))))

(defn- validate-vector
  [bound-syms ast]
  (let [contents (map #(validate-expression bound-syms %) ast)
        errors (filter :angler.errors/error contents)]
    (if (empty? errors)
      ast
      (validate-error
        (string/join \newline (map :angler.errors/message errors))))))

(defn- validate-map
  [bound-syms ast]
  (let [pairs (map #(validate-expression bound-syms %) ast)
        errors (filter :angler.errors/error pairs)]
    (if (empty? errors)
      ast
      (validate-error
        (string/join \newline (map :angler.errors/message errors))))))

(defn- validate-expression
  [bound-syms ast]
  (cond
    (and (list? ast) (seq ast)) (validate-list bound-syms ast)
    (vector? ast) (validate-vector bound-syms ast)
    (map? ast) (validate-map bound-syms ast)
    (symbol? ast) (validate-identifier bound-syms ast)
    :else ast))

(defn- validate-defn
  [bound-syms ast]
  (checks
    [(list? ast) (validate-error "Unexpected " (class ast) " for defn in\n"
                                 (prettify ast))]
    (let [[defn-kw fn-name arguments body & more] ast]
      (checks
        [(= 'defn defn-kw)
         (validate-error "Expected defn, found " defn-kw "\n" (prettify ast))

         (not (:angler.errors/error (validate-identifier bound-syms fn-name)))
         (validate-error "Expected function name as a symbol, found "
                         (class fn-name) "\n" (prettify ast))

         (vector? arguments)
         (validate-error
           "Expected vector of arguments, found " (class arguments) "\n"
           (prettify ast))

         (every? #(not (:angler.errors/error
                         (validate-identifier bound-syms %)))
                 arguments)
         (validate-error "Expected arguments to be symbols\n" (prettify ast))

         (nil? more)
         (validate-error "Unexpected " more "\n" (prettify ast))]
        (let [validated-exp (validate-expression bound-syms body)]
          (if (:angler.errors/error validated-exp)
            (validate-error "Found below error while parsing body of "
                            fn-name "\n"
                            (:angler.errors/message validated-exp))
            ast))))))

(defn- validate-program
  [exps]
  (checks
    [(seq exps) (validate-error "Empty program")]
    (let [[bound-syms validated-defns]
          (reduce (fn [[bound-syms validated-defns] exp]
                    (let [validated-defn (validate-defn bound-syms exp)]
                      [(if (:angler.errors/error validated-defn)
                         bound-syms
                         (assoc bound-syms (second validated-defn) validated-defn))
                       (conj validated-defns validated-defn)]))
                  [built-ins []]
                  (pop exps))
          validated-exp (validate-expression bound-syms (peek exps))
          errors (concat (filter :angler.errors/error validated-defns)
                         (if (:angler.errors/error validated-exp) [validated-exp] []))]
      (checks
        [(empty? errors)
         (validate-error (string/join \newline
                                      (map :angler.errors/message errors)))]
        exps))))

(defn validate
  [exps]
  (validate-program exps))
