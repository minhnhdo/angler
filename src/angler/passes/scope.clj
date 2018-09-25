(ns angler.passes.scope
  (:require [clojure.set :refer [union]]
            [clojure.string :as string]
            [angler.errors :refer [checks scope-error]]
            [angler.types :refer [built-ins]]))

(defn- scope-identifier
  [bound-syms identifier]
  (if (or (= '_ identifier)
          (contains? bound-syms identifier)
          (resolve identifier))
    identifier
    (scope-error "Unknown identifier " identifier)))

(declare scope-expression)

(defn- params-helper
  [bound-syms params]
  (let [errors (filter :angler.errors/error
                       (map #(scope-expression bound-syms %) params))]
    (when (seq errors)
      (scope-error
        (string/join \newline (map :angler.errors/message errors))))))

(defn- bindings-helper
  [bound-syms bindings]
  (let [[new-bound-syms scoping-results]
        (reduce (fn [[bound-syms r] [v exp]]
                  [(assoc bound-syms v {:is-fn? false})
                   (conj r (scope-expression bound-syms exp))])
                [bound-syms []]
                (partition 2 bindings))
        errors (filter :angler.errors/error scoping-results)]
    [new-bound-syms
     (when (seq errors)
        (scope-error
          (string/join \newline (map :angler.errors/message errors))))]))

(defn- bindings-and-body-helper
  [bound-syms bindings body]
  (let [[new-bound-syms scoping-results] (bindings-helper bound-syms bindings)
        scoped-body (params-helper new-bound-syms body)]
    (if (and (:angler.errors/error scoping-results)
             (:angler.errors/error scoped-body))
      (scope-error
        (str (:angler.errors/message scoping-results) "\n"
             (:angler.errors/message scoped-body)))
      (or scoping-results scoped-body))))

(defn- scope-list
  [bound-syms list-exp]
  (let [[op & params] list-exp]
    (cond
      (= 'let op) (let [[bindings & body] params]
                    (or (bindings-and-body-helper bound-syms bindings body)
                        list-exp))
      (= 'foreach op) (let [[_ bindings & body] params]
                        (or (bindings-and-body-helper bound-syms bindings body)
                            list-exp))
      (or (contains? built-ins op) (resolve op)) (or (params-helper bound-syms
                                                                    params)
                                                     list-exp)
      (and (contains? bound-syms op)
           (:is-fn? (bound-syms op))) (let [{:keys [nargs]} (bound-syms op)]
                                        (or (when (not= (:nargs (bound-syms op))
                                                        (count params))
                                              (scope-error "Procedure " op
                                                           " requires "
                                                           nargs " arguments "
                                                           " but is called "
                                                           " with "
                                                           (count params)
                                                           " arguments\n"
                                                           list-exp))
                                            (params-helper bound-syms params)
                                            list-exp))
      :else (or (params-helper bound-syms list-exp) list-exp))))

(defn- scope-expression
  [bound-syms exp]
  (cond
    (and (list? exp) (seq exp)) (scope-list bound-syms exp)
    (seqable? exp) (or (params-helper bound-syms exp) exp)
    (symbol? exp) (scope-identifier bound-syms exp)
    :else exp))

(defn- scope-defn
  [bound-syms ast]
  (let [[_ _ arguments body] ast]
    (scope-expression (into bound-syms
                            (map #(vector % {:is-fn? false}) arguments))
                      body)))

(defn scope
  [program]
  (let [[bound-syms scoped-defns]
        (reduce (fn [[bound-syms r] [_ fn-name arguments :as ast]]
                  [(assoc bound-syms fn-name {:is-fn? true
                                              :nargs (count arguments)})
                   (conj r (scope-defn bound-syms ast))])
                [{} []]
                (pop program))
        scoped-exp (scope-expression bound-syms (peek program))
        errors (filter :angler.errors/error (conj (vec scoped-defns) scoped-exp))]
    (when (seq errors)
      (println "WARN")
      (println (string/join \newline (map :angler.errors/message errors))))
    program))
