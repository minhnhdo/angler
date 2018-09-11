(ns angler.passes.parse
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [angler.errors :refer [checks parse-error read-error]]))

(defn- ||
  [& parsers]
  (fn [param]
    (let [parsed-results (pmap #(% param) parsers)
          successful-results (filter #(not (:error %)) parsed-results)]
      (if (empty? successful-results)
        (assoc (parse-error "No alternative succeeded") :errors parsed-results)
        (first successful-results)))))

(defn- parse-identifier
  [identifier]
  (checks
    [(symbol? identifier) (parse-error "Expected identifier, found " (class identifier) "\n" identifier)
     (not (contains? #{'if 'let} identifier)) (parse-error "Expected identifier, found " identifier)]
    {:identifier identifier}))

(defn- parse-operator
  [op]
  op)

(declare parse-expression)

(defn- parse-list
  [ast]
  (let [op (first ast)
        parsed-params (vec (pmap parse-expression (rest ast)))]
    (cond
      (= 'if op) (if (= 3 (count parsed-params))
                   {:type :angler.types/if
                    :condition (nth parsed-params 0)
                    :then (nth parsed-params 1)
                    :else (nth parsed-params 2)}
                   (parse-error "Expected 3 arguments to if\n" ast))
      (= 'let op) (checks
                    [(= 2 (count parsed-params)) (parse-error "Expected 2 arguments to let\n" ast)
                     (vector? (nth parsed-params 0)) (parse-error "Expected bindings as a vector\n" ast)
                     (= 0 (mod (count parsed-params) 2)) (parse-error "Expected even number of elements in binding vector\n" ast)]
                    (let [bindings (pmap #(vector (parse-identifier (nth % 0))
                                                  (parse-expression (nth % 1)))
                                         (partition 2 parsed-params))]
                      {:type :angler.types/sugared-let
                       :bindings (nth parsed-params 0)
                       :body (nth parsed-params 1)}))
      :else (let [parsed-op (parse-operator op)]
              (if (:error parsed-op)
                false)))))

(defn- parse-expression
  [ast]
  (cond
    (list? ast) (parse-list ast)
    :else (parse-error "Invalid expression\n" ast)))

(defn- parse-defn
  [ast]
  (checks
    [(list? ast) (parse-error "Unexpected " (class ast) " for defn in\n" ast)]
    (let [[defn-kw fn-name arguments body & more] ast]
      (checks
        [(= 'defn defn-kw) (parse-error "Expected defn, found " defn-kw "\n" ast)
         (symbol? fn-name) (parse-error "Expected function name as a symbol, found " (class fn-name) "\n" ast)
         (vector? arguments) (parse-error "Expected vector of arguments, found " (class arguments) "\n" ast)
         (every? symbol? arguments) (parse-error "Expected arguments to be symbols\n" ast)
         (nil? more) (parse-error "Unexpected " more "\n" ast)]
        (let [parsed-exp (parse-expression body)]
          (if (:error parsed-exp)
            (parse-error "Found below error while parsing body of " fn-name "\n" (:message parsed-exp))
            {:type :angler.types/defn
             :name fn-name
             :arguments arguments
             :body parsed-exp}))))))

(defn- transform
  [exps]
  (if (empty? exps)
    (parse-error "Empty program")
    (let [parsed-defns (vec (pmap parse-defn (pop exps)))
          errors-defns (filter :error parsed-defns)]
      (if (seq errors-defns)
        (parse-error "Parse errors for defns\n"
                     (string/join \newline (map :message errors-defns)))
        (let [parsed-exp (parse-expression (peek exps))]
          (if (:error parsed-exp)
            (parse-error "Parse error for expression\n"
                         (:message parsed-exp))
            {:type :angler.types/program
             :defns parsed-defns
             :exp parsed-exp}))))))

(defn- collect
  [^java.io.PushbackReader r]
  (loop [exps []]
    (let [e (try
              {:e (edn/read {:readers {}, :eof ::done} r)}
              (catch java.lang.RuntimeException e
                (if (empty? exps)
                  (read-error "Parse error at beginning of file")
                  (read-error "Parse error after expression\n"
                              (with-out-str (pprint (last exps)))))))]
      (cond
        (:error e) e
        (= ::done (:e e)) {:exps exps}
        :else (recur (conj exps (:e e)))))))

(defn parse
  [^java.io.PushbackReader r]
  (let [collect-result (collect r)]
    (if (:error collect-result)
      collect-result
      (transform (:exps collect-result)))))
