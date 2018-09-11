(ns angler.passes.parse
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [angler.errors :refer [checks parse-error read-error]]))

(defn- parse-expression
  [ast]
  ast)

(defn- parse-defn
  [ast]
  (checks
    [(list? ast) (parse-error (str "Unexpected " (class ast) " for defn in\n" ast))]
    (let [[defn-kw fn-name arguments body & more] ast]
      (checks
        [(= 'defn defn-kw) (parse-error (str "Expected defn, found " defn-kw "\n" ast))
         (symbol? fn-name) (parse-error (str "Expected function name as a symbol, found " (class fn-name) "\n" ast))
         (vector? arguments) (parse-error (str "Expected vector of arguments, found " (class arguments) "\n" ast))
         (every? symbol? arguments) (parse-error (str "Expected arguments to be symbols\n" ast))
         (nil? more) (parse-error (str "Unexpected " more "\n" ast))]
        (let [parsed-exp (parse-expression body)]
          (if (:error parsed-exp)
            (parse-error (str "Found below error while parsing body of " fn-name "\n" (:message parsed-exp)))
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
        (parse-error (str "Parse errors for defns\n"
                          (string/join \newline (map :message errors-defns))))
        (let [parsed-exp (parse-expression (peek exps))]
          (if (:error parsed-exp)
            (parse-error (str "Parse error for expression\n"
                              (:message parsed-exp)))
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
                  (read-error (str "Parse error after expression\n"
                                   (with-out-str (pprint (last exps))))))))]
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
