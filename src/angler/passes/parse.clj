(ns angler.passes.parse
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]))

(defn- collect
  [^java.io.PushbackReader r]
  (loop [exps []]
    (let [e (try
              {:e (edn/read {:readers {}, :eof ::done} r)}
              (catch java.lang.RuntimeException e
                (if (empty? exps)
                  {:error :angler/parse-error
                   :message "Parse error at beginning of file"}
                  {:error :angler/parse-error
                   :message (str "Parse error after expression\n\n"
                                 (with-out-str
                                   (pprint (last exps))))})))]
      (cond
        (:error e) e
        (= ::done (:e e)) {:exps exps}
        :else (recur (conj exps (:e e)))))))

(defn- transform
  [exps]
  exps)

(defn parse
  [^java.io.PushbackReader r]
  (let [collect-result (collect r)]
    (if (:error collect-result)
      collect-result
      (transform (:exps collect-result)))))
