(ns angler.passes.parse
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [angler.errors :refer [read-error]]))

(defn parse
  [^java.io.PushbackReader r]
  (loop [exps []]
    (let [e (try
              (edn/read {:readers {}, :eof ::done} r)
              (catch java.lang.RuntimeException e
                (if (empty? exps)
                  (read-error "Parse error at beginning of file")
                  (read-error "Parse error after expression\n"
                              (with-out-str (pprint (last exps)))))))]
      (cond
        (:error e) e
        (= ::done e) exps
        :else (recur (conj exps e))))))
