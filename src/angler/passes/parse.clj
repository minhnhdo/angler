(ns angler.passes.parse
  (:require [clojure.edn :as edn]))

(defn parse
  [^java.io.PushbackReader r]
  (println
    (loop [exps []]
      (let [e (edn/read {:readers {}, :eof ::done} r)]
        (if (= ::done e)
          exps
          (recur (conj exps e)))))))
