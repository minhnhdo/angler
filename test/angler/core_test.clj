(ns angler.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [angler.core :refer [check-error]]
            [angler.errors :refer [checked-pipeline->]]
            [angler.passes.desugar :refer [desugar]]
            [angler.passes.parse :refer [parse]]
            [angler.passes.validate :refer [validate]]))

(deftest validated-desugared-twice
  (doseq [i (range 1 9)]
    (let [filename (str "examples/e" i ".clj")]
      (testing (str "Validating and desugaring " filename " twice")
        (let [parse-result (with-open [r (java.io.PushbackReader. (io/reader (io/resource filename)))]
                (parse r))
              output (checked-pipeline->
                       parse-result
                       check-error validate
                       check-error desugar)]
          (is (= nil (:angler.errors/error output)))
          (is (= output (desugar (validate output)))))))))
