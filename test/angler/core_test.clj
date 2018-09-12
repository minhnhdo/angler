(ns angler.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [angler.core :refer [check-error]]
            [angler.errors :refer [checked-pipeline->]]
            [angler.passes.desugar :refer [desugar]]
            [angler.passes.parse :refer [parse]]
            [angler.passes.validate :refer [validate]]))

(deftest desugared-twice
  (doseq [i (range 1 9)]
    (let [filename (str "examples/e" i ".clj")]
      (testing (str "Desugaring " filename " twice")
        (let [parse-result (with-open [r (java.io.PushbackReader. (io/reader (io/resource filename)))]
                (parse r))
              output (checked-pipeline->
                       parse-result
                       check-error validate
                       check-error desugar)]
          (is (= nil (:angler.errors/error output)))
          (is (= output (desugar output))))))))
