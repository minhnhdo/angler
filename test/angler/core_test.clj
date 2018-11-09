(ns angler.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [angler.errors :refer [check-error checked->]]
            [angler.passes.desugar :refer [desugar]]
            [angler.passes.parse :refer [parse]]
            [angler.passes.scope :refer [scope]]
            [angler.passes.validate :refer [validate]]))

(deftest can-desugar-examples-twice
  (doseq [i (range 1 18)]
    (let [filename (str "examples/e" i ".clj")]
      (testing (str "can desugar " filename " twice")
        (let [parse-result (with-open [r (java.io.PushbackReader. (io/reader (io/resource filename)))]
                (parse r))
              output (checked->
                       parse-result
                       check-error validate
                       check-error scope
                       check-error desugar)]
          (is (nil? (:angler.errors/error output)))
          (is (nil? (:angler.errors/error (desugar (scope (validate output)))))))))))
