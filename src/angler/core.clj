(ns angler.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [angler.autodiff :refer [autodiff p1 p2 p3 p4 p5 p6 p7]]
            [angler.errors :refer [check-error checked->]]
            [angler.passes.compile.graph :refer [compile-to-graph]]
            [angler.passes.desugar :refer [desugar]]
            [angler.passes.parse :refer [parse]]
            [angler.passes.scope :refer [scope]]
            [angler.passes.validate :refer [validate]]
            [angler.types :refer [count-edges count-vertices print-graph
                                  edge-vector->adjacency-vector
                                  sample-from-joint sample-from-prior
                                  topological-sort]])
  (:gen-class)
  (:import (java.io PushbackReader)))

(def cli-options
  [["-h" "--help" "Print this help message" :default false]
   [nil "--parse" "Run up to parsing only" :default false]
   [nil "--validate" "Run up to validation only" :default false]
   [nil "--scope" "Run up to scoping only" :default false]
   [nil "--desugar" "Run up to desugaring only" :default false]
   [nil "--count-vertices" "Print the number of vertices in the resulting graph"
    :default false]
   [nil "--count-edges" "Print the number of edges in the resulting graph"
    :default false]
   [nil "--print-graph" "Print the resulting graph" :default false]
   [nil "--autodiff" "Run the autodiff functionality" :default false]
   [nil "--sample-from-joint" "Print a sample from the joint distribution"
    :default false]
   [nil "--sample-from-prior" "Print a sample from the prior distribution"
    :default false]])

(defn usage
  [option-summary]
  (->> ["Angler - a probabilistic programming language"
        ""
        "Usage: program-name [options] <input-files|->..."
        ""
        "Options:"
        option-summary]
       (string/join \newline)))

(defn process-options
  [{:keys [options summary errors arguments] :as parsed-options}]
  (cond
    (:help options) (do (println (usage summary))
                        (System/exit 1))
    errors (do (println (str "Unable to parse command line arguments:\n\n"
                             (string/join \newline errors)))
               (System/exit 1))
    (nil? (seq arguments)) (assoc parsed-options :arguments ["-"])
    :else parsed-options))

(defn -main
  [& args]
  (let [{:keys [options arguments]} (process-options (parse-opts args cli-options))]
    (if (:autodiff options)
      (let [[program & args] arguments
            parsed-args (map edn/read-string args)
            programs {"p1" p1, "p2" p2, "p3" p3, "p4" p4,
                      "p5" p5, "p6" p6, "p7" p7}]
        (println (autodiff
                   (if (contains? programs program)
                     (get programs program)
                     (edn/read-string program))
                   parsed-args)))
      (doseq [f arguments]
        (let [parse-result (with-open [r (if (= "-" f)
                                           *in*
                                           (PushbackReader. (io/reader f)))]
                             (parse r))
              output (checked->
                       parse-result
                       #(and (not (:parse options)) (check-error %))
                       validate
                       #(and (not (:validate options)) (check-error %))
                       scope
                       #(and (not (:scope options)) (check-error %))
                       desugar
                       #(and (not (:desugar options)) (check-error %))
                       compile-to-graph)]
          (if (:angler.errors/error output)
            (do (println (:angler.errors/message output))
                (System/exit 2))
            (let [[graph] output]
              (cond
                (or (:parse options)
                    (:validate options)
                    (:scope options)
                    (:desugar options)) (pprint output)
                (:count-vertices options) (println (count-vertices graph))
                (:count-edges options) (println (count-edges graph))
                (:print-graph options) (print-graph graph)
                (:sample-from-joint options) (pprint (sample-from-joint graph))
                (:sample-from-prior options) (pprint (sample-from-prior graph))
                :else (pprint output)))))))))
