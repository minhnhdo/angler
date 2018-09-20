(ns angler.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [angler.errors :refer [checked->]]
            [angler.passes.compile :refer [compile-to-graph
                                           edge-vector->adjacency-vector
                                           topological-sort]]
            [angler.passes.desugar :refer [desugar]]
            [angler.passes.parse :refer [parse]]
            [angler.passes.scope :refer [scope]]
            [angler.passes.validate :refer [validate]]
            [angler.types :refer [count-edges count-vertices print-graph]])
  (:gen-class))

(def cli-options
  [["-h" "--help" "Print this help message" :default false]
   [nil "--parse" "Run upto parsing only" :default false]
   [nil "--validate" "Run upto validation only" :default false]
   [nil "--scope" "Run upto scoping only" :default false]
   [nil "--desugar" "Run upto desugaring only" :default false]
   [nil "--count-vertices" "Print the number of vertices in the resulting graph"
    :default false]
   [nil "--count-edges" "Print the number of edges in the resulting graph"
    :default false]
   [nil "--print-graph" "Print the resulting graph" :default false]])

(defn usage
  [option-summary]
  (->> ["Angler - a probabilistic programming language"
        ""
        "Usage: program-name [options] [input-files|-]..."
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

(defn check-error
  [r]
  (not (:angler.errors/error r)))

(defn -main
  [& args]
  (let [{:keys [options arguments]} (process-options (parse-opts args cli-options))]
    (doseq [f arguments]
      (let [parse-result (with-open [r (if (= "-" f)
                                         *in*
                                         (java.io.PushbackReader. (io/reader f)))]
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
          (let [[graph compiled-exp] output]
            (cond
              (:count-vertices options) (println (count-vertices graph))
              (:count-edges options) (println (count-edges graph))
              (:print-graph options) (print-graph graph)
              :else (let [[{:keys [V A]}] output]
                      (pprint output)
                      (pprint
                        (topological-sort
                          V (edge-vector->adjacency-vector (vec A))))))))))))
