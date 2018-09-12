(ns angler.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [angler.errors :refer [checked-pipeline->]]
            [angler.passes.desugar :refer [desugar]]
            [angler.passes.parse :refer [parse]]
            [angler.passes.validate :refer [validate]])
  (:gen-class))

(def cli-options
  [["-h" "--help"]])

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
            output (checked-pipeline->
                     parse-result
                     check-error validate
                     check-error desugar)]
        (if (:angler.errors/error parse-result)
          (do (println (:angler.errors/message parse-result))
              (System/exit 2))
          (pprint output))))))
