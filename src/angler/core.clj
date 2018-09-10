(ns angler.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def cli-options
  [["-h" "--help"]])

(defn usage
  [option-summary]
  (->> ["Angler - a probabilistic programming language"
        ""
        "Usage: program-name [options] input-files|-"
        ""
        "Options:"
        option-summary]
       (string/join \newline)))

(defn process-options
  [{:keys [options summary errors] :as parsed-options}]
  (cond
    (:help options) (do (println (usage summary))
                        (System/exit 1))
    errors (do (println (str "Unable to parse command line arguments:\n\n"
                             (string/join \newline errors)))
               (System/exit 1))
    :else parsed-options))

(defn -main
  [& args]
  (let [{:keys [options arguments]} (process-options (parse-opts args cli-options))]
    (doseq [f arguments]
      (with-open [r (java.io.PushbackReader.  (if (= "-" f) *in* (io/reader f)))]
        (println
          (loop [exps []]
            (let [e (edn/read {:readers {}, :eof ::done} r)]
              (if (= ::done e)
                exps
                (recur (conj exps e))))))))))
