(defproject angler "0.1.0-SNAPSHOT"
  :description "Angler - a probabilistic programming language"
  :url "https://github.com/mrordinaire/angler"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.cli "0.3.7"]]
  :main ^:skip-aot angler.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
