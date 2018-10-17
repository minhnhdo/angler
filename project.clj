(defproject angler "1.2.1"
  :description "Angler - a probabilistic programming language"
  :url "https://github.com/mrordinaire/angler"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.cli "0.3.7"]
                 [net.mikera/core.matrix "0.62.0"]
                 [anglican "1.0.0"]]
  :main ^:skip-aot angler.core
  :target-path "target/%s"
  :aot [angler.errors]
  :profiles {:uberjar {:aot :all}})
