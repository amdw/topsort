(defproject topsort "0.1.0-SNAPSHOT"
  :description "Topological sort in Clojure"
  :url "https://github.com/amdw/topsort"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "0.9.0"]]
  :main ^:skip-aot topsort.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
