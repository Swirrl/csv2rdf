(defproject csv2rdf "0.1.0-SNAPSHOT"
  :description "Library for converting CSV to RDF"
  :url "https://github.com/Swirrl/csv2rdf"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [grafter "0.11.0-drafter-rdf4j"]]
  :profiles
  {:dev
   {:resource-paths ["test/resources"]}})
