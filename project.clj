(defproject csv2rdf "0.1.0-SNAPSHOT"
  :description "Library for converting CSV to RDF"
  :url "https://github.com/Swirrl/csv2rdf"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/data.json "0.2.6"]
                 [grafter "0.11.0-drafter-rdf4j"]
                 [com.github.fge/uri-template "0.9"]
                 [org.apache.httpcomponents/httpcore "4.4.9"]
                 [clj-http "3.7.0"]]
  :profiles
  {:dev
   {:dependencies [[org.clojure/test.check "0.9.0"]]
    :resource-paths ["test/resources"]}})
