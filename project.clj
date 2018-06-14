(defproject csv2rdf "0.2.5-SNAPSHOT"
  :description "Library for converting CSV to RDF"
  :url "https://github.com/Swirrl/csv2rdf"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.json "0.2.6"]
                 [grafter "0.11.2"]
                 [com.github.fge/uri-template "0.9"]
                 [org.apache.httpcomponents/httpcore "4.4.9"]
                 [clj-http "3.7.0"]
                 [org.clojure/tools.cli "0.3.7"]
                 [org.clojure/tools.logging "0.4.1"]
                 [org.slf4j/slf4j-api "1.7.25"]]
  
  :profiles
  {:uberjar {:main csv2rdf.main
             :dependencies [[org.apache.logging.log4j/log4j-api "2.11.0"]
                            [org.apache.logging.log4j/log4j-core "2.11.0"]
                            [org.apache.logging.log4j/log4j-slf4j-impl "2.11.0"]]}
   :dev
   {:dependencies [[org.clojure/test.check "0.9.0"]
                   [org.clojure/data.csv "0.1.4"]
                   [org.apache.logging.log4j/log4j-api "2.11.0"]
                   [org.apache.logging.log4j/log4j-core "2.11.0"]
                   [org.apache.logging.log4j/log4j-slf4j-impl "2.11.0"]]
    :resource-paths ["test/resources"]}})
