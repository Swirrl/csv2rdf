(defproject swirrl/csv2rdf "0.4.5-SNAPSHOT"
  :description "Library for converting CSV to RDF"
  :url "https://github.com/Swirrl/csv2rdf"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-tools-deps "0.4.3"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

  :profiles
  {:uberjar {:main csv2rdf.main
             :lein-tools-deps/config {:resolve-aliases [:with-logging]}
             :resource-paths ["profiles/with-logging/resources"]}
   :dev
   {:lein-tools-deps/config {:resolve-aliases [:with-logging :test]}
    :resource-paths ["test/resources" "profiles/with-logging/resources"]}})
