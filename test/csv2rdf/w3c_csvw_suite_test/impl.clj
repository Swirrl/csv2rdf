(ns csv2rdf.w3c-csvw-suite-test.impl
  (:require [grafter-2.rdf4j.io :as gio]
            [grafter-2.rdf4j.repository :as repo]
            [csv2rdf.logging :as logging]
            [csv2rdf.csvw :refer [csv->rdf->destination]])
  (:import [org.eclipse.rdf4j.model.util Models]))

(defn is-isomorphic? [expected-statements actual-statements]
  (Models/isomorphic
    (map gio/quad->backend-quad expected-statements)
    (map gio/quad->backend-quad actual-statements)))

(defn test-csv->rdf [tabular-source metadata-source options]
  (let [repo (repo/sail-repo)
        logger (logging/memory-logger)]
    (logging/with-logger logger
      (with-open [destination (repo/->connection repo)]
        (try
          (csv->rdf->destination tabular-source metadata-source nil destination options)
          {:errors [] :warnings @(:warnings logger) :result (into [] (gio/statements destination))}
          (catch Exception ex
            {:errors [(.getMessage ex)] :warnings @(:warnings logger) :result nil}))))))
