(ns csv2rdf.csvw-test.impl
  (:require [grafter-2.rdf4j.io :as gio]            
            [grafter-2.rdf4j.repository :as repo]
            [csv2rdf.http :as http]
            [csv2rdf.logging :as logging]
            [csv2rdf.csvw :refer [csv->rdf->destination]])
  (:import [org.eclipse.rdf4j.model.util Models]))

(defn is-isomorphic? [expected-statements actual-statements]
  (Models/isomorphic
    (map gio/quad->backend-quad expected-statements)
    (map gio/quad->backend-quad actual-statements)))

(defrecord TestHttpClient [request-map]
  http/HttpClient
  (http-get [_this uri]
    (if-let [response (get request-map uri)]
      (assoc response :status 200)
      {:status 404 :headers {} :body ""})))

(defn test-csv->rdf [tabular-source metadata-source options]
  (let [repo (repo/sail-repo)
        logger (logging/memory-logger)]
    (logging/with-logger logger
      (with-open [destination (repo/->connection repo)]
        (try
          (csv->rdf->destination tabular-source metadata-source destination options)
          {:errors [] :warnings @(:warnings logger) :result (into [] (gio/statements destination))}
          (catch Exception ex
            {:errors [(.getMessage ex)] :warnings @(:warnings logger) :result nil}))))))
