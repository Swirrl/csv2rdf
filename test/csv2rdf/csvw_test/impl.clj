(ns csv2rdf.csvw-test.impl
  (:require [grafter.rdf.io :as gio]
            [grafter.rdf :as rdf]
            [grafter.rdf.repository :as repo]
            [csv2rdf.http :as http]
            [csv2rdf.logging :as logging]
            [csv2rdf.csvw :refer [csv->rdf->destination]])
  (:import [org.openrdf.model.util Models]))

(defn is-isomorphic? [expected-statements actual-statements]
  (Models/isomorphic
    (map gio/IStatement->sesame-statement expected-statements)
    (map gio/IStatement->sesame-statement actual-statements)))

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
          {:errors [] :warnings @(:warnings logger) :result (into [] (rdf/statements destination))}
          (catch Exception ex
            {:errors [(.getMessage ex)] :warnings @(:warnings logger) :result nil}))))))