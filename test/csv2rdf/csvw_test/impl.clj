(ns csv2rdf.csvw-test.impl
  (:require [grafter.rdf.io :as gio]
            [grafter.rdf.protocols :as gproto]
            [grafter.rdf :as rdf]
            [grafter.rdf.repository :as repo]
            [csv2rdf.http :as http]
            [csv2rdf.xml.datatype.parsing :as parsing]
            [csv2rdf.logging :as logging]
            [csv2rdf.csvw :refer [csv->rdf->destination]])
  (:import [org.openrdf.model.util Models]))

;;Grafter does not support the floating literals INF or -INF. Replace the implementations for these types
(remove-method gio/literal-datatype->type "http://www.w3.org/2001/XMLSchema#double")
(remove-method gio/literal-datatype->type "http://www.w3.org/2001/XMLSchema#float")

(defmethod gio/literal-datatype->type "http://www.w3.org/2001/XMLSchema#double" [literal]
  (parsing/parse "double" (gproto/raw-value literal)))

(defmethod gio/literal-datatype->type "http://www.w3.org/2001/XMLSchema#float" [literal]
  (parsing/parse "float" (gproto/raw-value literal)))

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