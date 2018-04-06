(ns csv2rdf.csvw-test.impl
  (:require [grafter.rdf4j.io :as rdf4j-io]
            [csv2rdf.http :as http])
  (:import [org.eclipse.rdf4j.model.util Models]))

(defn is-isomorphic? [expected-statements actual-statements]
  (Models/isomorphic
    (map rdf4j-io/->backend-type expected-statements)
    (map rdf4j-io/->backend-type actual-statements)))

(defrecord TestHttpClient [request-map]
  http/HttpClient
  (http-get [_this uri]
    (if-let [response (get request-map uri)]
      (assoc response :status 200)
      {:status 404 :headers {} :body ""})))