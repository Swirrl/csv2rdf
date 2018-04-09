(ns csv2rdf.csvw-test.impl
  (:require [grafter.rdf.io :as gio]
            [csv2rdf.http :as http])
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