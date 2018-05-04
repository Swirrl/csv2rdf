(ns csv2rdf.csvw-test.impl
  (:require [grafter.rdf.io :as gio]
            [grafter.rdf.protocols :as gproto]
            [csv2rdf.http :as http]
            [csv2rdf.xml.datatype.parsing :as parsing])
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