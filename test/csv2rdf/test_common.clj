(ns csv2rdf.test-common
  (:require [csv2rdf.logging :as logging]
            [clojure.walk :as walk]
            [csv2rdf.metadata.properties :as properties]
            [csv2rdf.http :as http]))

(defn suppress-test-logging [t]
  (logging/suppress-logging (t)))

(defn remove-ns-kws
  "Removes all namespaced map keywords within a structure"
  [x]
  (walk/postwalk (fn [v]
                   (if (map? v)
                     (into {} (keep (fn [kvp]
                                      (when (nil? (namespace (key kvp)))
                                        kvp))
                                    v))
                     v))
                 x))

(defn remove-parent-references [metadata]
  (walk/postwalk (fn [v]
                   (if (map? v)
                     (dissoc v ::properties/parent)
                     v))
                 metadata))

(defrecord TestHttpClient [request-map]
  http/HttpClient
  (http-get [_this uri]
    (if-let [response (get request-map uri)]
      (assoc response :status 200)
      {:status 404 :headers {} :body ""})))