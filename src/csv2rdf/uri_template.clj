(ns csv2rdf.uri-template
  (:require [csv2rdf.util :as util])
  (:import [com.github.fge.uritemplate URITemplate]
           [com.github.fge.uritemplate.vars VariableMap]))

(defn make-variable-map [m]
  (let [builder (reduce (fn [b [k v]]
                          (.addScalarValue b (name k) v))
                        (VariableMap/newBuilder)
                        m)]
    (.freeze builder)))

(defn parse-template [template-string]
  ;;TODO: handle/convert exceptions
  (URITemplate. template-string))

(defn try-parse-template [template-string]
  (util/ignore-exceptions (parse-template template-string)))

(defn expand-template [template vars]
  (.toURI template (make-variable-map vars)))

