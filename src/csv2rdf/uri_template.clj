(ns csv2rdf.uri-template
  (:require [csv2rdf.util :as util])
  (:import [com.github.fge.uritemplate URITemplate]
           [com.github.fge.uritemplate.vars VariableMap]))

(defn make-variable-map [m]
  (let [builder (reduce (fn [b [k v]]
                          (let [var-name (if (keyword? k) (name k) k)]
                            (cond
                              (map? v) (.addMapValue b var-name v)
                              (coll? v) (.addListValue b var-name v)
                              :else (.addScalarValue b var-name v))))
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

(defn expand-template-to-string [template vars]
  (.toString template (make-variable-map vars)))

(defn expand-template-string [template-string vars]
  (expand-template (parse-template template-string) vars))

