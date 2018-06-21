(ns csv2rdf.uri-template
  "Functions for parsing and expanding URI templates.

  RFC 6570: https://tools.ietf.org/html/rfc6570"
  (:require [csv2rdf.util :as util])
  (:import [com.github.fge.uritemplate URITemplate]
           [com.github.fge.uritemplate.vars VariableMap VariableMapBuilder]
           [java.net URI]))

(defn- add-variable
  "Adds the given key-value pair to the given map builder."
  [^VariableMapBuilder b [k v]]
  (let [var-name (if (keyword? k) (name k) k)]
    (cond
      (string? v) (.addScalarValue b var-name v)
      (map? v) (.addMapValue b var-name v)
      (coll? v) (.addListValue b var-name v)
      (nil? v) b
      :else (.addScalarValue b var-name v))))

(defn- make-variable-map
  "Converts a clojure map of variable bindings into a VariableMap."
  [m]
  (let [^VariableMapBuilder builder (reduce add-variable (VariableMap/newBuilder) m)]
    (.freeze builder)))

(defn parse-template
  "Parses the given URI template string."
  [template-string]
  (URITemplate. template-string))

(defn try-parse-template
  "Parses the given URI template string and returns nil if invalid."
  [template-string]
  (util/ignore-exceptions (parse-template template-string)))

(defn expand-template
  "Expands the given URITemplate into a URI given the variable bindings in the vars map."
  ^URI
  [^URITemplate template vars]
  (.toURI template (make-variable-map vars)))

(defn expand-template-to-string
  "Expands the given URITemplate into a string given the variable bindings in the vars map."
  [^URITemplate template vars]
  (.toString template (make-variable-map vars)))

(defn expand-template-string
  "Parses the URI templates string then expands it into a URI given the variable bindings in the vars map."
  [template-string vars]
  (expand-template (parse-template template-string) vars))

