(ns csv2rdf.metadata.context
  (:require [csv2rdf.util :as util]
            [clojure.java.io :as io])
  (:import [java.net URI]
           [java.util Map HashMap]
           [com.github.jsonldjava.core Context]))

(def id-key :id)
(def base-key :base)
(def language-key :language)

(defn make-context [base-uri]
  {:base-uri base-uri :path [] :language nil})

(defn language-code-or-default [{:keys [language] :as context}]
  (or language "und"))

(defn update-from-local-context
  "Updates the context from a parsed local context definition"
  [context local-context]
  (let [key-mapping {base-key     :base-uri
                     language-key :language}
        from-local (util/select-keys-as local-context key-mapping)
        local (util/filter-values some? from-local)]
    (merge context local)))

(defn append-path [context path-element]
  (update context :path conj path-element))

(defn resolve-uri [{:keys [base-uri] :as context} ^URI uri]
  (.resolve base-uri uri))

;;TODO: try to resolve from URI first before falling-back on resource version?
(defn- get-csvw-context []
  (let [doc (util/read-json (io/resource "csvw.json"))]
    (get doc "@context")))

;;TODO: see if this can be done through the public API
(def resolve-iri-method (util/get-declared-method Context "expandIri" [String Boolean/TYPE Boolean/TYPE Map Map]))

(def csvw-context (delay (get-csvw-context)))

(defn expand-uri-string [uri-str]
  (util/invoke-method resolve-iri-method (Context.) [uri-str false false @csvw-context (HashMap.)]))

