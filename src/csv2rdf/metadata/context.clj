(ns csv2rdf.metadata.context
  (:require [csv2rdf.util :as util])
  (:import [java.net URI]))

(def id-key :id)
(def base-key (keyword "@base"))
(def language-key :language)

(defn document-uri ^URI [context]
  (:document-uri context))

(defn make-context
  ([metadata-uri] (make-context metadata-uri nil))
  ([metadata-uri table-schema-source]
   {:base-uri metadata-uri
    :document-uri metadata-uri
    :path []
    :language nil
    :table-schema-source table-schema-source}))

(defn language-code-or-default [{:keys [language] :as context}]
  (or language "und"))

(defn ^{:metadata-spec "5.2"} update-from-local-context
  "Updates the context from a parsed local context definition"
  [context local-context]
  (let [;;NOTE: @base key is 'a URL which is resolved against the location of the metadata document to provide the
        ;; base URL for other URLs in the metadata document'
        ^URI context-base-uri (get local-context base-key)
        base-uri (some->> context-base-uri (.resolve (document-uri context)))
        from-local {:base-uri base-uri :language (get local-context language-key)}
        local (util/filter-values some? from-local)]
    (merge context local)))

(defn append-path [context path-element]
  (update context :path conj path-element))

(defn with-document-uri [context ^URI new-document-uri]
  (assoc context :document-uri new-document-uri))

