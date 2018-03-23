(ns csv2rdf.metadata.context
  (:require [csv2rdf.util :as util])
  (:import [java.net URI]))

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

