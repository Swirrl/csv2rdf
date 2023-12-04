(ns csv2rdf.metadata
  (:require [csv2rdf.metadata.context :refer [make-context]]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.metadata.table-group :as table-group]
            [csv2rdf.metadata.validator :refer [make-error]]
            [csv2rdf.metadata.properties :as properties]
            [csv2rdf.source :as source]
            [clojure.spec.alpha :as s]))

(defn parse-metadata-json [base-uri json]
  (let [context (make-context base-uri)]
    (cond
      (table-group/looks-like-table-group-json? json)
      (properties/set-table-group-parent-references (table-group/parse-table-group-json context json))

      (table/looks-like-table-json? json)
      (properties/set-table-group-parent-references (table/parse-table-json context json))

      :else (make-error context "Expected top-level of metadata document to describe a table or table group"))))

(defn parse-table-group-from-source [source base-uri]
  (let [json (source/get-json source)]
    (parse-metadata-json (or base-uri (source/->uri source)) json)))

(defn ^{:tabular-spec "5.1"} overriding-metadata
  "Construct an 'overriding metadata' document if the user provides both tabular and
   user metadata. If the metadata contains a table definition, set the URI to that for
   the tabular file."
  [tabular-source metadata-source]
  (let [json (source/get-json metadata-source)
        overridden (if (table-group/looks-like-table-group-json? json)
                     json
                     (table/override-table-json-uri json (source/->uri tabular-source)))]
    (source/->MapMetadataSource (source/->uri metadata-source) overridden)))
