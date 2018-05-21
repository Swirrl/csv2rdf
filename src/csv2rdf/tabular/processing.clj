(ns csv2rdf.tabular.processing
  (:require [csv2rdf.tabular.metadata :as tmeta]
            [csv2rdf.metadata :as meta]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.tabular.csv :as csv]
            [csv2rdf.metadata.properties :as properties]))

(defn set-table-group-parents [table-group]
  (properties/table-group-hierarchy table-group))

(defn ^{:table-spec "6.1"} from-tabular-source [file-source]
  (set-table-group-parents (tmeta/get-metadata file-source)))

(defn validate-merge-table [validating? dialect {:keys [url] :as user-table}]
  (let [embedded-metadata (csv/extract-embedded-metadata url dialect)
        table-metadata (get-in embedded-metadata [:tables 0])]
    (table/validate-compatible validating? user-table table-metadata)
    (table/compatibility-merge user-table table-metadata)))

;;TODO: check the correct order to expand/inherit metadata and check compatibility with the
;;metadata embedded in the tabular file
;;current order:
;;1. parse from source
;;2. check compatible with embedded metadata
;;3. merge with embedded metadata
;;4. set parents
(defn ^{:table-spec "6.1"} get-metadata [tabular-source metadata-source]
  (cond
    (some? metadata-source)
    (let [{:keys [dialect tables] :as user-table-group} (meta/parse-table-group-from-source metadata-source)
          validating? false
          merged-tables (mapv (fn [table] (validate-merge-table validating? dialect table)) tables)
          user-table-group (assoc user-table-group :tables merged-tables)]
      (set-table-group-parents user-table-group))

    (some? tabular-source)
    (from-tabular-source tabular-source)

    :else
    (throw (IllegalArgumentException. "Either metadata or tabular data source required"))))

