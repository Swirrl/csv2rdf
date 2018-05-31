(ns csv2rdf.tabular.processing
  (:require [csv2rdf.tabular.metadata :as tmeta]
            [csv2rdf.metadata :as meta]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.tabular.csv :as csv]
            [csv2rdf.metadata.properties :as properties]))

(defn set-table-group-parents [table-group]
  (properties/set-table-group-parent-references table-group))

(defn ^{:table-spec "6.1"} from-tabular-source [file-source]
  (set-table-group-parents (tmeta/get-metadata file-source)))

(defn validate-merge-table [validating? dialect {:keys [url] :as user-table}]
  (let [embedded-metadata (csv/extract-embedded-metadata url dialect)
        table-metadata (get-in embedded-metadata [:tables 0])]
    (table/validate-compatible validating? user-table table-metadata)
    (table/compatibility-merge user-table table-metadata)))

(defn ^{:table-spec "6.1"} get-metadata
  "Retrieves and resolves the metadata given either a tabular data source or metadata source. If user metadata
  is provided, each referenced table definition is validated against the corresponding tabular data file."
  [tabular-source metadata-source]
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

