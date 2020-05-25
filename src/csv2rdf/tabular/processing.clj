(ns csv2rdf.tabular.processing
  (:require [csv2rdf.tabular.metadata :as tmeta]
            [csv2rdf.metadata :as meta]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.tabular.csv :as csv]
            [csv2rdf.metadata.properties :as properties]))

(defn set-table-group-parents [table-group]
  (properties/set-table-group-parent-references table-group))

(defn ^{:tabular-spec "6.1"} from-tabular-source [file-source]
  (set-table-group-parents (tmeta/get-metadata file-source)))

(defn- validate-merge-table [validating? {:keys [url] :as user-table}]
  (let [embedded-metadata (csv/extract-embedded-metadata url (properties/dialect user-table) (properties/lang user-table))
        table-metadata (get-in embedded-metadata [:tables 0])]
    (table/validate-compatible validating? user-table table-metadata)
    (table/compatibility-merge user-table table-metadata)))

(defn- from-metadata-source [metadata-source]
  (let [{:keys [tables] :as user-table-group} (meta/parse-table-group-from-source metadata-source)
        validating? false
        merged-tables (mapv (fn [table] (validate-merge-table validating? table)) tables)
        merged-table-group (assoc user-table-group :tables merged-tables)]
    (set-table-group-parents merged-table-group)))

(defn ^{:tabular-spec "6.1"} get-metadata
  "Retrieves and resolves the metadata given either a tabular data source or metadata source. If user metadata
  is provided, each referenced table definition is validated against the corresponding tabular data file."
  [tabular-source metadata-source]
  (cond
    (and (some? tabular-source) (some? metadata-source))
    (from-metadata-source (meta/overriding-metadata tabular-source metadata-source))

    (some? metadata-source)
    (from-metadata-source metadata-source)

    (some? tabular-source)
    (from-tabular-source tabular-source)

    :else
    (throw (IllegalArgumentException. "Either metadata or tabular data source required"))))

