(ns csv2rdf.tabular.processing
  (:require [csv2rdf.tabular.metadata :as tmeta]
            [csv2rdf.metadata :as meta]
            [csv2rdf.validation :as v]
            [csv2rdf.metadata.table-group :as table-group]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.tabular.csv :as csv]))

(defn ^{:table-spec "6.1"} from-tabular-source [file-source]
  (table-group/expand-properties (tmeta/get-metadata file-source)))

(defn check-table-metadata-compatible [dialect {:keys [url] :as table}]
  ;;TODO: fix CSV parser!
  ;;TODO: only return table metadata from extract-embedded-metadata
  (let [embedded-metadata (csv/extract-embedded-metadata url (assoc dialect :doubleQuote false))
        table-metadata (get-in embedded-metadata [:tables 0])]
    (table/compatibility-merge table table-metadata)))

(defn ^{:table-spec "6.1"} get-metadata [tabular-source metadata-source]
  (cond
    (some? metadata-source)
    (let [meta-validation (meta/parse-table-group-from-source metadata-source)]
      (v/fmap (fn [{:keys [dialect] :as user-table-group}]
                (let [checked (update user-table-group :tables (fn [tables]
                                                                 (mapv #(check-table-metadata-compatible dialect %) tables)))]
                  (table-group/expand-properties checked)))
              meta-validation))

    (some? tabular-source)
    (v/pure (from-tabular-source tabular-source))

    :else
    (throw (IllegalArgumentException. "Either metadata or tabular data source required"))))

