(ns csv2rdf.tabular.processing
  (:require [csv2rdf.tabular.metadata :as tmeta]
            [csv2rdf.metadata :as meta]
            [csv2rdf.validation :as v]))

(defn ^{:table-spec "6.1"} from-tabular-source [file-source]
  (tmeta/get-metadata file-source))

(defn ^{:table-spec "6.1"} from-metadata-source [metadata-source]
  (let [metadata-validation (meta/parse-table-group-from-source metadata-source)]
    (v/get-value metadata-validation "Invalid metadata document")))

(defn ^{:table-spec "6.1"} get-metadata [tabular-source metadata-source]
  (cond
    (some? metadata-source)
    (from-metadata-source metadata-source)

    (some? tabular-source)
    (from-tabular-source tabular-source)

    :else
    (throw (IllegalArgumentException. "Either metadata or tabular data source required"))))

