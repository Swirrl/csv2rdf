(ns csv2rdf.csvw
  (:require [csv2rdf.tabular.processing :as processing]
            [grafter.rdf4j.repository :as repo]
            [grafter.rdf :as rdf]
            [csv2rdf.csvw.common :refer [table-group-context table-statements]]
            [csv2rdf.csvw.minimal]
            [csv2rdf.csvw.standard]
            [csv2rdf.metadata.dialect :as dialect]
            [clojure.java.io :as io]
            [csv2rdf.tabular.csv :as csv]
            [grafter.rdf4j.io :as gio]))

(defn csv->rdf->destination [tabular-source metadata-source destination {:keys [minimal?] :as options}]
  (let [{:keys [tables] :as metadata} (processing/get-metadata tabular-source metadata-source)
        mode (if minimal? :minimal :standard)
        table-group-dialect (:dialect metadata)
        output-tables (filter (fn [t] (= false (:suppressOutput t))) tables)
        {:keys [statements] :as ctx} (table-group-context mode metadata)]

    (rdf/add destination statements)

    (doseq [{:keys [url dialect] :as table} output-tables]
      ;;TODO: use any headers from opening tabular file to create dialect
      ;;TODO: pass IO stream instead of reader since dialect defines encoding
      ;;TODO: fix reader to allow same escape and quote chars
      (let [dialect (or dialect table-group-dialect (dialect/get-default-dialect {}))
            options (dialect/dialect->options (assoc dialect :doubleQuote false))]
        (with-open [r (io/reader url)]
          (let [annotated-rows (csv/annotated-rows r table options)]
            (rdf/add destination (table-statements ctx table annotated-rows))))))))

(defn csv->rdf
  ([csv-source metadata-source] (csv->rdf csv-source metadata-source {}))
  ([tabular-source metadata-source options]
   (let [destination (repo/sail-repo)]
     (try
       (csv->rdf->destination tabular-source metadata-source destination options)
       {:errors [] :warnings [] :result (into [] (rdf/statements destination))}
       (catch Exception ex
         {:errors [(.getMessage ex)] :warnings [] :result nil})))))

(defn csv->rdf->file [tabular-source metadata-source dest-file options]
  (with-open [os (io/output-stream dest-file)]
    (let [writer (gio/rdf-writer os :format :ttl)]
      (csv->rdf->destination tabular-source metadata-source writer options))))
