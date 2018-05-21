(ns csv2rdf.csvw
  (:require [csv2rdf.tabular.processing :as processing]
            [grafter.rdf.repository :as repo]
            [grafter.rdf :as rdf]
            [csv2rdf.csvw.common :refer [table-group-context write-table-statements]]
            [csv2rdf.csvw.minimal]
            [csv2rdf.csvw.standard]
            [csv2rdf.metadata.dialect :as dialect]
            [clojure.java.io :as io]
            [csv2rdf.tabular.csv :as csv]
            [grafter.rdf.io :as gio]
            [csv2rdf.logging :as log]
            [csv2rdf.metadata.table :as table]))

(defn csv->rdf->destination [tabular-source metadata-source destination {:keys [mode] :as options}]
  (let [mode (or mode :standard)
        {:keys [tables] :as metadata} (processing/get-metadata tabular-source metadata-source)
        table-group-dialect (:dialect metadata)
        output-tables (remove table/suppress-output? tables)
        {:keys [statements] :as ctx} (table-group-context mode metadata)]

    (rdf/add destination (seq statements))

    (doseq [{:keys [url dialect] :as table} output-tables]
      ;;TODO: use any headers from opening tabular file to create dialect
      ;;TODO: pass IO stream instead of reader since dialect defines encoding
      (let [dialect (or dialect table-group-dialect (dialect/get-default-dialect {}))
            options (dialect/dialect->options dialect)]
        (with-open [r (io/reader url)]
          (let [annotated-rows (csv/annotated-rows r table options)]
            (write-table-statements ctx destination table annotated-rows)))))))

(defn csv->rdf
  ([csv-source metadata-source] (csv->rdf csv-source metadata-source {}))
  ([tabular-source metadata-source options]
   (let [repo (repo/sail-repo)
         logger (log/memory-logger)]
     (log/with-logger
       logger
       (with-open [destination (repo/->connection repo)]
         (try
           (csv->rdf->destination tabular-source metadata-source destination options)
           {:errors [] :warnings @(:warnings logger) :result (into [] (rdf/statements destination))}
           (catch Exception ex
             {:errors [(.getMessage ex)] :warnings @(:warnings logger) :result nil})))))))

(defn csv->rdf->file [tabular-source metadata-source dest-file options]
  (log/with-logger
    (log/->ForwardingLogger)
    (with-open [os (io/output-stream dest-file)]
      (let [writer (gio/rdf-serializer os :format :ttl :prefixes nil)]
        (csv->rdf->destination tabular-source metadata-source writer options)))))
