(ns csv2rdf.csvw
  (:require [csv2rdf.tabular.processing :as processing]
            [grafter.rdf :as rdf]
            [csv2rdf.csvw.common :refer [table-group-context table-statements]]
            [csv2rdf.csvw.minimal]
            [csv2rdf.csvw.standard]
            [clojure.java.io :as io]
            [csv2rdf.tabular.csv :as csv]
            [grafter.rdf.io :as gio]
            [csv2rdf.metadata.properties :as properties]
            [csv2rdf.util :as util]))

(defn get-table-statements [context {:keys [url dialect] :as table} table-group-dialect]
  (let [dialect (or dialect table-group-dialect)]
    (let [annotated-rows (csv/annotated-rows url table dialect)]
      (table-statements context table annotated-rows))))

(defn csv->rdf
  ([csv-source metadata-source] (csv->rdf csv-source metadata-source {}))
  ([tabular-source metadata-source {:keys [mode] :as options}]
   (let [mode (or mode :standard)
         {:keys [tables] :as metadata} (processing/get-metadata tabular-source metadata-source)
         table-group-dialect (:dialect metadata)
         output-tables (remove properties/suppress-output? tables)
         {:keys [statements] :as ctx} (table-group-context mode metadata)
         table-statements (util/mapcat-seq (fn [table]
                                              (get-table-statements ctx table table-group-dialect))
                                           output-tables)]
     (concat statements table-statements))))

(defn csv->rdf->destination [tabular-source metadata-source destination options]
  (rdf/add destination (csv->rdf tabular-source metadata-source options)))

(defn csv->rdf->file [tabular-source metadata-source dest-file options]
  (with-open [os (io/output-stream dest-file)]
    (let [writer (gio/rdf-serializer os :format :ttl :prefixes nil)]
      (csv->rdf->destination tabular-source metadata-source writer options))))
