(ns csv2rdf.csvw
  (:require [csv2rdf.tabular.processing :as processing]
            [grafter-2.rdf.protocols :as gproto]
            [csv2rdf.csvw.common :refer [table-group-context table-statements]]
            [csv2rdf.csvw.minimal]
            [csv2rdf.csvw.standard]
            [csv2rdf.csvw.annotated]
            [clojure.java.io :as io]
            [csv2rdf.tabular.csv :as csv]
            [grafter-2.rdf4j.io :as gio]
            [grafter-2.rdf4j.formats :as formats]
            [csv2rdf.metadata.properties :as properties]
            [csv2rdf.util :as util])
  (:import [org.eclipse.rdf4j.rio RDFFormat]))

(defn- get-table-statements [context {:keys [url dialect] :as table} table-group-dialect]
  (let [dialect (or dialect table-group-dialect)
        annotated-rows (csv/annotated-rows url table dialect)]
    (table-statements context table annotated-rows)))

(defn csv->rdf
  "Runs the CSVW process for the given tabular or metadata data sources
  and options.

  `tabular-source` and `metadata-source` can be any of the following
  types:

     - java.io.File
     - java.lang.String
     - java.net.URI
     - java.nio.file.Path (including nio Paths that are inside zip filesystems)

  If metadata-source is non-nil then processing will start from the
  asscociated metadata document, otherwise it will start from
  tabular-source. Returns a lazy sequence of statements containing the
  CSVW output for the specified CSVW mode.

  The processing mode can be specified by the :mode key of the options
  map if provided, otherwise `:standard` mode will be used. Valid
  `:mode` options are:

  - `:standard` this mode corresponds to the standard mode specified
    in the \"Generating RDF from Tabular Data on the Web\" specification.

    It outputs triples for all information gleaned from the cells of the
    tabular data with details of the rows, tables, and table groups.

    This mode yields the most data.

  - `:minimal` this mode corresponds to the minimal mode specified in
    the \"Generating RDF from Tabular Data on the Web\" specification.

    It essentially yields the salient RDF; but omits the tabular structure.

  - `:annotated` a custom mode, not part of the standard, which is
    like `:minimal`, but it also includes RDF data from the CSVW metadata
    json file."
  ([tabular-source metadata-source] (csv->rdf tabular-source metadata-source {}))
  ([tabular-source metadata-source {:keys [mode] :as options}]
   (let [mode (or mode :standard)
         {:keys [tables] :as metadata} (processing/get-metadata tabular-source metadata-source)
         table-group-dialect (:dialect metadata)
         output-tables (remove properties/suppress-output? tables)
         {:keys [statements] :as ctx} (table-group-context mode metadata)
         table-statements (util/liberal-mapcat (fn [table]
                                                 (get-table-statements ctx table table-group-dialect))
                                               output-tables)]
     (concat statements table-statements))))

(defn csv->rdf->destination
  "Run csv->rdf for the given tabular/metadata sources and options then write the resulting
   statements to the given destination. destination must implement
   grafter-2.rdf.protocols/ITripleWriteable."
  [tabular-source metadata-source destination options]
  (gproto/add destination (csv->rdf tabular-source metadata-source options)))

(defn csv->rdf->file
  "Run csv->rdf for the given tabular/metadata source and options then write the resulting
   statements to dest-file."
  [tabular-source metadata-source dest-file options]
  (with-open [os (io/output-stream dest-file)]
    (let [rdf-format (or (formats/->rdf-format dest-file) RDFFormat/TURTLE)
          writer (gio/rdf-writer os :format rdf-format :prefixes nil)]
      (csv->rdf->destination tabular-source metadata-source writer options))))
