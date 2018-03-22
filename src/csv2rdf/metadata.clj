(ns csv2rdf.metadata
  (:require [csv2rdf.metadata.validator :refer :all]
            [csv2rdf.metadata.context :refer :all]
            [csv2rdf.metadata.json :refer :all]
            [csv2rdf.metadata.types :refer :all]
            [csv2rdf.metadata.dialect :refer [dialect]]
            [csv2rdf.metadata.transformation :as transformation]
            [csv2rdf.metadata.inherited :refer [metadata-of]]
            [csv2rdf.metadata.schema :as schema]
            [csv2rdf.util :as util]))

(def table-defaults
  {"suppressOutput" false
   "tableDirection" "auto"})

(def table
  (metadata-of
    {:required {"url" link-property}
     :optional {"notes" (array-of note)
                "suppressOutput" bool
                "tableDirection" table-direction
                "tableSchema" (object-property schema/schema)
                "transformations" (array-of transformation/transformation)
                "@id" id
                "@type" (eq "Table")}}))

(def table-group-defaults
  {"tableDirection" "auto"})

(def table-group
  (metadata-of
    {:required {"tables" (array-of table {:min-length 1})}
     :optional {"dialect"         (object-property dialect)
                "notes"           (array-of note)
                "tableDirection"  table-direction
                "tableSchema"     (object-property schema/schema)
                "transformations" (array-of transformation/transformation)
                "@id"             id
                "@type"           (eq "TableGroup")}}))

(defn parse-file [f]
  (let [json (util/read-json f)
        context (make-context (.toURI f))]
    ((contextual-object true table) context json)))