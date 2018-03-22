(ns csv2rdf.metadata.table-group
  (:require [csv2rdf.metadata.validator :refer [array-of eq]]
            [csv2rdf.metadata.types :refer [object-property id table-direction note]]
            [csv2rdf.metadata.inherited :refer [metadata-of]]
            [csv2rdf.metadata.schema :as schema]
            [csv2rdf.metadata.transformation :as transformation]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.metadata.table :as table]))

(def table-group-defaults
  {"tableDirection" "auto"})

(def table-group
  (metadata-of
    {:required {"tables" (array-of table/table {:min-length 1})}
     :optional {"dialect"         (object-property dialect/dialect)
                "notes"           (array-of note)
                "tableDirection"  table-direction
                "tableSchema"     (object-property schema/schema)
                "transformations" (array-of transformation/transformation)
                "@id"             id
                "@type"           (eq "TableGroup")}}))

