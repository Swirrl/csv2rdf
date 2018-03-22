(ns csv2rdf.metadata.table-group
  (:require [csv2rdf.metadata.validator :refer [array-of eq]]
            [csv2rdf.metadata.types :refer [object-property id table-direction note contextual-object]]
            [csv2rdf.metadata.inherited :refer [metadata-of]]
            [csv2rdf.metadata.schema :as schema]
            [csv2rdf.metadata.transformation :as transformation]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.validation :as v]))

(def table-group
  (metadata-of
    {:required {:tables (array-of table/table {:min-length 1})}
     :optional {:dialect         (object-property dialect/dialect)
                :notes           (array-of note)
                :tableDirection  table-direction
                :tableSchema     (object-property schema/schema)
                :transformations (array-of transformation/transformation)
                :id             id
                :type           (eq "TableGroup")}
     :defaults {:tableDirection "auto"}}))

(defn looks-like-table-group-json? [doc]
  (contains? doc "tables"))

(defn inherit-and-apply-defaults [table-group]
  (update table-group :tables (fn [tables]
                                (mapv (fn [t] (table/inherit-and-apply-defaults table-group t)) tables))))

(defn parse-table-group-json [context doc]
  (v/fmap inherit-and-apply-defaults ((contextual-object true table-group) context doc)))