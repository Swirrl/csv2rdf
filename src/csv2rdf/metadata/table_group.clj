(ns csv2rdf.metadata.table-group
  (:require [csv2rdf.metadata.validator :refer [array-of type-eq chain make-error]]
            [csv2rdf.metadata.types :refer [object-property id table-direction note contextual-object]]
            [csv2rdf.metadata.inherited :refer [metadata-of]]
            [csv2rdf.metadata.schema :as schema]
            [csv2rdf.metadata.transformation :as transformation]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.metadata.inherited :as inherited]))

(def table-group-defaults
  {:tableDirection "auto"})

(defn validate-tables [context tables]
  (if (empty? tables)
    (make-error context "Table group must contain at least one table definition")
    tables))

(def table-group
  (metadata-of
    {:required {:tables (chain (array-of table/table) validate-tables)}
     :optional {:dialect         (object-property dialect/dialect)
                :notes           (array-of note)
                :tableDirection  table-direction
                :tableSchema     (object-property schema/schema)
                :transformations (array-of transformation/transformation)
                :id              id
                :type            (type-eq "TableGroup")}}))

(defn looks-like-table-group-json? [doc]
  (contains? doc "tables"))

(defn expand-properties
  "Expands all properties for this table group by expanding the properties of its contained tables. There is no
   parent, so any inherited properties not specified directly will use their default values."
  [table-group]
  (let [with-defaults (inherited/inherit-defaults table-group)]
    (update with-defaults :tables (fn [tables]
                                  (mapv (fn [t] (table/expand-properties with-defaults t)) tables)))))

(defn parse-table-group-json [context doc]
  ((contextual-object true table-group) context doc))

(defn from-table
  "Creates a table group from the given table"
  [table]
  {:tables [table]})