(ns csv2rdf.metadata.table
  (:require [csv2rdf.metadata.validator :refer [array-of bool eq]]
            [csv2rdf.metadata.types :refer [link-property note table-direction object-property id contextual-object]]
            [csv2rdf.metadata.inherited :refer [metadata-of] :as inherited]
            [csv2rdf.metadata.schema :as schema]
            [csv2rdf.metadata.transformation :as transformation]
            [csv2rdf.validation :as v]))

(def table-defaults
  {:suppressOutput false
   :tableDirection "auto"})

(def table
  (metadata-of
    {:required {:url link-property}
     :optional {:notes           (array-of note)
                :suppressOutput  bool
                :tableDirection  table-direction
                :tableSchema     (object-property schema/schema)
                :transformations (array-of transformation/transformation)
                :id             id
                :type           (eq "Table")}}))

(defn columns [table]
  (get-in table [:tableSchema :columns]))

(defn into-table-group [table]
  {:tables [table]})

(defn looks-like-table-json? [doc]
  (contains? doc "url"))

(defn expand-children [table]
  (update table :tableSchema (fn [s] (schema/expand-properties table s))))

(defn expand-properties
  "Expands all properties for this table by inheriting any undefined inherited properties from its parent table group."
  [parent-table-group table]
  (let [table (inherited/inherit-with-defaults parent-table-group (merge table-defaults table))]
    (expand-children table)))

(defn parse-table-json [context doc]
  (v/fmap (fn [t]
            (into-table-group t))
          ((contextual-object true table) context doc)))

(defn from-schema [table-uri schema]
  {:url table-uri
   :tableSchema schema})

(defn compatibility-merge [user-table embedded-table]
  ;;TODO: validate tables are compatible
  (let [notes (vec (concat (:notes user-table) (:notes embedded-table)))
        schema (schema/compatibility-merge (:tableSchema user-table) (:tableSchema embedded-table))]
    (-> (merge embedded-table user-table)
        (assoc :notes notes)
        (assoc :tableSchema schema))))
