(ns csv2rdf.metadata.table
  (:require [csv2rdf.metadata.validator :refer [array-of bool type-eq strict]]
            [csv2rdf.metadata.types :refer [link-property note table-direction object-property id contextual-object]]
            [csv2rdf.metadata.inherited :refer [metadata-of]]
            [csv2rdf.metadata.schema :as schema]
            [csv2rdf.metadata.transformation :as transformation]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.logging :as logging])
  (:import [java.net URI]))

(defn suppress-output? [table]
  (boolean (:suppressOutput table)))

;;NOTE: link properties have a default value if the property is invalid
;;CSVW tests (see test 103) expect an error to be raised if the table URL is invalid
(def table-url (strict link-property))

(def table
  (metadata-of
    {:required {:url table-url}
     :optional {:dialect         dialect/dialect
                :notes           (array-of note)
                :suppressOutput  bool
                :tableDirection  table-direction
                :tableSchema     (object-property schema/schema)
                :transformations (array-of transformation/transformation)
                :id              id
                :type            (type-eq "Table")}}))

(defn columns [table]
  (get-in table [:tableSchema :columns]))

(defn into-table-group [table]
  {:tables [table]})

(defn looks-like-table-json? [doc]
  (contains? doc "url"))

(defn parse-table-json [context doc]
  (let [t ((contextual-object true table) context doc)]
    (into-table-group t)))

(defn from-schema [table-uri schema]
  {:url table-uri
   :tableSchema schema})

(defn ^{:metadata-spec "5.4.3"} validate-compatible [validating? {^URI uri1 :url schema1 :tableSchema :as table1} {^URI uri2 :url schema2 :tableSchema :as table2}]
  ;;TODO: normalise URIs during parsing?
  (when-not (= (.normalize uri1) (.normalize uri2))
    (logging/log-warning (format "Table URIs %s and %s not equal after normalisation" uri1 uri2)))
  (schema/validate-compatible validating? schema1 schema2))

(defn compatibility-merge [user-table embedded-table]
  ;;TODO: validate tables are compatible
  (let [notes (vec (concat (:notes user-table) (:notes embedded-table)))
        schema (schema/compatibility-merge (:tableSchema user-table) (:tableSchema embedded-table))]
    (-> (merge embedded-table user-table)
        (assoc :notes notes)
        (assoc :tableSchema schema))))
