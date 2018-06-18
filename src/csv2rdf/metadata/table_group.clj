(ns csv2rdf.metadata.table-group
  (:require [csv2rdf.metadata.validator :refer [array-of type-eq chain make-error type-error-message]]
            [csv2rdf.json :as mjson]
            [csv2rdf.metadata.types :refer [object-property id table-direction note contextual-object]]
            [csv2rdf.metadata.inherited :refer [metadata-of]]
            [csv2rdf.metadata.schema :as schema]
            [csv2rdf.metadata.transformation :as transformation]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.metadata.context :refer [append-path id-key]]))

(defn tables [context x]
  (if (mjson/array? x)
    (let [tables ((array-of table/table) context x)]
      (if (empty? tables)
        (make-error context "Table group must contain at least one table definition")
        tables))
    (make-error context (type-error-message #{:array} (mjson/get-json-type x)))))

(defn- find-schema-by-table-url
  "Finds the schema for the table with the given URL in a collection of tables. Raises a validation error
   if there is not exactly one matching table."
  [context tables url]
  (let [matches (into [] (filter (fn [table] (= url (:url table))) tables))]
    (case (count matches)
      0 (make-error context (format "Invalid table reference %s" url))
      1 (:tableSchema (first matches))
      (make-error context (format "Multiple tables found with url %s" url)))))

(defn- find-schema-by-id
  "Finds the schema with the given @id in a collection of table definitions. Raises a validation error
   if there is not exactly one matching schema."
  [context tables id]
  (let [matches (into [] (filter (fn [schema]
                                   (= id (get schema id-key)))
                                 (map :tableSchema tables)))]
    (case (count matches)
      0 (make-error context (format "No table schema found with @id %s" id))
      1 (first matches)
      (make-error context (format "Multiple table schemas found with url %s" id)))))

(defn validate-foreign-key-references
  "Validates that each foreign key reference in a table group is valid by checking the referenced table/schema and
   foreign column is declared within the group."
  [context {:keys [tables] :as table-group}]
  (doseq [[table-index table] (map-indexed vector tables)]
    (when-let [foreign-keys (get-in table [:tableSchema :foreignKeys])]
      (doseq [[fk-index fk] (map-indexed vector foreign-keys)]
        (let [{{:keys [resource schemaReference columnReference]} :reference} fk
              ref-schema (if (some? resource)
                           (find-schema-by-table-url (append-path context [:tables table-index :tableSchema :foreignKeys fk-index :resource])
                                                     tables
                                                     resource)
                           (find-schema-by-id (append-path context [:tables table-index :tableSchema :foreignKeys fk-index :schemaReference])
                                              tables
                                              schemaReference))
              ref-schema-columns (into #{} (remove nil? (map :name (:columns ref-schema))))]
          (doseq [[col-ref-index col-ref] (map-indexed vector columnReference)]
            (when-not (contains? ref-schema-columns col-ref)
              (let [col-ref-path [:tables table-index :tableSchema :foreignKeys fk-index :columnReference col-ref-index]]
                (make-error (append-path context col-ref-path) (format "Invalid column reference '%s'" col-ref)))))))))
  table-group)

(def table-group
  (chain
    (metadata-of
      {:required {:tables tables}
       :optional {:dialect         (object-property dialect/dialect)
                  :notes           (array-of note)
                  :tableDirection  table-direction
                  :tableSchema     (object-property schema/schema)
                  :transformations (array-of transformation/transformation)
                  :id              id
                  :type            (type-eq "TableGroup")}})
    validate-foreign-key-references))

(defn looks-like-table-group-json? [doc]
  (contains? doc "tables"))

(defn parse-table-group-json [context doc]
  ((contextual-object true table-group) context doc))

(defn from-table
  "Creates a table group from the given table"
  [table]
  {:tables [table]})
