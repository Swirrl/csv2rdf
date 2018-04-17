(ns csv2rdf.metadata.schema
  (:require [csv2rdf.metadata.validator :refer [make-warning invalid chain array-of eq type-eq]]
            [csv2rdf.metadata.context :refer [append-path]]
            [csv2rdf.metadata.types :refer [object-of object-property link-property column-reference id]]
            [csv2rdf.metadata.inherited :refer [metadata-of] :as inherited]
            [csv2rdf.metadata.column :as column]
            [csv2rdf.validation :as v]
            [clojure.string :as string]))

(defn validate-foreign-key-reference [context reference]
  (if (and (contains? reference :resource) (contains? reference :schemaReference))
    (make-warning context "Foreign key reference cannot specify both resource and schemaReference keys" invalid)
    (v/pure reference)))

(def foreign-key-reference
  (chain
    (object-of
      {:optional {"resource" link-property
                  "schemaReference" link-property
                  "columnReference" column-reference}})
    validate-foreign-key-reference))

(def foreign-key
  (object-of
    {:required {"columnReference" column-reference
                "reference"       (object-property foreign-key-reference)}}))

(defn validate-column-references
  "Validates all the referenced column names under the parent key exist in the given set of column names defined
   by the schema. If the reference column names do not appear directly under the parent key, child-column-name-fn
   should return all ancestor column names referenced within the value associated with key-name. If any referenced
   columns do not exist, a warning is added and the value associated with key-name is removed from the schema as
   though it was not specified."
  ([key-name column-names] (validate-column-references key-name column-names identity))
  ([key-name column-names child-column-name-fn]
   (fn [context schema]
     (if (contains? schema key-name)
       (let [specified-columns (child-column-name-fn (get schema key-name))
             invalid-columns (remove (fn [cn] (contains? column-names cn)) specified-columns)]
         (if (seq invalid-columns)
           (let [msg (format "Referenced column names: %s do not exist in the list of columns" (string/join ", " invalid-columns))]
             (make-warning (append-path context (name key-name)) msg (dissoc schema key-name)))
           (v/pure schema)))
       (v/pure schema)))))

(defn validate-schema-column-references
  "Validator which checks all column name properties within the schema reference columns defined in the columns collection.
   Any components which reference non-existent columns are removed."
  [context {:keys [columns] :as schema}]
  (let [column-names (into #{} (map :name columns))
        validator (chain
                    (validate-column-references :primaryKey column-names)
                    (validate-column-references :rowTitles column-names)
                    (validate-column-references :foreignKeys column-names (fn [foreign-keys] (mapcat :columnReference foreign-keys))))]
    (validator context schema)))

(def schema
  (chain
    (metadata-of
      {:optional {:columns     column/columns
                  :foreignKeys (array-of foreign-key)      ;;TODO: validate foreign keys
                  :primaryKey  column-reference            ;;TODO: validators MUST check that each row has a unique combination of values of cells in the indicated columns
                  :rowTitles   column-reference
                  :id         id
                  :type       (type-eq "Schema")}})
    validate-schema-column-references))

(defn expand-properties
  "Expands all properties for this schema by inheriting any undefined properties from its parent table."
  [parent-table schema]
  (let [schema (inherited/inherit parent-table schema)]
    (update schema :columns (fn [cols]
                              (mapv (fn [c] (column/expand-properties schema c)) cols)))))

(defn compatibility-merge [user-schema embedded-schema]
  ;;TODO: validate schemas are compatible
  (or user-schema embedded-schema))