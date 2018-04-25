(ns csv2rdf.metadata.schema
  (:require [csv2rdf.metadata.validator :refer [make-warning make-error invalid chain array-of eq type-eq strict variant]]
            [csv2rdf.metadata.context :refer [append-path]]
            [csv2rdf.metadata.types :refer [object-of object-property link-property column-reference id]]
            [csv2rdf.metadata.inherited :refer [metadata-of] :as inherited]
            [csv2rdf.metadata.column :as column]
            [csv2rdf.validation :as v]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn validate-foreign-key-reference [context reference]
  (let [has-resource? (contains? reference :resource)
        has-reference? (contains? reference :schemaReference)]
    (cond
      (and has-resource? has-reference?)
      (make-error context "Foreign key reference cannot specify both resource and schemaReference keys")

      (or has-resource? has-reference?)
      (v/pure reference)

      :else
      (make-error context "Foreign key reference must specify one of resource or schemaReference keys"))))

(def foreign-key-reference
  (chain
    (strict (object-of
              {:required {:columnReference column-reference}
               :optional {:resource        link-property
                          :schemaReference link-property}}))
    validate-foreign-key-reference))

;;NOTE: should be warning if foreign-key is not an object, but if it is then it should not contain any common property
;;keys
;;TODO: validate using different combinator?
(def foreign-key
  (variant {:object (strict (object-of
                              {:required {:columnReference column-reference
                                          :reference       (object-property foreign-key-reference)}}))}))

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
  (let [schema (inherited/expand-inherit parent-table schema)]
    (update schema :columns (fn [cols]
                              (mapv (fn [c] (column/expand-properties schema c)) cols)))))

(defn ^{:tabular-spec "8.10.4.5.1.1"} merge-columns
  "Creates any extra columns which exist in the embedded schema but which are not defined in the user metadata."
  [user-columns embedded-columns]
  (let [actual-column-count (count embedded-columns)
        user-column-count (count user-columns)
        extra-column-indexes (range user-column-count actual-column-count)]
    (vec (concat user-columns (map column/from-index extra-column-indexes)))))

;;TODO: should this be done before metadata is normalised/expanded/inherited?
(defn ^{:metadata-spec "5.5.1"} validate-compatible [validating? {columns1 :columns :as schema1} {columns2 :columns :as schema2}]
  (let [col1-non-virtual (column/indexed-non-virtual-columns columns1)
        col2-non-virtual (column/indexed-non-virtual-columns columns2)
        common-indexes (set/intersection (set (keys col1-non-virtual)) (set (keys col2-non-virtual)))
        column-validations (map (fn [idx]
                                  (column/validate-compatible validating? idx (get col1-non-virtual idx) (get col2-non-virtual idx)))
                                common-indexes)]
    (v/with-value (v/collect column-validations) nil)))

(defn compatibility-merge [user-schema embedded-schema]
  ;;TODO: validate schemas are compatible
  (update user-schema :columns merge-columns (:columns embedded-schema)))