(ns csv2rdf.metadata.schema
  (:require [csv2rdf.metadata.validator :refer [make-warning make-error invalid chain array-of type-eq strict variant
                                                type-error-message]]
            [csv2rdf.json :as mjson]
            [csv2rdf.metadata.context :refer [append-path]]
            [csv2rdf.metadata.types :refer [object-of object-property link-property column-reference id]]
            [csv2rdf.metadata.inherited :refer [metadata-of]]
            [csv2rdf.metadata.column :as column]
            [clojure.string :as string]
            [clojure.set :as set]
            [csv2rdf.logging :as logging]
            [csv2rdf.util :as util]))

(defn foreign-key-reference [context x]
  (if (mjson/object? x)
    (let [obj x
          permitted-keys ["columnReference" "resource" "schemaReference"]
          [has-column-ref? has-resource? has-schema-ref?] (mapv #(contains? obj %) permitted-keys)
          extra-keys (apply dissoc obj permitted-keys)]
      (cond
        (seq extra-keys)
        (make-error context "Foreign key references can only contain the keys 'columnReference' and 'resource' or 'schemaReference'")

        (not has-column-ref?)
        (make-error context "Foreign key references must specify columnReference key")

        (and has-resource? has-schema-ref?)
        (make-error context "Foreign key reference cannot specify both resource and schemaReference keys")

        (or has-resource? has-schema-ref?)
        (let [column-ref ((strict column-reference) (append-path context "columnReference") (get obj "columnReference"))]
          (if has-resource?
            {:columnReference column-ref
             :resource ((strict link-property) (append-path context "resource") (get obj "resource"))}
            {:columnReference column-ref
             :schemaReference ((strict link-property) (append-path context "schemaReference") (get obj "schemaReference"))}))

        :else
        (make-error context "Foreign key reference must specify one of resource or schemaReference keys")))
    (make-error context (type-error-message #{:object} (mjson/get-json-type x)))))

(defn foreign-key-object [context obj]
  (let [[{:strs [columnReference reference] :as required} extra] (util/partition-keys obj ["columnReference" "reference"])]
    (cond
      (not= 2 (count required))
      (make-error context "Foreign key must contain both columnReference and reference keys")

      (seq extra)
      (make-error context "Foreign key must only contain columnReference and reference keys")

      :else
      {:columnReference ((strict column-reference) (append-path context "columnReference") columnReference)
       :reference (foreign-key-reference (append-path context "reference") reference)})))

;;NOTE: should be warning if foreign-key is not an object, but if it is then it should not contain any common property
;;keys
(def foreign-key (variant {:object foreign-key-object}))

(defn validate-column-references
  "Validates all the referenced column names under the parent key exist in the given set of column names defined
   by the schema. If the reference column names do not appear directly under the parent key, child-column-name-fn
   should return all ancestor column names referenced within the value associated with key-name. If any referenced
   columns do not exist, a warning is added and the value associated with key-name is removed from the schema as
   though it was not specified."
  ([schema context key-name column-names required-property?] (validate-column-references schema context key-name column-names required-property? identity))
  ([schema context key-name column-names required-property? child-column-name-fn]
   (if (contains? schema key-name)
     (let [specified-columns (child-column-name-fn (get schema key-name))
           invalid-columns (remove (fn [cn] (contains? column-names cn)) specified-columns)]
       (if (seq invalid-columns)
         (let [msg (format "Referenced column names: %s do not exist in the list of columns" (string/join ", " invalid-columns))]
           (if required-property?
             (make-error (append-path context (name key-name)) msg)
             (make-warning (append-path context (name key-name)) msg (dissoc schema key-name))))
         schema))
     schema)))

(defn validate-schema-column-references
  "Validator which checks all column name properties within the schema reference columns defined in the columns collection.
   Any components which reference non-existent columns are removed."
  [context {:keys [columns] :as schema}]
  (let [column-names (into #{} (remove nil? (map :name columns)))]
    (-> schema
        (validate-column-references context :primaryKey column-names false)
        (validate-column-references context :rowTitles column-names false)
        (validate-column-references context :foreignKeys column-names true (fn [foreign-keys] (mapcat :columnReference foreign-keys))))))

(def schema-def
  (metadata-of
    {:optional {:columns     column/columns
                :foreignKeys (array-of foreign-key)
                :primaryKey  column-reference            ;;TODO: validators MUST check that each row has a unique combination of values of cells in the indicated columns
                :rowTitles   column-reference
                :id         id
                :type       (type-eq "Schema")}}))

(defn schema [context x]
  (let [schema (schema-def context x)]
    (validate-schema-column-references context schema)
    schema))

(defn ^{:tabular-spec "8.10.4.5.1.1"} merge-columns
  "Creates any extra columns which exist in the embedded schema but which are not defined in the user metadata."
  [user-columns embedded-columns]
  (let [actual-column-count (count embedded-columns)
        user-column-count (count user-columns)
        extra-column-indexes (range user-column-count actual-column-count)]
    (vec (concat user-columns (map column/from-index extra-column-indexes)))))

(defn ^{:metadata-spec "5.5.1"} validate-compatible [validating? {columns1 :columns :as schema1} {columns2 :columns :as schema2}]
  ;;NOTE: it is legal for the metadata table to only include the URL of the tabular file and not include a schema
  ;;in this case, consider the schemas trivially compatible
  (when (and (some? schema1) (some? schema2))
    (let [col1-non-virtual (column/indexed-non-virtual-columns columns1)
          col2-non-virtual (column/indexed-non-virtual-columns columns2)
          common-indexes (set/intersection (set (keys col1-non-virtual)) (set (keys col2-non-virtual)))]
      ;;Two schemas are compatible if they have the same number of non-virtual column descriptions,
      (when-not (= (count col1-non-virtual) (count col2-non-virtual))
        (logging/log-warning "Schemas have different number of non-virtual columns"))

      ;;and the non-virtual column descriptions at the same index within each are compatible with each other
      (doseq [idx common-indexes]
        (let [col1 (get col1-non-virtual idx)
              col2 (get col2-non-virtual idx)]
          (when-not (column/compatible? validating? col1 col2)
            (logging/log-warning (format "Columns at index %d not compatible" idx))))))))

(defn compatibility-merge [user-schema embedded-schema]
  (update user-schema :columns merge-columns (:columns embedded-schema)))