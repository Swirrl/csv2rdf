(ns csv2rdf.metadata
  (:require [csv2rdf.validation :as v]
            [csv2rdf.xml.datatype :as xml-datatype]
            [clojure.string :as string]
            [csv2rdf.metadata.validator :refer :all]
            [csv2rdf.metadata.context :refer :all]
            [csv2rdf.metadata.json :refer :all]
            [csv2rdf.metadata.types :refer :all]
            [csv2rdf.metadata.dialect :refer [dialect]]
            [csv2rdf.util :as util])
  (:import [java.nio CharBuffer]
           [com.github.fge.uritemplate.parse VariableSpecParser]
           [com.github.fge.uritemplate URITemplateParseException]
           [java.lang.reflect InvocationTargetException]))

(def parse-variable-method (util/get-declared-method VariableSpecParser "parseFullName" [CharBuffer]))

(defn parse-uri-template-variable [s]
  (try
    (.invoke parse-variable-method nil (into-array [(CharBuffer/wrap s)]))
    (catch InvocationTargetException ex
      (throw (.getCause ex)))))

(defn uri-template-variable [context s]
  (try
    (let [result (parse-uri-template-variable s)]
      ;;parseFullName returns the prefix of the input that could be parsed
      (if (= result s)
        (v/pure s)
        (make-warning context (str "Invalid template variable: '" s "'") invalid)))
    (catch URITemplateParseException _ex
      (make-warning context (str "Invalid template variable: '" s "'") invalid))))

(def csvw-ns "http://www.w3.org/ns/csvw")

(defn validate-object-context-pair [context [_ns m]]
  (if (or (contains? m base-key) (contains? m language-key))
    (v/pure m)
    (make-error context "Top-level object must contain @base or @language keys")))

(def parse-context-pair (tuple
                          (eq csvw-ns)
                          (object-of {:optional {"@base"     (strict uri)
                                                 "@language" (strict language-code)}})))

(def context-pair (chain parse-context-pair validate-object-context-pair))

(def object-context (variant {:string (eq csvw-ns) :array context-pair}))

(defn validate-contextual-object
  "Returns a validator for an object which may contain a @context key mapped to a context definition.
   If the key exists, and contain a local context definition for the object, this is used to update the
   validation context used to parse the object and its children."
  [context-required? object-validator]
  (let [context-validator (if context-required?
                            (required-key "@context" object-context)
                            (optional-key "@context" object-context))]
    (fn [context obj]
      (v/bind (fn [context-pair]
                (cond
                  ;;if no context then validate entire object
                  (nil? context-pair) (object-validator context obj)

                  ;;context is literal containing csvw-ns so remove it and validate rest of object
                  (string? (second context-pair)) (object-validator context (dissoc obj "@context"))

                  ;;context is pair containing csvw-ns and local context definition
                  :else (let [local-context (second context-pair)
                              updated-context (update-from-local-context context local-context)]
                          (object-validator updated-context (dissoc obj "@context")))))
              (context-validator context obj)))))

(defn contextual-object [context-required? object-validator]
  (variant {:object (validate-contextual-object context-required? object-validator)}))

(defn resolve-linked-object-property-object [context object-uri]
  (try
    (v/pure (util/read-json object-uri))
    (catch Exception ex
      (make-error context (format "Failed to resolve linked object property at URI %s: %s" object-uri (.getMessage ex))))))

(defn ^{:metadata-spec "6.4"} linked-object-property [object-validator]
  (fn [context object-uri]
    (->> (resolve-linked-object-property-object context object-uri)
         (v/bind (fn [obj]
                   ((contextual-object false object-validator) context obj)))
         (v/fmap (fn [obj]
                   ;;TODO: error if resolved JSON document is not an object? Specification does not mention this
                   ;;case but requires an empty object in other error cases. Add @id key as required due to normalisation
                   (cond (invalid? obj) {id-key object-uri}
                         (contains? obj id-key) obj
                         :else (assoc obj id-key object-uri)))))))

(defn ^{:metadata-spec "5.1.5"} object-property
  "Object which may be specified in line in the metadata document or referenced through a URI"
  [object-validator]
  (variant {:string (chain link-property (linked-object-property object-validator))
            :object object-validator
            :default {}}))

(def transformation-source-types #{"json" "rdf"})

(def ^{:metadata-spec "5.10.2"} transformation-source
  (variant {:string (one-of transformation-source-types)}))

(def ^{:metadata-spec "5.11.2"} datatype-name
  (variant {:string (one-of xml-datatype/type-names)}))

;;TODO: implement!
(def datatype-format any)

(def datatype-bound (variant {:number any :string any}))

(defn ^{:metadata-spec "5.11.2"} validate-derived-datatype
  [context {:strs [base length minLength maxLength minimum minInclusive minExclusive maximum maxInclusive maxExclusive] :as dt}]
  (cond
    ;;applications MUST raise an error if both length and minLength are specified and length is less than minLength
    (and (some? length) (some? minLength) (< length minLength))
    (make-error context "Length must be >= minLength")

    ;;applications MUST raise an error if both length and maxLength are specified and length is greater than maxLength
    (and (some? length) (some? maxLength) (> length maxLength))
    (make-error context "Length must be <= maxLength")

    ;;applications MUST raise an error if minLength and maxLength are both specified and minLength is greater than maxLength
    (and (some? minLength) (some? maxLength) (> minLength maxLength))
    (make-error context "minLength must be <= maxLength")

    ;;applications MUST raise an error if length, maxLength, or minLength are specified and the base datatype is
    ;;neither string, a subtype of string, nor a binary type
    (and (or (some? length) (some? minLength) (some? maxLength))
         (not (or (xml-datatype/is-subtype? "string" base) (xml-datatype/is-binary-type? base))))
    (make-error context "length, minLength and maxLength properties only valid on string or binary data types")

    ;;Applications MUST raise an error if both minimum and minInclusive are specified and they do not have the same value
    (and (some? minimum) (some? minInclusive) (not= minimum minInclusive))
    (make-error context "minimum and minInclusive must be equal when both specified")

    ;;applications MUST raise an error if both maximum and maxInclusive are specified and they do not have the same value
    (and (some? maximum) (some? maxInclusive) (not= maximum maxInclusive))
    (make-error context "maximum and maxInclusive must be equal when both specified")

    ;;applications MUST raise an error if both minInclusive and minExclusive are specified
    (and (some? minInclusive) (some? minExclusive))
    (make-error context "Cannot specify both minInclusive and minExclusive")

    ;;...or if both maxInclusive and maxExclusive are specified
    (and (some? maxInclusive) (some? maxExclusive))
    (make-error context "Cannot specify both maxInclusive and maxExclusive")

    ;;Applications MUST raise an error if both minInclusive and maxInclusive are specified and maxInclusive is less than minInclusive
    (and (some? minInclusive) (some? maxInclusive) (< maxInclusive minInclusive))
    (make-error context "minInclusive must be <= maxInclusive")

    ;;...or if both minInclusive and maxExclusive are specified and maxExclusive is less than or equal to minInclusive
    (and (some? minInclusive) (some? maxExclusive) (<= maxExclusive minInclusive))
    (make-error context "minInclusive must be < maxExclusive")

    ;;applications MUST raise an error if both minExclusive and maxExclusive are specified and maxExclusive is less than minExclusive
    (and (some? minExclusive) (some? maxExclusive) (< maxExclusive minExclusive))
    (make-error context "minExclusive must be <= maxExclusive")

    ;;...or if both minExclusive and maxInclusive are specified and maxInclusive is less than or equal to minExclusive
    (and (some? minExclusive) (some? maxInclusive) (<= maxInclusive minExclusive))
    (make-error context "maxInclusive must be > minExclusive")

    ;;Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive
    ;;are specified and the base datatype is not a numeric, date/time, or duration type
    (and (or (some? minimum) (some? minInclusive) (some? maximum) (some? maxInclusive) (some? minExclusive) (some? maxExclusive))
         (not (or (xml-datatype/is-numeric-type? base) (xml-datatype/is-date-time-type? base) (xml-datatype/is-duration-type? base))))
    (make-error context "minimum, minInclusive, maximum, maxInclusvie, minExclusive, maxExclusive only valid for numeric, date/time or duration types")

    :else (v/pure dt)))

;;TODO: implement!
;;datatype id MUST NOT be the URL of a built-in datatype.
(def validate-datatype-id any)

(def derived-datatype
  (chain
    (object-of
      {:optional {"base"         datatype-name
                  "format"       datatype-format
                  "length"       non-negative
                  "minLength"    non-negative
                  "maxLength"    non-negative
                  "minimum"      datatype-bound
                  "maximum"      datatype-bound
                  "minInclusive" datatype-bound
                  "maxInclusive" datatype-bound
                  "minExclusive" datatype-bound
                  "maxExclusive" datatype-bound
                  "@id"          (chain id validate-datatype-id)
                  "@type"        (eq "Datatype")
                  }
       :allow-common-properties? true})
    validate-derived-datatype))

(defn ^{:metadata-spec "5.7"} normalise-datatype-name [_context type-name]
  (v/pure {"base" type-name}))

(def ^{:metadata-spec "5.7"} datatype
  (variant {:string (chain datatype-name normalise-datatype-name)
            :object derived-datatype}))

(def null-value (variant {:string any :array (array-of string)}))

(def inherited-properties
  {"aboutUrl" template-property
   "datatype" datatype
   "default" string
   "lang" language-code
   "null" null-value
   "ordered" boolean
   "propertyUrl" template-property
   "required" boolean
   "separator" string
   "textDirection" (one-of #{"ltr" "rtl" "auto" "inherit"})
   "valueUrl" template-property})

(def inherited-defaults
  {"default" ""
   "lang" "und"
   "null" [""]
   "ordered" false
   "required" false
   "separator" nil
   "textDirection" "inherit"})

(defn common-property-key? [k]
  ;;TODO: improve?
  (.contains k ":"))

(defn validate-metadata [context declared-keys m]
  (let [validate-kvp (fn [[k v]]
                       (cond (contains? declared-keys k)
                             (let [value-validator (get declared-keys k)
                                   value-validation (value-validator (append-path context k) v)]
                               (v/fmap (fn [value] [[(keyword k)] value]) value-validation))

                             (common-property-key? k)
                             (v/pure [[::common-properties k] v])

                             :else (make-error context (str "Invalid metadata key '" k "'"))))
        pairs-validation (v/collect (map validate-kvp m))]
    (v/fmap (fn [pairs]
              (reduce (fn [acc [k v]]
                        (assoc-in acc k v))
                      {}
                      pairs))
            pairs-validation)))

(defn metadata-of [{:keys [required optional defaults]}]
  (object-of {:required required
              :optional (merge inherited-properties optional)
              :defaults defaults
              :allow-common-properties? true}))

(defn ^{:metadata-spec "5.6"} validate-column-name [context s]
  (if (.startsWith s "_")
    (make-warning context "Columns names cannot begin with _" invalid)
    (uri-template-variable context s)))

(def ^{:metadata-spec "5.6"} column-name
  (chain string validate-column-name))

(def column
  (metadata-of
    {:optional {"name"           column-name                             ;;TODO: use first title as name if not provided, see spec
                "suppressOutput" bool
                "titles"         natural-language                   ;;TODO: should be array?
                "virtual"        bool
                "@id"            id
                "@type" (eq "Column")}
     :defaults {"suppressOutput" false
                "virtual" false}}))

(defn get-duplicate-names [columns]
  (->> columns
       (map :name)
       (frequencies)
       (filter (fn [[n count]] (> count 1)))
       (map first)))

(defn ^{:metadata-spec "5.5"} validate-column-names [context columns]
  ;;columns property: The name properties of the column descriptions MUST be unique within a given table description.
  (let [duplicate-names (get-duplicate-names columns)]
    (if (seq duplicate-names)
      (make-error context (str "Duplicate names for columns: " (string/join ", " duplicate-names)))
      (v/pure columns))))

(defn ^{:metadata-spec "5.6"} validate-virtual-columns [context columns]
  ;;virtual property: If present, a virtual column MUST appear after all other non-virtual column definitions.
  (let [non-virtual? (fn [col] (= false (:virtual col)))
        virtual-columns (drop-while non-virtual? columns)
        invalid-columns (filter non-virtual? virtual-columns)]
    (if (seq invalid-columns)
      (let [first-virtual (first virtual-columns)           ;;NOTE: must exist
            msg (format "Non-virtual columns %s defined after first virtual column %s - All virtual columns must exist after all non-virtual columns"
                        (string/join ", " (map :name invalid-columns))
                        (:name first-virtual))]
        (make-error context msg))
      (v/pure columns))))

(def ^{:table-spec "4.3"} index->column-number inc)

(defn ^{:metadata-spec "5.6"} get-column-name [{:keys [name titles] :as column} column-index default-language]
  (or name
      (first (get titles default-language))
      (str "_col." (index->column-number column-index))))

(defn set-column-name [column column-index context]
  (assoc column :name (get-column-name column column-index (language-code-or-default context))))

(defn set-column-names [context columns]
  (->> columns
       (map-indexed (fn [idx col] (set-column-name col idx context)))
       (vec)
       (v/pure)))

(def columns (chain (array-of column) set-column-names validate-column-names validate-virtual-columns))

;;NOTE: transformations may contain common properties but not inherited properties?
(def transformation
  (object-of
    {:required {"url" link-property
                "scriptFormat" link-property
                "targetFormat" link-property}
     :optional {"source" transformation-source
                "titles" natural-language
                "@id" id
                "@type" (eq "Template")}
     :allow-common-properties? false}))

(def table-defaults
  {"suppressOutput" false
   "tableDirection" "auto"})

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
      {:optional {"columns"     columns
                  "foreignKeys" (array-of foreign-key)      ;;TODO: validate foreign keys
                  "primaryKey"  column-reference            ;;TODO: validators MUST check that each row has a unique combination of values of cells in the indicated columns
                  "rowTitles"   column-reference
                  "@id"         id
                  "@type"       (eq "Schema")}})
    validate-schema-column-references))

(def table
  (metadata-of
    {:required {"url" link-property}
     :optional {"notes" (array-of note)
                "suppressOutput" bool
                "tableDirection" table-direction
                "tableSchema" (object-property schema)
                "transformations" (array-of transformation)
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
                "tableSchema"     (object-property schema)
                "transformations" (array-of transformation)
                "@id"             id
                "@type"           (eq "TableGroup")}}))

(defn parse-file [f]
  (let [json (util/read-json f)
        context (make-context (.toURI f))]
    ((contextual-object true table) context json)))