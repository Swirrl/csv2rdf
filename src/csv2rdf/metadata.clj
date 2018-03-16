(ns csv2rdf.metadata
  (:require [csv2rdf.validation :as v]
            [csv2rdf.metadata.datatype :as datatype]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [csv2rdf.uri-template :as template]
            [csv2rdf.metadata.validator :refer :all]
            [csv2rdf.metadata.context :refer :all]
            [csv2rdf.metadata.json :refer :all]
            [csv2rdf.util :as util])
  (:import [java.net URI URISyntaxException]
           [java.nio.charset Charset IllegalCharsetNameException]
           [java.util Locale$Builder IllformedLocaleException]
           [java.nio CharBuffer]
           [com.github.fge.uritemplate.parse VariableSpecParser]
           [com.github.fge.uritemplate URITemplateParseException]
           [java.lang.reflect InvocationTargetException]))

(def ^{:metadata-spec "5.2"} context nil)

(def default-uri (URI. ""))

(defn ^{:metadata-spec "6.3"} normalise-link-property
  "Normalises a link property URI by resolving it against the current base URI."
  [context uri]
  (v/pure (resolve-uri context uri)))

(defn ^{:metadata-spec "5.1.2"} parse-link-property
  "Converts link properties to URIs, or logs a warning if the URI is invalid. Link properties are resolved
   at a higher level."
  [context x]
  (if (string? x)
    (try
      (v/pure (URI. x))
      (catch URISyntaxException _ex
        (make-warning context (format "Link property '%s' cannot be parsed as a URI" x) default-uri)))
    (make-warning context (format "Invalid link property '%s': expected string containing URI, got %s" x (get-json-type-name x)) default-uri)))

(def link-property (chain parse-link-property normalise-link-property))

(defn ^{:metadata-spec "5.1.3"} template-property [context x]
  (->> (string context x)
       (v/bind (fn [s]
                 ))))

(def template-property
  (variant {:string (fn [context s]
                      (if-let [t (template/try-parse-template s)]
                        (v/pure t)
                        (make-warning context (str "Invalid URI template: '" s "'") invalid)))}))

(defn common-property-key? [k]
  ;;TODO: improve?
  (.contains k ":"))

(defn common-property-pair [context [k v]]
  (if (common-property-key? k)
    (v/pure [k v])
    (make-warning context (str "Invalid common property key '" k "'") nil)))

(defn ^{:metadata-spec "4"} invalid-key-pair
  "Generates a warning for any invalid keys found in a metadata document."
  [context [k _]]
  (make-warning context (str "Invalid key '" k "'") nil))

(defn validate-object-of [{:keys [required optional allow-common-properties?]}]
  (let [required-keys (map (fn [[k v]] (required-key k v)) required)
        optional-keys (map (fn [[k v]] (optional-key k v)) optional)]
    (fn [context obj]
      (let [[required-obj opt-obj] (util/partition-keys obj required)
            [optional-obj remaining-obj] (util/partition-keys opt-obj optional)
            required-validations (map (fn [validator] (validator context required-obj)) required-keys)
            optional-validations (map (fn [validator] (validator context optional-obj)) optional-keys)
            declared-pairs-validation (v/collect (concat required-validations optional-validations))
            declared-validation (v/fmap (fn [pairs]
                                          ;;optional keys not declared in the input are nil so remove them
                                          ;;convert keys to keywords
                                          (->> pairs
                                               (remove nil?)
                                               (map (fn [[k v]] [(keyword k) v]))
                                               (into {})))
                                        declared-pairs-validation)]
        (if allow-common-properties?
          (let [common-pairs-validation (v/collect (map (fn [p] (common-property-pair context p)) remaining-obj))
                common-validation (v/fmap (fn [pairs]
                                            (into {} (remove nil? pairs))) common-pairs-validation)]
            (v/fmap (fn [[declared common]]
                      (if (empty? common)
                        declared
                        (assoc declared ::common-properties common)))
                    (v/collect [declared-validation common-validation])))
          (let [invalid-validation (v/collect (map (fn [p] (invalid-key-pair context p)) remaining-obj))]
            (v/combine invalid-validation declared-validation)))))))

(defn object-of [opts]
  (variant {:object (validate-object-of opts)}))

(defn validate-language-code [context s]
  (try
    (do
      (.setLanguage (Locale$Builder.) s)
      ;;TODO: validate against known list of language codes?
      (v/pure s))
    (catch IllformedLocaleException _ex
      (make-warning context (str "Invalid language code: '" s "'") invalid))))

(defn language-code [context x]
  (if (string? x)
    (validate-language-code context x)
    (make-warning context (str "Expected language code, got " (get-json-type-name x)) invalid)))

(defn ^{:metadata-spec "6.6"} normalise-string-natural-language-property [context code]
  {(language-code-or-default context) [code]})

(defn ^{:metadata-spec "6.6"} normalise-array-natural-language-property [context codes]
  {(language-code-or-default context) codes})

(def ^{:metadata-spec "5.1.6"
        :doc "Validates a value in a natual language property defined as an object. Values can
        be either strings or string arrays, and returns an array if valid."} language-code-map-value
  (variant {:string (fn [_context s] (v/pure [s]))
            :array (array-of string)
            :default []}))

(defn ^{:metadata-spec "5.1.6"} language-object-pair
  "Validates a language code -> string or string array pair within a natural language property defined as an object.
   Returns a validation containing a pair language code -> string array if valid or nil if invalid. A pair is invalid
   if either the key or value is invalid."
  [context [code-key value]]
  (v/bind (fn [code]
            (if (invalid? code)
              (v/pure code)
              (v/fmap (fn [value] [code value])
                      (language-code-map-value (append-path context code) value))))
          (language-code context code-key)))

(defn natural-language-object [context obj]
  (v/fmap (fn [pairs]
            (into {} (remove invalid? pairs)))
          (v/collect (map (fn [pair] (language-object-pair context pair)) obj))))

(defn ^{:metadata-spec "5.1.6"} natural-language
  "Parses a natural language property according to the specification. Normalises the result into a map of language
   codes to an array of associated values. Uses the default language if none specified in the context."
  [context x]
  (cond
    (string? x) (v/pure (normalise-string-natural-language-property context x))
    (array? x) (v/fmap #(normalise-array-natural-language-property context %)
                       ((array-of string) context x))
    (object? x) (natural-language-object context x)
    :else (make-warning context "Expected string, array or object for natual language property" {(language-code-or-default context) []})))

(defn validate-encoding [context s]
  ;;NOTE: some valid encodings defined in https://www.w3.org/TR/encoding/
  ;;may not be supported by the underlying platform, reject these as invalid
  (try
    (if (Charset/isSupported s)
      (v/pure s)
      (make-warning context (str "Invalid encoding: '" s "'") invalid))
    (catch IllegalCharsetNameException _ex
      (make-warning context (str "Invalid encoding: '" s "'") invalid))))

(def encoding (variant {:string validate-encoding}))

;;TODO: merge with tabular.csv.dialect/parse-trim
(def trim-modes {"true" :all "false" :none "start" :start "end" :end})

(defn mapping [m]
  (fn [context k]
    (if (contains? m k)
      (v/pure (get m k))
      (make-warning context (str "Expected one of " (string/join ", " (keys m))) invalid))))

(def trim-mode (variant {:string  (mapping trim-modes)
                         :boolean (fn [_context b] (v/pure (if b :all :none)))}))

(defn where [pred desc]
  (fn [context x]
    (if (pred x)
      (v/pure x)
      (make-warning context (str "Expected '" x "' to be " desc) invalid))))

(def non-negative (variant {:number (where util/non-negative? "non-negative")} ))

(def parse-variable-method
  (let [m (.getDeclaredMethod VariableSpecParser "parseFullName" (into-array [CharBuffer]))]
    (.setAccessible m true)
    m))

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

(defn validate-id [context s]
  (if (.startsWith s "_:")
    (make-warning context "Ids cannot start with _:" invalid)
    ((try-parse-with #(URI. %)) context s)))

(def ^{:doc "An id is a link property whose value cannot begin with _:"} id
  (variant {:string validate-id}))

(def line-terminators
  (variant {:string any
            :array (array-of string)}))

(defn one-of [values]
  (fn [context x]
    (if (contains? values x)
      (v/pure x)
      (make-warning context (str "Expected one of " (string/join ", " values)) invalid))))

(def table-direction (one-of #{"rtl" "ltr" "auto"}))

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

(defn column-reference-array [context arr]
  (cond (= 0 (count arr)) (make-warning context "Column references should not be empty" invalid)
        (not-every? string? arr) (make-warning context "Column references should all be strings" invalid)
        :else (v/pure arr)))

;;TODO: validation that each referenced column exists occurs at higher-level
(def ^{:metadata-spec "5.1.4"}  column-reference
  (variant {:string (fn [_context s] (v/pure [s]))
            :array column-reference-array}))

;;TODO: The properties on these objects are interpreted equivalently to common properties as described in section 5.8 Common Properties.
(def note any)

(def transformation-source-types #{"json" "rdf"})

(def ^{:metadata-spec "5.10.2"} transformation-source
  (variant {:string (one-of transformation-source-types)}))

(def ^{:metadata-spec "5.11.2"} datatype-name
  (variant {:string (one-of datatype/type-names)}))

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
         (not (or (datatype/is-subtype? "string" base) (datatype/is-binary-type? base))))
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
         (not (or (datatype/is-numeric-type? base) (datatype/is-date-time-type? base) (datatype/is-duration-type? base))))
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
                  }})
    validate-derived-datatype))

(defn ^{:metadata-spec "5.7"} normalise-datatype-name [_context type-name]
  (v/pure {"base" type-name}))

(def ^{:metadata-spec "5.7"} datatype
  (variant {:string (chain datatype-name normalise-datatype-name)
            :object derived-datatype}))

(def null-value (variant {:string any :array (array-of string)}))

(def inherited-properties
  {"aboutUrl" template-property
   "datatype" datatype                         ;;TODO: The normalized value of this property becomes the datatype annotation for the described column.
   "default" string
   "lang" language-code
   "null" null-value
   "ordered" boolean
   "propertyUrl" template-property
   "required" boolean
   "separator" string
   "tableDirection" (one-of #{"ltr" "rtl" "auto" "inherit"}) ;;NOTE: different to table-direction
   "valueUrl" template-property
   })

(def inherited-defaults
  {"default" ""
   "lang" "und"
   "null" [""]
   "ordered" false
   "required" false
   "separator" nil
   "tableDirection" "inherit"})

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

(defn metadata-of [{:keys [required optional]}]
  (object-of {:required required
              :optional (merge inherited-properties optional)
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
                "@type" (eq "Column")}}))

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

(def foreign-key
  (object-of
    {:required {"columnReference" column-reference
                "reference"       object                    ;;TODO: specify reference
                }}))

(def schema
  (metadata-of
    {:optional {"columns" columns
                "foreignKeys" (array-of foreign-key)        ;;TODO: validate foreign keys
                "primaryKey" column-reference               ;;TODO: validators MUST check that each row has a unique combination of values of cells in the indicated columns
                "rowTitles" column-reference
                "@id" id
                "@type" (eq "Schema")}}))

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

(def dialect
  (object-of
    {:optional {"commentPrefix" character
                "delimiter" character
                "doubleQuote" bool
                "encoding" encoding
                "header" bool
                "headerRowCount" non-negative
                "lineTerminators" line-terminators
                "quoteChar" character
                "skipBlankRows" bool
                "skipColumns" non-negative
                "skipInitialSpace" bool
                "skipRows" non-negative
                "trimMode" trim-mode
                "@id" id
                "@type" (eq "Dialect")}
     :allow-common-properties? false}))

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