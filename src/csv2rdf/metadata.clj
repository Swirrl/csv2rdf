(ns csv2rdf.metadata
  (:require [csv2rdf.validation :as v]
            [csv2rdf.metadata.datatype :as datatype]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [csv2rdf.uri-template :as template])
  (:import [java.net URI URISyntaxException]
           [java.nio.charset Charset IllegalCharsetNameException]
           [java.util Locale$Builder IllformedLocaleException]
           [java.nio CharBuffer]
           [com.github.fge.uritemplate.parse VariableSpecParser]
           [com.github.fge.uritemplate URITemplateParseException]
           [java.lang.reflect InvocationTargetException]))

(defn make-error [{:keys [path] :as context} msg]
  (v/of-error (str "Error at path " path ": " msg)))

(defn make-warning [{:keys [path] :as context} msg value]
  (v/with-warning (str "At path " path ": " msg) value))

(defn append-path [context path-element]
  (update context :path conj path-element))

(defn make-context [base-uri]
  {:base-uri base-uri :path []})

(defn eq [expected]
  (fn [context x]
    (if (= expected x)
      (v/pure x)
      (make-error context (str "Expected '" expected "' received '" x "'")))))

(def ^{:metadata-spec "5.2"} context nil)

(defn array? [x]
  (vector? x))

(def object? map?)

(defn- get-json-type [x]
  (cond (array? x) :array
        (number? x) :number
        (string? x) :string
        (boolean? x) :boolean
        (object? x) :object
        :else (throw (ex-info (str "Unknown JSON type for type " (type x))
                              {:type ::json-type-error
                               :value x}))))

(defn- get-json-type-name [x]
  (name (get-json-type x)))

(defn type-error-message [permitted-types actual-type]
  (let [c (count permitted-types)]
    (case c
      0 (str "Unexpected type " (name actual-type))
      1 (str "Expected " (name (first permitted-types)) " but got " (name actual-type))
      (let [[except-last [last]] (split-at (dec c) (map name permitted-types))
            list (str (string/join ", " except-last) " or " last)]
        (str "Expected " list " but got " (name actual-type))))))

(defn- type-error [context permitted-types actual-type]
  (make-error context (type-error-message permitted-types actual-type)))

(defn- expect-type [expected-type]
  (fn [context x]
    (let [actual-type (get-json-type x)]
      (if (= expected-type actual-type)
        (v/pure x)
        (type-error context [expected-type] actual-type)))))

(def array (expect-type :array))

(defn validate-array [context arr {:keys [length min-length] :as opts}]
  (cond
    (and (some? length) (not= length (count arr)))
    (make-error context (format "Expected array to contain %d elements" length))

    (and (some? min-length) (< (count arr) min-length))
    (make-error context (format "Expected array to contain at least %d elements" min-length))

    :else (v/pure arr)))

(defn array-with [opts]
  (fn [context x]
    (->> (array context x)
         (v/bind (fn [arr]
                   (validate-array context arr opts))))))

(defn any "Matches any value successfully"
  [_context x]
  (v/pure x))

(def number (expect-type :number))
(def bool (expect-type :boolean))
(def object (expect-type :object))
(def string (expect-type :string))

(defn variant [tag-validators]
  {:pre [(pos? (count tag-validators))]}
  (fn [context x]
    (if-let [validator (get tag-validators (get-json-type x))]
      (validator context x)
      (type-error context (keys tag-validators) (get-json-type x)))))

(defn character [context x]
  (->> (string context x)
       (v/bind (fn [s]
                 (if (= 1 (.length s))
                   (v/pure (.charAt s 0))
                   (make-error context "Expected single character"))))))

(defn compm [& validators]
  (reduce (fn [acc validator]
            (fn [context value]
              (v/bind (fn [v] (validator context v)) (acc context value)))) (fn [_c v] (v/pure v)) validators))

(defn array-of
  ([element-validator] (array-of element-validator {}))
  ([element-validator opts]
   (fn [context x]
     (v/bind (fn [arr]
               (v/collect (map-indexed (fn [idx v]
                                         (element-validator (append-path context idx) v)) arr)))
             ((array-with opts) context x)))))

(defn tuple [& fs]
  (compm (array-with {:length (count fs)})
         (fn [arr]
           (v/collect (map (fn [f x] (f x)) fs arr)))))

(defn try-parse-with [f]
  (fn [context x]
    (try
      (v/pure (f x))
      (catch Exception ex
        (make-error context (.getMessage ex))))))

(def uri (compm string (try-parse-with #(URI. %))))

(def default-uri (URI. ""))

(defn ^{:metadata-spec "5.1.2"} link-property
  "Converts link properties to URIs, or logs a warning if the URI is invalid. Link properties are resolved
   at a higher level."
  [context x]
  (if (string? x)
    (try
      (v/pure (URI. x))
      (catch URISyntaxException _ex
        (make-warning context (format "Link property '%s' cannot be parsed as a URI" x) default-uri)))
    (make-warning context (format "Invalid link property '%s': expected string containing URI, got %s" x (get-json-type-name x)) default-uri)))

(defn ^{:metadata-spec "5.1.3"} template-property [context x]
  (->> (string context x)
       (v/bind (fn [s]
                 (if-let [t (template/try-parse-template s)]
                   (v/pure t)
                   (make-error context (str "Invalid URI template: '" s "'")))))))

(defn each [& validators]
  (fn [context x]
    (let [vs (map (fn [validator] (validator context x)) validators)]
      (v/collect vs))))

(defn required-key [k value-validator]
  (fn [context m]
    (if (contains? m k)
      (->> (value-validator (append-path context k) (get m k))
           (v/fmap (fn [v] [k v])))
      (make-error context (str "Missing required key '" k "'")))))

(defn optional-key [k value-validator]
  (fn [context m]
    (if (contains? m k)
      (->> (value-validator (append-path context k) (get m k))
           (v/fmap (fn [v] [k v])))
      (v/pure [k nil]))))

(defn common-property-key? [k]
  ;;TODO: improve?
  (.contains k ":"))

(defn common-property-pair [context [k v]]
  (if (common-property-key? k)
    (v/pure [k v])
    (make-error context (str "Invalid common property key '" k "'"))))

(defn invalid-key-pair [context [k _]]
  (make-error context (str "Invalid key '" k "'")))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn partition-keys [source-map ref-map]
  (let [ref-keys (keys ref-map)]
    [(select-keys source-map ref-keys) (apply dissoc source-map ref-keys)]))

(defn object-of [{:keys [required optional allow-common-properties?]}]
  (let [required-keys (map (fn [[k v]] (required-key k v)) required)
        optional-keys (map (fn [[k v]] (optional-key k v)) optional)]
    (fn [context x]
      (->> (object context x)
           (v/bind (fn [obj]
                     (let [[required-obj opt-obj] (partition-keys obj required)
                           [optional-obj remaining-obj] (partition-keys opt-obj optional)
                           required-validations (map (fn [validator] (validator context required-obj)) required-keys)
                           optional-validations (map (fn [validator] (validator context optional-obj)) optional-keys)
                           declared-pairs-validation (v/collect (concat required-validations optional-validations))
                           declared-validation (v/fmap (fn [pairs]
                                                         (map-keys keyword (into {} pairs)))
                                                       declared-pairs-validation)]
                       (if allow-common-properties?
                         (let [common-pairs-validation (v/collect (map (fn [p] (common-property-pair context p)) remaining-obj))
                               common-validation (v/fmap #(into {} %) common-pairs-validation)]
                           (v/fmap (fn [[declared common]]
                                     (assoc declared ::common-properties common))
                                   (v/collect [declared-validation common-validation])))
                         (let [invalid-validation (v/collect (map (fn [p] (invalid-key-pair context p)) remaining-obj))]
                           (v/combine invalid-validation declared-validation))))))))))

(defn validate-language-code [context s]
  (try
    (do
      (.setLanguage (Locale$Builder.) s)
      ;;TODO: validate against known list of language codes?
      (v/pure s))
    (catch IllformedLocaleException _ex
      (make-warning context (str "Invalid language code: '" s "'") nil))))

(defn language-code [context x]
  (if (string? x)
    (validate-language-code context x)
    (make-warning context (str "Expected language code, got " (get-json-type-name x)) nil)))

(defn language-code-array [context arr]
  (v/fmap (fn [codes]
            (vec (remove nil? codes)))
          ((array-of language-code) context arr)))

(defn language-code-map-value [context v]
  (cond (string? v) (language-code context v)
        (array? v) (language-code-array context v)
        :else (make-warning context (str "Expected language code or language code array, got " (get-json-type-name v)) nil)))

(defn ^{:metadata-spec "5.1.6"} natural-language [context x]
  (cond
    (string? x) (validate-language-code context x)
    (array? x) (language-code-array context x)
    (object? x) (let [pair-validator (tuple language-code language-code-map-value)]
                  (v/fmap (fn [pairs]
                            ;;ignore any pairs with invalid key or value
                            (into {} (remove (fn [[k v]] (or (nil? k) (nil? v))) pairs)))
                          (v/collect (map pair-validator x))))
    :else (make-warning context "Expected string, array or object for natual language property" [])))

(defn encoding [context x]
  ;;NOTE: some valid encodings defined in https://www.w3.org/TR/encoding/
  ;;may not be supported by the underlying platform, reject these as invalid
  (->> (string context x)
       (v/bind (fn [s]
                 (try
                   (if (Charset/isSupported s)
                     (v/pure s)
                     (make-error context (str "Invalid encoding: '" s "'")))
                   (catch IllegalCharsetNameException _ex
                     (make-error context (str "Invalid encoding: '" s "'"))))))))

;;TODO: merge with tabular.csv.dialect/parse-trim
(def trim-modes {"true" :all "false" :none "start" :start "end" :end})

(defn mapping [m]
  (fn [context k]
    (if (contains? m k)
      (v/pure (get m k))
      (make-error context (str "Expected one of " (string/join ", " (keys m)))))))

(def trim-mode (variant {:string (mapping trim-modes)
                         :boolean (fn [_context b] (if b :all :none))}))

(defn where [pred desc]
  (fn [context x]
    (if (pred x)
      (v/pure x)
      (make-error context (str "Expected '" x "' to be " desc)))))

(def non-negative (compm number (where pos? "positive")))

(def csvw-ns "http://www.w3.org/ns/csvw")

(def context-pair
  (each (tuple
          (eq csvw-ns)
          (object-of {:optional {"@base"     uri
                                 "@language" (fn [context x]
                                               (v/warnings-as-errors (language-code context x)))}}))
        (fn [[_ns m]]
          (if (or (contains? m "@base") (contains? m "@language"))
            (v/pure m)
            (v/of-error "Top-level object must contain @base or @language keys")))))

(defn top-level-object [context x]
  (cond (string? x) ((eq csvw-ns) x)
        (array? x) (context-pair x)
        :else (make-error context (str "Expected namespace string or pair of namespace and local context definition, got " (get-json-type-name x)))))

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
        (make-error context (str "Invalid template variable: '" s "'"))))
    (catch URITemplateParseException _ex
      (make-error context (str "Invalid template variable: '" s "'")))))

(defn ^{:template-spec "5.6"} column-name [context x]
  (v/bind (fn [s]
            (if (.startsWith s "_")
              (make-error context "Columns names cannot begin with _")
              (uri-template-variable context s)))
          (string context x)))

(defn ^{:doc "An id is a link property whose value cannot begin with _:"} id [context x]
  (->> (string context x)
       (v/bind (fn [s]
                 (if (.startsWith s "_:")
                   (make-error context "Ids cannot start with _:")
                   ((try-parse-with #(URI. %)) context s))))))

(defn get-duplicate-names [columns]
  (->> columns
       (map #(get % "name"))
       (frequencies)
       (filter (fn [[n count]] (> count 1)))
       (map first)))

(defn ^{:metadata-spec "5.5"} validate-column-names [context columns]
  ;;columns property: The name properties of the column descriptions MUST be unique within a given table description.
  (let [duplicate-names (get-duplicate-names columns)]
    (if (seq duplicate-names)
      (make-error context (str "Duplicate names for columns: " (string/join ", " duplicate-names)))
      (v/pure nil))))

(defn get-column-name [column]
  (get column "name"))

(defn ^{:metadata-spec "5.6"} validate-virtual-columns [context columns]
  ;;virtual property: If present, a virtual column MUST appear after all other non-virtual column definitions.
  (let [non-virtual? (fn [col] (= false (get col "virtual")))
        virtual-columns (drop-while non-virtual? columns)
        invalid-columns (filter non-virtual? virtual-columns)]
    (if (seq invalid-columns)
      (let [first-virtual (first virtual-columns)           ;;NOTE: must exist
            msg (format "Non-virtual columns %s defined after first virtual column %s - All virtual columns must exist after all non-virtual columns"
                        (string/join ", " (map get-column-name invalid-columns))
                        (get-column-name first-virtual))]
        (make-error context msg))
      (v/pure nil))))

(defn validate-columns [context columns]
  (let [names-val (validate-column-names context columns)
        virtual-val (validate-virtual-columns context columns)]
    (v/combine names-val virtual-val)))

(defn columns [context x]
  (v/bind (fn [cols]
            (v/fmap (constantly cols) (validate-columns context cols)))
          ((array-of column) context x)))

(def line-terminators
  (variant {:string any
            :array (array-of string)}))

(defn one-of [values]
  (fn [context x]
    (if (contains? values x)
      (v/pure x)
      (make-error context (str "Expected one of " (string/join ", " values))))))

(def table-direction (one-of #{"rtl" "ltr" "auto"}))

(defn object-property
  "Object which may be specified in line in the metadata document or referenced through a URI"
  [validator]
  (fn [context x]
    (cond (string? x) (make-error context "TODO: resolve URI and validate") ;;TODO: implement
          (map? x) (validator context x)
          :else (make-error context (str "Expected URI or object, got " (get-json-type-name x))))))

(defn ^{:metadata-spec "5.1.4"} column-reference [context x]
  ;;TODO: validation that each referenced column exists occurs at higher-level
  (cond (string? x) (v/pure x)                              ;;TODO: always return vector of column references?
        (array? x) (cond (= 0 (count x)) (make-warning context "Column references should not be empty" nil)
                         (not-every? string? x) (make-warning context "Column references should all be strings" nil)
                         :else (v/pure x))
        :else (make-warning context (type-error-message [:string :array] (get-json-type x)) nil)))

;;TODO: The properties on these objects are interpreted equivalently to common properties as described in section 5.8 Common Properties.
(def note any)

(def transformation-source-types #{"json" "rdf"})

(defn validate-transformation-source [context s]
  (if (contains? transformation-source-types s)
    (v/pure (keyword s))
    (make-warning context (str "Expected one of " (string/join ", " transformation-source-types)) nil)))

(def transformation-source
  (compm string validate-transformation-source))

(def datatype-name
  (compm string (one-of datatype/type-names)))

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
  (compm
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
                  "@id"          (compm id validate-datatype-id)
                  "@type"        (eq "Datatype")
                  }})
    validate-derived-datatype))

(defn datatype [context x]
  (cond (string? x) (datatype-name x)
        (object? x) (derived-datatype x)
        :else (make-error context (str "Expected data type name or derived datatype object, got " (get-json-type-name x)))))

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

(def column
  (metadata-of
    {:optional {"name"           column-name                             ;;TODO: use first title as name if not provided, see spec
                "suppressOutput" bool
                "titles"         natural-language                   ;;TODO: should be array?
                "virtual"        bool
                "@id"            id
                "@type" (eq "Column")}}))

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
