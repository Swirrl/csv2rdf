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

(defn eq [expected]
  (fn [x]
    (if (= expected x)
      (v/pure x)
      (v/of-error (str "Expected '" expected "' received '" x "'")))))

(def ^{:metadata-spec "5.2"} context nil)

(defn- get-json-type [x]
  (cond (array? x) "array"
        (number? x) "number"
        (string? x) "string"
        (boolean? x) "boolean"
        (map? x) "object"
        :else (throw (ex-info (str "Unknown JSON type for type " (type x))
                              {:type ::json-type-error
                               :value x}))))

(defn- expect-type [type-p type-name]
  (fn [x]
    (if (type-p x)
      (v/pure x)
      (v/of-error (str "Expected " type-name ", got " (get-json-type x))))))

(defn array? [x]
  (vector? x))
(def object? map?)

(def array (expect-type array? "array"))

(defn validate-array [arr {:keys [length min-length] :as opts}]
  (cond
    (and (some? length) (not= length (count arr)))
    (v/of-error (format "Expected array to contain %d elements " length))

    (and (some? min-length) (< (count arr) min-length))
    (v/of-error (format "Expected array to contain at least %d elements" min-length))

    :else (v/pure arr)))

(defn array-with [opts]
  (fn [x]
    (->> (array x)
         (v/bind (fn [arr]
                   (validate-array arr opts))))))

(def number (expect-type number? "number"))
(def bool (expect-type boolean? "boolean"))
(def object (expect-type map? "object"))
(def string (expect-type string? "string"))

(defn character [x]
  (if (string? x)
    (if (= 1 (.length x))
      (v/pure (.charAt x 0))
      (v/of-error "Expected single character"))
    (v/of-error "Expected string")))

(defn compm [& fs]
  (reduce (fn [acc f]
            (fn [v]
              (v/bind f (acc v)))) v/pure fs))

(defn array-of
  ([validator] (array-of validator {}))
  ([f opts]
   (compm (array-with opts)
          (fn [arr]
            (v/collect (map f arr))))))

(defn tuple [& fs]
  (compm (array-with {:length (count fs)})
         (fn [arr]
           (v/collect (map (fn [f x] (f x)) fs arr)))))

(defn try-parse-with [f]
  (fn [x]
    (try
      (v/pure (f x))
      (catch Exception ex
        (v/of-error (.getMessage ex))))))

(def uri (compm string (try-parse-with #(URI. %))))

(def default-uri (URI. ""))

(defn ^{:metadata-spec "5.1.2"} link-property
  "Converts link properties to URIs, or logs a warning if the URI is invalid. Link properties are resolved
   at a higher level."
  [x]
  (if (string? x)
    (try
      (v/pure (URI. x))
      (catch URISyntaxException _ex
        (v/with-warning (format "Link property '%s' cannot be parsed as a URI" x) default-uri)))
    (v/with-warning (format "Invalid link property '%s': expected string containing URI, got %s" x (get-json-type x)) default-uri)))

(defn ^{:metadata-spec "5.1.3"} template-property [x]
  (v/bind (fn [s]
            (if-let [t (template/try-parse-template s)]
              (v/pure t)
              (v/of-error (str "Invalid URI template: '" s "'"))))
          (string x)))

(defn each [& fs]
  (fn [x]
    (let [vs (map (fn [f] (f x)) fs)
          collected (v/collect vs)]
      collected)))

(defn prefix-errors-key-name [key validation]
  (v/map-errors (fn [err] (str "Key '" key "': " err)) validation))

(defn required-key [k vf]
  (fn [m]
    (if (contains? m k)
      (->> (vf (get m k))
           (prefix-errors-key-name k)
           (v/fmap (fn [v] [k v])))
      (v/of-error (str "Missing required key '" k "'")))))

(defn optional-key [k vf & {:keys [default]}]
  (fn [m]
    (if (contains? m k)
      (->> (vf (get m k))
           (prefix-errors-key-name k)
           (v/fmap (fn [v] [k v])))
      (v/pure [k default]))))

(defn object-of [{:keys [required optional defaults]}]
  (let [required-keys (map (fn [[k v]] (required-key k v)) required)
        optional-keys (map (fn [[k v]] (optional-key k v :default (get defaults k))) optional)
        keys-f (apply each (concat required-keys optional-keys))]
    (compm
      object
      keys-f
      (fn [pairs]
        (v/pure (into {} pairs))))))

(defn map-of [key-validator value-validator]
  (fn [obj]
    (let [pair-validator (tuple key-validator value-validator)
          validations (map pair-validator obj)]
      (v/fmap #(into {} %) (v/collect validations)))))

(defn validate-language-code [s]
  (try
    (do
      (.setLanguage (Locale$Builder.) s)
      ;;TODO: validate against known list of language codes?
      (v/pure s))
    (catch IllformedLocaleException _ex
      (v/with-warning (str "Invalid language code: '" s "'") nil))))

(defn language-code [x]
  (if (string? x)
    (validate-language-code x)
    (v/with-warning (str "Expected language code, got " (get-json-type x)) nil)))

(defn language-code-array [arr]
  (v/fmap (fn [codes]
            (vec (remove nil? codes)))
          ((array-of language-code) arr)))

(defn language-code-map-value [v]
  (cond (string? v) (language-code v)
        (array? v) (language-code-array v)
        :else (v/with-warning (str "Expected language code or language code array, got " (get-json-type v)) nil)))

(defn ^{:metadata-spec "5.1.6"} natural-language [x]
  (cond
    (string? x) (validate-language-code x)
    (array? x) (language-code-array x)
    (object? x) (let [pair-validator (tuple language-code language-code-map-value)]
                  (v/fmap (fn [pairs]
                            ;;ignore any pairs with invalid key or value
                            (into {} (remove (fn [[k v]] (or (nil? k) (nil? v))) pairs)))
                          (v/collect (map pair-validator x))))
    :else (v/with-warning "Expected string, array or object for natual language property" [])))

(defn encoding [x]
  ;;NOTE: some valid encodings defined in https://www.w3.org/TR/encoding/
  ;;may not be supported by the underlying platform, reject these as invalid
  (v/bind (fn [s]
            (try
              (if (Charset/isSupported s)
                (v/pure s)
                (v/of-error (str "Invalid encoding: '" s "'")))
              (catch IllegalCharsetNameException _ex
                (v/of-error (str "Invalid encoding: '" s "'")))))
          (string x)))

;;TODO: merge with tabular.csv.dialect/parse-trim
(def trim-modes {"true" :all "false" :none "start" :start "end" :end})

(defn mapping [m]
  (fn [k]
    (if (contains? m k)
      (v/pure (get m k))
      (v/of-error (str "Expected one of " (string/join ", " (keys m)))))))

(defn trim-mode [x]
  (cond (boolean? x) (v/pure (if x :all :none))
        (string? x) ((mapping trim-modes) x)
        :else (v/of-error (str "Expected boolean or string, got " (get-json-type x)))))

(defn where [pred desc]
  (fn [x]
    (if (pred x)
      (v/pure x)
      (v/of-error (str "Expected '" x "' to be " desc)))))

(def non-negative (compm number (where pos? "positive")))

(def csvw-ns "http://www.w3.org/ns/csvw")

(def context-pair
  (each (tuple
          (eq csvw-ns)
          (object-of {:optional {"@base"     uri
                                 "@language" (compm language-code v/warnings-as-errors)}}))
        (fn [[_ns m]]
          (if (or (contains? m "@base") (contains? m "@language"))
            (v/pure m)
            (v/of-error "Top-level object must contain @base or @language keys")))))

(defn top-level-object [x]
  (cond (string? x) ((eq csvw-ns) x)
        (array? x) (context-pair x)
        :else (v/of-error (str "Expected namespace string or pair of namespace and local context definition, got " (get-json-type x)))))

(def parse-variable-method
  (let [m (.getDeclaredMethod VariableSpecParser "parseFullName" (into-array [CharBuffer]))]
    (.setAccessible m true)
    m))

(defn parse-uri-template-variable [s]
  (try
    (.invoke parse-variable-method nil (into-array [(CharBuffer/wrap s)]))
    (catch InvocationTargetException ex
      (throw (.getCause ex)))))

(defn uri-template-variable [s]
  (try
    (let [result (parse-uri-template-variable s)]
      ;;parseFullName returns the prefix of the input that could be parsed
      (if (= result s)
        (v/pure s)
        (v/of-error (str "Invalid template variable: '" s "'"))))
    (catch URITemplateParseException _ex
      (v/of-error (str "Invalid template variable: '" s "'")))))

(defn ^{:template-spec "5.6"} column-name [x]
  (v/bind (fn [s]
            (if (.startsWith s "_")
              (v/of-error "Columns names cannot begin with _")
              (uri-template-variable s)))
          (string x)))

(def ^{:doc "An id is a link property whose value cannot begin with _:"} id
  (compm
    string
    (fn [s]
      (if (.startsWith s "_:")
        (v/of-error "Ids cannot start with _:")
        (v/pure s)))
    (try-parse-with #(URI. %))))

(def column
  (object-of
    {:optional {"name"           column-name                             ;;TODO: use first title as name if not provided, see spec
                "suppressOutput" bool
                "titles"         natural-language                   ;;TODO: should be array?
                "virtual"        bool
                "@id"            id
                }}))

(defn get-duplicate-names [columns]
  (->> columns
       (map #(get % "name"))
       (frequencies)
       (filter (fn [[n count]] (> count 1)))
       (map first)))

(defn ^{:metadata-spec "5.5"} validate-column-names [columns]
  ;;columns property: The name properties of the column descriptions MUST be unique within a given table description.
  (let [duplicate-names (get-duplicate-names columns)]
    (if (seq duplicate-names)
      (v/of-error (str "Duplicate names for columns: " (string/join ", " duplicate-names)))
      (v/pure nil))))

(defn get-column-name [column]
  (get column "name"))

(defn ^{:metadata-spec "5.6"} validate-virtual-columns [columns]
  ;;virtual property: If present, a virtual column MUST appear after all other non-virtual column definitions.
  (let [non-virtual? (fn [col] (= false (get col "virtual")))
        virtual-columns (drop-while non-virtual? columns)
        invalid-columns (filter non-virtual? virtual-columns)]
    (if (seq invalid-columns)
      (let [first-virtual (first virtual-columns)           ;;NOTE: must exist
            msg (format "Non-virtual columns %s defined after first virtual column %s - All virtual columns must exist after all non-virtual columns"
                        (string/join ", " (map get-column-name invalid-columns))
                        (get-column-name first-virtual))]
        (v/of-error msg))
      (v/pure nil))))

(defn validate-columns [columns]
  (let [names-val (validate-column-names columns)
        virtual-val (validate-virtual-columns columns)]
    (v/combine names-val virtual-val)))

(defn columns [x]
  (v/bind (fn [cols]
            (v/fmap (constantly cols) (validate-columns cols)))
          ((array-of column) x)))

(defn line-terminators [x]
  (cond (string? x) (v/pure [x])
        (array? x) ((array-of string) x)
        :else (v/of-error (str "Expected string or string array, got " (get-json-type x)))))

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
                "@type" (eq "Dialect")
                }
     :defaults {"commentPrefix" \#
                "delimiter" \,
                "doubleQuote" true
                "encoding" "utf-8"
                "header" true
                "headerRowCount" 1
                "lineTerminators" ["\r\n", "\n"]
                "quoteChar" \"
                "skipBlankRows" false
                "skipColumns" 0
                "skipInitialSpace" false
                "skipRows" 0
                "trimMode" :all
                }}))

(defn one-of [values]
  (fn [x]
    (if (contains? values x)
      (v/pure x)
      (v/of-error (str "Expected one of " (string/join ", " values))))))

(def table-direction (one-of #{"rtl" "ltr" "auto"}))

(defn linked-object
  "Object which may be specified in line in the metadata document or referenced through a URI"
  [validator]
  (fn [x]
    (cond (string? x) (v/of-error "TODO: resolve URI and validate") ;;TODO: implement
          (map? x) (validator x)
          :else (v/of-error (str "Expected URI or object, got " (get-json-type x))))))

(defn ^{:metadata-spec "5.1.4"} column-reference [x]
  ;;TODO: validation that each referenced column exists occurs at higher-level
  (cond (string? x) (v/pure x)                              ;;TODO: always return vector of column references?
        (array? x) (cond (= 0 (count x)) (v/with-warning "Column references should not be empty" nil)
                          (not-every? string? x) (v/with-warning "Column references should all be strings" nil)
                          :else (v/pure x))))
(def foreign-key
  (object-of
    {:required {"columnReference" column-reference
                "reference"       object                    ;;TODO: specify reference
                }}))
(def schema
  (object-of
    {:optional {"columns" columns
                "foreignKeys" (array-of foreign-key)        ;;TODO: validate foreign keys
                "primaryKey" column-reference               ;;TODO: validators MUST check that each row has a unique combination of values of cells in the indicated columns
                "rowTitles" column-reference
                "@id" id
                "@type" (eq "Schema")
                }}))

;;TODO: The properties on these objects are interpreted equivalently to common properties as described in section 5.8 Common Properties.
(def note v/pure)

(def transformation-source-types #{"json" "rdf"})

(defn validate-transformation-source [s]
  (if (contains? transformation-source-types s)
    (v/pure (keyword s))
    (v/with-warning (str "Expected one of " (string/join ", " transformation-source-types)) nil)))

(def transformation-source
  (compm string validate-transformation-source))

(def transformation
  (object-of
    {:required {"url" link-property
                "scriptFormat" link-property
                "targetFormat" link-property}
     :optional {"source" transformation-source
                "titles" natural-language
                "@id" id
                "@type" "Template"}}))

(def table
  (object-of
    {:required {"url" link-property}
     :optional {"notes" (array-of note)
                "suppressOutput" bool
                "tableDirection" table-direction
                "tableSchema" (linked-object schema)
                "transformations" (array-of transformation)
                "@id" id
                "@type" (eq "Table")
                }
     :defaults {"suppressOutput" false
                "tableDirection" "auto"}}))

(def table-group
  (object-of
    {:required {"tables" (array-of table {:min-length 1})}
     :optional {"dialect"         (linked-object dialect)
                "notes"           (array-of note)
                "tableDirection"  table-direction
                "tableSchema"     (linked-object schema)
                "transformations" (array-of transformation)
                "@id"             id
                "@type"           (eq "TableGroup")}
     :defaults {"tableDirection" "auto"}}))

(def datatype-name
  (compm string (one-of datatype/type-names)))

(defn datatype-format [x]
  ;;TODO: implement!
  (v/pure x)
  )

(defn datatype-bound [x]
  (if (or (number? x) (string? x))
    (v/pure x)
    (v/of-error (str "Expected string or number, got " (get-json-type x)))))

(defn ^{:metadata-spec "5.11.2"} validate-derived-datatype
  [{:strs [base length minLength maxLength minimum minInclusive minExclusive maximum maxInclusive maxExclusive] :as dt}]
  (cond
    ;;applications MUST raise an error if both length and minLength are specified and length is less than minLength
    (and (some? length) (some? minLength) (< length minLength))
    (v/of-error "Length must be >= minLength")

    ;;applications MUST raise an error if both length and maxLength are specified and length is greater than maxLength
    (and (some? length) (some? maxLength) (> length maxLength))
    (v/of-error "Length must be <= maxLength")

    ;;applications MUST raise an error if minLength and maxLength are both specified and minLength is greater than maxLength
    (and (some? minLength) (some? maxLength) (> minLength maxLength))
    (v/of-error "minLength must be <= maxLength")

    ;;applications MUST raise an error if length, maxLength, or minLength are specified and the base datatype is
    ;;neither string, a subtype of string, nor a binary type
    (and (or (some? length) (some? minLength) (some? maxLength))
         (not (or (datatype/is-subtype? "string" base) (datatype/is-binary-type? base))))
    (v/of-error "length, minLength and maxLength properties only valid on string or binary data types")

    ;;Applications MUST raise an error if both minimum and minInclusive are specified and they do not have the same value
    (and (some? minimum) (some? minInclusive) (not= minimum minInclusive))
    (v/of-error "minimum and minInclusive must be equal when both specified")

    ;;applications MUST raise an error if both maximum and maxInclusive are specified and they do not have the same value
    (and (some? maximum) (some? maxInclusive) (not= maximum maxInclusive))
    (v/of-error "maximum and maxInclusive must be equal when both specified")

    ;;applications MUST raise an error if both minInclusive and minExclusive are specified
    (and (some? minInclusive) (some? minExclusive))
    (v/of-error "Cannot specify both minInclusive and minExclusive")

    ;;...or if both maxInclusive and maxExclusive are specified
    (and (some? maxInclusive) (some? maxExclusive))
    (v/of-error "Cannot specify both maxInclusive and maxExclusive")

    ;;Applications MUST raise an error if both minInclusive and maxInclusive are specified and maxInclusive is less than minInclusive
    (and (some? minInclusive) (some? maxInclusive) (< maxInclusive minInclusive))
    (v/of-error "minInclusive must be <= maxInclusive")

    ;;...or if both minInclusive and maxExclusive are specified and maxExclusive is less than or equal to minInclusive
    (and (some? minInclusive) (some? maxExclusive) (<= maxExclusive minInclusive))
    (v/of-error "minInclusive must be < maxExclusive")

    ;;applications MUST raise an error if both minExclusive and maxExclusive are specified and maxExclusive is less than minExclusive
    (and (some? minExclusive) (some? maxExclusive) (< maxExclusive minExclusive))
    (v/of-error "minExclusive must be <= maxExclusive")

    ;;...or if both minExclusive and maxInclusive are specified and maxInclusive is less than or equal to minExclusive
    (and (some? minExclusive) (some? maxInclusive) (<= maxInclusive minExclusive))
    (v/of-error "maxInclusive must be > minExclusive")

    ;;Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive
    ;;are specified and the base datatype is not a numeric, date/time, or duration type
    (and (or (some? minimum) (some? minInclusive) (some? maximum) (some? maxInclusive) (some? minExclusive) (some? maxExclusive))
         (not (or (datatype/is-numeric-type? base) (datatype/is-date-time-type? base) (datatype/is-duration-type? base))))
    (v/of-error "minimum, minInclusive, maximum, maxInclusvie, minExclusive, maxExclusive only valid for numeric, date/time or duration types")

    :else (v/pure dt)))

(defn validate-datatype-id [id]
  ;;TODO: implement!
  ;;datatype id MUST NOT be the URL of a built-in datatype.
  (v/pure id))

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

(defn datatype [x]
  (cond (string? x) (datatype-name x)
        (object? x) (derived-datatype x)
        :else (v/of-error (str "Expected data type name or derived datatype object, got " (get-json-type x)))))

(defn null-value [x]
  (cond (string? x) (v/pure [x])
        (array? x) ((array-of string) x)
        :else (v/of-error (str "Expected string or string array, got " (get-json-type x)))))

(def inherited-properties
  (object-of
    {:optional {"aboutUrl" template-property
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
                }
     :defaults {"default" ""
                "lang" "und"
                "null" [""]
                "ordered" false
                "required" false
                "separator" nil
                "tableDirection" "inherit"}}))