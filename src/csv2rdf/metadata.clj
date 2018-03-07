(ns csv2rdf.metadata
  (:require [csv2rdf.validation :as v]
            [clojure.spec.alpha :as s]
            [clojure.string :as string])
  (:import (java.net URI URISyntaxException)))

(defn eq [expected]
  (fn [x]
    (if (= expected x)
      (v/pure x)
      (v/of-error (str "Expected '" expected "' received '" x "'")))))

(def ^{:metadata-spec "5.2"} context nil)

(defn- expect-type [type-p type-name]
  (fn [x]
    (if (type-p x)
      (v/pure x)
      (v/of-error (str "Expected " type-name)))))

(defn array? [x]
  (vector? x))

(defn- get-json-type [x]
  (cond (array? x) "array"
        (number? x) "number"
        (string? x) "string"
        (boolean? x) "boolean"
        (map? x) "object"
        :else (throw (ex-info (str "Unknown JSON type for type " (type x))
                              {:type ::json-type-error
                               :value x}))))

(def array (expect-type array? "array"))
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

(defn of-length [len]
  (fn [arr]
    (if (= len (count arr))
      (v/pure arr)
      (v/of-error (str "Expected length " len)))))

(defn compm [& fs]
  (reduce (fn [acc f]
            (fn [v]
              (v/bind f (acc v)))) v/pure fs))

(defn array-of [f & {:keys [length]}]
  (let [len-v (if (some? length) (of-length length) v/pure)]
    (compm array len-v (fn [arr]
                         (v/collect (map f arr))))))

(defn tuple [& fs]
  (compm array
         (of-length (count fs))
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

(defn either [& desc-validation-pairs]
  (let [pairs (partition-all 2 desc-validation-pairs)]
    (fn [x]
      (loop [ps pairs]
        (if (seq ps)
          (let [[_desc vf] (first ps)
                v (vf x)]
            (if (v/error? v)
              (recur (rest ps))
              v))
          (v/of-error (str "Expected one of: " (string/join ", " (map first pairs)))))))))

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

(defn bcp47-language [x]
  ;;TODO: validate valid language tag
  (v/pure x))

(defn encoding [x]
  ;;TODO: validate encoding string
  (string x))

;;TODO: merge with tabular.csv.dialect/parse-trim
(def trim-modes {"true" :all "false" :none "start" :start "end" :end})

(defn mapping [m]
  (fn [k]
    (if (contains? m k)
      (v/pure (get m k))
      (v/of-error (str "Expected one of " (string/join ", " (keys m)))))))

(def trim-mode (either "boolean" (compm boolean (fn [b] (v/pure (if b :all :none))))
                        "trim mode" (mapping trim-modes)))

(defn where [pred desc]
  (fn [x]
    (if (pred x)
      (v/pure x)
      (v/of-error (str "Expected '" x "' to be " desc)))))

(defn non-negative [x]
  (compm number (where pos? "positive")))

(def csvw-ns "http://www.w3.org/ns/csvw")

(def top-level-object (either "ns" (eq csvw-ns)
                              "local context" (each (tuple
                                                      (eq csvw-ns)
                                                      (object-of {:optional {"@base"     uri
                                                                             "@language" bcp47-language}}))
                                                    (fn [[_ns m]]
                                                      (if (or (contains? m "@base") (contains? m "@language"))
                                                        (v/pure m)
                                                        (v/of-error "Top-level object must contain @base or @language keys"))))))

(defn variable [x]
  ;;TODO: validate according to specification
  ;;TODO: add warning if not a string?
  (v/pure (if (string? x) x)))

(defn natural-language [x]
  ;;TODO: implement!
  (v/pure x))

(defn id [x]
  ;;TODO: implement!
  (v/pure x))

(def column
  (object-of
    {:optional {"name" variable                             ;;TODO: use first title as name if not provided, see spec
                "suppressOutput" bool
                "titles" natural-language                   ;;TODO: should be array?
                "virtual" bool
                "@id" id
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

(defn column-name [column]
  (get column "name"))

(defn ^{:metadata-spec "5.6"} validate-virtual-columns [columns]
  ;;virtual property: If present, a virtual column MUST appear after all other non-virtual column definitions.
  (let [non-virtual? (fn [col] (= false (get col "virtual")))
        virtual-columns (drop-while non-virtual? columns)
        invalid-columns (filter non-virtual? virtual-columns)]
    (if (seq invalid-columns)
      (let [first-virtual (first virtual-columns)           ;;NOTE: must exist
            msg (format "Non-virtual columns %s defined after first virtual column %s - All virtual columns must exist after all non-virtual columns"
                        (string/join ", " (map column-name invalid-columns))
                        (column-name first-virtual))]
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

(def dialect
  (object-of
    {:optional {"commentPrefix" character
                "delimiter" character
                "doubleQuote" bool
                "encoding" encoding
                "header" bool
                "headerRowCount" non-negative
                "lineTerminators" (either "single" (compm string (fn [x] (v/pure [x])))
                                          "array" (array-of string))
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

(def table
  (object-of
    {:required {"url" link-property}
     :optional {"notes" v/pure                              ;;TODO: The properties on these objects are interpreted equivalently to common properties as described in section 5.8 Common Properties.
                "suppressOutput" bool
                "tableDirection" table-direction
                "tableSchema" (linked-object schema)
                "transformations" v/pure                    ;;TODO: define transformation object
                "@id" id
                "@type" (eq "Table")
                }
     :defaults {"suppressOutput" false
                "tableDirection" "auto"}}))