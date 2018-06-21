(ns csv2rdf.metadata.dialect
  (:require [clojure.spec.alpha :as s]
            [csv2rdf.util :as util]
            [csv2rdf.metadata.types :refer [object-of id non-negative]]
            [csv2rdf.metadata.validator :refer [make-warning invalid mapping variant any array-of string character
                                                bool type-eq]]
            [csv2rdf.http :as http])
  (:import [java.nio.charset Charset IllegalCharsetNameException]))

(def ^{:metadata-spec "5.9"} default-dialect
  {:encoding "utf-8"
   :lineTerminators ["\r\n", "\n"]
   :quoteChar \"
   :doubleQuote nil                                         ;;NOTE: differs from spec - see calculate-quote-escape-chars
   :skipRows 0
   :commentPrefix nil                                       ;;NOTE: differs from spec - some tests contain header lines starting with # without unsetting the comment prefix
   :header nil                                              ;;NOTE: differs from spec - see calculate-header-row-count
   :headerRowCount nil                                      ;;NOTE: differs from spec - see calculate-header-row-count
   :delimiter \,
   :skipColumns 0
   :skipBlankRows false
   :skipInitialSpace nil                                    ;;NOTE: differs from spec - see calculate-trim-mode
   :trim nil                                                ;;NOTE: differs from spec - see calculate-trim-mode
   })

(def trim-mode-mapping {"true" :all "false" :none "start" :start "end" :end})
(def trim-modes (into #{} (vals trim-mode-mapping)))

(s/def ::encoding string?)
(s/def ::lineTerminators (s/coll-of string? :kind vector? :into []))
(s/def ::quoteChar (s/nilable char?))                       ;;NOTE: differs from specs which allows strings
(s/def ::doubleQuote (s/nilable boolean?))
(s/def ::skipRows util/non-negative?)
(s/def ::commentPrefix (s/nilable char?))
(s/def ::header (s/nilable boolean?))
(s/def ::headerRowCount (s/nilable util/non-negative?))
(s/def ::delimiter char?)                                   ;;NOTE: differs from spec which allows strings
(s/def ::skipColumns util/non-negative?)
(s/def ::skipBlankRows boolean?)
(s/def ::skipInitialSpace (s/nilable boolean?))
(s/def ::trim (s/nilable trim-modes))

(s/def ::dialect (s/keys :req-un [::commentPrefix ::delimiter ::doubleQuote ::encoding ::header ::headerRowCount
                                  ::lineTerminators ::quoteChar ::skipBlankRows ::skipColumns ::skipInitialSpace
                                  ::skipRows ::trim]))

(s/def ::trim-mode trim-modes)
(s/def ::options (s/keys :req-un [::encoding ::skipRows ::commentPrefix ::delimiter ::escapeChar ::quoteChar
                                  ::skipColumns ::skipBlankRows ::trim-mode ::num-header-rows ::lineTerminators]))

(defn ^{:metadata-spec "5.9"} calculate-trim-mode
  "Calculates how whitespace should be trimmed around cell values based on the 'trim' and 'skipInitialSpace'
   configuration values. The specification states skipInitialSpace should only be used if trim is not set, but
   specifies a default value of true for trim. A default value of false is also specified for skipInitialSpace.
   We use nil for the default values of both trim and skip-initial-space and only apply the defaults if neither
   is specified."
  [{:keys [trim skipInitialSpace] :as dialect}]
  ;; from the specification:
  ;; if true, sets the trim flag to "start" and if false, to false. If the trim property is provided, the
  ;; skipInitialSpace property is ignored
  (cond
    ;;use trim if specified
    (some? trim) trim

    (some? skipInitialSpace)
    (if skipInitialSpace :start :none)

    ;;use default for trim (true => :all) if neither specified
    :else :all))

(defn ^{:metadata-spec "5.9"} calculate-header-row-count
  "Calculate the number of expected header rows in the input CSV file"
  [{:keys [header headerRowCount] :as dialect}]
  (cond
    ;;use header-row-count if specified
    (some? headerRowCount)
    headerRowCount

    (some? header)
    (if header 1 0)

    :else 1))

(defn ^{:metadata-spec "5.9"} calculate-quote-escape-chars
  "Calculates the quote and escape characters to use when reading the source CSV file. These are set by the
   quoteChar and doubleQuote dialect settings. The default quote and escape characters are both \". The specification
   is unclear on the expected behaviour if quote-char is null and double-quote is specified - this function will
   raise an exception."
  [{:keys [quoteChar doubleQuote] :as dialect}]
  (if (nil? quoteChar)
    (if (nil? doubleQuote)
      {:quoteChar nil :escapeChar nil}
      (throw (ex-info "Cannot specify doubleQuote when quoteChar is null"
                      {:type ::configuration-error
                       :subtype ::dialect
                       :config (select-keys dialect [:quote-char :double-quote])})))
    {:quoteChar quoteChar
     :escapeChar (if (or doubleQuote (nil? doubleQuote)) \" \\)}))

(defn calculate-dialect-options
  "Calculates the options used to configure reading for the source CSV data from the given dialect definition"
  [dialect]
  (merge (select-keys dialect [:encoding :skipRows :commentPrefix :delimiter :skipColumns :skipBlankRows :lineTerminators])
         (calculate-quote-escape-chars dialect)
         {:trim-mode (calculate-trim-mode dialect)
          :num-header-rows (calculate-header-row-count dialect)}))

(defn expand-dialect
  "Expands a possibly partial dialect definition into a complete definition where all configuration
  values are explicitly specified."
  [dialect]
  (merge default-dialect dialect))

(defn dialect->options
  "Creates a CSV parsing options map from a dialect definition."
  [dialect]
  (calculate-dialect-options (expand-dialect dialect)))

(s/fdef expand-dialect
        :ret ::dialect)

(s/fdef calculate-dialect-options
        :args (s/cat :dialect ::dialect)
        :ret ::options)

(defn ^{:tabular-spec "6.1.3.2"} get-default-dialect
  "Get the default dialect updated according to the HTTP response headers returned along with the tabular file response."
  [tabular-file-http-headers]
  (if-let [content-type-header (get tabular-file-http-headers "Content-Type")]
    (let [{:keys [value params]} (http/parse-header content-type-header)
          {:keys [header charset]} params]
      (-> default-dialect
          (util/assoc-if (= value http/tsv-content-type) :delimiter \tab)
          (util/assoc-if (util/equals-ignore-case? "absent" header) :header false)
          (util/assoc-if (some? charset) :encoding charset)))
    default-dialect))

(defn get-default-options
  ([] (get-default-options {}))
  ([tabular-file-http-headers]
    (dialect->options (get-default-dialect tabular-file-http-headers))))

(defn ^{:tabular-spec "6.3.2"} resolve-dialect
  "Calculates the dialect for a tabular file - uses dialect if it is non-nil, otherwise
   calculates the default dialect based on the headers associated with accessing the
   tabular data source."
  [dialect tabular-file-headers]
  (or dialect (get-default-dialect tabular-file-headers)))

(defn ^{:tabular-spec "6.3.2"} resolve-options
  "Calculates the options for a tabular file based on the provided dialect and any headers
  associated with the tabular data source."
  [dialect tabular-file-headers]
  (dialect->options (resolve-dialect dialect tabular-file-headers)))

;;metadata parsing

(defn validate-encoding [context s]
  ;;NOTE: some valid encodings defined in https://www.w3.org/TR/encoding/
  ;;may not be supported by the underlying platform, reject these as invalid
  (try
    (if (Charset/isSupported s)
      s
      (make-warning context (str "Invalid encoding: '" s "'") invalid))
    (catch IllegalCharsetNameException _ex
      (make-warning context (str "Invalid encoding: '" s "'") invalid))))

(def encoding (variant {:string validate-encoding}))

(def trim-mode (variant {:string  (mapping trim-mode-mapping)
                         :boolean (fn [_context b] (if b :all :none))}))

(def line-terminators
  (variant {:string any
            :array (array-of string)}))

(def dialect
  (object-of
    {:optional {:commentPrefix character
                :delimiter character
                :doubleQuote bool
                :encoding encoding
                :header bool
                :headerRowCount non-negative
                :lineTerminators line-terminators
                :quoteChar character
                :skipBlankRows bool
                :skipColumns non-negative
                :skipInitialSpace bool
                :skipRows non-negative
                :trim trim-mode
                :id id
                :type (type-eq "Dialect")}
     :allow-common-properties? false}))