(ns csv2rdf.metadata.dialect
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [csv2rdf.util :as util]
            [csv2rdf.metadata.types :refer [object-of id non-negative] :as types]
            [csv2rdf.metadata.validator :refer [make-warning invalid mapping variant any array-of string character
                                                bool eq]]
            [csv2rdf.validation :as v])
  (:import [java.nio.charset Charset IllegalCharsetNameException]))

(def ^{:metadata-spec "5.9"} default-dialect
  {:encoding "utf-8"
   :line-terminators ["\r\n", "\n"]
   :quoteChar \"
   :doubleQuote nil                                         ;;NOTE: differs from spec - see calculate-quote-escape-chars
   :skipRows 0
   :commentPrefix \#
   :header nil                                              ;;NOTE: differs from spec - see calculate-header-row-count
   :headerRowCount nil                                      ;;NOTE: differs from spec - see calculate-header-row-count
   :delimiter \,
   :skipColumns 0
   :skipBlankRows false
   :skipInitialSpace nil                                    ;;NOTE: differs from spec - see calculate-trim-mode
   :trim nil                                                ;;NOTE: differs from spec - see calculate-trim-mode
   })

(defn parse-trim [s]
  ;;TODO: warn on parse failure?
  (some-> s (string/lower-case) {"true" :all "false" :none "start" :start "end" :end}))

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
  ;;TODO: line terminators cannot currently be configured
  (merge (select-keys dialect [:encoding :skipRows :commentPrefix :delimiter :skipColumns :skipBlankRows])
         (calculate-quote-escape-chars dialect)
         {:trim-mode (calculate-trim-mode dialect)
          :num-header-rows (calculate-header-row-count dialect)}))

(s/def ::commentPrefix char?)
(s/def ::delimiter char?)                                   ;;NOTE: differs from spec which allows strings
(s/def ::doubleQuote boolean?)
(s/def ::encoding string?)
(s/def ::header boolean?)
(s/def ::headerRowCount util/non-negative?)
(s/def ::lineTerminators (s/coll-of string? :kind vector? :into []))
(s/def ::quoteChar (s/nilable char?))                       ;;NOTE: differs from specs which allows strings
(s/def ::skipBlankRows boolean?)
(s/def ::skipRows util/non-negative?)
(s/def ::skipColumns util/non-negative?)
(s/def ::skipInitialSpace (s/nilable boolean?))
(s/def ::trim #{nil "true" "false" "start" "end"})
(s/def ::trim-mode #{:none :all :start :end})

(s/def ::dialect (s/keys :req-un [::commentPrefix ::delimiter ::doubleQuote ::encoding ::header ::headerRowCount
                                  ::lineTerminators ::quoteChar ::skipBlankRows ::skipColumns ::skipInitialSpace
                                  ::skipRows ::trim]))
(s/def ::options (s/keys :req-un [::encoding ::skipRows ::commentPrefix ::delimiter ::escapeChar ::quoteChar
                                  ::skipColumns ::skipBlankRows ::trim-mode ::num-header-rows]))

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

;;metadata parsing

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

;;TODO: merge with parse-trim
(def trim-modes {"true" :all "false" :none "start" :start "end" :end})

(def trim-mode (variant {:string  (mapping trim-modes)
                         :boolean (fn [_context b] (v/pure (if b :all :none)))}))

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
                :trimMode trim-mode
                :id id
                :type (eq "Dialect")}
     :allow-common-properties? false}))