(ns csv2rdf.tabular.csv.dialect
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]))

(s/def ::trim-mode #{:none :all :start :end})

(def ^{:metadata-spec "5.9"} default-dialect
  {:encoding "utf-8"
   :line-terminators ["\r\n", "\n"]
   :quote-char \"
   :double-quote true
   :skip-rows 0
   :comment-prefix \#
   :header nil                                              ;;NOTE: differs from spec - see calculate-header-row-count
   :header-row-count nil                                    ;;NOTE: differs from spec - see calculate-header-row-count
   :delimiter \,
   :skip-columns 0
   :skip-blank-rows false
   :skip-initial-space nil                                  ;;NOTE: differs from spec - see calculate-trim-mode
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
  [{:keys [trim skip-initial-space] :as dialect}]
  ;; from the specification:
  ;; if true, sets the trim flag to "start" and if false, to false. If the trim property is provided, the
  ;; skipInitialSpace property is ignored
  (cond
    ;;use trim if specified
    (some? trim) trim

    (some? skip-initial-space)
    (if skip-initial-space :start :none)

    ;;use default for trim (true => :all) if neither specified
    :else :all))

(defn ^{:metadata-spec "5.9"} calculate-header-row-count
  "Calculate the number of expected header rows in the input CSV file"
  [{:keys [header header-row-count] :as dialect}]
  (cond
    ;;use header-row-count if specified
    (some? header-row-count)
    header-row-count

    (some? header)
    (if header 1 0)

    :else 1))

(defn ^{:metadata-spec "5.9"} calculate-quote-escape-chars
  "Calculates the quote and escape characters to use when reading the source CSV file. These are set by the
   quoteChar and doubleQuote dialect settings. The default quote and escape characters are both \". The specification
   is unclear on the expected behaviour if quote-char is null and double-quote is specified - this function will
   raise an exception."
  [{:keys [quote-char double-quote] :as dialect}]
  (if (nil? quote-char)
    (if (nil? double-quote)
      {:quote-char nil :escape-char nil}
      (throw (ex-info "Cannot specify doubleQuote when quoteChar is null"
                      {:type ::configuration-error`
                       :subtype ::dialect
                       :config (select-keys dialect [:quote-char :double-quote])})))
    {:quote-char quote-char
     :escape-char (if (or double-quote (nil? double-quote)) \" \\)}))

(defn calculate-dialect-options
  "Calculates the options used to configure reading for the source CSV data from the given dialect definition"
  [dialect]
  ;;TODO: line terminators cannot currently be configured
  (merge (select-keys dialect [:encoding :skip-rows :comment-prefix :delimiter :skip-columns :skip-blank-rows])
         (calculate-quote-escape-chars dialect)
         {:trim-mode (calculate-trim-mode dialect)
          :num-header-rows (calculate-header-row-count dialect)}))

(defn create-dialect [dialect]
  (merge-with (fn [v default]
                (if (some? v) v default)) dialect default-dialect))


