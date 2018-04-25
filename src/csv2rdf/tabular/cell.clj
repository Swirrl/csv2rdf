(ns csv2rdf.tabular.cell
  (:require [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [csv2rdf.metadata.column :as mcolumn]
            [csv2rdf.metadata.datatype :as datatype]
            [csv2rdf.xml.datatype :as xml-datatype]
            [csv2rdf.xml.datatype.parsing :as xml-parsing]
            [grafter.rdf.io :refer [language]]
            [grafter.rdf :as rdf]
            [csv2rdf.vocabulary :refer :all])
  (:import [java.util.regex Pattern]
           [java.math BigDecimal BigInteger]
           [java.text DecimalFormat ParseException]
           [grafter.rdf.protocols IRDFString]
           [java.time.format DateTimeFormatter DateTimeParseException]
           [java.time LocalDate ZoneId LocalDateTime ZonedDateTime LocalTime]
           [java.util Date]
           [javax.xml.datatype XMLGregorianCalendar]
           [java.net URI URISyntaxException]
           [java.time.temporal TemporalAccessor ChronoField]))

(def column-required-message "Column value required")

(defn ^{:table-spec "6.4.1"} replace-special-whitespace [value column]
  (if-not (contains? #{"string" "json" "xml" "html" "anyAtomicType"} (mcolumn/datatype-base column))
    (string/replace value #"[\x{d}\x{a}\x{9}]" " ")
    value))

(defn ^{:table-spec "6.4.2"} strip-whitespace [value column]
  (if-not (contains? #{"string" "json" "xml" "html" "anyAtomicType" "normalizedString"} (mcolumn/datatype-base column))
    (-> value (string/trim) (string/replace #"\s+" " "))
    value))

(defn ^{:table-spec "6.4.3"} column-default-if-empty [^String value column]
  (if (.isEmpty value)
    (mcolumn/default column)
    value))

(defn is-column-null? [value {:keys [null]}]
  (some #(= value %) null))

(defn ^{:table-spec "6.4.7"} apply-column-null [value {:keys [required separator] :as column}]
  (if (is-column-null? value column)
    (if (and (nil? separator) required)
      {:value nil :errors [column-required-message]}
      {:value nil :errors []})
    {:value value :errors []}))

(defn add-error [cell error-message]
  (update cell :errors conj error-message))

(defn fail-parse [string-value error-message]
  {:value string-value :datatype {:base "string"} :errors [error-message]})

(defn parse-xml-unformatted [string-value {:keys [base] :as datatype}]
  (try
    (let [value (xml-parsing/parse base string-value)]
      {:value value :datatype datatype :errors []})
    (catch IllegalArgumentException ex
      (fail-parse string-value (format "Cannot parse '%s' as type '%s': %s" string-value base (.getMessage ex))))))

(defn bound-num [type-name min max]
  {:pre [(or (some? min) (some? max))]}
  (let [check-min (if (some? min)
                    (fn [n]
                      (if (>= n min)
                        n
                        (throw (IllegalArgumentException. (format "%s values must be >= %d" type-name min)))))
                    identity)
        check-max (if (some? max)
                    (fn [n]
                      (if (<= n max)
                        n
                        (throw (IllegalArgumentException. (format "%s values must be <= %d" type-name max)))))
                    identity)]
    (fn [n]
      (-> n (check-min) (check-max)))))

;;TODO: implement Grafter procotol so java Date/DateTime/Duration types can be used as values directly?

(defn parse-integer [^String s]
  (BigInteger. s))
(def parse-long #(Long/parseLong %))
(def parse-int #(Integer/parseInt %))
(def parse-short #(Short/parseShort %))

(def numeric-parsers
  {"double" #(Double/parseDouble %)
   "float" #(Float/parseFloat %)
   "decimal" (fn [^String s] (BigDecimal. s))
   "integer" parse-integer
   "long" parse-long
   "int" parse-int
   "short" parse-short
   "byte" #(Byte/parseByte %)
   "nonNegativeInteger" (comp (bound-num "nonNegativeInteger" 0 nil) parse-integer)
   "positiveInteger" (comp (bound-num "positiveInteger" 1 nil) parse-integer)
   "unsignedLong" (comp (bound-num "unsignedLong" 0 (biginteger 18446744073709551615)) parse-integer)
   "unsignedInt" (comp (bound-num "unsignedInt" 0 4294967295) parse-long)
   "unsignedShort" (comp (bound-num "unsignedShort" 0 65535) parse-int)
   "unsignedByte" (comp (bound-num "unsignedByte" 0 255) parse-short)
   "nonPositiveInteger" (comp (bound-num "nonPositiveInteger" nil 0) parse-integer)
   "negativeInteger" (comp (bound-num "negativeInteger" nil -1) parse-integer)})

(def special-floating-values
  {"float" {"NaN" Float/NaN "INF" Float/POSITIVE_INFINITY "-INF" Float/NEGATIVE_INFINITY}
   "double" {"NaN" Double/NaN "INF" Double/POSITIVE_INFINITY "-INF" Double/NEGATIVE_INFINITY}})

(def special-floating-names (into #{} (keys (second (first special-floating-values)))))

(defn get-special-floating-value [value base]
  (if-let [result (get-in special-floating-values [base value])]
    result
    (throw (IllegalArgumentException. (format "Unknown numeric constant %s for type %s" value base)))))

(defn parse-number-from-constructed-format [^String string-value {{:keys [decimalChar groupChar]} :format base :base :as datatype}]
  (let [is-special? (contains? special-floating-names string-value)
        has-exponent? (or (.contains string-value "e") (.contains string-value "E"))
        is-decimal-type? (xml-datatype/is-subtype? "decimal" base)
        strip-group (fn ^String [^String s] (if (some? groupChar) (.replace s (str groupChar) "") s))]
    (cond
      (and (some? decimalChar) (xml-datatype/is-integral-type? base))
      (fail-parse string-value (format "Cannot specify decimalChar for integral datatype %s" base))

      (and is-special? is-decimal-type?)
      (fail-parse string-value (format "Cannot specify floating point value %s for decimal type %s" string-value base))

      (and has-exponent? is-decimal-type?)
      (fail-parse string-value (format "Cannot specify exponent for type %s" base))

      is-special?
      {:value (get-special-floating-value string-value base) :datatype datatype :errors []}

      (xml-datatype/is-integral-type? base)
      (let [parser (get numeric-parsers base)]
        (try
          {:value (parser (strip-group string-value)) :datatype datatype :errors []}
          (catch Exception _ex
            (fail-parse string-value (format "Failed to parse '%s' as type %s" string-value base)))))

      :else
      ;;remove group character, normalise decimal character to . and remove any trailing percent or mph modifier
      ;;then parse with the type parser and modify the result according to the trailing modifier
      (let [^String s (strip-group string-value)
            s (.replace s (str (or decimalChar \.)) ".")
            [s modifier] (cond (.endsWith s "%") [(.substring s 0 (dec (.length s))) :percent]
                               (.endsWith s "\u2030") [(.substring s 0 (dec (.length s))) :mph]
                               :else [s nil])
            parser (get numeric-parsers base)]
        (try
          (let [result (parser s)
                result (case modifier
                         :percent (/ result 100)
                         :mph (/ result 1000)
                         result)]
            {:value result :datatype datatype :errors []})
          (catch Exception _ex
            (fail-parse string-value (format "Failed to parse '%s' as type %s" string-value base))))))))

(defn parse-number-format [string-value {{:keys [^DecimalFormat pattern]} :format :as datatype}]
  (if (some? pattern)
    (try
      {:value (.parse pattern string-value) :datatype datatype :errors []}
      (catch ParseException _ex
        (fail-parse string-value (format "Cannot parse value '%s' with the pattern '%s'" string-value (.toPattern pattern)))))
    (parse-number-from-constructed-format string-value datatype)))

(defn local-date->date [ld]
  (let [zoned-date (.atStartOfDay ld (ZoneId/systemDefault))]
    (Date/from (.toInstant zoned-date))))

(defn try-parse-date-format [string-value datatype]
  (let [dtf (or (:format datatype) DateTimeFormatter/ISO_DATE)
        local-date (LocalDate/parse string-value dtf)
        formatted (.format DateTimeFormatter/ISO_DATE local-date)]
    (rdf/literal formatted xsd:date)))

(defn try-parse-datetime-format [string-value datatype]
  (let [dtf (or (:format datatype) DateTimeFormatter/ISO_DATE_TIME)
        local-datetime (LocalDateTime/parse string-value dtf)
        formatted (.format DateTimeFormatter/ISO_DATE_TIME local-datetime)]
    (rdf/literal formatted xsd:dateTime)))

(defn try-parse-datetimestamp-format [string-value datatype]
  (let [dtf (or (:format datatype) DateTimeFormatter/ISO_ZONED_DATE_TIME)
        zoned-datetime (ZonedDateTime/parse string-value dtf)
        formatted (.format zoned-datetime DateTimeFormatter/ISO_ZONED_DATE_TIME)]
    (rdf/literal formatted xsd:dateTimeStamp)))

(defn try-parse-time-format [string-value datatype]
  (let [tf (or (:format datatype) DateTimeFormatter/ISO_TIME)
        local-date (LocalTime/parse string-value tf)
        formatted (.format local-date DateTimeFormatter/ISO_TIME)]
    (rdf/literal formatted xsd:time)))

(def date-parsers
  {"date" try-parse-date-format
   "dateTime" try-parse-datetime-format
   "dateTimeStamp" try-parse-datetimestamp-format
   "time" try-parse-time-format})

(defn parse-date-format [string-value {:keys [base] :as datatype}]
  (if-let [parser (get date-parsers (xml-datatype/resolve-type-name base))]
    (try
      {:value (parser string-value datatype) :datatype datatype :errors []}
      (catch DateTimeParseException ex
        (fail-parse string-value (format "Cannot parse value '%s' with the expected date pattern for type '%s'" string-value base))))
    (throw (IllegalArgumentException. (format "Invalid date/time type '%s'" base)))))

(defn xml-gregorian-calendar->literal [^XMLGregorianCalendar calendar {:keys [base] :as datatype}]
  (rdf/literal (.toXMLFormat calendar) (xml-datatype/get-datatype-iri base)))

(defn is-compatible-gregorian-calendar-type? [^XMLGregorianCalendar calendar {:keys [base] :as datatype}]
  (let [local-part (.getLocalPart (.getXMLSchemaType calendar))]
    (or (and (= base "dateTimeStamp")
             (= local-part "dateTime"))
        (= (xml-datatype/resolve-type-name base) local-part))))

(defn parse-xml-gregorian-calendar [string-value {:keys [base] :as datatype}]
  (try
    (let [^XMLGregorianCalendar calendar (xml-parsing/parse base string-value)]
      ;;TODO: remove check and validate output types in each parsing function
      (if (is-compatible-gregorian-calendar-type? calendar datatype)
        {:value (xml-gregorian-calendar->literal calendar datatype) :datatype datatype :errors []}
        (fail-parse string-value (format "Cannot parse value '%s' as type %s" string-value base))))
    (catch IllegalArgumentException _ex
      (fail-parse string-value (format "Cannot parse value '%s' as type %s" string-value base)))))

(defn is-xml-gregorian-calendar-type? [datatype-base]
  (or (xml-datatype/is-date-time-type? datatype-base)
      (contains? #{"gDay" "gMonth" "gMonthDay" "gYear" "gYearMonth"} datatype-base)))

;;TODO: remove when parse-formatted function has been added to xml.datatype.parsing
(defn parse-boolean-with-mapping [string-value datatype {:keys [true-values false-values] :as mapping}]
  (cond
    (contains? true-values string-value) {:value true :datatype datatype :errors []}
    (contains? false-values string-value) {:value false :datatype datatype :errors []}
    :else (let [allowed-values (mapcat identity (vals mapping))
                msg (format "Cannot parse '%s' as a boolean: expected one of %s" string-value (string/join ", " allowed-values))]
            (fail-parse string-value msg))))

(defn parse-boolean-format [string-value {:keys [format] :as datatype}]
  (parse-boolean-with-mapping string-value datatype format))

(defn parse-duration [string-value {:keys [base] :as datatype}]
  (try
    ;;NOTE: just used for validation
    ;;XML Duration type normalises the parsed value when outputting as strings, which the test cases do not
    ;;expect e.g. an input string of P20M is formatted as P1Y8M
    (xml-parsing/parse base string-value)
    (let [datatype-uri (xml-datatype/get-datatype-iri base)
          rdf-lit (rdf/literal string-value datatype-uri)]
      {:value rdf-lit :datatype datatype :errors []})
    (catch IllegalArgumentException _ex
      (fail-parse string-value (format "Cannot parse '%s' as type %s" string-value base)))
    (catch UnsupportedOperationException _ex
      (fail-parse string-value (format "Value '%s' of type %s too large for the implementation" string-value base)))))

(defn parse-datatype-unformatted [string-value {:keys [base] :as datatype} {:keys [lang] :as column}]
  (cond
    (= "string" base)
    (let [value (if (nil? lang) string-value (language string-value (keyword lang)))]
      {:value value :datatype datatype :errors []})

    ;;TODO: resolve all type URIs in this way
    ;;TODO: delay resolution of type URI until CSVW output
    (xml-datatype/is-string-type? base)
    {:value (rdf/literal string-value (datatype/get-datatype-iri datatype)) :datatype datatype :errors []}

    (is-xml-gregorian-calendar-type? base)
    (parse-xml-gregorian-calendar string-value datatype)

    (xml-datatype/is-duration-type? base)
    (parse-duration string-value datatype)

    :else
    (parse-xml-unformatted string-value datatype)))

(defn ^{:table-spec "6.4.6"} parse-other-types-format [string-value {base :base pattern :format :as datatype} column]
  {:pre [(instance? Pattern pattern)]}
  (if (contains? #{"html" "xml" "json"} base)
    ;;Values that are labelled as html, xml, or json SHOULD NOT be validated against those formats
    (parse-datatype-unformatted string-value datatype column)
    (if (some? (re-matches pattern string-value))
      (parse-datatype-unformatted string-value datatype column)
      (fail-parse string-value (format "'%s' does not match regular expression %s" string-value pattern)))))

(defn ^{:table-spec "6.4.5"} parse-duration-format [string-value {pattern :format :as datatype}]
  {:pre [(instance? Pattern pattern)]}
  ;;NOTE: it's not clear from the spec whether values which pass the regex must also be validated to check
  ;;they match the required duration format
  (if (some? (re-matches pattern string-value))
    (parse-duration string-value datatype)
    (fail-parse string-value (format "'%s' does not match regular expression %s" string-value pattern))))

(defn parse-time-format [string-value {time-format :format base :base :as datatype}]
  ;;TODO: merge this with parse-date-format
  (try
    (let [^TemporalAccessor ta (.parse time-format string-value)

          ;;assume OffsetTime if format has an associated offest otherwise LocalDate
          output-format (if (.isSupported ta ChronoField/OFFSET_SECONDS)
                          DateTimeFormatter/ISO_OFFSET_TIME
                          DateTimeFormatter/ISO_TIME)
          formatted (.format output-format ta)
          lit (rdf/literal formatted (xml-datatype/datatype->iri base))]
      {:value lit :datatype datatype :errors []})
    (catch DateTimeParseException _ex
      (fail-parse string-value (format "'%s' does not match the time format")))))

(defn parse-datatype-format [string-value {:keys [base format] :as datatype} column]
  (cond
    (xml-datatype/is-numeric-type? base)
    (parse-number-format string-value format)

    (= "boolean" base)
    (parse-boolean-format string-value datatype)

    ;;TODO: support time formats
    (= "time" base)
    (parse-time-format string-value datatype)

    (xml-datatype/is-date-time-type? base)
    (parse-date-format string-value datatype)

    (xml-datatype/is-duration-type? base)
    (parse-duration-format string-value datatype)

    :else
    (parse-other-types-format string-value datatype column)))

(defn ^{:table-spec "6.4.8"} parse-datatype [string-value {:keys [datatype] :as column}]
  ;;TODO: create protcol for parsing?
  (if (some? (:format datatype))
    (parse-datatype-format string-value datatype column)
    (parse-datatype-unformatted string-value datatype column)))

(defn get-length-error [{:keys [stringValue] :as cell} rel-sym length constraint]
  (if (some? constraint)
    (let [f (resolve rel-sym)]
      (if-not (f length constraint)
        (format "Invalid length %s for value '%s' - expected %s %s" length stringValue rel-sym constraint)))))

(def length-relations {:length '= :minLength '>= :maxLength '<=})

(defn ^{:table-spec "6.4.9"} validate-length
  "Validates the length of the cell value is valid for the constraints on the column metadata"
  [{:keys [value datatype] :as cell} column]
  (if-let [len (xml-datatype/get-length value datatype)]
    (let [len-errors (->> length-relations
                          (map (fn [[k sym]] (get-length-error cell sym len (get column k))))
                          (remove nil?))]
      (update cell :errors concat len-errors))
    cell))

(defn add-cell-error [{:keys [stringValue] :as cell-element} error-message]
  (-> cell-element
      (assoc :value stringValue)
      (assoc :datatype {:base "string"})
      (update :errors conj error-message)))

;;TODO: create protocol for range validations?
(defn validate-numeric-value [{:keys [value stringValue datatype] :as cell-element}]
  (let [{:keys [minimum maximum minExclusive minInclusive maxExclusive maxInclusive]} datatype]
    (cond
      (and (some? minimum) (< value minimum))
      (add-cell-error cell-element (format "'%s' fails constraint minimum" stringValue))

      (and (some? maximum) (> value maximum))
      (add-cell-error cell-element (format "'%s' fails constraint maximum" stringValue))

      (and (some? minExclusive) (<= value minExclusive))
      (add-cell-error cell-element (format "'%s' fails constraint minExclusive" stringValue))

      (and (some? minInclusive) (< value minInclusive))
      (add-cell-error cell-element (format "'%s' fails constraint minInclusive" stringValue))

      (and (some? maxExclusive) (>= value maxExclusive))
      (add-cell-error cell-element (format "'%s' fails constraint maxExclusive" stringValue))

      (and (some? maxInclusive) (> value maxInclusive))
      (add-cell-error cell-element (format "'%s' fails constraint maxInclusive" stringValue))

      :else
      cell-element)))

(defn ^{:table-spec "6.4.9"} validate-value
  "Validates the range of the cell value is valid for the constraints on the column metadata"
  [cell {{:keys [base] :as datatype} :datatype :as column}]
  (cond
    (xml-datatype/is-numeric-type? base)
    (validate-numeric-value cell)

    ;;TODO: implement for date/time and duration types
    :else cell))

(s/def ::stringValue string?)
(s/def ::list boolean?)

;;TODO: move cell datatypes to own namespace?
(s/def ::datetime (constantly true))                        ;;TODO: fix
(defn duration? [x] true)
(defn lang-string? [x] (satisfies? IRDFString x))           ;;TODO: test for record? Extend protocol to other string types?
(s/def ::value (s/or :string string? :langstring lang-string? :number number? :datetime ::datetime :duration duration? :boolean boolean? :list (s/coll-of ::element :kind vector?)))
(s/def ::element (s/keys :req-un [::value ::stringValue ::datatype/datatype]))
(s/def ::errors (s/coll-of string? :kind vector?))          ;;TODO: fix error type and move spec
(s/def ::parsed-cell (s/keys :req-un [::value ::stringValue ::list ::errors]
                             :opt-un [::datatype/datatype]))

(defn ^{:table-spec "6.4.[6,7,8,9]"} parse-atomic-value
  "Parses an 'atomic' value within a cell i.e. one which should be parsed directly according to the
  column datatype."
  [string-value {:keys [required] :as column}]
  (let [value (column-default-if-empty string-value column)]
    (if (is-column-null? value column)
      {:value nil :stringValue string-value :errors (if required [column-required-message] [])}
      (let [result (parse-datatype string-value column)
            cell (assoc result :stringValue string-value)]
        (-> cell
            (validate-length column)
            (validate-value column))))))

(s/fdef parse-atomic-value
  :args (s/cat :value string? :column (constantly true))
  :ret ::element)

(defn append-cell-value [partial-cell {:keys [errors] :as cv}]
  (-> partial-cell
      (update :value conj (dissoc cv :errors))
      (update :errors concat errors)))

(defn combine-cell-values [cell-values]
  (reduce append-cell-value {:value [] :errors []} cell-values))

(defn separator->pattern [separator]
  (re-pattern (Pattern/quote separator)))

(defn parse-cell-value [^String value {:keys [separator required datatype] :as column}]
  (if (nil? separator)
    (let [result (parse-atomic-value value column)]
      (assoc result :list false))
    (if (.isEmpty value)
      (if required
        {:value [] :list true :stringValue value :errors [column-required-message]}
        {:value [] :list true :stringValue value :errors []})
      (if (is-column-null? value column)
        {:value nil :list true :stringValue value :errors []}
        (let [trim-fn (if (contains? #{"string" "anyAtomicType"} (:base datatype)) identity string/trim)
              components (map trim-fn (string/split value (separator->pattern separator)))
              component-cells (map #(parse-atomic-value % column) components)
              cell (combine-cell-values component-cells)]
          (assoc cell :list true :stringValue value))))))

(defn ^{:table-spec "6.4"} copy-column-annotations
  "Copy required annotations onto a cell from its column"
  [cell column]
  (merge cell (select-keys column [:ordered :textDirection])))

(defn ^{:table-spec "6.4"} parse-cell
  "Parses a cell value in the input CSV to obtain the semantic value."
  [value column]
  (let [cleaned (-> value
                    (replace-special-whitespace column)
                    (strip-whitespace column)
                    (column-default-if-empty column))
        cell (parse-cell-value cleaned column)]
    (copy-column-annotations cell column)))

(def value :value)

(defn semantic-value [{:keys [list value] :as cell}]
  (cond (nil? value) nil
        list (mapv :value value)
        :else value))

(def errors :errors)
(def lang :lang)