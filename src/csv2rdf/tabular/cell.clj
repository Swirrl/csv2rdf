(ns csv2rdf.tabular.cell
  (:require [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [csv2rdf.metadata.column :as mcolumn]
            [csv2rdf.metadata.datatype :as datatype]
            [csv2rdf.xml.datatype :as xml-datatype]
            [grafter.rdf.io :refer [language]]
            [grafter.rdf :as rdf]
            [csv2rdf.vocabulary :refer [xsd:date xsd:double]])
  (:import [java.util.regex Pattern]
           [java.math BigDecimal BigInteger]
           [java.text DecimalFormat]
           [grafter.rdf.protocols IRDFString]
           [java.time.format DateTimeFormatter DateTimeParseException]
           [java.time LocalDate ZoneId]
           [java.util Date]
           [javax.xml.datatype DatatypeFactory XMLGregorianCalendar]
           [javax.xml.namespace QName]
           [java.net URI]))

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

(defn add-error [cell error-message]
  (update cell :errors conj error-message))

(defn fail-parse [string-value error-message]
  {:value string-value :datatype {:base "string"} :errors [error-message]})

(defn ^{:table-spec "6.4.2"
        :xml-schema-spec "3"} parse-number-unformatted [string-value {:keys [base] :as datatype}]
  (let [parser (get numeric-parsers (xml-datatype/resolve-type-name base))]
    (try
      (let [result (parser string-value)]
        {:value result :datatype datatype :errors []})
      (catch Exception ex
        (fail-parse string-value (.getMessage ex))))))

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
    (if-let [result (.parse pattern string-value)]
      {:value result :datatype datatype :errors []}
      (fail-parse string-value (format "Cannot parse value '%s' with the pattern '%s'" string-value (.toPattern pattern))))
    (parse-number-from-constructed-format string-value datatype)))

(defn parse-numeric [string-value {:keys [format] :as datatype}]
  (if (some? format)
    (parse-number-format string-value datatype)
    (parse-number-unformatted string-value datatype)))

(defn local-date->date [ld]
  (let [zoned-date (.atStartOfDay ld (ZoneId/systemDefault))]
    (Date/from (.toInstant zoned-date))))

;;TODO: implement Grafter procotol so java Date/DateTime/Duration types can be used as values directly?
(defn local-date->rdf [^LocalDate ld]
  (let [output-format DateTimeFormatter/ISO_DATE]
    (rdf/literal (.format ld output-format) xsd:date)))

(defn parse-date [string-value datatype]
  (let [dtf (or (:format datatype) DateTimeFormatter/ISO_DATE)]
    (try
      (let [local-date (LocalDate/parse string-value dtf)
            result (local-date->rdf local-date)]
        {:value result :datatype datatype :errors []})
      (catch DateTimeParseException ex
        (fail-parse string-value (format "Cannot parse value '%s' with the expected date pattern" string-value))))))

(defn qname->uri [^QName qname]
  ;;TODO: better way to do this?
  (URI. (str (.getNamespaceURI qname) "#" (.getLocalPart qname))))

(defn xml-gregorian-calendar->literal [^XMLGregorianCalendar calendar]
  (rdf/literal (.toXMLFormat calendar) (qname->uri (.getXMLSchemaType calendar))))

(defn parse-xml-gregorian-calendar [string-value {:keys [base] :as datatype}]
  (try
    (let [dtf (DatatypeFactory/newInstance)
          ^XMLGregorianCalendar calendar (.newXMLGregorianCalendar dtf string-value)
          calendar-type (.getXMLSchemaType calendar)]
      (if (= base (.getLocalPart calendar-type))
        {:value (xml-gregorian-calendar->literal calendar) :datatype datatype :errors []}
        (fail-parse string-value (format "Cannot parse value '%s' as type %s" string-value base))))
    (catch IllegalArgumentException _ex
      (fail-parse string-value (format "Cannot parse value '%s' as type %s" string-value base)))))

(defn is-xml-gregorian-calendar-type? [datatype-base]
  (contains? #{"gDay" "gMonth" "gMonthDay" "gYear" "gYearMonth"} datatype-base))

(defn ^{:table-spec "6.4.8"} parse-format [string-value {:keys [lang datatype] :as column}]
  ;;TODO: create protcol for parsing?
  (let [base (:base datatype)]
    (cond
      ;;TODO: handle string subtypes e.g. xml, token, language etc.
      (= "string" base)
      (let [value (if (nil? lang) string-value (language string-value (keyword lang)))]
        {:value value :datatype datatype :errors []})

      (xml-datatype/is-numeric-type? base)
      (parse-numeric string-value datatype)

      (xml-datatype/is-date-time-type? base)
      (parse-date string-value datatype)

      (is-xml-gregorian-calendar-type? base)
      (parse-xml-gregorian-calendar string-value datatype)

      :else
      (throw (IllegalArgumentException. (format "Datatype %s not supported" base))))))

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

(defn ^{:table-spec "6.4.9"} validate-value
  "Validates the range of the cell value is valid for the constraints on the column metadata"
  [cell column]
  ;;TODO implement
  cell)

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
      (let [result (parse-format string-value column)
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