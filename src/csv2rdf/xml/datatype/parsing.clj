(ns csv2rdf.xml.datatype.parsing
  "Functions for parsing XML datatypes in either a specified format or the default format defined by XML schema.
   The representation for datatype formats vary by the target datatype and is defined by the metadata validator.

   XML schema spec: https://www.w3.org/TR/xmlschema11-2/"
  (:require [csv2rdf.xml.datatype :as xml-datatype]
            [clojure.string :as string]
            [csv2rdf.uax35 :as uax35]
            [csv2rdf.util :as util])
  (:refer-clojure :exclude [parse-long])
  (:import [java.net URI URISyntaxException]
           [java.util Base64]
           [javax.xml.datatype DatatypeFactory]
           [java.time.format DateTimeFormatter DateTimeParseException]
           [java.time.temporal ChronoField]
           [java.time LocalDate LocalDateTime ZonedDateTime OffsetTime LocalTime ZoneOffset]
           [java.util.regex Pattern]))

(def special-float-values {"NaN" Float/NaN "INF" Float/POSITIVE_INFINITY "-INF" Float/NEGATIVE_INFINITY})
(def special-double-values {"NaN" Double/NaN "INF" Double/POSITIVE_INFINITY "-INF" Double/NEGATIVE_INFINITY})

(defmulti parse "Parses a string value for the named XML datatype. Throws IllegalArgumentException if
                 the input string is not in the expected format."
          (fn [type-name string-value] (xml-datatype/dispatch-key type-name))
          :hierarchy #'xml-datatype/dispatch-hierarchy)

(defmulti parse-format "Parses a string value into the given datatype according to format."
          (fn [type-name string-value format]
            (xml-datatype/dispatch-key type-name))
          :hierarchy #'xml-datatype/dispatch-hierarchy)

(defmethod parse :string [_type-name string-value]
  string-value)

(defmethod parse :anyURI [_type-name string-value]
  (try
    (URI. string-value)
    (catch URISyntaxException ex
      (throw (IllegalArgumentException. "Invalid URI" ex)))))

(defmethod parse :base64Binary [_type-name ^String string-value]
  (.decode (Base64/getDecoder) string-value))

(defmethod parse :hexBinary [_type-name ^String string-value]
  (util/parse-hex-string string-value))

(def ^{:xml-schema-spec "3.3.2.2"} default-boolean-mapping
  {:true-values #{"1" "true"}
   :false-values #{"0" "false"}})

(defn parse-boolean-with-mapping [string-value {:keys [true-values false-values] :as mapping}]
  (cond
    (contains? true-values string-value) true
    (contains? false-values string-value) false
    :else (let [allowed-values (mapcat identity (vals mapping))
                msg (format "Expected one of %s" (string/join ", " allowed-values))]
            (throw (IllegalArgumentException. msg)))))

(defmethod parse :boolean [_type-name string-value]
  (parse-boolean-with-mapping string-value default-boolean-mapping))

(def ^DatatypeFactory xml-datatype-factory (DatatypeFactory/newInstance))

(defn- parse-xml-gregorian-calendar
  "Parses a string as an XMLGregorianCalendar then checks the parsed type matches the expected type."
  [type ^String string-value]
  (let [type-name (name type)
        gc (.newXMLGregorianCalendar xml-datatype-factory string-value)
        local-name (.. gc getXMLSchemaType getLocalPart)]
    (if (= local-name type-name)
      gc
      (throw (IllegalArgumentException. (format "Invalid %s: %s" type-name string-value))))))

(defmethod parse :date [type-name string-value]
  (parse-format type-name string-value DateTimeFormatter/ISO_DATE))

(defmethod parse :time [type-name string-value]
  (parse-format type-name string-value DateTimeFormatter/ISO_TIME))

(defmethod parse :dateTime [type-name string-value]
  (parse-format type-name string-value DateTimeFormatter/ISO_DATE_TIME))

(defmethod parse :dateTimeStamp [type-name string-value]
  (parse-format type-name string-value DateTimeFormatter/ISO_ZONED_DATE_TIME))

(defmethod parse :duration [_type-name ^String string-value]
  (.newDuration xml-datatype-factory string-value))

(defmethod parse :dayTimeDuration [_type-name ^String string-value]
  (.newDurationDayTime xml-datatype-factory string-value))

(defmethod parse :yearMonthDuration [_type-name ^String string-value]
  (.newDurationYearMonth xml-datatype-factory string-value))

(defmethod parse :gDay [type-name string-value]
  (parse-xml-gregorian-calendar type-name string-value))

(defmethod parse :gMonth [type-name string-value]
  (parse-xml-gregorian-calendar type-name string-value))

(defmethod parse :gMonthDay [type-name string-value]
  (parse-xml-gregorian-calendar type-name string-value))

(defmethod parse :gYear [type-name string-value]
  (parse-xml-gregorian-calendar type-name string-value))

(defmethod parse :gYearMonth [type-name string-value]
  (parse-xml-gregorian-calendar type-name string-value))

;;numeric parsers

(defn parse-integer [^String s]
  (BigInteger. s))
(def parse-long #(Long/parseLong %))
(def parse-int #(Integer/parseInt %))
(def parse-short #(Short/parseShort %))

(defn check-minimum [minimum value]
  (if (< value minimum)
    (throw (IllegalArgumentException. (str "Value must be >= " minimum)))
    value))

(defn check-maximum [maximum value]
  (if (> value maximum)
    (throw (IllegalArgumentException. (str "Value must be <= " maximum)))
    value))

(defn check-bounds [minimum maximum value]
  (check-maximum maximum (check-minimum minimum value)))

(defmethod parse :double [_type-name string-value]
  (if-let [v (get special-double-values string-value)]
    v
    (Double/parseDouble string-value)))

(defmethod parse :float [_type-name string-value]
  (if-let [v (get special-float-values string-value)]
    v
    (Float/parseFloat string-value)))

(defmethod parse :decimal [type-name ^String string-value]
  ;;NOTE: BigDecimal constructor allows exponent to be specified but XML schema lexical representation
  ;;for decimal type does not. Use the default format for decimal numbers instead to parse.
  (parse-format type-name string-value {}))

(defmethod parse :integer [_type-name ^String string-value]
  (parse-integer string-value))

(defmethod parse :long [_type-name string-value]
  (parse-long string-value))

(defmethod parse :int [_type-name string-value]
  (Integer/parseInt string-value))

(defmethod parse :short [_type-name string-value]
  (Short/parseShort string-value))

(defmethod parse :byte [_type-name string-value]
  (Byte/parseByte string-value))

(defmethod parse :nonNegativeInteger [_type-name string-value]
  (check-minimum 0 (parse-integer string-value)))

(defmethod parse :positiveInteger [_type-name string-value]
  (check-minimum 1 (parse-integer string-value)))

(defmethod parse :unsignedLong [_type-name string-value]
  (check-bounds 0 (biginteger 18446744073709551615) (parse-integer string-value)))

(defmethod parse :unsignedInt [_type-name string-value]
  (check-bounds 0 4294967295 (parse-long string-value)))

(defmethod parse :unsignedShort [_type-name string-value]
  (check-bounds 0 65535 (parse-int string-value)))

(defmethod parse :unsignedByte [_type-name string-value]
  (check-bounds 0 255 (parse-short string-value)))

(defmethod parse :nonPositiveInteger [_type-name string-value]
  (check-maximum 0 (parse-integer string-value)))

(defmethod parse :negativeInteger [_type-name string-value]
  (check-maximum -1 (parse-integer string-value)))

;;parse-format

(defmethod parse-format :boolean [_type-name ^String string-value format-mapping]
  (parse-boolean-with-mapping string-value format-mapping))

(defmethod parse-format :date [_type-name ^String string-value ^DateTimeFormatter date-format]
  (try
    (let [ta (.parse date-format string-value)]
      (if (.isSupported ta ChronoField/OFFSET_SECONDS)
        (let [date (LocalDate/from ta)
              offset (ZoneOffset/from ta)]
          (.. date (atStartOfDay) (atOffset offset)))
        (LocalDate/from ta)))
    (catch DateTimeParseException ex
      (throw (IllegalArgumentException. "Date does not match format" ex)))))

(defmethod parse-format :dateTime [_type-name ^String string-value ^DateTimeFormatter date-format]
  (try
    (let [ta (.parse date-format string-value)]
      (if (.isSupported ta ChronoField/OFFSET_SECONDS)
        (ZonedDateTime/from ta)
        (LocalDateTime/from ta)))
    (catch DateTimeParseException ex
      (throw (IllegalArgumentException. "DateTime does not match format" ex)))))

(defmethod parse-format :dateTimeStamp [_type-name ^String string-value ^DateTimeFormatter date-format]
  (try
    (ZonedDateTime/parse string-value date-format)
    (catch DateTimeParseException ex
      (throw (IllegalArgumentException. "DateTimeStamp does not match format" ex)))))

(defmethod parse-format :time [_type-name ^String string-value ^DateTimeFormatter date-format]
  (try
    (let [ta (.parse date-format string-value)]
      (if (.isSupported ta ChronoField/OFFSET_SECONDS)
        (OffsetTime/from ta)
        (LocalTime/from ta)))
    (catch DateTimeParseException ex
      (throw (IllegalArgumentException. "Time does not match format" ex)))))

(defmethod parse-format :html [type-name ^String string-value ^Pattern _pattern]
  (parse type-name string-value))

(defmethod parse-format :xml [type-name ^String string-value ^Pattern _pattern]
  (parse type-name string-value))

(defmethod parse-format :json [type-name ^String string-value ^Pattern _pattern]
  (parse type-name string-value))

;;numbers

(defn- construct-integer-string [{:keys [negative? integer-digits]}]
  (format "%s%s" (if negative? "-" "") integer-digits))

(defmethod parse-format :integer [type-name string-value {:keys [pattern decimalChar groupChar] :as fmt}]
  (let [int-format (if (some? pattern)
                     pattern
                     (uax35/create-integer-format groupChar decimalChar))
        {:keys [modifier] :as result} (uax35/parse-number string-value int-format)
        constructed (construct-integer-string result)]
    (uax35/apply-modifier (parse type-name constructed) modifier)))

(defn- ^String construct-decimal-string [{:keys [decimal-digits] :as result}]
  (format "%s.%s" (construct-integer-string result) decimal-digits))

(defmethod parse-format :decimal [type-name string-value {:keys [pattern decimalChar groupChar] :as fmt}]
  (let [decimal-format (or pattern (uax35/create-decimal-format groupChar decimalChar))
        {:keys [modifier] :as result} (uax35/parse-number string-value decimal-format)
        constructed (construct-decimal-string result)]
    (uax35/apply-modifier (BigDecimal. constructed) modifier)))

(defn- construct-floating-string [{:keys [negative? integer-digits decimal-digits exponent-digits exponent-negative?]}]
  (let [exponent (if (string/blank? exponent-digits)
                  ""
                  (format "e%s%s" (if exponent-negative? "-" "") exponent-digits))]
    (format "%s%s.%s%s"
            (if negative? "-" "")
            integer-digits
            decimal-digits
            exponent)))

(defn- normalise-formatted-floating [string-value {:keys [pattern decimalChar groupChar] :as fmt}]
  (let [float-format (if (some? pattern)
                       pattern
                       (uax35/create-floating-format groupChar decimalChar))
        {:keys [modifier] :as result} (uax35/parse-number string-value float-format)
        constructed (construct-floating-string result)]
    {:floating-string constructed :modifier modifier}))

(defmethod parse-format :float [_type-name string-value fmt]
  (if-let [special (get special-float-values string-value)]
    special
    (let [{:keys [floating-string modifier] :as normalised} (normalise-formatted-floating string-value fmt)]
      (uax35/apply-modifier (Float/parseFloat floating-string) modifier))))

(defmethod parse-format :double [_type-name string-value fmt]
  (if-let [special (get-in special-double-values string-value)]
    special
    (let [{:keys [floating-string modifier] :as normalised} (normalise-formatted-floating string-value fmt)]
      (uax35/apply-modifier (Double/parseDouble floating-string) modifier))))

(defmethod parse-format :default [type-name ^String string-value ^Pattern pattern]
  (if (some? (re-matches pattern string-value))
    (parse type-name string-value)
    (throw (IllegalArgumentException. (format "'%s' does not match regular expression %s" string-value pattern)))))
