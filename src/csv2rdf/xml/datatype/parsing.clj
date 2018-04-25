(ns csv2rdf.xml.datatype.parsing
  (:require [csv2rdf.xml.datatype :as xml-datatype]
            [clojure.string :as string])
  (:import [java.net URI URISyntaxException]
           [java.util Base64]
           [javax.xml.datatype DatatypeFactory XMLGregorianCalendar]))

(defmulti parse "Parses a values to one for the named XML datatype"
          (fn [type-name string-value] (xml-datatype/dispatch-key type-name))
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

(defn parse-hex-digit [^Character c]
  (let [i (Character/digit c 16)]
    (if (= -1 i)
      (throw (IllegalArgumentException. (str "Invalid hex digit " c)))
      i)))

(defmethod parse :hexBinary [_type-name ^String string-value]
  (if (even? (.length string-value))
    (let [byte-count (/ (.length string-value) 2)
          bytes (byte-array byte-count)]
      (doseq [idx (range byte-count)]
        (let [offset (* 2 idx)
              b1 (parse-hex-digit (.charAt string-value offset))
              b2 (parse-hex-digit (.charAt string-value (inc offset)))]
          (aset bytes idx (byte (bit-and (bit-shift-left b1 4) b2)))))
      bytes)
    (throw (IllegalArgumentException. "Hex string must contain even number of characters"))))

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

;;TODO: change representation of parse date/time values?
;;TODO: validate returned values match expected XML type
(def xml-datatype-factory (DatatypeFactory/newInstance))

(defmethod parse :date [_type-name string-value]
  (.newXMLGregorianCalendar xml-datatype-factory string-value))

(defmethod parse :dateTime [_type-name string-value]
  (.newXMLGregorianCalendar xml-datatype-factory string-value))

(defmethod parse :duration [_type-name string-value]
  (.newDuration xml-datatype-factory string-value))

(defmethod parse :dayTimeDuration [_type-name string-value]
  (.newDurationDayTime xml-datatype-factory string-value))

(defmethod parse :yearMonthDuration [_type-name string-value]
  (.newDurationYearMonth xml-datatype-factory string-value))

(defmethod parse :gDay [_type-name string-value]
  (.newXMLGregorianCalendar xml-datatype-factory string-value))

(defmethod parse :gMonth [_type-name string-value]
  (.newXMLGregorianCalendar xml-datatype-factory string-value))

(defmethod parse :gMonthDay [_type-name string-value]
  (.newXMLGregorianCalendar xml-datatype-factory string-value))

(defmethod parse :gYear [_type-name string-value]
  (.newXMLGregorianCalendar xml-datatype-factory string-value))

(defmethod parse :gYearMonth [_type-name string-value]
  (.newXMLGregorianCalendar xml-datatype-factory string-value))

(defmethod parse :time [_type-name string-value]
  (.newXMLGregorianCalendar xml-datatype-factory string-value))

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
  (Double/parseDouble string-value))

(defmethod parse :float [_type-name string-value]
  (Float/parseFloat string-value))

(defmethod parse :decimal [_type-name ^String string-value]
  (BigDecimal. string-value))

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
