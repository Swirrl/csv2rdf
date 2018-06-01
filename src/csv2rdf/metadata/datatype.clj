(ns csv2rdf.metadata.datatype
  (:require [csv2rdf.metadata.validator :refer [variant one-of make-error make-warning any chain type-eq
                                                character try-parse-with invalid? invalid type-error-message
                                                string map-of ignore-invalid]]
            [csv2rdf.json :refer [object?]]
            [csv2rdf.metadata.types :refer [object-of non-negative id]]
            [csv2rdf.metadata.context :refer [append-path]]
            [csv2rdf.xml.datatype :as xml-datatype]
            [csv2rdf.xml.datatype.parsing :as xml-parsing]
            [csv2rdf.xml.datatype.compare :refer [neql? lt? lte?]]
            [csv2rdf.uax35 :as uax35]
            [clojure.string :as string])
  (:import [java.time.format DateTimeFormatter]))

(def ^{:metadata-spec "5.11.2"} datatype-name
  (variant {:string (one-of xml-datatype/type-names)}))

(def number-format-pattern (chain string (try-parse-with #(uax35/parse-number-format %))))

(defn validate-number-format-type
  "Returns a validator which checks a parsed number format is valid for the named data type"
  [type-name]
  (fn [context format]
    (cond
      (and (xml-datatype/is-subtype? "integer" type-name)
           (uax35/allows-decimal-part? format))
      (make-warning context "Decimal part not allows in integer number format" invalid)

      (and (xml-datatype/is-subtype? "decimal" type-name)
           (uax35/allows-exponent-part? format))
      (make-warning context "Exponent part not allowed in decimal number format" invalid)

      :else format)))

(defn typed-number-format-pattern [type-name]
  (chain number-format-pattern (validate-number-format-type type-name)))

(def numeric-type-format
  (object-of {:optional {:decimalChar character
                         :groupChar character
                         :pattern number-format-pattern}}))

(defn ^{:table-spec "6.4.2"} validate-numeric-format [context format]
  (if (empty? format)
    (make-warning context "Object must contain at least one of the keys decimalChar, groupChar or pattern" nil)
    format))

;;NOTE: numeric datatypes are the only ones that permit an object specifying the format, all other types allow only
;;a format string. The format of the format string depends on the datatype, so this can only be validated at the datatype
;;level, not here. See validate-derived-datatype-format
(def ^{:table-spec "6.4.2"} datatype-format
  (variant {:string any
            :object (chain numeric-type-format validate-numeric-format)}))

(def datatype-bound (variant {:number any :string any}))

(defn ^{:metadata-spec "5.11.2"} validate-derived-datatype
  [context {:keys [base length minLength maxLength minimum minInclusive minExclusive maximum maxInclusive maxExclusive] :as dt}]
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
         (not (or (xml-datatype/is-string-type? base) (xml-datatype/is-binary-type? base))))
    (make-error context "length, minLength and maxLength properties only valid on string or binary data types")

    ;;Applications MUST raise an error if both minimum and minInclusive are specified and they do not have the same value
    (and (some? minimum) (some? minInclusive) (neql? minimum minInclusive))
    (make-error context "minimum and minInclusive must be equal when both specified")

    ;;applications MUST raise an error if both maximum and maxInclusive are specified and they do not have the same value
    (and (some? maximum) (some? maxInclusive) (neql? maximum maxInclusive))
    (make-error context "maximum and maxInclusive must be equal when both specified")

    ;;applications MUST raise an error if both minInclusive and minExclusive are specified
    (and (some? minInclusive) (some? minExclusive))
    (make-error context "Cannot specify both minInclusive and minExclusive")

    ;;...or if both maxInclusive and maxExclusive are specified
    (and (some? maxInclusive) (some? maxExclusive))
    (make-error context "Cannot specify both maxInclusive and maxExclusive")

    ;;Applications MUST raise an error if both minInclusive and maxInclusive are specified and maxInclusive is less than minInclusive
    (and (some? minInclusive) (some? maxInclusive) (lt? maxInclusive minInclusive))
    (make-error context "minInclusive must be <= maxInclusive")

    ;;...or if both minInclusive and maxExclusive are specified and maxExclusive is less than or equal to minInclusive
    (and (some? minInclusive) (some? maxExclusive) (lte? maxExclusive minInclusive))
    (make-error context "minInclusive must be < maxExclusive")

    ;;applications MUST raise an error if both minExclusive and maxExclusive are specified and maxExclusive is less than minExclusive
    (and (some? minExclusive) (some? maxExclusive) (lt? maxExclusive minExclusive))
    (make-error context "minExclusive must be <= maxExclusive")

    ;;...or if both minExclusive and maxInclusive are specified and maxInclusive is less than or equal to minExclusive
    (and (some? minExclusive) (some? maxInclusive) (lte? maxInclusive minExclusive))
    (make-error context "maxInclusive must be > minExclusive")

    ;;Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive
    ;;are specified and the base datatype is not a numeric, date/time, or duration type
    (and (or (some? minimum) (some? minInclusive) (some? maximum) (some? maxInclusive) (some? minExclusive) (some? maxExclusive))
         (not (or (xml-datatype/is-numeric-type? base) (xml-datatype/is-date-time-type? base) (xml-datatype/is-duration-type? base))))
    (make-error context "minimum, minInclusive, maximum, maxInclusvie, minExclusive, maxExclusive only valid for numeric, date/time or duration types")

    :else dt))

(defn ^{:metadata-spec "5.11.2"} validate-datatype-id [context id]
  ;;datatype id MUST NOT be the URL of a built-in datatype.
  (if (xml-datatype/is-built-in-type-iri? id)
    (make-error context "Datatype @id property must not be the URL of a built-in datatype")
    id))

(defn ^{:table-spec "6.4.3"} validate-boolean-format [context format-string]
  (let [values (string/split format-string #"\|")]
    (if (= 2 (count values))
      (let [[true-value false-value] (map string/trim values)]
        {:true-values #{true-value} :false-values #{false-value}})
      (make-warning context "Boolean format string should have format 'true-value|false-value'" invalid))))

(defn parse-datetime-format [^String format-string]
  ;;escape any T literals in the pattern
  ;;TODO: do this properly
  (DateTimeFormatter/ofPattern (.replace format-string "T" "'T'")))

(defn ^{:table-spec "6.4"} validate-derived-datatype-format [context {:keys [format base] :as datatype}]
  (let [set-format (fn [value]
                     (if (invalid? value)
                       (dissoc datatype :format)
                       (assoc datatype :format value)))]
    (cond
      (nil? format)
      datatype

      ;;NOTE: non-string formats are validated as part of the datatype definition
      (xml-datatype/is-numeric-type? base)
      (if (string? format)
        (let [format-validator (typed-number-format-pattern base)
              number-format (format-validator (append-path context "format") format)]
          (if (invalid? number-format)
            (dissoc datatype :format)
            (assoc datatype :format {:pattern number-format})))
        datatype)

      ;;only numeric types support object format descriptions
      (object? format)
      (make-warning (append-path context "format") (type-error-message [:string] :object) (dissoc datatype :format))

      (xml-datatype/is-boolean-type? base)
      (set-format (validate-boolean-format (append-path context "format") format))

      ;;TODO: check DateTimeFormatter works as required by the spec
      (xml-datatype/is-date-time-type? base)
      (let [dtf ((try-parse-with parse-datetime-format) (append-path context "format") format)]
        (set-format dtf))

      :else
      (let [re ((try-parse-with re-pattern) (append-path context "format") format)]
        (set-format re)))))

(defn parse-datatype-bound [datatype-base]
  (fn [context bound-value]
    (if (number? bound-value)
      bound-value
      (let [parser (fn [s] (xml-parsing/parse datatype-base s))
            validator (ignore-invalid (try-parse-with parser))]
        (validator context bound-value)))))

(defn parse-datatype-bounds [context {:keys [base] :as derived-datatype}]
  (let [bounds-keys [:minimum :maximum :minInclusive :maxInclusive :minExclusive :maxExclusive]
        bound-validator (parse-datatype-bound base)
        bounds (select-keys derived-datatype bounds-keys)
        parsed-obj ((map-of any bound-validator) context bounds)]
    (merge derived-datatype parsed-obj)))

(def derived-datatype
  (chain
    (object-of
      {:optional {:base         datatype-name
                  :format       datatype-format
                  :length       non-negative
                  :minLength    non-negative
                  :maxLength    non-negative
                  :minimum      datatype-bound
                  :maximum      datatype-bound
                  :minInclusive datatype-bound
                  :maxInclusive datatype-bound
                  :minExclusive datatype-bound
                  :maxExclusive datatype-bound
                  :id          (chain id validate-datatype-id)
                  :type        (type-eq "Datatype")}
       :allow-common-properties? true})
    parse-datatype-bounds
    validate-derived-datatype
    validate-derived-datatype-format))

(defn ^{:metadata-spec "5.7"} normalise-datatype-name [_context type-name]
  {:base type-name})

(def ^{:metadata-spec "5.7"} datatype
  (variant {:string (chain datatype-name normalise-datatype-name)
            :object derived-datatype}))

(def datatype-defaults {:base "string"})

(defn expand-properties [datatype]
  (merge datatype-defaults datatype))