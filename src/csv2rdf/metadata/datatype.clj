(ns csv2rdf.metadata.datatype
  (:require [csv2rdf.metadata.validator :refer [variant one-of make-error make-warning any chain eq character
                                                try-parse-with invalid? invalid type-error-message string]]
            [csv2rdf.metadata.json :refer [object?]]
            [csv2rdf.metadata.types :refer [object-of non-negative id]]
            [csv2rdf.metadata.context :refer [append-path]]
            [csv2rdf.xml.datatype :as xml-datatype]
            [csv2rdf.validation :as v]
            [clojure.string :as string])
  (:import [java.time.format DateTimeFormatter]
           [java.text DecimalFormat]))

(def ^{:metadata-spec "5.11.2"} datatype-name
  (variant {:string (one-of xml-datatype/type-names)}))

;;TODO: implement
(def number-format-pattern (chain string (try-parse-with #(DecimalFormat. %))))

;;TODO: implement!
(def numeric-type-format
  (object-of {:optional {:decimalChar character
                         :groupChar character
                         :pattern number-format-pattern}}))

(defn ^{:table-spec "6.4.2"} validate-numeric-format [context format]
  (if (empty? format)
    (make-warning context "Object must contain at least one of the keys decimalChar, groupChar or pattern" nil)
    (v/pure format)))

;;NOTE: numeric datatypes are the only ones that permit an object specifying the format, all other types allow only
;;a format string. The format of the format string depends on the datatype, so this can only be validated at the datatype
;;level, not here. See validate-derived-datatype-format
(def ^{:table-spec "6.4.2"} datatype-format
  (variant {:string number-format-pattern
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
         (not (or (xml-datatype/is-subtype? "string" base) (xml-datatype/is-binary-type? base))))
    (make-error context "length, minLength and maxLength properties only valid on string or binary data types")

    ;;Applications MUST raise an error if both minimum and minInclusive are specified and they do not have the same value
    (and (some? minimum) (some? minInclusive) (not= minimum minInclusive))
    (make-error context "minimum and minInclusive must be equal when both specified")

    ;;applications MUST raise an error if both maximum and maxInclusive are specified and they do not have the same value
    (and (some? maximum) (some? maxInclusive) (not= maximum maxInclusive))
    (make-error context "maximum and maxInclusive must be equal when both specified")

    ;;applications MUST raise an error if both minInclusive and minExclusive are specified
    (and (some? minInclusive) (some? minExclusive))
    (make-error context "Cannot specify both minInclusive and minExclusive")

    ;;...or if both maxInclusive and maxExclusive are specified
    (and (some? maxInclusive) (some? maxExclusive))
    (make-error context "Cannot specify both maxInclusive and maxExclusive")

    ;;Applications MUST raise an error if both minInclusive and maxInclusive are specified and maxInclusive is less than minInclusive
    (and (some? minInclusive) (some? maxInclusive) (< maxInclusive minInclusive))
    (make-error context "minInclusive must be <= maxInclusive")

    ;;...or if both minInclusive and maxExclusive are specified and maxExclusive is less than or equal to minInclusive
    (and (some? minInclusive) (some? maxExclusive) (<= maxExclusive minInclusive))
    (make-error context "minInclusive must be < maxExclusive")

    ;;applications MUST raise an error if both minExclusive and maxExclusive are specified and maxExclusive is less than minExclusive
    (and (some? minExclusive) (some? maxExclusive) (< maxExclusive minExclusive))
    (make-error context "minExclusive must be <= maxExclusive")

    ;;...or if both minExclusive and maxInclusive are specified and maxInclusive is less than or equal to minExclusive
    (and (some? minExclusive) (some? maxInclusive) (<= maxInclusive minExclusive))
    (make-error context "maxInclusive must be > minExclusive")

    ;;Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive
    ;;are specified and the base datatype is not a numeric, date/time, or duration type
    (and (or (some? minimum) (some? minInclusive) (some? maximum) (some? maxInclusive) (some? minExclusive) (some? maxExclusive))
         (not (or (xml-datatype/is-numeric-type? base) (xml-datatype/is-date-time-type? base) (xml-datatype/is-duration-type? base))))
    (make-error context "minimum, minInclusive, maximum, maxInclusvie, minExclusive, maxExclusive only valid for numeric, date/time or duration types")

    :else (v/pure dt)))

;;TODO: implement!
;;datatype id MUST NOT be the URL of a built-in datatype.
(def validate-datatype-id any)

(defn ^{:table-spec "6.4.3"} validate-boolean-format [context format-string]
  (let [values (string/split format-string #"\|")]
    (if (= 2 (count values))
      (v/pure (zipmap [:true-value :false-value] (map string/trim values)))
      (make-warning context "Boolean format string should have format 'true-value|false-value'" invalid))))

(defn ^{:table-spec "6.4"} validate-derived-datatype-format [context {:keys [format base] :as datatype}]
  (let [set-format (fn [value]
                     (if (invalid? value)
                       (dissoc datatype :format)
                       (assoc datatype :format value)))]
    (cond
      (nil? format)
      (v/pure datatype)

      (xml-datatype/is-numeric-type? base)
      (if (string? format)
        (v/pure (assoc datatype :format {:pattern format}))
        (v/pure datatype))

      ;;only numeric types support object format descriptions
      (object? format)
      (make-warning (append-path context "format") (type-error-message [:string] :object) (dissoc datatype :format))

      (xml-datatype/is-boolean-type? base)
      (v/fmap set-format
              (validate-boolean-format (append-path context "format") format))

      ;;TODO: check DateTimeFormatter works as required by the spec
      (xml-datatype/is-date-time-type? base)
      (v/fmap set-format
              ((try-parse-with #(DateTimeFormatter/ofPattern %)) (append-path context "format") format))

      :else
      (v/fmap set-format
              ((try-parse-with re-pattern) (append-path context "format") format)))))

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
                  :type        (eq "Datatype")}
       :allow-common-properties? true})
    validate-derived-datatype
    validate-derived-datatype-format))

(defn ^{:metadata-spec "5.7"} normalise-datatype-name [_context type-name]
  (v/pure {:base type-name}))

(def ^{:metadata-spec "5.7"} datatype
  (variant {:string (chain datatype-name normalise-datatype-name)
            :object derived-datatype}))
