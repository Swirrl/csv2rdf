(ns csv2rdf.tabular.cell
  (:require [clojure.string :as string]
            [csv2rdf.metadata.column :as mcolumn]
            [csv2rdf.xml.datatype :as xml-datatype])
  (:import [java.util.regex Pattern]
           [java.math BigDecimal BigInteger]
           [java.text DecimalFormat]))

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

(defn fail-parse [{:keys [value] :as cell} error-message]
  (-> cell
      (assoc :value {:value value :stringValue value :datatype {:base "string"}})
      (add-error error-message)))

(defn ^{:table-spec "6.4.2"
        :xml-schema-spec "3"} parse-number-unformatted [{:keys [value] :as cell} {:keys [base] :as datatype}]
  (let [parser (get numeric-parsers (xml-datatype/resolve-type-name base))]
    (try
      (let [result (parser value)]
        (assoc cell :value {:value result :stringValue value :datatype datatype}))
      (catch Exception ex
        (fail-parse cell (.getMessage ex))))))

(def special-floating-values
  {"float" {"NaN" Float/NaN "INF" Float/POSITIVE_INFINITY "-INF" Float/NEGATIVE_INFINITY}
   "double" {"NaN" Double/NaN "INF" Double/POSITIVE_INFINITY "-INF" Double/NEGATIVE_INFINITY}})

(def special-floating-names (into #{} (keys (second (first special-floating-values)))))

(defn get-special-floating-value [value base]
  (if-let [result (get-in special-floating-values [base value])]
    result
    (throw (IllegalArgumentException. (format "Unknown numeric constant %s for type %s" value base)))))

(defn parse-number-from-constructed-format [{:keys [^String value] :as cell} {{:keys [decimalChar groupChar]} :format base :base :as datatype}]
  (let [is-special? (contains? special-floating-names value)
        has-exponent? (or (.contains value "e") (.contains value "E"))
        is-decimal-type? (xml-datatype/is-subtype? "decimal" base)
        strip-group (fn ^String [^String s] (if (some? groupChar) (.replace s (str groupChar) "") s))]
    (cond
      (and (some? decimalChar) (xml-datatype/is-integral-type? base))
      (fail-parse cell (format "Cannot specify decimalChar for integral datatype %s" base))

      (and is-special? is-decimal-type?)
      (fail-parse cell (format "Cannot specify floating point value %s for decimal type %s" value base))

      (and has-exponent? is-decimal-type?)
      (fail-parse cell (format "Cannot specify exponent for type %s" base))

      is-special?
      (assoc cell :value {:value (get-special-floating-value value base) :stringValue value :datatype datatype})

      (xml-datatype/is-integral-type? base)
      (let [parser (get numeric-parsers base)]
        (try
          (assoc cell :value {:value (parser (strip-group value)) :stringValue value :datatype datatype})
          (catch Exception _ex
            (fail-parse cell (format "Failed to parse '%s' as type %s" value base)))))

      :else
      ;;remove group character, normalise decimal character to . and remove any trailing percent or mph modifier
      ;;then parse with the type parser and modify the result according to the trailing modifier
      (let [^String s (strip-group value)
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
            (assoc cell :value {:value result :stringValue value :datatype datatype}))
          (catch Exception _ex
            (fail-parse cell (format "Failed to parse '%s' as type %s" value base))))))))

(defn parse-number-format [{:keys [value] :as cell} {{:keys [^DecimalFormat pattern]} :format :as datatype}]
  (if (some? pattern)
    (if-let [result (.parse pattern value)]
      (assoc cell :value {:value result :stringValue value :datatype datatype})
      (fail-parse cell (format "Cannot parse value '%s' with the pattern '%s'" value (.toPattern pattern))))
    (parse-number-from-constructed-format cell datatype)))

(defn parse-numeric [cell {:keys [format] :as datatype}]
  (if (some? format)
    (parse-number-format cell datatype)
    (parse-number-unformatted cell datatype)))

(defn ^{:table-spec "6.4.8"} parse-format [{:keys [value errors] :as cell} {:keys [lang datatype] :as column}]
  ;;TODO: implement according to the spec. So far we only expect string column types so set the lang
  ;;on the cell value
  ;;TODO: create protcol for parsing?
  (let [base (:base datatype)]
    (cond
      ;;TODO: handle string subtypes e.g. xml, token, language etc.
      (= "string" base)
      {:value {:value value :stringValue value :datatype datatype :lang lang}
       :errors errors}

      (xml-datatype/is-numeric-type? base)
      (parse-numeric cell datatype)

      :else
      (throw (IllegalArgumentException. "Only string base types supported")))))

(defn get-length-error [{:keys [stringValue]} rel-sym length constraint]
  (if (some? constraint)
    (let [f (resolve rel-sym)]
      (if-not (f length constraint)
        (format "Invalid length %s for value %s - expected %s %s" length stringValue rel-sym constraint)))))

(def length-relations {:length '= :minLength '>= :maxLength '<=})

(defn ^{:table-spec "6.4.9"} validate-length
  "Validates the length of the cell value is valid for the constraints on the column metadata"
  [{:keys [value] :as cell} column]
  (if-let [len (xml-datatype/get-length (:value value) (:datatype value))]
    (let [len-errors (->> length-relations
                          (map (fn [[k sym]] (get-length-error value sym len (get column k))))
                          (remove nil?))]
      (update cell :errors concat len-errors))
    cell))

(defn ^{:table-spec "6.4.9"} validate-value
  "Validates the range of the cell value is valid for the constraints on the column metadata"
  [cell column]
  ;;TODO implement
  cell)

(defn ^{:table-spec "6.4.[6,7,8,9]"} parse-atomic-value
  "Parses an 'atomic' value within a cell i.e. one which should be parsed directly according to the
  column datatype."
  [value column]
  (-> value
      (column-default-if-empty column)
      (apply-column-null column)
      (parse-format column)
      (validate-length column)
      (validate-value column)))

(defn combine-cell-values [cell-values]
  (reduce (fn [acc {:keys [value errors] :as cv}]
            (-> acc
                (update :value conj value)
                (update :errors concat errors)))
          {:value [] :errors []}
          cell-values))

(defn separator->pattern [separator]
  (re-pattern (Pattern/quote separator)))

(defn parse-cell-value [^String value {:keys [separator required datatype] :as column}]
  (if (nil? separator)
    (parse-atomic-value value column)
    (if (.isEmpty value)
      (if required
        {:value [] :errors [column-required-message]}
        {:value [] :errors []})
      (if (is-column-null? value column)
        {:value nil :errors [] :list true}
        (let [trim-fn (if (contains? #{"string" "anyAtomicType"} (:base datatype)) identity string/trim)
              components (map trim-fn (string/split value (separator->pattern separator)))
              component-cells (map #(parse-atomic-value % column) components)
              cell (combine-cell-values component-cells)]
          (assoc cell :list true))))))

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
(defn semantic-value [cell]
  (let [val (value cell)]
    (if (sequential? val)
      (mapv :value val)
      (:value val))))

(def errors :errors)
(def lang :lang)