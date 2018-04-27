(ns csv2rdf.tabular.cell
  (:require [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [csv2rdf.metadata.column :as mcolumn]
            [csv2rdf.metadata.datatype :as datatype]
            [csv2rdf.xml.datatype :as xml-datatype]
            [csv2rdf.xml.datatype.parsing :as xml-parsing]
            [csv2rdf.xml.datatype.compare :refer [lt? lte? gt? gte?]]
            [grafter.rdf.io :refer [language]]
            [csv2rdf.vocabulary :refer :all])
  (:import [java.util.regex Pattern]
           [grafter.rdf.protocols IRDFString]))

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

(defn ^{:table-spec "6.4.7"} is-column-null? [value {:keys [null]}]
  (some #(= value %) null))

(defn fail-parse [string-value error-message]
  {:value string-value :datatype {:base "string"} :errors [error-message]})

(defn add-cell-error [{:keys [stringValue] :as cell-element} error-message]
  (-> cell-element
      (assoc :value stringValue)
      (assoc :datatype {:base "string"})
      (update :errors conj error-message)))

(defn add-cell-errors [cell-element errors]
  (reduce add-cell-error cell-element errors))

(defn ^{:table-spec "6.4.8"} parse-datatype [string-value {fmt :format base :base :as datatype}]
  (try
    (let [value (if (some? fmt)
                  (xml-parsing/parse-format base string-value fmt)
                  (xml-parsing/parse base string-value))]
      {:value value :datatype datatype :errors []})
    (catch IllegalArgumentException ex
      (fail-parse string-value (format "Cannot parse '%s' as type '%s': %s" string-value base (.getMessage ex))))))

(defn get-length-error [{:keys [stringValue] :as cell-element} rel-sym length constraint]
  (if (some? constraint)
    (let [f (resolve rel-sym)]
      (if-not (f length constraint)
        (format "Invalid length %s for value '%s' - expected %s %s" length stringValue rel-sym constraint)))))

(def length-relations {:length '= :minLength '>= :maxLength '<=})

(defn ^{:table-spec "6.4.9"} validate-length
  "Validates the length of the cell value is valid for the constraints on the column metadata"
  [{:keys [value datatype] :as cell-element}]
  (if-let [len (xml-datatype/get-length value datatype)]
    (let [len-errors (->> length-relations
                          (map (fn [[k sym]] (get-length-error cell-element sym len (get datatype k))))
                          (remove nil?))]
      (add-cell-errors cell-element len-errors))
    cell-element))

(defn ^{:table-spec "6.4.9"} validate-value-bounds
  "Validates the cell value is valid for any bounds specified on its datatype"
  [{:keys [value stringValue datatype] :as cell-element}]
  (let [{:keys [minimum maximum minExclusive minInclusive maxExclusive maxInclusive]} datatype]
    (cond
      (and (some? minimum) (lt? value minimum))
      (add-cell-error cell-element (format "'%s' fails constraint minimum" stringValue))

      (and (some? maximum) (gt? value maximum))
      (add-cell-error cell-element (format "'%s' fails constraint maximum" stringValue))

      (and (some? minExclusive) (lte? value minExclusive))
      (add-cell-error cell-element (format "'%s' fails constraint minExclusive" stringValue))

      (and (some? minInclusive) (lt? value minInclusive))
      (add-cell-error cell-element (format "'%s' fails constraint minInclusive" stringValue))

      (and (some? maxExclusive) (gte? value maxExclusive))
      (add-cell-error cell-element (format "'%s' fails constraint maxExclusive" stringValue))

      (and (some? maxInclusive) (gt? value maxInclusive))
      (add-cell-error cell-element (format "'%s' fails constraint maxInclusive" stringValue))

      :else
      cell-element)))

(defn validate-value [{{:keys [base]} :datatype :as cell-element}]
  (if (or (xml-datatype/is-numeric-type? base) (xml-datatype/is-date-time-type? base) (xml-datatype/is-duration-type? base))
    (validate-value-bounds cell-element)
    cell-element))

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
  [string-value {:keys [required datatype] :as column}]
  (let [value (column-default-if-empty string-value column)]
    (if (is-column-null? value column)
      {:value nil :stringValue string-value :errors (if required [column-required-message] [])}
      (let [result (parse-datatype string-value datatype)
            cell (assoc result :stringValue string-value)]
        (-> cell
            (validate-length)
            (validate-value))))))

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
  ;;NOTE: lang not required by specification but will be needed to set the string language?
  ;;TODO: associate lang with each cell element?
  (merge cell (select-keys column [:ordered :textDirection :lang])))

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