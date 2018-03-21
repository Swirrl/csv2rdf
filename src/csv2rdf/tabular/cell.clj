(ns csv2rdf.tabular.cell
  (:require [clojure.string :as string]
            [csv2rdf.metadata.column :as mcolumn]
            [csv2rdf.xml.datatype :as xml-datatype])
  (:import [java.util.regex Pattern]))

(def column-required-message "Column value required")

(defn ^{:table-spec "6.4.1"} replace-special-whitespace [value column]
  (if-not (contains? #{"string" "json" "xml" "html" "anyAtomicType"} (mcolumn/datatype-base column))
    (string/replace value #"[\x{d}\x{a}\x{9}]" " ")
    value))

(defn ^{:table-spec "6.4.2"} strip-whitespace [value column]
  (if-not (contains? #{"string" "json" "xml" "html" "anyAtomicType" "normalizedString"} (mcolumn/datatype-base column))
    (-> value (string/trim) (string/replace #"\s+" " "))
    value))

(defn ^{:table-spec "6.4.3"} column-default-if-empty [value column]
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

(defn ^{:table-spec "6.4.8"} parse-format [{:keys [value errors] :as cell} {:keys [lang datatype] :as column}]
  ;;TODO: implement according to the spec. So far we only expect string column types so set the lang
  ;;on the cell value
  (if-not (= "string" (:base datatype))
    (throw (IllegalArgumentException. "Only string base types supported"))
    {:value {:value value :stringValue value :datatype datatype :lang lang}
     :errors errors}))

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

(defn parse-cell-value [value {:keys [separator required datatype] :as column}]
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

(defn ^{:table-spec "6.4"} expand-urls [cell column]
  ;;TODO: implement!
  cell)

(defn ^{:table-spec "6.4"} parse-cell
  "Parses a cell value in the input CSV to obtain the semantic value."
  [value {:keys [] :as column}]
  (let [cleaned (-> value
                    (replace-special-whitespace column)
                    (strip-whitespace column)
                    (column-default-if-empty column))
        cell (parse-cell-value cleaned column)
        expanded (expand-urls cell column)]
    expanded))

(def value :value)
(defn semantic-value [cell]
  (let [val (value cell)]
    (if (sequential? val)
      (mapv :value val)
      (:value val))))

(def errors :errors)
(def lang :lang)