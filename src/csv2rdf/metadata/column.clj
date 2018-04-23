(ns csv2rdf.metadata.column
  (:require [csv2rdf.xml.datatype :as xml-datatype]
            [csv2rdf.util :as util]
            [csv2rdf.validation :as v]
            [csv2rdf.metadata.validator :refer [make-warning make-error chain string invalid bool eq type-eq array-of]]
            [csv2rdf.metadata.context :refer [language-code-or-default]]
            [csv2rdf.metadata.types :refer [natural-language id]]
            [csv2rdf.metadata.inherited :refer [metadata-of] :as inherited]
            [clojure.string :as string]
            [clojure.set :as set])
  (:import [java.nio CharBuffer]
           [com.github.fge.uritemplate URITemplateParseException]
           [com.github.fge.uritemplate.parse VariableSpecParser]))

(defn datatype
  "Gets the effective datatype for this column"
  [{:keys [datatype] :as col}]
  (xml-datatype/expand datatype))

(defn datatype-base [column]
  (:base (datatype column)))

(defn ^{:metadata-spec "5.7"} default
  "Gets the effective default value for this column"
  [{:keys [default]}]
  (or default ""))

(def parse-variable-method (util/get-declared-method VariableSpecParser "parseFullName" [CharBuffer]))

(defn parse-uri-template-variable [^String s]
  (util/invoke-method parse-variable-method [(CharBuffer/wrap s)]))

(defn uri-template-variable [context s]
  (try
    (let [result (parse-uri-template-variable s)]
      ;;parseFullName returns the prefix of the input that could be parsed
      (if (= result s)
        (v/pure s)
        (make-warning context (str "Invalid template variable: '" s "'") invalid)))
    (catch URITemplateParseException _ex
      (make-warning context (str "Invalid template variable: '" s "'") invalid))))

(defn ^{:metadata-spec "5.6"} validate-column-name [context ^String s]
  (if (.startsWith s "_")
    (make-warning context "Columns names cannot begin with _" invalid)
    (uri-template-variable context s)))

(def ^{:metadata-spec "5.6"} column-name
  (chain string validate-column-name))

(def column-defaults
  {:suppressOutput false
   :virtual false})

(def column
  (metadata-of
    {:optional {:name           column-name                             ;;TODO: use first title as name if not provided, see spec
                :suppressOutput bool
                :titles         natural-language
                :virtual        bool
                :id            id
                :type (type-eq "Column")}}))

(defn get-duplicate-names [columns]
  (->> columns
       (map :name)
       (frequencies)
       (filter (fn [[n count]] (> count 1)))
       (map first)))

(defn ^{:metadata-spec "5.5"} validate-column-names [context columns]
  ;;columns property: The name properties of the column descriptions MUST be unique within a given table description.
  (let [duplicate-names (get-duplicate-names columns)]
    (if (seq duplicate-names)
      (make-error context (str "Duplicate names for columns: " (string/join ", " duplicate-names)))
      (v/pure columns))))

(defn is-virtual? [{:keys [virtual] :as column}]
  (boolean virtual))

(def non-virtual? (complement is-virtual?))

(defn ^{:metadata-spec "5.6"} validate-virtual-columns [context columns]
  ;;virtual property: If present, a virtual column MUST appear after all other non-virtual column definitions.
  (let [virtual-columns (drop-while non-virtual? columns)
        invalid-columns (filter non-virtual? virtual-columns)]
    (if (seq invalid-columns)
      (let [first-virtual (first virtual-columns)           ;;NOTE: must exist
            msg (format "Non-virtual columns %s defined after first virtual column %s - All virtual columns must exist after all non-virtual columns"
                        (string/join ", " (map :name invalid-columns))
                        (:name first-virtual))]
        (make-error context msg))
      (v/pure columns))))

(def ^{:table-spec "4.3"} index->column-number inc)

(defn index-column-name [column-index]
  (str "_col." (index->column-number column-index)))

(defn ^{:metadata-spec "5.6"} get-column-name [{:keys [name titles] :as column} column-index default-language]
  (or name
      (some-> (first (get titles default-language)) (util/percent-encode))
      (index-column-name column-index)))

(defn set-column-name [column column-index context]
  (assoc column :name (get-column-name column column-index (language-code-or-default context))))

(defn set-column-names [context columns]
  (->> columns
       (map-indexed (fn [idx col] (set-column-name col idx context)))
       (vec)
       (v/pure)))

(def columns (chain (array-of column) set-column-names validate-column-names validate-virtual-columns))

(defn expand-properties
  "Expands all properties for this column by inheriting any unspecified inherited properties from its parent
   schema."
  [parent-schema column]
  (inherited/expand-inherit parent-schema (merge column-defaults column)))

(defn from-titles
  "Creates a new column given the column index and the sequence of titles"
  [column-index titles]
  (let [normalised-titles {"und" titles}
        column-name (get-column-name {:titles normalised-titles} column-index "und")]
    {:name column-name
     :titles normalised-titles
     :virtual false
     :suppressOutput false}))

(defn from-index [column-index]
  {:name (index-column-name column-index)})

(defn indexed-non-virtual-columns
  "Returns a map {column-index column} of the non-virtual columns in the given sequence of columns"
  [columns]
  (into {} (filter (fn [[_idx col]] (non-virtual? col)) (map-indexed vector columns))))

(defn intersect-titles
  "Returns a map {lang #{matching-titles}} containing the titles that match for each language within the titles definition
   of a column."
  [titles1 titles2]
  ;;TODO: languages match if they are equal when truncated, as defined in [BCP47], to the length of the shortest language tag
  ;;und matches any language
  (let [und-intersection (set/union
                           (set/intersection (set (get titles1 "und")) (set (flatten (vals titles2))))
                           (set/intersection (set (get titles2 "und")) (set (flatten (vals titles1)))))
        common-langs (disj (set/intersection (set (keys titles1)) (set (keys titles2))) "und")]
    (into {"und" und-intersection}
          (map (fn [lang]
                 [lang (set/intersection (set (get titles1 lang)) (set (get titles2 lang)))])
               common-langs))))

(defn- titles-intersect? [column1 column2]
  (let [title-intersection (intersect-titles (:titles column1) (:titles column2))]
    (boolean (some seq (vals title-intersection)))))

(defn ^{:metadata-spec "5.5.1"} compatible? [validating? column1 column2]
  (letfn [(has-name? [column] (some? (:name column)))
          (has-title? [column] (boolean (seq (:titles column))))
          (has-name-or-title? [column] (or (has-name? column) (has-title? column)))
          (has-name-no-title? [column] (and (has-name? column) (not (has-title? column))))
          (has-title-no-name? [column] (and (has-title? column) (not (has-name? column))))]
    (or (not (has-name-or-title? column1))
        (not (has-name-or-title? column2))
        (= (:name column1) (:name column2))
        (titles-intersect? column1 column2)
        (and
          (not validating?)
          (or (and (has-name-no-title? column1) (has-title-no-name? column2))
              (and (has-name-no-title? column2) (has-title-no-name? column1)))))))

;;TODO: this may need to be done before column definitions have been normalised/expanded/inherited from their parents
(defn ^{:metadata-spec "5.5.1"} validate-compatible [validating? column-index column1 column2]
  (if (compatible? validating? column1 column2)
    (v/pure nil)
    (v/with-warning (format "Columns at index %d not compatible" column-index) nil)))
