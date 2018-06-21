(ns csv2rdf.metadata.column
  (:require [csv2rdf.xml.datatype :as xml-datatype]
            [csv2rdf.util :as util]
            [csv2rdf.metadata.validator :refer [make-warning make-error chain string invalid bool type-eq array-of]]
            [csv2rdf.metadata.context :refer [language-code-or-default]]
            [csv2rdf.metadata.types :refer [natural-language id]]
            [csv2rdf.metadata.inherited :refer [metadata-of]]
            [csv2rdf.bcp47 :as bcp47]
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

(def parse-variable-method (util/get-declared-method VariableSpecParser "parseFullName" [CharBuffer]))

(defn parse-uri-template-variable [^String s]
  (util/invoke-method parse-variable-method [(CharBuffer/wrap s)]))

(defn uri-template-variable [context s]
  (try
    (let [result (parse-uri-template-variable s)]
      ;;parseFullName returns the prefix of the input that could be parsed
      (if (= result s)
        s
        (make-warning context (str "Invalid template variable: '" s "'") invalid)))
    (catch URITemplateParseException _ex
      (make-warning context (str "Invalid template variable: '" s "'") invalid))))

(defn ^{:metadata-spec "5.6"} validate-column-name [context ^String s]
  (if (.startsWith s "_")
    (make-warning context "Columns names cannot begin with _" invalid)
    (uri-template-variable context s)))

(def ^{:metadata-spec "5.6"} column-name
  (chain string validate-column-name))

(def column
  (metadata-of
    {:optional {:name           column-name
                :suppressOutput bool
                :titles         natural-language
                :virtual        bool
                :id            id
                :type (type-eq "Column")}}))

(def ^{:tabular-spec "4.3"} index->column-number inc)

(defn index-column-name [column-index]
  (str "_col." (index->column-number column-index)))

(defn ^{:metadata-spec "5.6"} get-column-name [{:keys [name titles] :as column} column-index default-language]
  (or name
      (some-> (first (get titles default-language)) (util/percent-encode))
      (index-column-name column-index)))

(defn get-duplicate-names [columns]
  (->> columns
       (map ::name)
       (frequencies)
       (filter (fn [[n count]] (> count 1)))
       (map first)))

(defn ^{:metadata-spec "5.5"} validate-column-names [context columns]
  ;;columns property: The name properties of the column descriptions MUST be unique within a given table description.
  (let [duplicate-names (get-duplicate-names columns)]
    (when (seq duplicate-names)
      (make-error context (str "Duplicate names for columns: " (string/join ", " duplicate-names))))))

(defn is-virtual? [{:keys [virtual] :as column}]
  (boolean virtual))

(def non-virtual? (complement is-virtual?))

(defn ^{:metadata-spec "5.6"} validate-virtual-columns [context columns]
  ;;virtual property: If present, a virtual column MUST appear after all other non-virtual column definitions.
  (let [virtual-columns (drop-while non-virtual? columns)
        invalid-columns (filter non-virtual? virtual-columns)]
    (when (seq invalid-columns)
      (make-error context "Non-virtual columns defined after first virtual column - All virtual columns must exist after all non-virtual columns"))))

(defn set-column-names [context columns]
  (let [default-language (language-code-or-default context)]
    (->> columns
         (map-indexed (fn [idx col]
                        (assoc col ::name (get-column-name col idx default-language))))
         (vec))))

(defn columns [context x]
  (let [cols ((array-of column) context x)
        cols (set-column-names context cols)]
    (validate-column-names context cols)
    (validate-virtual-columns context cols)
    cols))

(defn from-titles
  "Creates a new column given the column index and the sequence of titles with the specified language"
  [column-index titles lang]
  (let [lang (or lang "und")
        normalised-titles {lang titles}
        column-name (get-column-name {:titles normalised-titles} column-index lang)]
    {::name          column-name
     :titles         normalised-titles
     :virtual        false
     :suppressOutput false}))

(defn from-index [column-index]
  {::name (index-column-name column-index)})

(defn indexed-non-virtual-columns
  "Returns a map {column-index column} of the non-virtual columns in the given sequence of columns"
  [columns]
  (into {} (filter (fn [[_idx col]] (non-virtual? col)) (map-indexed vector columns))))

(defn- ^{:metadata-spec "5.5.1"} has-matching-truncation?
  "Returns whether any truncations of long-lang-tag match short-lang-tag."
  [^String short-lang-tag ^String long-lang-tag]
  {:pre [(>= (.length long-lang-tag) (.length short-lang-tag))]}
  (boolean (some #(bcp47/language-tag-strings-equal? short-lang-tag %) (bcp47/language-tag-string-truncation-strings long-lang-tag))))

(defn- ^{:metadata-spec "5.5.1"} common-lang [^String lang1 ^String lang2]
  (cond
    (bcp47/language-tag-strings-equal? lang1 lang2)
    lang1

    (and (< (.length lang1) (.length lang2))
         (has-matching-truncation? lang1 lang2))
    lang1

    (and (> (.length lang1) (.length lang2))
         (has-matching-truncation? lang2 lang1))
    lang2

    :else nil))

(defn find-compatible-titles [lang titles titles-map]
  (let [title-set (set titles)]
    (reduce (fn [acc [other-lang lang-titles]]
              (if-let [cl (common-lang lang other-lang)]
                (assoc acc cl (set/union (get acc cl) (set/intersection title-set (set lang-titles))))
                acc))
            {}
            titles-map)))

(defn intersect-titles
  "Returns a map {lang #{matching-titles}} containing the titles that match for each language within the titles definition
   of a column."
  [titles1 titles2]
  ;;und matches any language
  (let [und-intersection (set/union
                           (set/intersection (set (get titles1 "und")) (set (flatten (vals titles2))))
                           (set/intersection (set (get titles2 "und")) (set (flatten (vals titles1)))))
        titles1-real (dissoc titles1 "und")
        titles2-real (dissoc titles2 "und")
        common-langs (map (fn [[lang titles]] (find-compatible-titles lang titles titles2-real)) titles1-real)]
    (merge (if (empty? und-intersection) {} {"und" und-intersection})
           (apply merge-with set/union common-langs))))

(defn- titles-intersect? [column1 column2]
  (let [title-intersection (intersect-titles (:titles column1) (:titles column2))]
    (boolean (some seq (vals title-intersection)))))

(defn- column-names-equal? [{name1 :name :as col1} {name2 :name :as col2}]
  (and (some? name1) (some? name2) (= name1 name2)))

(defn ^{:metadata-spec "5.5.1"} compatible? [validating? column1 column2]
  (letfn [(has-name? [column] (some? (:name column)))
          (has-title? [column] (boolean (seq (:titles column))))
          (has-name-or-title? [column] (or (has-name? column) (has-title? column)))
          (has-name-no-title? [column] (and (has-name? column) (not (has-title? column))))
          (has-title-no-name? [column] (and (has-title? column) (not (has-name? column))))]
    (or (not (has-name-or-title? column1))
        (not (has-name-or-title? column2))
        (column-names-equal? column1 column2)
        (titles-intersect? column1 column2)
        (and
          (not validating?)
          (or (and (has-name-no-title? column1) (has-title-no-name? column2))
              (and (has-name-no-title? column2) (has-title-no-name? column1)))))))
