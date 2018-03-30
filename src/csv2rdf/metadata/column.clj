(ns csv2rdf.metadata.column
  (:require [csv2rdf.xml.datatype :as xml-datatype]
            [csv2rdf.util :as util]
            [csv2rdf.validation :as v]
            [csv2rdf.metadata.validator :refer [make-warning make-error chain string invalid bool eq array-of]]
            [csv2rdf.metadata.context :refer [language-code-or-default]]
            [csv2rdf.metadata.types :refer [natural-language id]]
            [csv2rdf.metadata.inherited :refer [metadata-of] :as inherited]
            [clojure.string :as string])
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

(def column
  (metadata-of
    {:optional {:name           column-name                             ;;TODO: use first title as name if not provided, see spec
                :suppressOutput bool
                :titles         natural-language
                :virtual        bool
                :id            id
                :type (eq "Column")}
     :defaults {:suppressOutput false
                :virtual false}}))

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

(defn ^{:metadata-spec "5.6"} validate-virtual-columns [context columns]
  ;;virtual property: If present, a virtual column MUST appear after all other non-virtual column definitions.
  (let [non-virtual? (fn [col] (= false (:virtual col)))
        virtual-columns (drop-while non-virtual? columns)
        invalid-columns (filter non-virtual? virtual-columns)]
    (if (seq invalid-columns)
      (let [first-virtual (first virtual-columns)           ;;NOTE: must exist
            msg (format "Non-virtual columns %s defined after first virtual column %s - All virtual columns must exist after all non-virtual columns"
                        (string/join ", " (map :name invalid-columns))
                        (:name first-virtual))]
        (make-error context msg))
      (v/pure columns))))

(def ^{:table-spec "4.3"} index->column-number inc)

(defn ^{:metadata-spec "5.6"} get-column-name [{:keys [name titles] :as column} column-index default-language]
  (or name
      (some-> (first (get titles default-language)) (util/percent-encode))
      (str "_col." (index->column-number column-index))))

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
  (inherited/inherit parent-schema column))

(defn from-titles
  "Creates a new column given the column index and the sequence of titles"
  [column-index titles]
  (let [normalised-titles {"und" titles}
        column-name (get-column-name {:titles normalised-titles} column-index "und")]
    {:name column-name
     :titles normalised-titles
     :virtual false
     :suppressOutput false}))