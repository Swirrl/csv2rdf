(ns csv2rdf.tabular.csv
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.tabular.csv.reader :as reader]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.tabular.cell :as cell]
            [csv2rdf.util :as util]
            [csv2rdf.metadata.column :as column]
            [csv2rdf.metadata.uri-template-property :as template-property]
            [csv2rdf.metadata.table-group :as table-group]
            [csv2rdf.source :as source]))

(defn ^{:table-spec "8.6"} get-skipped-rows-comments [skipped-rows]
  (remove nil? (map (fn [{:keys [type comment content] :as row}]
                      (cond (= :comment type) comment
                            (not (string/blank? content)) content
                            :else nil))
                    skipped-rows)))

(defn ^{:table-spec "8.7"} get-header-row-columns [header-rows]
  {:pre [(some? (seq header-rows))]}
  (let [comment-rows (filter reader/is-comment-row? header-rows)
        title-rows (remove reader/is-comment-row? header-rows)
        titles (apply map vector (map :cells title-rows))
        columns (map-indexed (fn [idx titles]
                               (column/from-titles idx titles))
                             titles)]
    {:comments (mapv :comment comment-rows)
     :columns columns}))

(defn ^{:table-spec "8.10.3"} validate-data-rows
  "Validates the data rows in the tabular file and extracts any embedded comments. The row number of any rows
  containing an unexpected number of cells (i.e. different from the first data row) are returned under the
  :invalid-row-numbers key."
  [data-rows]
  (reduce (fn [{:keys [cell-count] :as acc} {:keys [comment cells source-row-number] :as row}]
            (if (reader/is-comment-row? row)
              (update acc :comments conj comment)
              (let [row-cell-count (count cells)]
                (cond
                  (nil? cell-count) (assoc acc :cell-count row-cell-count)
                  (= cell-count row-cell-count) acc
                  :else (update acc :invalid-row-numbers conj source-row-number)))))
          {:comments [] :cell-count nil :invalid-row-numbers []}
          data-rows))

(defn ^{:table-spec ["8.7" "8.8"]} get-header
  "Gets the header given a sequence of header/data rows, and a returns a pair of
  [data-rows, header]. The header is a map containing the columns definitions
  and any comments found in the header."
  [rows {:keys [num-header-rows skipColumns] :as options}]
  (if (zero? num-header-rows)
    (if-let [first-row (first rows)]
      (let [cells (:cells first-row)
            num-columns (max 0 (- (count cells) skipColumns))]
        [rows {:columns (mapv column/from-index (range num-columns))}])
      [rows {:columns []}])
    (let [[header-rows data-rows] (split-at num-header-rows rows)]
      [data-rows (get-header-row-columns header-rows)])))

;;TODO: section 8.10.4.5.1 - add any extra columns for rows not defined in the input table
(defn ^{:table-spec "8"} extract-embedded-metadata
  ([csv-source] (extract-embedded-metadata csv-source {:doubleQuote false}))
  ([csv-source {:keys [encoding] :as dialect}]
   (with-open [r (io/reader csv-source :encoding encoding)]
     (let [{:keys [skipRows] :as options} (dialect/dialect->options dialect)
           rows (reader/read-rows r options)
           [skipped-rows remaining-rows] (split-at skipRows rows)
           skipped-row-comments (get-skipped-rows-comments skipped-rows)
           [data-rows {:keys [columns] :as header}] (get-header remaining-rows options)
           {:keys [invalid-row-numbers] :as data-validation} (validate-data-rows data-rows)]
       (if (empty? invalid-row-numbers)
         (let [embedded-comments (vec (concat skipped-row-comments (:comments header) (:comments data-validation)))
               schema {:columns columns}
               table (table/from-schema (source/->uri csv-source) schema)
               table (if (empty? embedded-comments)
                       table
                       (assoc table :comments embedded-comments))]
           (table-group/from-table table))
         (let [msg (format "Rows %s contain an unexpected number of cell values (expected: %d)"
                           (string/join ", " invalid-row-numbers)
                           (:cell-count data-validation))]
           (throw (ex-info msg {:invalid-rows invalid-row-numbers}))))))))

;;TODO: move this
(def index->row-number inc)

(defn title-row-column-indexes [{{:keys [rowTitles columns] :as schema} :tableSchema :as table}]
  (let [title-columns (into #{} rowTitles)]
    (into #{} (remove nil? (map-indexed (fn [idx {:keys [name] :as col}]
                                          (if (contains? title-columns name)
                                            idx))
                                        columns)))))

(defn annotate-row [row-index {:keys [source-row-number cells] :as data-row} table title-column-indexes {:keys [skipColumns] :as csv-options}]
  (let [columns (table/columns table)
        row-number (index->row-number row-index)
        cell-values (drop skipColumns cells)

        ;;extend cells to cover any virtual columns
        ;;TODO: handle virtual and non-virtual columns separately?
        padded-cell-values (concat cell-values (repeat ""))
        parsed-cells (map cell/parse-cell padded-cell-values columns)
        row-titles (remove nil? (map-indexed (fn [col-index cell]
                                              (if (contains? title-column-indexes col-index)
                                                cell))
                                            parsed-cells))
        column-value-bindings (into {} (map (fn [cell column]
                                              ;;TODO: need 'canonical value' according to XML schema
                                              ;;see metadata spec 5.1.3
                                              [(util/percent-decode (:name column)) (:stringValue cell)])
                                            parsed-cells columns))
        row-bindings (assoc column-value-bindings :_row row-number :_sourceRow source-row-number)
        cells (map-indexed
                (fn [col-index [cell column]]
                  (let [column-number (column/index->column-number col-index)
                        bindings (assoc row-bindings :_name (util/percent-decode (:name column))
                                                     :_column column-number
                                                     :_sourceColumn (+ skipColumns column-number))
                        property-urls {:aboutUrl (some-> (:aboutUrl column) (template-property/resolve-uri-template-property bindings table))
                                       :propertyUrl (some-> (:propertyUrl column) (template-property/resolve-uri-template-property bindings table))
                                       :valueUrl (some-> (:valueUrl column) (template-property/resolve-value-uri-template-property cell column bindings table))}]
                    (-> cell
                        (merge (util/filter-values some? property-urls))
                        (assoc :column column))))
                (map vector parsed-cells columns))]
    {:number        row-number
     :source-number source-row-number
     :cells         (vec cells)
     :titles row-titles}))

(defn ^{:table-spec "8"} annotated-rows [reader table {:keys [skipRows num-header-rows skipBlankRows] :as csv-options}]
  (let [title-column-indexes (title-row-column-indexes table)
        should-skip-row? (fn [{:keys [cells] :as row}] (and skipBlankRows (every? string/blank? cells)))
        rows (reader/read-rows reader csv-options)
        row-offset (+ skipRows num-header-rows)
        data-rows (remove should-skip-row? (drop row-offset rows))]
    (map-indexed (fn [row-index row]
                   (annotate-row row-index row table title-column-indexes csv-options))
         data-rows)))
