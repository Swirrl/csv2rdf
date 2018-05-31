(ns csv2rdf.tabular.csv
  (:require [clojure.string :as string]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.tabular.csv.reader :as reader]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.tabular.cell :as cell]
            [csv2rdf.util :as util]
            [csv2rdf.metadata.column :as column]
            [csv2rdf.metadata.uri-template-property :as template-property]
            [csv2rdf.metadata.table-group :as table-group]
            [csv2rdf.metadata.properties :as properties]
            [csv2rdf.source :as source]
            [csv2rdf.logging :as logging]))

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

(defn rows->embedded-metadata [csv-uri {:keys [skipRows] :as options} rows]
  (let [[skipped-rows remaining-rows] (split-at skipRows rows)
        skipped-row-comments (get-skipped-rows-comments skipped-rows)
        [data-rows {:keys [columns] :as header}] (get-header remaining-rows options)
        {:keys [invalid-row-numbers] :as data-validation} (validate-data-rows data-rows)]
    (if (empty? invalid-row-numbers)
      (let [embedded-comments (vec (concat skipped-row-comments (:comments header) (:comments data-validation)))
            schema {:columns columns}
            table (table/from-schema csv-uri schema)
            table (if (empty? embedded-comments)
                    table
                    (assoc table :comments embedded-comments))]
        (table-group/from-table table))
      (let [msg (format "Rows %s contain an unexpected number of cell values (expected: %d)"
                        (string/join ", " invalid-row-numbers)
                        (:cell-count data-validation))]
        (throw (ex-info msg {:invalid-rows invalid-row-numbers}))))))

;;TODO: section 8.10.4.5.1 - add any extra columns for rows not defined in the input table
(defn ^{:table-spec "8"} extract-embedded-metadata
  ([csv-source] (extract-embedded-metadata csv-source dialect/default-dialect))
  ([csv-source dialect]
   (let [options (dialect/dialect->options dialect)
         rows (reader/read-rows csv-source dialect)]
     (rows->embedded-metadata (source/->uri csv-source) options rows))))

;;TODO: move this
(def index->row-number inc)

(defn title-row-column-indexes [{{:keys [rowTitles columns] :as schema} :tableSchema :as table}]
  (let [title-columns (into #{} rowTitles)]
    (into #{} (remove nil? (map-indexed (fn [idx {:keys [name] :as col}]
                                          (if (contains? title-columns name)
                                            idx))
                                        columns)))))

(defn get-row-titles [title-column-indexes parsed-cells]
  (remove nil? (map-indexed (fn [col-index cell]
                              (if (contains? title-column-indexes col-index)
                                cell))
                            parsed-cells)))

(defn parse-row-cells [{:keys [cells] :as row} table {:keys [skipColumns] :as options}]
  (let [columns (table/columns table)
        cell-values (concat (drop skipColumns cells) (repeat "")) ;;extend cells to cover any virtual columns
        cell-column-pairs (map vector cell-values columns)
        parsed-cells (map-indexed (fn [col-idx [cell column]]
                                    (let [result (cell/parse-cell cell column)
                                          column-number (column/index->column-number col-idx)]
                                      (assoc result
                                        :column-number column-number
                                        :source-column-number (+ skipColumns column-number)
                                        :column column)))
                                  cell-column-pairs)]
    ;;log cell errors
    (doseq [err (mapcat cell/errors parsed-cells)]
      (logging/log-warning err))

    (assoc row :parsed-cells parsed-cells)))

(defn parse-rows [rows table options]
  (map #(parse-row-cells % table options) rows))

(defn get-row-template-bindings [{:keys [number source-row-number parsed-cells]}]
  (let [column-value-bindings (into {} (map (fn [{:keys [column] :as cell}]
                                              ;;TODO: need 'canonical value' according to XML schema
                                              ;;see metadata spec 5.1.3
                                              [(util/percent-decode (properties/column-name column)) (:stringValue cell)])
                                            parsed-cells))]
    (assoc column-value-bindings :_row number :_sourceRow source-row-number)))

(defn get-cell-template-bindings [{:keys [column-number source-column-number column] :as cell}]
  {:_name (util/percent-decode (properties/column-name column))
   :_column column-number
   :_sourceColumn source-column-number})

(defn get-cell-urls [bindings table {:keys [column] :as cell}]
  (let [property-urls {:aboutUrl (some-> (properties/about-url column) (template-property/resolve-uri-template-property bindings table))
                       :propertyUrl (some-> (properties/property-url column) (template-property/resolve-uri-template-property bindings table))
                       :valueUrl (some-> (properties/value-url column) (template-property/resolve-value-uri-template-property cell column bindings table))}]
    (util/filter-values some? property-urls)))

(defn annotate-row [{:keys [number source-row-number parsed-cells] :as data-row} table title-column-indexes]
  (let [row-bindings (get-row-template-bindings data-row)
        cells (map
                (fn [cell]
                  (let [cell-bindings (get-cell-template-bindings cell)
                        bindings (merge row-bindings cell-bindings)
                        property-urls (get-cell-urls bindings table cell)]
                    (merge cell property-urls)))
                parsed-cells)]
    {:number number
     :source-number source-row-number
     :cells         (vec cells)
     :titles (get-row-titles title-column-indexes parsed-cells)}))

(defn skip-to-data-rows [rows {:keys [skipRows num-header-rows] :as options}]
  (let [row-offset (+ skipRows num-header-rows)]
    (drop row-offset rows)))

(defn is-row-blank? [{:keys [cells] :as row}]
  (every? string/blank? cells))

(defn skip-blank-rows [rows {:keys [skipBlankRows] :as options}]
  (if skipBlankRows
    (remove is-row-blank? rows)
    rows))

(defn set-row-numbers [rows]
  (map-indexed (fn [idx row]
                 (assoc row :number (index->row-number idx)))
               rows))

(defn annotate-rows [rows table options]
  (let [title-column-indexes (title-row-column-indexes table)
        data-rows (-> rows
                      (skip-to-data-rows options)
                      (skip-blank-rows options)
                      (set-row-numbers)
                      (parse-rows table options))]
    (map (fn [row]
           (annotate-row row table title-column-indexes))
         data-rows)))

(defn ^{:table-spec "8"} annotated-rows [source table dialect]
  (let [options (dialect/dialect->options dialect)
        rows (reader/read-rows source dialect)]
    (annotate-rows rows table options)))
