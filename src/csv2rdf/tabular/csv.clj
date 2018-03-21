(ns csv2rdf.tabular.csv
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.tabular.csv.reader :as reader]))

(defn ^{:table-spec "8.6"} get-skipped-rows-comments [skipped-rows]
  (remove nil? (map (fn [{:keys [type comment content] :as row}]
                      (cond (= :comment type) comment
                            (not (.isEmpty content)) content
                            :else nil))
                    skipped-rows)))

(defn ^{:table-spec "8.7"} get-header-row-columns [header-rows]
  (let [comment-rows (filter reader/is-comment-row? header-rows)
        title-rows (remove reader/is-comment-row? header-rows)
        columns (apply mapv (fn [& titles]
                            {:titles (vec (remove string/blank? titles))})
                      (map :cells title-rows))]
    {:comments (mapv :comment comment-rows)
     :columns columns}))

(defn ^{:table-spec "8.10.3"} get-data-comments [data-rows]
  (map :comment (filter reader/is-comment-row? data-rows)))

(defn ^{:table-spec "8"} extract-embedded-metadata
  ([csv-source] (extract-embedded-metadata csv-source {:doubleQuote false}))
  ([csv-source {:keys [encoding] :as dialect}]
   (with-open [r (io/reader csv-source :encoding encoding)]
     (let [{:keys [skipRows num-header-rows] :as options} (dialect/dialect->options dialect)
           rows (reader/read-rows r options)
           [skipped-rows remaining-rows] (split-at skipRows rows)
           skipped-row-comments (get-skipped-rows-comments skipped-rows)
           [header-rows data-rows] (split-at num-header-rows remaining-rows)
           {:keys [columns] :as header} (get-header-row-columns header-rows)
           data-row-comments (get-data-comments data-rows)
           comments (vec (concat skipped-row-comments (:comments header) data-row-comments))
           metadata {:tableSchema {:columns columns}}
           metadata (if (empty? comments)
                      metadata
                      (assoc metadata :comments comments))]
       metadata))))

