(ns csv2rdf.tabular.csv.reader
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string])
  (:import [com.opencsv CSVReader]))

(s/def ::source-row-number (s/and integer? pos?))
(s/def ::cells (s/coll-of string? :kind vector? :into []))
(s/def ::content string?)
(s/def ::comment string?)
(s/def ::type #{:comment :data})

(defn trim-cell [value trim-mode]
  (case trim-mode
    :all (string/trim value)
    :start (string/triml value)
    :end (string/trimr value)
    value))

(defn is-comment-row? [{:keys [type]}]
  (= :comment type))

(defn make-row [cells source-row-num {:keys [delimiter comment-prefix trim-mode] :as options}]
  (let [content (string/join delimiter cells)
        is-comment? (and (some? comment-prefix)
                         (.startsWith content (str comment-prefix)))]
    {:source-row-number source-row-num
     :content content
     :comment (if is-comment?
                (.substring content 1))
     :cells (if-not is-comment?
              (mapv #(trim-cell % trim-mode) cells))
     :type (if is-comment? :comment :data)}))

(defn read-rows
  "Returns a lazy sequence of CSV rows from the underlying reader. The row records contain
   the source row number (reader is initially assumed to be positioned on row 1), the parsed
   content and cells along with any comment. Rows are classified as comments or data rows containing
   cell data. Cell data values are trimmed according to the trim-mode specified by the options."
  [reader {:keys [quote-char escape-char delimiter] :as options}]
  ;;TODO: can quote, escape or delimiters be nil?
  (println options)
  (let [csv-reader (CSVReader. reader delimiter quote-char escape-char)
        rows (iterator-seq (.iterator csv-reader))]
    (map-indexed (fn [idx row]
                   (make-row row (inc idx) options))
                 rows)))
