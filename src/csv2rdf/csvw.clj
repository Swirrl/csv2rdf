(ns csv2rdf.csvw
  (:require [grafter.rdf4j.io :as gio]
            [csv2rdf.tabular.csv :as csv]
            [grafter.rdf :as rdf]
            [clojure.java.io :as io]
            [csv2rdf.metadata.dialect :as dialect]
            [csv2rdf.tabular.cell :as cell]
            [csv2rdf.util :as util]))

(defn gen-blank-node
  "Generates a grafter representation of a new blank node"
  []
  (keyword (gensym "bnode")))

(defn column-about-url [table-url column]
  (util/set-fragment table-url (:name column)))

(defn cell-predicate [table-url {:keys [propertyUrl column] :as cell}]
  (or propertyUrl (column-about-url table-url column)))

;;TODO: ensure all cell values have RDF representations in grafter
;;TODO: use datatype annotation on cell?
(defn minimal-cell-statements [table-url default-subject {:keys [aboutUrl valueUrl ordered] :as cell}]
  (let [subject (or aboutUrl default-subject)
        predicate (cell-predicate table-url cell)
        cell-value (cell/value cell)
        is-list? (= true (:list cell-value))
        semantic-value (cell/semantic-value cell)]
    (cond
      (some? valueUrl)
      [(rdf/->Triple subject predicate valueUrl)]

      (and is-list? ordered)
      (throw (ex-info "TODO: rdf:List output" {}))

      is-list?
      (map (fn [v] (rdf/->Triple subject predicate (:value v))) semantic-value)

      (some? semantic-value)
      [(rdf/->Triple subject predicate semantic-value)]

      :else [])))

(defn minimal-row-statements [table-url row]
  (let [default-subject (gen-blank-node)
        unsuppressed-cells (filter (fn [cell]
                                     (= false (get-in cell [:column :suppressOutput])))
                                   (:cells row))]
    (mapcat (fn [cell]
              (minimal-cell-statements table-url default-subject cell))
            unsuppressed-cells)))

(defn minimal-table-statements [{:keys [url dialect] :as table} table-group-dialect]
  ;;TODO: pass IO stream instead of reader since dialect defines encoding
  ;;TODO: get headers if table URL is HTTP scheme
  (with-open [r (io/reader url)]
    (let [dialect (or dialect table-group-dialect (dialect/get-default-dialect {}))
          options (dialect/dialect->options (assoc dialect :doubleQuote false)) ;;TODO: fix reader to allow same escape and quote chars
          annotated-rows (csv/annotated-rows r table options)]
      (mapcat (fn [row] (minimal-row-statements url row)) annotated-rows))))

(defn csv->minimal-rdf->file ^{:csvw-spec "4.2"} [table-group destination]
  (let [output-tables (filter (fn [t] (= false (:suppressOutput t))) (:tables table-group))]
    (with-open [os (io/output-stream destination)]
      (let [writer (gio/rdf-writer os :format :ttl)]
        (doseq [t output-tables]
          (rdf/add writer (minimal-table-statements t (:dialect table-group))))))))

(defn csv->rdf
  ([csv-source metadata-source] (csv->rdf csv-source metadata-source {}))
  ([csv-source metadata-source options]
    {:errors [] :warnings [] :result nil}))
