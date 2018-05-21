(ns csv2rdf.csvw.minimal
  (:require [csv2rdf.vocabulary :refer [rdf:nil rdf:first rdf:rest]]
            [csv2rdf.csvw.common :refer :all]
            [grafter.rdf :as rdf]))

(defn minimal-cell-statements [table-url default-subject {:keys [aboutUrl] :as cell}]
  (let [subject (or aboutUrl default-subject)
        predicate (cell-predicate table-url cell)]
    (cell-value-statements subject predicate cell)))

(defn minimal-row-statements [table-url row]
  (let [default-subject (gen-blank-node)
        unsuppressed-cells (row-unsuppressed-cells row)]
    (mapcat (fn [cell]
              (minimal-cell-statements table-url default-subject cell))
            unsuppressed-cells)))

(defmethod table-group-context :minimal [mode _table-group]
  {:mode mode
   :statements []})

(defmethod write-table-statements :minimal [_context destination {:keys [url] :as table} annotated-rows]
  (doseq [row annotated-rows]
    (log-cell-errors row)
    (rdf/add destination (minimal-row-statements url row))))
