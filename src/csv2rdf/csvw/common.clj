(ns csv2rdf.csvw.common
  (:require [csv2rdf.util :as util]
            [csv2rdf.tabular.cell :as cell]
            [csv2rdf.vocabulary :refer :all]
            [grafter.rdf :refer [->Triple]]))

(defn gen-blank-node
  "Generates a grafter representation of a new blank node"
  ([] (gen-blank-node "bnode"))
  ([prefix] (keyword (gensym prefix))))

(defn row-unsuppressed-cells
  "Gets the all the cells within a row whose column output is not suppressed"
  [{:keys [cells] :as row}]
  (filter (fn [cell]
            (= false (get-in cell [:column :suppressOutput])))
          cells))

(defn column-about-url [tabular-data-file-url column]
  (util/set-fragment tabular-data-file-url (:name column)))

(defn ^{:csvw-spec "4.6.8.3"} cell-predicate [tabular-data-file-url {:keys [propertyUrl column] :as cell}]
  (or propertyUrl (column-about-url tabular-data-file-url column)))

(defn rdf-list [ordered-values]
  (reduce (fn [[tail-subject tail-statements] value]
            (let [list-subject (gen-blank-node "list")
                  ft (->Triple list-subject rdf:first value)
                  rt (->Triple list-subject rdf:rest tail-subject)]
              [list-subject (cons ft (cons rt tail-statements))]))
          [rdf:nil []]
          (reverse ordered-values)))

;;TODO: ensure all cell values have RDF representations in grafter
;;TODO: use datatype annotation on cell
(defn cell-value-statements [subject predicate {:keys [valueUrl ordered] :as cell}]
  (let [cell-value (cell/value cell)
        is-list? (= true (:list cell))
        semantic-value (cell/semantic-value cell)]
    (cond
      (some? valueUrl)
      [(->Triple subject predicate valueUrl)]

      (and is-list? ordered)
      (let [values (map :value cell-value)
            [list-subject list-triples] (rdf-list values)]
        (cons (->Triple subject predicate list-subject) list-triples))

      is-list?
      (map (fn [v] (->Triple subject predicate (:value v))) semantic-value)

      (some? semantic-value)
      [(->Triple subject predicate semantic-value)]

      :else [])))

(defmulti table-group-context (fn [mode _table-group] mode))
(defmulti table-statements (fn [context table rows] (:mode context)))