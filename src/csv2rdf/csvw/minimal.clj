(ns csv2rdf.csvw.minimal
  (:require [grafter.rdf :as rdf]
            [csv2rdf.tabular.cell :as cell]
            [csv2rdf.util :as util]
            [csv2rdf.vocabulary :refer [rdf:nil rdf:first rdf:rest]]))

(defn gen-blank-node
  "Generates a grafter representation of a new blank node"
  ([] (gen-blank-node "bnode"))
  ([prefix] (keyword (gensym prefix))))

(defn column-about-url [table-url column]
  (util/set-fragment table-url (:name column)))

(defn cell-predicate [table-url {:keys [propertyUrl column] :as cell}]
  (or propertyUrl (column-about-url table-url column)))

(defn rdf-list [ordered-values]
  (reduce (fn [[tail-subject tail-statements] value]
            (let [list-subject (gen-blank-node "list")
                  ft (rdf/->Triple list-subject rdf:first value)
                  rt (rdf/->Triple list-subject rdf:rest tail-subject)]
              [list-subject (cons ft (cons rt tail-statements))]))
          [rdf:nil []]
          (reverse ordered-values)))

;;TODO: ensure all cell values have RDF representations in grafter
;;TODO: use datatype annotation on cell?
(defn minimal-cell-statements [table-url default-subject {:keys [aboutUrl valueUrl ordered] :as cell}]
  (let [subject (or aboutUrl default-subject)
        predicate (cell-predicate table-url cell)
        cell-value (cell/value cell)
        is-list? (= true (:list cell))
        semantic-value (cell/semantic-value cell)]
    (cond
      (some? valueUrl)
      [(rdf/->Triple subject predicate valueUrl)]

      (and is-list? ordered)
      (let [values (map :value cell-value)
            [list-subject list-triples] (rdf-list values)]
        (cons (rdf/->Triple subject predicate list-subject) list-triples))

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

(defn minimal-table-statements [{:keys [url] :as table} annotated-rows]
  (mapcat (fn [row] (minimal-row-statements url row)) annotated-rows))
