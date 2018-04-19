(ns csv2rdf.csvw.common
  (:require [csv2rdf.util :as util]
            [csv2rdf.tabular.cell :as cell]
            [csv2rdf.vocabulary :refer :all]
            [grafter.rdf :refer [->Triple] :as rdf]
            [csv2rdf.xml.datatype :as xml-datatype]))

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

(def stringy-types #{"base64Binary" "hexBinary"})

(defn cell-element->rdf [element]
  (let [element-base (get-in element [:datatype :base])
        resolved-type-name (xml-datatype/resolve-type-name element-base)]
    (if (contains? stringy-types resolved-type-name)
      (rdf/literal (:stringValue element) (xml-datatype/get-datatype-iri element-base))
      (:value element))))

(defn rdf-list [ordered-value-elements]
  (reduce (fn [[tail-subject tail-statements] value-element]
            (let [list-subject (gen-blank-node "list")
                  ft (->Triple list-subject rdf:first (cell-element->rdf value-element))
                  rt (->Triple list-subject rdf:rest tail-subject)]
              [list-subject (cons ft (cons rt tail-statements))]))
          [rdf:nil []]
          (reverse ordered-value-elements)))

;;TODO: ensure all cell values have RDF representations in grafter
;;TODO: use datatype annotation on cell
(defn cell-value-statements [subject predicate {:keys [value valueUrl ordered] :as cell}]
  (let [is-list? (= true (:list cell))
        semantic-value (cell/semantic-value cell)]
    (cond
      (some? valueUrl)
      [(->Triple subject predicate valueUrl)]

      (and is-list? ordered)
      (let [[list-subject list-triples] (rdf-list value)]
        (cons (->Triple subject predicate list-subject) list-triples))

      is-list?
      (map (fn [ve] (->Triple subject predicate (cell-element->rdf ve))) value)

      (some? semantic-value)
      [(->Triple subject predicate (cell-element->rdf cell))]

      :else [])))

(defmulti table-group-context (fn [mode _table-group] mode))
(defmulti table-statements (fn [context table rows] (:mode context)))