(ns csv2rdf.csvw.standard
  (:require [csv2rdf.csvw.common :refer :all]
            [csv2rdf.vocabulary :refer :all]
            [grafter.rdf :refer [->Triple]]
            [csv2rdf.util :as util])
  (:import (java.net URI)))

(defn notes-non-core-annotation-statements [subject {:keys [notes]}]
  ;;TODO: implement!
  [])

(defn row-title-statements [row]
  ;;TODO: implement!
  [])

(def tabular-data-file-url (URI. "file:/data/test001.csv"))

(defn cell-statements [row-subject {:keys [aboutUrl] :as cell}]
  (let [default-subject (gen-blank-node)
        cell-subject (or aboutUrl default-subject)
        t-4_6_8_2 (->Triple row-subject csvw:describes cell-subject)
        predicate (cell-predicate tabular-data-file-url cell)]
    (cons t-4_6_8_2 (cell-value-statements cell-subject predicate cell))))

(defn row-statements [table-subject {table-url :url :as table} {:keys [number source-number] :as row}]
  (let [row-subject (gen-blank-node "row")
        t-4_6_2 (->Triple table-subject csvw:row row-subject)
        t-4_6_3 (->Triple row-subject rdf:type csvw:Row)
        t-4_6_4 (->Triple row-subject csvw:rownum (int number))
        t-4_6_5 (->Triple row-subject csvw:url (util/set-fragment table-url (str "row=" source-number)))]
    (concat [t-4_6_2 t-4_6_3 t-4_6_4 t-4_6_5]
            (notes-non-core-annotation-statements row-subject row)
            (row-title-statements row)
            (mapcat (fn [cell] (cell-statements row-subject cell)) (row-unsuppressed-cells row)))))

(defmethod table-statements :standard [{:keys [table-group-subject] :as context} {:keys [id url] :as table} annotated-rows]
  (let [table-subject (or id (gen-blank-node "table"))
        t-4_2 (->Triple table-group-subject csvw:table table-subject)
        t-4_3 (->Triple table-subject rdf:type csvw:Table)
        t-4_4 (->Triple table-subject csvw:url url)]
    (concat [t-4_2 t-4_3 t-4_4]
            (notes-non-core-annotation-statements table-subject table)
            (mapcat (fn [row] (row-statements table-subject table row)) annotated-rows))))

(defmethod table-group-context :standard [mode {:keys [id] :as table-group}]
  (let [tg-subject (or id (gen-blank-node "tablegroup"))
        t (->Triple tg-subject rdf:type csvw:TableGroup)]
    {:mode mode
     :table-group-subject tg-subject
     :statements (cons t (notes-non-core-annotation-statements tg-subject table-group))}))