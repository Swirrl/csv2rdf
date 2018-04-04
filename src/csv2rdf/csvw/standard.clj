(ns csv2rdf.csvw.standard
  (:require [csv2rdf.csvw.common :refer :all]
            [csv2rdf.vocabulary :refer :all]
            [csv2rdf.json-ld :as json-ld]
            [grafter.rdf :refer [->Triple]]
            [grafter.rdf.protocols :refer [language literal]]
            [csv2rdf.util :as util])
  (:import [java.net URI]))

;;TODO: move csv2rdf.metadata.json namespace and use definitions in there
(def array? vector?)
(def object? map?)

(defn ^{:csvw-spec "6.1"} try-expand-property [property]
  (cond
    (instance? URI property) property
    (string? property) (let [expanded (json-ld/expand-uri-string property)
                             expanded-uri (util/ignore-exceptions (URI. expanded))]
                         (if (and (some? expanded-uri) (.isAbsolute expanded-uri))
                           expanded-uri))
    :else nil))

(defn normalise-types
  "Extracts and expands all types within a common property value. Returns a sequence of all contained
   type URIs."
  [t]
  (cond (nil? t) []
        (array? t) (mapcat normalise-types t)
        :else (if-let [t (try-expand-property t)]
                [t]
                [])))

(defn ^{:csvw-spec "6"} json-ld->rdf [subject property value]
  (if-let [property (try-expand-property property)]
    (cond
      (nil? value) []
      (array? value)
      (mapcat (fn [v] (json-ld->rdf subject property v)) value)

      (and (object? value) (contains? value "@value"))
      (let [inner-value (get value "@value")]
        (cond
          (contains? value "@language")
          [(->Triple subject property (language inner-value (keyword (get value "@language"))))]

          (contains? value "@type")
          (let [type (get value "@type")]
            (map (fn [type-uri] (->Triple subject property (literal inner-value type-uri))) (normalise-types type)))

          :else [(->Triple subject property (literal inner-value xsd:string))]))

      (object? value)
      (let [s-node (if (contains? value "@id") (get value "@id") (gen-blank-node))
            type-uris (normalise-types (get value "@type"))
            t_4_1 (->Triple subject property s-node)
            ts_4_2 (map (fn [type-uri] (->Triple s-node rdf:type type-uri)) type-uris)
            ts_4_3 (mapcat (fn [[^String k v]]
                             (if-not (.startsWith k "@")
                               (json-ld->rdf s-node k v)))
                           value)]
        (cons t_4_1 (concat ts_4_2 ts_4_3)))

      :else [(->Triple subject property value)])))

(defn notes-non-core-annotation-statements [subject {:keys [notes] :as element}]
  (let [common-properties (:csv2rdf.metadata.types/common-properties element)]
    (mapcat (fn [[k v]]
              (json-ld->rdf subject k v))
            (concat notes common-properties))))

(defn row-title-statements [row]
  ;;TODO: implement!
  [])

;;TODO: remove
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