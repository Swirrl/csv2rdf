(ns csv2rdf.csvw.standard
  (:require [csv2rdf.csvw.common :refer :all]
            [csv2rdf.vocabulary :refer :all]
            [csv2rdf.json-ld :as json-ld]
            [csv2rdf.json :refer [array? object?]]
            [grafter.rdf :refer [->Triple]]
            [grafter.rdf.io :refer [language literal]]
            [csv2rdf.util :refer [liberal-mapcat] :as util])
  (:import [java.net URI]))

(defn ^{:csvw-spec "6.1"} try-expand-property [property]
  (cond
    (instance? URI property) property
    (string? property) (let [expanded (json-ld/expand-uri-string property)
                             ^URI expanded-uri (util/ignore-exceptions (URI. expanded))]
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
            ts_4_3 (mapcat (fn [[k v]]
                             ;;TODO: keep common properties as strings when parsing metadata?
                             (cond
                               (instance? URI k) (json-ld->rdf s-node k v)
                               (and (string? k) (let [^String s k] (not (.startsWith s "@")))) (json-ld->rdf s-node k v)
                               :else nil))
                           value)]
        (cons t_4_1 (concat ts_4_2 ts_4_3)))

      :else [(->Triple subject property value)])))

(defn notes-non-core-annotation-statements [subject {:keys [notes] :as element}]
  (let [common-properties (:csv2rdf.metadata.types/common-properties element)
        note-pairs (map (fn [n] [csvw:note n]) notes)]
    (mapcat (fn [[k v]]
              (json-ld->rdf subject k v))
            (concat note-pairs common-properties))))

(defn row-title-object [{:keys [list value stringValue] :as cell-value}]
  (if list stringValue value))

(defn ^{:csvw-spec "4.6.6"
        :metadata-spec "5.5"} row-title-statements
  "Gets the statements for a rows titles annotation. The titles annotation should contain a list of cell values
   for the columns referenced in the schema "
  [row-subject {:keys [titles] :as row}]
  (map (fn [cell-value] (->Triple row-subject csvw:title (row-title-object cell-value))) titles))

(defn cell-statements [table-url row-subject default-cell-subject {:keys [aboutUrl] :as cell}]
  (let [cell-subject (or aboutUrl default-cell-subject)
        t-4_6_8_2 (->Triple row-subject csvw:describes cell-subject)
        predicate (cell-predicate table-url cell)]
    (cons t-4_6_8_2 (cell-value-statements cell-subject predicate cell))))

(defn row-statements [table-subject {table-url :url :as table} {:keys [number source-number] :as row}]
  (let [row-subject (gen-blank-node "row")
        default-cell-subject (gen-blank-node)
        t-4_6_2 (->Triple table-subject csvw:row row-subject)
        t-4_6_3 (->Triple row-subject rdf:type csvw:Row)
        t-4_6_4 (->Triple row-subject csvw:rownum (literal (str number) xsd:integer))
        t-4_6_5 (->Triple row-subject csvw:url (util/set-fragment table-url (str "row=" source-number)))]
    (concat [t-4_6_2 t-4_6_3 t-4_6_4 t-4_6_5]
            (notes-non-core-annotation-statements row-subject row)
            (row-title-statements row-subject row)
            (mapcat (fn [cell] (cell-statements table-url row-subject default-cell-subject cell)) (row-unsuppressed-cells row)))))

(defmethod table-group-context :standard [mode {:keys [id] :as table-group}]
  (let [tg-subject (or id (gen-blank-node "tablegroup"))
        t (->Triple tg-subject rdf:type csvw:TableGroup)]
    {:mode mode
     :table-group-subject tg-subject
     :statements (cons t (notes-non-core-annotation-statements tg-subject table-group))}))

(defmethod table-statements :standard [{:keys [table-group-subject] :as context} {:keys [id url] :as table} annotated-rows]
  (let [table-subject (or id (gen-blank-node "table"))
        t-4_2 (->Triple table-group-subject csvw:table table-subject)
        t-4_3 (->Triple table-subject rdf:type csvw:Table)
        t-4_4 (->Triple table-subject csvw:url url)]
    (concat
      [t-4_2 t-4_3 t-4_4]
      (notes-non-core-annotation-statements table-subject table)
      (liberal-mapcat (fn [row]
                        (row-statements table-subject table row))
                      annotated-rows))))