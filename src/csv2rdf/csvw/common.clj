(ns csv2rdf.csvw.common
  (:require [csv2rdf.tabular.cell :as cell]
            [csv2rdf.vocabulary :refer :all]
            [grafter-2.rdf.protocols :refer [->Triple] :as gproto]
            [csv2rdf.xml.datatype :as xml-datatype]
            [csv2rdf.xml.datatype.canonical :as xml-canonical]
            [csv2rdf.metadata.properties :as properties])
  (:import [java.net URI]
           [org.eclipse.rdf4j.model.impl URIImpl]))

(def bnode-id-counter (atom 0))

(defn gen-blank-node
  "Generates a grafter representation of a new blank node"
  ([] (gen-blank-node "bnode"))
  ([prefix] (gproto/make-blank-node (str prefix "__" (swap! bnode-id-counter inc)))))

(defn row-unsuppressed-cells
  "Gets the all the cells within a row whose column output is not suppressed"
  [{:keys [cells] :as row}]
  (remove (fn [{:keys [column] :as cell}]
            (properties/suppress-output? column))
          cells))

(defn set-encoded-fragment
  "Returns a sesame URI containing for the given URI whose fragment is replaced with the encoded string. The java URI
   constructor encodes only % character within fragements, so cannot be used to encode an unencoded fragment. When
   given an encoded fragment, it encodes the % character which leads to a double-encoding. The sesame URIImpl does
   not process the constructed string so can be used when output."
  [^URI uri encoded-fragment]
  (URIImpl. (str (.getScheme uri) ":" (.getRawSchemeSpecificPart uri) "#" encoded-fragment)))

(defn column-about-url [tabular-data-file-url column]
  (set-encoded-fragment tabular-data-file-url (properties/column-name column)))

(defn ^{:csvw-spec "4.6.8.3"} cell-predicate [tabular-data-file-url {:keys [propertyUrl column] :as cell}]
  (or propertyUrl (column-about-url tabular-data-file-url column)))

(defn cell-element->rdf [{:keys [value stringValue datatype] :as cell-element} lang]
  (let [{:keys [id base]} datatype]
    (cond
      ;;if the datatype's id annotation is not null, then its value MUST be used as the RDF datatype IRI;
      (some? id)
      (gproto/literal (xml-canonical/canonical-value value base) id)

      ;; if a cell has any datatype other than string, the value of lang MUST be ignored
      (and (= "string" base) (some? lang))
      (gproto/language value (keyword lang))

      :else
      (let [;;NOTE: XML Duration type normalises the parsed value when outputting as strings, which the test cases do not
            ;;expect e.g. an input string of P20M is formatted as P1Y8M
            rdf-value (if (xml-datatype/is-duration-type? base)
                        stringValue
                        (xml-canonical/canonical-value value base))]
        (gproto/literal rdf-value (xml-datatype/datatype->iri base))))))

(defn rdf-list [ordered-value-elements lang]
  (reduce (fn [[tail-subject tail-statements] value-element]
            (let [list-subject (gen-blank-node "list")
                  ft (->Triple list-subject rdf:first (cell-element->rdf value-element lang))
                  rt (->Triple list-subject rdf:rest tail-subject)]
              [list-subject (cons ft (cons rt tail-statements))]))
          [rdf:nil []]
          (reverse ordered-value-elements)))

(defn cell-value-statements [subject predicate {:keys [value valueUrl ordered lang] :as cell}]
  (let [is-list? (= true (:list cell))
        semantic-value (cell/semantic-value cell)]
    (cond
      (some? valueUrl)
      [(->Triple subject predicate valueUrl)]

      (and is-list? ordered)
      (let [[list-subject list-triples] (rdf-list value lang)]
        (cons (->Triple subject predicate list-subject) list-triples))

      is-list?
      (map (fn [ve] (->Triple subject predicate (cell-element->rdf ve lang))) value)

      (some? semantic-value)
      [(->Triple subject predicate (cell-element->rdf cell lang))]

      :else [])))

(defmulti table-group-context (fn [mode _table-group] mode))
(defmulti table-statements (fn [context table rows] (:mode context)))
