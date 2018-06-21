(ns csv2rdf.xml.datatype
  "Defines the XML schema type hierarchy and funtions for querying sub/super type relationships."
  (:require [clojure.spec.alpha :as s]
            [csv2rdf.util :as util]
            [csv2rdf.vocabulary :refer :all]
            [clojure.set :as set]))

(def type-hierarchy
  ^{:metadata-spec "5.11.1"
    :doc "Defines the XML schema type hierarchy. Leaf nodes are strings, internal nodes are a pair of
          the type name and the list of immediate subtypes."}
  ["anyAtomicType"
   ["anyURI"
    "base64Binary"
    "boolean"
    "date"
    ["dateTime" ["dateTimeStamp"]]
    ["decimal" [["integer"
                 [["long"
                   [["int"
                     [["short"
                       ["byte"]]]]]]
                  ["nonNegativeInteger"
                   ["positiveInteger"
                    ["unsignedLong"
                     [["unsignedInt"
                       [["unsignedShort"
                         ["unsignedByte"]]]]]]]]
                  ["nonPositiveInteger" ["negativeInteger"]]]]]]
    "double"
    ["duration" ["dayTimeDuration" "yearMonthDuration"]]
    "float"
    "gDay"
    "gMonth"
    "gMonthDay"
    "gYear"
    "gYearMonth"
    "hexBinary"
    "QName"
    ["string"
     [["normalizedString" [["token" ["language" "Name" "NMTOKEN"]]]]
      "xml"
      "html"
      "json"]]
    "time"]])

(def ^{:metadata-spec "5.11.1"} aliases
  {"number" "double"
   "binary" "base64Binary"
   "datetime" "dateTime"
   "any" "anyAtomicType"})

(def ^{:doc "Type names which are not aliases for other types"} primitive-type-names (flatten type-hierarchy))

(def ^{:doc "All known type names"} type-names
  (into #{} (concat primitive-type-names (keys aliases))))

(defn- resolve-type-name
  "Resolves any type aliases to the aliased type name. Returns the type name directly if it is not an alias."
  [type-name]
  (get aliases type-name type-name))

(defn is-leaf? [x] (string? x))
(defn is-inner? [x] (and (vector? x) (= 2 (count x))))
(defn children [node]
  (if (is-leaf? node) nil (second node)))
(defn node-name [x]
  (if (is-leaf? x) x (first x)))

(defn- find-root
  "Finds the subtree with the given type name (or alias) as the root."
  ([type-name] (find-root type-name type-hierarchy))
  ([type-name node]
   (let [type-name (resolve-type-name type-name)]
     (loop [q [node]]
       (if-let [node (first q)]
         (if (= type-name (node-name node))
           node
           (recur (concat (rest q) (children node)))))))))

(defn- make-dispatch-hierarchy
  "Constructs a hierarchy for the XML datatype hierarchy"
  ([] (make-dispatch-hierarchy (make-hierarchy) nil type-hierarchy))
  ([h parent-key node]
   (let [node-key (keyword (node-name node))
         h (if (nil? parent-key)
             h
             (derive h node-key parent-key))]
     (if (is-leaf? node)
       h
       (reduce (fn [h child-node]
                 (make-dispatch-hierarchy h node-key child-node))
               h
               (children node))))))

(def dispatch-hierarchy (make-dispatch-hierarchy))

(defn dispatch-key
  "Returns the key for the given type name in the multimethod dispatch hierarchy."
  [type-name]
  (if-let [resolved-name (resolve-type-name type-name)]
    (keyword resolved-name)
    (throw (IllegalArgumentException. (str "Unknown type: " type-name)))))

(defn- node-subtypes [node]
  (if (is-leaf? node)
    [node]
    (cons (node-name node) (mapcat node-subtypes (children node)))))

(defn- subtypes
  "Returns a sequence of all subtypes for the given type name."
  [type-name]
  (if-let [root (find-root type-name)]
    (node-subtypes root)))

(defn is-subtype?
  "Returns whether type b is a subtype of type a"
  [a b]
  (let [type-name (resolve-type-name b)]
    (boolean (some #(= type-name %) (subtypes a)))))

(defn is-binary-type? [type-name]
  (contains? #{"hexBinary" "base64Binary"} (resolve-type-name type-name)))

(defn is-numeric-type? [type-name]
  (boolean (some #(is-subtype? % type-name) ["decimal" "double" "float"])))

(defn is-date-time-type? [type-name]
  (or (is-subtype? "date" type-name)
      (is-subtype? "dateTime" type-name)
      (is-subtype? "time" type-name)))

(defn is-duration-type? [type-name]
  (is-subtype? "duration" type-name))

(defn is-boolean-type? [type-name]
  (is-subtype? "boolean" type-name))

(defn is-uri-type? [type-name]
  (is-subtype? "anyURI" type-name))

(defn is-string-type? [type-name]
  (is-subtype? "string" type-name))

(s/def ::base  (into #{} (flatten type-hierarchy)))

(s/def ::num-or-string (s/or :string string? :num number?))

(s/def ::decimalChar string?)
(s/def ::groupChar string?)
(s/def ::pattern string?)
(s/def ::numeric-format (s/keys :opt-un [::decimalChar ::groupChar ::pattern]))
(s/def ::format (s/nilable (s/or :string string? :num ::numeric-format)))
(s/def ::length (s/nilable integer?))
(s/def ::minLength (s/nilable integer?))
(s/def ::minimum (s/nilable ::num-or-string))
(s/def ::maximum (s/nilable ::num-or-string))
(s/def ::minInclusive (s/nilable ::num-or-string))
(s/def ::maxInclusive (s/nilable ::num-or-string))

(s/def ::datatype (s/keys :opt-un [::base]))

(def default ^{:metadata-spec "5.11.2"} {:base "string"})

(defn expand
  "Expands a partial datatype definition into one which conforms to the ::datatype spec"
  [dt]
  (merge-with #(or %1 %2) dt default))

(s/fdef expand :ret ::datatype)

(defn ^{:table-spec "4.6.1"} get-length
  "Returns the length of the value of the given datatype."
  [value {:keys [base] :as datatype}]
  (cond
    (nil? value) 0

    (is-string-type? base) (let [^String s value]
                             (.count (.codePoints s)))

    ;;binary data expected to be byte array
    (is-binary-type? base) (let [#^bytes arr value]
                             (alength arr))

    ;;length undefined for type
    :else nil))

(def ^{:doc "Type IRIs which are not mapped directly to an XSD type IRI based on their name"} indirect-mapping-iris
  {"xml" rdf:XMLLiteral
   "html" rdf:HTML
   "json" csvw:JSON})

(defn- type-name->xmls-url
  "Returns the URI for the named datatype"
  [type-name]
  (let [resolved (resolve-type-name type-name)]
    (util/set-fragment xsd resolved)))

(def datatype->iri (merge
                     (into {} (map (fn [type-name] [type-name (type-name->xmls-url type-name)]) type-names))
                     indirect-mapping-iris))

(def get-datatype-iri datatype->iri)

(def iri->datatype-name (into (set/map-invert indirect-mapping-iris)
                              (map (fn [type-name] [(type-name->xmls-url type-name) type-name]) primitive-type-names)))

(defn is-built-in-type-iri?
  "Whether the given IRI identifies a built-in datatype."
  [type-iri]
  (contains? iri->datatype-name type-iri))
