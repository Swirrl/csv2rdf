(ns csv2rdf.metadata.datatype
  (:require [clojure.spec.alpha :as s]))

(def type-hierarchy
  ["anyAtomicType"
   ["anyURI"
    "base64Binary"
    "boolean"
    ["date" ["dateTime"]]
    ["decimal" [["integer"
                 [["long"
                   [["int"
                     [["short"
                       ["byte"]]]]]]]]
                ["nonNegativeInteger"
                 ["positiveInteger"
                  ["unsignedLong"
                   [["unsignedInt"
                     [["unsignedShort"
                       ["unsignedByte"]]]]]]]]
                ["nonPositiveInteger" ["negativeInteger"]]]]
    "double"
    ["duration" ["dayTimeDuration" "yearMonthDuration"]]
    "float"
    "gDay"
    "gYear"
    "gMonth"
    "hexBinary"
    "QName"
    ["string"
     [["normalizedString" [["token" ["language" "Name" "NMTOKEN"]]]]
      "xml"
      "html"
      "json"]]
    "time"]])

(defn is-leaf? [x] (string? x))
(defn is-inner? [x] (and (vector? x) (= 2 (count x))))
(defn children [node]
  (if (is-leaf? node) nil (second node)))
(defn node-name [x]
  (if (is-leaf? x) x (first x)))

(defn find-root
  ([type-name] (find-root type-name type-hierarchy))
  ([type-name node]
   (loop [q [node]]
     (if-let [node (first q)]
       (if (= type-name (node-name node))
         node
         (recur (concat (rest q) (children node))))))))

(defn node-subtypes [node]
  (if (is-leaf? node)
    [node]
    (cons (node-name node) (mapcat node-subtypes (children node)))))

(defn subtypes [type-name]
  (if-let [root (find-root type-name)]
    (node-subtypes root)))

(defn is-subtype? [supertype-name type-name]
  (boolean (some #(= type-name %) (subtypes supertype-name))))

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

(defn is-binary-type? [datatype]
  ;;TODO: figure out what this means
  )

(defn binary-length [value datatype]
  (throw (IllegalStateException. "Should not be called until is-binary-type? is implemented")))

(defn ^{:table-spec "4.6.1"} get-length [value {:keys [base] :as datatype}]
  (cond
    (nil? value) 0
    (is-subtype? "string" base) (.count (.codePoints value))
    (is-binary-type? datatype) (binary-length value datatype)
    :else (throw (IllegalStateException. "TODO: return 0?"))
    ))
