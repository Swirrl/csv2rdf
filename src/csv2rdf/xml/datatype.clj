(ns csv2rdf.xml.datatype
  (:require [clojure.spec.alpha :as s]
            [csv2rdf.util :as util]))

(def type-hierarchy
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

(def ^{:metadata-spec "5.11.1"} aliases
  {"number" "double"
   "binary" "base64Binary"
   "datetime" "dateTime"
   "any" "anyAtomicType"})

(def ^{:doc "All known type names"} type-names
  (into #{} (concat (flatten type-hierarchy) (keys aliases))))

(defn valid-type-name? [s]
  (contains? type-names s))

(defn resolve-type-name [type-name]
  (get aliases type-name type-name))

(defn is-leaf? [x] (string? x))
(defn is-inner? [x] (and (vector? x) (= 2 (count x))))
(defn children [node]
  (if (is-leaf? node) nil (second node)))
(defn node-name [x]
  (if (is-leaf? x) x (first x)))

(defn find-root
  ([type-name] (find-root type-name type-hierarchy))
  ([type-name node]
   (let [type-name (resolve-type-name type-name)]
     (loop [q [node]]
       (if-let [node (first q)]
         (if (= type-name (node-name node))
           node
           (recur (concat (rest q) (children node)))))))))

(defn node-subtypes [node]
  (if (is-leaf? node)
    [node]
    (cons (node-name node) (mapcat node-subtypes (children node)))))

(defn subtypes [type-name]
  (if-let [root (find-root type-name)]
    (node-subtypes root)))

(defn is-subtype? [supertype-name type-name]
  (let [type-name (resolve-type-name type-name)]
    (boolean (some #(= type-name %) (subtypes supertype-name)))))

(defn is-binary-type? [type-name]
  (contains? #{"hexBinary" "base64Binary"} (resolve-type-name type-name)))

(defn is-numeric-type? [type-name]
  (some #(is-subtype? % type-name) ["decimal" "double" "float"]))

(defn is-integral-type? [type-name]
  (is-subtype? "integer" type-name))

(defn is-date-time-type? [type-name]
  (or (is-subtype? "date" type-name)
      (is-subtype? "dateTime" type-name)
      (is-subtype? "time" type-name)))

(defn is-duration-type? [type-name]
  (is-subtype? "duration" type-name))

(defn is-boolean-type? [type-name]
  (is-subtype? "boolean" type-name))

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

(defn ^{:table-spec "4.6.1"} get-length [value {:keys [base] :as datatype}]
  (cond
    (nil? value) 0
    (is-subtype? "string" base) (let [^String str-val (util/->string value)]
                                  (.count (.codePoints str-val)))

    ;;binary data expected to be byte array
    (is-binary-type? base) (let [#^bytes arr value]
                             (alength arr))

    ;;length undefined for type
    :else nil))
