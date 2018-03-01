(ns csv2rdf.metadata.datatype
  (:require [clojure.spec.alpha :as s]))

(s/def ::base #{"anyAtomicType" "anyURI" "base64Binary" "boolean" "date" "dateTime" "dateTimeStamp"
                "decimal" "integer" "long" "int" "short" "byte" "nonNegativeInteger" "positiveInteger"
                "unsignedLong" "unsignedInt" "unsignedShort" "unsignedByte" "nonPositiveInteger"
                "negativeInteger" "double" "duration" "dayTimeDuration" "yearMonthDuration" "float"
                "gDay" "gMonth" "gMonthDay" "gYear" "gYearMonth" "hexBinary" "QName" "string"
                "normalizedString" "token" "language" "Name" "NMTOKEN" "xml" "html" "json" "time"})

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
