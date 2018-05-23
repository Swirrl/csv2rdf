(ns csv2rdf.metadata.transformation
  (:require [csv2rdf.metadata.types :refer [object-of link-property natural-language id]]
            [csv2rdf.metadata.validator :refer [variant one-of type-eq]]))

(def transformation-source-types #{"json" "rdf"})

(def ^{:metadata-spec "5.10.2"} transformation-source
  (variant {:string (one-of transformation-source-types)}))

;;NOTE: transformations may contain common properties but not inherited properties?
(def transformation
  (object-of
    {:required {:url link-property
                :scriptFormat link-property
                :targetFormat link-property}
     :optional {:source transformation-source
                :titles natural-language
                :id id
                :type (type-eq "Template")}
     :allow-common-properties? true}))

