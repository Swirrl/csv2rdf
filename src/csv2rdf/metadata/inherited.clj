(ns csv2rdf.metadata.inherited
  (:require [csv2rdf.metadata.validator :refer [variant any array-of string one-of bool nullable]]
            [csv2rdf.metadata.types :refer [template-property language-code object-of]]
            [csv2rdf.metadata.datatype :as datatype]))

(def null-value (variant {:string (fn [_context s] [s])
                          :array  (array-of string)}))

(def inherited-properties
  {:aboutUrl      template-property
   :datatype      datatype/datatype
   :default       string
   :lang          language-code
   :null          null-value
   :ordered       bool
   :propertyUrl   template-property
   :required      bool
   :separator     (nullable string)
   :textDirection (one-of #{"ltr" "rtl" "auto" "inherit"})
   :valueUrl      template-property})

(defn metadata-of [{:keys [required optional defaults]}]
  (object-of {:required required
              :optional (merge inherited-properties optional)
              :defaults defaults
              :allow-common-properties? true}))

