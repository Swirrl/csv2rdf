(ns csv2rdf.metadata.inherited
  (:require [csv2rdf.metadata.validator :refer [variant any array-of string one-of]]
            [csv2rdf.metadata.types :refer [template-property language-code object-of]]
            [csv2rdf.metadata.datatype :as datatype]))

(def null-value (variant {:string any :array (array-of string)}))

(def inherited-properties
  {:aboutUrl      template-property
   :datatype      datatype/datatype
   :default       string
   :lang          language-code
   :null          null-value
   :ordered       boolean
   :propertyUrl   template-property
   :required      boolean
   :separator     string
   :textDirection (one-of #{"ltr" "rtl" "auto" "inherit"})
   :valueUrl      template-property})

(def inherited-defaults
  {:default ""
   :lang "und"
   :null [""]
   :ordered false
   :required false
   :separator nil
   :textDirection "inherit"})

(defn inherit
  "Inherits in the child any inherited properties defined by the parent"
  [parent child]
  (let [inherited-keys (keys inherited-properties)
        inherited (select-keys parent inherited-keys)]
    (merge inherited child)))

(defn inherit-defaults [obj]
  (merge inherited-defaults obj))

(defn metadata-of [{:keys [required optional defaults]}]
  (object-of {:required required
              :optional (merge inherited-properties optional)
              :defaults defaults
              :allow-common-properties? true}))

