(ns csv2rdf.metadata.inherited
  (:require [csv2rdf.metadata.validator :refer [variant any array-of string one-of bool nullable]]
            [csv2rdf.metadata.types :refer [template-property language-code object-of]]
            [csv2rdf.metadata.datatype :as datatype]
            [csv2rdf.validation :as v]))

(def null-value (variant {:string (fn [_context s] (v/pure [s]))
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

(def inherited-defaults
  {:default ""
   ;;NOTE: spec specified default as 'und' but does not use it for string in the test data
   :lang nil
   :null [""]
   :ordered false
   :required false
   :separator nil
   :textDirection "inherit"
   :datatype datatype/datatype-defaults})

(defn inherit
  "Inherits in the child any inherited properties defined by the parent"
  [parent child]
  (let [inherited-keys (keys inherited-properties)
        inherited (select-keys parent inherited-keys)]
    (merge inherited child)))

(defn inherit-defaults [obj]
  (let [expanded (if (contains? obj :datatype)
                   (update obj :datatype datatype/expand-properties)
                   obj)]
    (merge inherited-defaults expanded)))

(defn inherit-with-defaults [parent child]
  (inherit-defaults (inherit parent child)))

(defn expand-inherit [parent child]
  ;;1. expand any inherited properties in the child
  ;;2. inherit any inherited properties from the parent
  ;;3. apply default values for any inherited properties still undefined
  ;;TODO: step 3 redundant if this process has been applied to the parent?
  (let [expanded-child (if (contains? child :datatype)
                         (update child :datatype datatype/expand-properties)
                         child)
        inherited (inherit parent expanded-child)]
    (merge inherited-defaults inherited)))

(defn metadata-of [{:keys [required optional defaults]}]
  (object-of {:required required
              :optional (merge inherited-properties optional)
              :defaults defaults
              :allow-common-properties? true}))

