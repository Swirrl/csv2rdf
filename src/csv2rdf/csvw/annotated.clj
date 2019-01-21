(ns csv2rdf.csvw.annotated
  "Implementation of 'minimal-plus' mode - this mode operates the same as minimal mode with the addition
   that notes and non-core annotations are output for tables and table groups which specify an @id."
  (:require [csv2rdf.vocabulary :refer [rdf:nil rdf:first rdf:rest]]
            [csv2rdf.csvw.common :refer :all]
            [csv2rdf.csvw.minimal :refer [minimal-row-statements]]
            [csv2rdf.csvw.standard :refer [notes-non-core-annotation-statements]]
            [csv2rdf.util :refer [liberal-mapcat]]))

(defn- optional-annotation-statements
  "Returns the notes and non-core annotations for the given element if it specifies an @id."
  [item]
  (when-let [id (:id item)]
    (notes-non-core-annotation-statements id item)))

(defmethod table-group-context :annotated [mode table-group]
  {:mode mode
   :statements (optional-annotation-statements table-group)})

(defmethod table-statements :annotated [_context {:keys [url] :as table} annotated-rows]
  (concat
    (optional-annotation-statements table)
    (liberal-mapcat (fn [row]
                      (minimal-row-statements url row))
                    annotated-rows)))