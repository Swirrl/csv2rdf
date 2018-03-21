(ns csv2rdf.metadata.column
  (:require [csv2rdf.xml.datatype :as xml-datatype]))

(defn datatype
  "Gets the effective datatype for this column"
  [{:keys [datatype] :as col}]
  (xml-datatype/expand datatype))

(defn datatype-base [column]
  (:base (datatype column)))

(defn ^{:metadata-spec "5.7"} default
  "Gets the effective default value for this column"
  [{:keys [default]}]
  (or default ""))
