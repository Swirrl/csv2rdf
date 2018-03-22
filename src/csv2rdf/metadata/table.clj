(ns csv2rdf.metadata.table
  (:require [csv2rdf.metadata.validator :refer [array-of bool eq]]
            [csv2rdf.metadata.types :refer [link-property note table-direction object-property id contextual-object]]
            [csv2rdf.metadata.inherited :refer [metadata-of] :as inherited]
            [csv2rdf.metadata.schema :as schema]
            [csv2rdf.metadata.transformation :as transformation]
            [csv2rdf.validation :as v]))

(def table-defaults
  {:suppressOutput false
   :tableDirection "auto"})

(def table
  (metadata-of
    {:required {:url link-property}
     :optional {:notes           (array-of note)
                :suppressOutput  bool
                :tableDirection  table-direction
                :tableSchema     (object-property schema/schema)
                :transformations (array-of transformation/transformation)
                :id             id
                :type           (eq "Table")}}))

(defn into-table-group [table]
  {:tables [table]})

(defn looks-like-table-json? [doc]
  (contains? doc "url"))


(defn inherit-and-apply-defaults [parent table]
  (->> table
       (inherited/inherit parent)
       (inherited/inherit-defaults)
       (merge table-defaults)))

(defn parse-table-json [context doc]
  (v/fmap (fn [t]
            (into-table-group (inherit-and-apply-defaults {} t)))
          ((contextual-object true table) context doc)))

