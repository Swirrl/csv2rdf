(ns csv2rdf.metadata
  (:require [csv2rdf.metadata.context :refer [make-context]]
            [csv2rdf.metadata.table :as table]
            [csv2rdf.metadata.types :refer [contextual-object]]
            [csv2rdf.util :as util]))

(defn parse-file [f]
  (let [json (util/read-json f)
        context (make-context (.toURI f))]
    ((contextual-object true table/table) context json)))