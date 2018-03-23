(ns csv2rdf.json-ld
  (:require [csv2rdf.util :as util]
            [clojure.java.io :as io])
  (:import [java.util Map HashMap]
           [com.github.jsonldjava.core Context]))

;;TODO: try to resolve from URI first before falling-back on resource version?
(defn- get-csvw-context []
  (let [doc (util/read-json (io/resource "csvw.json"))]
    (get doc "@context")))

(def csvw-context (delay (get-csvw-context)))

;;TODO: see if this can be done through the public API
(def resolve-iri-method (util/get-declared-method Context "expandIri" [String Boolean/TYPE Boolean/TYPE Map Map]))

(defn expand-uri-string [uri-str]
  (util/invoke-method resolve-iri-method (Context.) [uri-str false false @csvw-context (HashMap.)]))