(ns csv2rdf.csvw-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [csv2rdf.csvw :as csvw])
  (:import [java.net URI]
           [java.nio.file FileSystems]
           [grafter_2.rdf.protocols Quad]))

(defn- make-path [& path]
  (.getPath (FileSystems/getDefault) "w3c-csvw" (into-array String path)))

(deftest csv->rdf-with-paths
  (is (instance? Quad
                 (first (csvw/csv->rdf (make-path "tests" "test011" "tree-ops.csv")
                                       (make-path "tests" "test011" "tree-ops.csv-metadata.json"))))))
