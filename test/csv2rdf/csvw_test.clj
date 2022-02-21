(ns csv2rdf.csvw-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [csv2rdf.csvw :as csvw])
  (:import [java.net URI]
           [java.nio.file FileSystems]
           [grafter_2.rdf.protocols Quad]))

(def w3c-dir "w3c-csvw")

(defn- make-path [& path]
  (.getPath (FileSystems/getDefault) w3c-dir (into-array String path)))

(defn make-file [& path]
  (apply io/file w3c-dir path))

(defn csv->rdf? [csv metadata]
  (instance? Quad
             (first (csvw/csv->rdf csv metadata))))

(deftest csv->rdf-supported-types-test
  (testing "with java.io.File"
    (let [csv      (make-file "./tests" "test011" "tree-ops.csv")
          metadata (make-file "./tests" "test011" "tree-ops.csv-metadata.json")]

      (is (csv->rdf? csv metadata))
      (is (csv->rdf? nil metadata)
          "Resolves csv")))

  (testing "with URI"
    (let [csv      (.toURI (make-file "./tests" "test011" "tree-ops.csv"))
          metadata (.toURI (make-file "./tests" "test011" "tree-ops.csv-metadata.json"))]

      (is (csv->rdf? csv metadata))
      (is (csv->rdf? nil metadata)
          "Resolves csv")))

  (testing "with java.nio.Path"
    (let [csv      (make-path "tests" "test011" "tree-ops.csv")
          metadata (make-path "tests" "test011" "tree-ops.csv-metadata.json")]

      (is (csv->rdf? csv metadata))
      (is (csv->rdf? nil metadata)
          "Resolves csv"))))
