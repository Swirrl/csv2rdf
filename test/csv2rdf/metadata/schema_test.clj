(ns csv2rdf.metadata.schema-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.schema :refer :all]
            [csv2rdf.metadata.test-common :refer [test-context validation-error validates-as warns-with warns-invalid]]
            [csv2rdf.metadata.validator :refer [invalid?]])
  (:import [java.net URI]))

(deftest foreign-key-reference-test
  (testing "Valid definition with resource"
    (let [resource-uri-str "http://resource/table"
          fk {"columnReference" "col1"
              "resource" resource-uri-str}]
      (validates-as {:columnReference ["col1"] :resource (URI. resource-uri-str)}
                    (foreign-key-reference test-context fk))))

  (testing "Valid definition with schemaReference"
    (let [schema-uri-str "http://schema/id"
          fk {"columnReference" ["col1" "col4"]
              "schemaReference" schema-uri-str}]
      (validates-as {:columnReference ["col1" "col4"] :schemaReference (URI. schema-uri-str)}
                    (foreign-key-reference test-context fk))))

  (testing "Invalid contain no columnReference"
    (validation-error (foreign-key-reference test-context {"resource" "http://table"})))

  (testing "Invalid contains neither resource nor schemaReference"
    (validation-error (foreign-key-reference test-context {"columnReference" "col3"})))

  (testing "Invalid contains both resource and schemaReference"
    (validation-error (foreign-key-reference test-context {"columnReference" ["col1" "col2"]
                                                           "resource" "http://table"
                                                           "schemaReference" "http://schema"})))

  (testing "Invalid contains extra properties"
    (validation-error (foreign-key-reference test-context {"columnReference" ["col1" "col2"]
                                                           "resource" "http://table"
                                                           "dc:title" "not allowed"})))

  (testing "Invalid type"
    (validation-error (foreign-key-reference test-context ["not" "an" "object"]))))

(deftest foreign-key-test
  (testing "Valid"
    (let [fk {"columnReference" "keyCol"
              "reference" {"columnReference" "refCol"
                           "resource" "http://table"}}]
      (validates-as
        {:columnReference ["keyCol"]
         :reference {:columnReference ["refCol"]
                     :resource (URI. "http://table")}}
        (foreign-key test-context fk))))

  (testing "Invalid no column reference"
    (validation-error (foreign-key test-context {"reference" {"columnReference" "col1" "resource" "http://table"}})))

  (testing "Invalid reference"
    (validation-error (foreign-key test-context {"columnReference" "col1" "reference" "not an object"})))

  (testing "Invalid type"
    (warns-invalid (foreign-key test-context "not an object")))

  (testing "Contains extra keys"
    (validation-error (foreign-key test-context {"columnReference" "col"
                                                 "reference" {"columnReference" "col4"
                                                              "resource" "http://table"}
                                                 "invalid" "extra key"}))))

(deftest validate-schema-column-references-test
  (let [columns [{:name "col1"} {:name "col2"}]]
    (testing "Valid"
      (let [schema {:columns     columns
                    :primaryKey  ["col1"]
                    :rowTitles   ["col1" "col2"]
                    :foreignKeys [{:columnReference ["col2"]
                                   :reference       {:columnReference ["refCol"]
                                                     :resource        (URI. "http://table")}}]}]
        (validates-as schema (validate-schema-column-references test-context schema))))

    (testing "Invalid primary key"
      (let [schema {:columns    columns
                    :primaryKey ["col1" "missing"]}]
        (warns-with {:columns columns}
                    (validate-schema-column-references test-context schema))))

    (testing "Invalid row titles"
      (let [schema {:columns columns
                    :rowTitles ["missing" "bad column"]}]
        (warns-with {:columns columns} (validate-schema-column-references test-context schema))))

    (testing "Invalid foreign key"
      (let [schema {:columns columns
                    :foreignKeys [{:columnReference "missing"
                                   :reference {:columnReference ["refCol"]
                                               :resource (URI. "http://table")}}]}]
        (validation-error (validate-schema-column-references test-context schema))))))