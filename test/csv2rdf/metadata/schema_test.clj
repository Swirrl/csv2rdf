(ns csv2rdf.metadata.schema-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.schema :refer :all]
            [csv2rdf.metadata.test-common :refer [test-context validation-error]]
            [csv2rdf.logging :as logging])
  (:import [java.net URI]))

(deftest foreign-key-reference-test
  (testing "Valid definition with resource"
    (let [resource-uri-str "http://resource/table"
          fk {"columnReference" "col1"
              "resource" resource-uri-str}
          {:keys [warnings result]} (logging/capture-warnings (foreign-key-reference test-context fk))]
      (is (empty? warnings))
      (is (= {:columnReference ["col1"] :resource (URI. resource-uri-str)} result))))

  (testing "Valid definition with schemaReference"
    (let [schema-uri-str "http://schema/id"
          fk {"columnReference" ["col1" "col4"]
              "schemaReference" schema-uri-str}
          {:keys [warnings result]} (logging/capture-warnings (foreign-key-reference test-context fk))]
      (is (empty? warnings))
      (is (= {:columnReference ["col1" "col4"] :schemaReference (URI. schema-uri-str)} result))))

  (testing "Invalid contain no columnReference"
    (validation-error (foreign-key-reference test-context {"resource" "http://table"})))

  (testing "Invalid contains neither resource nor schemaReference"
    (validation-error (foreign-key-reference test-context {"columnReference" "col3"})))

  (testing "Invalid contains both resource and schemaReference"
    (validation-error (foreign-key-reference test-context {"columnReference" ["col1" "col2"]
                                                           "resource" "http://table"
                                                           "schemaReference" "http://schema"})))

  (testing "Invalid type"
    (validation-error (foreign-key-reference test-context ["not" "an" "object"]))))
