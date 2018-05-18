(ns csv2rdf.metadata.datatype-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.datatype :refer :all]
            [csv2rdf.metadata.test-common :refer [test-context validation-error validates-as warns-with warns-invalid]]
            [csv2rdf.metadata.validator :refer [invalid?]]
            [csv2rdf.logging :as logging])
  (:import [java.time.format DateTimeFormatter]
           [java.util.regex Pattern]))

(deftest datatype-test
  (testing "valid datatype name"
    (validates-as {:base "string"} (datatype test-context "string")))

  (testing "invalid datatype name"
    (warns-invalid (datatype test-context "invalid datatype")))

  (testing "derived datatype with valid name"
    (validates-as {:base "string"} (datatype test-context {"base" "string"})))

  (testing "derived datatype with invalid name"
    (warns-with {} (datatype test-context {"base" "invalid datatype"})))

  (testing "numeric type with valid string format"
    (let [{:keys [warnings result]} (logging/capture-warnings (datatype test-context {"base" "integer"
                                                                                      "format" "##0"}))]
      (is (empty? warnings))
      (contains? result :format)))

  (testing "numeric type with invalid string format"
    (warns-with {:base "integer"} (datatype test-context {"base" "integer"
                                                          "format" "invalid format"})))

  (testing "numeric type with object format"
    (let [{:keys [warnings result]} (logging/capture-warnings (datatype test-context {"base"   "integer"
                                                                                      "format" {"pattern"   "##0"
                                                                                                "groupChar" ","}}))
          {:keys [pattern groupChar]} (:format result)]
      (is (empty? warnings))
      (is (some? pattern))
      (is (= \, groupChar))))

  (testing "numeric type with object format containing invalid numeric format string"
    (warns-with {:base "integer"
                 :format {:decimalChar \,}}
                (datatype test-context {"base" "integer"
                                        "format" {"decimalChar" ","
                                                  "format" "invalid number format"}})))

  (testing "boolean type with valid format"
    (validates-as {:base "boolean"
                   :format {:true-values #{"Y"}
                            :false-values #{"N"}}}
                  (datatype test-context {"base" "boolean"
                                          "format" "Y|N"})))

  (testing "boolean type with invalid format"
    (warns-with {:base "boolean"} (datatype test-context {"base" "boolean"
                                                          "format" "invalid format"})))

  (testing "date/time type with valid format"
    (let [{:keys [warnings result]} (logging/capture-warnings (datatype test-context {"base" "date" "format" "ddMMyy"}))]
      (is (empty? warnings))
      (is (= "date" (:base result)))
      (is (instance? DateTimeFormatter (:format result)))))

  (testing "date/time with invalid format"
    (warns-with {:base "datetime"} (datatype test-context {"base" "datetime" "format" "invalid date format"})))

  (testing "other type with format"
    (let [{:keys [warnings result]} (logging/capture-warnings (datatype test-context {"base" "anyURI"
                                                                                      "format" "[ab]+"}))
          pat (:format result)]
      (is (empty? warnings))
      (is (instance? Pattern pat))
      (is (= "[ab]+" (str pat)))))

  (testing "string with valid length constraints"
    (validates-as {:base "string"
                   :minLength 4
                   :maxLength 10}
                  (datatype test-context {"base" "string"
                                          "minLength" 4
                                          "maxLength" 10})))

  (testing "length < minLength"
    (validation-error (datatype test-context {"base" "string"
                                              "length" 4
                                              "minLength" 5})))

  (testing "length > maxLength"
    (validation-error (datatype test-context {"base" "string"
                                              "length" 10
                                              "maxLength" 7})))

  (testing "minLength > maxLength"
    (validation-error (datatype test-context {"base" "string"
                                              "minLength" 10
                                              "maxLength" 4})))

  (testing "Invalid type for length constraints"
    (validation-error (datatype test-context {"base" "integer"
                                              "minLength" 4
                                              "maxLength" 10})))

  (testing "minimum different from minInclusive"
    (validation-error (datatype test-context {"base" "integer"
                                              "minimum" 4
                                              "minInclusive" 5})))

  (testing "maximum different from maxInclusive"
    (validation-error (datatype test-context {"base" "integer"
                                              "maximum" 10
                                              "maxInclusive" 11})))

  (testing "minInclusive and minExclusive both specified"
    (validation-error (datatype test-context {"base" "integer"
                                              "minInclusive" 4
                                              "minExclusive" 3})))

  (testing "maxInclusive and maxExclusive both specified"
    (validation-error (datatype test-context {"base" "integer"
                                              "maxInclusive" 5
                                              "maxExclusive" 6})))

  (testing "maxInclusive < minInclusive"
    (validation-error (datatype test-context {"base" "integer"
                                              "minInclusive" 5
                                              "maxInclusive" 4})))

  (testing "maxExclusive <= minInclusive"
    (validation-error (datatype test-context {"base" "integer"
                                              "minInclusive" 6
                                              "maxExclusive" 6})))

  (testing "maxExclusive < minExclusive"
    (validation-error (datatype test-context {"base" "integer"
                                              "minExclusive" 8
                                              "maxExclusive" 7})))

  (testing "maxInclusive <= minExclusive"
    (validation-error (datatype test-context {"base" "integer"
                                              "minExclusive" 7
                                              "maxInclusive" 7})))

  (testing "minimum/maximum on invalid type"
    (validation-error (datatype test-context {"base" "string"
                                              "minimum" 5
                                              "maximum" 8})))

  (testing "minInclusive/maxInclusive on invalid type"
    (validation-error (datatype test-context {"base" "string"
                                              "minInclusive" 4
                                              "maxInclusive" 8})))

  (testing "minExclusive/maxExclusive on invalid type"
    (validation-error (datatype test-context {"base" "string"
                                              "minExclusive" 4
                                              "maxExclusive" 9})))

  (testing "datatype id matches built-in datatype URI"
    (validation-error (datatype test-context {"base" "integer"
                                              "@id" "http://www.w3.org/2001/XMLSchema#double"})))

  (testing "type property invalid"
    (validation-error (datatype test-context {"base" "string"
                                              "@type" "not Datatype"})))

  (testing "invalid type"
    (warns-invalid (datatype test-context ["not" "an" "object" "or" "string"]))))
