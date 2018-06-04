(ns csv2rdf.metadata.inherited-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.test-common :refer [test-context validates-as warns-with warns-invalid]]
            [csv2rdf.metadata.inherited :refer :all]
            [csv2rdf.metadata.validator :refer [invalid?]]))

(deftest null-value-test
  (testing "string"
    (validates-as ["null"] (null-value test-context "null")))

  (testing "array"
    (let [arr ["null" "missing" "nicht"]]
      (validates-as arr (null-value test-context arr))))

  (testing "array with invalid values"
    (warns-with ["null" "missing"] (null-value test-context ["null" 3 false {} "missing"])))

  (testing "invalid type"
    (warns-invalid (null-value test-context 4))))
