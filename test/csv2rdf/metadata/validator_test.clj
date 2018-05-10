(ns csv2rdf.metadata.validator-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.validator :refer :all]
            [csv2rdf.logging :as logging])
  (:import [clojure.lang ExceptionInfo]))

(deftest eq-test
  (let [v (eq "value")]
    (testing "Matches value"
      (let [{:keys [warnings result]} (logging/capture-warnings (v {} "value"))]
        (is (empty? warnings))
        (is (= "value" result))))

    (testing "Does not match value"
      (let [{:keys [warnings result]} (logging/capture-warnings (v {} "other value"))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))

(deftest type-eq-test
  (let [value "Table"
        v (type-eq value)]
    (testing "Matches value"
      (is (= value (v {} value))))

    (testing "Does not match value"
      (is (thrown? ExceptionInfo (v {} "Schema"))))))

(deftest character-test
  (is (= \c (character {} "c")))
  (is (invalid? (character {} 4)))
  (is (invalid? (character {} "too many"))))