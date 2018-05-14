(ns csv2rdf.metadata.inherited-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.inherited :refer :all]
            [csv2rdf.metadata.validator :refer [invalid?]]
            [csv2rdf.logging :as logging]
            [csv2rdf.metadata.context :as context])
  (:import [java.net URI]))

(deftest null-value-test
  (testing "string"
    (let [context (context/make-context (URI. "http://example"))
          {:keys [warnings result]} (logging/capture-warnings (null-value context "null"))]
      (empty? warnings)
      (is (= ["null"] result))))

  (testing "array"
    (let [arr ["null" "missing" "nicht"]
          context (context/make-context (URI. "http://example"))
          {:keys [warnings result]} (logging/capture-warnings (null-value context arr))]
      (is (empty? warnings))
      (is (= arr result))))

  (testing "array with invalid values"
    (let [context (context/make-context (URI. "http://example"))
          {:keys [warnings result]} (logging/capture-warnings (null-value context ["null" 3 false {} "missing"]))]
      (is (= 3 (count warnings)))
      (is (= ["null" "missing"] result))))

  (testing "invalid type"
    (let [context (context/make-context (URI. "http://example"))
          {:keys [warnings result]} (logging/capture-warnings (null-value context 4))]
      (is (= 1 (count warnings)))
      (is (invalid? result)))))
