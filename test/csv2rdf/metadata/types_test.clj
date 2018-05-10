(ns csv2rdf.metadata.types-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.types :refer :all]
            [csv2rdf.metadata.validator :refer [invalid?]]
            [csv2rdf.metadata.context :as context]
            [csv2rdf.logging :as logging])
  (:import [java.net URI]))

(deftest non-negative-test
  (let [context (context/make-context "http://example")]
    (testing "Valid value"
      (let [{:keys [warnings result]} (logging/capture-warnings (non-negative context 0))]
        (is (empty? warnings))
        (is (= 0 result))))

    (testing "Invalid value"
      (let [{:keys [warnings result]} (logging/capture-warnings (non-negative context -3))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))

    (testing "Invalid type"
      (let [{:keys [warnings result]} (logging/capture-warnings (non-negative context "not a number"))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))

(deftest language-code-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Valid language code"
      (let [code "en-gb"
            {:keys [warnings result]} (logging/capture-warnings (language-code context code))]
        (is (empty? warnings))
        (is (= code result))))

    (testing "Invalid language code"
      (let [{:keys [warnings result]} (logging/capture-warnings (language-code context "not a valid language code"))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))
