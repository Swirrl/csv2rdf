(ns csv2rdf.metadata.validator-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.test-common :refer :all]
            [csv2rdf.test-common :refer [suppress-test-logging]]
            [csv2rdf.metadata.validator :refer :all]
            [csv2rdf.logging :as logging]
            [csv2rdf.metadata.test-common :refer [validates-as warns-with warns-invalid]]
            [csv2rdf.metadata.context :as context])
  (:import [java.net URI]))

(deftest default-if-invalid-test
  (let [default "default"
        v (default-if-invalid string default)
        context (context/make-context "http://example")]
    (testing "Valid value"
      (let [value "ok"]
        (validates-as value (v context value))))

    (testing "Invalid value"
      (warns-with default (v context 4)))))

(deftest eq-test
  (let [v (eq "value")]
    (testing "Matches value"
      (validates-as "value" (v {} "value")))

    (testing "Does not match value"
      (warns-invalid (v {} "other value")))))

(deftest type-eq-test
  (let [value "Table"
        v (type-eq value)]
    (testing "Matches value"
      (is (= value (v {} value))))

    (testing "Does not match value"
      (validation-error (v {} "Schema")))))

(deftest character-test
  (is (= \c (character {} "c")))
  (is (invalid? (character {} 4)))
  (is (invalid? (character {} "too many"))))

(deftest array-of-test
  (let [v (array-of string)
        context (context/make-context (URI. "http://example"))]
    (testing "Valid array with valid elements"
      (let [arr ["foo" "bar" "baz"]]
        (validates-as arr (v context arr))))

    (testing "Array with invalid elements"
      (let [arr ["foo" 2 "bar" {} "baz" nil]
            {:keys [warnings result]} (logging/capture-warnings (v context arr))]
        (is (= 3 (count warnings)))
        (is (= ["foo" "bar" "baz"] result))))

    (testing "Non-array"
      (warns-with [] (v context "not an array")))))

(deftest nullable-test
  (let [v (nullable string)
        context (context/make-context (URI. "http://example"))]
    (validates-as nil (v context nil))

    (testing "Non-nil and valid for inner validator"
      (validates-as "s" (v context "s")))

    (testing "Non-nil and invalid for inner validator"
      (warns-invalid (v context [1,2,3])))))

(deftest try-parse-with-test
  (let [v (try-parse-with #(Integer/parseInt %))
        context (context/make-context (URI. "http://example"))]
    (validates-as 123 (v context "123"))

    (testing "Invalid"
      (warns-invalid (v context "not a number")))))

(deftest uri-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Valid URI"
      (let [uri-str "http://example.com/some/file.txt"]
        (validates-as (URI. uri-str) (uri context uri-str))))

    (testing "Invalid URI"
      (warns-invalid (uri context "not a URI")))

    (testing "Invalid type"
      (warns-invalid (uri context 4)))))

(deftest required-key-test
  (let [v (required-key :k string)
        context (context/make-context (URI. "http://example"))]
    (testing "Key exists with valid value"
      (validates-as [:k "value"] (v context {:k "value" :other "ignored"})))

    (testing "Key missing"
      (validation-error (v context {:other 4})))

    (testing "Key exists with invalid value"
      (validation-error (v context {:k ["not" "a" "string"]})))))

(deftest optional-key-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Without default"
      (let [v (optional-key :k string)]
        (testing "Key exists with valid value"
          (validates-as [:k "value"] (v context {:k "value" :other "ignored"})))

        (testing "Key missing"
          (validates-as nil (v context {:other "ignored"})))

        (testing "Key exists with invalid value"
          (warns-with nil (v context {:k 4})))))

    (testing "With default"
      (let [default "default"
            key :k
            v (optional-key key string default)]
        (testing "Key exists with valid value"
          (let [value "value"]
            (validates-as [key value] (v context {key value :other "ignored"}))))

        (testing "Key missing"
          (validates-as [key default] (v context {:other "ignored"})))

        (testing "Key exists with invalid value"
          (warns-with [key default] (v context {key 5 :other "ignored"})))))))

(deftest map-of-test
  (let [v (map-of string number)
        context (context/make-context (URI. "http://example"))]
    (testing "All keys and values valid"
      (let [m {"foo" 1 "bar" 2 "baz" 3}]
        (validates-as m (v context m))))

    (testing "Invalid keys"
      (warns-with {"foo" 1 "bar" 4} (v context {"foo" 1 [] 2 true 3 "bar" 4 {} 5})))

    (testing "Invalid values"
      (warns-with {"foo" 1 "quux" 4} (v context {"foo" 1 "bar" false "baz" "x" "quux" 4})))))

(deftest one-of-test
  (let [values #{"foo" "bar" "baz"}
        v (one-of values)
        context (context/make-context (URI. "http://example"))]
    (testing "Valid value"
      (doseq [value values]
        (validates-as value (v context value))))

    (testing "Invalid value"
      (warns-invalid (v context "invalid")))))

(deftest mapping-test
  (let [value-map {"foo" 1 "bar" 2 "baz" 3}
        context (context/make-context (URI. "http://example"))
        v (mapping value-map)]
    (testing "Valid value"
      (doseq [[key value] value-map]
        (validates-as value (v context key))))

    (testing "Invalid value"
      (warns-invalid (v context "invalid")))))

(deftest where-test
  (let [v (where even? "even")
        context (context/make-context (URI. "http://example"))]
    (testing "Valid"
      (validates-as 2 (v context 2)))

    (testing "Invalid"
      (warns-invalid (v context 3)))))

(use-fixtures :each suppress-test-logging)