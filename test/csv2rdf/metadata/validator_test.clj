(ns csv2rdf.metadata.validator-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.test-common :refer :all]
            [csv2rdf.test-common :refer [suppress-test-logging]]
            [csv2rdf.metadata.validator :refer :all]
            [csv2rdf.logging :as logging]
            [csv2rdf.metadata.context :as context])
  (:import [java.net URI]))

(deftest default-if-invalid-test
  (let [default "default"
        v (default-if-invalid string default)
        context (context/make-context "http://example")]
    (testing "Valid value"
      (let [value "ok"
            {:keys [warnings result]} (logging/capture-warnings (v context value))]
        (is (empty? warnings))
        (is (= value result))))

    (testing "Invalid value"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context 4))]
        (is (= 1 (count warnings)))
        (is (= default result))))))

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
      (validation-error (v {} "Schema")))))

(deftest character-test
  (is (= \c (character {} "c")))
  (is (invalid? (character {} 4)))
  (is (invalid? (character {} "too many"))))

(deftest array-of-test
  (let [v (array-of string)
        context (context/make-context (URI. "http://example"))]
    (testing "Valid array with valid elements"
      (let [arr ["foo" "bar" "baz"]
            {:keys [warnings result]} (logging/capture-warnings (v context arr))]
        (is (empty? warnings))
        (is (= arr result))))

    (testing "Array with invalid elements"
      (let [arr ["foo" 2 "bar" {} "baz" nil]
            {:keys [warnings result]} (logging/capture-warnings (v context arr))]
        (is (= 3 (count warnings)))
        (is (= ["foo" "bar" "baz"] result))))

    (testing "Non-array"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context "not an array"))]
        (is (= 1 (count warnings)))
        (is (= [] result))))))

(deftest tuple-test
  (let [v (tuple string number)
        context (context/make-context (URI. "http://example"))]
    (testing "Valid array"
      (let [arr ["s" 4]
            {:keys [warnings result]} (logging/capture-warnings (v context arr))]
        (is (empty? warnings))
        (is (= arr result))))

    (testing "Array with invalid length"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context ["s" 4 {} true]))]
        (is (some? (seq warnings)))
        (is (invalid? result))))

    (testing "Array with valid length and invalid elements"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context [true "not a number"]))]
        (is (= 2 (count warnings)))
        (is (invalid? result))))

    (testing "Non-array"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context 5))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))

(deftest nullable-test
  (let [v (nullable string)
        context (context/make-context (URI. "http://example"))]
    (testing "nil"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context nil))]
        (is (empty? warnings))
        (is (nil? result))))

    (testing "Non-nil and valid for inner validator"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context "s"))]
        (is (empty? warnings))
        (is (= "s" result))))

    (testing "Non-nil and invalid for inner validator"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context [1,2,3]))]
        (is (some? (seq warnings)))
        (is (invalid? result))))))

(deftest try-parse-with-test
  (let [v (try-parse-with #(Integer/parseInt %))
        context (context/make-context (URI. "http://example"))]
    (testing "Valid"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context "123"))]
        (is (empty? warnings))
        (is (= 123 result))))

    (testing "Invalid"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context "not a number"))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))

(deftest uri-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Valid URI"
      (let [uri-str "http://example.com/some/file.txt"
            {:keys [warnings result]} (logging/capture-warnings (uri context uri-str))]
        (is (empty? warnings))
        (is (= (URI. uri-str) result))))

    (testing "Invalid URI"
      (let [{:keys [warnings result]} (logging/capture-warnings (uri context "not a URI"))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))

    (testing "Invalid type"
      (let [{:keys [warnings result]} (logging/capture-warnings (uri context 4))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))

(deftest required-key-test
  (let [v (required-key :k string)
        context (context/make-context (URI. "http://example"))]
    (testing "Key exists with valid value"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context {:k "value" :other "ignored"}))]
        (is (empty? warnings))
        (is (= [:k "value"] result))))

    (testing "Key missing"
      (validation-error (v context {:other 4})))

    (testing "Key exists with invalid value"
      (validation-error (v context {:k ["not" "a" "string"]})))))

(deftest optional-key-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Without default"
      (let [v (optional-key :k string)]
        (testing "Key exists with valid value"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {:k "value" :other "ignored"}))]
            (is (empty? warnings))
            (is (= [:k "value"] result))))

        (testing "Key missing"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {:other "ignored"}))]
            (is (empty? warnings))
            (is (nil? result))))

        (testing "Key exists with invalid value"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {:k 4}))]
            (is (some? (seq warnings)))
            (is (nil? result))))))

    (testing "With default"
      (let [default "default"
            key :k
            v (optional-key key string default)]
        (testing "Key exists with valid value"
          (let [value "value"
                {:keys [warnings result]} (logging/capture-warnings (v context {key value :other "ignored"}))]
            (is (empty? warnings))
            (is (= [key value] result))))

        (testing "Key missing"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {:other "ignored"}))]
            (is (empty? warnings))
            (is (= [key default] result))))

        (testing "Key exists with invalid value"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {key 5 :other "ignored"}))]
            (is (some? (seq warnings)))
            (is (= [key default] result))))))))

(deftest map-of-test
  (let [v (map-of string number)
        context (context/make-context (URI. "http://example"))]
    (testing "All keys and values valid"
      (let [m {"foo" 1 "bar" 2 "baz" 3}
            {:keys [warnings result]} (logging/capture-warnings (v context m))]
        (is (empty? warnings))
        (is (= m result))))

    (testing "Invalid keys"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context {"foo" 1 [] 2 true 3 "bar" 4 {} 5}))]
        (is (some? (seq warnings)))
        (is (= {"foo" 1 "bar" 4} result))))

    (testing "Invalid values"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context {"foo" 1 "bar" false "baz" "x" "quux" 4}))]
        (is (some? (seq warnings)))
        (is (= {"foo" 1 "quux" 4} result))))))

(deftest one-of-test
  (let [values #{"foo" "bar" "baz"}
        v (one-of values)
        context (context/make-context (URI. "http://example"))]
    (testing "Valid value"
      (doseq [value values]
        (let [{:keys [warnings result]} (logging/capture-warnings (v context value))]
          (is (empty? warnings))
          (is (= value result)))))

    (testing "Invalid value"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context "invalid"))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))

(deftest mapping-test
  (let [value-map {"foo" 1 "bar" 2 "baz" 3}
        context (context/make-context (URI. "http://example"))
        v (mapping value-map)]
    (testing "Valid value"
      (doseq [[key value] value-map]
        (let [{:keys [warnings result]} (logging/capture-warnings (v context key))]
          (is (empty? warnings))
          (is (= value result)))))

    (testing "Invalid value"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context "invalid"))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))

(deftest where-test
  (let [v (where even? "even")
        context (context/make-context (URI. "http://example"))]
    (testing "Valid"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context 2))]
        (is (empty? warnings))
        (is (= 2 result))))

    (testing "Invalid"
      (let [{:keys [warnings result]} (logging/capture-warnings (v context 3))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))

(use-fixtures :each suppress-test-logging)