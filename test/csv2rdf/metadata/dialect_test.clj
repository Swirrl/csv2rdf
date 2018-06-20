(ns csv2rdf.metadata.dialect-test
  (:require [csv2rdf.metadata.dialect :refer :all :as dialect]
            [csv2rdf.metadata.test-common :refer [test-context validates-as warns-with warns-invalid]]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [csv2rdf.metadata.validator :refer [invalid?]])
  (:import [clojure.lang ExceptionInfo]))

(deftest dialect->options-trim-test
  (testing "None"
    (is (= :all (:trim-mode (dialect->options {})))))

  (testing "skipInitialSpace"
    (are [skip expected] (= expected (:trim-mode (dialect->options {:skipInitialSpace skip})))
      true :start
      false :none))

  (testing "Explicit trim"
    (let [p (prop/for-all [[trim skip] (gen/tuple (s/gen ::dialect/trim-mode) (s/gen ::dialect/skipInitialSpace))]
                          (= (or trim :all) (:trim-mode (dialect->options {:trim trim :skipInitialSpace skip}))))
          {:keys [result]} (tc/quick-check 100 p)]
      (is result))))

(deftest dialect->options-num-header-rows-test
  (testing "None"
    (is (= 1 (:num-header-rows (dialect->options {})))))

  (testing "header"
    (are [header expected] (= expected (:num-header-rows (dialect->options {:header header})))
      true 1
      false 0))

  (testing "Explcit headerRowCount"
    (let [p (prop/for-all [[header row-count] (gen/tuple gen/boolean gen/nat)]
                          (= row-count (:num-header-rows (dialect->options {:header header :headerRowCount row-count}))))
          {:keys [result]} (tc/quick-check 100 p)]
      (is result))))

(deftest dialect->options-quote-escape-char-test
  (testing "None"
    (let [{:keys [quoteChar escapeChar]} (dialect->options {})]
      (is (= \" quoteChar))
      (is (= \" escapeChar))))

  (testing "doubleQuote true without quoteChar"
    (let [{:keys [quoteChar escapeChar]} (dialect->options {:doubleQuote true})]
      (is (= \" quoteChar))
      (is (= \" escapeChar))))

  (testing "doubleQuote false without quoteChar"
    (let [{:keys [quoteChar escapeChar]} (dialect->options {:doubleQuote false})]
      (is (= \" quoteChar))
      (is (= \\ escapeChar))))

  (testing "quoteChar nil"
    (let [{:keys [quoteChar escapeChar]} (dialect->options {:quoteChar nil})]
      (is (nil? quoteChar))
      (is (nil? escapeChar))))

  (testing "doubleQuote and quoteChar set"
    (let [{:keys [quoteChar escapeChar]} (dialect->options {:quoteChar \@ :doubleQuote false})]
      (is (= \@ quoteChar))
      (is (= \\ escapeChar))))

  (testing "quoteChar nil when doubleQuote set"
    (is (thrown? ExceptionInfo (dialect->options {:quoteChar nil :doubleQuote false})))))

(deftest get-default-dialect-test
  (are [headers sub-result] (= sub-result (select-keys (get-default-dialect headers) (keys sub-result)))
       {} {:encoding "utf-8" :header nil :delimiter \,}
    {"Content-Type" "text/tab-separated-values"} {:encoding "utf-8" :header nil :delimiter \tab}
    {"Content-Type" "text/csv; header=absent"} {:encoding "utf-8" :header false :delimiter \,}
    {"Content-Type" "text/csv; charset=latin-1; header=absent"} {:encoding "latin-1" :header false :delimiter \,}))

(deftest encoding-test
  (testing "valid encoding"
    (let [enc "utf-16"]
      (validates-as enc (encoding test-context enc))))

  (testing "invalid encoding"
    (warns-invalid (encoding test-context "unknown encoding")))

  (testing "invalid type"
    (warns-invalid (encoding test-context ["not" "a" "string"]))))

(deftest trim-mode-test
  (testing "valid string"
    (are [expected s] (= expected (trim-mode test-context s))
      :all "true"
      :start "start"
      :end "end"
      :none "false"))

  (testing "invalid string"
    (warns-invalid (trim-mode test-context "unknown")))

  (testing "boolean"
    (are [expected b] (= expected (trim-mode test-context b))
      :all true
      :none false))

  (testing "invalid type"
    (warns-invalid (trim-mode test-context ["not" "a" "string" "or" "boolean"]))))

(defn with-instrumentation [f]
  (let [to-instrument [`calculate-dialect-options `expand-dialect]]
    (doseq [sym to-instrument]
      (stest/instrument sym))
    (try
      (f)
      (finally
        (doseq [sym to-instrument]
          (stest/unstrument sym))))))

(use-fixtures :once with-instrumentation)
