(ns csv2rdf.xml.datatype.parsing-test
  (:require [clojure.test :refer :all]
            [csv2rdf.xml.datatype.parsing :refer :all]
            [csv2rdf.uax35 :as uax35]))

(deftest numeric-format-test
  ;;test 282
  (testing "##0"
    (let [format (uax35/parse-number-format "##0")]
      (are [expected numeric-string] (= expected (parse-format "integer" numeric-string {:pattern format}))
           1 "1"
        12 "12"
        123 "123"
        1234 "1234")))

  (testing "#,#00"
    (let [format (uax35/parse-number-format "#,#00")]
      (are [expected numeric-string] (= expected (parse-format "integer" numeric-string {:pattern format}))
           12 "12"
        123 "123"
        1234 "1,234"
        1234567 "1,234,567")))

  (testing "#,##,#00"
    (let [format (uax35/parse-number-format "#,##,#00")]
      (are [expected numeric-string] (= expected (parse-format "integer" numeric-string {:pattern format}))
           12 "12"
        123 "123"
        1234 "1,234"
        1234567 "12,34,567")))

  (testing "#0.#"
    (let [format (uax35/parse-number-format "#0.#")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1M "1"
        1.2M "1.2"
        1234.5M "1234.5")))

  (testing "#0.0"
    (let [format (uax35/parse-number-format "#0.#")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1.2M "1.2"
        12.3M "12.3"
        1234.5M "1234.5")))

  (testing "#0.0#"
    (let [format (uax35/parse-number-format "#0.0#")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1.2M "1.2"
        1.23M "1.23"
        12.3M "12.3"
        12.34M "12.34")))

  (testing "#0.0#,#"
    (let [format (uax35/parse-number-format "#0.0#,#")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           12.3M "12.3"
        12.34M "12.34"
        12.245M "12.24,5")))

  ;;test 284
  (testing "###0.#####"
    (let [format (uax35/parse-number-format "###0.#####")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1M "1"
        12.3M "12.3"
        123.45M "123.45"
        1.23456M "1.23456")))

  (testing "###0.0000#"
    (let [format (uax35/parse-number-format "###0.0000#")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1.2345M "1.2345"
        12.34567M "12.34567")))

  (testing "00000.0000"
    (let [format (uax35/parse-number-format "00000.0000")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           12345.6789M "12345.6789")))

  (testing "#,##,##0"
    (let [format (uax35/parse-number-format "#,##,##0")]
      (are [expected numeric-string] (= expected (parse-format "integer" numeric-string {:pattern format}))
           1 "1"
        123 "123"
        12345 "12,345"
        123456 "1,23,456")))

  (testing "#,##,#00"
    (let [format (uax35/parse-number-format "#,##,#00")]
      (are [expected numeric-string] (= expected (parse-format "integer" numeric-string {:pattern format}))
           12 "12"
        123 "123"
        12345 "12,345"
        123456 "1,23,456")))

  (testing "#,##,000"
    (let [format (uax35/parse-number-format "#,##,000")]
      (are [expected numeric-string] (= expected (parse-format "integer" numeric-string {:pattern format}))
           123 "123"
        12345 "12,345"
        123456 "1,23,456")))

  (testing "#,#0,000"
    (let [format (uax35/parse-number-format "#,#0,000")]
      (are [expected numeric-string] (= expected (parse-format "integer" numeric-string {:pattern format}))
           1234 "1,234"
        12345 "12,345"
        123456 "1,23,456")))

  (testing "#,00,000"
    (let [format (uax35/parse-number-format "#,00,000")]
      (is (= 12345 (parse-format "integer" "12,345" {:pattern format})))))

  (testing "0,00,000"
    (let [format (uax35/parse-number-format "0,00,000")]
      (is (= 123456 (parse-format "integer" "1,23,456" {:pattern format})))))

  ;;test 285
  (testing "0.0##,###"
    (let [format (uax35/parse-number-format "0.0##,###")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1.1M "1.1"
        1.12M "1.12"
        1.123M "1.123"
        1.1234M "1.123,4"
        1.12345M "1.123,45"
        1.123456M "1.123,456")))

  (testing "0.00#,###"
    (let [format (uax35/parse-number-format "0.00#,###")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1.19M "1.19"
        1.12M "1.12"
        1.123M "1.123"
        1.1234M "1.123,4"
        1.12345M "1.123,45"
        1.123456M "1.123,456")))

  (testing "0.000,###"
    (let [format (uax35/parse-number-format "0.000,###")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1.199M "1.199"
        1.129M "1.129"
        1.123M "1.123"
        1.1234M "1.123,4"
        1.12345M "1.123,45"
        1.123456M "1.123,456")))

  (testing "0.000,0##"
    (let [format (uax35/parse-number-format "0.000,0##")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1.1999M "1.199,9"
        1.1299M "1.129,9"
        1.1239M "1.123,9"
        1.1234M "1.123,4"
        1.12345M "1.123,45"
        1.123456M "1.123,456")))

  (testing "0.000,00#"
    (let [format (uax35/parse-number-format "0.000,00#")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
           1.19999M "1.199,99"
        1.12999M "1.129,99"
        1.12399M "1.123,99"
        1.12349M "1.123,49"
        1.12345M "1.123,45"
        1.123456M "1.123,456")))

  (testing "0.000,000"
    (let [format (uax35/parse-number-format "0.000,000")]
      (are [expected numeric-string] (= expected (parse-format "decimal" numeric-string {:pattern format}))
        1.199999M "1.199,999"
        1.129999M "1.129,999"
        1.123999M "1.123,999"
        1.123499M "1.123,499"
        1.123459M "1.123,459"
        1.123456M "1.123,456"))))
