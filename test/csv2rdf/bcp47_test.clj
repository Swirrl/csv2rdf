(ns csv2rdf.bcp47-test
  (:require [clojure.test :refer :all]
            [csv2rdf.bcp47 :refer :all]))

(deftest parse-language-tag-test
  (testing "language only"
    (is (= {:language "en"} (parse-language-tag "en"))))

  (testing "language and script"
    (is (= {:language "sr" :script "Latn"} (parse-language-tag "sr-Latn"))))

  (testing "language and region tag"
    (is (= {:language "de" :region "AT"} (parse-language-tag "de-AT"))))

  (testing "langauge, script and region"
    (is (= {:language "sr" :script "Latn" :region "RS"} (parse-language-tag "sr-Latn-RS"))))

  (testing "language and region code"
    (is (= {:language "es" :region "419"} (parse-language-tag "es-419"))))

  (testing "language, region and variant"
    (is (= {:language "de" :region "CH" :variant "1996"} (parse-language-tag "de-CH-1996"))))

  (testing "language and extension"
    (is (= {:language "en" :extension {:tag "r" :elements ["extended" "sequence"]}}
           (parse-language-tag "en-r-extended-sequence"))))

  (testing "all elements"
    (is (= {:language "en" :script "Latn" :region "GB" :variant "boont"
            :extension {:tag "r" :elements ["extended" "sequence"]} :private ["private"]}
           (parse-language-tag "en-Latn-GB-boont-r-extended-sequence-x-private"))))

  (testing "Invalid language"
    (is (thrown? IllegalArgumentException (parse-language-tag "invalid-language"))))

  (testing "Invalid extension missing"
    (is (thrown? IllegalArgumentException (parse-language-tag "en-US-a"))))

  (testing "Invalid extension sequence"
    (is (thrown? IllegalArgumentException (parse-language-tag "en-US-a-w"))))

  (testing "Invalid private"
    (is (thrown? IllegalArgumentException (parse-language-tag "x"))))

  (testing "private only"
    (is (= {:private ["wadegile" "private1"]} (parse-language-tag "x-wadegile-private1")))))

(deftest language-tag-string-truncation-strings-test
  (is (= ["zh-Latn-CN-variant1-a-extend1-x-wadegile-private1"
          "zh-Latn-CN-variant1-a-extend1-x-wadegile"
          "zh-Latn-CN-variant1-a-extend1"
          "zh-Latn-CN-variant1"
          "zh-Latn-CN"
          "zh-Latn"
          "zh"]
         (language-tag-string-truncation-strings "zh-Latn-CN-variant1-a-extend1-x-wadegile-private1"))))


