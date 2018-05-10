(ns csv2rdf.metadata.types-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.types :refer :all]
            [csv2rdf.metadata.validator :refer [invalid?]]
            [csv2rdf.metadata.context :as context]
            [csv2rdf.logging :as logging])
  (:import [java.net URI]))

(defn set-context-language [context language]
  (context/update-from-local-context context {context/language-key language}))

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

(deftest natural-language-test
  (let [context (context/make-context "http://example")]
    (testing "string"
      (testing "default langauge"
        (let [value "title"
              {:keys [warnings result]} (logging/capture-warnings (natural-language context value))]
          (is (empty? warnings))
          (is (= {"und" [value]} result))))

      (testing "specified language"
        (let [value "der title"
              language "de"
              context (set-context-language context language)
              {:keys [warnings result]} (logging/capture-warnings (natural-language context value))]
          (is (empty? warnings))
          (is (= {language [value]} result)))))

    (testing "array"
      (testing "default langauge"
        (testing "all values valid"
          (let [values ["title1" "title2"]
                {:keys [warnings result]} (logging/capture-warnings (natural-language context values))]
            (is (empty? warnings))
            (is (= {"und" values} result))))

        (testing "with invalid values"
          (let [{:keys [warnings result]} (logging/capture-warnings (natural-language context ["title1" 3 {} "title2" []]))]
            (is (= 3 (count warnings)))
            (is (= {"und" ["title1" "title2"]} result)))))

      (testing "specified language"
        (let [language "en"
              context (set-context-language context language)]
          (testing "all values valid"
            (let [values ["title1" "title2"]
                  {:keys [warnings result]} (logging/capture-warnings (natural-language context values))]
              (is (empty? warnings))
              (is (= {language values} result))))

          (testing "with invalid values"
            (let [{:keys [warnings result]} (logging/capture-warnings (natural-language context ["title1" 1 "title2" false [] {}]))]
              (is (= 4 (count warnings)))
              (is (= {language ["title1" "title2"]} result)))))))

    (testing "object"
      (testing "valid values"
        (let [{:keys [warnings result]} (logging/capture-warnings (natural-language context {"en" "title" "fr" ["title un" "title deux"]}))]
          (is (empty? warnings))
          (is (= {"en" ["title"]
                  "fr" ["title un" "title deux"]})
              result)))

      (testing "with invalid values"
        (let [{:keys [warnings result]} (logging/capture-warnings (natural-language context {"en" "title" "fr" 4 "de" {} "es" false}))]
          (is (= 3 (count warnings)))
          (is (= {"en" ["title"]
                  "fr" []
                  "de" []
                  "es" []}
                 result))))

      (testing "with invalid keys"
        (let [{:keys [warnings result]} (logging/capture-warnings (natural-language context {"en" "title" 4 "?" "de" ["title eins" "title zwei"] false "!!!"}))]
          (is (= 2 (count warnings)))
          (is (= {"en" ["title"]
                  "de" ["title eins" "title zwei"]}
                 result)))))

    (testing "Invalid type"
      (testing "Default language"
        (let [{:keys [warnings result]} (logging/capture-warnings (natural-language context 4))]
          (is (= 1 (count warnings)))
          (is (= {"und" []} result))))

      (testing "Specified langauge"
        (let [language "en"
              context (set-context-language context language)
              {:keys [warnings result]} (logging/capture-warnings (natural-language context 3))]
          (is (= 1 (count warnings)))
          (is (= {language []} result)))))))

(deftest link-property-test
  (let [base-uri (URI. "http://example.com/")
        context (context/make-context base-uri)]
    (testing "Valid value"
      (let [rel "file.csv"
            {:keys [warnings result]} (logging/capture-warnings (link-property context rel))]
        (is (empty? warnings))
        (is (= (.resolve base-uri rel) result))))

    (testing "Invalid value"
      (let [{:keys [warnings result]} (logging/capture-warnings (link-property context "not a URI"))]
        (is (= 1 (count warnings)))
        (is (= base-uri result))))

    (testing "Invalid type"
      (let [{:keys [warnings result]} (logging/capture-warnings (link-property context 43))]
        (is (= 1 (count warnings)))
        (is (= base-uri result))))))