(ns csv2rdf.metadata.types-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.types :refer :all]
            [csv2rdf.metadata.test-common :refer :all]
            [csv2rdf.metadata.validator :refer [invalid?]]
            [csv2rdf.metadata.context :as context]
            [csv2rdf.logging :as logging]
            [csv2rdf.vocabulary :refer [csvw:Table csvw:TableGroup]])
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

(deftest id-test
  (let [base-uri (URI. "http://example.com/")
        context (context/make-context base-uri)]
    (testing "Valid value"
      (let [id-value "id"
            {:keys [warnings result]} (logging/capture-warnings (id context id-value))]
        (is (empty? warnings))
        (is (= (.resolve base-uri id-value) result))))

    (testing "Invalid prefix"
      (validation-error (id context "_:id")))

    (testing "Invalid value"
      (let [{:keys [warnings result]} (logging/capture-warnings (id context "invalid id"))]
        (is (= 1 (count warnings)))
        (is (= base-uri result))))

    (testing "Invalid type"
      (let [{:keys [warnings result]} (logging/capture-warnings (id context 4))]
        (is (= 1 (count warnings)))
        (is (= base-uri result))))))

(deftest common-property-value-type-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Description object type"
      (is (= csvw:Table (common-property-value-type context "Table"))))

    (testing "Compact URI"
      (is (= (URI. "http://purl.org/dc/terms/description") (common-property-value-type context "dc:description"))))

    (testing "Absolute URI"
      (let [uri-str "http://example.com/some/path/to.csv"]
        (is (= (URI. uri-str) (common-property-value-type context uri-str)))))

    (testing "Invalid value"
      (validation-error (common-property-value-type context "not a type or URI")))

    (testing "Array of valid values"
      (is (= [csvw:TableGroup (URI. "http://xmlns.com/foaf/0.1/name") (URI. "http://example.com/test.csv")]
             (common-property-value-type context ["TableGroup" "foaf:name" "http://example.com/test.csv"]))))

    (testing "Array with invalid values"
      (validation-error (common-property-value-type context ["TableGroup" "foaf:name" "not a type or URI"])))

    (testing "Invalid type"
      (validation-error (common-property-value-type context 3)))))