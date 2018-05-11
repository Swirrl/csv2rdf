(ns csv2rdf.metadata.types-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.types :refer :all]
            [csv2rdf.metadata.test-common :refer :all]
            [csv2rdf.metadata.validator :refer [invalid?]]
            [csv2rdf.metadata.context :as context]
            [csv2rdf.logging :as logging]
            [csv2rdf.vocabulary :refer [csvw:Table csvw:TableGroup xsd:integer]])
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

(deftest common-property-key-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Compact URI"
      (let [{:keys [warnings result]} (logging/capture-warnings (common-property-key context "dc:description"))]
        (is (empty? warnings))
        (is (= (URI. "http://purl.org/dc/terms/description") result))))

    (testing "Absolute URI"
      (let [uri-str "http://example.com/some/path#f"
            {:keys [warnings result]} (logging/capture-warnings (common-property-key context uri-str))]
        (is (empty? warnings))
        (is (= (URI. uri-str) result))))

    (testing "Invalid key"
      (let [{:keys [warnings result]} (logging/capture-warnings (common-property-key context "not a URI"))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))

    (testing "Invalid type"
      (let [{:keys [warnings result]} (logging/capture-warnings (common-property-key context 4))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))

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

(deftest common-property-value-test
  (let [base-uri (URI. "http://example.com/")
        context (context/make-context base-uri)]
    (testing "Invalid values"
      (testing "Invalid type for @value"
        (validation-error (common-property-value context {"@value" {"not" "string, number or boolean"}})))

      (testing "Contains @value with both @type and @language keys"
        (validation-error (common-property-value context {"@value" "something"
                                                          "@language" "en"
                                                          "@type" "http://some-type"})))

      (testing "Contains @value with keys other than @type or @language"
        (validation-error (common-property-value context {"@value" "something"
                                                          "forbidden" "key name"})))

      (testing "Invalid @type when specified alongside @value"
        (validation-error (common-property-value context {"@value" "4"
                                                          "@type" "Not a built-in type, prefixed name or absolute URI"})))

      (testing "Invalid @language"
        (validation-error (common-property-value context {"@value" "foo"
                                                          "@language" "invalid language code"})))

      (testing "Invalid @type when no @value specified"
        (validation-error (common-property-value context {"@type" "Not a description object type, prefixed name or absolute URI"})))

      (testing "Invalid @id"
        (validation-error (common-property-value context {"@id" "_:someid"})))

      (testing "@language key on object without @value"
        (validation-error (common-property-value context {"@language" "en"})))

      (testing "Invalid key prefix"
        (validation-error (common-property-value context {"@forbidden" "keys other than @id @type @langauge @value must not start with @"})))

      (testing "Invalid object within array"
        (validation-error (common-property-value context [{"@id" "_:invalidid"}
                                                          {"dc:description" "ok object"}])))

      (testing "Invalid object within object"
        (validation-error (common-property-value context {"http://something" {"@invalidkey" "starts with @"}}))))

    (testing "Valid values"
      (testing "Objects"
        (testing "Containing @value key"
          (let [obj {"@value" "bonjour"
                     "@language" "fr"}]
            (is (= obj (common-property-value context obj)))))

        ;;NOTE: metadata spec requires @type keys to be unchanged and expanded in the CSVW process
        ;;easier to expand here since valid values for @type depend on the existence of the @value key
        ;;TODO: change to be in accordance with the spec
        (testing "@type when specified with @value"
          (is (= {"@value" "4"
                  "@type" xsd:integer}
                 (common-property-value context {"@value" "4" "@type" "integer"}))))

        (testing "@type when specified without @value"
          (is (= {"@type" csvw:Table} (common-property-value context {"@type" "Table"}))))

        (testing "With @id property"
          (testing "Absolute"
            (is (= {"@id" (URI. "http://xmlns.com/foaf/0.1/name")} (common-property-value context {"@id" "foaf:name"}))))

          (testing "Relative"
            (let [id-str "SomeId"]
              (is (= {"@id" (.resolve base-uri id-str)} (common-property-value context {"@id" id-str}))))))

        (testing "Common property keys"
          (is (= {(URI. "http://purl.org/dc/terms/description") {"@value" "some description"}
                  (URI. "http://something") 4}
                 (common-property-value context {"dc:description" "some description"
                                                 "http://something" 4})))))

      (testing "Arrays"
        (is (= [{(URI. "http://purl.org/dc/terms/description") {"@value" "Description"}}
                {"@value" "some string"}
                {"@value" "foux" "@language" "fr"}]
               (common-property-value context [{"dc:description" "Description"}
                                               "some string"
                                               {"@value" "foux" "@language" "fr"}]))))

      (testing "Strings"
        (testing "No default langauge"
          (let [s "some string"]
            (is (= {"@value" s} (common-property-value context s)))))

        (testing "With default language"
          (let [s "enhorabuena"
                lang "es"]
            (is (= {"@value" s "@language" lang} (common-property-value (set-context-language context lang) s))))))

      (testing "Atomic types"
        (is (= true (validate-common-property-value context true)))
        (is (= 4 (validate-common-property-value context 4)))))))