(ns csv2rdf.metadata.types-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.types :refer :all :as types]
            [csv2rdf.metadata.test-common :refer :all]
            [csv2rdf.metadata.validator :refer [invalid? string]]
            [csv2rdf.metadata.context :refer [base-key language-key] :as context]
            [csv2rdf.logging :as logging]
            [csv2rdf.vocabulary :refer [csvw:Table csvw:TableGroup xsd:integer]])
  (:import [java.net URI]))

(defn set-context-language [context language]
  (context/update-from-local-context context {language-key language}))

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

(deftest common-property-type-with-value-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Datatype name"
      (is (= xsd:integer (common-property-type-with-value context "integer"))))

    (testing "Compact URI"
      (is (= (URI. "http://purl.org/dc/terms/description") (common-property-type-with-value context "dc:description"))))

    (testing "Absolute URI"
      (let [uri-str "http://example.com"]
        (is (= (URI. uri-str) (common-property-type-with-value context uri-str)))))

    (testing "Invalid value"
      (validation-error (common-property-type-with-value context "not a type name or URI")))

    (testing "Invalid type"
      (validation-error (common-property-type-with-value context 3)))))

(deftest common-property-type-without-value-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Description object type"
      (is (= csvw:Table (common-property-type-without-value context "Table"))))

    (testing "Compact URI"
      (is (= (URI. "http://purl.org/dc/terms/description") (common-property-type-without-value context "dc:description"))))

    (testing "Absolute URI"
      (let [uri-str "http://example.com/some/path/to.csv"]
        (is (= (URI. uri-str) (common-property-type-without-value context uri-str)))))

    (testing "Invalid value"
      (validation-error (common-property-type-without-value context "not a type or URI")))

    (testing "Array of valid values"
      (is (= [csvw:TableGroup (URI. "http://xmlns.com/foaf/0.1/name") (URI. "http://example.com/test.csv")]
             (common-property-type-without-value context ["TableGroup" "foaf:name" "http://example.com/test.csv"]))))

    (testing "Array with invalid values"
      (validation-error (common-property-type-without-value context ["TableGroup" "foaf:name" "not a type or URI"])))

    (testing "Invalid type"
      (validation-error (common-property-type-without-value context 3)))))

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

(deftest column-reference-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "String"
      (let [ref "col1"
            {:keys [warnings result]} (logging/capture-warnings (column-reference context ref))]
        (is (empty? warnings))
        (is (= [ref] result))))

    (testing "Valid array"
      (let [refs ["col1" "col3"]
            {:keys [warnings result]} (logging/capture-warnings (column-reference context refs))]
        (is (empty? warnings))
        (is (= refs result))))

    (testing "Invalid type"
      (let [{:keys [warnings result]} (logging/capture-warnings (column-reference context {"not" "string or array"}))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))

    (testing "Empty array"
      (let [{:keys [warnings result]} (logging/capture-warnings (column-reference context []))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))

    (testing "Array containing non-strings"
      (let [{:keys [warnings result]} (logging/capture-warnings (column-reference context ["col1" false "col2" 4]))]
        (is (some? (seq warnings)))
        (is (invalid? result))))))

(deftest object-of-test
  (let [context (context/make-context (URI. "http://example"))]
    (testing "Required key"
      (let [v (object-of {:required {:key string}})]
        (testing "which is valid"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {"key" "foo"}))]
            (is (empty? warnings))
            (is (= {:key "foo"} result))))

        (testing "which does not exist"
          (validation-error (v context {"other" 4})))

        (testing "which is invalid"
          (validation-error (v context {"key" ["not" "a" "string"]})))))

    (testing "Optional key"
      (let [v (object-of {:optional {:key string}})]
        (testing "which exists"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {"key" "foo"}))]
            (is (empty? warnings))
            (= {:key "foo"} result)))

        (testing "which does not exist"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {}))]
            (is (empty? warnings))
            (= {} result)))

        (testing "which is invalid"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {"key" ["not" "a" "string"]}))]
            (is (= 1 (count warnings)))
            (is (= {} result))))))

    (testing "Default value"
      (let [default "default"
            v (object-of {:optional {:key string}
                          :defaults {:key default}})]
        (testing "when specified"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {"key" "value"}))]
            (is (empty? warnings))
            (is (= {:key "value"} result))))

        (testing "when not specified"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {}))]
            (is (empty? warnings))
            (is (= {:key default} result))))

        (testing "when invalid"
          (let [{:keys [warnings result]} (logging/capture-warnings (v context {"key" ["not" "a" "string"]}))]
            (is (= 1 (count warnings)))
            (is (= {:key default} result))))))

    (testing "Common properties allowed"
      (let [v (object-of {:required {:key string}
                          :allow-common-properties? true})
            {:keys [warnings result]} (logging/capture-warnings (v context {"key" "value"
                                                                            "dc:description" "description"
                                                                            "http://example.com/concept" 4}))]
        (is (empty? warnings))
        (is (= {:key "value"
                ::types/common-properties {(URI. "http://purl.org/dc/terms/description") {"@value" "description"}
                                           (URI. "http://example.com/concept") 4}}
               result))))

    (testing "Common properties not allowed"
      (let [v (object-of {:required {:key string}
                          :allow-common-properties? false})
            {:keys [warnings result]} (logging/capture-warnings (v context {"key" "value"
                                                                            "dc:description" "description"
                                                                            "http://example.com/concept" 4}))]
        (is (= 2 (count warnings)))
        (is (= {:key "value"} result))))

    (testing "Special key mappings"
      (let [v (object-of {:required {:id string
                                     :type string}})
            {:keys [warnings result]} (logging/capture-warnings (v context {"@id" "id"
                                                                            "@type" "type"}))]
        (is (empty? warnings))
        (is (= {:id "id" :type "type"} result))))))

(deftest object-context-test
  (let [base-uri (URI. "http://example.com/")
        context (context/make-context base-uri)]
    (testing "CSVW namespace"
      (let [{:keys [warnings result]} (logging/capture-warnings (object-context context csvw-ns))]
        (is (empty? warnings))
        (is (= csvw-ns result))))

    (testing "Valid pair"
      (let [base-str "base"
            lang "de"
            {:keys [warnings result]} (logging/capture-warnings (object-context context [csvw-ns {"@base" base-str
                                                                                                  "@language" lang}]))]
        (is (empty? warnings))
        (is (= {base-key (URI. base-str) language-key lang} result))))

    (testing "Pair with invalid CSVW namespace"
      (validation-error (object-context context ["http://invalid" {"@language" "fr"}])))

    (testing "Pair with invalid @base key"
      (validation-error (object-context context [csvw-ns {"@base" "not a URI"}])))

    (testing "Pair with empty object"
      (validation-error (object-context context [csvw-ns {}])))

    (testing "Pair with invalid language code"
      (let [base-str "base"
            {:keys [warnings result]} (logging/capture-warnings (object-context context [csvw-ns {"@base" base-str "@language" "invalid language code"}]))]
        (is (= 1 (count warnings)))
        (is (= {base-key (URI. base-str) language-key nil} result))))))

(deftest contextual-object-test
  (let [base-uri (URI. "http://example.com")
        context (context/make-context base-uri)]
    (testing "Context as string"
      (let [v (contextual-object true (object-of {:required {:s string}}))
            {:keys [warnings result]} (logging/capture-warnings (v context {"@context" csvw-ns "s" "foo"}))]
        (is (empty? warnings))
        (is (= {:s "foo"} result))))

    (testing "Context as pair"
      (let [context-lang "fr"
            v (contextual-object true (object-of {:required {:title natural-language}}))
            obj-context [csvw-ns {"@language" context-lang}]
            {:keys [warnings result]} (logging/capture-warnings (v context {"@context" obj-context "title" "le title"}))]
        (is (empty? warnings))
        (is (= {:title {context-lang ["le title"]}} result))))

    (testing "Context missing when required"
      (let [v (contextual-object true (object-of {:optional {:s string}}))]
        (validation-error (v context {"s" "foo"}))))

    (testing "Context missing when not required"
      (let [v (contextual-object false (object-of {:optional {:s string}}))
            {:keys [warnings result]} (logging/capture-warnings (v context {"s" "foo"}))]
        (is (empty? warnings))
        (is (= {:s "foo"} result))))

    (testing "Invalid context"
      (let [v (contextual-object false (object-of {:optional {:s string}}))]
        (validation-error (v context {"@context" ["http://invalid" {"@langugage" "de"}] "s" "foo"}))))

    (testing "Invalid type"
      (let [v (contextual-object false {:required {:s string}})
            {:keys [warnings result]} (logging/capture-warnings (v context ["not" "an" "object"]))]
        (is (= 1 (count warnings)))
        (is (invalid? result))))))