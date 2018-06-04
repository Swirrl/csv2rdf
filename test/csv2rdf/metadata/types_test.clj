(ns csv2rdf.metadata.types-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.types :refer :all :as types]
            [csv2rdf.metadata.test-common :refer :all]
            [csv2rdf.test-common :refer [suppress-test-logging]]
            [csv2rdf.metadata.validator :refer [invalid? string]]
            [csv2rdf.metadata.context :refer [base-key language-key] :as context]
            [csv2rdf.logging :as logging]
            [csv2rdf.vocabulary :refer [csvw:Table csvw:TableGroup xsd:integer]])
  (:import [java.net URI]))

(defn set-context-language [context language]
  (context/update-from-local-context context {language-key language}))

(deftest non-negative-test
  (testing "Valid value"
    (validates-as 0 (non-negative test-context 0)))

  (testing "Invalid value"
    (warns-invalid (non-negative test-context -3)))

  (testing "Invalid type"
    (warns-invalid (non-negative test-context "not a number"))))

(deftest language-code-test
  (testing "Valid language code"
    (let [code "en-gb"]
      (validates-as code (language-code test-context code))))

  (testing "Invalid language code"
    (warns-invalid (language-code test-context "not a valid language code"))))

(deftest natural-language-test
  (testing "string"
    (testing "default langauge"
      (let [value "title"]
        (validates-as {"und" [value]} (natural-language test-context value))))

    (testing "specified language"
      (let [value "der title"
            language "de"
            context (set-context-language test-context language)]
        (validates-as {language [value]} (natural-language context value)))))

  (testing "array"
    (testing "default langauge"
      (testing "all values valid"
        (let [values ["title1" "title2"]]
          (validates-as {"und" values} (natural-language test-context values))))

      (testing "with invalid values"
        (let [{:keys [warnings result]} (logging/capture-warnings (natural-language test-context ["title1" 3 {} "title2" []]))]
          (is (= 3 (count warnings)))
          (is (= {"und" ["title1" "title2"]} result)))))

    (testing "specified language"
      (let [language "en"
            context (set-context-language test-context language)]
        (testing "all values valid"
          (let [values ["title1" "title2"]]
            (validates-as {language values} (natural-language context values))))

        (testing "with invalid values"
          (let [{:keys [warnings result]} (logging/capture-warnings (natural-language context ["title1" 1 "title2" false [] {}]))]
            (is (= 4 (count warnings)))
            (is (= {language ["title1" "title2"]} result)))))))

  (testing "object"
    (testing "valid values"
      (validates-as {"en" ["title"]
                     "fr" ["title un" "title deux"]}
                    (natural-language test-context {"en" "title" "fr" ["title un" "title deux"]})))

    (testing "with invalid values"
      (warns-with {"en" ["title"]
                   "fr" []
                   "de" []
                   "es" []}
                  (natural-language test-context {"en" "title" "fr" 4 "de" {} "es" false})))

    (testing "with invalid keys"
      (warns-with {"en" ["title"]
                   "de" ["title eins" "title zwei"]}
                  (natural-language test-context {"en" "title" 4 "?" "de" ["title eins" "title zwei"] false "!!!"}))))

  (testing "Invalid type"
    (testing "Default language"
      (warns-with {"und" []} (natural-language test-context 4)))

    (testing "Specified langauge"
      (let [language "en"
            context (set-context-language test-context language)]
        (warns-with {language []} (natural-language context 3))))))

(deftest link-property-test
  (let [base-uri (URI. "http://example.com/")
        context (context/make-context base-uri)]
    (testing "Valid value"
      (let [rel "file.csv"]
        (validates-as (.resolve base-uri rel) (link-property context rel))))

    (testing "Invalid value"
      (warns-with base-uri (link-property context "not a URI")))

    (testing "Invalid type"
      (warns-with base-uri (link-property context 43)))))

(deftest id-test
  (let [base-uri (URI. "http://example.com/")
        context (context/make-context base-uri)]
    (testing "Valid value"
      (let [id-value "id"]
        (validates-as (.resolve base-uri id-value) (id context id-value))))

    (testing "Invalid prefix"
      (validation-error (id context "_:id")))

    (testing "Invalid value"
      (warns-with base-uri (id context "invalid id")))

    (testing "Invalid type"
      (warns-with base-uri (id context 4)))))

(deftest common-property-key-test
  (testing "Compact URI"
    (validates-as (URI. "http://purl.org/dc/terms/description") (common-property-key test-context "dc:description")))

  (testing "Absolute URI"
    (let [uri-str "http://example.com/some/path#f"]
      (validates-as (URI. uri-str) (common-property-key test-context uri-str))))

  (testing "Invalid key"
    (warns-invalid (common-property-key test-context "not a URI")))

  (testing "Invalid type"
    (warns-invalid (common-property-key test-context 4))))

(deftest common-property-type-with-value-test
  (testing "Datatype name"
    (validates-as xsd:integer (common-property-type-with-value test-context "integer")))

  (testing "Compact URI"
    (validates-as (URI. "http://purl.org/dc/terms/description") (common-property-type-with-value test-context "dc:description")))

  (testing "Absolute URI"
    (let [uri-str "http://example.com"]
      (validates-as (URI. uri-str) (common-property-type-with-value test-context uri-str))))

  (testing "Invalid value"
    (validation-error (common-property-type-with-value test-context "not a type name or URI")))

  (testing "Invalid type"
    (validation-error (common-property-type-with-value test-context 3))))

(deftest common-property-type-without-value-test
  (testing "Description object type"
    (validates-as csvw:Table (common-property-type-without-value test-context "Table")))

  (testing "Compact URI"
    (validates-as (URI. "http://purl.org/dc/terms/description") (common-property-type-without-value test-context "dc:description")))

  (testing "Absolute URI"
    (let [uri-str "http://example.com/some/path/to.csv"]
      (validates-as (URI. uri-str) (common-property-type-without-value test-context uri-str))))

  (testing "Invalid value"
    (validation-error (common-property-type-without-value test-context "not a type or URI")))

  (testing "Array of valid values"
    (is (validates-as
          [csvw:TableGroup (URI. "http://xmlns.com/foaf/0.1/name") (URI. "http://example.com/test.csv")]
          (common-property-type-without-value test-context ["TableGroup" "foaf:name" "http://example.com/test.csv"]))))

  (testing "Array with invalid values"
    (validation-error (common-property-type-without-value test-context ["TableGroup" "foaf:name" "not a type or URI"])))

  (testing "Invalid type"
    (validation-error (common-property-type-without-value test-context 3))))

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
          (let [obj {"@value" "value"}]
            (validates-as obj (common-property-value context obj))))

        (testing "Containing @value and @language keys"
          (let [obj {"@value" "bonjour"
                     "@language" "fr"}]
            (validates-as obj (common-property-value context obj))))

        ;;NOTE: metadata spec requires @type keys to be unchanged and expanded in the CSVW process
        ;;easier to expand here since valid values for @type depend on the existence of the @value key
        ;;TODO: change to be in accordance with the spec
        (testing "@type when specified with @value"
          (validates-as
            {"@value" "4"
             "@type"  xsd:integer}
            (common-property-value context {"@value" "4" "@type" "integer"})))

        (testing "@type when specified without @value"
          (validates-as {"@type" csvw:Table} (common-property-value context {"@type" "Table"})))

        (testing "With @id property"
          (testing "Absolute"
            (validates-as {"@id" (URI. "http://xmlns.com/foaf/0.1/name")} (common-property-value context {"@id" "foaf:name"})))

          (testing "Relative"
            (let [id-str "SomeId"]
              (validates-as {"@id" (.resolve base-uri id-str)} (common-property-value context {"@id" id-str})))))

        (testing "Common property keys"
          (validates-as {(URI. "http://purl.org/dc/terms/description") {"@value" "some description"}
                         (URI. "http://something")                     4}
                        (common-property-value context {"dc:description"   "some description"
                                                           "http://something" 4}))))

      (testing "Arrays"
        (validates-as [{(URI. "http://purl.org/dc/terms/description") {"@value" "Description"}}
                       {"@value" "some string"}
                       {"@value" "foux" "@language" "fr"}]
                      (common-property-value context [{"dc:description" "Description"}
                                                      "some string"
                                                      {"@value" "foux" "@language" "fr"}])))

      (testing "Strings"
        (testing "No default langauge"
          (let [s "some string"]
            (validates-as {"@value" s} (common-property-value context s))))

        (testing "With default language"
          (let [s "enhorabuena"
                lang "es"]
            (validates-as {"@value" s "@language" lang} (common-property-value (set-context-language context lang) s)))))

      (testing "Atomic types"
        (validates-as true (validate-common-property-value context true))
        (validates-as 4 (validate-common-property-value context 4))))))

(deftest column-reference-test
  (testing "String"
    (let [ref "col1"]
      (validates-as [ref] (column-reference test-context ref))))

  (testing "Valid array"
    (let [refs ["col1" "col3"]]
      (validates-as refs (column-reference test-context refs))))

  (testing "Invalid type"
    (warns-invalid (column-reference test-context {"not" "string or array"})))

  (testing "Empty array"
    (warns-invalid (column-reference test-context [])))

  (testing "Array containing non-strings"
    (warns-invalid (column-reference test-context ["col1" false "col2" 4]))))

(deftest object-of-test
  (testing "Required key"
    (let [v (object-of {:required {:key string}})]
      (testing "which is valid"
        (validates-as {:key "foo"} (v test-context {"key" "foo"})))

      (testing "which does not exist"
        (validation-error (v test-context {"other" 4})))

      (testing "which is invalid"
        (validation-error (v test-context {"key" ["not" "a" "string"]})))))

  (testing "Optional key"
    (let [v (object-of {:optional {:key string}})]
      (testing "which exists"
        (validates-as {:key "foo"} (v test-context {"key" "foo"})))

      (testing "which does not exist"
        (validates-as {} (v test-context {})))

      (testing "which is invalid"
        (warns-with {} (v test-context {"key" ["not" "a" "string"]})))))

  (testing "Default value"
    (let [default "default"
          v (object-of {:optional {:key string}
                        :defaults {:key default}})]
      (testing "when specified"
        (validates-as {:key "value"} (v test-context {"key" "value"})))

      (testing "when not specified"
        (validates-as {:key default} (v test-context {})))

      (testing "when invalid"
        (warns-with {:key default} (v test-context {"key" ["not" "a" "string"]})))))

  (testing "Common properties allowed"
    (let [v (object-of {:required                 {:key string}
                        :allow-common-properties? true})]
      (validates-as
        {:key                      "value"
         ::types/common-properties {(URI. "http://purl.org/dc/terms/description") {"@value" "description"}
                                    (URI. "http://example.com/concept")           4}}
        (v test-context {"key"                        "value"
                         "dc:description"             "description"
                         "http://example.com/concept" 4}))))

  (testing "Common properties not allowed"
    (let [v (object-of {:required                 {:key string}
                        :allow-common-properties? false})]
      (warns-with {:key "value"} (v test-context {"key"                        "value"
                                                  "dc:description"             "description"
                                                  "http://example.com/concept" 4}))))

  (testing "Special key mappings"
    (let [v (object-of {:required {:id   string
                                   :type string}})]
      (validates-as {:id "id" :type "type"}
                    (v test-context {"@id"   "id"
                                     "@type" "type"})))))

(deftest object-context-test
  (let [base-uri (URI. "http://example.com/")
        context (context/make-context base-uri)]
    (testing "CSVW namespace"
      (validates-as csvw-ns (object-context context csvw-ns)))

    (testing "Valid pair"
      (let [base-str "base"
            lang "de"]
        (validates-as {base-key (URI. base-str) language-key lang}
                      (object-context context [csvw-ns {"@base" base-str
                                                        "@language" lang}]))))

    (testing "Pair with invalid CSVW namespace"
      (validation-error (object-context context ["http://invalid" {"@language" "fr"}])))

    (testing "Pair with invalid @base key"
      (validation-error (object-context context [csvw-ns {"@base" "not a URI"}])))

    (testing "Pair with empty object"
      (validation-error (object-context context [csvw-ns {}])))

    (testing "Pair with extra properties"
      (validation-error (object-context context [csvw-ns {"@language" "fr"
                                                          "dc:title" "not allowed"}])))

    (testing "Pair with invalid language code"
      (let [base-str "base"
            {:keys [warnings result]} (logging/capture-warnings (object-context context [csvw-ns {"@base" base-str "@language" "invalid language code"}]))]
        (is (= 1 (count warnings)))
        (is (= {base-key (URI. base-str)} result))))))

(deftest contextual-object-test
  (let [base-uri (URI. "http://example.com")
        context (context/make-context base-uri)]
    (testing "Context as string"
      (let [v (contextual-object true (object-of {:required {:s string}}))]
        (validates-as {:s "foo"} (v context {"@context" csvw-ns "s" "foo"}))))

    (testing "Context as pair"
      (let [context-lang "fr"
            v (contextual-object true (object-of {:required {:title natural-language}}))
            obj-context [csvw-ns {"@language" context-lang}]]
        (validates-as {:title {context-lang ["le title"]}}
                      (v context {"@context" obj-context "title" "le title"}))))

    (testing "Context missing when required"
      (let [v (contextual-object true (object-of {:optional {:s string}}))]
        (validation-error (v context {"s" "foo"}))))

    (testing "Context missing when not required"
      (let [v (contextual-object false (object-of {:optional {:s string}}))]
        (validates-as {:s "foo"} (v context {"s" "foo"}))))

    (testing "Invalid context"
      (let [v (contextual-object false (object-of {:optional {:s string}}))]
        (validation-error (v context {"@context" ["http://invalid" {"@langugage" "de"}] "s" "foo"}))))

    (testing "Invalid type"
      (let [v (contextual-object false {:required {:s string}})]
        (warns-invalid (v context ["not" "an" "object"]))))))

(use-fixtures :each suppress-test-logging)