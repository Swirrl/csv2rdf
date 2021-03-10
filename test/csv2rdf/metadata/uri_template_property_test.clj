(ns csv2rdf.metadata.uri-template-property-test
  (:require [clojure.test :as t]
            [csv2rdf.metadata.uri-template-property :refer :all]
            [csv2rdf.uri-template :as template])
  (:import [java.net URI]))

(t/deftest resolve-uri-template-property-test
  (t/are [template-str bindings table-uri-str expected-str]
         (= (URI. expected-str) (let [template (template/parse-template template-str)
                                      table {:url (some-> table-uri-str (URI.))}]
                                  (resolve-uri-template-property template bindings table)))
    ;; absolute template URI, no table URI
    "http://example.com/{x}" {"x" "value"} nil "http://example.com/value"

    ;; absolute template URI, no table URI, value contains spaces
    "http://example.com/{x}" {"x" "foo bar"} nil "http://example.com/foo%20bar"

    ;; compact template URI, no table URI
    "dcterms:issued" {} nil "http://purl.org/dc/terms/issued"

    ;; relative template URI, no table URI
    "/some/path/{x}" {"x" "file.csv"} nil "/some/path/file.csv"

    ;; absolute template URI, table URI
    "http://example.com/{x}" {"x" "value"} "file:///path/to/metadata.json" "http://example.com/value"

    ;; absolute template URI, table URI, value contains spaces
    "http://example.com/{x}" {"x" "foo bar"} "http://example.com/metadata.json" "http://example.com/foo%20bar"

    ;; compact template URI, table URI
    "dcterms:issued" {} "http://example.com/metadata.json" "http://purl.org/dc/terms/issued"

    ;; relative template URI, table URI
    "/some/path/{x}" {"x" "file.xslx"} "http://example.com/path/to/metadata.json" "http://example.com/some/path/file.xslx"
    ))