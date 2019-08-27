(ns csv2rdf.metadata-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata :refer :all]
            [csv2rdf.source :as source]
            [csv2rdf.metadata.properties :as properties]
            [csv2rdf.test-common :refer [remove-ns-kws]])
  (:import [java.net URI]))

(deftest parse-metadata-json-test
  (testing "Creates parent references"
    (testing "for table group"
      (let [json {"@context" "http://www.w3.org/ns/csvw"
                  "dialect" {"quoteChar" "{"}
                  "tableSchema" {"columns" [{"name" "col1"}
                                            {"name" "col2"}]}
                  "tables" [{"url" "http://example.com/table.csv"}]}            
            parsed (parse-metadata-json (URI. "http://example.com/metadata.json") json)
            table (get-in parsed [:tables 0])
            dialect (properties/dialect table)
            schema (properties/table-schema table)]
        (is (= {:quoteChar \{} dialect))
        (is (= {:columns [{:name "col1"} {:name "col2"}]} (remove-ns-kws schema)))))

    (testing "for table"
      (let [json {"@context" "http://www.w3.org/ns/csvw"
                  "url" "http://example.com/table.csv"
                  "dialect" {"quoteChar" "{"}
                  "tableSchema" {"columns" [{"name" "col1"}
                                            {"name" "col2"}]}}
            parsed (parse-metadata-json (URI. "http://example.com/metadata.json") json)
            table (get-in parsed [:tables 0])
            dialect (properties/dialect table)
            schema (properties/table-schema table)]
        (is (= {:quoteChar \{} dialect))
        (is (= {:columns [{:name "col1"} {:name "col2"}]} (remove-ns-kws schema)))))))
