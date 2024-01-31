(ns csv2rdf.metadata-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata :refer :all]
            [csv2rdf.source :as source]
            [csv2rdf.metadata.properties :as properties]
            [csv2rdf.test-common :refer [remove-ns-kws]]
            [clojure.java.io :as io])
  (:import [java.net URI]))

(deftest parse-metadata-json-test
  (testing "Creates parent references"
    (testing "for table group"
      (let [json {"@context" "http://www.w3.org/ns/csvw"
                  "dialect" {"quoteChar" "{"}
                  "tableSchema" {"columns" [{"name" "col1"}
                                            {"name" "col2"}]}
                  "tables" [{"url" "http://example.com/table.csv"}]}            
            parsed (parse-metadata-json (URI. "http://example.com/metadata.json") json nil)
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
            parsed (parse-metadata-json (URI. "http://example.com/metadata.json") json nil)
            table (get-in parsed [:tables 0])
            dialect (properties/dialect table)
            schema (properties/table-schema table)]
        (is (= {:quoteChar \{} dialect))
        (is (= {:columns [{:name "col1"} {:name "col2"}]} (remove-ns-kws schema)))))))

(deftest overriding-metadata-test
  (let [test-dir (io/file "w3c-csvw/tests/")]
    (testing "table group"
      (let [tabular-source (io/file test-dir "test007.csv")
            metadata-source (io/file test-dir "test008.json")
            m (overriding-metadata tabular-source metadata-source)
            doc (source/get-json m)]
        (is (= (source/get-json metadata-source) doc))))

    (testing "table"
      (let [tabular-source (io/file test-dir "test007.csv")
            metadata-source (io/file test-dir "test022-metadata.json")
            m (overriding-metadata tabular-source metadata-source)
            doc (source/get-json m)]
        (println m)
        (is (= (get doc "url") (str (source/->uri tabular-source))))))))
