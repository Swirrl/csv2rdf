(ns csv2rdf.metadata.table-group-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.table-group :refer :all]
            [csv2rdf.metadata.test-common :refer [validation-error validates-as test-context]]
            [csv2rdf.metadata.validator :refer [invalid?]]
            [csv2rdf.logging :as logging])
  (:import [java.net URI]))

(deftest tables-test
  (testing "Valid tables"
    (let [url1-str "http://table1"
          url2-str "http://table2"]
      (validates-as [{:url (URI. url1-str)}
                     {:url (URI. url2-str)}]
                    (tables test-context [{"url" url1-str} {"url" url2-str}]))))

  (testing "Empty tables"
    (validation-error (tables test-context [])))

  (testing "Invalid type"
    (let [{:keys [warnings]} (logging/capture-warnings
                               (validation-error (tables test-context "not an array")))]
      (is (empty? warnings)))))

(deftest validate-foreign-key-references-test
  (testing "Valid table reference"
    (let [tg {:tables [{:url (URI. "http://tables/1")
                        :tableSchema {:columns [{:name "col1"} {:name "col2"}]}}
                       {:url (URI. "http://tables/2")
                        :tableSchema {:columns [{:name "ref"}]
                                      :foreignKeys [{:columnReference ["ref"]
                                                     :reference {:resource (URI. "http://tables/1")
                                                                 :columnReference ["col1"]}}]}}]}]
      (validates-as tg (validate-foreign-key-references test-context tg))))

  (testing "Invalid table reference"
    (let [tg {:tables [{:url (URI. "http://tables/1")
                        :tableSchema {:columns [{:name "col1"} {:name "col2"}]}}
                       {:url (URI. "http://tables/2")
                        :tableSchema {:columns [{:name "ref"}]
                                      :foreignKeys [{:columnReference ["ref"]
                                                     :reference {:resource (URI. "http://invalid-table")
                                                                 :columnReference ["col1"]}}]}}]}]
      (validation-error (validate-foreign-key-references test-context tg))))

  (testing "Multiple matching table references"
    (let [tg {:tables [{:url (URI. "http://tables/1")
                        :tableSchema {:columns [{:name "col1"} {:name "col2"}]}}
                       {:url (URI. "http://tables/2")
                        :tableSchema {:columns [{:name "ref"}]
                                      :foreignKeys [{:columnReference ["ref"]
                                                     :reference {:resource (URI. "http://tables/1")
                                                                 :columnReference ["col1"]}}]}}
                       {:url (URI. "http://tables/1")
                        :tableSchema {:columns [{:name "col1"}]}}]}]
      (validation-error (validate-foreign-key-references test-context tg))))

  (testing "Valid schema reference"
    (let [tg {:tables [{:url (URI. "http://tables/1")
                        :tableSchema {:columns [{:name "col1"} {:name "col2"}]
                                      :id (URI. "http://schemas/1")}}
                       {:url (URI. "http://tables/2")
                        :tableSchema {:columns [{:name "ref"}]
                                      :foreignKeys [{:columnReference ["ref"]
                                                     :reference {:schemaReference (URI. "http://schemas/1")
                                                                 :columnReference ["col1"]}}]}}]}]
      (validates-as tg (validate-foreign-key-references test-context tg))))

  (testing "Invalid schema reference"
    (let [tg {:tables [{:url (URI. "http://tables/1")
                        :tableSchema {:columns [{:name "col1"} {:name "col2"}]
                                      :id (URI. "http://schemas/1")}}
                       {:url (URI. "http://tables/2")
                        :tableSchema {:columns [{:name "ref"}]
                                      :foreignKeys [{:columnReference ["ref"]
                                                     :reference {:schemaReference (URI. "http://invalid-schema")
                                                                 :columnReference ["col1"]}}]}}]}]
      (validation-error (validate-foreign-key-references test-context tg))))

  (testing "Multiple matching schema references"
    (let [tg {:tables [{:url (URI. "http://tables/1")
                        :tableSchema {:columns [{:name "col1"} {:name "col2"}]
                                      :id (URI. "http://schemas/1")}}
                       {:url (URI. "http://tables/2")
                        :tableSchema {:columns [{:name "ref"}]
                                      :foreignKeys [{:columnReference ["ref"]
                                                     :reference {:schemaReference (URI. "http://schemas/1")
                                                                 :columnReference ["col1"]}}]}}
                       {:url (URI. "http://tables/3")
                        :tableSchema {:columns [{:name "test"}]
                                      :id (URI. "http://schemas/1")}}]}]
      (validation-error (validate-foreign-key-references test-context tg))))

  (testing "Invalid column reference"
    (let [tg {:tables [{:url (URI. "http://tables/1")
                        :tableSchema {:columns [{:name "col1"} {:name "col2"}]}}
                       {:url (URI. "http://tables/2")
                        :tableSchema {:columns [{:name "ref"}]
                                      :foreignKeys [{:columnReference ["ref"]
                                                     :reference {:resource (URI. "http://tables/1")
                                                                 :columnReference ["missing-col"]}}]}}]}]
      (validation-error (validate-foreign-key-references test-context tg)))))
