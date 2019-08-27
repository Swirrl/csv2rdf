(ns csv2rdf.csvw-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [csv2rdf.csvw-test.impl :refer :all]
            [csv2rdf.csvw :as csvw]
            [csv2rdf.http :as http]
            [grafter-2.rdf4j.io :as gio])
  (:import [java.net URI URL]))

(deftest
  ^
  {:title "Simple table", :description "The simplest possible table without metadata"}
  test001
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test001.csv")
                       {:body (io/file "w3c-csvw/tests/test001.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test001.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test001.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test001.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "Identifier references",
   :description "A table with entity identifiers and references to other entities without metadata"}
  test005
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test005.csv")
                       {:body (io/file "w3c-csvw/tests/test005.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test005.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test005.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test005.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "No identifiers",
   :description "Records contain two entities with relationships which are duplicated without metadata"}
  test006
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test006.csv")
                       {:body (io/file "w3c-csvw/tests/test006.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test006.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test006.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test006.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "Joined table with unique identifiers", :description "Joined data with identified records without metadata"}
  test007
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test007.csv")
                       {:body (io/file "w3c-csvw/tests/test007.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test007.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test007.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test007.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "Microsyntax - internal field separator",
   :description "One field has comma-separated values without metadata"}
  test008
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test008.csv")
                       {:body (io/file "w3c-csvw/tests/test008.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test008.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test008.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test008.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "Microsyntax - formatted time", :description "Field with parseable human formatted time without metadata"}
  test009
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test009.csv")
                       {:body (io/file "w3c-csvw/tests/test009.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test009.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test009.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test009.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "Country-codes-and-names example", :description "Country-codes-and-names example"}
  test010
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test010.csv")
                       {:body (io/file "w3c-csvw/tests/test010.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test010.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test010.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test010.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops example with metadata",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test011
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/test011/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test011/tree-ops.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test011/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops example with directory metadata",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test012
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/test012/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test012/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test012/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test012/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops example from user metadata",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test013
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test013-user-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test013-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test013-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test013.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops example with linked metadata",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test014
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/test014/tree-ops.csv"),
                        :headers
                        {"Content-Type" "text/csv; charset=UTF-8",
                         "Link" "<linked-metadata.json>; rel=\"describedby\"; type=\"application/csvm+json\""}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test014/linked-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test014/linked-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test014/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops example with user and directory metadata",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test015
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/test015/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test015/user-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test015/user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test015/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test015/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test015/user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test015/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops example with linked and directory metadata",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test016
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/test016/tree-ops.csv"),
                        :headers
                        {"Content-Type" "text/csv; charset=UTF-8",
                         "Link" "<linked-metadata.json>; rel=\"describedby\"; type=\"application/csvm+json\""}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test016/linked-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test016/linked-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test016/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test016/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test016/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops example with file and directory metadata",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test017
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/test017/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test017/tree-ops.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test017/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test017/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test017/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops example with user, file and directory metadata",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test018
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/test018/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test018/user-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test018/user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test018/tree-ops.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test018/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test018/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test018/user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test018/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: header=false",
   :description
   "If `true`, sets the `header row count` flag to 1, and if `false` to 0, unless `headerRowCount` is provided, in which case the value provided for the `header` property is ignored."}
  test023
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test023-user-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test023-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test023-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test023.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops minimal output",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test027
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test027-user-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test027-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test027-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test027.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "countries.csv example",
   :description "If no metadata is supplied or found, processors MUST use embedded metadata."}
  test028
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test028.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/countries.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "countries.csv minimal",
   :description "If no metadata is supplied or found, processors MUST use embedded metadata."}
  test029
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test029.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/countries.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "countries.json example", :description "countries.json from metadata"}
  test030
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/countries.json")
                       {:body (io/file "w3c-csvw/tests/countries.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/countries.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test030.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/countries.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "countries.json example minimal output", :description "countries.json from metadata minimal output"}
  test031
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/countries.json")
                       {:body (io/file "w3c-csvw/tests/countries.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/countries.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test031.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/countries.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "events-listing.csv example",
   :description "events-listing example from metadata, virtual columns and multiple subjects per row"}
  test032
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test032/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test032/csv-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test032/events-listing.csv")
                       {:body (io/file "w3c-csvw/tests/test032/events-listing.csv"),
                        :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test032/csv-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test032/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test032/csv-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "events-listing.csv minimal output",
   :description "events-listing example from metadata, virtual columns and multiple subjects per row; minimal output"}
  test033
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test033/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test033/csv-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test033/events-listing.csv")
                       {:body (io/file "w3c-csvw/tests/test033/events-listing.csv"),
                        :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test033/csv-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test033/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test033/csv-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "roles example",
   :description
   "Public Sector Roles example with referenced schemas. Validation fails because organization.csv intentionally contains an invalid reference."}
  test034
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/data/organizations.csv")
                       {:body (io/file "w3c-csvw/tests/test034/gov.uk/data/organizations.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test034/csv-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/senior-roles.csv")
                       {:body (io/file "w3c-csvw/tests/test034/senior-roles.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/schema/organizations.json")
                       {:body (io/file "w3c-csvw/tests/test034/gov.uk/schema/organizations.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/junior-roles.csv")
                       {:body (io/file "w3c-csvw/tests/test034/junior-roles.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/schema/senior-roles.json")
                       {:body (io/file "w3c-csvw/tests/test034/gov.uk/schema/senior-roles.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/schema/professions.json")
                       {:body (io/file "w3c-csvw/tests/test034/gov.uk/schema/professions.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/data/professions.csv")
                       {:body (io/file "w3c-csvw/tests/test034/gov.uk/data/professions.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/schema/junior-roles.json")
                       {:body (io/file "w3c-csvw/tests/test034/gov.uk/schema/junior-roles.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test034/csv-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test034/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test034/csv-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "roles minimal",
   :description
   "Public Sector Roles example with referenced schemas; minimal output. Validation fails because organization.csv intentionally contains an invalid reference."}
  test035
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/data/professions.csv")
                       {:body (io/file "w3c-csvw/tests/test035/gov.uk/data/professions.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/senior-roles.csv")
                       {:body (io/file "w3c-csvw/tests/test035/senior-roles.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/schema/professions.json")
                       {:body (io/file "w3c-csvw/tests/test035/gov.uk/schema/professions.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/data/organizations.csv")
                       {:body (io/file "w3c-csvw/tests/test035/gov.uk/data/organizations.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/schema/junior-roles.json")
                       {:body (io/file "w3c-csvw/tests/test035/gov.uk/schema/junior-roles.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/junior-roles.csv")
                       {:body (io/file "w3c-csvw/tests/test035/junior-roles.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/schema/organizations.json")
                       {:body (io/file "w3c-csvw/tests/test035/gov.uk/schema/organizations.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/schema/senior-roles.json")
                       {:body (io/file "w3c-csvw/tests/test035/gov.uk/schema/senior-roles.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test035/csv-metadata.json"), :headers {}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test035/csv-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test035/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test035/csv-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops-ext example", :description "tree-ops extended example"}
  test036
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test036/tree-ops-ext.csv")
                       {:body (io/file "w3c-csvw/tests/test036/tree-ops-ext.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test036/tree-ops-ext.csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test036/tree-ops-ext.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test036/tree-ops-ext.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test036/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test036/tree-ops-ext.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops-ext minimal", :description "tree-ops extended example; minimal output"}
  test037
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test037/tree-ops-ext.csv")
                       {:body (io/file "w3c-csvw/tests/test037/tree-ops-ext.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test037/tree-ops-ext.csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test037/tree-ops-ext.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test037/tree-ops-ext.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test037/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test037/tree-ops-ext.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "inherited properties propagation",
   :description "Setting inherited properties at different levels inherit to cell"}
  test038
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test038-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test038-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test038.csv")
                       {:body (io/file "w3c-csvw/tests/test038.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test038-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test038.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test038-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "valid inherited properties", :description "Different combinations of valid inherited properties"}
  test039
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test039-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test039-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test039.csv")
                       {:body (io/file "w3c-csvw/tests/test039.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test039-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test039.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test039-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid null",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test040
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test040-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test040-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test040.csv")
                       {:body (io/file "w3c-csvw/tests/test040.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test040-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test040.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test040-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid lang",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test041
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test041-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test041-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test041.csv")
                       {:body (io/file "w3c-csvw/tests/test041.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test041-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test041.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test041-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid textDirection",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test042
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test042-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test042-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test042.csv")
                       {:body (io/file "w3c-csvw/tests/test042.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test042-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test042.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test042-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid separator",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test043
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test043-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test043-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test043.csv")
                       {:body (io/file "w3c-csvw/tests/test043.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test043-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test043.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test043-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid ordered",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test044
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test044-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test044-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test044.csv")
                       {:body (io/file "w3c-csvw/tests/test044.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test044-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test044.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test044-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid default",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test045
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test045-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test045-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test045.csv")
                       {:body (io/file "w3c-csvw/tests/test045.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test045-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test045.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test045-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid dataype",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test046
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test046-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test046-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test046.csv")
                       {:body (io/file "w3c-csvw/tests/test046.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test046-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test046.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test046-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid aboutUrl",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test047
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test047-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test047-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test047.csv")
                       {:body (io/file "w3c-csvw/tests/test047.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test047-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test047.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test047-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid propertyUrl",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test048
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test048-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test048-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test048.csv")
                       {:body (io/file "w3c-csvw/tests/test048.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test048-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test048.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test048-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid valueUrl",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test049
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test049-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test049-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test049.csv")
                       {:body (io/file "w3c-csvw/tests/test049.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test049-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test049.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test049-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid commentPrefix",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test059
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test059-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test059-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test059-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test059.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test059-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid delimiter",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test060
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test060-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test060-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test060-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test060.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test060-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid doubleQuote",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test061
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test061-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test061-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test061-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test061.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test061-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid encoding",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test062
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test062-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test062-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test062-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test062.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test062-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid header",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test063
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test063-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test063-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test063-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test063.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test063-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid headerRowCount",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test065
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test065-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test065-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test065-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test065.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test065-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid lineTerminators",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test066
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test066-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test066-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test066-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test066.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test066-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid quoteChar",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test067
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test067-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test067-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test067-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test067.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test067-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid skipBlankRows",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test068
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test068-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test068-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test068-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test068.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test068-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid skipColumns",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test069
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test069-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test069-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test069-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test069.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test069-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid skipInitialSpace",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test070
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test070-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test070-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test070-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test070.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test070-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid skipRows",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test071
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test071-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test071-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test071-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test071.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test071-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dialect: invalid trim",
   :description
   "If a property has a value that is not permitted by this specification, then if a default value is provided for that property, compliant applications MUST use that default value and MUST generate a warning. If no default value is provided for that property, compliant applications MUST generate a warning and behave as if the property had not been specified."}
  test072
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test072-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test072-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test072-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test072.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test072-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid @language", :description "The value of `@language` MUST be a valid `BCP47` language code"}
  test073
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test073-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test073-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test073-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test073.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test073-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "empty tables",
   :description
   "Compliant application MUST raise an error if this array does not contain one or more `table descriptions`."}
  test074
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test074-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test074-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test074-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid tableGroup tableDirection",
   :description "An atomic property that MUST have a single string value that is one of \"rtl\", \"ltr\" or \"auto\"."}
  test075
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test075-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test075-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test075-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test075.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test075-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid table tableDirection",
   :description "An atomic property that MUST have a single string value that is one of \"rtl\", \"ltr\" or \"auto\"."}
  test076
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test076-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test076-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test076-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test076.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test076-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid tableGroup @id", :description "It MUST NOT start with `_:`."}
  test077
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test077-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test077-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test077-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid table @id", :description "It MUST NOT start with `_:`."}
  test078
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test078-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test078-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test078-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid schema @id", :description "It MUST NOT start with `_:`."}
  test079
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test079-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test079-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test079-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid column @id", :description "It MUST NOT start with `_:`."}
  test080
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test080-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test080-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test080-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid dialect @id", :description "It MUST NOT start with `_:`."}
  test081
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test081-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test081-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test081-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid template @id", :description "It MUST NOT start with `_:`."}
  test082
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test082-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test082-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test082-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid tableGroup @type", :description "If included `@type` MUST be `TableGroup`"}
  test083
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test083-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test083-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test083-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid table @type", :description "If included `@type` MUST be `TableGroup`"}
  test084
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test084-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test084-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test084-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid schema @type", :description "If included `@type` MUST be `TableGroup`"}
  test085
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test085-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test085-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test085-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid column @type", :description "If included `@type` MUST be `TableGroup`"}
  test086
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test086-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test086-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test086-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid dialect @type", :description "If included `@type` MUST be `Dialect`"}
  test087
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test087-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test087-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test087-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid transformation @type", :description "If included `@type` MUST be `Template`"}
  test088
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test088-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test088-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test088-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "missing tables in TableGroup", :description "The `tables` property is required in a `TableGroup`"}
  test089
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test089-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test089-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test089-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "missing url in Table", :description "The `url` property is required in a `Table`"}
  test090
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test090-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test090-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test090-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "same number of columns in each row",
   :description "each `row` MUST contain the same number of cells (although some of these cells may be empty)"}
  test091
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test091.csv")
                       {:body (io/file "w3c-csvw/tests/test091.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test091.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "undefined properties",
   :description
   "Compliant applications MUST ignore properties (aside from _common properties_) which are not defined in this specification and MUST generate a warning when they are encoutered"}
  test093
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test093-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test093-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test093-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test093.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test093-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "inconsistent array values: transformations",
   :description "Any items within an array that are not valid objects of the type expected are ignored"}
  test095
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test095-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test095-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test095-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test095.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test095-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "inconsistent array values: foreignKeys",
   :description "Any items within an array that are not valid objects of the type expected are ignored"}
  test097
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test097-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test097-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test097-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test097.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test097-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "inconsistent array values: tables",
   :description
   "If the supplied value of an array property is not an array (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been supplied with an empty array. Compliant application MUST raise an error if this array does not contain one or more table descriptions."}
  test098
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test098-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test098-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test098-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "inconsistent array values: transformations",
   :description
   "If the supplied value of an array property is not an array (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been supplied with an empty array"}
  test099
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test099-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test099-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test099-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test099.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test099-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "inconsistent array values: columns",
   :description
   "If the supplied value of an array property is not an array (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been supplied with an empty array"}
  test100
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test100-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test100-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test100-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test100.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test100-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "inconsistent array values: foreignKeys",
   :description
   "If the supplied value of an array property is not an array (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been supplied with an empty array"}
  test101
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test101-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test101-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test101-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test101.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test101-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "inconsistent link values: @id",
   :description
   "If the supplied value of an array property is not an array (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been supplied with an empty array"}
  test102
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test102-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test102-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test102-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test102.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test102-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "inconsistent link values: url",
   :description
   "If the supplied value of an array property is not an array (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been supplied with an empty array"}
  test103
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test103-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test103-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test103-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid columnReference", :description "The referenced description object MUST have a name property"}
  test104
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test104-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test104-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test104-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid primaryKey", :description "The referenced description object MUST have a name property"}
  test105
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test105-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test105-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test105-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test105.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test105-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid dialect",
   :description
   "If the supplied value of an object property is not a string or object (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been specified as an object with no properties."}
  test106
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test106-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test106-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test106-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test106.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test106-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid tableSchema",
   :description
   "If the supplied value of an object property is not a string or object (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been specified as an object with no properties."}
  test107
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test107-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test107-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test107-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test107.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test107-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid reference",
   :description
   "If the supplied value of an object property is not a string or object (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been specified as an object with no properties."}
  test108
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test108-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test108-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test108-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "titles with invalid language",
   :description
   "Natural Language properties may be objects whose properties MUST be language codes as defined by [BCP47] and whose values are either strings or arrays, providing natural language strings in that language. Validation fails because without a title, the metadata is incompatible with the CSV, which isn't a problem when not validating."}
  test109
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test109-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test109-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test109-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test109.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test109-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "titles with non-string values",
   :description
   "Natural Language properties may be objects whose properties MUST be language codes as defined by [BCP47] and whose values are either strings or arrays, providing natural language strings in that language"}
  test110
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test110-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test110-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test110-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test110.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test110-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "titles with invalid value",
   :description
   "If the supplied value of a natural language property is not a string, array or object (eg if it is an integer), compliant applications MUST issue a warning and proceed as if the property had been specified as an empty array. Validation fails because without a title, the metadata is incompatible with the CSV, which isn't a problem when not validating."}
  test111
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test111-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test111-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test111-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test111.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test111-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "titles with non-string array values",
   :description "If the supplied value is an array, any items in that array that are not strings MUST be ignored"}
  test112
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test112-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test112-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test112-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test112.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test112-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid suppressOutput",
   :description "Atomic properties: Processors MUST issue a warning if a property is set to an invalid value type"}
  test113
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test113-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test113-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test113-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test113.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test113-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid name",
   :description "Atomic properties: Processors MUST issue a warning if a property is set to an invalid value type"}
  test114
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test114-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test114-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test114-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test114.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test114-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid virtual",
   :description "Atomic properties: Processors MUST issue a warning if a property is set to an invalid value type"}
  test115
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test115-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test115-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test115-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test115.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test115-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "file-metadata with query component not found",
   :description "processors MUST attempt to locate a metadata documents through site-wide configuration."}
  test116
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test116.csv?query")
                       {:body (io/file "w3c-csvw/tests/test116.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test116.csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test116.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test116.csv?query")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test116.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test116.csv?query"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "file-metadata not referencing file",
   :description
   "If the metadata file found at this location does not explicitly include a reference to the requested tabular data file then it MUST be ignored."}
  test117
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test117.csv")
                       {:body (io/file "w3c-csvw/tests/test117.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test117.csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test117.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test117.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test117.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test117.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "directory-metadata with query component",
   :description "processors MUST attempt to locate a metadata documents through site-wide configuration. component."}
  test118
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test118/action.csv?query")
                       {:body (io/file "w3c-csvw/tests/test118/action.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test118/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test118/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test118/action.csv?query")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test118/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test118/action.csv?query"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "directory-metadata not referencing file",
   :description
   "If the metadata file found at this location does not explicitly include a reference to the requested tabular data file then it MUST be ignored."}
  test119
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test119/action.csv")
                       {:body (io/file "w3c-csvw/tests/test119/action.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test119/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test119/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test119/action.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test119/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test119/action.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "link-metadata not referencing file",
   :description
   "If the metadata file found at this location does not explicitly include a reference to the requested tabular data file then it MUST be ignored."}
  test120
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test120.csv")
                       {:body (io/file "w3c-csvw/tests/test120.csv"),
                        :headers
                        {"Content-Type" "text/csv; charset=UTF-8",
                         "Link"
                         "<test120-linked-metadata.json>; rel=\"describedby\"; type=\"application/csvm+json\""}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test120-linked-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test120-linked-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test120.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test120.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test120.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "user-metadata not referencing file",
   :description "User-specified metadata does not need to reference the starting CSV"}
  test121
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test121.csv")
                       {:body (io/file "w3c-csvw/tests/test121.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test121-user-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test121-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test121-ref.csv")
                       {:body (io/file "w3c-csvw/tests/test121-ref.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test121.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test121-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test121.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test121.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "link-metadata not describing file uses file-metadata",
   :description
   "If the metadata file found at this location does not explicitly include a reference to the requested tabular data file then it MUST be ignored."}
  test122
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test122.csv")
                       {:body (io/file "w3c-csvw/tests/test122.csv"),
                        :headers
                        {"Content-Type" "text/csv; charset=UTF-8",
                         "Link"
                         "<test122-linked-metadata.json>; rel=\"describedby\"; type=\"application/csvm+json\""}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test122-linked-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test122-linked-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test122.csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test122.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test122.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test122.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test122.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "file-metadata not describing file uses directory-metadata",
   :description
   "If the metadata file found at this location does not explicitly include a reference to the requested tabular data file then it MUST be ignored."}
  test123
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test123/action.csv")
                       {:body (io/file "w3c-csvw/tests/test123/action.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test123/action.csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test123/action.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test123/csv-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test123/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test123/action.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test123/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test123/action.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "metadata with columns not matching csv titles",
   :description
   "If not validating, and one schema has a name property but not a titles property, and the other has a titles property but not a name property."}
  test124
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test124-user-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test124-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test124-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test124.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "required column with empty cell",
   :description "If the column required annotation is true, add an error to the list of errors for the cell."}
  test125
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test125-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test125-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test125.csv")
                       {:body (io/file "w3c-csvw/tests/test125.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test125-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test125.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test125-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "required column with cell matching null",
   :description
   "if the string is the same as any one of the values of the column null annotation, then the resulting value is null. If the column separator annotation is null and the column required annotation is true, add an error to the list of errors for the cell."}
  test126
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test126-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test126-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test126.csv")
                       {:body (io/file "w3c-csvw/tests/test126.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test126-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test126.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test126-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "incompatible table",
   :description
   "if TM is not compatible with EM validators MUST raise an error, other processors MUST generate a warning and continue processing"}
  test127
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test127-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test127-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test127.csv")
                       {:body (io/file "w3c-csvw/tests/test127.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test127-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test127.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test127-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "duplicate column names",
   :description "The name properties of the column descriptions MUST be unique within a given table description."}
  test128
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test128-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test128-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test128-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "columnn name as integer",
   :description
   "This (name) MUST be a string and this property has no default value, which means it MUST be ignored if the supplied value is not a string."}
  test129
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test129-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test129-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test129-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test129.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test129-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid column name", :description "column names are restricted as defined in Variables in [URI-TEMPLATE] "}
  test130
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test130-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test130-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test130-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test130.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test130-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid column name",
   :description
   "column names are restricted ... names beginning with '_' are reserved by this specification and MUST NOT be used within metadata documents."}
  test131
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test131-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test131-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test131-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test131.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test131-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "name annotation from title percent encoded",
   :description
   "If there is no name property defined on this column, the first titles value having the same language tag as default language, or und or if no default language is specified, becomes the name annotation for the described column. This annotation MUST be percent-encoded as necessary to conform to the syntactic requirements defined in [RFC3986]"}
  test132
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test132-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test132-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test132-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test132.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test132-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "virtual before non-virtual",
   :description "If present, a virtual column MUST appear after all other non-virtual column definitions."}
  test133
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test133-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test133-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test133-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "context in common property", :description "A metadata document MUST NOT add a new context"}
  test134
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test134-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test134-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test134-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@list value", :description "Values MUST NOT use list objects or set objects."}
  test135
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test135-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test135-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test135-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@set value", :description "Values MUST NOT use list objects or set objects."}
  test136
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test136-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test136-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test136-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@type out of range (as datatype)",
   :description "The value of any @id or @type contained within a metadata document MUST NOT be a blank node."}
  test137
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test137-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test137-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test137-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@type out of range (as node type)",
   :description "The value of any @id or @type contained within a metadata document MUST NOT be a blank node."}
  test138
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test138-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test138-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test138-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@type out of range (as node type) - string",
   :description
   "The value of any member of @type MUST be either a term defined in [csvw-context], a prefixed name where the prefix is a term defined in [csvw-context], or an absolute URL."}
  test139
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test139-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test139-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test139-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@type out of range (as node type) - integer",
   :description
   "The value of any member of @type MUST be either a term defined in [csvw-context], a prefixed name where the prefix is a term defined in [csvw-context], or an absolute URL."}
  test140
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test140-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test140-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test140-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@id out of range (as node type) - bnode",
   :description "The value of any @id or @type contained within a metadata document MUST NOT be a blank node."}
  test141
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test141-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test141-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test141-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@value with @language and @type",
   :description
   "If a @value property is used on an object, that object MUST NOT have any other properties aside from either @type or @language, and MUST NOT have both @type and @language as properties. The value of the @value property MUST be a string, number, or boolean value."}
  test142
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test142-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test142-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test142-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@value with extra properties",
   :description
   "If a @value property is used on an object, that object MUST NOT have any other properties aside from either @type or @language, and MUST NOT have both @type and @language as properties. The value of the @value property MUST be a string, number, or boolean value."}
  test143
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test143-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test143-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test143-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@language outside of @value",
   :description "A @language property MUST NOT be used on an object unless it also has a @value property."}
  test144
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test144-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test144-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test144-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "Invalid faux-keyword",
   :description
   "Aside from @value, @type, @language, and @id, the properties used on an object MUST NOT start with @."}
  test146
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test146-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test146-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test146-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "title incompatible with title on case",
   :description
   "If there is a non-empty case-sensitive intersection between the titles values, where matches MUST have a matching language; `und` matches any language, and languages match if they are equal when truncated, as defined in [BCP47], to the length of the shortest language tag."}
  test147
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test147-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test147-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test147-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test147.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test147-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "title incompatible with title on language",
   :description
   "If there is a non-empty case-sensitive intersection between the titles values, where matches MUST have a matching language; `und` matches any language, and languages match if they are equal when truncated, as defined in [BCP47], to the length of the shortest language tag."}
  test148
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test148-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test148-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test148-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test148.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test148-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "title compatible with title on less specific language",
   :description
   "If there is a non-empty case-sensitive intersection between the titles values, where matches MUST have a matching language; `und` matches any language, and languages match if they are equal when truncated, as defined in [BCP47], to the length of the shortest language tag."}
  test149
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test149-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test149-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test149-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test149.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test149-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "non-builtin datatype (datatype value)",
   :description
   "If the value of this property is a string, it MUST be one of the built-in datatypes defined in section 5.11.1 Built-in Datatypes or an absolute URL"}
  test150
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test150-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test150-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test150-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test150.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test150-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "non-builtin datatype (base value)",
   :description "If the value of this property is a string, it MUST be one of the built-in datatypes"}
  test151
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test151-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test151-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test151-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test151.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test151-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "string format (valid combinations)",
   :description
   "If the datatype base is not numeric, boolean, a date/time type, or a duration type, the datatype format annotation provides a regular expression for the string values"}
  test152
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test152-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test152-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test152.csv")
                       {:body (io/file "w3c-csvw/tests/test152.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test152-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test152.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test152-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "string format (bad format string)",
   :description
   "If the datatype base is not numeric, boolean, a date/time type, or a duration type, the datatype format annotation provides a regular expression for the string values"}
  test153
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test153-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test153-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test153.csv")
                       {:body (io/file "w3c-csvw/tests/test153.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test153-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test153.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test153-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "string format (value not matching format)",
   :description
   "If the datatype base is not numeric, boolean, a date/time type, or a duration type, the datatype format annotation provides a regular expression for the string values"}
  test154
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test154-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test154-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test154.csv")
                       {:body (io/file "w3c-csvw/tests/test154.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test154-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test154.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test154-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "number format (valid combinations)",
   :description
   "If the datatype format annotation is a single string, this is interpreted in the same way as if it were an object with a pattern property whose value is that string"}
  test155
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test155-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test155-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test155.csv")
                       {:body (io/file "w3c-csvw/tests/test155.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test155-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test155.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test155-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "number format (bad format string)",
   :description
   "If the datatype format annotation is a single string, this is interpreted in the same way as if it were an object with a pattern property whose value is that string"}
  test156
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test156-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test156-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test156.csv")
                       {:body (io/file "w3c-csvw/tests/test156.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test156-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test156.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test156-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "number format (value not matching format)",
   :description
   "If the datatype format annotation is a single string, this is interpreted in the same way as if it were an object with a pattern property whose value is that string"}
  test157
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test157-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test157-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test157.csv")
                       {:body (io/file "w3c-csvw/tests/test157.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test157-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test157.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test157-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "number format (valid combinations)", :description "Numeric dataype with object format"}
  test158
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test158-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test158-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test158.csv")
                       {:body (io/file "w3c-csvw/tests/test158.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test158-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test158.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test158-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "number format (bad pattern format string)",
   :description
   "If the datatype format annotation is a single string, this is interpreted in the same way as if it were an object with a pattern property whose value is that string"}
  test159
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test159-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test159-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test159.csv")
                       {:body (io/file "w3c-csvw/tests/test159.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test159-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test159.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test159-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "number format (not matching values with pattern)",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell if the string being parsed"}
  test160
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test160-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test160-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test160.csv")
                       {:body (io/file "w3c-csvw/tests/test160.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test160-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test160.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test160-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "number format (not matching values without pattern)",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell if the string being parsed"}
  test161
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test161-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test161-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test161.csv")
                       {:body (io/file "w3c-csvw/tests/test161.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test161-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test161.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test161-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "numeric format (consecutive groupChar)",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell if the string being parsed contains two consecutive groupChar strings"}
  test162
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test162-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test162-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test162.csv")
                       {:body (io/file "w3c-csvw/tests/test162.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test162-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test162.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test162-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "integer datatype with decimalChar",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell if the string being parsed contains the decimalChar, if the datatype base is integer or one of its sub-values"}
  test163
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test163-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test163-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test163.csv")
                       {:body (io/file "w3c-csvw/tests/test163.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test163-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test163.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test163-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "decimal datatype with exponent",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, if the datatype base is decimal or one of its sub-values"}
  test164
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test164-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test164-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test164.csv")
                       {:body (io/file "w3c-csvw/tests/test164.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test164-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test164.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test164-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "decimal type with NaN",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, is one of the special values NaN, INF, or -INF, if the datatype base is decimal or one of its sub-values"}
  test165
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test165-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test165-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test165.csv")
                       {:body (io/file "w3c-csvw/tests/test165.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test165-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test165.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test165-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "decimal type with INF",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, is one of the special values NaN, INF, or -INF, if the datatype base is decimal or one of its sub-values"}
  test166
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test166-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test166-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test166.csv")
                       {:body (io/file "w3c-csvw/tests/test166.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test166-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test166.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test166-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "decimal type with -INF",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, is one of the special values NaN, INF, or -INF, if the datatype base is decimal or one of its sub-values"}
  test167
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test167-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test167-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test167.csv")
                       {:body (io/file "w3c-csvw/tests/test167.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test167-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test167.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test167-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "decimal with implicit groupChar",
   :description
   "When parsing the string value of a cell against this format specification, implementations MUST recognise and parse numbers"}
  test168
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test168-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test168-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test168.csv")
                       {:body (io/file "w3c-csvw/tests/test168.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test168-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test168.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test168-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid decimal",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test169
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test169-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test169-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test169.csv")
                       {:body (io/file "w3c-csvw/tests/test169.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test169-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test169.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test169-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "decimal with percent",
   :description
   "Implementations MUST use the sign, exponent, percent, and per-mille signs when parsing the string value of a cell to provide the value of the cell"}
  test170
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test170-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test170-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test170.csv")
                       {:body (io/file "w3c-csvw/tests/test170.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test170-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test170.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test170-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "decimal with per-mille",
   :description
   "Implementations MUST use the sign, exponent, percent, and per-mille signs when parsing the string value of a cell to provide the value of the cell"}
  test171
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test171-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test171-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test171.csv")
                       {:body (io/file "w3c-csvw/tests/test171.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test171-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test171.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test171-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid byte",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test172
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test172-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test172-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test172.csv")
                       {:body (io/file "w3c-csvw/tests/test172.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test172-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test172.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test172-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invald unsignedLong",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test173
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test173-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test173-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test173.csv")
                       {:body (io/file "w3c-csvw/tests/test173.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test173-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test173.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test173-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid unsignedShort",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test174
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test174-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test174-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test174.csv")
                       {:body (io/file "w3c-csvw/tests/test174.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test174-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test174.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test174-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid unsignedByte",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test175
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test175-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test175-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test175.csv")
                       {:body (io/file "w3c-csvw/tests/test175.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test175-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test175.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test175-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid positiveInteger",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test176
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test176-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test176-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test176.csv")
                       {:body (io/file "w3c-csvw/tests/test176.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test176-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test176.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test176-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid negativeInteger",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test177
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test177-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test177-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test177.csv")
                       {:body (io/file "w3c-csvw/tests/test177.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test177-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test177.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test177-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid nonPositiveInteger",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test178
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test178-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test178-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test178.csv")
                       {:body (io/file "w3c-csvw/tests/test178.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test178-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test178.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test178-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid nonNegativeInteger",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test179
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test179-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test179-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test179.csv")
                       {:body (io/file "w3c-csvw/tests/test179.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test179-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test179.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test179-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid double",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test180
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test180-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test180-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test180.csv")
                       {:body (io/file "w3c-csvw/tests/test180.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test180-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test180.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test180-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid number",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test181
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test181-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test181-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test181.csv")
                       {:body (io/file "w3c-csvw/tests/test181.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test181-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test181.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test181-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid float",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell contains an exponent, does not meet the numeric format defined above"}
  test182
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test182-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test182-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test182.csv")
                       {:body (io/file "w3c-csvw/tests/test182.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test182-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test182.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test182-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "boolean format (valid combinations)",
   :description
   "If the datatype base for a cell is boolean, the datatype format annotation provides the true and false values expected, separated by `|`."}
  test183
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test183-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test183-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test183.csv")
                       {:body (io/file "w3c-csvw/tests/test183.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test183-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test183.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test183-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "boolean format (bad format string)",
   :description
   "If the datatype base for a cell is boolean, the datatype format annotation provides the true and false values expected, separated by `|`."}
  test184
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test184-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test184-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test184.csv")
                       {:body (io/file "w3c-csvw/tests/test184.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test184-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test184.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test184-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "boolean format (value not matching format)",
   :description
   "If the datatype base for a cell is boolean, the datatype format annotation provides the true and false values expected, separated by `|`."}
  test185
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test185-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test185-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test185.csv")
                       {:body (io/file "w3c-csvw/tests/test185.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test185-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test185.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test185-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "boolean format (not matching datatype)",
   :description
   "Implementations MUST add a validation error to the errors annotation for the cell if the string being parsed"}
  test186
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test186-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test186-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test186.csv")
                       {:body (io/file "w3c-csvw/tests/test186.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test186-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test186.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test186-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date format (valid native combinations)",
   :description
   "The supported date and time formats listed here are expressed in terms of the date field symbols defined in [UAX35] and MUST be interpreted by implementations as defined in that specification."}
  test187
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test187-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test187-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test187.csv")
                       {:body (io/file "w3c-csvw/tests/test187.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test187-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test187.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test187-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date format (valid date combinations with formats)",
   :description
   "The supported date and time formats listed here are expressed in terms of the date field symbols defined in [UAX35] and MUST be interpreted by implementations as defined in that specification."}
  test188
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test188-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test188-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test188.csv")
                       {:body (io/file "w3c-csvw/tests/test188.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test188-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test188.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test188-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date format (valid time combinations with formats)",
   :description
   "The supported date and time formats listed here are expressed in terms of the date field symbols defined in [UAX35] and MUST be interpreted by implementations as defined in that specification."}
  test189
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test189-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test189-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test189.csv")
                       {:body (io/file "w3c-csvw/tests/test189.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test189-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test189.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test189-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date format (valid dateTime combinations with formats)",
   :description
   "The supported date and time formats listed here are expressed in terms of the date field symbols defined in [UAX35] and MUST be interpreted by implementations as defined in that specification."}
  test190
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test190-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test190-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test190.csv")
                       {:body (io/file "w3c-csvw/tests/test190.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test190-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test190.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test190-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date format (bad format string)",
   :description
   "The supported date and time formats listed here are expressed in terms of the date field symbols defined in [UAX35] and MUST be interpreted by implementations as defined in that specification."}
  test191
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test191-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test191-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test191.csv")
                       {:body (io/file "w3c-csvw/tests/test191.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test191-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test191.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test191-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date format (value not matching format)",
   :description
   "The supported date and time formats listed here are expressed in terms of the date field symbols defined in [UAX35] and MUST be interpreted by implementations as defined in that specification."}
  test192
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test192-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test192-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test192.csv")
                       {:body (io/file "w3c-csvw/tests/test192.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test192-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test192.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test192-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "duration format (valid combinations)",
   :description
   "If the datatype base is a duration type, the datatype format annotation provides a regular expression for the string values"}
  test193
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test193-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test193-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test193.csv")
                       {:body (io/file "w3c-csvw/tests/test193.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test193-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test193.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test193-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "duration format (value not matching format)",
   :description
   "If the datatype base is a duration type, the datatype format annotation provides a regular expression for the string values"}
  test194
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test194-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test194-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test194.csv")
                       {:body (io/file "w3c-csvw/tests/test194.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test194-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test194.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test194-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "values with matching length",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test195
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test195-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test195-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test195.csv")
                       {:body (io/file "w3c-csvw/tests/test195.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test195-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test195.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test195-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "values with wrong length",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test196
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test196-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test196-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test196.csv")
                       {:body (io/file "w3c-csvw/tests/test196.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test196-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test196.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test196-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "values with wrong maxLength",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test197
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test197-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test197-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test197.csv")
                       {:body (io/file "w3c-csvw/tests/test197.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test197-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test197.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test197-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "values with wrong minLength",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test198
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test198-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test198-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test198.csv")
                       {:body (io/file "w3c-csvw/tests/test198.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test198-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test198.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test198-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "length < minLength",
   :description
   "Applications MUST raise an error if both length and minLength are specified and length is less than minLength. "}
  test199
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test199-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test199-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test199.csv")
                       {:body (io/file "w3c-csvw/tests/test199.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test199-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "length > maxLength",
   :description
   "Applications MUST raise an error if both length and maxLength are specified and length is greater than maxLength. "}
  test200
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test200-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test200-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test200.csv")
                       {:body (io/file "w3c-csvw/tests/test200.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test200-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "length on date",
   :description
   "Applications MUST raise an error if length, maxLength, or minLength are specified and the base datatype is not string or one of its subtypes, or a binary type."}
  test201
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test201-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test201-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test201.csv")
                       {:body (io/file "w3c-csvw/tests/test201.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test201-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "float matching constraints",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test202
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test202-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test202-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test202.csv")
                       {:body (io/file "w3c-csvw/tests/test202.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test202-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test202.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test202-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "float value constraint not matching minimum",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test203
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test203-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test203-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test203.csv")
                       {:body (io/file "w3c-csvw/tests/test203.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test203-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test203.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test203-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "float value constraint not matching maximum",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test204
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test204-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test204-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test204.csv")
                       {:body (io/file "w3c-csvw/tests/test204.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test204-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test204.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test204-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "float value constraint not matching minInclusive",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test205
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test205-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test205-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test205.csv")
                       {:body (io/file "w3c-csvw/tests/test205.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test205-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test205.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test205-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "float value constraint not matching minExclusive",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test206
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test206-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test206-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test206.csv")
                       {:body (io/file "w3c-csvw/tests/test206.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test206-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test206.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test206-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "float value constraint not matching maxInclusive",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test207
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test207-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test207-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test207.csv")
                       {:body (io/file "w3c-csvw/tests/test207.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test207-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test207.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test207-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "float value constraint not matching maxExclusive",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test208
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test208-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test208-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test208.csv")
                       {:body (io/file "w3c-csvw/tests/test208.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test208-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test208.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test208-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date matching constraints",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test209
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test209-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test209-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test209.csv")
                       {:body (io/file "w3c-csvw/tests/test209.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test209-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test209.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test209-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date value constraint not matching minimum",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test210
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test210-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test210-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test210.csv")
                       {:body (io/file "w3c-csvw/tests/test210.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test210-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test210.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test210-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date value constraint not matching maximum",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test211
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test211-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test211-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test211.csv")
                       {:body (io/file "w3c-csvw/tests/test211.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test211-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test211.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test211-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date value constraint not matching minInclusive",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test212
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test212-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test212-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test212.csv")
                       {:body (io/file "w3c-csvw/tests/test212.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test212-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test212.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test212-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date value constraint not matching minExclusive",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test213
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test213-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test213-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test213.csv")
                       {:body (io/file "w3c-csvw/tests/test213.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test213-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test213.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test213-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date value constraint not matching maxInclusive",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test214
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test214-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test214-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test214.csv")
                       {:body (io/file "w3c-csvw/tests/test214.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test214-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test214.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test214-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date value constraint not matching maxExclusive",
   :description
   "validate the value based on the length constraints described in section 4.6.1 Length Constraints, the value constraints described in section 4.6.2 Value Constraints and the datatype format annotation if one is specified, as described below. If there are any errors, add them to the list of errors for the cell."}
  test215
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test215-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test215-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test215.csv")
                       {:body (io/file "w3c-csvw/tests/test215.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test215-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test215.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test215-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "minInclusive and minExclusive",
   :description
   "Applications MUST raise an error if both minInclusive and minExclusive are specified, or if both maxInclusive and maxExclusive are specified. "}
  test216
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test216-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test216-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test216.csv")
                       {:body (io/file "w3c-csvw/tests/test216.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test216-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "maxInclusive and maxExclusive",
   :description
   "Applications MUST raise an error if both minInclusive and minExclusive are specified, or if both maxInclusive and maxExclusive are specified. "}
  test217
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test217-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test217-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test217.csv")
                       {:body (io/file "w3c-csvw/tests/test217.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test217-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "maxInclusive < minInclusive",
   :description
   "Applications MUST raise an error if both minInclusive and maxInclusive are specified and maxInclusive is less than minInclusive, or if both minInclusive and maxExclusive are specified and maxExclusive is less than or equal to minInclusive."}
  test218
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test218-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test218-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test218.csv")
                       {:body (io/file "w3c-csvw/tests/test218.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test218-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "maxExclusive = minInclusive",
   :description
   "Applications MUST raise an error if both minInclusive and maxInclusive are specified and maxInclusive is less than minInclusive, or if both minInclusive and maxExclusive are specified and maxExclusive is less than or equal to minInclusive."}
  test219
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test219-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test219-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test219.csv")
                       {:body (io/file "w3c-csvw/tests/test219.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test219-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "maxExclusive < minExclusive",
   :description
   "Applications MUST raise an error if both minExclusive and maxExclusive are specified and maxExclusive is less than minExclusive, or if both minExclusive and maxInclusive are specified and maxInclusive is less than or equal to minExclusive."}
  test220
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test220-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test220-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test220.csv")
                       {:body (io/file "w3c-csvw/tests/test220.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test220-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "maxInclusive = minExclusive",
   :description
   "Applications MUST raise an error if both minExclusive and maxExclusive are specified and maxExclusive is less than minExclusive, or if both minExclusive and maxInclusive are specified and maxInclusive is less than or equal to minExclusive."}
  test221
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test221-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test221-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test221.csv")
                       {:body (io/file "w3c-csvw/tests/test221.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test221-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "string datatype with minimum",
   :description
   "Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive are specified and the base datatype is not a numeric, date/time, or duration type."}
  test222
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test222-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test222-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test222.csv")
                       {:body (io/file "w3c-csvw/tests/test222.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test222-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "string datatype with maxium",
   :description
   "Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive are specified and the base datatype is not a numeric, date/time, or duration type."}
  test223
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test223-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test223-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test223.csv")
                       {:body (io/file "w3c-csvw/tests/test223.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test223-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "string datatype with minInclusive",
   :description
   "Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive are specified and the base datatype is not a numeric, date/time, or duration type."}
  test224
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test224-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test224-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test224.csv")
                       {:body (io/file "w3c-csvw/tests/test224.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test224-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "string datatype with maxInclusive",
   :description
   "Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive are specified and the base datatype is not a numeric, date/time, or duration type."}
  test225
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test225-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test225-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test225.csv")
                       {:body (io/file "w3c-csvw/tests/test225.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test225-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "string datatype with minExclusive",
   :description
   "Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive are specified and the base datatype is not a numeric, date/time, or duration type."}
  test226
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test226-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test226-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test226.csv")
                       {:body (io/file "w3c-csvw/tests/test226.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test226-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "string datatype with maxExclusive",
   :description
   "Applications MUST raise an error if minimum, minInclusive, maximum, maxInclusive, minExclusive, or maxExclusive are specified and the base datatype is not a numeric, date/time, or duration type."}
  test227
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test227-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test227-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test227.csv")
                       {:body (io/file "w3c-csvw/tests/test227.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test227-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "length with separator",
   :description "If the value is a list, the constraint applies to each element of the list."}
  test228
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test228-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test228-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test228.csv")
                       {:body (io/file "w3c-csvw/tests/test228.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test228-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test228.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test228-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "matching minLength with separator",
   :description "If the value is a list, the constraint applies to each element of the list."}
  test229
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test229-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test229-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test229.csv")
                       {:body (io/file "w3c-csvw/tests/test229.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test229-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test229.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test229-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "failing minLength with separator",
   :description "If the value is a list, the constraint applies to each element of the list."}
  test230
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test230-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test230-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test230.csv")
                       {:body (io/file "w3c-csvw/tests/test230.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test230-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test230.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test230-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "single column primaryKey success",
   :description
   "As defined in [tabular-data-model], validators MUST check that each row has a unique combination of values of cells in the indicated columns."}
  test231
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test231-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test231-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test231.csv")
                       {:body (io/file "w3c-csvw/tests/test231.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test231-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test231.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test231-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "single column primaryKey violation",
   :description "Validators MUST raise errors if there is more than one row with the same primary key"}
  test232
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test232-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test232-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test232.csv")
                       {:body (io/file "w3c-csvw/tests/test232.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test232-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test232.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test232-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "multiple column primaryKey success",
   :description
   "As defined in [tabular-data-model], validators MUST check that each row has a unique combination of values of cells in the indicated columns."}
  test233
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test233-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test233-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test233.csv")
                       {:body (io/file "w3c-csvw/tests/test233.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test233-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test233.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test233-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "multiple column primaryKey violation",
   :description "Validators MUST raise errors if there is more than one row with the same primary key"}
  test234
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test234-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test234-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test234.csv")
                       {:body (io/file "w3c-csvw/tests/test234.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test234-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test234.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test234-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "rowTitles on one column",
   :description
   "if row titles is not null, insert any titles specified for the row. For each value, tv, of the row titles annotation"}
  test235
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test235-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test235-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test235-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test235.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test235-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "rowTitles on multiple columns",
   :description
   "if row titles is not null, insert any titles specified for the row. For each value, tv, of the row titles annotation"}
  test236
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test236-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test236-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test236-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test236.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test236-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "rowTitles on one column (minimal)",
   :description
   "if row titles is not null, insert any titles specified for the row. For each value, tv, of the row titles annotation"}
  test237
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test237-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test237-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test237-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test237.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test237-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "datatype value an absolute URL",
   :description "it must be the name of one of the built-in datatypes defined in section 5.11.1 Built-in Datatypes"}
  test238
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test238-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test238-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test238.csv")
                       {:body (io/file "w3c-csvw/tests/test238.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test238-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test238.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test238-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "datatype @id an absolute URL",
   :description
   "If included, @id is a link property that identifies the datatype described by this datatype description."}
  test242
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test242-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test242-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test242.csv")
                       {:body (io/file "w3c-csvw/tests/test242.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test242-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test242.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test242-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid datatype @id", :description "It MUST NOT start with `_:`."}
  test243
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test243-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test243-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test243.csv")
                       {:body (io/file "w3c-csvw/tests/test243.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test243-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "invalid datatype @id", :description "It MUST NOT be the URL of a built-in datatype."}
  test244
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test244-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test244-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test244.csv")
                       {:body (io/file "w3c-csvw/tests/test244.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test244-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "date format (valid time combinations with formats and milliseconds)",
   :description
   "The supported date and time formats listed here are expressed in terms of the date field symbols defined in [UAX35] and MUST be interpreted by implementations as defined in that specification."}
  test245
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test245-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test245-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test245.csv")
                       {:body (io/file "w3c-csvw/tests/test245.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test245-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test245.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test245-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date format (valid dateTime combinations with formats and milliseconds)",
   :description
   "The supported date and time formats listed here are expressed in terms of the date field symbols defined in [UAX35] and MUST be interpreted by implementations as defined in that specification."}
  test246
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test246-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test246-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test246.csv")
                       {:body (io/file "w3c-csvw/tests/test246.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test246-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test246.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test246-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "date format (extra milliseconds)",
   :description
   "The supported date and time formats listed here are expressed in terms of the date field symbols defined in [UAX35] and MUST be interpreted by implementations as defined in that specification."}
  test247
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test247-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test247-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test247.csv")
                       {:body (io/file "w3c-csvw/tests/test247.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test247-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test247.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test247-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "Unicode in non-Normalized form",
   :description "No Unicode normalization (as specified in [UAX15]) is applied to these string values"}
  test248
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test248-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test248-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test248.csv")
                       {:body (io/file "w3c-csvw/tests/test248.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test248-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test248.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test248-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "missing source reference",
   :description
   "As defined in [tabular-data-model], validators MUST check that, for each row, the combination of cells in the referencing columns references a unique row within the referenced table through a combination of cells in the referenced columns."}
  test251
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test251-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test251-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test251-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "missing destination reference column",
   :description
   "As defined in [tabular-data-model], validators MUST check that, for each row, the combination of cells in the referencing columns references a unique row within the referenced table through a combination of cells in the referenced columns."}
  test252
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test252-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test252-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test252-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "missing destination table",
   :description
   "As defined in [tabular-data-model], validators MUST check that, for each row, the combination of cells in the referencing columns references a unique row within the referenced table through a combination of cells in the referenced columns."}
  test253
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test253-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test253-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "w3c-csvw/tests/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test253-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "tree-ops example with csvm.json (w3.org/.well-known/csvm)",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test259
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test259/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/test259/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test259/csvm.json")
                       {:body (io/file "w3c-csvw/tests/test259/csvm.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/.well-known/csvm")
                       {:body "{+url}-metadata.json\ncsv-metadata.json\n{+url}.json\ncsvm.json", :headers {}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test259/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test259/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test259/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "tree-ops example with {+url}.json (w3.org/.well-known/csvm)",
   :description
   "Processors MUST use the first metadata found for processing a tabular data file by using overriding metadata, if provided. Otherwise processors MUST attempt to locate the first metadata document from the Link header or the metadata located through site-wide configuration."}
  test260
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test260/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/test260/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test260/tree-ops.csv.json")
                       {:body (io/file "w3c-csvw/tests/test260/tree-ops.csv.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/.well-known/csvm")
                       {:body "{+url}-metadata.json\ncsv-metadata.json\n{+url}.json\ncsvm.json", :headers {}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test260/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test260/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test260/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "maxLength < minLength",
   :description
   "Applications MUST raise an error if both minLength and maxLength are specified and minLength is greater than maxLength."}
  test261
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test261-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test261-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test261.csv")
                       {:body (io/file "w3c-csvw/tests/test261.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test261-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "@type on a common property can be a built-in type",
   :description
   "The value of any member of `@type` MUST be either a _term_ defined in [csvw-context], a _prefixed name_ where the prefix is a term defined in [csvw-context], or an absolute URL."}
  test263
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test263-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test263-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test263-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test263.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test263-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "@type on a common property can be a CURIE if the prefix is one of the built-in ones",
   :description
   "The value of any member of `@type` MUST be either a _term_ defined in [csvw-context], a _prefixed name_ where the prefix is a term defined in [csvw-context], or an absolute URL."}
  test264
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test264-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test264-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test264-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test264.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test264-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "`null` contains an array of (valid) string & (invalid) numeric values",
   :description "Processors MUST issue a warning if a property is set to an invalid value type"}
  test266
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test266-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test266-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test266-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test266.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test266-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "@id on datatype is invalid (eg starts with _:)",
   :description "It MUST NOT start with `_:` and it MUST NOT be the URL of a built-in datatype."}
  test267
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test267-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test267-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test267-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "`base` missing on datatype (defaults to string)",
   :description
   "An atomic property that contains a single string: the name of one of the built-in datatypes, as listed above (and which are defined as terms in the default context). Its default is string."}
  test268
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test268-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test268-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test268-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test268.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test268-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "`format` for a boolean datatype is a string but in the wrong form (eg YN)",
   :description
   "If the datatype base for a cell is `boolean`, the datatype format annotation provides the true value followed by the false value, separated by `|`. If the format does not follow this syntax, implementations MUST issue a warning and proceed as if no format had been provided."}
  test269
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test269-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test269-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test269.csv")
                       {:body (io/file "w3c-csvw/tests/test269.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test269-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test269.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test269-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "transformation includes an invalid property (eg foo)",
   :description
   "All terms used within a metadata document MUST be defined in [csvw-context] defined for this specification"}
  test270
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test270-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test270-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test270-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test270.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test270-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "foreign key includes an invalid property (eg `dc:description`)",
   :description "A foreign key definition is a JSON object that must contain only the following properties. . ."}
  test271
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test271-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test271-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test271.csv")
                       {:body (io/file "w3c-csvw/tests/test271.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test271-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "foreign key reference includes an invalid property (eg `dc:description`)",
   :description "A foreign key definition is a JSON object that must contain only the following properties. . ."}
  test272
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test272-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test272-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test272.csv")
                       {:body (io/file "w3c-csvw/tests/test272.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "w3c-csvw/tests/countries.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test272-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "`@base` set in `@context` overriding eg CSV location",
   :description
   "If present, its value MUST be a string that is interpreted as a URL which is resolved against the location of the metadata document to provide the **base URL** for other URLs in the metadata document."}
  test273
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test273-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test273-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test273/action.csv")
                       {:body (io/file "w3c-csvw/tests/test273/action.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test273-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test273.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test273-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "`@context` object includes properties other than `@base` and `@language`",
   :description
   "The `@context` MUST have one of the following values: An array composed of a string followed by an object, where the string is `http://www.w3.org/ns/csvw` and the object represents a local context definition, which is restricted to contain either or both of the following members."}
  test274
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test274-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test274-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test274-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  ^
  {:title "property acceptable on column appears on table group",
   :description "Table Group may only use defined properties."}
  test275
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test275-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test275-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test275-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test275.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test275-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "property acceptable on column appears on table", :description "Table may only use defined properties."}
  test276
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test276-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test276-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test276-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test276.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test276-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "property acceptable on table appears on column", :description "Column may only use defined properties."}
  test277
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test277-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test277-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test277-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test277.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test277-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "CSV has more headers than there are columns in the metadata",
   :description
   "Two schemas are compatible if they have the same number of non-virtual column descriptions, and the non-virtual column descriptions at the same index within each are compatible with each other."}
  test278
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test278-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test278-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "w3c-csvw/tests/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test278-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test278.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test278-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "duration not matching xsd pattern", :description "Value MUST be a valid xsd:duration."}
  test279
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test279-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test279-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test279.csv")
                       {:body (io/file "w3c-csvw/tests/test279.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test279-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test279.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test279-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "dayTimeDuration not matching xsd pattern", :description "Value MUST be a valid xsd:dayTimeDuration."}
  test280
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test280-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test280-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test280.csv")
                       {:body (io/file "w3c-csvw/tests/test280.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test280-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test280.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test280-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "yearMonthDuration not matching xsd pattern", :description "Value MUST be a valid xsd:yearMonthDuration."}
  test281
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test281-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test281-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test281.csv")
                       {:body (io/file "w3c-csvw/tests/test281.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test281-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test281.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test281-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "valid number patterns",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test282
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test282-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test282-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test282.csv")
                       {:body (io/file "w3c-csvw/tests/test282.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test282-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test282.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test282-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "valid number patterns (signs and percent/permille)",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test283
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test283-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test283-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test283.csv")
                       {:body (io/file "w3c-csvw/tests/test283.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test283-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test283.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test283-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "valid number patterns (grouping)",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test284
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test284-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test284-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test284.csv")
                       {:body (io/file "w3c-csvw/tests/test284.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test284-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test284.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test284-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "valid number patterns (fractional grouping)",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test285
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test285-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test285-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test285.csv")
                       {:body (io/file "w3c-csvw/tests/test285.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test285-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test285.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test285-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid ##0 1,234",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test286
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test286-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test286-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test286.csv")
                       {:body (io/file "w3c-csvw/tests/test286.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test286-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test286.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test286-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid ##0 123.4",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test287
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test287-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test287-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test287.csv")
                       {:body (io/file "w3c-csvw/tests/test287.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test287-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test287.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test287-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #,#00 1",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test288
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test288-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test288-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test288.csv")
                       {:body (io/file "w3c-csvw/tests/test288.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test288-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test288.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test288-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #,#00 1234",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test289
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test289-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test289-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test289.csv")
                       {:body (io/file "w3c-csvw/tests/test289.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test289-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test289.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test289-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #,#00 12,34",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test290
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test290-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test290-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test290.csv")
                       {:body (io/file "w3c-csvw/tests/test290.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test290-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test290.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test290-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #,#00 12,34,567",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test291
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test291-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test291-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test291.csv")
                       {:body (io/file "w3c-csvw/tests/test291.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test291-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test291.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test291-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #,##,#00 1",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test292
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test292-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test292-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test292.csv")
                       {:body (io/file "w3c-csvw/tests/test292.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test292-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test292.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test292-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #,##,#00 1234",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test293
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test293-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test293-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test293.csv")
                       {:body (io/file "w3c-csvw/tests/test293.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test293-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test293.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test293-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #,##,#00 12,34",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test294
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test294-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test294-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test294.csv")
                       {:body (io/file "w3c-csvw/tests/test294.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test294-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test294.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test294-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #,##,#00 1,234,567",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test295
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test295-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test295-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test295.csv")
                       {:body (io/file "w3c-csvw/tests/test295.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test295-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test295.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test295-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #0.# 12.34",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test296
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test296-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test296-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test296.csv")
                       {:body (io/file "w3c-csvw/tests/test296.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test296-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test296.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test296-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #0.# 1,234.5",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test297
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test297-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test297-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test297.csv")
                       {:body (io/file "w3c-csvw/tests/test297.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test297-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test297.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test297-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #0.0 1",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test298
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test298-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test298-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test298.csv")
                       {:body (io/file "w3c-csvw/tests/test298.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test298-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test298.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test298-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #0.0 12.34",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test299
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test299-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test299-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test299.csv")
                       {:body (io/file "w3c-csvw/tests/test299.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test299-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test299.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test299-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #0.0# 1",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test300
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test300-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test300-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test300.csv")
                       {:body (io/file "w3c-csvw/tests/test300.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test300-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test300.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test300-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #0.0# 12.345",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test301
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test301-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test301-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test301.csv")
                       {:body (io/file "w3c-csvw/tests/test301.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test301-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test301.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test301-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #0.0#,# 1",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test302
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test302-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test302-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test302.csv")
                       {:body (io/file "w3c-csvw/tests/test302.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test302-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test302.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test302-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #0.0#,# 12.345",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test303
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test303-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test303-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test303.csv")
                       {:body (io/file "w3c-csvw/tests/test303.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test303-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test303.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test303-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "invalid #0.0#,# 12.34,567",
   :description
   "A number format pattern as defined in [UAX35]. Implementations MUST recognise number format patterns containing the symbols `0`, `#`, the specified decimalChar (or `.` if unspecified), the specified groupChar (or `,` if unspecified), `E`, `+`, `%` and `&permil;`."}
  test304
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test304-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test304-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test304.csv")
                       {:body (io/file "w3c-csvw/tests/test304.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test304-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test304.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test304-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "multiple values with same subject and property (unordered)",
   :description "Values in separate columns using the same propertyUrl are kept in proper relative order."}
  test305
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test305-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test305-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test305.csv")
                       {:body (io/file "w3c-csvw/tests/test305.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test305-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test305.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test305-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "multiple values with same subject and property (ordered)",
   :description "Values in separate columns using the same propertyUrl are kept in proper relative order."}
  test306
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test306-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test306-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test306.csv")
                       {:body (io/file "w3c-csvw/tests/test306.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test306-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test306.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test306-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  ^
  {:title "multiple values with same subject and property (ordered and unordered)",
   :description "Values in separate columns using the same propertyUrl are kept in proper relative order."}
  test307
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test307-metadata.json")
                       {:body (io/file "w3c-csvw/tests/test307-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test307.csv")
                       {:body (io/file "w3c-csvw/tests/test307.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test307-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (test-csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (gio/statements
                                (io/file "w3c-csvw/tests/test307.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test307-metadata.json"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

