(ns csv2rdf.csvw-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [csv2rdf.csvw-test.impl :refer :all]
            [csv2rdf.csvw :as csvw]
            [csv2rdf.http :as http]
            [grafter.rdf :as rdf])
  (:import [java.net URI URL]))

(deftest
  test001
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test001.csv")
                       {:body (io/file "csvw_data/test001.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test001.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test001.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test001.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test005
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test005.csv")
                       {:body (io/file "csvw_data/test005.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test005.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test005.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test005.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test006
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test006.csv")
                       {:body (io/file "csvw_data/test006.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test006.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test006.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test006.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test007
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test007.csv")
                       {:body (io/file "csvw_data/test007.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test007.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test007.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test007.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test008
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test008.csv")
                       {:body (io/file "csvw_data/test008.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test008.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test008.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test008.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test009
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test009.csv")
                       {:body (io/file "csvw_data/test009.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test009.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test009.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test009.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test010
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test010.csv")
                       {:body (io/file "csvw_data/test010.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test010.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test010.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test010.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test011
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv")
                       {:body (io/file "csvw_data/test011/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv-metadata.json")
                       {:body (io/file "csvw_data/test011/tree-ops.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test011/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test012
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv")
                       {:body (io/file "csvw_data/test012/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test012/csv-metadata.json")
                       {:body (io/file "csvw_data/test012/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test012/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test013
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test013-user-metadata.json")
                       {:body (io/file "csvw_data/test013-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test013-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test013.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test014
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv")
                       {:body (io/file "csvw_data/test014/tree-ops.csv"),
                        :headers
                        {"Content-Type" "text/csv; charset=UTF-8",
                         "Link" "<linked-metadata.json>; rel=\"describedby\"; type=\"application/csvm+json\""}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test014/linked-metadata.json")
                       {:body (io/file "csvw_data/test014/linked-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test014/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test015
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv")
                       {:body (io/file "csvw_data/test015/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test015/user-metadata.json")
                       {:body (io/file "csvw_data/test015/user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test015/csv-metadata.json")
                       {:body (io/file "csvw_data/test015/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test015/user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test015/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test016
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv")
                       {:body (io/file "csvw_data/test016/tree-ops.csv"),
                        :headers
                        {"Content-Type" "text/csv; charset=UTF-8",
                         "Link" "<linked-metadata.json>; rel=\"describedby\"; type=\"application/csvm+json\""}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test016/linked-metadata.json")
                       {:body (io/file "csvw_data/test016/linked-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test016/csv-metadata.json")
                       {:body (io/file "csvw_data/test016/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test016/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test017
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv")
                       {:body (io/file "csvw_data/test017/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv-metadata.json")
                       {:body (io/file "csvw_data/test017/tree-ops.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test017/csv-metadata.json")
                       {:body (io/file "csvw_data/test017/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test017/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test018
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv")
                       {:body (io/file "csvw_data/test018/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test018/user-metadata.json")
                       {:body (io/file "csvw_data/test018/user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv-metadata.json")
                       {:body (io/file "csvw_data/test018/tree-ops.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test018/csv-metadata.json")
                       {:body (io/file "csvw_data/test018/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test018/user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test018/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test023
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test023-user-metadata.json")
                       {:body (io/file "csvw_data/test023-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test023-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test023.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test027
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test027-user-metadata.json")
                       {:body (io/file "csvw_data/test027-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test027-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test027.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test028
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test028.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/countries.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test029
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test029.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/countries.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test030
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/countries.json")
                       {:body (io/file "csvw_data/countries.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/countries.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test030.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test031
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/countries.json")
                       {:body (io/file "csvw_data/countries.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/countries.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test031.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test032
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test032/csv-metadata.json")
                       {:body (io/file "csvw_data/test032/csv-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test032/events-listing.csv")
                       {:body (io/file "csvw_data/test032/events-listing.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test032/csv-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test032/result.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test033
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test033/csv-metadata.json")
                       {:body (io/file "csvw_data/test033/csv-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test033/events-listing.csv")
                       {:body (io/file "csvw_data/test033/events-listing.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test033/csv-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test033/result.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test034
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/data/organizations.csv")
                       {:body (io/file "csvw_data/test034/gov.uk/data/organizations.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/csv-metadata.json")
                       {:body (io/file "csvw_data/test034/csv-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/senior-roles.csv")
                       {:body (io/file "csvw_data/test034/senior-roles.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/schema/organizations.json")
                       {:body (io/file "csvw_data/test034/gov.uk/schema/organizations.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/junior-roles.csv")
                       {:body (io/file "csvw_data/test034/junior-roles.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/schema/senior-roles.json")
                       {:body (io/file "csvw_data/test034/gov.uk/schema/senior-roles.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/schema/professions.json")
                       {:body (io/file "csvw_data/test034/gov.uk/schema/professions.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/data/professions.csv")
                       {:body (io/file "csvw_data/test034/gov.uk/data/professions.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test034/gov.uk/schema/junior-roles.json")
                       {:body (io/file "csvw_data/test034/gov.uk/schema/junior-roles.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test034/csv-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test034/result.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test035
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/data/professions.csv")
                       {:body (io/file "csvw_data/test035/gov.uk/data/professions.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/senior-roles.csv")
                       {:body (io/file "csvw_data/test035/senior-roles.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/schema/professions.json")
                       {:body (io/file "csvw_data/test035/gov.uk/schema/professions.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/data/organizations.csv")
                       {:body (io/file "csvw_data/test035/gov.uk/data/organizations.csv"),
                        :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/schema/junior-roles.json")
                       {:body (io/file "csvw_data/test035/gov.uk/schema/junior-roles.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/junior-roles.csv")
                       {:body (io/file "csvw_data/test035/junior-roles.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/schema/organizations.json")
                       {:body (io/file "csvw_data/test035/gov.uk/schema/organizations.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/gov.uk/schema/senior-roles.json")
                       {:body (io/file "csvw_data/test035/gov.uk/schema/senior-roles.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test035/csv-metadata.json")
                       {:body (io/file "csvw_data/test035/csv-metadata.json"), :headers {}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test035/csv-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test035/result.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test036
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test036/tree-ops-ext.csv")
                       {:body (io/file "csvw_data/test036/tree-ops-ext.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test036/tree-ops-ext.csv-metadata.json")
                       {:body (io/file "csvw_data/test036/tree-ops-ext.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test036/tree-ops-ext.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test036/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test036/tree-ops-ext.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test037
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test037/tree-ops-ext.csv")
                       {:body (io/file "csvw_data/test037/tree-ops-ext.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test037/tree-ops-ext.csv-metadata.json")
                       {:body (io/file "csvw_data/test037/tree-ops-ext.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test037/tree-ops-ext.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test037/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test037/tree-ops-ext.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test038
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test038-metadata.json")
                       {:body (io/file "csvw_data/test038-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test038.csv")
                       {:body (io/file "csvw_data/test038.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test038-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test038.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test039
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test039-metadata.json")
                       {:body (io/file "csvw_data/test039-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test039.csv")
                       {:body (io/file "csvw_data/test039.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test039-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test039.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test040
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test040-metadata.json")
                       {:body (io/file "csvw_data/test040-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test040.csv")
                       {:body (io/file "csvw_data/test040.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test040-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test040.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test041
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test041-metadata.json")
                       {:body (io/file "csvw_data/test041-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test041.csv")
                       {:body (io/file "csvw_data/test041.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test041-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test041.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test042
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test042-metadata.json")
                       {:body (io/file "csvw_data/test042-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test042.csv")
                       {:body (io/file "csvw_data/test042.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test042-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test042.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test043
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test043-metadata.json")
                       {:body (io/file "csvw_data/test043-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test043.csv")
                       {:body (io/file "csvw_data/test043.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test043-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test043.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test044
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test044-metadata.json")
                       {:body (io/file "csvw_data/test044-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test044.csv")
                       {:body (io/file "csvw_data/test044.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test044-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test044.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test045
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test045-metadata.json")
                       {:body (io/file "csvw_data/test045-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test045.csv")
                       {:body (io/file "csvw_data/test045.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test045-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test045.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test046
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test046-metadata.json")
                       {:body (io/file "csvw_data/test046-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test046.csv")
                       {:body (io/file "csvw_data/test046.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test046-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test046.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test047
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test047-metadata.json")
                       {:body (io/file "csvw_data/test047-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test047.csv")
                       {:body (io/file "csvw_data/test047.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test047-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test047.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test048
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test048-metadata.json")
                       {:body (io/file "csvw_data/test048-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test048.csv")
                       {:body (io/file "csvw_data/test048.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test048-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test048.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test049
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test049-metadata.json")
                       {:body (io/file "csvw_data/test049-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test049.csv")
                       {:body (io/file "csvw_data/test049.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test049-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test049.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test059
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test059-metadata.json")
                       {:body (io/file "csvw_data/test059-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test059-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test059.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test060
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test060-metadata.json")
                       {:body (io/file "csvw_data/test060-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test060-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test060.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test061
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test061-metadata.json")
                       {:body (io/file "csvw_data/test061-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test061-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test061.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test062
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test062-metadata.json")
                       {:body (io/file "csvw_data/test062-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test062-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test062.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test063
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test063-metadata.json")
                       {:body (io/file "csvw_data/test063-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test063-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test063.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test065
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test065-metadata.json")
                       {:body (io/file "csvw_data/test065-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test065-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test065.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test066
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test066-metadata.json")
                       {:body (io/file "csvw_data/test066-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test066-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test066.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test067
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test067-metadata.json")
                       {:body (io/file "csvw_data/test067-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test067-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test067.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test068
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test068-metadata.json")
                       {:body (io/file "csvw_data/test068-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test068-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test068.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test069
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test069-metadata.json")
                       {:body (io/file "csvw_data/test069-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test069-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test069.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test070
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test070-metadata.json")
                       {:body (io/file "csvw_data/test070-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test070-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test070.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test071
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test071-metadata.json")
                       {:body (io/file "csvw_data/test071-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test071-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test071.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test072
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test072-metadata.json")
                       {:body (io/file "csvw_data/test072-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test072-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test072.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test073
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test073-metadata.json")
                       {:body (io/file "csvw_data/test073-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test073-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test073.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test074
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test074-metadata.json")
                       {:body (io/file "csvw_data/test074-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test074-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test075
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test075-metadata.json")
                       {:body (io/file "csvw_data/test075-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test075-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test075.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test076
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test076-metadata.json")
                       {:body (io/file "csvw_data/test076-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test076-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test076.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test077
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test077-metadata.json")
                       {:body (io/file "csvw_data/test077-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test077-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test078
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test078-metadata.json")
                       {:body (io/file "csvw_data/test078-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test078-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test079
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test079-metadata.json")
                       {:body (io/file "csvw_data/test079-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test079-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test080
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test080-metadata.json")
                       {:body (io/file "csvw_data/test080-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test080-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test081
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test081-metadata.json")
                       {:body (io/file "csvw_data/test081-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test081-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test082
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test082-metadata.json")
                       {:body (io/file "csvw_data/test082-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test082-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test083
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test083-metadata.json")
                       {:body (io/file "csvw_data/test083-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test083-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test084
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test084-metadata.json")
                       {:body (io/file "csvw_data/test084-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test084-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test085
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test085-metadata.json")
                       {:body (io/file "csvw_data/test085-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test085-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test086
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test086-metadata.json")
                       {:body (io/file "csvw_data/test086-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test086-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test087
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test087-metadata.json")
                       {:body (io/file "csvw_data/test087-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test087-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test088
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test088-metadata.json")
                       {:body (io/file "csvw_data/test088-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test088-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test089
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test089-metadata.json")
                       {:body (io/file "csvw_data/test089-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test089-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test090
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test090-metadata.json")
                       {:body (io/file "csvw_data/test090-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test090-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test091
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test091.csv")
                       {:body (io/file "csvw_data/test091.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test091.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test093
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test093-metadata.json")
                       {:body (io/file "csvw_data/test093-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test093-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test093.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test095
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test095-metadata.json")
                       {:body (io/file "csvw_data/test095-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test095-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test095.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test097
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test097-metadata.json")
                       {:body (io/file "csvw_data/test097-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test097-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test097.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test098
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test098-metadata.json")
                       {:body (io/file "csvw_data/test098-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test098-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test099
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test099-metadata.json")
                       {:body (io/file "csvw_data/test099-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test099-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test099.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test100
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test100-metadata.json")
                       {:body (io/file "csvw_data/test100-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test100-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test100.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test101
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test101-metadata.json")
                       {:body (io/file "csvw_data/test101-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test101-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test101.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test102
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test102-metadata.json")
                       {:body (io/file "csvw_data/test102-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test102-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test102.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test103
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test103-metadata.json")
                       {:body (io/file "csvw_data/test103-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test103-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test104
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test104-metadata.json")
                       {:body (io/file "csvw_data/test104-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test104-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test105
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test105-metadata.json")
                       {:body (io/file "csvw_data/test105-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test105-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test105.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test106
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test106-metadata.json")
                       {:body (io/file "csvw_data/test106-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test106-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test106.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test107
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test107-metadata.json")
                       {:body (io/file "csvw_data/test107-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test107-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test107.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test108
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test108-metadata.json")
                       {:body (io/file "csvw_data/test108-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test108-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test109
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test109-metadata.json")
                       {:body (io/file "csvw_data/test109-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test109-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test109.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test110
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test110-metadata.json")
                       {:body (io/file "csvw_data/test110-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test110-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test110.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test111
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test111-metadata.json")
                       {:body (io/file "csvw_data/test111-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test111-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test111.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test112
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test112-metadata.json")
                       {:body (io/file "csvw_data/test112-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test112-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test112.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test113
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test113-metadata.json")
                       {:body (io/file "csvw_data/test113-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test113-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test113.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test114
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test114-metadata.json")
                       {:body (io/file "csvw_data/test114-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test114-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test114.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test115
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test115-metadata.json")
                       {:body (io/file "csvw_data/test115-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test115-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test115.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test116
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test116.csv?query")
                       {:body (io/file "csvw_data/test116.csv?query"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test116.csv-metadata.json")
                       {:body (io/file "csvw_data/test116.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test116.csv?query")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test116.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test116.csv?query"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test117
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test117.csv")
                       {:body (io/file "csvw_data/test117.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test117.csv-metadata.json")
                       {:body (io/file "csvw_data/test117.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test117.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test117.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test117.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test118
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test118/action.csv?query")
                       {:body (io/file "csvw_data/test118/action.csv?query"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test118/csv-metadata.json")
                       {:body (io/file "csvw_data/test118/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test118/action.csv?query")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test118/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test118/action.csv?query"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test119
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test119/action.csv")
                       {:body (io/file "csvw_data/test119/action.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test119/csv-metadata.json")
                       {:body (io/file "csvw_data/test119/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test119/action.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test119/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test119/action.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test120
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test120.csv")
                       {:body (io/file "csvw_data/test120.csv"),
                        :headers
                        {"Content-Type" "text/csv; charset=UTF-8",
                         "Link"
                         "<test120-linked-metadata.json>; rel=\"describedby\"; type=\"application/csvm+json\""}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test120-linked-metadata.json")
                       {:body (io/file "csvw_data/test120-linked-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test120.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test120.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test120.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test121
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test121.csv")
                       {:body (io/file "csvw_data/test121.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test121-user-metadata.json")
                       {:body (io/file "csvw_data/test121-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test121-ref.csv")
                       {:body (io/file "csvw_data/test121-ref.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test121.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test121-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test121.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test121.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test122
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test122.csv")
                       {:body (io/file "csvw_data/test122.csv"),
                        :headers
                        {"Content-Type" "text/csv; charset=UTF-8",
                         "Link"
                         "<test122-linked-metadata.json>; rel=\"describedby\"; type=\"application/csvm+json\""}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test122-linked-metadata.json")
                       {:body (io/file "csvw_data/test122-linked-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test122.csv-metadata.json")
                       {:body (io/file "csvw_data/test122.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test122.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test122.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test122.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test123
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test123/action.csv")
                       {:body (io/file "csvw_data/test123/action.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test123/action.csv-metadata.json")
                       {:body (io/file "csvw_data/test123/action.csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test123/csv-metadata.json")
                       {:body (io/file "csvw_data/test123/csv-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test123/action.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test123/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test123/action.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test124
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test124-user-metadata.json")
                       {:body (io/file "csvw_data/test124-user-metadata.json"),
                        :headers {"Content-Type" "application/csvm+json"}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test124-user-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test124.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test125
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test125-metadata.json")
                       {:body (io/file "csvw_data/test125-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test125.csv")
                       {:body (io/file "csvw_data/test125.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test125-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test125.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test126
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test126-metadata.json")
                       {:body (io/file "csvw_data/test126-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test126.csv")
                       {:body (io/file "csvw_data/test126.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test126-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test126.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test127
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test127-metadata.json")
                       {:body (io/file "csvw_data/test127-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test127.csv")
                       {:body (io/file "csvw_data/test127.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test127-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test127.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test128
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test128-metadata.json")
                       {:body (io/file "csvw_data/test128-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test128-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test129
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test129-metadata.json")
                       {:body (io/file "csvw_data/test129-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test129-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test129.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test130
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test130-metadata.json")
                       {:body (io/file "csvw_data/test130-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test130-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test130.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test131
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test131-metadata.json")
                       {:body (io/file "csvw_data/test131-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test131-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test131.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test132
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test132-metadata.json")
                       {:body (io/file "csvw_data/test132-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test132-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test132.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test133
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test133-metadata.json")
                       {:body (io/file "csvw_data/test133-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test133-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test134
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test134-metadata.json")
                       {:body (io/file "csvw_data/test134-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test134-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test135
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test135-metadata.json")
                       {:body (io/file "csvw_data/test135-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test135-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test136
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test136-metadata.json")
                       {:body (io/file "csvw_data/test136-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test136-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test137
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test137-metadata.json")
                       {:body (io/file "csvw_data/test137-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test137-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test138
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test138-metadata.json")
                       {:body (io/file "csvw_data/test138-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test138-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test139
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test139-metadata.json")
                       {:body (io/file "csvw_data/test139-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test139-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test140
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test140-metadata.json")
                       {:body (io/file "csvw_data/test140-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test140-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test141
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test141-metadata.json")
                       {:body (io/file "csvw_data/test141-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test141-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test142
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test142-metadata.json")
                       {:body (io/file "csvw_data/test142-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test142-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test143
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test143-metadata.json")
                       {:body (io/file "csvw_data/test143-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test143-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test144
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test144-metadata.json")
                       {:body (io/file "csvw_data/test144-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test144-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test146
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test146-metadata.json")
                       {:body (io/file "csvw_data/test146-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test146-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test147
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test147-metadata.json")
                       {:body (io/file "csvw_data/test147-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test147-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test147.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test148
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test148-metadata.json")
                       {:body (io/file "csvw_data/test148-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test148-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test148.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test149
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test149-metadata.json")
                       {:body (io/file "csvw_data/test149-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test149-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test149.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test150
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test150-metadata.json")
                       {:body (io/file "csvw_data/test150-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test150-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test150.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test151
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test151-metadata.json")
                       {:body (io/file "csvw_data/test151-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test151-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test151.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test152
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test152-metadata.json")
                       {:body (io/file "csvw_data/test152-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test152.csv")
                       {:body (io/file "csvw_data/test152.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test152-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test152.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test153
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test153-metadata.json")
                       {:body (io/file "csvw_data/test153-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test153.csv")
                       {:body (io/file "csvw_data/test153.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test153-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test153.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test154
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test154-metadata.json")
                       {:body (io/file "csvw_data/test154-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test154.csv")
                       {:body (io/file "csvw_data/test154.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test154-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test154.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test155
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test155-metadata.json")
                       {:body (io/file "csvw_data/test155-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test155.csv")
                       {:body (io/file "csvw_data/test155.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test155-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test155.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test156
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test156-metadata.json")
                       {:body (io/file "csvw_data/test156-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test156.csv")
                       {:body (io/file "csvw_data/test156.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test156-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test156.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test157
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test157-metadata.json")
                       {:body (io/file "csvw_data/test157-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test157.csv")
                       {:body (io/file "csvw_data/test157.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test157-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test157.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test158
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test158-metadata.json")
                       {:body (io/file "csvw_data/test158-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test158.csv")
                       {:body (io/file "csvw_data/test158.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test158-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test158.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test159
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test159-metadata.json")
                       {:body (io/file "csvw_data/test159-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test159.csv")
                       {:body (io/file "csvw_data/test159.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test159-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test159.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test160
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test160-metadata.json")
                       {:body (io/file "csvw_data/test160-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test160.csv")
                       {:body (io/file "csvw_data/test160.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test160-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test160.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test161
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test161-metadata.json")
                       {:body (io/file "csvw_data/test161-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test161.csv")
                       {:body (io/file "csvw_data/test161.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test161-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test161.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test162
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test162-metadata.json")
                       {:body (io/file "csvw_data/test162-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test162.csv")
                       {:body (io/file "csvw_data/test162.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test162-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test162.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test163
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test163-metadata.json")
                       {:body (io/file "csvw_data/test163-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test163.csv")
                       {:body (io/file "csvw_data/test163.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test163-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test163.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test164
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test164-metadata.json")
                       {:body (io/file "csvw_data/test164-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test164.csv")
                       {:body (io/file "csvw_data/test164.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test164-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test164.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test165
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test165-metadata.json")
                       {:body (io/file "csvw_data/test165-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test165.csv")
                       {:body (io/file "csvw_data/test165.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test165-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test165.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test166
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test166-metadata.json")
                       {:body (io/file "csvw_data/test166-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test166.csv")
                       {:body (io/file "csvw_data/test166.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test166-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test166.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test167
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test167-metadata.json")
                       {:body (io/file "csvw_data/test167-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test167.csv")
                       {:body (io/file "csvw_data/test167.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test167-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test167.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test168
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test168-metadata.json")
                       {:body (io/file "csvw_data/test168-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test168.csv")
                       {:body (io/file "csvw_data/test168.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test168-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test168.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test169
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test169-metadata.json")
                       {:body (io/file "csvw_data/test169-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test169.csv")
                       {:body (io/file "csvw_data/test169.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test169-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test169.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test170
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test170-metadata.json")
                       {:body (io/file "csvw_data/test170-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test170.csv")
                       {:body (io/file "csvw_data/test170.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test170-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test170.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test171
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test171-metadata.json")
                       {:body (io/file "csvw_data/test171-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test171.csv")
                       {:body (io/file "csvw_data/test171.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test171-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test171.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test172
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test172-metadata.json")
                       {:body (io/file "csvw_data/test172-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test172.csv")
                       {:body (io/file "csvw_data/test172.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test172-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test172.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test173
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test173-metadata.json")
                       {:body (io/file "csvw_data/test173-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test173.csv")
                       {:body (io/file "csvw_data/test173.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test173-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test173.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test174
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test174-metadata.json")
                       {:body (io/file "csvw_data/test174-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test174.csv")
                       {:body (io/file "csvw_data/test174.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test174-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test174.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test175
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test175-metadata.json")
                       {:body (io/file "csvw_data/test175-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test175.csv")
                       {:body (io/file "csvw_data/test175.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test175-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test175.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test176
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test176-metadata.json")
                       {:body (io/file "csvw_data/test176-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test176.csv")
                       {:body (io/file "csvw_data/test176.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test176-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test176.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test177
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test177-metadata.json")
                       {:body (io/file "csvw_data/test177-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test177.csv")
                       {:body (io/file "csvw_data/test177.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test177-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test177.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test178
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test178-metadata.json")
                       {:body (io/file "csvw_data/test178-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test178.csv")
                       {:body (io/file "csvw_data/test178.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test178-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test178.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test179
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test179-metadata.json")
                       {:body (io/file "csvw_data/test179-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test179.csv")
                       {:body (io/file "csvw_data/test179.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test179-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test179.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test180
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test180-metadata.json")
                       {:body (io/file "csvw_data/test180-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test180.csv")
                       {:body (io/file "csvw_data/test180.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test180-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test180.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test181
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test181-metadata.json")
                       {:body (io/file "csvw_data/test181-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test181.csv")
                       {:body (io/file "csvw_data/test181.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test181-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test181.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test182
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test182-metadata.json")
                       {:body (io/file "csvw_data/test182-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test182.csv")
                       {:body (io/file "csvw_data/test182.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test182-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test182.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test183
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test183-metadata.json")
                       {:body (io/file "csvw_data/test183-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test183.csv")
                       {:body (io/file "csvw_data/test183.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test183-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test183.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test184
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test184-metadata.json")
                       {:body (io/file "csvw_data/test184-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test184.csv")
                       {:body (io/file "csvw_data/test184.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test184-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test184.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test185
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test185-metadata.json")
                       {:body (io/file "csvw_data/test185-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test185.csv")
                       {:body (io/file "csvw_data/test185.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test185-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test185.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test186
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test186-metadata.json")
                       {:body (io/file "csvw_data/test186-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test186.csv")
                       {:body (io/file "csvw_data/test186.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test186-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test186.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test187
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test187-metadata.json")
                       {:body (io/file "csvw_data/test187-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test187.csv")
                       {:body (io/file "csvw_data/test187.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test187-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test187.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test188
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test188-metadata.json")
                       {:body (io/file "csvw_data/test188-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test188.csv")
                       {:body (io/file "csvw_data/test188.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test188-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test188.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test189
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test189-metadata.json")
                       {:body (io/file "csvw_data/test189-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test189.csv")
                       {:body (io/file "csvw_data/test189.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test189-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test189.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test190
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test190-metadata.json")
                       {:body (io/file "csvw_data/test190-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test190.csv")
                       {:body (io/file "csvw_data/test190.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test190-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test190.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test191
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test191-metadata.json")
                       {:body (io/file "csvw_data/test191-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test191.csv")
                       {:body (io/file "csvw_data/test191.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test191-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test191.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test192
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test192-metadata.json")
                       {:body (io/file "csvw_data/test192-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test192.csv")
                       {:body (io/file "csvw_data/test192.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test192-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test192.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test193
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test193-metadata.json")
                       {:body (io/file "csvw_data/test193-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test193.csv")
                       {:body (io/file "csvw_data/test193.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test193-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test193.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test194
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test194-metadata.json")
                       {:body (io/file "csvw_data/test194-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test194.csv")
                       {:body (io/file "csvw_data/test194.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test194-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test194.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test195
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test195-metadata.json")
                       {:body (io/file "csvw_data/test195-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test195.csv")
                       {:body (io/file "csvw_data/test195.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test195-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test195.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test196
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test196-metadata.json")
                       {:body (io/file "csvw_data/test196-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test196.csv")
                       {:body (io/file "csvw_data/test196.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test196-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test196.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test197
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test197-metadata.json")
                       {:body (io/file "csvw_data/test197-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test197.csv")
                       {:body (io/file "csvw_data/test197.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test197-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test197.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test198
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test198-metadata.json")
                       {:body (io/file "csvw_data/test198-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test198.csv")
                       {:body (io/file "csvw_data/test198.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test198-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test198.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test199
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test199-metadata.json")
                       {:body (io/file "csvw_data/test199-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test199.csv")
                       {:body (io/file "csvw_data/test199.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test199-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test200
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test200-metadata.json")
                       {:body (io/file "csvw_data/test200-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test200.csv")
                       {:body (io/file "csvw_data/test200.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test200-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test201
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test201-metadata.json")
                       {:body (io/file "csvw_data/test201-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test201.csv")
                       {:body (io/file "csvw_data/test201.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test201-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test202
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test202-metadata.json")
                       {:body (io/file "csvw_data/test202-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test202.csv")
                       {:body (io/file "csvw_data/test202.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test202-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test202.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test203
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test203-metadata.json")
                       {:body (io/file "csvw_data/test203-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test203.csv")
                       {:body (io/file "csvw_data/test203.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test203-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test203.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test204
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test204-metadata.json")
                       {:body (io/file "csvw_data/test204-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test204.csv")
                       {:body (io/file "csvw_data/test204.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test204-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test204.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test205
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test205-metadata.json")
                       {:body (io/file "csvw_data/test205-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test205.csv")
                       {:body (io/file "csvw_data/test205.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test205-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test205.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test206
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test206-metadata.json")
                       {:body (io/file "csvw_data/test206-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test206.csv")
                       {:body (io/file "csvw_data/test206.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test206-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test206.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test207
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test207-metadata.json")
                       {:body (io/file "csvw_data/test207-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test207.csv")
                       {:body (io/file "csvw_data/test207.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test207-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test207.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test208
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test208-metadata.json")
                       {:body (io/file "csvw_data/test208-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test208.csv")
                       {:body (io/file "csvw_data/test208.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test208-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test208.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test209
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test209-metadata.json")
                       {:body (io/file "csvw_data/test209-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test209.csv")
                       {:body (io/file "csvw_data/test209.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test209-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test209.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test210
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test210-metadata.json")
                       {:body (io/file "csvw_data/test210-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test210.csv")
                       {:body (io/file "csvw_data/test210.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test210-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test210.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test211
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test211-metadata.json")
                       {:body (io/file "csvw_data/test211-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test211.csv")
                       {:body (io/file "csvw_data/test211.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test211-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test211.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test212
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test212-metadata.json")
                       {:body (io/file "csvw_data/test212-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test212.csv")
                       {:body (io/file "csvw_data/test212.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test212-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test212.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test213
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test213-metadata.json")
                       {:body (io/file "csvw_data/test213-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test213.csv")
                       {:body (io/file "csvw_data/test213.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test213-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test213.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test214
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test214-metadata.json")
                       {:body (io/file "csvw_data/test214-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test214.csv")
                       {:body (io/file "csvw_data/test214.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test214-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test214.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test215
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test215-metadata.json")
                       {:body (io/file "csvw_data/test215-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test215.csv")
                       {:body (io/file "csvw_data/test215.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test215-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test215.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test216
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test216-metadata.json")
                       {:body (io/file "csvw_data/test216-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test216.csv")
                       {:body (io/file "csvw_data/test216.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test216-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test217
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test217-metadata.json")
                       {:body (io/file "csvw_data/test217-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test217.csv")
                       {:body (io/file "csvw_data/test217.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test217-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test218
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test218-metadata.json")
                       {:body (io/file "csvw_data/test218-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test218.csv")
                       {:body (io/file "csvw_data/test218.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test218-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test219
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test219-metadata.json")
                       {:body (io/file "csvw_data/test219-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test219.csv")
                       {:body (io/file "csvw_data/test219.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test219-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test220
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test220-metadata.json")
                       {:body (io/file "csvw_data/test220-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test220.csv")
                       {:body (io/file "csvw_data/test220.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test220-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test221
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test221-metadata.json")
                       {:body (io/file "csvw_data/test221-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test221.csv")
                       {:body (io/file "csvw_data/test221.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test221-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test222
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test222-metadata.json")
                       {:body (io/file "csvw_data/test222-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test222.csv")
                       {:body (io/file "csvw_data/test222.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test222-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test223
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test223-metadata.json")
                       {:body (io/file "csvw_data/test223-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test223.csv")
                       {:body (io/file "csvw_data/test223.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test223-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test224
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test224-metadata.json")
                       {:body (io/file "csvw_data/test224-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test224.csv")
                       {:body (io/file "csvw_data/test224.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test224-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test225
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test225-metadata.json")
                       {:body (io/file "csvw_data/test225-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test225.csv")
                       {:body (io/file "csvw_data/test225.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test225-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test226
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test226-metadata.json")
                       {:body (io/file "csvw_data/test226-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test226.csv")
                       {:body (io/file "csvw_data/test226.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test226-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test227
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test227-metadata.json")
                       {:body (io/file "csvw_data/test227-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test227.csv")
                       {:body (io/file "csvw_data/test227.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test227-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test228
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test228-metadata.json")
                       {:body (io/file "csvw_data/test228-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test228.csv")
                       {:body (io/file "csvw_data/test228.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test228-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test228.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test229
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test229-metadata.json")
                       {:body (io/file "csvw_data/test229-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test229.csv")
                       {:body (io/file "csvw_data/test229.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test229-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test229.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test230
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test230-metadata.json")
                       {:body (io/file "csvw_data/test230-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test230.csv")
                       {:body (io/file "csvw_data/test230.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test230-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test230.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test231
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test231-metadata.json")
                       {:body (io/file "csvw_data/test231-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test231.csv")
                       {:body (io/file "csvw_data/test231.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test231-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test231.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test232
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test232-metadata.json")
                       {:body (io/file "csvw_data/test232-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test232.csv")
                       {:body (io/file "csvw_data/test232.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test232-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test232.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test233
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test233-metadata.json")
                       {:body (io/file "csvw_data/test233-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test233.csv")
                       {:body (io/file "csvw_data/test233.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test233-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test233.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test234
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test234-metadata.json")
                       {:body (io/file "csvw_data/test234-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test234.csv")
                       {:body (io/file "csvw_data/test234.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test234-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test234.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test235
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test235-metadata.json")
                       {:body (io/file "csvw_data/test235-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test235-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test235.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test236
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test236-metadata.json")
                       {:body (io/file "csvw_data/test236-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test236-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test236.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test237
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test237-metadata.json")
                       {:body (io/file "csvw_data/test237-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test237-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :minimal}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test237.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test238
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test238-metadata.json")
                       {:body (io/file "csvw_data/test238-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test238.csv")
                       {:body (io/file "csvw_data/test238.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test238-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test238.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test242
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test242-metadata.json")
                       {:body (io/file "csvw_data/test242-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test242.csv")
                       {:body (io/file "csvw_data/test242.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test242-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test242.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test243
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test243-metadata.json")
                       {:body (io/file "csvw_data/test243-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test243.csv")
                       {:body (io/file "csvw_data/test243.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test243-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test244
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test244-metadata.json")
                       {:body (io/file "csvw_data/test244-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test244.csv")
                       {:body (io/file "csvw_data/test244.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test244-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test245
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test245-metadata.json")
                       {:body (io/file "csvw_data/test245-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test245.csv")
                       {:body (io/file "csvw_data/test245.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test245-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test245.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test246
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test246-metadata.json")
                       {:body (io/file "csvw_data/test246-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test246.csv")
                       {:body (io/file "csvw_data/test246.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test246-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test246.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test247
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test247-metadata.json")
                       {:body (io/file "csvw_data/test247-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test247.csv")
                       {:body (io/file "csvw_data/test247.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test247-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test247.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test248
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test248-metadata.json")
                       {:body (io/file "csvw_data/test248-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test248.csv")
                       {:body (io/file "csvw_data/test248.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test248-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test248.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test251
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test251-metadata.json")
                       {:body (io/file "csvw_data/test251-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test251-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test252
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test252-metadata.json")
                       {:body (io/file "csvw_data/test252-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test252-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test253
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test253-metadata.json")
                       {:body (io/file "csvw_data/test253-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/country_slice.csv")
                       {:body (io/file "csvw_data/country_slice.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test253-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test259
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test259/tree-ops.csv")
                       {:body (io/file "csvw_data/test259/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test259/csvm.json")
                       {:body (io/file "csvw_data/test259/csvm.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/.well-known/csvm")
                       {:body (URL. "file:/Users/lee/src/csv2rdf/test/resources/csvm"), :headers {}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test259/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test259/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test259/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test260
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test260/tree-ops.csv")
                       {:body (io/file "csvw_data/test260/tree-ops.csv"),
                        :headers {"Content-Type" "text/csv; charset=UTF-8"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test260/tree-ops.csv.json")
                       {:body (io/file "csvw_data/test260/tree-ops.csv.json"),
                        :headers {"Content-Type" "application/csvm+json"}},
                       (URI. "http://www.w3.org/.well-known/csvm")
                       {:body (URL. "file:/Users/lee/src/csv2rdf/test/resources/csvm"), :headers {}}})
        csv-uri (URI. "http://www.w3.org/2013/csvw/tests/test260/tree-ops.csv")
        metadata-uri nil
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements
                                (io/file "csvw_data/test260/result.ttl")
                                :base-uri
                                (URI. "http://www.w3.org/2013/csvw/tests/test260/tree-ops.csv"))]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test261
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test261-metadata.json")
                       {:body (io/file "csvw_data/test261-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test261.csv")
                       {:body (io/file "csvw_data/test261.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test261-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test263
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test263-metadata.json")
                       {:body (io/file "csvw_data/test263-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test263-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test263.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test264
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test264-metadata.json")
                       {:body (io/file "csvw_data/test264-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test264-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test264.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test266
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test266-metadata.json")
                       {:body (io/file "csvw_data/test266-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test266-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test266.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test267
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test267-metadata.json")
                       {:body (io/file "csvw_data/test267-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test267-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test268
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test268-metadata.json")
                       {:body (io/file "csvw_data/test268-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test268-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test268.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test269
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test269-metadata.json")
                       {:body (io/file "csvw_data/test269-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test269.csv")
                       {:body (io/file "csvw_data/test269.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test269-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test269.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test270
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test270-metadata.json")
                       {:body (io/file "csvw_data/test270-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test270-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test270.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test271
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test271-metadata.json")
                       {:body (io/file "csvw_data/test271-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test271.csv")
                       {:body (io/file "csvw_data/test271.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test271-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test272
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test272-metadata.json")
                       {:body (io/file "csvw_data/test272-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test272.csv")
                       {:body (io/file "csvw_data/test272.csv"), :headers {"Content-Type" "text/csv"}},
                       (URI. "http://www.w3.org/2013/csvw/tests/countries.csv")
                       {:body (io/file "csvw_data/countries.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test272-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test273
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test273-metadata.json")
                       {:body (io/file "csvw_data/test273-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test273/action.csv")
                       {:body (io/file "csvw_data/test273/action.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test273-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test273.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test274
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test274-metadata.json")
                       {:body (io/file "csvw_data/test274-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test274-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    nil
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (pos? (count errors)) "Expected errors but none was found")))

(deftest
  test275
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test275-metadata.json")
                       {:body (io/file "csvw_data/test275-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test275-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test275.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test276
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test276-metadata.json")
                       {:body (io/file "csvw_data/test276-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test276-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test276.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test277
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test277-metadata.json")
                       {:body (io/file "csvw_data/test277-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test277-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test277.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test278
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test278-metadata.json")
                       {:body (io/file "csvw_data/test278-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/tree-ops.csv")
                       {:body (io/file "csvw_data/tree-ops.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test278-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test278.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test279
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test279-metadata.json")
                       {:body (io/file "csvw_data/test279-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test279.csv")
                       {:body (io/file "csvw_data/test279.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test279-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test279.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test280
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test280-metadata.json")
                       {:body (io/file "csvw_data/test280-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test280.csv")
                       {:body (io/file "csvw_data/test280.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test280-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test280.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test281
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test281-metadata.json")
                       {:body (io/file "csvw_data/test281-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test281.csv")
                       {:body (io/file "csvw_data/test281.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test281-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test281.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test282
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test282-metadata.json")
                       {:body (io/file "csvw_data/test282-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test282.csv")
                       {:body (io/file "csvw_data/test282.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test282-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test282.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test283
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test283-metadata.json")
                       {:body (io/file "csvw_data/test283-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test283.csv")
                       {:body (io/file "csvw_data/test283.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test283-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test283.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test284
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test284-metadata.json")
                       {:body (io/file "csvw_data/test284-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test284.csv")
                       {:body (io/file "csvw_data/test284.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test284-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test284.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test285
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test285-metadata.json")
                       {:body (io/file "csvw_data/test285-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test285.csv")
                       {:body (io/file "csvw_data/test285.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test285-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test285.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test286
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test286-metadata.json")
                       {:body (io/file "csvw_data/test286-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test286.csv")
                       {:body (io/file "csvw_data/test286.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test286-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test286.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test287
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test287-metadata.json")
                       {:body (io/file "csvw_data/test287-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test287.csv")
                       {:body (io/file "csvw_data/test287.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test287-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test287.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test288
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test288-metadata.json")
                       {:body (io/file "csvw_data/test288-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test288.csv")
                       {:body (io/file "csvw_data/test288.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test288-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test288.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test289
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test289-metadata.json")
                       {:body (io/file "csvw_data/test289-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test289.csv")
                       {:body (io/file "csvw_data/test289.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test289-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test289.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test290
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test290-metadata.json")
                       {:body (io/file "csvw_data/test290-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test290.csv")
                       {:body (io/file "csvw_data/test290.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test290-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test290.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test291
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test291-metadata.json")
                       {:body (io/file "csvw_data/test291-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test291.csv")
                       {:body (io/file "csvw_data/test291.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test291-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test291.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test292
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test292-metadata.json")
                       {:body (io/file "csvw_data/test292-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test292.csv")
                       {:body (io/file "csvw_data/test292.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test292-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test292.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test293
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test293-metadata.json")
                       {:body (io/file "csvw_data/test293-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test293.csv")
                       {:body (io/file "csvw_data/test293.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test293-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test293.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test294
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test294-metadata.json")
                       {:body (io/file "csvw_data/test294-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test294.csv")
                       {:body (io/file "csvw_data/test294.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test294-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test294.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test295
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test295-metadata.json")
                       {:body (io/file "csvw_data/test295-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test295.csv")
                       {:body (io/file "csvw_data/test295.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test295-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test295.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test296
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test296-metadata.json")
                       {:body (io/file "csvw_data/test296-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test296.csv")
                       {:body (io/file "csvw_data/test296.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test296-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test296.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test297
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test297-metadata.json")
                       {:body (io/file "csvw_data/test297-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test297.csv")
                       {:body (io/file "csvw_data/test297.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test297-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test297.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test298
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test298-metadata.json")
                       {:body (io/file "csvw_data/test298-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test298.csv")
                       {:body (io/file "csvw_data/test298.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test298-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test298.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test299
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test299-metadata.json")
                       {:body (io/file "csvw_data/test299-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test299.csv")
                       {:body (io/file "csvw_data/test299.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test299-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test299.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test300
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test300-metadata.json")
                       {:body (io/file "csvw_data/test300-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test300.csv")
                       {:body (io/file "csvw_data/test300.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test300-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test300.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test301
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test301-metadata.json")
                       {:body (io/file "csvw_data/test301-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test301.csv")
                       {:body (io/file "csvw_data/test301.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test301-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test301.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test302
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test302-metadata.json")
                       {:body (io/file "csvw_data/test302-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test302.csv")
                       {:body (io/file "csvw_data/test302.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test302-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test302.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test303
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test303-metadata.json")
                       {:body (io/file "csvw_data/test303-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test303.csv")
                       {:body (io/file "csvw_data/test303.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test303-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test303.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test304
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test304-metadata.json")
                       {:body (io/file "csvw_data/test304-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test304.csv")
                       {:body (io/file "csvw_data/test304.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test304-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test304.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (pos? (count warnings)) "Expected warnings but none was found")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test305
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test305-metadata.json")
                       {:body (io/file "csvw_data/test305-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test305.csv")
                       {:body (io/file "csvw_data/test305.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test305-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test305.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test306
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test306-metadata.json")
                       {:body (io/file "csvw_data/test306-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test306.csv")
                       {:body (io/file "csvw_data/test306.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test306-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test306.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

(deftest
  test307
  (let [http-client (->TestHttpClient
                      {(URI. "http://www.w3.org/2013/csvw/tests/test307-metadata.json")
                       {:body (io/file "csvw_data/test307-metadata.json"), :headers {}},
                       (URI. "http://www.w3.org/2013/csvw/tests/test307.csv")
                       {:body (io/file "csvw_data/test307.csv"), :headers {"Content-Type" "text/csv"}}})
        csv-uri nil
        metadata-uri (URI. "http://www.w3.org/2013/csvw/tests/test307-metadata.json")
        {:keys [warnings errors result]} (http/with-http-client
                                           http-client
                                           (csvw/csv->rdf csv-uri metadata-uri {:mode :standard}))]
    (let [expected-statements (rdf/statements (io/file "csvw_data/test307.ttl") :base-uri nil)]
      (is (= true (is-isomorphic? expected-statements result))))
    (is (= 0 (count warnings)) "Received warnings but none was expected")
    (is (= 0 (count errors)) "Received errors but none was expected")))

