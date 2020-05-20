(ns csv2rdf.source-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [csv2rdf.http :as http]
            [csv2rdf.source :refer :all]
            [csv2rdf.test-common :refer [->TestHttpClient]])
  (:import java.net.URI))

(deftest get-json-test
  (testing "URI"
    (testing "http scheme"
      (let [uri (URI. "http://example.com")
            json {"objects" [{"name" "first"} {"name" "second"}]}
            requests {uri {:body (json/write-str json)}}]
        (http/with-http-client
          (->TestHttpClient requests)
          (let [result (get-json uri)]
            (is (= json result))))))
    (testing "file scheme"
      (let [uri (.toURI (io/file "w3c-csvw/tests/test104.json"))
            json {}
            result (get-json uri)]
        (is (= json result))))))
