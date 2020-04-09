(ns csv2rdf.source-test
  (:require [clojure.test :refer :all]
            [csv2rdf.source :refer :all]
            [clojure.data.json :as json]
            [csv2rdf.http :as http]
            [csv2rdf.test-common :refer [->TestHttpClient]])
  (:import [java.net URI]))

(deftest get-json-test
  (testing "URI"
    (let [uri (URI. "http://example.com")
          json {"objects" [{"name" "first"} {"name" "second"}]}
          requests {uri {:body (json/write-str json)}}]
      (http/with-http-client
        (->TestHttpClient requests)
        (let [result (get-json uri)]
          (is (= json result)))))))
