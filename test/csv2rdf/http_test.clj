(ns csv2rdf.http-test
  (:require [csv2rdf.http :refer :all :as http]
            [clojure.test :refer :all])
  (:import [java.net URI]
           [clojure.lang ExceptionInfo]))

(deftest parse-link-header-test
  (testing "Valid header URI only"
    (is (= {::http/link-uri (URI. "metadata.json")}
           (parse-link-header "<metadata.json>"))))

  (testing "Valid header with parameters"
    (let [header "<metadata.json>; rel=\"describedBy\"; type=\"application/csvm+json\""]
      (is (= {::http/link-uri (URI. "metadata.json")
              :rel "describedBy"
              :type "application/csvm+json"}
             (parse-link-header header)))))

  (testing "Invalid URI"
    (is (thrown? ExceptionInfo (parse-link-header "<not a URI>; rel=\"describedby\""))))

  (testing "Malformed"
    (is (thrown? ExceptionInfo (parse-link-header "Invalid format")))))


