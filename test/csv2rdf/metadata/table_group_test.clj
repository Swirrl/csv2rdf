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
