(ns csv2rdf.metadata.validator-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.validator :refer :all]
            [csv2rdf.logging :as logging]))

(deftest eq-test
  (let [v (eq "value")]
    (testing "Matches value"
      (let [{:keys [warnings result]} (logging/capture-warnings (v {} "value"))]
        (is (empty? warnings))
        (is (= "value" result))))

    (testing "Does not match value"
      (let [{:keys [warnings result]} (logging/capture-warnings (v {} "other value"))]
        (is (= 1 (count warnings)))
        (is (nil? result))))))


