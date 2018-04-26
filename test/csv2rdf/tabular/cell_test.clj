(ns csv2rdf.tabular.cell-test
  (:require [clojure.test :refer :all]
            [csv2rdf.tabular.cell :refer :all]))

(deftest parse-cell-string-type-test
  (testing "Maintains whitespace"
    (let [s "\r\ncontains\t   whitespace\t\t   "
          cell (parse-cell s {:datatype {:base "string"}})]
      (is (= s (semantic-value cell)))))

  (testing "Applies default"
    (let [default "default"
          cell (parse-cell "" {:datatype {:base "string"} :default default})]
      (is (= default (semantic-value cell)))))

  (testing "Applies null"
    (let [null-values ["null1" "null2"]
          cell (parse-cell "null2" {:datatype {:base "string"} :null null-values})]
      (is (nil? (semantic-value cell)))))

  (testing "Null for required column"
    (let [null-values ["null"]
          cell (parse-cell "null" {:datatype {:base "string"} :null null-values :required true})]
      (is (not (empty? (errors cell))))))

  (testing "Invalid exact length"
    (let [cell (parse-cell "value" {:datatype {:base "string"} :length 4})]
      (is (not (empty? (errors cell))))))

  (testing "Invalid min length"
    (let [cell (parse-cell "value" {:datatype {:base "string"} :minLength 10})]
      (is (not (empty? (errors cell))))))

  (testing "Invalid max length"
    (let [cell (parse-cell "value" {:datatype {:base "string"} :maxLength 3})]
      (is (not (empty? (errors cell))))))

  (testing "Multiple values"
    (let [cell (parse-cell "a|b|c" {:datatype {:base "string"} :separator "|"})]
      (is (= ["a" "b" "c"] (semantic-value cell)))))

  (testing "Empty with separator"
    (let [cell (parse-cell "" {:datatype {:base "string"} :separator "|"})]
      (is (= [] (semantic-value cell)))))

  (testing "Empty with separator when required"
    (let [cell (parse-cell "" {:datatype {:base "string"} :separator "|" :required true})]
      (is (not (empty? (errors cell))))))

  (testing "Default with separator"
    (let [default "default"
          cell (parse-cell "" {:datatype {:base "string"} :separator "|" :default default})]
      (is (= [default] (semantic-value cell)))))

  (testing "Null with separator"
    (let [null-values ["null"]
          cell (parse-cell "null" {:datatype {:base "string"} :separator "|" :null null-values})]
      (is (nil? (semantic-value cell)))))

  (testing "Should set column lang on cell"
    (let [col-lang "en"
          cell (parse-cell "a|b|c" {:datatype {:base "string"} :separator "|" :lang col-lang})]
      (= col-lang (:lang cell))))

  (testing "Multiple values with exact length violation"
    (let [cell (parse-cell "foo|bar|quux|qaal" {:datatype {:base "string"} :separator "|" :length 3})]
      (is (= 2 (count (errors cell))))))

  (testing "Multiple values with min length violations"
    (let [cell (parse-cell "a|b|foo|bar||" {:datatype {:base "string"} :separator "|" :minLength 3})]
      (is (= 2 (count (errors cell))))))

  (testing "Multiple values with max length violations"
    (let [cell (parse-cell "foo|bar|quux|qaal" {:datatype {:base "string"} :separator "|" :maxLength 3})]
      (is (= 2 (count (errors cell)))))))
