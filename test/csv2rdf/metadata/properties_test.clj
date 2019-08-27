(ns csv2rdf.metadata.properties-test
  (:require [csv2rdf.metadata.properties :refer :all]
            [clojure.test :refer :all]
            [csv2rdf.test-common :refer [remove-parent-references]])
  (:import [java.net URI]))

(deftest child-test
  (testing "Missing child key"
    (let [parent-rec {:foo :bar}
          f (child :child)
          with-parent (f parent-rec)]
      (is (= parent-rec (f parent-rec)))))

  (testing "Existing child key"
    (let [child-rec {:foo :bar}
          parent-rec {:child child-rec}
          f (child :child)
          {:keys [child] :as with-parent} (f parent-rec)]
      (is (= child-rec (remove-parent-references child)))
      (is (= parent-rec (parent child))))))

(deftest children-test
  (testing "Missing children key"
    (let [parent-rec {:foo :bar}
          f (children :children)
          with-parents (f parent-rec)]
      (is (= parent-rec with-parents))))

  (testing "With children key"
    (let [parent-rec {:foo :bar
                      :children [{:name :child1} {:name :child2}]}
          f (children :children)
          {:keys [children] :as with-parents} (f parent-rec)]
      (is (= (count (:children parent-rec)) (count children)))
      (is (every? (fn [child] = parent-rec (parent child)) children)))))

(deftest set-table-group-parent-references-test
  (testing "Should resolve table group schema"
    (let [schema {:columns [{:name "col1"} {:name "col2"}]}
          table-group {:tableSchema schema
                       :tables [{:url (URI. "http://example.com/table.csv")}]}
          with-parents (set-table-group-parent-references table-group)
          table (get-in with-parents [:tables 0])]
      (is (= schema (table-schema table)))))

  (testing "Should resolve table schema"
    (let [schema {:columns [{:name "col1"} {:name "col2"}]}
          table-group {:tableSchema {:columns [{:name "default1"} {:name "default2"}]}
                       :tables [{:url (URI. "http://example.com/table.csv")
                                 :tableSchema schema}]}
          with-parents (set-table-group-parent-references table-group)
          table (get-in with-parents [:tables 0])]
      (is (= schema (remove-parent-references (table-schema table)))))))
