(ns csv2rdf.metadata.column-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.column :refer :all :as column]
            [csv2rdf.metadata.validator :refer [invalid?]]
            [csv2rdf.metadata.test-common :refer [test-context validation-error validates-as warns-invalid]]))

(deftest column-name-test
  (testing "valid column name"
    (let [col-name "description"]
      (validates-as col-name (column-name test-context col-name))))

  (testing "starts with underscore"
    (warns-invalid (column-name test-context "_invalid")))

  (testing "invalid template variable"
    (warns-invalid (column-name test-context "not a valid variable name")))

  (testing "invalid type"
    (warns-invalid (column-name test-context ["not" "a" "string"]))))

(deftest columns-test
  (testing "valid columns"
    (validates-as [{:name "col1" ::column/name "col1"}
                   {:name "col2" :titles {"und" ["title"]} :virtual false ::column/name "col2"}
                   {:name "col3" :virtual true ::column/name "col3"}]
                  (columns test-context [{"name" "col1"}
                                         {"name" "col2" "titles" "title" "virtual" false}
                                         {"name" "col3" "virtual" true}])))

  (testing "missing column names"
    (validates-as [{:titles {"und" ["title"]} ::column/name "title"}
                   {:titles {"und" ["title1" "title2"]} ::column/name "title1"}]
                  (columns test-context [{"titles" "title"}
                                         {"titles" ["title1" "title2"]}])))

  (testing "duplicate column names"
    (is (validation-error (columns test-context [{"name" "col" "titles" "title"}
                                                 {"name" "col" "titles" "other"}]))))


  (testing "non-virtual column following virtual column"
    (is (validation-error (columns test-context [{"name" "col1" "titles" "title"}
                                                 {"name" "col2" "virtual" true}
                                                 {"name" "col3"}])))))

(deftest compatible?-test
  (testing "without name or title properties"
    (is (compatible? false {} {:virtual false})))

  (testing "case-sensitive match between names"
    (is (compatible? false {:name "column"} {:name "column"})))

  (testing "intersection between titles with language"
    (is (compatible? false
                     {:titles {"en" ["title1" "title2"]
                                     "fr" ["le title"]}}
                     {:titles {"en" ["title2"]}})))

  (testing "intersection between und and language"
    (is (compatible? false {:titles {"und" ["title"]}} {:titles {"de" ["der title" "title"]}})))

  (testing "name without title"
    (is (compatible? false {:name "column1"} {:titles {"en" ["title"]}})))

  (testing "title without name"
    (is (compatible? false {:titles {"en" ["title"]}} {:name "column2"})))

  (testing "incompatible"
    (is (= false (compatible? false
                              {:name "column1" :titles {"en" ["title"] "fr" ["les title"]}}
                              {:name "column2" :titles {"en" ["other title"] "de" ["das title"]}}))))

  (testing "incompatible titles"
    (is (= false (compatible? false
                              {:titles {"und" ["title1"]}}
                              {:titles {"und" ["title2"]}})))))


