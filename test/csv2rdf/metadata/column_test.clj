(ns csv2rdf.metadata.column-test
  (:require [clojure.test :refer :all]
            [csv2rdf.metadata.column :refer :all]
            [csv2rdf.metadata.validator :refer [invalid?]]
            [csv2rdf.logging :as logging]
            [csv2rdf.metadata.test-common :refer [test-context validation-error validates-as]]))

(deftest column-name-test
  (testing "valid column name"
    (let [col-name "description"
          {:keys [warnings result]} (logging/capture-warnings (column-name test-context col-name))]
      (is (empty? warnings))
      (is (= col-name result))))

  (testing "starts with underscore"
    (let [{:keys [warnings result]} (logging/capture-warnings (column-name test-context "_invalid"))]
      (is (= 1 (count warnings)))
      (is (invalid? result))))

  (testing "invalid template variable"
    (let [{:keys [warnings result]} (logging/capture-warnings (column-name test-context "not a valid variable name"))]
      (is (= 1 (count warnings)))
      (is (invalid? result))))

  (testing "invalid type"
    (let [{:keys [warnings result]} (logging/capture-warnings (column-name test-context ["not" "a" "string"]))]
      (is (= 1 (count warnings)))
      (is (invalid? result)))))

(deftest columns-test
  (testing "valid columns"
    (let [{:keys [warnings result]} (logging/capture-warnings (columns test-context [{"name" "col1"}
                                                                                     {"name" "col2" "titles" "title" "virtual" false}
                                                                                     {"name" "col3" "virtual" true}]))]
      (is (empty? warnings))
      (is (= [{:name "col1"}
              {:name "col2" :titles {"und" ["title"]} :virtual false}
              {:name "col3" :virtual true}]
             result))))

  (testing "missing column names"
    (validates-as [{:titles {"und" ["title"]}}
                   {:titles {"und" ["title1" "title2"]}}]
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
                     {:title {"en" ["title1" "title2"]
                                     "fr" ["le title"]}}
                     {:title {"en" ["title2"]}})))

  (testing "intersection between und and language"
    (is (compatible? false {:title {"und" ["title"]}} {:title {"de" ["der title" "title"]}})))

  (testing "name without title"
    (is (compatible? false {:name "column1"} {:title {"en" ["title"]}})))

  (testing "title without name"
    (is (compatible? false {:title {"en" ["title"]}} {:name "column2"})))

  (testing "incompatible"
    (is (= false (compatible? false
                              {:name "column1" :title {"en" ["title"] "fr" ["les title"]}}
                              {:name "column2" :title {"en" ["other title"] "de" ["das title"]}})))))


