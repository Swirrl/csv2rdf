(ns csv2rdf.main-test
  (:require [csv2rdf.logging :as logging]
            [csv2rdf.main :as sut]
            [clojure.test :as t]))

;; See issue 47
;; Resolving template property URIs with values containing spaces should work
(defn test-validate-data [tabular-file
                          metadata-file
                          failures]
  (let [is-valid? (empty? failures)
        warnings (atom [])
        errors (atom [])]
    (logging/with-logger
      (logging/memory-logger warnings errors)
      (t/is (= (sut/inner-main ["-t" tabular-file
                                "-u" metadata-file
                                "--validate-data"])
               {:data-validation-errors? (not is-valid?)}))
      (t/is (= @warnings failures))
      (t/is (= @errors [])))))

(t/deftest inner-main-test-validate-data
  (t/testing "--validate-data"

    (test-validate-data
      "./test/examples/validation/success.csv"
      "./test/examples/validation/named-numbers.json"
      [])
    (test-validate-data
      "./test/examples/validation/fail-1.csv"
      "./test/examples/validation/named-numbers.json"
      ["Row #3 col #2 (column 'number') in file: fail-1.csv has error: Cannot parse 'two' as type 'int': For input string: \"two\""])
    (test-validate-data
      "./test/examples/validation/fail-2.csv"
      "./test/examples/validation/named-numbers.json"
      ["Row #3 col #2 (column 'number') in file: fail-2.csv has error: Cannot parse 'three' as type 'int': For input string: \"three\""])
    (test-validate-data
      "./test/examples/validation/fail-3.csv"
      "./test/examples/validation/named-numbers.json"
      ["Row #3 col #2 (column 'number') in file: fail-3.csv has error: Cannot parse 'three' as type 'int': For input string: \"three\""
       "Row #4 col #2 (column 'number') in file: fail-3.csv has error: Cannot parse 'four' as type 'int': For input string: \"four\""
       "Row #5 col #2 (column 'number') in file: fail-3.csv has error: Cannot parse 'five' as type 'int': For input string: \"five\""])

    (test-validate-data
      "./test/examples/validation/fail-4.csv"
      "./test/examples/validation/named-numbers.json"
      ["Row #3 col #2 (column 'number') in file: fail-4.csv has error: Column value required"])
    (test-validate-data
      "./test/examples/validation/success.csv"
      "./test/examples/validation/named-numbers-incorrect-schema.json"
      ["Row #2 col #1 (column 'name') in file: success.csv has error: Cannot parse 'one' as type 'int': For input string: \"one\""
       "Row #3 col #1 (column 'name') in file: success.csv has error: Cannot parse 'two' as type 'int': For input string: \"two\""
       "Row #4 col #1 (column 'name') in file: success.csv has error: Cannot parse 'three' as type 'int': For input string: \"three\""
       "Row #5 col #1 (column 'name') in file: success.csv has error: Cannot parse 'four' as type 'int': For input string: \"four\""
       "Row #6 col #1 (column 'name') in file: success.csv has error: Cannot parse 'five' as type 'int': For input string: \"five\""])))
