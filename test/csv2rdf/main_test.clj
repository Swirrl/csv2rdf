(ns csv2rdf.main-test
  (:require [csv2rdf.main :as sut]
            [clojure.test :as t]))

;; See issue 47
;; Resolving template property URIs with values containing spaces should work

(defmacro capture
  "Capture return value of body and stdout, and return a hashmap
  of :return-value and :stdout."
  [body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [ret# ~body]
         {:return-value ret#
          :stdout (str s#)}))))

(t/deftest inner-main-test-validate-data
  (t/testing "--validate-data")
  (let [{:keys [return-value stdout]}
        (capture (sut/inner-main ["-t" "./test/examples/validation/success.csv"
                                  "-u" "./test/examples/validation/named-numbers.json"
                                  "--validate-data"]))]
    (t/is (= {:data-validation-errors? false} return-value))
    (t/is (= "" stdout)))

  (let [{:keys [return-value stdout]}
        (capture (sut/inner-main ["-t" "./test/examples/validation/fail-1.csv"
                                  "-u" "./test/examples/validation/named-numbers.json"
                                  "--validate-data"]))]
    (t/is (= {:data-validation-errors? true} return-value))
    (t/is (= "Row #3 col #2 (column 'number') has error:  Cannot parse 'two' as type 'int': For input string: \"two\"\n"
             stdout)))

  (let [{:keys [return-value stdout]}
        (capture (sut/inner-main ["-t" "./test/examples/validation/fail-2.csv"
                                  "-u" "./test/examples/validation/named-numbers.json"
                                  "--validate-data"]))]
    (t/is (= {:data-validation-errors? true} return-value))
    (t/is (= "Row #3 col #2 (column 'number') has error:  Cannot parse 'three' as type 'int': For input string: \"three\"\n"
             stdout))))
