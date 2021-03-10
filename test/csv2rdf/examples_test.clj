(ns csv2rdf.examples-test
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [csv2rdf.csvw-test.impl :as impl]))

;; See issue 47
;; Resolving template property URIs with values containing spaces should work
(t/deftest template-property-value-whitespace-test
  (let [metadata-file (io/file "test/examples/template_property_whitespace/metadata.json")
        {:keys [errors warnings]} (impl/test-csv->rdf nil metadata-file {})]
    (t/is (zero? (count errors)))
    (t/is (zero? (count warnings)))))
