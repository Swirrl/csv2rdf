(ns csv2rdf.test-common
  (:require [csv2rdf.logging :as logging]))

(defn suppress-test-logging [t]
  (logging/suppress-logging (t)))
