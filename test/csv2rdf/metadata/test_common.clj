(ns csv2rdf.metadata.test-common
  (:require [clojure.test :refer [is]]
            [csv2rdf.metadata.context :as context])
  (:import [clojure.lang ExceptionInfo]
           [java.net URI]))

(defmacro validation-error [& body]
  `(is (~'thrown? ExceptionInfo ~@body)))

(def test-base-uri (URI. "http://example.com/"))
(def test-context (context/make-context test-base-uri))
