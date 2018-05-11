(ns csv2rdf.metadata.test-common
  (:require [clojure.test :refer [is]])
  (:import [clojure.lang ExceptionInfo]))

(defmacro validation-error [& body]
  `(is (~'thrown? ExceptionInfo ~@body)))
