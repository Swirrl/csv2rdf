(ns csv2rdf.metadata.test-common
  (:require [clojure.test :refer [is]]
            [csv2rdf.metadata.context :as context]
            [csv2rdf.logging :as logging]
            [csv2rdf.metadata.validator :refer [invalid?]])
  (:import [clojure.lang ExceptionInfo]
           [java.net URI]))

(defmacro validation-error [& body]
  `(is (~'thrown? ExceptionInfo ~@body)))

(defmacro validates-as [expected & body]
  `(let [{warnings# :warnings result# :result} (logging/capture-warnings ~@body)]
     (is (empty? warnings#))
     (is (= ~expected result#))))

(defmacro warns-invalid [& body]
  `(let [{warnings# :warnings result# :result} (logging/capture-warnings ~@body)]
     (is (pos? (count warnings#)))
     (is (invalid? result#))))

(defmacro warns-with [expected & body]
  `(let [{warnings# :warnings result# :result} (logging/capture-warnings ~@body)]
     (is (pos? (count warnings#)))
     (is (= ~expected result#))))


(def test-base-uri (URI. "http://example.com/"))
(def test-context (context/make-context test-base-uri))
