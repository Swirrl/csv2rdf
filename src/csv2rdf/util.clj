(ns csv2rdf.util
  (:require [clojure.java.io :as io])
  (:import [java.io BufferedReader]
           [java.net URI]))

(defn read-lines
  "Eagerly reads the lines from the given source."
  [source]
  (let [r (io/reader source)]
    (with-open [r (if (instance? BufferedReader r) r (BufferedReader. r))]
      (into [] (line-seq r)))))

(defmacro ignore-exceptions [& body]
  `(try
     ~@body
     (catch Exception ex# nil)))

(defn remove-fragment [uri]
  (URI. (.getScheme uri) (.getUserInfo uri) (.getHost uri) (.getPort uri) (.getPath uri) (.getQuery uri) nil))

(defn ->coll [x]
  (if (coll? x) x [x]))

;;TODO: make into spec?
(defn non-negative?
  "Whether the given value is a non-negative number."
  [x]
  (and (number? x)
       (or (zero? x)
           (pos? x))))