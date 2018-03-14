(ns csv2rdf.util
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json])
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

(defn read-json [source]
  (with-open [r (io/reader source)]
    (json/read r)))

(defn select-keys-as
  "Selects keys from the source map m, renaming them to the mapped keys in the result map e.g.

  (select-keys-as {\"foo\" 1 \"bar\" 2} {\"foo\" :a \"quux\" :b})
  => {:a 1}"
  [m key-mapping]
  (reduce (fn [acc [source-key dest-key]]
            (if (contains? m source-key)
              (assoc acc dest-key (get m source-key))
              acc))
          {}
          key-mapping))

(defn filter-values
  "Filters a map given a predicate on the values."
  [pred m]
  (into {} (filter (fn [[_k v]] (pred v)) m)))

(defn partition-keys
  "Splits a source map into two sub-maps - the first contain the keys in both source-map and ref-map,
   the second contains the keys in source-map which do not exist in ref-map."
  [source-map ref-map]
  (let [ref-keys (keys ref-map)]
    [(select-keys source-map ref-keys) (apply dissoc source-map ref-keys)]))