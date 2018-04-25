(ns csv2rdf.xml.datatype.parsing
  (:require [csv2rdf.xml.datatype :as xml-datatype]
            [clojure.string :as string])
  (:import [java.net URI URISyntaxException]
           [java.util Base64]))

(defmulti parse "Parses a values to one for the named XML datatype"
          (fn [type-name string-value] (xml-datatype/dispatch-key type-name))
          :hierarchy #'xml-datatype/dispatch-hierarchy)

(defmethod parse :string [_type-name string-value]
  string-value)

(defmethod parse :anyURI [_type-name string-value]
  (try
    (URI. string-value)
    (catch URISyntaxException ex
      (throw (IllegalArgumentException. "Invalid URI" ex)))))

(defmethod parse :base64Binary [_type-name ^String string-value]
  (.decode (Base64/getDecoder) string-value))

(defn parse-hex-digit [^Character c]
  (let [i (Character/digit c 16)]
    (if (= -1 i)
      (throw (IllegalArgumentException. (str "Invalid hex digit " c)))
      i)))

(defmethod parse :hexBinary [_type-name ^String string-value]
  (if (even? (.length string-value))
    (let [byte-count (/ (.length string-value) 2)
          bytes (byte-array byte-count)]
      (doseq [idx (range byte-count)]
        (let [offset (* 2 idx)
              b1 (parse-hex-digit (.charAt string-value offset))
              b2 (parse-hex-digit (.charAt string-value (inc offset)))]
          (aset bytes idx (byte (bit-and (bit-shift-left b1 4) b2)))))
      bytes)
    (throw (IllegalArgumentException. "Hex string must contain even number of characters"))))

(def ^{:xml-schema-spec "3.3.2.2"} default-boolean-mapping
  {:true-values #{"1" "true"}
   :false-values #{"0" "false"}})

(defn parse-boolean-with-mapping [string-value {:keys [true-values false-values] :as mapping}]
  (cond
    (contains? true-values string-value) true
    (contains? false-values string-value) false
    :else (let [allowed-values (mapcat identity (vals mapping))
                msg (format "Expected one of %s" (string/join ", " allowed-values))]
            (throw (IllegalArgumentException. msg)))))

(defmethod parse :boolean [_type-name string-value]
  (parse-boolean-with-mapping string-value default-boolean-mapping))



