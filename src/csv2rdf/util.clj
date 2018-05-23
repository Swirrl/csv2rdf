(ns csv2rdf.util
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as string])
  (:import [java.net URI]
           [java.lang.reflect InvocationTargetException Method]
           [java.nio.charset Charset]
           [java.io ByteArrayOutputStream]
           [java.lang AutoCloseable]
           [java.util Iterator]))

(defmacro ignore-exceptions [& body]
  `(try
     ~@body
     (catch Exception ex# nil)))

(defn set-fragment [^URI uri fragment]
  (URI. (.getScheme uri) (.getUserInfo uri) (.getHost uri) (.getPort uri) (.getPath uri) (.getQuery uri) fragment))

(defn remove-fragment [uri]
  (set-fragment uri nil))

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
  "Splits a source map into two sub-maps - the first contain the keys in both source-map and ref-keys,
   the second contains the keys in source-map which do not exist in ref-keys."
  [source-map ref-keys]
  [(select-keys source-map ref-keys) (apply dissoc source-map ref-keys)])

(defn get-declared-method [^Class cls method-name arg-types]
  (let [^Method m (.getDeclaredMethod cls method-name (into-array Class arg-types))]
    (.setAccessible m true)
    m))

(defn invoke-method
  "Invokes the given Method instance with the given arguments and receiver if one is specified. Unwraps the inner
   exception if an InvocationTargetException is thrown."
  ([method args] (invoke-method method nil args))
  ([^Method method receiver args]
   (try
     (.invoke method receiver (into-array Object args))
     (catch InvocationTargetException ex
       (throw (.getCause ex))))))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn map-values [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn equals-ignore-case?
  "Whether two string are equal ignoring case differences. Nil references are considered equal."
  [^String s1 ^String s2]
  (if (nil? s1)
    (nil? s2)
    (.equalsIgnoreCase s1 s2)))

(defn assoc-if
  "Associates v with the key k in the associative structure m if condition is truthy, otherwise returns m."
  [m condition k v]
  (if condition
    (assoc m k v)
    m))

;;NOTE: Ruby RDF::URI('http://example.com/some/path/file.csv').join("") returns 'http://example.com/some/path/file.csv'
;;instead of 'http://example.com/some/path/' returned by URI/resolve.
;;TODO: check the correct behaviour
(defn resolve-uri
  "Resolves the given string against a base URI. This differs from java.net.URI/resolve in that
  empty strings are ignored and return the base URI is returned unaltered."
  [^URI base-uri rel]
  (cond
    (string/blank? (str rel))
    base-uri

    (string? rel)
    (let [^String s rel]
      (.resolve base-uri s))

    (instance? URI rel)
    (let [^URI uri rel]
      (.resolve base-uri uri))

    :else (throw (IllegalArgumentException. "Resolving URI must be either string or URI instance"))))

;;NOTE: '-' and ' ' are not mentioned in the spec? Some test cases expected '-' to be encoded
(def ^{:rfc3986 "2.2"} ^String percent-unreserved-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.~")

(defn ^{:rfc3986 "2.2"} percent-encode
  "Percent-encodes the bytes in the given string."
  ([s] (percent-encode s (Charset/forName "utf-8")))
  ([^String s ^Charset charset]
   (let [bytes (.getBytes s charset)
         len (int (alength bytes))
         sb-len (int (+ 12 len))
         sb (StringBuilder. sb-len)]
     (loop [idx 0]
       (if (>= idx len)
         (str sb)
         (let [octet (aget bytes idx)
               ci (Byte/toUnsignedInt octet)
               requires-encoding? (= -1 (.indexOf percent-unreserved-chars ci))]
           (if requires-encoding?
             (doto sb (.append \%) (.append (string/upper-case (Integer/toHexString ci))))
             (.appendCodePoint sb ci))
           (recur (inc idx))))))))

(defn ^{:rfc3986 "2.2"} percent-decode
  "Decodes a percent-encoded string. Assumes all characters in the input are in the ASCII range. Throws an
   IllegalArgumentException if the input is malformed."
  [^String s]
  (let [buf (ByteArrayOutputStream.)]
    (loop [idx 0]
      (if (>= idx (.length s))
        (String. (.toByteArray buf) (Charset/forName "utf-8"))
        (let [c (.charAt s idx)]
          (if (= \% c)
            (if (>= (+ 2 idx) (.length s))
              (throw (IllegalArgumentException. (str "Out of range at index " idx)))
              (let [endIdx (+ 3 idx)
                    hex (.substring s (inc idx) endIdx)
                    ^Integer decoded (try
                                       (Integer/valueOf hex 16)
                                       (catch NumberFormatException _ex
                                         (throw (IllegalArgumentException. (str "Bad hex number: " hex)))))]
                (.write buf decoded)
                (recur endIdx)))
            (do
              (.write buf (int c))
              (recur (inc idx)))))))))

(def ^String hex-digits "0123456789ABCDEF")

(defn to-hex-string
  "Returns the hex representation of a byte array"
  [^bytes arr]
  (let [num-bytes (alength arr)
        chars (char-array (* 2 num-bytes))]
    (doseq [idx (range num-bytes)]
      (let [offset (* 2 idx)
            b (aget arr idx)
            c1 (.charAt hex-digits (bit-and (bit-shift-right b 4) 0x0F))
            c2 (.charAt hex-digits (bit-and b 0x0F))]
        (aset chars offset c1)
        (aset chars (inc offset) c2)))
    (String. chars)))

(defn owning-iterator [^AutoCloseable source factory-fn]
  (let [ita (atom nil)
        state (atom 0)
        move-done (fn []
                    (.close source)
                    (reset! ita nil)
                    (reset! state 2))
        move-open (fn []
                    (try
                      (reset! ita (factory-fn source))
                      (reset! state 1)
                      (catch Exception ex
                        (move-done)
                        (throw ex))))
        get-next (fn []
                   (try
                     (.next @ita)
                     (catch Exception ex
                       (move-done)
                       (throw ex))))]
    (reify
      Iterator
      (hasNext [this]
        (case @state
          0 (do
              (move-open)
              (.hasNext this))

          1 (let [^Iterator it @ita]
              (try
                (if (.hasNext it)
                  true
                  (do
                    (move-done)
                    false))
                (catch Exception ex
                  (move-done)
                  (throw ex))))

          2 false

          (throw (IllegalStateException. (str "Unexpected state " @state)))))

      (next [this]
        (case @state
          0 (do
              (move-open)
              (get-next))
          1 (get-next)
          2 (throw (IllegalStateException. "No next element"))
          (throw (IllegalStateException. (str "Unexpected state " @state)))))

      (remove [this]
        (throw (UnsupportedOperationException. "remove not supported")))

      AutoCloseable
      (close [this]
        (move-done)))))