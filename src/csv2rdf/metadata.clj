(ns csv2rdf.metadata
  (:require [csv2rdf.validation :as v]
            [clojure.spec.alpha :as s]
            [clojure.string :as string])
  (:import (java.net URI)))

(defn eq [expected]
  (fn [x]
    (if (= expected x)
      (v/pure x)
      (v/of-error (str "Expected '" expected "' received '" x "'")))))

(defn get-message [spec invalid-value]
  (str "Invalid value: " invalid-value))

(defn must [spec]
  (fn [context value]
    (let [v (s/conform spec value)]
      (if (= ::s/invalid v)
        (v/of-error (get-message spec value))
        (v/pure v)))))

(defn local-context [context value]
  )

(def ^{:metadata-spec "5.2"} context nil)

(defn- expect-type [type-p type-name]
  (fn [x]
    (if (type-p x)
      (v/pure x)
      (v/of-error (str "Expected " type-name)))))

(def array (expect-type vector? "array"))
(def number (expect-type number? "number"))

(defn object [x]
  (if (map? x)
    (v/pure x)
    (v/of-error "Expected object")))

(def string (expect-type string? "string"))

(defn character [x]
  (if (string? x)
    (if (= 1 (.length x))
      (v/pure (.charAt x 0))
      (v/of-error "Expected single character"))
    (v/of-error "Expected string")))

(defn of-length [len]
  (fn [arr]
    (if (= len (count arr))
      (v/pure arr)
      (v/of-error (str "Expected length " len)))))

(defn compm [& fs]
  (reduce (fn [acc f]
            (fn [v]
              (v/bind f (acc v)))) v/pure fs))

(defn array-of [f & {:keys [length]}]
  (let [len-v (if (some? length) (of-length length) v/pure)]
    (compm array len-v (fn [arr]
                         (v/collect (map f arr))))))

(defn tuple [& fs]
  (compm array
         (of-length (count fs))
         (fn [arr]
           (v/collect (map (fn [f x] (f x)) fs arr)))))

(defn try-parse-with [f]
  (fn [x]
    (try
      (v/pure (f x))
      (catch Exception ex
        (v/of-error (.getMessage ex))))))

(def uri (compm string (try-parse-with #(URI. %))))

(defn either [& desc-validation-pairs]
  (let [pairs (partition-all 2 desc-validation-pairs)]
    (fn [x]
      (loop [ps pairs]
        (if (seq ps)
          (let [[_desc vf] (first ps)
                v (vf x)]
            (if (v/error? v)
              (recur (rest ps))
              v))
          (v/of-error (str "Expected one of: " (string/join ", " (map first pairs)))))))))

(defn each [& fs]
  (fn [x]
    (v/collect (map (fn [f] (f x)) fs))))

(defn prefix-errors-key-name [key validation]
  (v/map-errors (fn [err] (str "Key '" key "': " err)) validation))

(defn required-key [k vf]
  (fn [m]
    (if (contains? m k)
      (->> (vf (get m k))
           (prefix-errors-key-name k)
           (v/fmap (fn [v] [k v])))
      (v/of-error (str "Missing required key '" k "'")))))

(defn optional-key [k vf & {:keys [default]}]
  (fn [m]
    (if (contains? m k)
      (->> (vf (get m k))
           (prefix-errors-key-name k)
           (v/fmap (fn [v] [k v])))
      (v/pure [k default]))))

(defn object-of [{:keys [required optional defaults]}]
  (let [required-keys (map (fn [[k v]] (required-key k v)) required)
        optional-keys (map (fn [[k v]] (optional-key k v :default (get defaults k))) optional)
        keys-f (apply each (concat required-keys optional-keys))]
    (compm
      object
      keys-f
      (fn [pairs]
        (v/pure (into {} pairs))))))

(defn bcp47-language [x]
  ;;TODO: validate valid language tag
  (v/pure x))

(defn encoding [x]
  ;;TODO: validate encoding string
  (string x))

;;TODO: merge with tabular.csv.dialect/parse-trim
(def trim-modes {"true" :all "false" :none "start" :start "end" :end})

(defn mapping [m]
  (fn [k]
    (if (contains? m k)
      (v/pure (get m k))
      (v/of-error (str "Expected one of " (string/join ", " (keys m)))))))

(def trim-mode (either "boolean" (compm boolean (fn [b] (v/pure (if b :all :none))))
                        "trim mode" (mapping trim-modes)))

(defn where [pred desc]
  (fn [x]
    (if (pred x)
      (v/pure x)
      (v/of-error (str "Expected '" x "' to be " desc)))))

(defn non-negative [x]
  (compm number (where pos? "positive")))

(def csvw-ns "http://www.w3.org/ns/csvw")

(def top-level-object (either "ns" (eq csvw-ns)
                              "local context" (each (tuple
                                                      (eq csvw-ns)
                                                      (object-of {:optional {"@base"     uri
                                                                             "@language" bcp47-language}}))
                                                    (fn [[_ns m]]
                                                      (if (or (contains? m "@base") (contains? m "@language"))
                                                        (v/pure m)
                                                        (v/of-error "Top-level object must contain @base or @language keys"))))))

(def dialect
  (object-of
    {:optional {"commentPrefix" character
                "delimiter" character
                "doubleQuote" boolean
                "encoding" encoding
                "header" boolean
                "headerRowCount" non-negative
                "lineTerminators" (either "single" (compm string (fn [x] (v/pure [x])))
                                          "array" (array-of string))
                "quoteChar" character
                "skipBlankRows" boolean
                "skipColumns" non-negative
                "skipInitialSpace" boolean
                "skipRows" non-negative
                "trimMode" trim-mode
                "@id" v/pure                                ;;TODO: validate
                "@type" (eq "Dialect")
                }
     :defaults {"commentPrefix" \#
                "delimiter" \,
                "doubleQuote" true
                "encoding" "utf-8"
                "header" true
                "headerRowCount" 1
                "lineTerminators" ["\r\n", "\n"]
                "quoteChar" \"
                "skipBlankRows" false
                "skipColumns" 0
                "skipInitialSpace" false
                "skipRows" 0
                "trimMode" :all
                }}))