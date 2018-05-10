(ns csv2rdf.metadata.validator
  (:require [csv2rdf.validation :as v]
            [csv2rdf.metadata.context :refer :all]
            [csv2rdf.metadata.json :as mjson]
            [clojure.string :as string]
            [csv2rdf.logging :as logging])
  (:import [java.net URI]))

(def invalid ::invalid)

(defn invalid? [x]
  (= ::invalid x))

(defn valid? [x]
  (not (invalid? x)))

;;TODO: remove invalid marker value?
(defn ignore-invalid
  "Validator which wraps an inner validator and converts invalid values into nil"
  [validator]
  (fn [context x]
    (v/fmap (fn [r] (if (invalid? r) nil r)) (validator context x))))

;;TODO: rename
(defn make-error [{:keys [path] :as context} msg]
  (throw (ex-info (str "Error at path " path ": " msg) {:type :bad-metadata
                                                        :path path})))

(defn make-warning [{:keys [path] :as context} msg value]
  (logging/log-warning (str "At path " path ": " msg))
  value)

(defn strict [validator]
  "Returns a validator which converts any warnings from the given validator into errors."
  (throw (IllegalStateException. "Replace with explicit validator!"))
  (fn [context x]
    (v/warnings-as-errors (validator context x))))

(defn type-error-message [permitted-types actual-type]
  (let [c (count permitted-types)]
    (case c
      0 (str "Unexpected type " (name actual-type))
      1 (str "Expected " (name (first permitted-types)) " but got " (name actual-type))
      (let [[except-last [last]] (split-at (dec c) (map name permitted-types))
            list (str (string/join ", " except-last) " or " last)]
        (str "Expected " list " but got " (name actual-type))))))

(defn expect-type [expected-type]
  (fn [context x]
    (let [actual-type (mjson/get-json-type x)]
      (if (= expected-type actual-type)
        x
        (make-warning context (type-error-message [expected-type] actual-type) invalid)))))

(defn eq [expected]
  (fn [context x]
    (if (= expected x)
      x
      (make-warning context (str "Expected '" expected "' received '" x "'") invalid))))

(defn type-eq
  "Validator that an object's @type property is the expected value"
  [expected]
  (fn [context x]
    (if (= expected x)
      x
      (make-error context (format "Expected type to be '%s', received '%s'" expected x)))))

(defn any "Matches any value successfully"
  [_context x]
  x)

(def string (expect-type :string))
(def number (expect-type :number))
(def bool (expect-type :boolean))
(def array (expect-type :array))
(def object (expect-type :object))

;;TODO: rewrite to use chain?
(defn character [context x]
  (let [s (string context x)]
    (if (invalid? s)
      invalid
      (let [^String s s]
        (if (= 1 (.length s))
          (.charAt s 0)
          (make-warning context "Expected single character" invalid))))))

(defn variant [{:keys [default] :as tag-validators}]
  {:pre [(pos? (count tag-validators))]}
  (fn [context x]
    (if-let [validator (get tag-validators (mjson/get-json-type x))]
      (validator context x)
      (let [valid-types (keys (dissoc tag-validators :default))
            actual-type (mjson/get-json-type x)]
        (make-warning context (type-error-message valid-types actual-type) (or default invalid))))))

(defn validate-array [context arr {:keys [length min-length] :as opts}]
  (cond
    (and (some? length) (not= length (count arr)))
    (make-error context (format "Expected array to contain %d elements" length))

    (and (some? min-length) (< (count arr) min-length))
    (make-error context (format "Expected array to contain at least %d elements" min-length))

    :else (v/pure arr)))

;;TODO: rewrite using chain?
(defn array-of [element-validator]
  (fn [context x]
    (let [arr ((variant {:array any :default []}) context x)]
      (vec (remove invalid? (map-indexed (fn [idx e]
                                           (element-validator (append-path context idx) e))
                                         arr))))))

;;TODO: this is only used in one place - remove?
(defn tuple [& validators]
  (fn [context x]
    (let [arr (array context x)]
      (cond
        (invalid? arr)
        invalid

        (not= (count validators) (count arr))
        (make-warning context (format "Expected array to contains %d elements" (count validators)) invalid)

        :else
        (let [validator-values (map vector validators arr)
              validated (vec (map-indexed (fn [idx [validator x]]
                                            (validator (append-path context idx) x))
                                          validator-values))]
          (if (every? valid? validated)
            validated
            invalid))))))

(defn nullable
  "Wraps a validator into one which allows explicit null values."
  [validator]
  (fn [context x]
    (if (nil? x)
      (v/pure x)
      (validator context x))))

(defn chain
  "Composes a sequence of validators into a validator which applies each validator in turn.
   The resulting validator returns invalid on any intermediate invalid result and does not
   call any subsequent validators in the chain."
  [& validators]
  (reduce (fn [acc validator]
            (fn [context value]
              (v/bind (fn [v]
                        (if (invalid? v)
                          (v/pure v)
                          (validator context v)))
                      (acc context value)))) any validators))

(defn try-parse-with [f]
  (fn [context x]
    (try
      (v/pure (f x))
      (catch Exception ex
        (make-warning context (.getMessage ex) invalid)))))

(def uri (variant {:string (try-parse-with #(URI. %))}))

(defn required-key [k value-validator]
  (fn [context m]
    (if (contains? m k)
      (v/bind (fn [result]
                (if (invalid? result)
                  (make-error context (str "Invalid value for required key '" k "'"))
                  (v/pure [k result])))
              (value-validator (append-path context k) (get m k)))
      (make-error context (str "Missing required key '" k "'")))))

(defn optional-key
  ([k value-validator]
   (fn [context m]
     (if (contains? m k)
       (let [value-validation (value-validator (append-path context k) (get m k))]
         (v/bind (fn [v]
                   (if (invalid? v)
                     nil
                     (v/pure [k v])))
                 value-validation))
       (v/pure nil))))
  ([k value-validator default]
   (fn [context m]
     (if (contains? m k)
       (let [value-validation (value-validator (append-path context k) (get m k))]
         (v/fmap (fn [v] [k (if (invalid? v) default v)])
                 value-validation))
       (v/pure [k default])))))

(defn ^{:metadata-spec "4"} invalid-key-pair
  "Generates a warning for any invalid keys found in an object."
  [context [k _]]
  (make-warning context (str "Invalid key '" k "'") nil))

(defn- combine-kvp-validations [kvp-validations]
  (v/fmap (fn [pairs]
            (into {} (remove invalid? pairs)))
          (v/collect kvp-validations)))

(defn kvps
  "Takes a collection of kvp specs which validate a map and return a key-value pair (or invalid).
   Combines results for all kvps into a map."
  [kvp-specs]
  (fn [context m]
    (combine-kvp-validations (map (fn [key-spec]
                                    (key-spec context m))
                                  kvp-specs))))

(defn kvp [key-validator value-validator]
  (fn [context [k v]]
    (v/bind (fn [key-result]
              (if (invalid? key-result)
                (v/pure key-result)
                (v/bind (fn [value-result]
                          (if (invalid? value-result)
                            (v/pure value-result)
                            (v/pure [key-result value-result])))
                        (value-validator (append-path context k) v))))
            (key-validator context k))))

(defn map-of [key-validator value-validator]
  (let [pair-validator (kvp key-validator value-validator)]
    (fn [context m]
      (combine-kvp-validations (map (fn [kvp] (pair-validator context kvp)) m)))))

(defn one-of
  "Returns a validator which expects its input to be one of the given values. Returns a sucessful
   validation containing the matching value if found, otherwise invalid."
  [values]
  (fn [context x]
    (if (contains? values x)
      (v/pure x)
      (make-warning context (str "Expected one of " (string/join ", " values)) invalid))))

(defn mapping
  "Returns a validator which expects its input to be one of the keys in the map m. Returns the
  value associated with the matching key in m if found, otherwise invalid."
  [m]
  (fn [context k]
    (if (contains? m k)
      (v/pure (get m k))
      (make-warning context (str "Expected one of " (string/join ", " (keys m))) invalid))))

(defn where
  "Returns a validator which applies the predicate pred to the input value. Returns the value if
   pred returns true, otherwise invalid. Desc should be a string describing pred."
  [pred desc]
  (fn [context x]
    (if (pred x)
      (v/pure x)
      (make-warning context (str "Expected '" x "' to be " desc) invalid))))