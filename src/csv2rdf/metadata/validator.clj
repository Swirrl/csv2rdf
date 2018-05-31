(ns csv2rdf.metadata.validator
  (:require [csv2rdf.metadata.context :refer :all]
            [csv2rdf.json :as mjson]
            [clojure.string :as string]
            [csv2rdf.logging :as logging])
  (:import [java.net URI]))

(def invalid ::invalid)

(defn invalid? [x]
  (= ::invalid x))

(defn valid? [x]
  (not (invalid? x)))

(defn make-error [{:keys [path] :as context} msg]
  (throw (ex-info (str "Error at path " path ": " msg) {:type :bad-metadata
                                                        :path path})))

(defn make-warning [{:keys [path] :as context} msg value]
  (logging/log-warning (str "At path " path ": " msg))
  value)

(defn warn-with [value]
  (fn [context message]
    (make-warning context message value)))

(def warn-invalid (warn-with invalid))

(defn default-if-invalid
  "Returns a validator which returns the given default if the inner validator returns invalid"
  [validator default]
  (fn [context x]
    (let [result (validator context x)]
      (if (invalid? result) default result))))

(defn ignore-invalid [validator] (default-if-invalid validator nil))

(defn with-error-handler
  "Returns a validator which invokes the inner validator with the given error function."
  [validator error-fn]
  (fn [context x] (validator context x error-fn)))

(defn strict [validator]
  "Returns a validator which causes the given validator to raise errors when validation fails.
   The given validator must support a third argument for the error function."
  (with-error-handler validator make-error))

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

(defn chain
  "Composes a sequence of validators into a validator which applies each validator in turn.
   The resulting validator returns invalid on any intermediate invalid result and does not
   call any subsequent validators in the chain."
  [& validators]
  (reduce (fn [acc validator]
            (fn [context x]
              (let [r (acc context x)]
                (if (invalid? r)
                  invalid
                  (validator context r))))) any validators))

(defn- validate-character
  "Validates a string contains a single character and returns that character if valid"
  [context ^String s]
  (if (= 1 (.length s))
    (.charAt s 0)
    (make-warning context "Expected single character" invalid)))

(def character (chain string validate-character))

(defn variant [tag-validators]
  {:pre [(pos? (count tag-validators))]}
  (fn v
    ([context x]
      (let [on-invalid (if (contains? tag-validators :default)
                         (:default tag-validators)
                         invalid)]
        (v context x (warn-with on-invalid))))
    ([context x error-fn]
     (if-let [validator (get tag-validators (mjson/get-json-type x))]
       (validator context x)
       (let [valid-types (keys (dissoc tag-validators :default))
             actual-type (mjson/get-json-type x)
             error-message (type-error-message valid-types actual-type)]
         (error-fn context error-message))))))

(defn array-of [element-validator]
  (fn [context x]
    (let [arr ((variant {:array any :default []}) context x)]
      (vec (remove invalid? (map-indexed (fn [idx e]
                                           (element-validator (append-path context idx) e))
                                         arr))))))

(defn nullable
  "Wraps a validator into one which allows explicit null values."
  [validator]
  (fn [context x]
    (if (nil? x)
      nil
      (validator context x))))

(defn try-parse-with
  "Returns a validator which tries to parse the incoming value with the function f. Returns invalid if f
   throws an exception."
  [f]
  (fn [context x]
    (try
      (f x)
      (catch Exception ex
        (make-warning context (.getMessage ex) invalid)))))

(def uri (variant {:string (try-parse-with #(URI. %))}))

(defn required-key [k value-validator]
  (fn [context m]
    (if (contains? m k)
      (let [value (value-validator (append-path context k) (get m k))]
        (if (invalid? value)
          (make-error context (str "Invalid value for required key '" k "'"))
          [k value]))
      (make-error context (str "Missing required key '" k "'")))))

(defn optional-key
  ([k value-validator]
   (fn [context m]
     (if (contains? m k)
       (let [value (value-validator (append-path context k) (get m k))]
         (if (invalid? value)
           nil
           [k value]))
       nil)))
  ([k value-validator default]
   (fn [context m]
     (if (contains? m k)
       (let [value (value-validator (append-path context k) (get m k))]
         [k (if (invalid? value) default value)])
       [k default]))))

(defn kvps
  "Takes a collection of kvp specs which validate a map and return a key-value pair (or invalid).
   Combines results for all kvps into a map."
  [kvp-specs]
  (fn [context m]
    (->> kvp-specs
         (map (fn [kvp-spec] (kvp-spec context m)))
         (remove invalid?)
         (into {}))))

(defn kvp [key-validator value-validator]
  (fn [context [k v]]
    (let [key (key-validator context k)
          value (value-validator (append-path context k) v)]
      (if (or (invalid? key) (invalid? value))
        invalid
        [key value]))))

(defn map-of [key-validator value-validator]
  (let [pair-validator (kvp key-validator value-validator)]
    (fn [context m]
      (->> m
           (map (fn [kvp] (pair-validator context kvp)))
           (remove invalid?)
           (into {})))))

(defn one-of
  "Returns a validator which expects its input to be one of the given values. Returns
   the matching value if found, otherwise invalid."
  [values]
  (fn [context x]
    (if (contains? values x)
      x
      (make-warning context (str "Expected one of " (string/join ", " values)) invalid))))

(defn mapping
  "Returns a validator which expects its input to be one of the keys in the map m. Returns the
  value associated with the matching key in m if found, otherwise invalid."
  [m]
  (fn [context k]
    (if (contains? m k)
      (get m k)
      (make-warning context (str "Expected one of " (string/join ", " (keys m))) invalid))))

(defn where
  "Returns a validator which applies the predicate pred to the input value. Returns the value if
   pred returns true, otherwise invalid. Desc should be a string describing pred."
  [pred desc]
  (fn [context x]
    (if (pred x)
      x
      (make-warning context (str "Expected '" x "' to be " desc) invalid))))