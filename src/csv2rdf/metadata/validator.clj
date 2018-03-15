(ns csv2rdf.metadata.validator
  (:require [csv2rdf.validation :as v]
            [csv2rdf.metadata.context :refer :all]
            [csv2rdf.metadata.json :as mjson]
            [csv2rdf.util :as util]
            [clojure.string :as string])
  (:import (java.net URI)))

(def invalid {:type ::invalid})

(defn invalid? [x]
  (and (map? x) (= ::invalid (:type x))))

(defn valid? [x]
  (not (invalid? x)))

(defn warn-invalid [validation]
  (v/bind (fn [value]
            (if (invalid? value)
              (v/with-warning (:message value) nil)         ;;TODO: specify default value
              validation))
          validation))

(defn error-invalid [validation]
  (v/bind (fn [value]
            (if (invalid? value)
              (v/of-error value)
              validation))
          validation))

(defn make-error [{:keys [path] :as context} msg]
  (v/of-error (str "Error at path " path ": " msg)))

(defn make-warning [{:keys [path] :as context} msg value]
  (v/with-warning (str "At path " path ": " msg) value))

(defn strict [validator]
  "Returns a validator which converts any warnings from the given validator into errors."
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

(defn type-error [context permitted-types actual-type]
  (make-error context (type-error-message permitted-types actual-type)))

(defn expect-type [expected-type]
  (fn [context x]
    (let [actual-type (mjson/get-json-type x)]
      (if (= expected-type actual-type)
        (v/pure x)
        (make-warning context (type-error-message [expected-type] actual-type) invalid)))))

(defn eq [expected]
  (fn [context x]
    (if (= expected x)
      (v/pure x)
      (make-warning context (str "Expected '" expected "' received '" x "'") invalid))))

(defn any "Matches any value successfully"
  [_context x]
  (v/pure x))

(def string (expect-type :string))
(def number (expect-type :number))
(def bool (expect-type :boolean))
(def array (expect-type :array))
(def object (expect-type :object))

(defn character [context x]
  (->> (string context x)
       (v/bind (fn [s]
                 (cond (invalid? s) (v/pure s)
                       (= 1 (.length s)) (v/pure (.charAt s 0))
                       :else (make-warning context "Expected single character" invalid))))))

(defn validate-array [context arr {:keys [length min-length] :as opts}]
  (cond
    (and (some? length) (not= length (count arr)))
    (make-error context (format "Expected array to contain %d elements" length))

    (and (some? min-length) (< (count arr) min-length))
    (make-error context (format "Expected array to contain at least %d elements" min-length))

    :else (v/pure arr)))

(defn traverse-array [context arr element-validator]
  (v/collect (map-indexed (fn [idx v]
                            (element-validator (append-path context idx) v)) arr)))

(defn array-with [{:keys [element-validator] :as opts}]
  (fn [context x]
    (->> (array context x)
         (v/bind (fn [arr]
                   (cond
                     (invalid? arr)
                     (v/pure [])

                     (some? element-validator)
                     (let [arr-validation (v/fmap #(vec (remove invalid? %)) (traverse-array context arr element-validator))]
                       (v/bind (fn [arr] (validate-array context arr opts)) arr-validation))

                     :else (validate-array context arr opts)))))))

(defn array-of
  ([element-validator] (array-of element-validator {}))
  ([element-validator opts]
   (array-with (assoc opts :element-validator element-validator))))

(defn tuple [& validators]
  (fn [context x]
    (->> (array context x)
         (v/bind (fn [arr]
                   (cond (invalid? arr)
                         (v/pure arr)

                         (not= (count validators) (count arr))
                         (make-warning context (format "Expected array to contains %d elements" (count validators)) invalid)

                         :else
                         (let [validator-values (map vector validators arr)
                               tuple-validation (v/collect (map-indexed (fn [idx [validator x]]
                                                                          (validator (append-path context idx) x))
                                                                        validator-values))]
                           (v/bind (fn [t]
                                     (v/pure (if (every? valid? t) t invalid)))
                                   tuple-validation))))))))

(defn variant [tag-validators]
  {:pre [(pos? (count tag-validators))]}
  (fn [context x]
    (if-let [validator (get tag-validators (mjson/get-json-type x))]
      (validator context x)
      (make-warning context (type-error-message (keys tag-validators) (mjson/get-json-type x)) invalid))))

(defn compm [& validators]
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

(defn optional-key [k value-validator]
  (fn [context m]
    (if (contains? m k)
      (let [value-validation (value-validator (append-path context k) (get m k))]
        (v/bind (fn [v]
                  (if (invalid? v)
                    nil
                    (v/pure [k v])))
                value-validation))
      (v/pure nil))))