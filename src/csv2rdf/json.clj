(ns csv2rdf.json
  "Functions for processing JSON maps"
  (:require [clojure.spec.alpha :as s]))

(s/def ::json-type #{:null :array :object :number :string :boolean})

(def array? vector?)

(def object? map?)

(defn get-json-type [x]
  (cond (nil? x) :null
        (array? x) :array
        (number? x) :number
        (string? x) :string
        (boolean? x) :boolean
        (object? x) :object
        :else (throw (ex-info (str "Unknown JSON type for type " (type x))
                              {:type ::json-type-error
                               :value x}))))

(defn get-json-type-name [x]
  (name (get-json-type x)))

