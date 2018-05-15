(ns csv2rdf.validation
  (:require [clojure.spec.alpha :as s]))

(s/def ::value (constantly true))
(s/def ::error string?)
(s/def ::warning string?)
(s/def ::errors (s/coll-of ::error))
(s/def ::warnings (s/coll-of ::warning))
(s/def ::validation (s/keys :req [::errors ::warnings ::value]))

(def empty-validation {::errors [] ::warnings [] ::value nil})

(defn pure [value]
  {::errors [] ::warnings [] ::value value})

(defn of-error [error-message]
  {::errors [error-message] ::warnings [] ::value nil})

(defn error? [validation]
  (not (empty? (::errors validation))))

(defn fmap [f validation]
  (if (error? validation)
    validation
    (update validation ::value f)))

(def warnings ::warnings)
(def errors ::errors)
