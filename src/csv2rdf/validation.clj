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

(defn with-warning [warning-message value]
  {::errors [] ::warnings [warning-message] ::value value})

(defn error? [validation]
  (not (empty? (::errors validation))))

(defn fmap [f validation]
  (if (error? validation)
    validation
    (update validation ::value f)))

(defn bind [f validation]
  (if (error? validation)
    validation
    (f (::value validation))))

(defn map-errors [f validation]
  (update validation ::errors (fn [errors] (map f errors))))

(defn combine-with [merge-fn v1 v2]
  (if (or (error? v1) (error? v2))
    {::errors (concat (::errors v1) (::errors v2))
     ::warnings (concat (::warnings v1) (::warnings v2))
     ::value nil}
    {::errors []
     ::warnings (concat (::warnings v1) (::warnings v2))
     ::value (merge-fn (::value v1) (::value v2))}))

(defn combine [v1 v2]
  (combine-with (fn [_ r] r) v1 v2))

(defn collect [vs]
  (reduce (fn [acc v] (combine-with conj acc v)) (pure []) vs))


