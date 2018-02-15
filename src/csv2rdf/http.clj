(ns csv2rdf.http
  (:require [clojure.spec.alpha :as s]))

(s/def ::header-value string?)

(defn is-ok-response? [{:keys [status] :as response}]
  (and (>= status 200) (<= status 300)))

(defn get-uri [uri]
  )
