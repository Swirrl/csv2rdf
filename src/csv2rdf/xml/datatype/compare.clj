(ns csv2rdf.xml.datatype.compare
  "Defines protocol and implementations for partially-ordered datatypes."
  (:import [javax.xml.datatype XMLGregorianCalendar DatatypeConstants]
           [java.time.temporal Temporal]))

(defprotocol Comp
  (lt? [this other])
  (lte? [this other])
  (gt? [this other])
  (gte? [this other])
  (eql? [this other])
  (neql? [this other]))

(def default-methods
  {:lte? (fn [c1 c2]
           (or (lt? c1 c2) (eql? c1 c2)))
   :gte? (fn [c1 c2]
           (or (gt? c1 c2) (eql? c1 c2)))
   :neql? (fn [c1 c2]
            (not (eql? c1 c2)))})

(extend XMLGregorianCalendar
  Comp
  (merge default-methods
         {:lt? (fn [^XMLGregorianCalendar this other]
                 (= DatatypeConstants/LESSER (.compare this other)))
          :ge? (fn [^XMLGregorianCalendar this other]
                 (= DatatypeConstants/GREATER (.compare this other)))
          :eql? (fn [^XMLGregorianCalendar this other]
                  (= DatatypeConstants/EQUAL (.compare this other)))}))

;;NOTE: this implementation is for the values of date/time datatypes. All the parsed result types implement
;;the Temporal interface. We could add an implementation for Comparable but that would overlap with the protocol
;;instances for numeric types
(extend Temporal
  Comp
  (merge default-methods
         {:lt? (fn [^Comparable this other]
                 (neg? (.compareTo this other)))
          :gt? (fn [^Comparable this other]
                 (pos? (.compareTo this other)))
          :eql? (fn [^Comparable this other]
                  (zero? (.compareTo this other)))}))

(extend-protocol Comp
  Number
  (lt? [this other] (< this other))
  (lte? [this other] (<= this other))
  (gt? [this other] (> this other))
  (gte? [this other] (>= this other))
  (eql? [this other] (= this other))
  (neql? [this other] (not= this other)))