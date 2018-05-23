(ns csv2rdf.logging
  (:require [clojure.tools.logging :as log]))

(defprotocol Logger
  (warn [this msg])
  (error [this msg]))

(defrecord MemoryLogger [warnings errors]
  Logger
  (warn [_this msg] (swap! warnings conj msg))
  (error [_this msg] (swap! errors conj msg)))

(defrecord ForwardingLogger []
  Logger
  (warn [_this msg] (log/warn msg))
  (error [_this msg] (log/error msg)))

(defrecord NullLogger []
  Logger
  (warn [_this _msg])
  (error [_this _msg]))

(defn memory-logger []
  (->MemoryLogger (atom []) (atom [])))

(def ^:dynamic *logger* (->ForwardingLogger))

(defmacro with-logger [logger & body]
  `(binding [*logger* ~logger]
     ~@body))

(defmacro capture-warnings [& body]
  `(let [warnings# (atom [])
         errors# (atom [])
         logger# (->MemoryLogger warnings# errors#)
         result# (with-logger logger# ~@body)]
     {:warnings @warnings# :result result#}))

(defmacro suppress-logging [& body]
  `(with-logger (->NullLogger) ~@body))

(defn log-warning [msg]
  (warn *logger* msg))

(defn log-error [msg]
  (error *logger* msg))
