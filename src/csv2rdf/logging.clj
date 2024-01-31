(ns csv2rdf.logging
  "Entry point for logging messages during the CSVW process. Components should call log-warning and log-error
   instead of writing log messages directly. The sink for log messages can be configured by setting *logger*."
  (:require [clojure.tools.logging :as log]))

(defprotocol Logger
  (warn [this msg])
  (error [this msg]))

;; For use with GraalVM since clojure.tools.logging relies on a lot of
;; reflection that doesn't seem to work properly in native images.
(defrecord PrintlnLogger []
  Logger
  (warn [_ msg] (println msg))
  (error [_ msg] (println msg)))

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

(defn memory-logger
  ([]
   (memory-logger (atom []) (atom [])))
  ([warnings errors]
   (->MemoryLogger warnings errors)))

(def ^:dynamic *logger* (->ForwardingLogger))

(defmacro with-logger
  "Executes body with the given log message sink."
  [logger & body]
  `(binding [*logger* ~logger]
     ~@body))

(defmacro capture-warnings
  "Captures any warning messages logged while executing body. Logged warnings are returned under
   the :warnings key in the returned map. The result of body is returned under the :result key."
  [& body]
  `(let [warnings# (atom [])
         errors# (atom [])
         logger# (->MemoryLogger warnings# errors#)
         result# (with-logger logger# ~@body)]
     {:warnings @warnings# :result result#}))

(defmacro suppress-logging
  "Discards all log messages generated while executing body."
  [& body]
  `(with-logger (->NullLogger) ~@body))

(defn log-warning
  "Logs the given warning to the current logger."
  [msg]
  (warn *logger* msg))

(defn log-error
  "Logs the given error to the current logger."
  [msg]
  (error *logger* msg))
