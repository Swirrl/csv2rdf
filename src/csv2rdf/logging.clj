(ns csv2rdf.logging)

;;TODO: configure logging framework

(defprotocol Logger
  (warn [this msg])
  (error [this msg]))

(defrecord MemoryLogger [warnings errors]
  Logger
  (warn [_this msg] (swap! warnings conj msg))
  (error [_this msg] (swap! errors conj msg)))

(defrecord ConsoleLogger []
  Logger
  (warn [_this msg] (println "WARNING: " msg))
  (error [_this msg] (throw (ex-info msg {}))))

(defrecord NullLogger []
  Logger
  (warn [_this _msg])
  (error [_this _msg]))

(defn memory-logger []
  (->MemoryLogger (atom []) (atom [])))

(def ^:dynamic *logger* (->NullLogger))

(defmacro with-logger [logger & body]
  `(binding [*logger* ~logger]
     ~@body))

(defn log-warning [msg]
  (warn *logger* msg))

(defn log-error [msg]
  (error *logger* msg))
