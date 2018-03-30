(ns csv2rdf.source
  "A source is an instance of clojure.java.io/IOFactory which has an associated URI"
  (:require [clojure.java.io :as io])
  (:import [java.net URI]
           [java.io File]))

(defprotocol URIable
  (->uri ^URI [this]))

(extend-protocol URIable
  File
  (->uri [file] (.toURI file))

  URI
  (->uri [uri] uri))

(defrecord IOSource [uri io]
  URIable
  (->uri [_this] uri)

  io/IOFactory
  (make-reader [_this opts] (io/make-reader io opts))
  (make-writer [_this opts] (io/make-writer io opts))
  (make-input-stream [_this opts] (io/make-input-stream io opts))
  (make-output-stream [_this opts] (io/make-output-stream io opts)))

(defn io-source [uri io]
  "Creates a source from a URI and an instance of IOFactory."
  {:pre [(satisfies? io/IOFactory io)]}
  (->IOSource uri io))

