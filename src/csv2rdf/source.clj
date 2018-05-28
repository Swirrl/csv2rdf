(ns csv2rdf.source
  "A source is an instance of clojure.java.io/IOFactory which has an associated URI"
  (:require [clojure.java.io :as io]
            [csv2rdf.util :as util]
            [clojure.spec.alpha :as s]
            [csv2rdf.http :as http])
  (:import [java.net URI]
           [java.io File InputStream ByteArrayInputStream]))

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

(defprotocol JSONSource
  "Protocol for loading a JSON map from a given source"
  (get-json [this]))

(def ^{:doc "Implementation of JSONSouce for types which implement the IOFactory protocol"} io-json-source
  {:get-json util/read-json})

(extend URI JSONSource io-json-source)
(extend File JSONSource io-json-source)

(defrecord MapMetadataSource [uri json]
  URIable
  (->uri [_this] uri)

  JSONSource
  (get-json [_this] json))

(defprotocol IntoInputStream
  (into-input-stream [this]))

(extend-protocol IntoInputStream
  InputStream
  (into-input-stream [is] is)

  String
  (into-input-stream [^String s] (ByteArrayInputStream. (.getBytes s)))

  File
  (into-input-stream [f] (io/input-stream f)))

(defprotocol InputStreamRequestable
  (request-input-stream [this]))

(defmulti request-uri-input-stream (fn [uri] (keyword (.getScheme uri))))

(defmethod request-uri-input-stream :http [uri]
  (let [{:keys [headers body]} (http/get-uri uri)]
    {:headers headers
     :stream (into-input-stream body)}))

(defmethod request-uri-input-stream :file [uri]
  {:headers {} :stream (io/input-stream uri)})

(extend-protocol InputStreamRequestable
  File
  (request-input-stream [f] {:headers {}
                             :stream (io/input-stream f)})

  URI
  (request-input-stream [uri] (request-uri-input-stream uri))

  IOSource
  (request-input-stream [{:keys [io]}]
    {:headers {} :stream (io/input-stream io)}))

(s/def ::uriable #(satisfies? URIable %))
(s/def ::json-source #(satisfies? JSONSource %))
