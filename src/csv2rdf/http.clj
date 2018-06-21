(ns csv2rdf.http
  "Functions for making HTTP requests and processing responses.

  RFC 5988: https://tools.ietf.org/html/rfc5988"
  (:require [clojure.spec.alpha :as s]
            [clj-http.client :as client]
            [csv2rdf.util :as util])
  (:import [org.apache.http.message BasicHeader]
           [java.net URI URISyntaxException]
           [org.apache.http HeaderElement NameValuePair]))

(s/def ::link-uri #(instance? URI %))
(s/def ::link (s/keys :req [::link-uri]))
(s/def ::status (s/and integer? pos?))
(s/def ::header-value (s/or :single string? :multi (s/coll-of string? :min-count 2)))
(s/def ::headers (s/map-of string? ::header-value))
(s/def ::response (s/keys :req-un [::status ::headers ::body]))

(defn is-ok-response?
  "Whether the given map represents a successful HTTP response"
  [{:keys [status] :as response}]
  (and (>= status 200) (<= status 300)))

(defn ^{:table-spec "5.3"} is-not-found-response?
  "Indicates whether the response map represents a 'not found' response."
  [{:keys [status]}]
  (and (>= status 400) (<= status 600)))

(defn- parse-parameters [^HeaderElement element]
  (into {} (map (fn [^NameValuePair p] [(keyword (.getName p)) (.getValue p)]) (.getParameters element))))

(defn- parse-link-uri
  "Extracts the URI referenced in a Link response header."
  [link-str]
  (if-let [[_ uri-str] (re-find #"<([^>]*)>" link-str)]
    (try
      (URI. uri-str)
      (catch URISyntaxException ex
        (throw (ex-info (.getMessage ex)
                        {:type ::invalid-link-uri
                         :link-string link-str}))))
    (throw (ex-info "Invalid link header format: expected <link-uri> (; key=value)*"
                    {:type ::invalid-link-uri
                     :link-string link-str}))))

(defn parse-header
  "Parses a header into its value and an associated map of parameter values."
  [header-string]
  (let [header (BasicHeader. "ignored" header-string)
        ^HeaderElement element (first (.getElements header))
        params (parse-parameters element)]
    {:value (.getName element) :params params}))

(defn parse-link-header
  "Parse a Link header value into a map conforming to the ::link spec. The target of the link is associated
   with the ::link-uri key, any other parameters of the link are associated with unqualified keys in the result
   map."
  [header-string]
  (let [{:keys [value params]} (parse-header header-string)]
    (assoc params ::link-uri (parse-link-uri value))))

(def ^{:table-spec "5.2"} link-header-name "Link")
(def ^{:table-spec "6.1.3.2"} tsv-content-type "text/tab-separated-values")

(def ^{:rfc5988 "4.1"
       :doc "Whether two link relation types are equal. Relation types are compared case-insensitively."}
relation-type= util/equals-ignore-case?)

(defn find-links
  "Finds and parses all the Link headers in the given header map. Links are returned in the same order as the
   corresponding headers in the response. Any malformed Link headers are ignored and removed from the output."
  [headers]
  (->> (get headers link-header-name)
       (util/->coll)
       (map (fn [header-str] (util/ignore-exceptions (parse-link-header header-str))))
       (remove nil?)))

(defprotocol HttpClient
  "Represents a client for making HTTP GET requests"
  (http-get [this uri]))

(defrecord CljHttpClient []
  HttpClient
  (http-get [_ uri]
    (client/get (str uri))))

(def ^{:dynamic true :doc "The currently-bound default HTTP client"} *http-client* (->CljHttpClient))

(defmacro with-http-client
  "Executes body with the given HttpClient implementation as the default"
  [client & body]
  `(binding [*http-client* ~client]
     ~@body))

(defn get-uri
  "Executes a HTTP GET request against the given URI"
  ([uri] (get-uri uri *http-client*))
  ([uri client] (http-get client uri)))
